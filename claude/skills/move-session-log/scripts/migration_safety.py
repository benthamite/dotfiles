"""Conservative offline file migration; not a transaction or writer lock."""
from __future__ import annotations

import ctypes
import errno
import hashlib
import json
import os
from pathlib import Path
import re
import stat
import subprocess
import sys
import tempfile


class MigrationError(RuntimeError):
    """A sanitized refusal, optionally identifying a retained recovery backup."""

    def __init__(self, message, backup_dir=None):
        self.backup_dir = backup_dir
        super().__init__(message + (f"; recovery backup: {backup_dir}" if backup_dir else ""))


def _identity(info):
    return (info.st_dev, info.st_ino, info.st_mode, info.st_uid, info.st_gid)


def _version(info):
    return _identity(info) + (info.st_size, info.st_mtime_ns, info.st_ctime_ns)


def _absolute(path):
    return Path(os.path.abspath(os.fspath(path)))


def _inside(path, root):
    return path == root or root in path.parents


def _capture(path):
    """Read a stable regular file and bind both the alias and resolved inode."""
    try:
        link = path.lstat()
        resolved = path.resolve(strict=True)
        if not stat.S_ISREG(resolved.lstat().st_mode):
            raise MigrationError("Input is not a regular file")
        with os.fdopen(os.open(resolved, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK), "rb") as stream:
            before = os.fstat(stream.fileno())
            if not stat.S_ISREG(before.st_mode):
                raise MigrationError("Input is not a regular file")
            data = stream.read()
            after = os.fstat(stream.fileno())
        if (_version(before) != _version(after)
                or _version(after) != _version(resolved.stat())
                or _version(link) != _version(path.lstat())
                or resolved != path.resolve(strict=True)):
            raise MigrationError("Input changed while being read")
        return {"path": path, "resolved": resolved, "link": _version(link),
                "version": _version(after), "data": data}
    except (OSError, ValueError, RuntimeError) as error:
        if isinstance(error, MigrationError):
            raise
        raise MigrationError("Cannot read a stable regular input") from None


def _lsof_safe_path(raw):
    """Match misc.c safestrprt in LC_ALL=C for printable path bytes only."""
    if any(value < 32 or value == 255 for value in raw):
        raise MigrationError("Unsupported writer-inspection path encoding")
    return b"".join(b"\\\\" if value == 92 else bytes([value]) if value < 127
                    else f"\\x{value:02x}".encode("ascii") for value in raw)


def _check_mount_diagnostics(stderr):
    """Accept only complete Apple 4.91 mount diagnostics caused by our -b.

    Apple's misc.c deliberately avoids path stat/readlink under -b.  Its
    libproc/dmnt.c retains those mounts using kernel mount-table device IDs.
    libproc/dfile.c obtains vnode dev/inode before consulting that table.
    No filename selection or filesystem-name attribution is used below.
    Reviewed source: https://github.com/apple-oss-distributions/lsof/tree/main/lsof
    (version: 4.91; misc.c, dialects/darwin/libproc/{dmnt,dfile,dproc}.c).
    """
    lines = stderr.splitlines()
    if len(lines) % 7:
        raise MigrationError("Unrecognized writer-inspection diagnostics")
    for offset in range(0, len(lines), 7):
        group = lines[offset:offset + 7]
        mount = re.fullmatch(rb"lsof: avoiding readlink\((.+)\): -b was specified\.", group[0])
        mount_stat = re.fullmatch(rb"lsof: avoiding stat\((.+)\): -b was specified\.", group[1])
        warning = re.fullmatch(rb"lsof: WARNING: can't stat\(\) [A-Za-z0-9_-]+ file system (.+)", group[2])
        device = re.fullmatch(rb"lsof: avoiding readlink\((.+)\): -b was specified\.", group[5])
        device_stat = re.fullmatch(rb"lsof: avoiding stat\((.+)\): -b was specified\.", group[6])
        if not (mount and mount_stat and warning and device and device_stat
                and warning[1] == mount[1] == _lsof_safe_path(mount_stat[1])
                and group[3] == b"      Output information may be incomplete."
                and re.fullmatch(rb'      assuming "dev=[0-9a-f]+" from mount table', group[4])
                and device[1] == _lsof_safe_path(device_stat[1])):
            raise MigrationError("Unrecognized writer-inspection diagnostics")


def _record_diagnostic(record):
    """Describe only fixed shape/error categories, never names, PIDs or paths."""
    name = record.get(b"n", b"")
    category = {b"(revoked)": "vnode-revoked", b"no more information": "vnode-EPERM"}.get(name)
    # Exact prefixes from the reviewed Darwin dfile/dsock/dproc error paths.
    prefixes = {"vnode": "vnode", "socket": "socket", "pipe": "pipe", "kqueue": "kqueue",
                "semaphore": "semaphore", "POSIX shared memory": "shared-memory",
                "FD info error": "process-fds", "FILEPORT info error": "process-fileports",
                "region info error": "mapped-region", "thread info error": "thread-directory"}
    for prefix, label in prefixes.items():
        if name == (prefix + ": FD unavailable").encode():
            category = label + "-EBADF"
        elif name == (prefix + ": process unavailable").encode():
            category = label + "-ESRCH"
        else:
            for number, code in errno.errorcode.items():
                if name == (prefix + ": " + os.strerror(number)).encode():
                    category = label + "-" + code
    access = {None: "absent", b" ": "blank", b"": "empty", b"r": "read",
              b"w": "write", b"u": "readwrite"}.get(record.get(b"a"), "other")
    descriptor = record.get(b"f", b"")
    shape = ("decimal" if descriptor.isdigit() else
             "star-three-digits" if re.fullmatch(rb"\*[0-9]{3}", descriptor) else
             "mapped" if descriptor == b"txt" else "other")
    keys = "".join(key.decode("ascii") for key in sorted(record) if key in (b"f", b"a", b"D", b"i", b"t", b"n"))
    return f" [fields={keys}; access={access}; reason={category or 'unknown-redacted'}; descriptor={shape}]"


def _check_kernel_records(output, selected):
    """Reject matching writers, ambiguous references and incomplete records."""
    # Darwin vnode/socket/descriptor types from the reviewed source and native
    # 4.91 field output.  A numeric mode or unknown type is not proof of non-file.
    nonregular = {b"FIFO", b"CHR", b"DIR", b"BLK", b"LINK", b"ATALK", b"FSEVENTS",
                  b"KQUEUE", b"PIPE", b"PSXSEM", b"PSXSHM", b"IPv4", b"IPv6", b"unix",
                  b"systm", b"CHAN", b"NEXUS", b"NPOLICY", b"rte", b"ndrv", b"key",
                  b"ppp", b"vsock", b"vsockp"}
    process = False
    records = 0
    for line in output.splitlines():
        if not line.endswith(b"\0"):
            raise MigrationError("Malformed writer-inspection fields")
        fields = line[:-1].split(b"\0")
        if fields[0].startswith(b"p"):
            if len(fields) != 1 or not fields[0][1:].isdigit():
                raise MigrationError("Malformed writer-inspection process record")
            process = True
            continue
        record = {}
        for field in fields:
            if not field or field[:1] not in (b"f", b"a", b"D", b"i", b"t", b"n") or field[:1] in record:
                raise MigrationError("Malformed writer-inspection file record")
            record[field[:1]] = field[1:]
        if not process or not record.get(b"f"):
            raise MigrationError("Writer-inspection record lacks process or descriptor identity")
        # Apple dfile.c/dsock.c: process_vnode/process_socket -> err2nm maps
        # EBADF to these exact messages.  The descriptor has closed; no inode or
        # access information was obtained.  proc.c initializes access to one
        # ASCII space and prints that selected field unconditionally.
        # Do not extend this exact wire shape to revoked/EPERM.
        if (record[b"f"].isdigit() and set(record) == {b"f", b"a", b"n"}
                and record[b"a"] == b" "
                and record[b"n"] in (b"vnode: FD unavailable", b"socket: FD unavailable")):
            records += 1
            continue
        if record[b"f"] == b"err":
            raise MigrationError("Writer inspection reports an incomplete process descriptor scan"
                                 + _record_diagnostic(record))
        if not record.get(b"t"):
            category = ("mapped" if record[b"f"] == b"txt" else
                        "directory" if record[b"f"] in (b"cwd", b"rtd", b"twd") else "descriptor")
            raise MigrationError("Writer inspection lacks " + category + " object type"
                                 + _record_diagnostic(record))
        if record[b"t"] != b"REG" and record[b"t"] not in nonregular:
            raise MigrationError("Unrecognized writer-inspection object type" + _record_diagnostic(record))
        records += 1
        device, inode = record.get(b"D"), record.get(b"i")
        if not device or not inode:
            if record[b"t"] == b"REG" or record[b"f"] == b"txt":
                raise MigrationError("Incomplete regular or mapped-file identity" + _record_diagnostic(record))
            continue  # A positively identified non-regular object cannot be an input.
        if not re.fullmatch(rb"0x[0-9a-fA-F]+", device) or not inode.isdigit():
            raise MigrationError("Invalid writer-inspection device or inode")
        if (int(device, 16), int(inode)) in selected and record.get(b"a") != b"r":
            raise MigrationError("An input has an open writer or unknown access mode")
    if not process or not records:
        raise MigrationError("Writer inspection returned no process/file records")


def _check_writers(paths):
    """Inspect kernel identities with the supported Darwin/libproc 4.91 route.

    This is not universal visibility: libproc may hide inaccessible processes,
    and a new writer can open after inspection.  Apply still requires offline.
    """
    selected = {(info.st_dev, info.st_ino) for info in (Path(path).stat() for path in paths)}
    if not selected:
        return
    if sys.platform != "darwin":
        raise MigrationError("Writer inspection requires Darwin libproc-based lsof 4.91")
    try:
        options = dict(capture_output=True, timeout=30, env=dict(os.environ, LC_ALL="C"))
        version = subprocess.run(["/usr/sbin/lsof", "-v"], **options)
        details = version.stdout + version.stderr
        if (version.returncode or not re.search(rb"(?m)^\s*revision: 4\.91$", details)
                or not re.search(rb"(?m)^\s*configuration info: libproc-based$", details)):
            raise MigrationError("Writer inspection requires the reviewed lsof version")
        # n is needed only for the exact EBADF classification.  Normal names
        # and unknown error text are discarded, never printed or journaled.
        result = subprocess.run(["/usr/sbin/lsof", "-b", "-nP", "-F0pfaDitn"], **options)
    except (OSError, subprocess.SubprocessError):
        raise MigrationError("Cannot inspect input writers") from None
    if result.returncode:
        raise MigrationError("Writer inspection failed")
    _check_mount_diagnostics(result.stderr)
    _check_kernel_records(result.stdout, selected)


def _rename_function():
    if sys.platform != "darwin":
        raise MigrationError("Atomic no-overwrite moves are unsupported on this platform")
    try:
        function = ctypes.CDLL(None, use_errno=True).renamex_np
        function.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_uint]
        function.restype = ctypes.c_int
        return function
    except (OSError, AttributeError):
        raise MigrationError("Atomic no-overwrite moves are unavailable") from None


def _move_no_replace(source, destination):
    # Darwin stdio.h: RENAME_EXCL == 0x00000004; rename(2) refuses collisions.
    if _rename_function()(os.fsencode(source), os.fsencode(destination), 4):
        raise MigrationError("No-overwrite move failed; source and destination require inspection")


def _private_file(path, data):
    with os.fdopen(os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600), "wb") as stream:
        os.fchmod(stream.fileno(), 0o600)
        stream.write(data)
        stream.flush()
        os.fsync(stream.fileno())


def _sync_directory(path):
    descriptor = os.open(path, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


class MigrationPlan:
    """Capture inputs first, then back up and apply with per-operation checks.

    Rewrites precede moves, irrespective of registration order.  Callbacks run
    last, must verify their own inputs immediately before writes, and own their
    post-write checks.  Their changes are not silently adopted as new snapshots.
    Offline is a caller assertion: lsof and hashes do not lock out future writers.
    """

    def __init__(self):
        self.files = {}
        self.absent = {}
        self.rewrites = {}
        self.moves = []
        self.watched = []
        self.directories = {}
        self.actions = []
        self.checks = []
        self.backup_dir = None
        self.journal = []
        self._used = False

    def read(self, path):
        path = _absolute(path)
        if path in self.files:
            self.verify(path)
        else:
            self.files[path] = _capture(path)
        return self.files[path]["data"]

    def expect_absent(self, path):
        path = _absolute(path)
        if path in self.absent:
            self.verify(path)
            return
        if path in self.files and self.files[path]["path"] == path:
            raise MigrationError("Existing captured input cannot be declared absent")
        try:
            if path.parent in self.directories and not os.path.lexists(path.parent):
                parent = self.absent[path.parent][0] / path.parent.name
                identity = None
            else:
                parent = path.parent.resolve(strict=True)
                identity = _identity(parent.stat())
            if os.path.lexists(path) or (identity is not None and not parent.is_dir()):
                raise MigrationError("Expected an absent destination with an existing parent")
            self.absent[path] = (parent, identity)
        except OSError:
            raise MigrationError("Cannot establish destination absence") from None

    def mkdir(self, path):
        """Plan one private destination directory; do not invent missing parents."""
        path = _absolute(path)
        if not path.parent.is_dir():
            raise MigrationError("Planned directory requires an existing parent")
        self.expect_absent(path)
        self.directories[path] = None

    def rewrite(self, path, data):
        path = _absolute(path)
        if path not in self.files or not isinstance(data, bytes):
            raise MigrationError("Rewrite requires previously captured input and bytes")
        for other in self.rewrites:
            if other != path and self.files[other]["resolved"] == self.files[path]["resolved"]:
                raise MigrationError("Multiple rewrites target the same resolved file")
        if data != self.files[path]["data"]:
            self.rewrites[path] = data

    def _inventory(self, root):
        entries = {}

        def visit(path):
            info = path.lstat()
            if stat.S_ISLNK(info.st_mode) or not (stat.S_ISDIR(info.st_mode) or stat.S_ISREG(info.st_mode)):
                raise MigrationError("Move sources must contain only regular files and directories")
            entries[path.relative_to(root)] = _identity(info)
            if stat.S_ISDIR(info.st_mode):
                for child in sorted(path.iterdir()):
                    visit(child)
        try:
            visit(root)
        except OSError:
            raise MigrationError("Cannot inventory a stable move source") from None
        return entries

    def move(self, source, destination):
        source, destination = _absolute(source), _absolute(destination)
        if source.is_symlink():
            raise MigrationError("Move source cannot be a symlink")
        try:
            root = source.resolve(strict=True)
        except OSError:
            raise MigrationError("Move source is unavailable") from None
        self.expect_absent(destination)
        target = self.absent[destination][0] / destination.name
        if _inside(target, root) or _inside(root, target):
            raise MigrationError("Move source and destination overlap")
        for move in self.moves:
            if any(_inside(a, b) or _inside(b, a)
                   for a in (root, target) for b in (move["root"], move["target"])):
                raise MigrationError("Planned moves overlap")
        entries = self._inventory(root)
        for relative, identity in entries.items():
            if stat.S_ISREG(identity[2]):
                self.read(source / relative)
        self.moves.append({"source": source, "root": root, "destination": destination,
                           "target": target, "entries": entries, "completed": False})
        self._verify_move(self.moves[-1])

    def watch_tree(self, path):
        """Capture complete membership without planning a move or other write."""
        path = _absolute(path)
        if path.is_symlink():
            raise MigrationError("Watched tree cannot itself be a symlink")
        root = path.resolve(strict=True)
        entries = self._inventory(root)
        for relative, identity in entries.items():
            if stat.S_ISREG(identity[2]):
                self.read(path / relative)
        self.watched.append({"source": path, "root": root, "entries": entries})
        self._verify_watched(self.watched[-1])

    def _verify_watched(self, tree):
        if (tree["source"].is_symlink() or tree["source"].resolve() != tree["root"]
                or self._inventory(tree["root"]) != tree["entries"]):
            raise MigrationError("Watched tree membership or location changed")

    def add_action(self, label, callback):
        if not isinstance(label, str) or not label or not callable(callback):
            raise MigrationError("Action requires a label and callable")
        self.actions.append((label, callback))

    def add_check(self, label, callback):
        """Register read-only inventory validation; raise or return False to refuse."""
        if not isinstance(label, str) or not label or not callable(callback):
            raise MigrationError("Inventory check requires a label and callable")
        self.checks.append((label, callback))

    def _run_checks(self):
        for _label, callback in self.checks:
            try:
                if callback() is False:
                    raise MigrationError("Inventory changed")
            except Exception:
                raise MigrationError("Additional inventory validation failed") from None

    def verify(self, path):
        path = _absolute(path)
        if path in self.files:
            if _capture(self.files[path]["path"]) != self.files[path]:
                raise MigrationError("Captured input changed")
        elif path not in self.absent:
            raise MigrationError("Input was not captured during preflight")
        if path in self.absent:
            parent, identity = self.absent[path]
            changed = (os.path.lexists(path) or path.parent.resolve() != parent)
            if identity is None:
                changed = changed or os.path.lexists(parent)
            else:
                changed = changed or _identity(parent.stat()) != identity
            if changed:
                raise MigrationError("Expected-absent path or its parent changed")

    def _verify_move(self, move):
        if self._inventory(move["root"]) != move["entries"]:
            raise MigrationError("Move source membership or identity changed")
        if not move["completed"]:
            if move["source"].is_symlink() or move["source"].resolve() != move["root"]:
                raise MigrationError("Move source location changed")
            self.verify(move["destination"])
            parent = move["target"].parent
            if not parent.exists() and move["destination"].parent in self.directories:
                parent = parent.parent
            if move["root"].stat().st_dev != parent.stat().st_dev:
                raise MigrationError("Cross-filesystem moves are not supported")
            _rename_function()

    def validate(self):
        self._run_checks()
        for path in self.files.keys() | self.absent.keys():
            self.verify(path)
        for move in self.moves:
            self._verify_move(move)
        for tree in self.watched:
            self._verify_watched(tree)
        for path, identity in self.directories.items():
            if identity is not None and (path.is_symlink() or _identity(path.stat()) != identity):
                raise MigrationError("Created destination directory changed")
        _check_writers(snapshot["resolved"] for snapshot in self.files.values())

    def _save_manifest(self):
        data = (json.dumps(self.manifest, indent=2, ensure_ascii=True) + "\n").encode()
        descriptor, name = tempfile.mkstemp(prefix=".journal-", dir=self.backup_dir)
        stage = Path(name)
        identity = _identity(os.fstat(descriptor))
        try:
            with os.fdopen(descriptor, "wb") as stream:
                stream.write(data)
                stream.flush()
                os.fsync(stream.fileno())
            os.replace(stage, self.backup_dir / "manifest.json")
            _sync_directory(self.backup_dir)
        finally:
            if os.path.lexists(stage) and _identity(stage.lstat()) == identity:
                stage.unlink()

    def _backup_target(self, directory):
        directory = Path(directory)
        if not directory.is_absolute():
            raise MigrationError("Backup directory must be absolute")
        try:
            directory = directory.parent.resolve(strict=True) / directory.name
        except OSError:
            raise MigrationError("Backup directory parent must already exist") from None
        drive = (Path.home() / "My Drive").resolve()
        if _inside(directory, drive) or any(_inside(directory, tree["root"])
                                           for tree in self.moves + self.watched):
            raise MigrationError("Backup must be outside Drive and inventoried source trees")
        if os.path.lexists(directory):
            raise MigrationError("Backup directory must be new")
        return directory

    def _backup(self, directory):
        directory = self._backup_target(directory)
        directory.mkdir(mode=0o700)
        self.backup_dir = directory
        directory.chmod(0o700)
        _sync_directory(directory.parent)
        backups = []
        for number, (path, snapshot) in enumerate(self.files.items()):
            name = f"input-{number:06d}.bin"
            _private_file(directory / name, snapshot["data"])
            backups.append({"originalPath": str(path), "resolvedPath": str(snapshot["resolved"]),
                            "mode": stat.S_IMODE(snapshot["version"][2]), "backup": name,
                            "sha256": hashlib.sha256(snapshot["data"]).hexdigest()})
        self.manifest = {"schemaVersion": 1, "inputs": backups,
                         "expectedAbsent": list(map(str, self.absent)),
                         "moves": [{"source": str(m["source"]), "destination": str(m["destination"])}
                                   for m in self.moves], "journal": self.journal,
                         "directories": list(map(str, self.directories)),
                         "recovery": "Inspect journal and current files first. Never restore over unknown changes."}
        self._save_manifest()

    def _rewrite(self, key, data):
        snapshot = self.files[key]
        target = snapshot["resolved"]
        self.verify(key)
        _check_writers([target])
        descriptor, name = tempfile.mkstemp(prefix=".session-migration-", dir=target.parent)
        stage = Path(name)
        identity = _identity(os.fstat(descriptor))
        try:
            with os.fdopen(descriptor, "wb") as stream:
                stream.write(data)
                stream.flush()
                os.fsync(stream.fileno())
                os.fchmod(stream.fileno(), stat.S_IMODE(snapshot["version"][2]))
                identity = _identity(os.fstat(stream.fileno()))
            self.verify(key)
            self._run_checks()
            os.replace(stage, target)
            for old in self.files.values():
                if old["resolved"] == target:
                    current = _capture(old["path"])
                    if (current["version"][:5] != identity or current["data"] != data
                            or (stat.S_ISLNK(old["link"][2]) and current["link"] != old["link"])):
                        raise MigrationError("Replacement identity changed")
                    old.update(current)
            for tree in self.moves + self.watched:
                if _inside(target, tree["root"]):
                    tree["entries"][target.relative_to(tree["root"])] = identity
            _sync_directory(target.parent)
        finally:
            if os.path.lexists(stage) and _identity(stage.lstat()) == identity:
                stage.unlink()

    def _move(self, move):
        self.validate()
        root, target = move["root"], move["target"]
        _move_no_replace(root, target)
        for snapshot in self.files.values():
            if _inside(snapshot["resolved"], root):
                current = _capture(target / snapshot["resolved"].relative_to(root))
                if current["version"][:7] != snapshot["version"][:7] or current["data"] != snapshot["data"]:
                    raise MigrationError("Moved input changed before readback")
                snapshot.update(current)
        del self.absent[move["destination"]]
        for old, (parent, identity) in list(self.absent.items()):
            if _inside(parent, root):
                relocated_parent = target / parent.relative_to(root)
                del self.absent[old]
                self.absent[relocated_parent / old.name] = (relocated_parent, identity)
        move["root"], move["completed"] = target, True
        for tree in self.watched:
            if _inside(tree["root"], root):
                tree["root"] = target / tree["root"].relative_to(root)
                tree["source"] = tree["root"]
            else:
                for relative in list(tree["entries"]):
                    if _inside(tree["root"] / relative, root):
                        del tree["entries"][relative]
                if _inside(target, tree["root"]):
                    for relative, identity in move["entries"].items():
                        tree["entries"][(target / relative).relative_to(tree["root"])] = identity
        self.expect_absent(move["source"])
        _sync_directory(root.parent)
        _sync_directory(target.parent)

    def _mkdir(self, path):
        self.verify(path)
        self._run_checks()
        path.mkdir(mode=0o700)
        path.chmod(0o700)
        identity = _identity(path.stat())
        self.directories[path] = identity
        del self.absent[path]
        for child, (parent, old_identity) in list(self.absent.items()):
            if old_identity is None and parent == path.resolve():
                self.absent[child] = (parent, identity)
        for tree in self.watched:
            if _inside(path.resolve(), tree["root"]):
                tree["entries"][path.resolve().relative_to(tree["root"])] = identity
        _sync_directory(path.parent)

    def run(self, dry_run=False, offline=False, backup_dir=None):
        if self._used:
            raise MigrationError("Migration plans cannot be applied twice")
        try:
            self.validate()
            if backup_dir is not None:
                self._backup_target(backup_dir)
            if dry_run or not (self.rewrites or self.moves or self.actions or self.directories):
                return None
            if not offline or backup_dir is None:
                raise MigrationError("Apply requires an offline assertion and explicit backup directory")
            self._used = True
            self._backup(backup_dir)
            self.validate()
            operations = [("mkdir", str(path), lambda p=path: self._mkdir(p)) for path in self.directories]
            operations += [("rewrite", str(path), lambda p=path, d=data: self._rewrite(p, d))
                           for path, data in self.rewrites.items()]
            operations += [("move", str(move["source"]), lambda m=move: self._move(m))
                           for move in self.moves]
            for kind, label, operation in operations:
                entry = {"kind": kind, "label": label, "status": "started"}
                self.journal.append(entry)
                self._save_manifest()
                operation()
                entry["status"] = "completed"
                self._save_manifest()
            self.validate()
            for label, callback in self.actions:
                entry = {"kind": "action", "label": label, "status": "started"}
                self.journal.append(entry)
                self._save_manifest()
                self._run_checks()
                callback()
                entry["status"] = "completed"
                self._save_manifest()
            return self.backup_dir
        except Exception as error:
            if self.backup_dir:
                if self.journal and self.journal[-1]["status"] == "started":
                    self.journal[-1]["status"] = "failed-or-uncertain"
                    try:
                        self._save_manifest()
                    except Exception:
                        pass  # Existing started record already requires manual inspection.
                reason = str(error) if isinstance(error, MigrationError) else "Migration stopped"
                raise MigrationError(reason + "; partial changes may remain", self.backup_dir) from None
            if isinstance(error, MigrationError):
                raise
            raise MigrationError("Migration preflight failed") from None
