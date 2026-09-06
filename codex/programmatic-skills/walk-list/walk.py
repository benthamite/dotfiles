#!/usr/bin/env python3
"""walk-list: strict one-at-a-time OR N-at-a-time item processor.

On `start`, the input file is MOVED to ~/.claude/walk-list-data/<uuid>/source.json
and replaced at the original path with a stub. Where installed, the hook rejects
recognized direct store access; it does not provide a universal access guarantee.

Sequential mode (default, max_concurrent=1):
    walk.py start <file>
    walk.py next <file> <decision>          # repeat until done
    walk.py restore <file>

Pool mode (parallel subagent dispatch, max_concurrent=N):
    walk.py start <file> --max-concurrent 10
    # main session loop:
    walk.py pool-status <file>
    walk.py dispatch <file>                 # returns TOKEN + item JSON
    # ... spawn subagent with prompt using that TOKEN
    # subagent itself calls:
    walk.py record <file> <TOKEN> <decision>
    # main session dispatches more when slots free up
    walk.py restore <file>

Utilities:
    walk.py status <file>
    walk.py show-decisions <file>
    walk.py release-stale <file> <max-age-seconds>
    walk.py set-max-concurrent <file> <n>
    walk.py abort <file>
"""
import contextlib
import ctypes
import errno
import fcntl
import functools
import io
import json
import math
import os
import shlex
import shutil
import stat
import sys
import tempfile
import uuid
from datetime import datetime, timezone
from pathlib import Path


DATA_ROOT = Path.home() / ".claude" / "walk-list-data"
REGISTRY_PATH = DATA_ROOT / "registry.json"
OUTPUT_ROOT = DATA_ROOT.parent / "walk-list-out"
STUB_MARKER = "_walk_locked"
DEFAULT_MAX = 1


def parse_positive_int(raw: str, label: str) -> int:
    try:
        value = int(raw)
    except ValueError as exc:
        raise SystemExit(f"ERROR: {label} must be a positive integer: {raw}") from exc
    if value < 1:
        raise SystemExit(f"ERROR: {label} must be a positive integer: {raw}")
    return value


def parse_non_negative_float(raw: str, label: str) -> float:
    try:
        value = float(raw)
    except ValueError as exc:
        raise SystemExit(f"ERROR: {label} must be a non-negative number: {raw}") from exc
    if not math.isfinite(value) or value < 0:
        raise SystemExit(f"ERROR: {label} must be a non-negative number: {raw}")
    return value


def walk_command(*args: str) -> str:
    script = Path(__file__).expanduser()
    if not script.is_absolute():
        script = script.absolute()
    return " ".join(shlex.quote(part) for part in ["python", str(script), *args])


def ensure_root() -> None:
    ensure_private_directory(DATA_ROOT)


def ensure_private_directory(path: Path) -> None:
    """Create private directories without changing permissions of existing ones."""
    try:
        existing = path.lstat()
    except FileNotFoundError:
        if not path.parent.exists():
            ensure_private_directory(path.parent)
        try:
            path.mkdir(mode=0o700)
        except FileExistsError:
            pass
        existing = path.lstat()
    require_owned_directory(path)


def require_owned_directory(path: Path) -> None:
    """Validate an existing directory without creating or chmodding it."""
    existing = path.lstat()
    if not stat.S_ISDIR(existing.st_mode) or existing.st_uid != os.getuid():
        raise SystemExit(f"ERROR: expected an owned, non-symlink directory: {path}")


@contextlib.contextmanager
def private_file(path: Path, flags: int, mode: str):
    """Open owned regular files without following links; new files request 0600."""
    fd = os.open(path, flags | os.O_NOFOLLOW | os.O_NONBLOCK, 0o600)
    try:
        metadata = os.fstat(fd)
        if not stat.S_ISREG(metadata.st_mode) or metadata.st_uid != os.getuid():
            raise SystemExit(f"ERROR: expected an owned regular file: {path}")
        text_options = {} if "b" in mode else {"encoding": "utf-8"}
        with os.fdopen(fd, mode, **text_options) as stream:
            fd = None
            yield stream
    finally:
        if fd is not None:
            os.close(fd)


def load_registry() -> dict:
    if not REGISTRY_PATH.exists():
        return {}
    return json.loads(REGISTRY_PATH.read_text(encoding="utf-8"))


def save_registry(reg: dict) -> None:
    ensure_root()
    atomic_json(REGISTRY_PATH, reg)


def registry_bytes():
    try:
        return REGISTRY_PATH.read_bytes()
    except FileNotFoundError:
        return None


def save_registry_checked(reg: dict) -> None:
    """Distinguish an unchanged registry from a committed-but-reported failure."""
    try:
        before = registry_bytes()
    except OSError as error:
        raise RegistryRecoveryError(
            "registry preimage is unreadable; session/recovery data retained"
        ) from error
    desired = json.dumps(reg, indent=2, ensure_ascii=False).encode("utf-8")
    try:
        save_registry(reg)
    except BaseException as error:
        try:
            observed = registry_bytes()
        except OSError as read_error:
            raise RegistryRecoveryError(
                "registry write outcome is unreadable; session/recovery data retained"
            ) from read_error
        if observed == before and observed != desired:
            raise
        if observed == desired and isinstance(error, Exception):
            print("WARNING: registry write reported a failure; exact intended bytes "
                  "were verified before continuing.", file=sys.stderr)
            return
        raise RegistryRecoveryError(
            "registry write outcome is uncertain or interrupted; session/recovery data retained"
        ) from error


def stage_bytes(path: Path, data: bytes) -> Path:
    """Prepare a complete sibling file; never truncate an existing destination."""
    fd, name = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    staged = Path(name)
    try:
        with os.fdopen(fd, "wb") as stream:
            stream.write(data)
            stream.flush()
            os.fsync(stream.fileno())
    except BaseException:
        staged.unlink(missing_ok=True)
        raise
    return staged


def atomic_json(path: Path, data: dict) -> None:
    encoded = json.dumps(data, indent=2, ensure_ascii=False).encode("utf-8")
    staged = stage_bytes(path, encoded)
    try:
        os.replace(staged, path)
    finally:
        staged.unlink(missing_ok=True)


def serialized_command(function):
    """Keep identity resolution and all lifecycle mutations in one lock domain."""
    @functools.wraps(function)
    def serialized(*args, **kwargs):
        ensure_root()
        with private_file(DATA_ROOT / "registry.lock", os.O_CREAT | os.O_APPEND | os.O_WRONLY, "a") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            try:
                output = io.StringIO()
                with contextlib.redirect_stdout(output):
                    result = function(*args, **kwargs)
                print(output.getvalue(), end="")
                return result
            finally:
                fcntl.flock(lock, fcntl.LOCK_UN)
    return serialized


def canonical_key(input_file: Path) -> str:
    return str(input_file.resolve(strict=False))


def resolve_session(input_file: Path) -> tuple[str, Path]:
    reg = load_registry()
    key = canonical_key(input_file)
    sid = reg.get(key)
    if not sid:
        raise SystemExit(
            f"ERROR: no active walk for {input_file}. Run: {walk_command('start', str(input_file))}"
        )
    sdir = DATA_ROOT / sid
    if not sdir.exists():
        raise SystemExit(f"ERROR: registry points to missing session dir {sdir}")
    require_owned_directory(sdir)
    require_owned_stub(input_file, sid)
    return sid, sdir


@contextlib.contextmanager
def locked_state(sdir: Path):
    """Read-modify-write the state file under an exclusive flock so concurrent
    walk.py invocations (e.g. subagents calling `record` simultaneously) don't
    clobber each other."""
    state_path = sdir / "state.json"
    with private_file(sdir / "state.lock", os.O_CREAT | os.O_APPEND | os.O_WRONLY, "a") as f:
        fcntl.flock(f.fileno(), fcntl.LOCK_EX)
        try:
            original = state_path.read_text(encoding="utf-8")
            data = json.loads(original)
            data.setdefault("in_flight", {})
            data.setdefault("to_redispatch", [])
            data.setdefault("max_concurrent", 1)
            yield data
            if data != json.loads(original):
                atomic_json(state_path, data)
        finally:
            fcntl.flock(f.fileno(), fcntl.LOCK_UN)


def parse_items(raw: str) -> list:
    raw = raw.strip()
    if not raw:
        return []
    if raw.startswith("["):
        try:
            data = json.loads(raw)
        except json.JSONDecodeError as exc:
            # Multiple JSONL arrays are not one top-level JSON document.
            lines = [line.strip() for line in raw.splitlines() if line.strip()]
            try:
                records = [json.loads(line) for line in lines]
            except json.JSONDecodeError:
                raise SystemExit("ERROR: malformed JSON array or JSONL input") from exc
            if len(records) > 1:
                return records
            raise SystemExit("ERROR: malformed JSON array input") from exc
        if not isinstance(data, list):
            raise SystemExit("ERROR: top-level JSON must be an array")
        return data
    items = []
    for line in raw.splitlines():
        line = line.strip()
        if not line:
            continue
        if line.startswith("{") or line.startswith("["):
            try:
                items.append(json.loads(line))
                continue
            except json.JSONDecodeError:
                pass
        items.append(line)
    return items


def load_source_items(sdir: Path) -> list:
    return parse_items((sdir / "source.json").read_text(encoding="utf-8"))


def is_stub(path: Path) -> bool:
    if not path.exists():
        return False
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
        return isinstance(data, dict) and data.get(STUB_MARKER) is True
    except (json.JSONDecodeError, OSError):
        return False


def require_owned_stub(path: Path, sid: str) -> None:
    if path.is_symlink() or not is_stub(path):
        raise SystemExit(f"ERROR: input is not this walk's owned stub: {path}")
    stub = json.loads(path.read_text(encoding="utf-8"))
    matches = (stub["session_id"] == sid if "session_id" in stub
               else stub.get("session_prefix") == sid[:8])
    if not matches:
        raise SystemExit(f"ERROR: input stub belongs to another session: {path}")


def pending_index(state: dict, total: int):
    if state["to_redispatch"]:
        return state["to_redispatch"][0]
    return state["cursor"] if state["cursor"] < total else None


def file_identity(path: Path) -> tuple:
    stat = path.lstat()
    return stat.st_dev, stat.st_ino, stat.st_size, stat.st_mtime_ns


def capture_entry(path: Path, *, with_metadata=False) -> tuple:
    """Capture an entry itself, never a symlink's referent."""
    before = path.lstat()
    identity = (before.st_dev, before.st_ino, before.st_mode,
                before.st_size, before.st_mtime_ns)
    if stat.S_ISREG(before.st_mode):
        fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW)
        with os.fdopen(fd, "rb") as stream:
            opened = os.fstat(stream.fileno())
            if (opened.st_dev, opened.st_ino) != identity[:2]:
                raise OSError("input changed while capturing its preimage")
            content = stream.read()
    elif stat.S_ISLNK(before.st_mode):
        content = os.readlink(path)
    else:
        content = None
    after = path.lstat()
    if identity != (after.st_dev, after.st_ino, after.st_mode,
                    after.st_size, after.st_mtime_ns):
        raise OSError("input changed while capturing its preimage")
    captured = identity, content
    return (captured, before) if with_metadata else captured


def matches_entry(path: Path, expected: tuple) -> bool:
    try:
        return capture_entry(path) == expected
    except OSError:
        return False


def exchange_paths(left: Path, right: Path) -> None:
    """Native atomic exchange, with pinned parent descriptors and no fallback.

    Darwin sys/stdio.h defines RENAME_SWAP=2 for renameatx_np.
    Linux uapi/linux/fs.h defines RENAME_EXCHANGE=(1 << 1) for renameat2.
    Both entries must exist; unsupported kernels/filesystems refuse.
    """
    name = "renameatx_np" if sys.platform == "darwin" else (
        "renameat2" if sys.platform.startswith("linux") else None)
    libc = ctypes.CDLL(None, use_errno=True)
    function = getattr(libc, name, None) if name else None
    if function is None:
        raise OSError(errno.ENOTSUP, "native atomic exchange is unavailable")
    function.argtypes = [ctypes.c_int, ctypes.c_char_p,
                         ctypes.c_int, ctypes.c_char_p, ctypes.c_uint]
    function.restype = ctypes.c_int
    flags = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW
    left_fd = os.open(left.parent.resolve(strict=True), flags)
    try:
        right_fd = os.open(right.parent.resolve(strict=True), flags)
        try:
            if function(left_fd, os.fsencode(left.name),
                        right_fd, os.fsencode(right.name), 2):
                code = ctypes.get_errno()
                raise OSError(code, os.strerror(code))
        finally:
            os.close(right_fd)
    finally:
        os.close(left_fd)


class PublicationRecoveryError(OSError):
    """Publication is uncertain; retained files must not be discarded."""


class RegistryRecoveryError(PublicationRecoveryError):
    """The registry cannot be reconciled with a known lifecycle transition."""


class InputPublication:
    """Exchange a staged input and retain every unproven displaced entry."""

    def __init__(self, target: Path, content: bytes, expected: tuple,
                 *, mode: int = 0o600, times=None):
        self.target = target
        self.expected = expected
        self.directory = Path(tempfile.mkdtemp(
            prefix=f".{target.name}.walk-recovery-", dir=target.parent)).resolve()
        self.staged = self.directory / "entry"
        self.published = None
        self.exchanged = False
        self.uncertain = False
        try:
            with private_file(self.staged, os.O_WRONLY | os.O_CREAT | os.O_EXCL, "wb") as stream:
                stream.write(content)
                stream.flush()
                os.fsync(stream.fileno())
                os.fchmod(stream.fileno(), mode)
                if times is not None:
                    os.utime(stream.fileno(), ns=times)
            self.published = capture_entry(self.staged)
        except BaseException as error:
            # Retain partially staged data rather than guessing its ownership.
            raise self.recovery_error() from error

    def recovery_error(self):
        self.uncertain = True
        return PublicationRecoveryError(
            f"input publication uncertain; retained recovery files at {self.directory}; "
            "source and session state were not discarded")

    def rollback(self) -> None:
        """Attempt at most one swap-back; retain both sides of another race."""
        if not matches_entry(self.target, self.published):
            raise self.recovery_error()
        try:
            displaced = capture_entry(self.staged)
        except OSError:
            raise self.recovery_error() from None
        try:
            exchange_paths(self.staged, self.target)
        except BaseException as error:
            # A reported transport/syscall failure may follow a completed swap.
            if not (matches_entry(self.target, displaced)
                    and matches_entry(self.staged, self.published)):
                raise self.recovery_error() from error
        if not (matches_entry(self.target, displaced)
                and matches_entry(self.staged, self.published)):
            raise self.recovery_error()
        self.exchanged = False

    def publish(self) -> None:
        try:
            exchange_paths(self.staged, self.target)
        except BaseException as error:
            if (matches_entry(self.staged, self.published)
                    and matches_entry(self.target, self.expected)):
                raise
            if matches_entry(self.target, self.published):
                self.exchanged = True
                self.rollback()
                raise error
            raise self.recovery_error() from error
        self.exchanged = True
        if not (matches_entry(self.staged, self.expected)
                and matches_entry(self.target, self.published)):
            self.rollback()
            raise SystemExit("ERROR: input changed at publication; replacement preserved.")

    def commit(self) -> None:
        if not (matches_entry(self.staged, self.expected)
                and matches_entry(self.target, self.published)):
            raise self.recovery_error()
        # The displaced original is now safely represented by the source copy
        # (start) or completed evidence/source state (restore).
        try:
            self.staged.unlink()
            self.exchanged = False
            self.directory.rmdir()
        except BaseException as error:
            raise self.recovery_error() from error

    def close(self) -> None:
        if self.uncertain or self.exchanged or not self.directory.exists():
            return
        if self.staged.exists() or self.staged.is_symlink():
            if self.published is None or not matches_entry(self.staged, self.published):
                raise self.recovery_error()
            self.staged.unlink()
        try:
            self.directory.rmdir()
        except OSError as error:
            # Unknown additions belong to their writer, not this operation.
            raise self.recovery_error() from error


def print_item(index: int, total: int, item) -> None:
    print(f"=== ITEM {index + 1} OF {total} ===")
    if isinstance(item, (dict, list)):
        print(json.dumps(item, indent=2, ensure_ascii=False))
    else:
        print(item)


@serialized_command
def cmd_start(input_file: Path, max_concurrent: int) -> None:
    if input_file.is_symlink():
        raise SystemExit("ERROR: symlink inputs are not supported; use the real file path.")
    if is_stub(input_file):
        sid, sdir = resolve_session(input_file)
        items = load_source_items(sdir)
        with locked_state(sdir) as state:
            max_c = state["max_concurrent"]
            cursor = state["cursor"]
            in_flight = state["in_flight"]
            done = len(state["decisions"])
            print(f"RESUMING walk (session {sid[:8]}). max_concurrent={max_c}.")
            print(
                f"Total: {len(items)}  Done: {done}  In-flight: {len(in_flight)}  "
                f"Cursor: {cursor}  Redispatch-queue: {len(state['to_redispatch'])}"
            )
            index = pending_index(state, len(items))
            if done >= len(items) and not in_flight and index is None:
                print(f"WALK COMPLETE. Run: {walk_command('restore', str(input_file))}")
                return
            if max_c == 1 and not in_flight and index is not None:
                print()
                print_item(index, len(items), items[index])
                state["sequential_index"] = index
                print()
                print(f"When done: {walk_command('next', str(input_file), '<decision>')}")
            else:
                print(f"Use: {walk_command('dispatch', str(input_file))}  (to claim items)")
        return

    if not input_file.exists():
        raise SystemExit(f"ERROR: input file not found: {input_file}")
    key = canonical_key(input_file)
    reg = load_registry()
    if key in reg:
        raise SystemExit("ERROR: active walk exists but its input stub was replaced.")
    max_concurrent = parse_positive_int(str(max_concurrent), "max_concurrent")
    original_identity = file_identity(input_file)
    original_entry, original_metadata = capture_entry(input_file, with_metadata=True)
    if not stat.S_ISREG(original_entry[0][2]):
        raise SystemExit("ERROR: input must be a regular file.")
    original_bytes = original_entry[1]
    items = parse_items(original_bytes.decode("utf-8"))
    if not items:
        raise SystemExit(f"ERROR: no items found in {input_file}")

    ensure_root()
    sid = uuid.uuid4().hex
    sdir = DATA_ROOT / sid
    sdir.mkdir(mode=0o700, parents=True, exist_ok=False)
    source_path = sdir / "source.json"
    # Preserve the original until source, state and stub are all staged.
    with private_file(source_path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, "wb") as source:
        source.write(original_bytes)
        source.flush()
        os.fsync(source.fileno())
        os.utime(source.fileno(), ns=(original_metadata.st_atime_ns,
                                     original_metadata.st_mtime_ns))
    state = {
        "cursor": 0,
        "in_flight": {},
        "to_redispatch": [],
        "decisions": [],
        "max_concurrent": max_concurrent,
        "original_path": key,
        "source_mode": stat.S_IMODE(original_metadata.st_mode),
        "source_atime_ns": original_metadata.st_atime_ns,
        "source_mtime_ns": original_metadata.st_mtime_ns,
        "sequential_index": 0 if max_concurrent == 1 else None,
    }
    atomic_json(sdir / "state.json", state)

    input_arg = str(input_file)
    stub = {
        STUB_MARKER: True,
        "message": "Locked by walk-list. Use walk.py commands to interact.",
        "session_prefix": sid[:8],
        "session_id": sid,
        "max_concurrent": max_concurrent,
        "commands": {
            "dispatch (pool)": walk_command("dispatch", input_arg),
            "record (pool)": walk_command("record", input_arg, "<token>", "<decision>"),
            "pool-status": walk_command("pool-status", input_arg),
            "next (sequential)": walk_command("next", input_arg, "<decision>"),
            "status": walk_command("status", input_arg),
            "restore": walk_command("restore", input_arg),
        },
    }
    publication = InputPublication(
        input_file, json.dumps(stub, indent=2).encode("utf-8"), original_entry)
    try:
        if (input_file.is_symlink() or file_identity(input_file) != original_identity
                or not matches_entry(input_file, original_entry)):
            raise SystemExit("ERROR: input changed during start; original copy retained.")
        new_registry = dict(reg, **{key: sid})
        save_registry_checked(new_registry)
        try:
            if (input_file.is_symlink() or file_identity(input_file) != original_identity
                    or not matches_entry(input_file, original_entry)):
                raise SystemExit("ERROR: input changed during start; original copy retained.")
            publication.publish()
            publication.commit()
        except BaseException:
            if not publication.uncertain:
                try:
                    save_registry_checked(reg)
                except BaseException as error:
                    publication.uncertain = True
                    raise RegistryRecoveryError(
                        f"registry rollback failed; recovery files retained at {publication.directory}"
                    ) from error
            raise
    except RegistryRecoveryError:
        publication.uncertain = True
        raise
    finally:
        publication.close()

    print(
        f"STARTED walk (session {sid[:8]}). {len(items)} items locked. "
        f"max_concurrent={max_concurrent}."
    )
    if max_concurrent == 1:
        print()
        print_item(0, len(items), items[0])
        print()
        print(f"When done: {walk_command('next', str(input_file), '<decision>')}")
    else:
        print(
            f"Use: {walk_command('dispatch', str(input_file))}  (claim up to {max_concurrent} items in flight)"
        )


@serialized_command
def cmd_next(input_file: Path, decision: str) -> None:
    if not decision.strip():
        raise SystemExit("ERROR: decision text required.")
    sid, sdir = resolve_session(input_file)
    items = load_source_items(sdir)
    with locked_state(sdir) as state:
        if state["max_concurrent"] != 1:
            raise SystemExit("ERROR: next requires sequential mode; use dispatch/record.")
        if state["in_flight"]:
            raise SystemExit(
                f"ERROR: in-flight claims exist. Use `{walk_command('record', str(input_file), '<token>', '<decision>')}` "
                "for the completed worker's verdict. Only after confirming worker termination "
                "and reconciling its effects, release abandoned claims with "
                f"`{walk_command('release-stale', str(input_file), '<age-seconds>')}`."
            )
        cursor = state["cursor"]
        if cursor >= len(items) and not state["to_redispatch"]:
            print(f"Already complete. Run: {walk_command('restore', str(input_file))}")
            return
        index = pending_index(state, len(items))
        if "sequential_index" not in state:
            # Legacy sequential walks already exposed their cursor. Pool history
            # or redispatch requires an explicit resume before a sequential verdict.
            stub = json.loads(input_file.read_text(encoding="utf-8"))
            legacy_sequential = (not state["to_redispatch"]
                                 and stub.get("max_concurrent", 1) == 1
                                 and not any("claim_token" in d for d in state["decisions"]))
            state["sequential_index"] = index if legacy_sequential else None
        if state["sequential_index"] != index:
            raise SystemExit("ERROR: item not shown sequentially; run start to resume first.")
        # In sequential mode, consume either a redispatch-queue entry or cursor
        if state["to_redispatch"]:
            index = state["to_redispatch"].pop(0)
        else:
            index = cursor
            state["cursor"] = cursor + 1
        state["decisions"].append(
            {"index": index, "item": items[index], "decision": decision.strip(),
             "recorded_at": datetime.now(timezone.utc).isoformat()}
        )
        state["sequential_index"] = pending_index(state, len(items))
        remaining = (len(items) - state["cursor"]) + len(state["to_redispatch"])
        print(f"Recorded decision for item {index + 1}. Remaining: {remaining}")
        if remaining == 0:
            print(f"WALK COMPLETE. Run: {walk_command('restore', str(input_file))}")
            return
        # Show next item
        if state["to_redispatch"]:
            next_index = state["to_redispatch"][0]
        else:
            next_index = state["cursor"]
        print()
        print_item(next_index, len(items), items[next_index])
        print()
        print(f"When done: {walk_command('next', str(input_file), '<decision>')}")


@serialized_command
def cmd_dispatch(input_file: Path) -> None:
    sid, sdir = resolve_session(input_file)
    items = load_source_items(sdir)
    with locked_state(sdir) as state:
        max_c = state["max_concurrent"]
        if len(state["in_flight"]) >= max_c:
            raise SystemExit(
                f"ERROR: pool full ({len(state['in_flight'])}/{max_c} in flight). "
                "Wait for a record() call."
            )
        if state["to_redispatch"]:
            index = state["to_redispatch"].pop(0)
        else:
            cursor = state["cursor"]
            if cursor >= len(items):
                raise SystemExit("ERROR: no more items to dispatch.")
            index = cursor
            state["cursor"] = cursor + 1
        token = uuid.uuid4().hex
        state["sequential_index"] = None
        state["in_flight"][token] = {
            "index": index,
            "item": items[index],
            "dispatched_at": datetime.now(timezone.utc).isoformat(),
        }
        in_flight_n = len(state["in_flight"])
        remaining = (len(items) - state["cursor"]) + len(state["to_redispatch"])
    print(f"CLAIM_TOKEN: {token}")
    print(f"INDEX: {index + 1} OF {len(items)}")
    print(f"IN_FLIGHT: {in_flight_n}/{max_c}  REMAINING_TO_CLAIM: {remaining}")
    print()
    item = items[index]
    if isinstance(item, (dict, list)):
        print(json.dumps(item, indent=2, ensure_ascii=False))
    else:
        print(item)


@serialized_command
def cmd_record(input_file: Path, token: str, decision: str) -> None:
    if not decision.strip():
        raise SystemExit("ERROR: decision text required.")
    sid, sdir = resolve_session(input_file)
    with locked_state(sdir) as state:
        if token not in state["in_flight"]:
            raise SystemExit(
                f"ERROR: unknown or already-recorded claim token: {token}"
            )
        claim = state["in_flight"].pop(token)
        state["decisions"].append(
            {
                "index": claim["index"],
                "item": claim["item"],
                "decision": decision.strip(),
                "claim_token": token,
                "dispatched_at": claim.get("dispatched_at"),
                "recorded_at": datetime.now(timezone.utc).isoformat(),
            }
        )
        in_flight_n = len(state["in_flight"])
        done_n = len(state["decisions"])
    print(
        f"Recorded decision for item {claim['index'] + 1}. "
        f"in_flight={in_flight_n} done={done_n}"
    )


@serialized_command
def cmd_pool_status(input_file: Path) -> None:
    sid, sdir = resolve_session(input_file)
    items = load_source_items(sdir)
    with locked_state(sdir) as state:
        total = len(items)
        in_flight = len(state["in_flight"])
        done = len(state["decisions"])
        cursor = state["cursor"]
        max_c = state["max_concurrent"]
        available = max(0, max_c - in_flight)
        remaining_to_claim = (total - cursor) + len(state["to_redispatch"])
        out = {
            "session": sid[:8],
            "total": total,
            "done": done,
            "in_flight": in_flight,
            "max_concurrent": max_c,
            "available_slots": available,
            "remaining_to_claim": remaining_to_claim,
            "cursor": cursor,
            "redispatch_queue": len(state["to_redispatch"]),
        }
    print(json.dumps(out, indent=2))


@serialized_command
def cmd_status(input_file: Path) -> None:
    sid, sdir = resolve_session(input_file)
    items = load_source_items(sdir)
    with locked_state(sdir) as state:
        total = len(items)
        print(f"Session: {sid[:8]}")
        print(f"Input: {input_file}")
        print(
            f"Total: {total}  Done: {len(state['decisions'])}  "
            f"In-flight: {len(state['in_flight'])}  Cursor: {state['cursor']}  "
            f"max_concurrent: {state['max_concurrent']}  "
            f"redispatch-queue: {len(state['to_redispatch'])}"
        )
        if state["in_flight"]:
            print("In-flight claims:")
            for tok, claim in state["in_flight"].items():
                print(
                    f"  item {claim['index'] + 1}  token {tok[:8]}  "
                    f"dispatched_at {claim['dispatched_at']}"
                )
        if state["decisions"]:
            last = state["decisions"][-1]
            print(f"Last decision (item {last['index'] + 1}): {last['decision']}")


@serialized_command
def cmd_show_decisions(input_file: Path) -> None:
    sid, sdir = resolve_session(input_file)
    with locked_state(sdir) as state:
        if not state["decisions"]:
            print("No decisions recorded.")
            return
        for d in sorted(state["decisions"], key=lambda x: x["index"]):
            print(f"[{d['index'] + 1}] {d['decision']}")


@serialized_command
def cmd_release_stale(input_file: Path, max_age_seconds: str) -> None:
    sid, sdir = resolve_session(input_file)
    threshold = parse_non_negative_float(max_age_seconds, "age-seconds")
    now = datetime.now(timezone.utc)
    released = []
    with locked_state(sdir) as state:
        new_in_flight = {}
        for token, claim in state["in_flight"].items():
            dispatched_at = datetime.fromisoformat(claim["dispatched_at"])
            age = (now - dispatched_at).total_seconds()
            if age > threshold:
                released.append((claim["index"], token, age))
                state["to_redispatch"].append(claim["index"])
            else:
                new_in_flight[token] = claim
        state["in_flight"] = new_in_flight
        if released:
            state["sequential_index"] = None
    if released:
        for idx, tok, age in released:
            print(
                f"Released claim on item {idx + 1} (token {tok[:8]}, age {age:.0f}s). "
                "Re-queued for dispatch."
            )
    else:
        print("No stale claims.")


@serialized_command
def cmd_set_max_concurrent(input_file: Path, n: str) -> None:
    sid, sdir = resolve_session(input_file)
    max_concurrent = parse_positive_int(n, "max_concurrent")
    with locked_state(sdir) as state:
        if max_concurrent < len(state["in_flight"]):
            raise SystemExit("ERROR: cap cannot be lower than the number of in-flight claims.")
        if max_concurrent != state["max_concurrent"]:
            state["sequential_index"] = None
        state["max_concurrent"] = max_concurrent
    print(f"max_concurrent set to {max_concurrent}")


@serialized_command
def cmd_restore(input_file: Path, preserve: bool = True) -> None:
    sid, sdir = resolve_session(input_file)
    source_path = sdir / "source.json"
    with locked_state(sdir) as state:
        if state["in_flight"]:
            raise SystemExit("ERROR: in-flight claims exist; stop workers and record or release claims before closing.")
        total = len(load_source_items(sdir))
        if preserve and (state["to_redispatch"]
                         or state["cursor"] != total
                         or sorted(d["index"] for d in state["decisions"]) != list(range(total))):
            raise SystemExit("ERROR: walk unfinished; complete it or explicitly abort to discard progress.")
        snapshot = dict(state, session_id=sid)
    if preserve:
        ensure_private_directory(OUTPUT_ROOT)
        out = OUTPUT_ROOT / f"{input_file.stem}.{sid}.{uuid.uuid4().hex}.walk-decisions.json"
        # Exclusive creation preserves earlier exports, including retry attempts.
        with private_file(out, os.O_WRONLY | os.O_CREAT | os.O_EXCL, "w") as stream:
            try:
                json.dump(snapshot, stream, indent=2, ensure_ascii=False)
                stream.flush()
                os.fsync(stream.fileno())
            except BaseException:
                out.unlink()
                raise
        print(f"Decisions preserved at: {out}")

    require_owned_stub(input_file, sid)
    original_stub = capture_entry(input_file)
    source_metadata = source_path.stat()
    source_mode = state.get("source_mode", stat.S_IMODE(source_metadata.st_mode))
    if type(source_mode) is not int or not 0 <= source_mode <= 0o7777:
        raise SystemExit("ERROR: stored original input mode is invalid.")
    times = (state.get("source_atime_ns", source_metadata.st_atime_ns),
             state.get("source_mtime_ns", source_metadata.st_mtime_ns))
    if any(type(value) is not int for value in times):
        raise SystemExit("ERROR: stored original input timestamps are invalid.")
    publication = InputPublication(input_file, source_path.read_bytes(), original_stub,
                                   mode=source_mode, times=times)
    try:
        require_owned_stub(input_file, sid)
        publication.publish()
        try:
            reg = load_registry()
            reg.pop(canonical_key(input_file), None)
            save_registry_checked(reg)
        except RegistryRecoveryError:
            publication.uncertain = True
            raise
        except BaseException:
            publication.rollback()
            raise
        publication.commit()
    finally:
        publication.close()

    # The source and completed evidence are safe. Never recursively erase
    # unexpected files that may have been added to a session directory.
    for name in ("source.json", "state.json", "state.lock"):
        (sdir / name).unlink(missing_ok=True)
    if any(sdir.iterdir()):
        print(f"Retained unexpected session files at: {sdir}")
    else:
        sdir.rmdir()
    print(f"Restored {input_file}.")


def cmd_abort(input_file: Path) -> None:
    cmd_restore(input_file, preserve=False)


def main() -> None:
    args = sys.argv[1:]
    if not args:
        print(__doc__)
        raise SystemExit(1)
    cmd = args[0]
    if cmd == "start":
        if len(args) < 2:
            raise SystemExit("Usage: walk.py start <file> [--max-concurrent N]")
        input_file = Path(args[1])
        max_c = DEFAULT_MAX
        rest = args[2:]
        if rest:
            if len(rest) != 2 or rest[0] != "--max-concurrent":
                raise SystemExit("Usage: walk.py start <file> [--max-concurrent N]")
            max_c = parse_positive_int(rest[1], "max_concurrent")
        cmd_start(input_file, max_c)
    elif cmd == "next" and len(args) >= 3:
        cmd_next(Path(args[1]), " ".join(args[2:]))
    elif cmd == "dispatch" and len(args) == 2:
        cmd_dispatch(Path(args[1]))
    elif cmd == "record" and len(args) >= 4:
        cmd_record(Path(args[1]), args[2], " ".join(args[3:]))
    elif cmd == "pool-status" and len(args) == 2:
        cmd_pool_status(Path(args[1]))
    elif cmd == "status" and len(args) == 2:
        cmd_status(Path(args[1]))
    elif cmd == "show-decisions" and len(args) == 2:
        cmd_show_decisions(Path(args[1]))
    elif cmd == "release-stale" and len(args) == 3:
        cmd_release_stale(Path(args[1]), args[2])
    elif cmd == "set-max-concurrent" and len(args) == 3:
        cmd_set_max_concurrent(Path(args[1]), args[2])
    elif cmd == "restore" and len(args) == 2:
        cmd_restore(Path(args[1]), preserve=True)
    elif cmd == "abort" and len(args) == 2:
        cmd_abort(Path(args[1]))
    else:
        print(__doc__)
        raise SystemExit(1)


if __name__ == "__main__":
    main()
