"""Validate extraction grants, bootstrap locked tools, then enter the VM isolator.

The cache is trusted local state, not an authenticated package store. npm ci
checks the committed lockfile's download integrity; cache reuse does not prove
that previously installed module bytes have not subsequently been changed.
"""

import ctypes
import fcntl
import hashlib
import json
import os
from pathlib import Path
import runpy
import shutil
import signal
import stat
import subprocess
import sys
import tempfile


SKILL = Path(__file__).resolve().parent.parent
ISOLATOR = SKILL.parents[3] / "bin/untrusted-run"
CLI = Path("node_modules/@electron/asar/bin/asar.mjs")
BOOTSTRAP_TIMEOUT = 120


def plain_path(value):
    """Keep literal caller-relative paths, rejecting symlink ancestors and '..'."""
    path = Path(value).absolute()
    for alias, canonical in (("/tmp", "/private/tmp"), ("/var", "/private/var")):
        if str(path) == alias or str(path).startswith(alias + "/"):
            path = Path(canonical + str(path)[len(alias):])
    if any(character in str(path) for character in (",", "\n", "\r")):
        raise ValueError("Unsupported character in host path")
    cursor = Path("/")
    for component in path.parts[1:]:
        if component == "..":
            raise ValueError("Paths must not contain parent traversal")
        cursor /= component
        if cursor.is_symlink():
            raise ValueError("Path roots and their parents must not be symlinks")
        if cursor != path and cursor.exists() and not cursor.is_dir():
            raise ValueError("Path parents must be ordinary directories")
    return path


def outside_drive(path):
    drive = Path.home() / "My Drive"
    ancestors = (path, *path.parents)
    if drive in ancestors or (drive.exists() and any(parent.exists() and parent.samefile(drive) for parent in ancestors)):
        raise ValueError("Extraction destinations and caches must be outside Google Drive")


def private_directory(path):
    info = path.lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise ValueError(f"Cache directories must be owned by this user and private (0700): {path}")


def input_identity(path):
    if path is None:
        return None
    info = path.lstat()
    return (str(path), info.st_dev, info.st_ino, info.st_mode, info.st_size, info.st_mtime_ns, info.st_ctime_ns)


def preflight(archive_value, destination_value):
    if sys.platform != "darwin" or not ISOLATOR.is_file() or not os.access(ISOLATOR, os.X_OK):
        raise ValueError("The required macOS Docker VM untrusted-run isolator is unavailable")
    # Import only the existing path validator: runpy's default name is not
    # '__main__', so no Docker control command or CLI entrypoint runs here.
    safe_input = runpy.run_path(str(ISOLATOR))["safe_host_path"]
    archive = safe_input(str(plain_path(archive_value)))
    if not archive.is_file():
        raise ValueError("The archive must be an ordinary file")
    unpacked = plain_path(str(archive) + ".unpacked")
    if unpacked.exists():
        unpacked = safe_input(str(unpacked))
        if not unpacked.is_dir():
            raise ValueError("The unpacked-archive root must be an ordinary directory")
    else:
        unpacked = None
    destination = plain_path(destination_value)
    if destination.exists() or not destination.parent.is_dir():
        raise ValueError("Destination must not exist and its parent must already be a directory")
    outside_drive(destination)
    cache = plain_path(os.environ.get("AUDIT_MAC_APP_CACHE_DIR") or
                       str(Path(os.environ.get("XDG_CACHE_HOME") or Path.home() / ".cache") /
                           "audit-mac-app/asar"))
    outside_drive(cache)
    if cache.exists():
        private_directory(cache)
        safe_input(str(cache))
    else:
        # Validate protected path components even before the directory exists.
        protected = {".ssh", ".gnupg", ".password-store", ".docker", ".codex", ".claude",
                     ".git", ".config", "keychains", ".env", ".netrc", ".npmrc", ".pypirc",
                     ".zshenv-secrets"}
        if any(part.lower() in protected or part.startswith(".env.") for part in cache.parts):
            raise ValueError("Credential and agent-control paths are not valid caches")
    npm_cache = plain_path(str(cache / "npm-cache"))
    if npm_cache.exists():
        private_directory(npm_cache)
    return archive, unpacked, destination, cache, safe_input


def select_node():
    explicit = os.environ.get("AUDIT_MAC_APP_NODE")
    candidates = [explicit] if explicit else [shutil.which("node"), "/opt/homebrew/bin/node", "/usr/local/bin/node"]
    if not explicit:
        nvm = Path(os.environ.get("NVM_DIR") or Path.home() / ".nvm")
        candidates += sorted(map(str, (nvm / "versions/node").glob("*/bin/node")), reverse=True)
    for candidate in candidates:
        if not candidate or not os.access(candidate, os.X_OK):
            continue
        try:
            check = subprocess.run([candidate, "-e", "const [a,b]=process.versions.node.split('.').map(Number);"
                                    "process.exit(a>22||(a===22&&b>=12)?0:1)"],
                                   stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
                                   stderr=subprocess.DEVNULL, timeout=5)
        except (OSError, subprocess.TimeoutExpired):
            continue
        if check.returncode == 0:
            return Path(candidate).absolute()
    raise ValueError("Electron extraction requires Node 22.12 or newer; check AUDIT_MAC_APP_NODE")


def valid_install(install):
    private_directory(install)
    for name in ("package.json", "package-lock.json"):
        path = plain_path(str(install / name))
        if not path.is_file() or path.read_bytes() != (SKILL / name).read_bytes():
            raise ValueError(f"Extractor cache has missing or mismatched {name}: {install}")
    executable = plain_path(str(install / CLI))
    metadata = plain_path(str(install / "node_modules/@electron/asar/package.json"))
    if not executable.is_file() or not metadata.is_file():
        raise ValueError(f"Extractor cache is incomplete: {install}")
    package = json.loads(metadata.read_text())
    if not isinstance(package, dict) or package.get("version") != "4.2.1":
        raise ValueError(f"Extractor cache is incomplete or has the wrong ASAR version: {install}")


def publish(staging, install):
    # macOS SDK sys/stdio.h: RENAME_EXCL=4; fail closed if unsupported. Unlike
    # mv, this never nests staging under an existing concurrent destination.
    rename = ctypes.CDLL(None, use_errno=True).renamex_np
    rename.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_uint]
    rename.restype = ctypes.c_int
    if rename(os.fsencode(staging), os.fsencode(install), 4):
        error = ctypes.get_errno()
        raise OSError(error, os.strerror(error), str(install))


def bootstrap(node, staging, cache):
    npm = node.parent / "npm"
    if not os.access(npm, os.X_OK):
        raise ValueError(f"Selected Node has no sibling npm executable: {node}")
    for name in ("package.json", "package-lock.json"):
        shutil.copyfile(SKILL / name, staging / name)
    environment = dict(os.environ, PATH=str(node.parent) + os.pathsep + os.environ.get("PATH", ""))
    # An inherited npm cache setting must not redirect installation into Drive.
    npm_cache = plain_path(str(cache / "npm-cache"))
    if npm_cache.exists():
        private_directory(npm_cache)
    print("Installing the locked Electron ASAR extractor...", file=sys.stderr)
    process = subprocess.Popen([str(npm), "ci", "--ignore-scripts", "--no-audit", "--no-fund",
                                "--cache", str(npm_cache)], cwd=staging, env=environment,
                               stdin=subprocess.DEVNULL, start_new_session=True)
    try:
        status = process.wait(timeout=BOOTSTRAP_TIMEOUT)
        if status:
            raise ValueError(f"Locked extractor installation failed (npm status {status})")
    finally:
        # This process group belongs solely to the bootstrap we launched.
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        process.wait(timeout=5)


def run(archive_value, destination_value):
    archive, unpacked, destination, cache, safe_input = preflight(archive_value, destination_value)
    input_before = (input_identity(archive), input_identity(unpacked))
    lock_hash = hashlib.sha256((SKILL / "package-lock.json").read_bytes()).hexdigest()
    install = plain_path(str(cache / lock_hash))
    os.umask(0o077)
    cache.mkdir(mode=0o700, parents=True, exist_ok=True)
    private_directory(cache)
    lock_path = cache / ".install.lock"
    descriptor = os.open(lock_path, os.O_RDWR | os.O_CREAT | os.O_NOFOLLOW | os.O_NONBLOCK, 0o600)
    with os.fdopen(descriptor, "r+") as lock:
        info = os.fstat(lock.fileno())
        if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077 or info.st_nlink != 1:
            raise ValueError("Extractor cache lock must be a private, owned regular file")
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            raise ValueError("Extractor cache installation is busy; no files were replaced") from None
        if install.exists():
            valid_install(install)
        else:
            trash = shutil.which("trash")
            if not trash:
                raise ValueError("A trash executable is required to clean up owned staging directories")
            node = select_node()
            staging = Path(tempfile.mkdtemp(prefix=".install.", dir=cache))
            try:
                bootstrap(node, staging, cache)
                valid_install(staging)
                publish(staging, install)
                staging = None
            finally:
                if staging is not None:
                    try:
                        cleaned = subprocess.run([trash, str(staging)], stdin=subprocess.DEVNULL, timeout=30)
                        if cleaned.returncode or staging.exists():
                            raise ValueError("trash did not remove the staging path")
                    except (OSError, ValueError, subprocess.SubprocessError) as error:
                        print(f"extract-asar: staging cleanup unconfirmed; retained path {staging}: {error}", file=sys.stderr)
        safe_input(str(install))
    # Repeat input/output checks after a potentially slow install. exec gives
    # the isolator direct ownership of signals and container cleanup.
    checked_archive, checked_unpacked, _, _, _ = preflight(str(archive), str(destination))
    if (input_identity(checked_archive), input_identity(checked_unpacked)) != input_before:
        raise ValueError("Archive or unpacked root changed during preparation; no extraction was started")
    command = [str(ISOLATOR), "--workspace", str(destination), "--read", f"dependencies={install}",
               "--read", f"archive.asar={archive}"]
    if unpacked is not None:
        command += ["--read", f"archive.asar.unpacked={unpacked}"]
    command += ["--", "node", "/inputs/dependencies/" + str(CLI), "extract", "/inputs/archive.asar", "/workspace"]
    os.execv(str(ISOLATOR), command)


def main():
    if len(sys.argv) != 3:
        print("Usage: extract-asar.sh ARCHIVE DESTINATION", file=sys.stderr)
        return 2
    def interrupted(signum, frame):
        raise KeyboardInterrupt
    signal.signal(signal.SIGTERM, interrupted)
    signal.signal(signal.SIGHUP, interrupted)
    try:
        run(*sys.argv[1:])
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        print(f"extract-asar: refused: {error}", file=sys.stderr)
        return 2
    except KeyboardInterrupt:
        print("extract-asar: interrupted", file=sys.stderr)
        return 130


if __name__ == "__main__":
    sys.exit(main())
