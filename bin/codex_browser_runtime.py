"""Retain browser runtime bytes verified by the caller's app-signature check.

Published snapshots are read-only and never garbage-collected here: a running
Emacs app-server may need to start another helper long after an app update.
"""

import fcntl
import hashlib
import json
import os
from pathlib import Path
import shutil
import stat
import tempfile


class RuntimeSnapshotError(Exception):
    """A runtime could not be retained without losing integrity."""


def _stamp(info):
    return (info.st_dev, info.st_ino, info.st_mode, info.st_size,
            info.st_mtime_ns, info.st_ctime_ns)


def _internal_link(path, root):
    """Reject every hop outside the tree, even links that eventually return."""
    pending = list(path.relative_to(root).parts)
    position = root
    hops = 0
    while pending:
        part = pending.pop(0)
        if part in ('', '.'):
            continue
        if part == '..':
            if position == root:
                raise RuntimeSnapshotError('Runtime symlink escapes its tree')
            position = position.parent
            continue
        position = position / part
        if position.is_symlink():
            hops += 1
            target = os.readlink(position)
            if hops > 40 or os.path.isabs(target):
                raise RuntimeSnapshotError('Unsafe runtime symlink chain')
            position = position.parent
            pending = list(Path(target).parts) + pending
    if not position.exists():
        raise RuntimeSnapshotError('Broken runtime symlink')


def inventory(root):
    """Hash all entries without following links; reject unstable or unsafe input.

    Write bits are deliberately normalized away: publication removes them.
    Read/execute bits, including directories, remain part of the identity.
    """
    root = Path(root)
    if root.is_symlink() or not root.is_dir():
        raise RuntimeSnapshotError('Runtime root must be a real directory')
    canonical = root.resolve(strict=True)
    result = {}

    def walk(directory):
        before_dir = directory.lstat()
        names = sorted(os.listdir(directory))
        for name in names:
            path = directory / name
            relative = path.relative_to(root).as_posix()
            before = path.lstat()
            mode = stat.S_IMODE(before.st_mode)
            if mode & 0o7000:
                raise RuntimeSnapshotError('Unsupported special permission: ' + relative)
            if stat.S_ISLNK(before.st_mode):
                target = os.readlink(path)
                if os.path.isabs(target):
                    raise RuntimeSnapshotError('Absolute runtime symlink: ' + relative)
                try:
                    _internal_link(path, root)
                    resolved = path.resolve(strict=True)
                    resolved.relative_to(canonical)
                except (ValueError, OSError, RuntimeError) as error:
                    raise RuntimeSnapshotError('Escaping or broken runtime symlink: ' + relative) from error
                result[relative] = ['link', target]
            elif stat.S_ISDIR(before.st_mode):
                result[relative] = ['dir', mode & ~0o222]
                walk(path)
            elif stat.S_ISREG(before.st_mode):
                digest = hashlib.sha256()
                fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW)
                with os.fdopen(fd, 'rb') as stream:
                    if _stamp(os.fstat(stream.fileno())) != _stamp(before):
                        raise RuntimeSnapshotError('Runtime changed while opening: ' + relative)
                    for block in iter(lambda: stream.read(1024 * 1024), b''):
                        digest.update(block)
                    if _stamp(os.fstat(stream.fileno())) != _stamp(before):
                        raise RuntimeSnapshotError('Runtime changed while reading: ' + relative)
                result[relative] = ['file', mode & ~0o222, digest.hexdigest()]
            else:
                raise RuntimeSnapshotError('Unsupported runtime entry: ' + relative)
            if _stamp(path.lstat()) != _stamp(before):
                raise RuntimeSnapshotError('Runtime entry changed: ' + relative)
        after_dir = directory.lstat()
        if _stamp(after_dir) != _stamp(before_dir):
            raise RuntimeSnapshotError('Runtime directory changed during inventory: ' + str(directory)
                                       + ' (' + repr(_stamp(before_dir)) + ' -> '
                                       + repr(_stamp(after_dir)) + ')')

    result['.'] = ['dir', stat.S_IMODE(root.stat().st_mode) & ~0o222]
    walk(root)
    return result


def _remove_stage(stage):
    """Remove only the unpublished directory created by this invocation."""
    stage.chmod(0o700)
    for directory, subdirs, _ in os.walk(stage, followlinks=False):
        for name in subdirs:
            child = Path(directory) / name
            if not child.is_symlink():
                child.chmod(0o700)
    shutil.rmtree(stage)


def snapshot_tree(source: Path, store: Path) -> Path:
    """Atomically retain a complete runtime; caller must verify its owning app."""
    source, store = Path(source), Path(store)
    expected = inventory(source)
    identity = hashlib.sha256(json.dumps(expected, sort_keys=True).encode()).hexdigest()
    store.mkdir(mode=0o700, parents=True, exist_ok=True)
    if store.is_symlink():
        raise RuntimeSnapshotError('Runtime store must not be a symlink')
    destination = store / identity
    lock_fd = os.open(store / '.publish.lock', os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
    with os.fdopen(lock_fd, 'a') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        if destination.exists() or destination.is_symlink():
            if destination.is_symlink() or inventory(destination / 'assets') != expected:
                raise RuntimeSnapshotError('Retained browser runtime failed integrity check')
            if inventory(source) != expected:
                raise RuntimeSnapshotError('Browser runtime changed during snapshot reuse')
            return destination / 'assets'
        stage = Path(tempfile.mkdtemp(prefix='.stage-', dir=store))
        try:
            copied = stage / 'assets'
            shutil.copytree(source, copied, symlinks=True)
            if inventory(copied) != expected or inventory(source) != expected:
                raise RuntimeSnapshotError('Browser runtime changed during snapshot creation')
            for relative, entry in expected.items():
                if entry[0] != 'link':
                    (copied / relative).chmod(entry[1])
            stage.chmod(0o500)
            stage.rename(destination)
            return destination / 'assets'
        finally:
            if stage.exists():
                _remove_stage(stage)


def snapshot_runtime(source: Path, store: Path) -> Path:
    """Retain a complete cua_node tree after checking its essential layout."""
    for relative in ('bin/node', 'bin/node_repl'):
        path = Path(source) / relative
        if path.is_symlink() or not path.is_file() or not path.stat().st_mode & 0o111:
            raise RuntimeSnapshotError('Missing executable runtime file: ' + relative)
    if not (Path(source) / 'lib/node_modules').is_dir():
        raise RuntimeSnapshotError('Missing runtime modules directory')
    return snapshot_tree(source, store)


def _app_cli_file(path):
    """Read a stable regular app executable without following symlinks."""
    before = path.lstat()
    if not stat.S_ISREG(before.st_mode) or not before.st_mode & 0o111:
        raise RuntimeSnapshotError('Missing regular app CLI executable: ' + path.name)
    digest = hashlib.sha256()
    fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW)
    with os.fdopen(fd, 'rb') as stream:
        if _stamp(os.fstat(stream.fileno())) != _stamp(before):
            raise RuntimeSnapshotError('App CLI changed while opening: ' + path.name)
        for block in iter(lambda: stream.read(1024 * 1024), b''):
            digest.update(block)
        if _stamp(os.fstat(stream.fileno())) != _stamp(before):
            raise RuntimeSnapshotError('App CLI changed while reading: ' + path.name)
    if _stamp(path.lstat()) != _stamp(before):
        raise RuntimeSnapshotError('App CLI changed during inspection: ' + path.name)
    return [digest.hexdigest(), stat.S_IMODE(before.st_mode) & ~0o222], _stamp(before)


def snapshot_app_cli(resources: Path, store: Path) -> Path:
    """Retain the signed app's CLI and sibling helper as one immutable pair.

    The caller verifies the owning app signature. Only these two app resources
    are included; unrelated resources cannot become executable dependencies.
    """
    resources, store = Path(resources), Path(store)
    if resources.is_symlink() or not resources.is_dir():
        raise RuntimeSnapshotError('App resources must be a real directory')
    names = ('codex', 'codex-code-mode-host')
    expected = {name: _app_cli_file(resources / name) for name in names}
    scratch = Path(tempfile.mkdtemp(prefix='codex-app-cli-'))
    try:
        for name in names:
            shutil.copy2(resources / name, scratch / name, follow_symlinks=False)
            if _app_cli_file(scratch / name)[0] != expected[name][0]:
                raise RuntimeSnapshotError('App CLI copy failed integrity check: ' + name)
        if any(_app_cli_file(resources / name) != expected[name] for name in names):
            raise RuntimeSnapshotError('App CLI changed during snapshot creation')
        retained = snapshot_tree(scratch, store)
        if any(_app_cli_file(resources / name) != expected[name] for name in names):
            raise RuntimeSnapshotError('App CLI changed during snapshot publication')
        return retained
    finally:
        _remove_stage(scratch)
