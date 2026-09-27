"""Reconcile an account's Chrome plugin with its configured desktop runtime.

Only the existing account-local Chrome marketplace subtree is published here.
Codex's installer owns installation and registration; runtime trust is untouched.
"""

import fcntl
import hashlib
import json
import os
from pathlib import Path
import shutil
import stat
import subprocess
import tempfile
import uuid


class BrowserSyncError(Exception):
    """Chrome cannot safely be reconciled with the configured runtime."""


def _run(executable, home, arguments, config_args=()):
    # Never expose MCP environment values or arbitrary CLI diagnostics.
    if arguments[0] == "plugin" and any(
            flag in ("-p", "--profile") or flag.startswith("--profile=")
            for flag in config_args):
        raise BrowserSyncError("Codex's plugin commands cannot inspect --profile settings; "
                               "Chrome startup reconciliation requires an account configuration")
    try:
        result = subprocess.run(
            [str(executable), *config_args, *arguments], capture_output=True, text=True,
            env={**os.environ, "CODEX_HOME": str(home)}, timeout=120,
        )
    except subprocess.TimeoutExpired:
        raise BrowserSyncError("Codex browser reconciliation command timed out") from None
    except OSError:
        raise BrowserSyncError("Codex browser reconciliation command could not start") from None
    if result.returncode:
        raise BrowserSyncError("Codex browser reconciliation command failed: "
                               + " ".join(arguments[:2]))
    try:
        return json.loads(result.stdout)
    except ValueError:
        raise BrowserSyncError("Codex browser reconciliation returned invalid JSON") from None


def _unlinked(path):
    """Reject links in every component, including account and ancestor paths."""
    if not path.is_absolute() or ".." in path.parts:
        raise BrowserSyncError("Chrome reconciliation requires absolute local paths")
    if any(part.is_symlink() for part in (path, *path.parents)):
        raise BrowserSyncError("Chrome reconciliation refuses symlinked paths")


def _inventory(root):
    _unlinked(root)
    if not root.is_dir():
        raise BrowserSyncError("Chrome bundle directory is missing: " + str(root))
    files = {}
    for path in sorted(root.rglob("*")):
        info = path.lstat()
        if stat.S_ISDIR(info.st_mode):
            continue
        if not stat.S_ISREG(info.st_mode):
            raise BrowserSyncError("Chrome bundle contains a nonregular file")
        files[path.relative_to(root).as_posix()] = (
            hashlib.sha256(path.read_bytes()).hexdigest(), info.st_mode & 0o111)
    return files


def _signature(app):
    try:
        result = subprocess.run(
            ["/usr/bin/codesign", "--verify", "--deep", "--strict", "-R",
             '=anchor apple generic and certificate leaf[subject.OU] = "2DC432GLL2"',
             str(app)], capture_output=True, timeout=120,
        )
    except subprocess.TimeoutExpired:
        raise BrowserSyncError("Configured browser app signature verification timed out") from None
    except OSError:
        raise BrowserSyncError("Configured browser app signature verifier could not start") from None
    if result.returncode:
        raise BrowserSyncError("Configured browser app failed OpenAI signature verification")


def _inspect(home, executable, config_args=()):
    _unlinked(home)
    plugins = _run(executable, home, ["plugin", "list", "--json"], config_args)
    chrome = next((entry for entry in plugins.get("installed", [])
                   if entry.get("pluginId") == "chrome@openai-bundled"), None)
    if chrome is None or not chrome.get("enabled"):
        return None
    runtime = _run(executable, home, ["mcp", "get", "node_repl", "--json"], config_args)
    transport = runtime.get("transport", {})
    command = Path(transport.get("command", ""))
    app = next((parent for parent in command.parents if parent.suffix == ".app"), None)
    if (not runtime.get("enabled") or app is None
            or command != app / "Contents/Resources/cua_node/bin/node_repl"):
        raise BrowserSyncError("Chrome requires reconciliation by its owning desktop app")
    _unlinked(command)
    bundle = app / "Contents/Resources/plugins/openai-bundled/plugins/chrome"
    bundle_files = _inventory(bundle)
    try:
        services = json.loads(transport.get("env", {}).get("NODE_REPL_TRUSTED_SERVICES", "{}"))
        service = Path(services["browser"])
    except (ValueError, KeyError, TypeError):
        raise BrowserSyncError("Desktop browser service registration needs reconciliation") from None
    _unlinked(service)
    bundled_service = bundle / "scripts/browser-service.mjs"
    _unlinked(bundled_service)
    if not service.is_file() or not bundled_service.is_file() or service.read_bytes() != bundled_service.read_bytes():
        raise BrowserSyncError("Desktop browser runtime changed; reconcile its browser service in the app")
    expected = home / ".tmp/bundled-marketplaces/openai-bundled"
    source_info = chrome.get("marketplaceSource", {})
    if source_info.get("sourceType") != "local" or source_info.get("source") != str(expected):
        raise BrowserSyncError("Chrome marketplace is not the expected account-local source")
    source = expected / "plugins/chrome"
    _unlinked(source)
    if chrome.get("source", {}) != {"source": "local", "path": str(source)}:
        raise BrowserSyncError("Chrome plugin source does not match its account-local marketplace")
    manifest_path = expected / ".agents/plugins/marketplace.json"
    _unlinked(manifest_path)
    marketplace = json.loads(manifest_path.read_text())
    entries = [entry for entry in marketplace.get("plugins", []) if entry.get("name") == "chrome"]
    if len(entries) != 1 or entries[0].get("source") != {"source": "local", "path": "./plugins/chrome"}:
        raise BrowserSyncError("Chrome marketplace entry has an unsupported source")
    metadata = json.loads((bundle / ".codex-plugin/plugin.json").read_text())
    version = metadata.get("version")
    if metadata.get("name") != "chrome" or not isinstance(version, str) or not version or Path(version).name != version or version in (".", ".."):
        raise BrowserSyncError("Bundled Chrome metadata is invalid")
    current_version = chrome.get("version")
    if not isinstance(current_version, str) or not current_version or Path(current_version).name != current_version or current_version in (".", ".."):
        raise BrowserSyncError("Installed Chrome version is invalid")
    current = home / "plugins/cache/openai-bundled/chrome" / current_version
    _unlinked(current)
    source_matches = _inventory(source) == bundle_files
    installed_matches = current_version == version and _inventory(current) == bundle_files
    return dict(app=app, bundle=bundle, files=bundle_files, source=source,
                source_matches=source_matches, installed_matches=installed_matches,
                current=current, current_version=current_version, version=version)


def _restore_previous_cache(current, recovery, expected, work):
    """Preserve imports by running sessions when Codex removes an older version."""
    _unlinked(current)
    if current.exists() and _inventory(current) == expected:
        return
    replacement = work / ("restore-cache-" + uuid.uuid4().hex)
    try:
        shutil.copytree(recovery, replacement)
        if _inventory(replacement) != expected:
            raise BrowserSyncError("Previous Chrome cache recovery failed its integrity check")
        if current.exists():
            # Keep unexpected installer output for inspection; never discard it.
            current.rename(work / ("changed-cache-" + uuid.uuid4().hex))
        current.parent.mkdir(parents=True, exist_ok=True)
        replacement.rename(current)
    finally:
        if replacement.exists():
            shutil.rmtree(replacement)


def synchronize(codex_home: Path, executable: Path, config_args=()):
    """Return skipped/current/updated, or fail before launching an unsafe repair.

    Retain previous marketplace subtrees and installed versions as recovery
    copies. Restore older cache versions removed by the standard installer so
    already-running sessions can continue importing their original paths.
    """
    home = Path(codex_home)
    state = _inspect(home, executable, config_args)
    if state is None:
        return "skipped"
    if state["source_matches"] and state["installed_matches"]:
        return "current"
    work = home / ".tmp/browser-sync"
    _unlinked(work)
    work.mkdir(parents=True, exist_ok=True, mode=0o700)
    lock_path = work / "lock"
    _unlinked(lock_path)
    with lock_path.open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        state = _inspect(home, executable, config_args)
        if state is None:
            return "skipped"
        if state["source_matches"] and state["installed_matches"]:
            return "current"
        _signature(state["app"])
        stage = Path(tempfile.mkdtemp(prefix="stage-", dir=work))
        replacement = stage / "chrome"
        previous = work / ("previous-chrome-" + uuid.uuid4().hex)
        cache_recoveries = []
        moved = False
        published = False
        try:
            shutil.copytree(state["bundle"], replacement)
            if _inventory(replacement) != state["files"] or _inventory(state["bundle"]) != state["files"]:
                raise BrowserSyncError("Desktop app changed during Chrome reconciliation; retry after its update")
            if not state["installed_matches"]:
                # The installer can remove every older version, including ones
                # retained for sessions launched before the currently active one.
                for cached in sorted(state["current"].parent.iterdir()):
                    if cached.name == "latest" and cached.is_symlink():
                        continue
                    _unlinked(cached)
                    if not cached.is_dir():
                        raise BrowserSyncError("Chrome cache contains an unexpected non-directory entry")
                    if cached.name == state["version"]:
                        continue
                    cache_files = _inventory(cached)
                    cache_recovery = work / ("previous-cache-" + uuid.uuid4().hex)
                    shutil.copytree(cached, cache_recovery)
                    if (_inventory(cache_recovery) != cache_files
                            or _inventory(cached) != cache_files):
                        raise BrowserSyncError("Installed Chrome changed while preserving its prior version")
                    cache_recoveries.append((cached, cache_recovery, cache_files))
            if not state["source_matches"]:
                state["source"].rename(previous)
                moved = True
                replacement.rename(state["source"])
                published = True
            if not state["installed_matches"]:
                _run(executable, home, ["plugin", "add", "chrome@openai-bundled", "--json"], config_args)
            verified = _inspect(home, executable, config_args)
            if verified is None or not verified["source_matches"] or not verified["installed_matches"]:
                raise BrowserSyncError("Chrome installer did not select the desktop app's matching bundle")
            return "updated"
        except BaseException:
            if published:
                state["source"].rename(stage / "failed-publication")
            if moved:
                previous.rename(state["source"])
            raise
        finally:
            try:
                for cached, cache_recovery, cache_files in cache_recoveries:
                    _restore_previous_cache(cached, cache_recovery, cache_files, work)
            finally:
                # Only our unpublished copied artifacts are disposable.
                shutil.rmtree(stage)
