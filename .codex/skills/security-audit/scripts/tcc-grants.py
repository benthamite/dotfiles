#!/usr/bin/env python3
"""List standing macOS privacy grants and flag those held by generic executors.

Reads the system and per-user TCC databases read-only (SQLite `mode=ro`); it
never writes, grants or prompts. Output is one JSON object: per database, a
status with a reason, and the allowed grants for the services below. A grant
held by a generic executor (an interpreter, shell, terminal, editor or script
host) is flagged: every script that program runs inherits the permission, so
any code running as the user can use it. A grant to a path that no longer
exists is reported as stale.

Statuses: `read`; `unreadable` (the database exists but could not be opened or
queried); `missing` (nothing exists at that path); `location-unknown` (the
per-user database is neither at its legacy path nor at a container path this
helper could discover). Only `read` gives the grants; every other status means
the grants are unknown, not absent.

Per-user database location: up to macOS 26 it is
`~/Library/Application Support/com.apple.TCC/TCC.db`. macOS 27 migrates it into
a protected system container,
`/private/var/containers/Data/ProtectedSystem/<UUID>/Data/Library/Application
Support/com.apple.TCC/TCC.db`, and removes the legacy directory. The UUID is
not derivable from the user, so the helper asks the user's own `tccd` which
TCC.db it holds open (`lsof`). That container is not readable even with Full
Disk Access, so on macOS 27 the per-user database normally reports
`unreadable`; read its grants in System Settings instead.
"""

from __future__ import annotations

import argparse
import json
import os
import sqlite3
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path


SYSTEM_DB = Path("/Library/Application Support/com.apple.TCC/TCC.db")
LEGACY_USER_DB = Path.home() / "Library/Application Support/com.apple.TCC/TCC.db"
CONTAINER_PREFIX = "/private/var/containers/Data/ProtectedSystem/"
CONTAINER_SUFFIX = "/Library/Application Support/com.apple.TCC/TCC.db"
ALLOWED = 2

SERVICES = {
    "kTCCServiceAccessibility": "Accessibility",
    "kTCCServicePostEvent": "synthetic input",
    "kTCCServiceListenEvent": "Input Monitoring",
    "kTCCServiceScreenCapture": "Screen Recording",
    "kTCCServiceSystemPolicyAllFiles": "Full Disk Access",
    "kTCCServiceSystemPolicySysAdminFiles": "administrator files",
    "kTCCServiceDeveloperTool": "Developer Tools (skips Gatekeeper for child processes)",
    "kTCCServiceAppleEvents": "Automation",
    "kTCCServiceEndpointSecurityClient": "Endpoint Security",
}

# Basenames of path clients and bundle identifiers that run arbitrary code
# handed to them. Matching is exact on the basename or bundle identifier, or a
# prefix for versioned interpreters (python3.14, ruby3.3).
GENERIC_EXECUTABLES = {
    "osascript", "osacompile", "bash", "zsh", "sh", "dash", "fish", "ksh", "tcsh", "csh",
    "env", "perl", "ruby", "node", "deno", "bun", "php", "lua", "tclsh", "swift",
    "cron", "crontab", "launchctl", "sudo", "emacs", "Emacs", "vim", "nvim",
}
GENERIC_PREFIXES = ("python", "ruby", "perl", "node")
GENERIC_BUNDLES = {
    "com.apple.Terminal", "com.apple.ScriptEditor2", "com.apple.Automator",
    "com.apple.shortcuts", "com.googlecode.iterm2", "com.mitchellh.ghostty",
    "net.kovidgoyal.kitty", "com.github.wez.wezterm", "org.alacritty",
    "dev.warp.Warp-Stable", "org.gnu.Emacs", "com.microsoft.VSCode",
    "com.todesktop.230313mzl4w4u92", "com.stairways.keyboardmaestro.engine",
    "com.apple.dt.Xcode",
    # Agent apps run commands a model chooses, so their grants reach any
    # instruction that reaches the model.
    "com.openai.codex", "com.openai.sky.CUAService", "com.anthropic.claudefordesktop",
}


def is_generic_executor(client: str, client_type: int) -> bool:
    if client_type == 1:
        name = Path(client).name
        return name in GENERIC_EXECUTABLES or name.startswith(GENERIC_PREFIXES)
    return client in GENERIC_BUNDLES or client.startswith("com.apple.ScriptEditor.id.")


def tcc_paths_from_lsof(output: str) -> list[str]:
    """Return container TCC.db paths from `lsof -F n` output."""
    paths = []
    for line in output.splitlines():
        if line.startswith("n"):
            name = line[1:]
            if name.startswith(CONTAINER_PREFIX) and name.endswith(CONTAINER_SUFFIX) \
                    and name not in paths:
                paths.append(name)
    return paths


def open_user_tccd_databases() -> tuple[list[str], str | None]:
    """Ask the current user's tccd which container TCC.db it holds open.

    Returns (paths, error); error explains why the lookup itself failed.
    """
    try:
        pids = subprocess.run(["/usr/bin/pgrep", "-U", str(os.getuid()), "-x", "tccd"],
                              capture_output=True, text=True, timeout=10).stdout.split()
    except (OSError, subprocess.SubprocessError) as error:
        return [], f"pgrep failed: {error}"
    if not pids:
        return [], "no tccd process for this user"
    try:
        output = subprocess.run(["/usr/sbin/lsof", "-a", "-p", ",".join(pids), "-F", "n"],
                                capture_output=True, text=True, timeout=30).stdout
    except (OSError, subprocess.SubprocessError) as error:
        return [], f"lsof failed: {error}"
    return tcc_paths_from_lsof(output), None


def user_db_report(legacy: Path = LEGACY_USER_DB, discover=open_user_tccd_databases) -> dict:
    """Locate and read the per-user database (legacy path first, then container)."""
    if path_state(legacy) != "missing":
        return read_db(legacy)
    paths, error = discover()
    if len(paths) == 1:
        return read_db(Path(paths[0])) | {"location": "protected system container (macOS 27+)"}
    reason = (error or ("several container databases open: " + ", ".join(paths)
                        if paths else "user tccd holds no container TCC.db open"))
    return {"path": None, "status": "location-unknown",
            "reason": f"not at {legacy}; container lookup: {reason}", "grants": []}


def path_state(path: Path) -> str:
    """`present`, `missing`, or `blocked` (the lookup itself was denied)."""
    try:
        os.stat(path)
    except FileNotFoundError:
        return "missing"
    except OSError:
        return "blocked"
    return "present"


def read_db(path: Path) -> dict:
    result = {"path": str(path)}
    state = path_state(path)
    if state == "missing":
        return result | {"status": "missing", "grants": []}
    if state == "blocked":
        return result | {"status": "unreadable",
                         "reason": "path lookup denied (protected location)", "grants": []}
    try:
        connection = sqlite3.connect(f"file:{path}?mode=ro", uri=True)
        try:
            rows = connection.execute(
                "SELECT service, client, client_type, indirect_object_identifier, last_modified "
                "FROM access WHERE auth_value = ?", (ALLOWED,)).fetchall()
        finally:
            connection.close()
    except sqlite3.Error as error:
        # Without Full Disk Access the open or the first read fails with
        # "authorization denied" or "unable to open database file".
        return result | {"status": "unreadable", "reason": str(error), "grants": []}
    grants = []
    for service, client, client_type, target, modified in rows:
        if service not in SERVICES:
            continue
        grant = {
            "service": SERVICES[service],
            "client": client,
            "generic_executor": is_generic_executor(client, client_type),
            "granted": datetime.fromtimestamp(modified, timezone.utc).date().isoformat()
            if modified else None,
        }
        if service == "kTCCServiceAppleEvents" and target:
            grant["target"] = target
        if client_type == 1:
            grant["stale"] = not Path(client).exists()
        grants.append(grant)
    grants.sort(key=lambda g: (not g["generic_executor"], g["service"], g["client"]))
    return result | {"status": "read", "grants": grants}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--db", type=Path, action="append",
                        help="database to read (repeatable); default: system and user")
    args = parser.parse_args(argv)
    if args.db:
        databases = [read_db(path) for path in args.db]
    else:
        databases = [read_db(SYSTEM_DB), user_db_report()]
    report = {"databases": databases}
    json.dump(report, sys.stdout, indent=2)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
