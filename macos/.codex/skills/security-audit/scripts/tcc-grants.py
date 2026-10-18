#!/usr/bin/env python3
"""List standing macOS privacy grants and flag those held by generic executors.

Reads the system and per-user TCC databases read-only (SQLite `mode=ro`); it
never writes, grants or prompts. Output is one JSON object: per database, a
status (read, unreadable, missing) with a reason, and the allowed grants for
the services below. A grant held by a generic executor (an interpreter, shell,
terminal, editor or script host) is flagged: every script that program runs
inherits the permission, so any code running as the user can use it. A grant
to a path that no longer exists is reported as stale.

An unreadable database means the grants are unknown, not absent.
"""

from __future__ import annotations

import argparse
import json
import sqlite3
import sys
from datetime import datetime, timezone
from pathlib import Path


SYSTEM_DB = Path("/Library/Application Support/com.apple.TCC/TCC.db")
USER_DB = Path.home() / "Library/Application Support/com.apple.TCC/TCC.db"
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
}


def is_generic_executor(client: str, client_type: int) -> bool:
    if client_type == 1:
        name = Path(client).name
        return name in GENERIC_EXECUTABLES or name.startswith(GENERIC_PREFIXES)
    return client in GENERIC_BUNDLES or client.startswith("com.apple.ScriptEditor.id.")


def read_db(path: Path) -> dict:
    result = {"path": str(path)}
    if not path.exists():
        return result | {"status": "missing", "grants": []}
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
    databases = args.db or [SYSTEM_DB, USER_DB]
    report = {"databases": [read_db(path) for path in databases]}
    json.dump(report, sys.stdout, indent=2)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
