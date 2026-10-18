#!/usr/bin/env python3
"""Flag LuLu outbound-firewall rules and preferences that widen what may connect out.

Reads LuLu's rules and preferences plists read-only; it never edits rules or
talks to LuLu. Output is one JSON object: a status per file (read,
unreadable, missing), the preferences that allow traffic without a rule, the
rule count, and the flagged allow rules. An allow rule is flagged when its
program runs arbitrary code or moves arbitrary data (an interpreter, shell,
`osascript`, `curl`) and it accepts any endpoint, since any script that
program runs can then reach any host. An allow rule on a path that no longer
exists is reported as stale. Rule paths are program paths, not secrets.

An unreadable file means the rules are unknown, not absent.
"""

from __future__ import annotations

import argparse
import json
import plistlib
import sys
from pathlib import Path


LULU_DIR = Path("/Library/Objective-See/LuLu")
ALLOW = 1
# Preferences that let traffic out without any per-program rule.
PERMISSIVE_PREFERENCES = ("allowApple", "allowInstalled", "allowDNS", "passiveMode", "disabled")

# Program basenames that run code or move data handed to them, matched
# exactly or, for versioned interpreters (python3.14), by prefix. A directory
# rule (path ending "*") is matched on the package directory it covers.
GENERIC_EXECUTABLES = {
    "osascript", "osacompile", "bash", "zsh", "sh", "dash", "fish", "ksh", "tcsh", "csh",
    "env", "perl", "ruby", "node", "deno", "bun", "php", "lua", "tclsh", "swift",
    "curl", "wget", "nc", "ncat", "socat", "ssh", "scp", "sftp", "rsync", "uv", "uvx",
    "npx", "pnpm", "npm", "pip", "pip3", "emacs", "Emacs",
}
GENERIC_PREFIXES = ("python", "ruby", "perl", "node", "pypy")


def is_generic(path: str) -> bool:
    parts = [part for part in path.rstrip("*").split("/") if part]
    if not parts:
        return False
    if path.endswith("*"):
        # /opt/homebrew/Cellar/python@3.14/* covers every program in the package.
        name = parts[-1].split("@")[0]
    else:
        name = parts[-1]
    return name in GENERIC_EXECUTABLES or name.startswith(GENERIC_PREFIXES)


def any_endpoint(rule: dict) -> bool:
    return rule.get("endpointAddr") in (None, "*", "$null") and rule.get("endpointPort") in (None, "*", "$null")


def path_missing(path: str) -> bool:
    """True only when the path is known not to exist; a denied lookup is not stale."""
    try:
        Path(path.rstrip("*") if path.endswith("*") else path).lstat()
    except FileNotFoundError:
        return True
    except OSError:
        return False
    return False


def load_rules(path: Path) -> list[dict]:
    """Return LuLu's NSKeyedArchiver rules as dicts of plain values."""
    with path.open("rb") as handle:
        objects = plistlib.load(handle)["$objects"]

    def resolve(value):
        return objects[value.data] if isinstance(value, plistlib.UID) else value

    return [{key: resolve(value) for key, value in item.items()}
            for item in objects if isinstance(item, dict) and "path" in item and "action" in item]


def read_preferences(path: Path) -> dict:
    result = {"path": str(path)}
    if not path.exists():
        return result | {"status": "missing"}
    try:
        with path.open("rb") as handle:
            preferences = plistlib.load(handle)
    except (OSError, plistlib.InvalidFileException) as error:
        return result | {"status": "unreadable", "reason": str(error)}
    return result | {"status": "read",
                     "permissive": {key: preferences[key] for key in PERMISSIVE_PREFERENCES
                                    if preferences.get(key)}}


def read_rules(path: Path) -> dict:
    result = {"path": str(path)}
    if not path.exists():
        return result | {"status": "missing", "flagged": []}
    try:
        rules = load_rules(path)
    except (OSError, plistlib.InvalidFileException, KeyError, IndexError, TypeError) as error:
        return result | {"status": "unreadable", "reason": str(error), "flagged": []}
    flagged = []
    for rule in rules:
        rule_path = rule.get("path")
        if (rule.get("action") != ALLOW or not isinstance(rule_path, str)
                or rule.get("isDisabled") not in (None, "$null", False, 0)):
            continue
        entry = {"path": rule_path}
        if is_generic(rule_path) and any_endpoint(rule):
            entry["generic_any_endpoint"] = True
        if path_missing(rule_path):
            entry["stale"] = True
        if len(entry) > 1:
            flagged.append(entry)
    flagged.sort(key=lambda entry: (not entry.get("generic_any_endpoint"), entry["path"]))
    return result | {"status": "read", "rules": len(rules), "flagged": flagged}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--dir", type=Path, default=LULU_DIR,
                        help="directory holding rules.plist and preferences.plist")
    args = parser.parse_args(argv)
    report = {"preferences": read_preferences(args.dir / "preferences.plist"),
              "rules": read_rules(args.dir / "rules.plist")}
    json.dump(report, sys.stdout, indent=2, default=str)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
