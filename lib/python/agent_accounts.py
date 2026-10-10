"""The agent account list shared by Emacs and scripts.

emacs/agent-accounts.json declares every Claude and Codex account (name, config
home, optional pool). Emacs reads it into `agent-claude-accounts' and
`agent-codex-accounts'; scripts load it with `load_accounts'.
"""

from __future__ import annotations

import json
from pathlib import Path

ACCOUNTS_FILE = Path(__file__).resolve().parents[2] / "emacs" / "agent-accounts.json"
BACKENDS = ("claude", "codex")


class AccountError(Exception):
    """An account name that cannot be resolved to one home."""

    def __init__(self, message: str, status: int = 1):
        super().__init__(message)
        self.status = status


def load_accounts(path: Path = ACCOUNTS_FILE) -> dict[str, list[dict]]:
    """Return the validated account lists from PATH."""
    data = json.loads(path.read_text())
    if not isinstance(data, dict) or set(data) != set(BACKENDS):
        raise AccountError(f"{path}: expected exactly the keys {', '.join(BACKENDS)}")
    for backend, entries in data.items():
        if not isinstance(entries, list):
            raise AccountError(f"{path}: {backend} must be a list")
        names = set()
        for entry in entries:
            if (not isinstance(entry, dict) or not set(entry) <= {"name", "home", "pool"}
                    or not all(isinstance(entry.get(field), str) and entry[field]
                               for field in ("name", "home"))
                    or ("pool" in entry and not (isinstance(entry["pool"], str) and entry["pool"]))):
                raise AccountError(f"{path}: invalid {backend} entry {entry!r}")
            if entry["name"] in names:
                raise AccountError(f"{path}: duplicate {backend} account {entry['name']}")
            names.add(entry["name"])
        pools = {entry["pool"] for entry in entries if "pool" in entry}
        if pools & names:
            raise AccountError(f"{path}: {backend} pool names collide with account names")
    return data
