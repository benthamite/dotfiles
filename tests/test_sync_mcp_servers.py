from __future__ import annotations

import json
import os
import subprocess
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "claude" / "bin" / "sync-mcp-servers.sh"
ACCOUNT_DIRS = (
    ".claude-epoch",
    ".claude-personal",
    ".claude-tlon",
    ".claude-trajectory",
)


def write_json(path: Path, payload: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload), encoding="utf-8")


def test_sync_deep_merges_each_active_account_config(tmp_path: Path) -> None:
    write_json(
        tmp_path / ".claude.json",
        {
            "mcpServers": {
                "shared": {
                    "command": "canonical-command",
                    "args": ["--canonical"],
                    "env": {
                        "CANONICAL_ONLY": "canonical-value",
                        "SHARED_KEY": "canonical-value",
                    },
                    "settings": {
                        "canonicalOnly": True,
                        "sharedSetting": "canonical-value",
                    },
                },
                "canonical-only": {"command": "new-command"},
            }
        },
    )

    for account_dir in ACCOUNT_DIRS:
        write_json(
            tmp_path / account_dir / ".claude.json",
            {
                "accountMetadata": account_dir,
                "mcpServers": {
                    "shared": {
                        "command": "old-command",
                        "args": ["--old"],
                        "accountOnlyField": "keep-me",
                        "env": {
                            "ACCOUNT_SECRET": f"{account_dir}-secret",
                            "SHARED_KEY": f"{account_dir}-override",
                        },
                        "settings": {
                            "accountOnly": True,
                            "sharedSetting": "old-value",
                        },
                    },
                    "account-only": {
                        "command": "account-command",
                        "env": {"ACCOUNT_ONLY_SECRET": f"{account_dir}-secret"},
                    },
                },
            },
        )

    result = subprocess.run(
        [str(SCRIPT)],
        env={**os.environ, "HOME": str(tmp_path)},
        text=True,
        capture_output=True,
    )

    assert result.returncode == 0, result.stderr
    for account_dir in ACCOUNT_DIRS:
        payload = json.loads(
            (tmp_path / account_dir / ".claude.json").read_text(encoding="utf-8")
        )
        assert payload["accountMetadata"] == account_dir
        assert payload["mcpServers"] == {
            "shared": {
                "command": "canonical-command",
                "args": ["--canonical"],
                "accountOnlyField": "keep-me",
                "env": {
                    "CANONICAL_ONLY": "canonical-value",
                    "ACCOUNT_SECRET": f"{account_dir}-secret",
                    "SHARED_KEY": f"{account_dir}-override",
                },
                "settings": {
                    "accountOnly": True,
                    "canonicalOnly": True,
                    "sharedSetting": "canonical-value",
                },
            },
            "account-only": {
                "command": "account-command",
                "env": {"ACCOUNT_ONLY_SECRET": f"{account_dir}-secret"},
            },
            "canonical-only": {"command": "new-command"},
        }
