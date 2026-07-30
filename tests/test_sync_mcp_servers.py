"""Fixture test for claude/bin/sync-mcp-servers.sh.

Proves the documented merge contract (claude/context/mcp-servers.md):
- canonical ~/.claude.json owns the server set and configuration,
- per-account env entries survive a sync and win on key conflicts,
- servers removed from the canonical file are removed from accounts,
- unrelated keys in account files are untouched.
"""

from __future__ import annotations

import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path("/Users/pablostafforini/My Drive/dotfiles/claude/bin/sync-mcp-servers.sh")


class SyncMcpServersTests(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.home = Path(temp.name)

    def write_json(self, rel: str, data: dict) -> Path:
        path = self.home / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(data, indent=2) + "\n")
        return path

    def run_sync(self) -> subprocess.CompletedProcess:
        env = dict(os.environ, HOME=str(self.home))
        return subprocess.run(
            ["bash", str(SCRIPT)],
            env=env,
            capture_output=True,
            text=True,
            check=True,
        )

    def test_account_env_survives_and_wins(self):
        self.write_json(
            ".claude.json",
            {
                "mcpServers": {
                    "example": {
                        "command": "example-server",
                        "args": ["--serve"],
                        "env": {"SHARED_FLAG": "on", "ACCOUNT_TOKEN": ""},
                    }
                }
            },
        )
        account = self.write_json(
            ".claude-epoch/.claude.json",
            {
                "otherSetting": True,
                "mcpServers": {
                    "example": {
                        "command": "stale-command",
                        "env": {"ACCOUNT_TOKEN": "op://Automations/Example/credential"},
                    },
                    "removed-server": {"command": "gone"},
                },
            },
        )

        self.run_sync()

        data = json.loads(account.read_text())
        server = data["mcpServers"]["example"]
        self.assertEqual(server["command"], "example-server")
        self.assertEqual(server["args"], ["--serve"])
        self.assertEqual(
            server["env"],
            {"SHARED_FLAG": "on", "ACCOUNT_TOKEN": "op://Automations/Example/credential"},
        )
        self.assertNotIn("removed-server", data["mcpServers"])
        self.assertTrue(data["otherSetting"])

    def test_empty_canonical_map_syncs_cleanly(self):
        self.write_json(".claude.json", {"mcpServers": {}})
        account = self.write_json(
            ".claude-personal/.claude.json",
            {"mcpServers": {"leftover": {"command": "x"}}},
        )

        self.run_sync()

        self.assertEqual(json.loads(account.read_text())["mcpServers"], {})

    def test_missing_account_dirs_are_skipped(self):
        self.write_json(".claude.json", {"mcpServers": {}})

        result = self.run_sync()

        self.assertIn("Skipping", result.stdout)
        self.assertIn("Done.", result.stdout)

    def test_all_active_account_profiles_are_targeted(self):
        self.write_json(
            ".claude.json",
            {"mcpServers": {"example": {"command": "example-server"}}},
        )
        accounts = ["epoch", "personal", "tlon", "trajectory"]
        for account in accounts:
            (self.home / f".claude-{account}").mkdir()

        self.run_sync()

        for account in accounts:
            path = self.home / f".claude-{account}" / ".claude.json"
            self.assertTrue(path.is_file(), f"missing sync target: {account}")
            self.assertIn("example", json.loads(path.read_text())["mcpServers"])


if __name__ == "__main__":
    unittest.main()
