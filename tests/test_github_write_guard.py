"""Behavioral parity tests for the Claude and Codex GitHub write guards."""

from __future__ import annotations

import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
GUARDS = {
    "claude": ROOT / "claude" / "hooks" / "block-github-write-command.sh",
    "codex": ROOT / "codex" / "hooks" / "block-github-write-command.sh",
}
ALLOWLIST = ROOT / "agents" / "github-write-allowlist.txt"


def payload(tool: str, command: str) -> str:
    if tool == "claude":
        data = {"tool_name": "Bash", "tool_input": {"command": command}}
    else:
        data = {
            "tool_name": "functions.exec_command",
            "tool_input": {"cmd": command},
        }
    return json.dumps(data)


def decision(result: subprocess.CompletedProcess[str]) -> str:
    if not result.stdout.strip():
        return "allow"
    output = json.loads(result.stdout)
    return output["hookSpecificOutput"]["permissionDecision"]


class GitHubWriteGuardParityTests(unittest.TestCase):
    def setUp(self) -> None:
        self.tempdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tempdir.cleanup)
        self.temp_path = Path(self.tempdir.name)
        self.gh_log = self.temp_path / "gh-args"
        fake_gh = self.temp_path / "gh"
        fake_gh.write_text(
            """#!/usr/bin/env bash
set -eu
printf 'called\\n' >> "$FAKE_GH_LOG"
exit 1
""",
            encoding="utf-8",
        )
        os.chmod(fake_gh, 0o755)

    def run_guard(
        self,
        tool: str,
        command: str,
    ) -> subprocess.CompletedProcess[str]:
        if self.gh_log.exists():
            self.gh_log.unlink()
        env = os.environ.copy()
        env["PATH"] = os.pathsep.join((str(self.temp_path), env["PATH"]))
        env["FAKE_GH_LOG"] = str(self.gh_log)
        return subprocess.run(
            ["bash", str(GUARDS[tool])],
            input=payload(tool, command),
            capture_output=True,
            text=True,
            check=True,
            cwd=ROOT,
            env=env,
        )

    def assert_both(
        self,
        command: str,
        *,
        expected: str,
    ) -> None:
        for tool in GUARDS:
            with self.subTest(tool=tool):
                result = self.run_guard(tool, command)
                gh_log = (
                    self.gh_log.read_text(encoding="utf-8")
                    if self.gh_log.exists()
                    else ""
                )
                self.assertEqual(
                    (decision(result), gh_log),
                    (expected, ""),
                )

    def test_account_wildcard_allows_repo(self) -> None:
        command = (
            "git push https://github.com/benthamite/yasnippet.git "
            "fix/post-command-handler-quit"
        )
        self.assert_both(command, expected="allow")

    def test_other_account_is_denied(self) -> None:
        command = (
            "git push https://github.com/example/unowned.git "
            "fix/post-command-handler-quit"
        )
        self.assert_both(command, expected="deny")

    def test_exact_repo_entry_remains_allowed(self) -> None:
        entries = [
            line.split("#", 1)[0].strip()
            for line in ALLOWLIST.read_text(encoding="utf-8").splitlines()
            if line.split("#", 1)[0].strip()
        ]
        exact_entries = [entry for entry in entries if "*" not in entry]
        self.assertTrue(exact_entries)
        command = (
            f"git push https://github.com/{exact_entries[0]}.git "
            "fix/post-command-handler-quit"
        )

        self.assert_both(command, expected="allow")

    def test_wildcard_does_not_match_similar_owner(self) -> None:
        command = (
            "git push https://github.com/benthamitee/yasnippet.git "
            "fix/post-command-handler-quit"
        )
        self.assert_both(command, expected="deny")


if __name__ == "__main__":
    unittest.main()
