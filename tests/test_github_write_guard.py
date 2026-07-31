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


    def declared_repos(self) -> list[str]:
        registry_repo = Path.home() / "My Drive" / "Epoch" / "projects" / "automations-dashboard" / "repo"
        blob = subprocess.run(
            ["git", "-C", str(registry_repo), "show", "HEAD:data/automations.json"],
            check=True,
            capture_output=True,
            text=True,
        ).stdout
        return [
            repo.lower()
            for project in json.loads(blob)["projects"]
            for repo in project.get("repos", [])
        ]

    def allowlist_entries(self) -> list[str]:
        return [
            line.split("#", 1)[0].strip().lower()
            for line in ALLOWLIST.read_text(encoding="utf-8").splitlines()
            if line.split("#", 1)[0].strip()
        ]

    def test_declared_repo_is_allowed_without_an_allowlist_entry(self) -> None:
        # The point of reading declarations: a repo Pablo owns becomes writable
        # by declaring it on its project, with no edit to the protected gate.
        entries = self.allowlist_entries()
        candidates = [repo for repo in self.declared_repos() if repo not in entries]
        self.assertTrue(candidates, "expected a declared repo absent from the allowlist")
        command = (
            f"git push https://github.com/{candidates[0]}.git "
            "fix/post-command-handler-quit"
        )

        self.assert_both(command, expected="allow")

    def test_undeclared_repo_in_the_same_org_is_denied(self) -> None:
        declared = set(self.declared_repos())
        self.assertNotIn("epoch-research/not-a-project-of-mine", declared)
        command = (
            "git push https://github.com/epoch-research/not-a-project-of-mine.git "
            "fix/post-command-handler-quit"
        )

        self.assert_both(command, expected="deny")

    def test_working_tree_edits_to_the_registry_do_not_widen_the_gate(self) -> None:
        # A blocked agent can edit the registry file; only a commit counts. This
        # asserts the guard reads HEAD, by checking a repo present in the working
        # tree but absent from the committed blob is still refused.
        registry = Path.home() / "My Drive" / "Epoch" / "projects" / "automations-dashboard" / "repo" / "data" / "automations.json"
        working = json.loads(registry.read_text(encoding="utf-8"))
        working_repos = {
            repo.lower()
            for project in working["projects"]
            for repo in project.get("repos", [])
        }
        # Nothing to assert if the working tree matches HEAD, which is the norm.
        for repo in sorted(working_repos - set(self.declared_repos())):
            command = f"git push https://github.com/{repo}.git branch"
            self.assert_both(command, expected="deny")


if __name__ == "__main__":
    unittest.main()
