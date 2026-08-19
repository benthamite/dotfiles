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
    """Repo-level allowlisting, for both guards.

    These tests prove that every GitHub write is decided by the repository
    allowlist, while read-only inspection remains available everywhere.
    """

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

    def run_codex_exec_guard(self, source: str) -> subprocess.CompletedProcess[str]:
        data = {"tool_name": "functions.exec", "tool_input": {"input": source}}
        return subprocess.run(
            ["bash", str(GUARDS["codex"])],
            input=json.dumps(data),
            capture_output=True,
            text=True,
            check=True,
            cwd=ROOT,
            env=os.environ.copy(),
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

    def test_codex_functions_exec_denies_nested_unowned_push(self) -> None:
        source = (
            'await tools.exec_command({cmd:"git push '
            'https://github.com/example/unowned.git HEAD:topic"});'
        )
        self.assertEqual(decision(self.run_codex_exec_guard(source)), "deny")

    def test_codex_functions_exec_denies_nested_unowned_contributions(self) -> None:
        for command in (
            "gh pr create --repo example/unowned --title T --body B",
            "gh pr comment --repo example/unowned 1 --body B",
            "gh issue create --repo example/unowned --title T --body B",
            "gh issue comment --repo example/unowned 1 --body B",
        ):
            with self.subTest(command=command):
                source = f"await tools.exec_command({{cmd:{json.dumps(command)}}});"
                self.assertEqual(decision(self.run_codex_exec_guard(source)), "deny")

    def test_codex_functions_exec_checks_every_nested_command(self) -> None:
        source = (
            'await tools.exec_command({cmd:"gh pr view --repo example/unowned 1"});'
            'await tools.exec_command({cmd:"gh issue create --repo example/unowned '
            '--title T --body B"});'
        )
        self.assertEqual(decision(self.run_codex_exec_guard(source)), "deny")

    def test_codex_functions_exec_allows_nested_read_only_commands(self) -> None:
        source = (
            'await tools.exec_command({cmd:"gh pr view --repo example/unowned 1"});'
            'await tools.exec_command({cmd:"gh issue list --repo example/unowned"});'
        )
        self.assertEqual(decision(self.run_codex_exec_guard(source)), "allow")

    def test_codex_functions_exec_allows_nested_allowlisted_push(self) -> None:
        source = (
            'await tools.exec_command({cmd:"git push '
            'https://github.com/benthamite/scratch.git HEAD:topic"});'
        )
        self.assertEqual(decision(self.run_codex_exec_guard(source)), "allow")

    def test_codex_functions_exec_denies_dynamic_or_ambiguous_call(self) -> None:
        for source in (
            'const args={cmd:"gh pr view --repo example/unowned 1"}; '
            "await tools.exec_command(args);",
            'const args={cmd:"gh pr view --repo example/unowned 1"}; '
            "await tools.exec_command({...args});",
            'await tools.exec_command({cmd:"gh pr view --repo example/unowned 1; " + '
            '"gh issue create --repo example/unowned --title T --body B"});',
            'const e=tools.exec_command; await e({cmd:"gh issue create '
            '--repo example/unowned --title T --body B"});',
            'await tools["exec_command"]({cmd:"gh issue create '
            '--repo example/unowned --title T --body B"});',
            'const {exec_command:e}=tools; await e({cmd:"gh issue create '
            '--repo example/unowned --title T --body B"});',
            'const name="exec_"+"command"; await tools[name]({cmd:"gh issue create '
            '--repo example/unowned --title T --body B"});',
        ):
            with self.subTest(source=source):
                self.assertEqual(decision(self.run_codex_exec_guard(source)), "deny")

    def test_git_global_options_and_executable_paths_are_gated(self) -> None:
        for command in (
            "git -C /tmp push https://github.com/example/unowned.git HEAD:topic",
            "/usr/bin/git push https://github.com/example/unowned.git HEAD:topic",
            '"/usr/bin/git" push https://github.com/example/unowned.git HEAD:topic',
            "git --paginate push https://github.com/example/unowned.git HEAD:topic",
            "git -p push https://github.com/example/unowned.git HEAD:topic",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="deny")

    def test_gh_repo_environment_overrides_ambient_repository(self) -> None:
        for command in (
            "GH_REPO=example/unowned gh pr create --title T --body B",
            "GH_REPO='example/unowned' gh pr create --title T --body B",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="deny")

    def test_compound_commands_cannot_borrow_an_allowlisted_target(self) -> None:
        for command in (
            "echo https://github.com/benthamite/scratch; git push https://github.com/example/unowned.git HEAD:topic",
            "gh pr view --repo benthamite/scratch 1; gh issue create --repo example/unowned --title T --body B",
            "gh issue create --repo benthamite/scratch --title T --body B; gh issue create --repo example/unowned --title T --body B",
            "git push --dry-run https://github.com/benthamite/scratch.git HEAD:topic; git push https://github.com/example/unowned.git HEAD:topic",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="deny")

    def test_additional_repo_write_families_are_gated(self) -> None:
        for command in (
            "gh cache delete --all --repo example/unowned",
            "gh discussion create --repo example/unowned --title T --body B",
            "gh repo deploy-key add key.pub --repo example/unowned",
            "gh repo autolink create --repo example/unowned --key-prefix T- --url-template https://example.test/<num>",
            "gh pr revert 1 --repo example/unowned",
            "gh release delete-asset v1 asset.zip --repo example/unowned --yes",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="deny")

    def test_non_repo_scoped_remote_writes_are_denied(self) -> None:
        for command in (
            "gh project create --owner example --title T",
            "gh ssh-key add key.pub --title T",
            "gh codespace create --repo example/unowned",
            "gh repo fork example/unowned",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="deny")

    def test_account_wildcard_allows_repo(self) -> None:
        command = (
            "git push https://github.com/benthamite/yasnippet.git "
            "main"
        )
        self.assert_both(command, expected="allow")

    def test_other_account_is_denied(self) -> None:
        command = (
            "git push https://github.com/example/unowned.git "
            "main"
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
            "main"
        )

        self.assert_both(command, expected="allow")

    def test_wildcard_does_not_match_similar_owner(self) -> None:
        command = (
            "git push https://github.com/benthamitee/yasnippet.git "
            "main"
        )
        self.assert_both(command, expected="deny")


    def declared_repos(self) -> list[str]:
        registry_repo = Path.home() / "repos" / "epoch" / "automations-dashboard"
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
            "main"
        )

        self.assert_both(command, expected="allow")

    def test_undeclared_repo_in_the_same_org_is_denied(self) -> None:
        declared = set(self.declared_repos())
        self.assertNotIn("epoch-research/not-a-project-of-mine", declared)
        command = (
            "git push https://github.com/epoch-research/not-a-project-of-mine.git "
            "main"
        )

        self.assert_both(command, expected="deny")

    def test_working_tree_edits_to_the_registry_do_not_widen_the_gate(self) -> None:
        # A blocked agent can edit the registry file; only a commit counts. This
        # asserts the guard reads HEAD, by checking a repo present in the working
        # tree but absent from the committed blob is still refused.
        registry = Path.home() / "repos" / "epoch" / "automations-dashboard" / "data" / "automations.json"
        working = json.loads(registry.read_text(encoding="utf-8"))
        working_repos = {
            repo.lower()
            for project in working["projects"]
            for repo in project.get("repos", [])
        }
        # Nothing to assert if the working tree matches HEAD, which is the norm.
        for repo in sorted(working_repos - set(self.declared_repos())):
            command = f"git push https://github.com/{repo}.git main"
            self.assert_both(command, expected="deny")

    def test_opening_a_pull_request_on_an_unowned_repo_is_denied(self) -> None:
        command = (
            "gh pr create --repo example/unowned --base main "
            "--title Fix --body Body"
        )
        self.assert_both(command, expected="deny")

    def test_filing_an_issue_on_an_unowned_repo_is_denied(self) -> None:
        command = "gh issue create --repo example/unowned --title Q --body Body"
        self.assert_both(command, expected="deny")

    def test_commenting_on_an_unowned_repo_is_denied(self) -> None:
        for command in (
            "gh issue comment --repo example/unowned 1 --body Thanks",
            "gh pr comment --repo example/unowned 1 --body Rebased",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="deny")

    def test_reading_an_unowned_repo_is_allowed(self) -> None:
        for command in (
            "gh pr view --repo example/unowned 1",
            "gh pr list --repo example/unowned",
            "gh pr status --repo example/unowned",
            "gh pr checks --repo example/unowned 1",
            "gh pr diff --repo example/unowned 1",
            "gh issue view --repo example/unowned 1",
            "gh issue list --repo example/unowned",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="allow")

    def test_merging_a_pull_request_on_an_unowned_repo_is_denied(self) -> None:
        command = "gh pr merge --repo example/unowned 1 --squash"
        self.assert_both(command, expected="deny")

    def test_changing_someone_elses_issue_is_denied(self) -> None:
        for command in (
            "gh issue close --repo example/unowned 1",
            "gh issue edit --repo example/unowned 1 --body Rewritten",
            "gh issue delete --repo example/unowned 1",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="deny")

    # `gh repo` names its target positionally and `gh api` endpoints are often
    # written with a leading slash or in quotes. Both used to slip past the
    # target parsers and land on the surrounding checkout's remote, which is
    # this repo -- allowlisted -- so an unowned target inherited its
    # authorization. These tests run from ROOT precisely to reproduce that.

    def test_gh_repo_create_reads_its_positional_target(self) -> None:
        self.assert_both("gh repo create example/unowned --private", expected="deny")

    def test_gh_repo_create_allows_an_allowlisted_positional_target(self) -> None:
        self.assert_both("gh repo create benthamite/scratch --private", expected="allow")

    def test_gh_repo_create_without_a_named_target_is_denied(self) -> None:
        # `create` never operates on the ambient repo, so the cwd remote must
        # not stand in for a target the command did not name.
        self.assert_both("gh repo create scratch --private", expected="deny")

    def test_other_gh_repo_verbs_read_their_positional_target(self) -> None:
        for command in (
            "gh repo delete example/unowned --yes",
            "gh repo edit example/unowned --visibility public",
            "gh repo archive example/unowned --yes",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="deny")

    def test_gh_api_endpoint_is_read_through_a_leading_slash(self) -> None:
        command = "gh api --method POST /repos/example/unowned/rulesets"
        self.assert_both(command, expected="deny")

    def test_gh_api_endpoint_is_read_through_quotes(self) -> None:
        for command in (
            'gh api --method POST "repos/example/unowned/rulesets"',
            "gh api --method POST '/repos/example/unowned/rulesets'",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="deny")

    def test_gh_api_endpoint_with_a_leading_slash_still_allows_owned_repos(
        self,
    ) -> None:
        command = "gh api --method POST /repos/benthamite/org/rulesets"
        self.assert_both(command, expected="allow")


if __name__ == "__main__":
    unittest.main()
