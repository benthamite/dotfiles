"""Behavioral parity tests for the Claude and Codex GitHub write guards."""

from __future__ import annotations

import functools
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parent))
from guard_harness import GuardResult, run_guard  # noqa: E402

from agent_guard.policies import github_write  # noqa: E402


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


def checked(result: GuardResult) -> GuardResult:
    """The shell runs used check=True: a guard failure is an error, not a deny."""
    if result.returncode != 0:
        raise AssertionError(result.stderr)
    return result


def decision(result: GuardResult) -> str:
    if not result.stdout.strip():
        return "allow"
    output = json.loads(result.stdout)
    return output["hookSpecificOutput"]["permissionDecision"]


def setUpModule() -> None:
    # Every check reads the allowlist and the declared-repo registry from
    # their repositories' HEAD (two `git show` runs, ~100 ms on Drive). No test
    # here commits, so read each blob once per run.
    show = functools.lru_cache(maxsize=None)(functools.partial(github_write._git_show, env=None))
    patch = mock.patch.object(github_write, "_git_show", lambda directory, spec, env: show(directory, spec))
    patch.start()
    unittest.addModuleCleanup(patch.stop)


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
    ) -> GuardResult:
        if self.gh_log.exists():
            self.gh_log.unlink()
        env = os.environ.copy()
        env["PATH"] = os.pathsep.join((str(self.temp_path), env["PATH"]))
        env["FAKE_GH_LOG"] = str(self.gh_log)
        return checked(run_guard(GUARDS[tool], payload(tool, command), cwd=ROOT, env=env))

    def run_codex_exec_guard(self, source: str) -> GuardResult:
        data = {"tool_name": "functions.exec", "tool_input": {"input": source}}
        return checked(run_guard(GUARDS["codex"], data, cwd=ROOT))

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

    def test_codex_functions_exec_resolves_literal_const_object(self) -> None:
        source = (
            'const args={cmd:"gh pr view --repo example/unowned 1"}; '
            "await tools.exec_command(args);"
        )
        self.assertEqual(decision(self.run_codex_exec_guard(source)), "allow")

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

    def test_broker_read_piped_to_gh_secret_set_binds_its_own_target(self) -> None:
        # The sanctioned secret-provisioning pipeline (context/secrets.md):
        # the reader stage runs no GitHub command, so the -R of the
        # `gh secret set` stage is the only target.
        for command in (
            'op-automations @personal read "op://Automation/x/credential" | gh secret set CLAUDE_CODE_OAUTH_TOKEN -R benthamite/rsa',
            "op-automations read op://Automations/x/credential | gh secret set TOKEN --repo benthamite/scratch",
            "op-automations @tlon read op://Automation/x/credential | gh secret set TOKEN --repo=benthamite/scratch",
            "op-desktop read --account my.1password.com op://Employee/x/credential | gh secret set TOKEN -R benthamite/scratch",
            "op-desktop read op://Employee/x/credential | gh secret set -R benthamite/scratch TOKEN",
            'op-automations @personal read --no-newline "op://Automation/x/credential" | gh secret set TOKEN -R benthamite/scratch',
            "op-desktop read -n --account my.1password.com op://Employee/x/credential | gh secret set TOKEN -R benthamite/scratch",
            # Each and-or element is judged alone; `true` runs no write (2026-10-05).
            "op-automations read op://A/x/credential | gh secret set TOKEN -R benthamite/scratch || true",
            # A quoted reference is one token, so its item name may contain spaces.
            'op-automations read "op://Automations/GitHub - X Reader/private_key" | gh secret set TOKEN -R benthamite/scratch',
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="allow")
        for command in (
            # the target is not allowlisted
            "op-automations @personal read op://Automation/x/credential | gh secret set TOKEN -R example/unowned",
            # no segment-local target: the ambient checkout must not stand in
            "op-automations @personal read op://Automation/x/credential | gh secret set TOKEN",
            "GH_REPO=benthamite/scratch op-automations read op://A/x/credential | gh secret set TOKEN",
            "op-automations read op://A/x/credential | GH_REPO=benthamite/scratch gh secret set TOKEN",
            # a read option other than --no-newline/-n (and op-desktop --account)
            "op-automations read --out-file /tmp/x op://A/x/credential | gh secret set TOKEN -R benthamite/scratch",
            # anything beyond the two stages
            "op-automations read op://A/x/credential | gh secret set TOKEN -R benthamite/scratch | cat",
            "op-automations read op://A/x/credential | gh secret set TOKEN -R benthamite/scratch && gh secret set T2 -R example/unowned",
            "op-automations read op://A/x/credential | gh secret set TOKEN -R benthamite/scratch; git push https://github.com/example/unowned.git HEAD:topic",
            "(op-automations read op://A/x/credential | gh secret set TOKEN -R benthamite/scratch)",
            "op-automations read op://A/x/credential | gh secret set TOKEN -R benthamite/scratch > /tmp/out",
            "op-automations read op://A/x/credential | gh secret set TOKEN -R benthamite/scratch -R example/unowned",
            "op-automations read op://A/x/credential | gh secret set TOKEN -R benthamite/scratch --org example",
            "op-automations read op://A/x/credential | gh secret set TOKEN -R benthamite/scratch --env prod",
            "op-automations read op://A/x/credential | gh secret set TOKEN -R $REPO",
            # the first stage is not a broker read
            "echo hunter2 | gh secret set TOKEN -R example/unowned",
            "cat token.txt | gh secret set TOKEN -R benthamite/scratch",
            "gh repo view example/unowned | gh secret set TOKEN -R benthamite/scratch",
            "op-automations item get x | gh secret set TOKEN -R benthamite/scratch",
            "op-automations @bogus read op://A/x/credential | gh secret set TOKEN -R benthamite/scratch",
            "op-automations read $(cat ref) | gh secret set TOKEN -R benthamite/scratch",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="deny")

    def test_compound_read_only_commands_are_allowed(self) -> None:
        for command in (
            "gh run list --repo example/unowned --json status | head -3",
            "gh api repos/example/unowned/releases --jq '.[0].tag_name' | tr -d '\\n'",
            "gh pr view --repo example/unowned 1 && gh pr checks --repo example/unowned 1",
            "cd /tmp && gh release view 1.0 --repo example/unowned",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="allow")

    def test_a_write_piped_only_into_output_filters_keeps_its_target(self) -> None:
        for command in (
            "git push https://github.com/benthamite/scratch.git HEAD:topic 2>&1 | tail -3",
            "gh issue create --repo benthamite/scratch --title T --body B | head -1",
            # Each and-or element is judged alone; `echo x` runs no write (2026-10-05).
            "echo x && git push https://github.com/benthamite/scratch.git HEAD:topic | tail -1",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="allow")
        for command in (
            "git push https://github.com/example/unowned.git HEAD:topic 2>&1 | tail -3",
            "git push https://github.com/benthamite/scratch.git HEAD:topic | sh",
            "git push https://github.com/benthamite/scratch.git HEAD:topic | tail -1 > out",
            "git push https://github.com/benthamite/scratch.git HEAD:topic | tail $(x)",
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

    def test_merging_into_an_owned_repo_falls_through_silently(self) -> None:
        """Repository authorization does not pre-approve an external write."""
        command = "gh pr merge --repo benthamite/yasnippet 1 --squash"
        for tool in GUARDS:
            with self.subTest(tool=tool):
                result = self.run_guard(tool, command)
                self.assertEqual(result.stdout.strip(), "")

    def test_other_pr_writes_on_an_owned_repo_stay_silent(self) -> None:
        """Other allowlisted PR writes also keep their permission layers."""
        command = "gh pr create --repo benthamite/yasnippet --title T --body B"
        for tool in GUARDS:
            with self.subTest(tool=tool):
                result = self.run_guard(tool, command)
                self.assertEqual(result.stdout.strip(), "")

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


class EpochPushTests(unittest.TestCase):
    """Agents publish to epoch-research through epoch_publish.py, not git push.

    GitHub notifies the pusher of a failed push-triggered run, so an agent
    pushing with Pablo's login fills his inbox. With the flag on, every agent
    `git push` to an epoch-research repository is refused, declared or not,
    through each Claude and Codex entry point; other GitHub writes keep their
    existing rules.
    """

    EPOCH = "https://github.com/epoch-research/ai-enablement.git"

    def setUp(self) -> None:
        patch = mock.patch.object(github_write, "EPOCH_PUSH_ENFORCED", True)
        patch.start()
        self.addCleanup(patch.stop)

    def decide(self, tool: str, command: str) -> GuardResult:
        return checked(run_guard(GUARDS[tool], payload(tool, command), cwd=ROOT))

    def test_pushes_to_epoch_research_are_refused_with_the_helper_named(self) -> None:
        epoch_checkout = Path.home() / "repos" / "epoch" / "ai-enablement"
        commands = [
            f"git push {self.EPOCH} master",
            f"git push {self.EPOCH} HEAD:topic",
            f"/usr/bin/git push {self.EPOCH} master",
            f"cd /tmp && git push {self.EPOCH} master",
            f"bash -c 'git push {self.EPOCH} master'",
        ]
        if epoch_checkout.is_dir():
            commands += [f"git -C {epoch_checkout} push", f"cd {epoch_checkout} && git push origin master"]
        for tool in GUARDS:
            for command in commands:
                with self.subTest(tool=tool, command=command):
                    result = self.decide(tool, command)
                    self.assertEqual(decision(result), "deny")
                    self.assertIn("epoch_publish.py", result.stdout)

    def test_codex_functions_exec_nested_push_is_refused(self) -> None:
        source = f'await tools.exec_command({{cmd:"git push {self.EPOCH} master"}});'
        result = checked(run_guard(GUARDS["codex"], {"tool_name": "functions.exec", "tool_input": {"input": source}},
                                   cwd=ROOT))
        self.assertEqual(decision(result), "deny")
        self.assertIn("epoch_publish.py", result.stdout)

    def test_other_writes_and_other_owners_are_unchanged(self) -> None:
        for tool in GUARDS:
            for command, expected in (
                (f"git push --dry-run {self.EPOCH} master", "allow"),
                ("git push https://github.com/benthamite/scratch.git HEAD:topic", "allow"),
                ("gh pr create --repo epoch-research/ai-enablement --title T --body B", "allow"),
                ("gh issue comment --repo epoch-research/ai-enablement 1 --body B", "allow"),
                ("git push https://github.com/epoch-research/not-a-project-of-mine.git main", "deny"),
            ):
                with self.subTest(tool=tool, command=command):
                    self.assertEqual(decision(self.decide(tool, command)), expected)

    def test_topic_branches_go_through_the_writer_app_where_it_is_installed(self) -> None:
        for tool in GUARDS:
            for command in (f"git push {self.EPOCH} topic", f"git push -u {self.EPOCH} HEAD:refs/heads/topic"):
                with self.subTest(tool=tool, command=command):
                    self.assertEqual(decision(self.decide(tool, command)), "deny")

    def test_without_the_writer_app_only_named_non_default_branches_may_be_pushed(self) -> None:
        url = "https://github.com/epoch-research/alert-responder-sandbox.git"
        missing = {"epoch-research/alert-responder-sandbox"}
        with mock.patch.object(github_write, "_writer_missing", lambda env: missing):
            for tool in GUARDS:
                for command, expected in (
                    (f"git push {url} topic", "allow"),
                    (f"git push -u {url} local:refs/heads/fix/x", "allow"),
                    (f"git push {url} main", "deny"),
                    (f"git push {url} topic:master", "deny"),
                    (f"git push {url} +topic", "deny"),
                    (f"git push --force {url} topic", "deny"),
                    (f"git push --force-with-lease {url} topic", "deny"),
                    (f"git push -fu {url} topic", "deny"),
                    (f"git push --delete {url} topic", "deny"),
                    (f"git push {url} :topic", "deny"),
                    (f"git push --all {url}", "deny"),
                    (f"git push {url} refs/tags/v1", "deny"),
                ):
                    with self.subTest(tool=tool, command=command):
                        self.assertEqual(decision(self.decide(tool, command)), expected)

    def test_the_committed_writer_missing_list_is_read(self) -> None:
        # Fails until agents/epoch-writer-missing.txt is committed: the guard
        # reads HEAD, never the working tree.
        self.assertIn("epoch-research/alert-responder-sandbox", github_write._writer_missing(os.environ.copy()))

    def test_push_destinations(self) -> None:
        current = lambda: "feature"  # noqa: E731
        cases = {
            "git push": ["feature"],
            "git push origin": ["feature"],
            "git push origin HEAD": ["feature"],
            "git push -o ci.skip origin a b:c": ["a", "c"],
            "git -C /x push origin refs/heads/a": ["a"],
            "git push origin +a": None,
            "git push origin refs/tags/v1": None,
            "git push --mirror origin": None,
        }
        for command, expected in cases.items():
            with self.subTest(command=command):
                self.assertEqual(github_write.push_destinations(command, current), expected)
        self.assertIsNone(github_write.push_destinations("git push origin HEAD", lambda: None))

    def test_the_flag_off_keeps_declared_pushes_allowed(self) -> None:
        with mock.patch.object(github_write, "EPOCH_PUSH_ENFORCED", False):
            for tool in GUARDS:
                with self.subTest(tool=tool):
                    self.assertEqual(decision(self.decide(tool, f"git push {self.EPOCH} master")), "allow")


class DispatcherPrefilterTests(unittest.TestCase):
    """The Claude Bash dispatcher must reach the guard for every push spelling.

    Only pretooluse-bash.sh is registered for Claude, so a prefilter that
    misses a form is a bypass, not a false negative in the guard. Until
    2026-09-18 `git -C DIR push` and `/usr/bin/git push` slipped through it
    (found by the learning-inbox triage).
    """

    DISPATCHER = ROOT / "claude" / "hooks" / "pretooluse-bash.sh"
    UNOWNED = "https://github.com/someone-else/not-owned.git"

    def run_dispatcher(self, command: str) -> str:
        return decision(checked(run_guard(self.DISPATCHER, payload("claude", command), cwd=ROOT)))

    def test_push_forms_with_git_options_reach_the_guard(self) -> None:
        for command in (
            f"git push {self.UNOWNED} master",
            f"git -C /tmp/somerepo push {self.UNOWNED} master",
            f"/usr/bin/git push {self.UNOWNED} master",
            f"git -c core.quotepath=off push {self.UNOWNED} master",
            f"pytest -q | tail -1 && git -C /tmp/somerepo push {self.UNOWNED} master",
        ):
            with self.subTest(command=command):
                self.assertEqual(self.run_dispatcher(command), "deny")

    def test_hidden_writes_reach_the_guard(self) -> None:
        # Until 2026-10-05 a backtick or `$'…'` before the word skipped the guard.
        for command in (
            f"echo `git push {self.UNOWNED} master`",
            f"eval $'git\\x20push {self.UNOWNED} master'",
            f'echo "$(git push {self.UNOWNED} master)"',
            f"printf 'git push {self.UNOWNED} master' | sh",
            f"bash -c 'git push {self.UNOWNED} master'",
        ):
            with self.subTest(command=command):
                self.assertEqual(self.run_dispatcher(command), "deny")

    def test_read_only_git_and_gh_still_pass_the_dispatcher(self) -> None:
        for command in (
            "git -C /tmp/somerepo status --short",
            "git -C /tmp/somerepo status --short && gh pr view 12 --repo someone-else/not-owned",
            "/usr/bin/git log --oneline -n 3",
        ):
            with self.subTest(command=command):
                self.assertEqual(self.run_dispatcher(command), "allow")


if __name__ == "__main__":
    unittest.main()
