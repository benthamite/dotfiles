from __future__ import annotations

import base64
import json
import os
import shlex
import subprocess
import tempfile
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
GATES = {
    "claude": DOTFILES / "claude/hooks/require-elisp-test-before-commit.sh",
    "codex": DOTFILES / "codex/hooks/require-elisp-test-before-commit.sh",
}
TRACKERS = {
    "claude": DOTFILES / "claude/hooks/track-elisp-verify.sh",
    "codex": DOTFILES / "codex/hooks/track-elisp-verify.sh",
}


class ElispCommitGateRoutingTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.repo = self.make_repo("elisp", "lisp/example.el")
        self.fallback = self.make_repo("fallback", "README.md")

    def make_repo(self, name: str, filename: str) -> Path:
        repo = self.root / name
        repo.mkdir(parents=True)
        subprocess.run(["git", "init", "-q", str(repo)], check=True)
        subprocess.run(
            ["git", "-C", str(repo), "config", "user.email", "test@example.com"],
            check=True,
        )
        subprocess.run(
            ["git", "-C", str(repo), "config", "user.name", "Hook Test"],
            check=True,
        )
        path = repo / filename
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("(provide 'example)\n" if filename.endswith(".el") else "fixture\n")
        subprocess.run(["git", "-C", str(repo), "add", filename], check=True)
        subprocess.run(["git", "-C", str(repo), "commit", "-qm", "fixture"], check=True)
        return repo

    def payload(self, tool: str, command: str, workdir: Path | None = None) -> dict:
        session = f"routing-{tool}-{os.getpid()}"
        if tool == "codex":
            tool_input = {"cmd": command}
            if workdir is not None:
                tool_input["workdir"] = str(workdir)
            return {
                "tool_name": "exec_command",
                "session_id": session,
                "tool_input": tool_input,
            }
        tool_input = {"command": command}
        if workdir is not None:
            tool_input["workdir"] = str(workdir)
        return {"session_id": session, "tool_input": tool_input}

    def run_gate(
        self,
        tool: str,
        command: str,
        workdir: Path | None = None,
        env: dict[str, str] | None = None,
    ):
        return subprocess.run(
            ["bash", str(GATES[tool])],
            input=json.dumps(self.payload(tool, command, workdir)),
            text=True,
            capture_output=True,
            check=False,
            cwd=self.fallback,
            env=env,
        )

    def git_records(self, command: str, workdir: Path) -> list[dict]:
        result = subprocess.run(
            [
                "bash",
                "-c",
                'source "$1"; codex_git_invocations "$2"',
                "parser-test",
                str(DOTFILES / "codex/hooks/lib-codex-hook-json.sh"),
                str(workdir),
            ],
            input=command.encode(),
            capture_output=True,
            check=True,
        )
        return [json.loads(value) for value in result.stdout.split(b"\0") if value]

    def stage_elisp_change(self):
        path = self.repo / "lisp/example.el"
        path.write_text("(provide 'changed)\n")
        subprocess.run(["git", "-C", str(self.repo), "add", str(path)], check=True)

    def leave_elisp_change_unstaged(self):
        (self.repo / "lisp/example.el").write_text("(provide 'changed)\n")

    def test_global_options_route_commit_for_both_gates(self):
        self.stage_elisp_change()
        commands = (
            f"git --no-pager -C {json.dumps(str(self.repo))} commit -m fixture",
            f"git -c user.name=test -C {json.dumps(str(self.repo))} commit -m fixture",
            f"bash -lc 'git --no-pager -C {json.dumps(str(self.repo))} commit -m fixture'",
        )
        for tool in GATES:
            for command in commands:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.fallback)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn("does not have matching test evidence", result.stdout)

    def test_command_assignment_scope_does_not_leak_to_later_commit(self):
        self.stage_elisp_change()
        commands = (
            "GIT_OPTIONAL_LOCKS=0 git status --short; git commit -m fixture",
            "UNRELATED=value git commit -m fixture",
        )
        for tool in GATES:
            for command in commands:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn("does not have matching test evidence", result.stdout)
                    self.assertNotIn("dynamic or ambiguous", result.stdout)

    def test_commit_text_in_argument_or_comment_is_inert(self):
        self.stage_elisp_change()
        command = "printf '%s\\n' 'git commit -m decoy' # git commit -m comment"
        for tool in GATES:
            with self.subTest(tool=tool):
                result = self.run_gate(tool, command, self.repo)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "")

    def test_unknown_global_option_with_commit_fails_closed(self):
        self.stage_elisp_change()
        for tool in GATES:
            with self.subTest(tool=tool):
                result = self.run_gate(
                    tool, "git --future-option value commit -m fixture", self.repo
                )
                self.assertIn("global options make the commit subcommand ambiguous", result.stdout)

    def test_git_target_and_index_environment_fail_closed_before_repo_lookup(self):
        commands = (
            f"GIT_DIR={shlex.quote(str(self.repo / '.git'))} git commit -m fixture",
            f"GIT_WORK_TREE={shlex.quote(str(self.repo))} git commit -m fixture",
            f"GIT_INDEX_FILE={shlex.quote(str(self.root / 'alternate-index'))} git commit -m fixture",
        )
        for tool in GATES:
            for command in commands:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.root)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn("dynamic or ambiguous", result.stdout)

    def test_config_env_alias_uses_effective_prefix_assignment(self):
        self.stage_elisp_change()
        cases = (
            (
                "ENVVAR=commit git --config-env=alias.x=ENVVAR x -m fixture",
                "does not have matching test evidence",
                "commit",
            ),
            (
                "ENVVAR=status git --config-env=alias.x=ENVVAR x --short",
                "",
                "status",
            ),
        )
        for tool in GATES:
            for command, message, subcommand in cases:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    if message:
                        self.assertIn(message, result.stdout)
                    else:
                        self.assertEqual(result.stdout, "")
                    records = self.git_records(command, self.repo)
                    self.assertEqual(records[0]["subcommand"], subcommand)

    def test_commit_aliases_are_resolved_or_fail_closed(self):
        subprocess.run(
            ["git", "-C", str(self.repo), "config", "alias.local-ci", "commit"],
            check=True,
        )
        global_home = self.root / "global-home"
        global_home.mkdir()
        global_env = os.environ.copy()
        global_env["HOME"] = str(global_home)
        subprocess.run(
            ["git", "config", "--global", "alias.user-ci", "commit"],
            check=True,
            env=global_env,
        )
        self.stage_elisp_change()
        cases = (
            (
                "git local-ci -m fixture",
                None,
                "does not have matching test evidence",
            ),
            (
                "git user-ci -m fixture",
                global_env,
                "does not have matching test evidence",
            ),
            (
                "git -c alias.inline-ci=commit inline-ci -m fixture",
                None,
                "does not have matching test evidence",
            ),
        )
        for tool in GATES:
            for command, env, message in cases:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo, env=env)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn(message, result.stdout)

    def test_non_commit_aliases_and_absent_subcommands_are_not_commits(self):
        subprocess.run(
            ["git", "-C", str(self.repo), "config", "alias.local-st", "status"],
            check=True,
        )
        self.stage_elisp_change()
        commands = (
            "git local-st --short",
            "git absent-alias",
            "git lfs version",
            "git -c alias.inline-st=status inline-st --short",
        )
        for tool in GATES:
            for command in commands:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout, "")

    def test_shell_forms_that_execute_commit_are_intercepted(self):
        self.stage_elisp_change()
        cases = (
            ("eval 'git commit -m fixture'", "dynamic or ambiguous", "shell-eval"),
            (
                "bash --norc -c 'git commit -m fixture'",
                "does not have matching test evidence",
                None,
            ),
            (
                "command -- git commit -m fixture",
                "does not have matching test evidence",
                None,
            ),
            ("! git commit -m fixture", "dynamic or ambiguous", "shell-control-flow"),
            (
                'verb=commit; git "$verb" -m fixture',
                "does not have matching test evidence",
                None,
            ),
            ("git {commit,-m,fixture}", "does not have matching test evidence", None),
            (
                'printf ignored "$(git commit -m fixture)"',
                "dynamic or ambiguous",
                "shell-command-substitution",
            ),
            (
                "git $'commit' -m fixture",
                "dynamic or ambiguous",
                "dynamic-git-subcommand",
            ),
            ("time git commit -m fixture", "does not have matching test evidence", None),
            (
                "if git commit -m fixture; then :; fi",
                "dynamic or ambiguous",
                "shell-control-flow",
            ),
            (
                "while git commit -m fixture; do break; done",
                "dynamic or ambiguous",
                "shell-control-flow",
            ),
            (
                "until git commit -m fixture; do break; done",
                "dynamic or ambiguous",
                "shell-control-flow",
            ),
        )
        for tool in GATES:
            for command, message, ambiguity in cases:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn(message, result.stdout)
                    if ambiguity is not None:
                        commit = next(
                            record
                            for record in self.git_records(command, self.repo)
                            if record["subcommand"] == "commit"
                        )
                        self.assertTrue(commit["ambiguous"])
                        self.assertEqual(commit["ambiguity"], ambiguity)

    def test_computed_git_subcommand_is_resolved_or_proportionally_ambiguous(self):
        self.stage_elisp_change()
        cases = (
            (
                'git "$(printf commit)" -m fixture',
                "does not have matching test evidence",
                False,
            ),
            ('git "$(printf status)" --short', "", False),
            ('git "$(unknown-helper)" -m fixture', "dynamic or ambiguous", True),
        )
        for tool in GATES:
            for command, message, ambiguous in cases:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    if message:
                        self.assertIn(message, result.stdout)
                    else:
                        self.assertEqual(result.stdout, "")
                    records = self.git_records(command, self.repo)
                    if ambiguous:
                        self.assertTrue(records[0]["ambiguous"])
                        self.assertEqual(
                            records[0]["ambiguity"], "dynamic-git-subcommand"
                        )
                    else:
                        expected = "commit" if "commit" in command else "status"
                        self.assertEqual(records[0]["subcommand"], expected)
                        self.assertFalse(records[0]["ambiguous"])

        readme = self.fallback / "README.md"
        readme.write_text("changed\n")
        subprocess.run(["git", "-C", str(self.fallback), "add", "README.md"], check=True)
        for tool in GATES:
            result = self.run_gate(
                tool, 'git "$(unknown-helper)" -m fixture', self.fallback
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, "")

    def test_shell_line_continuation_preserves_git_subcommand(self):
        self.stage_elisp_change()
        command = "git \\\ncommit -m fixture"
        for tool in GATES:
            with self.subTest(tool=tool):
                result = self.run_gate(tool, command, self.repo)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn("does not have matching test evidence", result.stdout)
        record = self.git_records(command, self.repo)[0]
        self.assertEqual(record["subcommand"], "commit")

    def test_line_continuation_in_inert_text_or_status_is_not_commit(self):
        commands = (
            "git \\\nstatus --short",
            "printf '%s' 'git \\\ncommit'",
        )
        for tool in GATES:
            for command in commands:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout, "")

    def test_additional_shell_executors_intercept_commit(self):
        self.stage_elisp_change()
        cases = (
            ("noglob git commit -m fixture", "does not have matching test evidence", None),
            (
                "function f { git commit -m fixture; }; f",
                "dynamic or ambiguous",
                "shell-function",
            ),
            (
                'env -S "git commit -m fixture"',
                "does not have matching test evidence",
                None,
            ),
            ("nice git commit -m fixture", "does not have matching test evidence", None),
            ("nohup git commit -m fixture", "does not have matching test evidence", None),
            (
                "printf x | xargs -I{} git commit -m fixture",
                "dynamic or ambiguous",
                "shell-xargs",
            ),
            (
                "find . -maxdepth 0 -exec git commit -m fixture \\;",
                "dynamic or ambiguous",
                "shell-find-exec",
            ),
        )
        for tool in GATES:
            for command, message, ambiguity in cases:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn(message, result.stdout)
                    if ambiguity:
                        commit = next(
                            record
                            for record in self.git_records(command, self.repo)
                            if record["subcommand"] == "commit"
                        )
                        self.assertTrue(commit["ambiguous"])
                        self.assertEqual(commit["ambiguity"], ambiguity)

    def test_additional_shell_executors_preserve_non_commit_controls(self):
        commands = (
            "noglob git status --short",
            "function f { git status --short; }; f",
            'env -S "git status --short"',
            "nice git status --short",
            "nohup git status --short",
            "printf x | xargs -I{} git status --short",
            "find . -maxdepth 0 -exec git status --short \\;",
        )
        for tool in GATES:
            for command in commands:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout, "")

    def test_inert_shell_forms_do_not_count_as_commits(self):
        self.stage_elisp_change()
        commands = (
            "command -v git commit",
            "eval 'printf \"%s\\n\" \"git commit\"'",
            'verb=status; git "$verb" --short',
            "git {status,--short}",
            'printf "%s" "$(printf \'git commit\')"',
            "time printf git commit",
            "if printf git commit; then :; fi",
            "bash --norc git commit",
            "printf x; true",
        )
        for tool in GATES:
            for command in commands:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout, "")

    def test_interpreter_recursion_limit_fails_closed(self):
        self.stage_elisp_change()
        command = "git commit -m fixture"
        for _ in range(5):
            command = f"bash -c {shlex.quote(command)}"
        for tool in GATES:
            with self.subTest(tool=tool):
                result = self.run_gate(tool, command, self.repo)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn("dynamic or ambiguous", result.stdout)

    def test_status_decoupling_after_commit_is_ambiguous_for_elisp(self):
        self.stage_elisp_change()
        commands = (
            "git commit -m fixture; true",
            "git commit -m fixture || true",
            "git commit -m fixture && false",
        )
        for tool in GATES:
            for command in commands:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn("dynamic or ambiguous", result.stdout)
                    commit = next(
                        record
                        for record in self.git_records(command, self.repo)
                        if record["subcommand"] == "commit"
                    )
                    self.assertTrue(commit["ambiguous"])
                    self.assertEqual(commit["ambiguity"], "shell-status-decoupled")

    def test_status_decoupling_does_not_block_non_elisp_commit(self):
        readme = self.fallback / "README.md"
        readme.write_text("changed\n")
        subprocess.run(["git", "-C", str(self.fallback), "add", "README.md"], check=True)
        for tool in GATES:
            with self.subTest(tool=tool):
                result = self.run_gate(
                    tool, "git commit -m fixture; true", self.fallback
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "")

    def test_safe_command_below_interpreter_recursion_limit_is_not_a_commit(self):
        command = "git status --short"
        for _ in range(4):
            command = f"bash -c {shlex.quote(command)}"
        for tool in GATES:
            with self.subTest(tool=tool):
                result = self.run_gate(tool, command, self.repo)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "")

    def test_terminal_global_options_do_not_count_as_commits(self):
        self.stage_elisp_change()
        for tool in GATES:
            for command in (
                "git --help commit",
                "git --version commit",
                "git --no-pager --version commit",
                "git -c alias.ci=commit --help ci",
            ):
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout, "")

    def test_broad_or_directory_add_with_commit_is_denied(self):
        commands = ("git add .", "git add ./", "git add -A", "git add -u", "git add lisp")
        for tool in GATES:
            for add in commands:
                with self.subTest(tool=tool, add=add):
                    subprocess.run(
                        ["git", "-C", str(self.repo), "restore", "lisp/example.el"],
                        check=True,
                    )
                    self.leave_elisp_change_unstaged()
                    result = self.run_gate(tool, f"{add} && git commit -m fixture", self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn("Stage Elisp source in a separate command", result.stdout)

    def test_interactive_add_with_commit_is_denied(self):
        self.leave_elisp_change_unstaged()
        for tool in GATES:
            for add in ("git add -p", "git add -i"):
                with self.subTest(tool=tool, add=add):
                    result = self.run_gate(tool, f"{add} && git commit -m fixture", self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn("Stage Elisp source in a separate command", result.stdout)

    def test_interactive_add_ignores_unselected_elisp_paths(self):
        readme = self.repo / "README.md"
        readme.write_text("baseline\n")
        subprocess.run(["git", "-C", str(self.repo), "add", "README.md"], check=True)
        subprocess.run(
            ["git", "-C", str(self.repo), "commit", "-qm", "add readme"], check=True
        )
        readme.write_text("changed\n")
        self.leave_elisp_change_unstaged()
        commands = (
            "git add -p README.md && git commit -m fixture",
            "git add -i README.md && git commit -m fixture",
        )
        for tool in GATES:
            for command in commands:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, self.repo)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout, "")

    def test_interactive_add_with_only_readme_change_is_not_elisp(self):
        readme = self.fallback / "README.md"
        readme.write_text("changed\n")
        for tool in GATES:
            for add in ("git add -p", "git add -i"):
                with self.subTest(tool=tool, add=add):
                    result = self.run_gate(
                        tool, f"{add} && git commit -m fixture", self.fallback
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout, "")

    def run_functions_gate(self, source: str, cwd: Path):
        payload = {
            "tool_name": "functions.exec",
            "session_id": f"nested-routing-{os.getpid()}",
            "cwd": str(cwd),
            "tool_input": {"input": source},
        }
        return subprocess.run(
            ["bash", str(GATES["codex"])],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
            cwd=cwd,
        )

    def test_nested_omitted_workdir_uses_outer_cwd(self):
        self.stage_elisp_change()
        result = self.run_functions_gate(
            "await tools.exec_command({cmd: 'git commit -m fixture'});", self.repo
        )
        self.assertIn("does not have matching test evidence", result.stdout)

    def test_nested_dynamic_workdir_commit_is_denied(self):
        source = (
            "const target = '/tmp/dynamic'; "
            "await tools.exec_command({cmd: 'git commit -m fixture', workdir: target});"
        )
        result = self.run_functions_gate(source, self.fallback)
        self.assertIn("dynamic or ambiguous", result.stdout)

    def test_nested_local_alias_uses_nested_workdir_for_resolution(self):
        subprocess.run(
            ["git", "-C", str(self.repo), "config", "alias.nested-ci", "commit"],
            check=True,
        )
        self.stage_elisp_change()
        source = "await tools.exec_command(" + json.dumps(
            {"cmd": "git nested-ci -m fixture", "workdir": str(self.repo)}
        ) + ");"
        result = self.run_functions_gate(source, self.fallback)
        self.assertIn("does not have matching test evidence", result.stdout)

    def test_nested_literal_object_variable_is_resolved(self):
        self.stage_elisp_change()
        workdir = json.dumps(str(self.repo))
        commit_source = (
            f"const a = {{cmd: 'git commit -m fixture', workdir: {workdir}}}; "
            "await tools.exec_command(a);"
        )
        result = self.run_functions_gate(commit_source, self.fallback)
        self.assertIn("does not have matching test evidence", result.stdout)

        status_source = (
            f"const a = {{cmd: 'git status --short', workdir: {workdir}}}; "
            "await tools.exec_command(a);"
        )
        result = self.run_functions_gate(status_source, self.fallback)
        self.assertEqual(result.stdout, "")

    def test_each_commit_uses_its_own_repository_context(self):
        self.stage_elisp_change()
        commands = (
            (
                f"git -C {shlex.quote(str(self.fallback))} commit -m clean; "
                f"git -C {shlex.quote(str(self.repo))} commit -m elisp",
                self.fallback,
                "does not have matching test evidence",
            ),
            (
                f"env --chdir={shlex.quote(str(self.repo))} git commit -m fixture",
                self.fallback,
                "does not have matching test evidence",
            ),
            (
                f"git commit -m fixture; cd {shlex.quote(str(self.fallback))}",
                self.repo,
                "dynamic or ambiguous",
            ),
        )
        for tool in GATES:
            for command, workdir, message in commands:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, workdir)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn(message, result.stdout)

    def test_repository_context_does_not_leak_between_invocations(self):
        self.stage_elisp_change()
        commands = (
            (
                f"env --chdir={shlex.quote(str(self.fallback))} "
                "git commit -m fixture",
                self.repo,
            ),
            (
                f"git commit -m fixture; cd {shlex.quote(str(self.repo))}",
                self.fallback,
            ),
        )
        for tool in GATES:
            for command, workdir in commands:
                with self.subTest(tool=tool, command=command):
                    result = self.run_gate(tool, command, workdir)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout, "")

    def test_separate_nested_add_and_commit_share_explicit_workdir(self):
        self.leave_elisp_change_unstaged()
        workdir = json.dumps(str(self.repo))
        source = (
            f"await tools.exec_command({{cmd: 'git add .', workdir: {workdir}}});"
            f"await tools.exec_command({{cmd: 'git commit -m fixture', workdir: {workdir}}});"
        )
        result = self.run_functions_gate(source, self.fallback)
        self.assertIn("Stage Elisp source in a separate command", result.stdout)

    def test_global_option_forms_are_counted_by_both_trackers(self):
        tracker_repo = self.make_repo(
            "profile/elpaca/sources/tracker-global", "tracker-global.el"
        )
        commands = (
            f"git --no-pager -C {json.dumps(str(tracker_repo))} commit -m fixture",
            f"git -c user.name=test -C {json.dumps(str(tracker_repo))} commit -m fixture",
            f"git -c alias.inline-ci=commit -C {json.dumps(str(tracker_repo))} inline-ci -m fixture",
        )
        for tool, tracker in TRACKERS.items():
            for index, command in enumerate(commands):
                session = f"tracker-global-{tool}-{index}-{os.getpid()}"
                marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
                marker.unlink(missing_ok=True)
                self.addCleanup(marker.unlink, missing_ok=True)
                payload = self.payload(tool, command, self.fallback)
                payload["session_id"] = session
                if tool == "codex":
                    payload["tool_response"] = {"exit_code": 0}
                else:
                    payload["tool_output"] = {"exitCode": 0}
                result = subprocess.run(
                    ["bash", str(tracker)],
                    input=json.dumps(payload),
                    text=True,
                    capture_output=True,
                    check=False,
                    cwd=self.fallback,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertTrue(marker.exists(), f"{tool}: {command}")

        for index, command in enumerate(commands):
            session = f"tracker-global-nested-codex-{index}-{os.getpid()}"
            marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
            marker.unlink(missing_ok=True)
            self.addCleanup(marker.unlink, missing_ok=True)
            source = (
                "await tools.exec_command("
                + json.dumps({"cmd": command, "workdir": str(self.fallback)})
                + ");"
            )
            payload = {
                "tool_name": "functions.exec",
                "session_id": session,
                "tool_input": source,
                "tool_response": {"exit_code": 0},
            }
            result = subprocess.run(
                ["bash", str(TRACKERS["codex"])],
                input=json.dumps(payload),
                text=True,
                capture_output=True,
                check=False,
                cwd=self.fallback,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertTrue(marker.exists(), f"nested codex: {command}")

    def test_trackers_attribute_each_commit_to_its_own_repository(self):
        tracker_repo = self.make_repo(
            "profile/elpaca/sources/per-repo-tracker", "per-repo-tracker.el"
        )
        expected_repo = base64.b64encode(str(tracker_repo.resolve()).encode()).decode()
        unexpected_repo = base64.b64encode(str(self.fallback.resolve()).encode()).decode()
        cases = (
            (
                f"git -C {shlex.quote(str(self.fallback))} commit -m clean; "
                f"git -C {shlex.quote(str(tracker_repo))} commit -m elisp",
                self.fallback,
            ),
            (
                f"env --chdir={shlex.quote(str(tracker_repo))} git commit -m fixture",
                self.fallback,
            ),
            (
                f"cd {shlex.quote(str(tracker_repo))} && git commit -m fixture",
                self.fallback,
            ),
        )
        for tool, tracker in TRACKERS.items():
            for index, (command, workdir) in enumerate(cases):
                session = f"tracker-per-repo-{tool}-{index}-{os.getpid()}"
                marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
                marker.unlink(missing_ok=True)
                self.addCleanup(marker.unlink, missing_ok=True)
                payload = self.payload(tool, command, workdir)
                payload["session_id"] = session
                if tool == "codex":
                    payload["tool_response"] = {"exit_code": 0}
                else:
                    payload["tool_output"] = {"exitCode": 0}
                result = subprocess.run(
                    ["bash", str(tracker)],
                    input=json.dumps(payload),
                    text=True,
                    capture_output=True,
                    check=False,
                    cwd=self.fallback,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertTrue(marker.exists(), f"{tool}: {command}")
                self.assertTrue(
                    any(
                        line.startswith(expected_repo + ":")
                        for line in marker.read_text().splitlines()
                    ),
                    f"{tool}: {command}",
                )
                self.assertFalse(
                    any(
                        line.startswith(unexpected_repo + ":")
                        for line in marker.read_text().splitlines()
                    ),
                    f"{tool}: {command}",
                )
                self.assertNotIn("MALFORMED", marker.read_text())

    def test_trackers_do_not_record_status_decoupled_commit(self):
        tracker_repo = self.make_repo(
            "profile/elpaca/sources/status-ambiguous", "status-ambiguous.el"
        )
        command = f"git commit -m fixture; cd {shlex.quote(str(self.fallback))}"
        for tool, tracker in TRACKERS.items():
            session = f"tracker-status-ambiguous-{tool}-{os.getpid()}"
            marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
            marker.unlink(missing_ok=True)
            self.addCleanup(marker.unlink, missing_ok=True)
            payload = self.payload(tool, command, tracker_repo)
            payload["session_id"] = session
            if tool == "codex":
                payload["tool_response"] = {"exit_code": 0}
            else:
                payload["tool_output"] = {"exitCode": 0}
            result = subprocess.run(
                ["bash", str(tracker)],
                input=json.dumps(payload),
                text=True,
                capture_output=True,
                check=False,
                cwd=self.fallback,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertFalse(marker.exists(), f"{tool}: {command}")

    def test_terminal_global_options_do_not_create_tracker_markers(self):
        tracker_repo = self.make_repo(
            "profile/elpaca/sources/tracker-terminal", "tracker-terminal.el"
        )
        for tool, tracker in TRACKERS.items():
            for index, command in enumerate(
                (
                    "git --help commit",
                    "git --version commit",
                    "git --no-pager --version commit",
                    "git -c alias.ci=commit --help ci",
                )
            ):
                session = f"tracker-terminal-{tool}-{index}-{os.getpid()}"
                marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
                marker.unlink(missing_ok=True)
                self.addCleanup(marker.unlink, missing_ok=True)
                payload = self.payload(tool, command, tracker_repo)
                payload["session_id"] = session
                if tool == "codex":
                    payload["tool_response"] = {"exit_code": 0}
                else:
                    payload["tool_output"] = {"exitCode": 0}
                result = subprocess.run(
                    ["bash", str(tracker)],
                    input=json.dumps(payload),
                    text=True,
                    capture_output=True,
                    check=False,
                    cwd=tracker_repo,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertFalse(marker.exists(), f"{tool}: {command}")


if __name__ == "__main__":
    unittest.main()
