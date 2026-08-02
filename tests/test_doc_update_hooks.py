from __future__ import annotations

import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
HOOKS = (
    (DOTFILES / "claude/hooks/require-doc-update.sh", "command"),
    (DOTFILES / "codex/hooks/require-doc-update.sh", "cmd"),
)


def permission_decision(result: subprocess.CompletedProcess[str]) -> str:
    if not result.stdout.strip():
        return "allow"
    output = json.loads(result.stdout)
    return output["hookSpecificOutput"].get("permissionDecision", "allow")


class DocUpdateHookTests(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        self.repo = Path(self.temp_dir.name) / "repo"
        self.repo.mkdir()
        subprocess.run(["git", "init", "-q", str(self.repo)], check=True)
        (self.repo / "README.md").write_text("# Example\n")
        subprocess.run(
            ["git", "-C", str(self.repo), "add", "README.md"],
            check=True,
        )
        subprocess.run(
            [
                "git",
                "-C",
                str(self.repo),
                "-c",
                "user.name=Test",
                "-c",
                "user.email=test@example.com",
                "commit",
                "-qm",
                "initial",
            ],
            check=True,
        )

    def run_hook(
        self,
        hook: Path,
        command_field: str,
        command: str = "git commit -m test",
    ):
        payload = {
            "tool_name": "exec_command",
            "tool_input": {
                command_field: command,
                "workdir": str(self.repo),
            },
        }
        return subprocess.run(
            ["bash", str(hook)],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
        )

    def test_allows_test_only_elisp_commit(self):
        for directory in ("test", "tests"):
            test_file = self.repo / directory / "helpers.el"
            test_file.parent.mkdir(exist_ok=True)
            test_file.write_text("(provide 'helpers)\n")
            subprocess.run(
                ["git", "-C", str(self.repo), "add", str(test_file)],
                check=True,
            )
            for hook, command_field in HOOKS:
                with self.subTest(directory=directory, hook=hook):
                    result = self.run_hook(hook, command_field)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(permission_decision(result), "allow")
            subprocess.run(
                ["git", "-C", str(self.repo), "reset", "-q", "HEAD", "--", directory],
                check=True,
            )

    def test_still_requires_docs_for_production_elisp(self):
        source = self.repo / "example.el"
        source.write_text("(provide 'example)\n")
        subprocess.run(
            ["git", "-C", str(self.repo), "add", str(source)],
            check=True,
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "deny")

    def test_allows_quoted_test_path_in_one_shot_add_and_commit(self):
        test_file = self.repo / "test" / "helper file.el"
        test_file.parent.mkdir()
        test_file.write_text("(provide 'helper)\n")
        command = 'git add "test/helper file.el" && git commit -m test'
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "allow")

    def test_requires_docs_for_normalized_production_path(self):
        source = self.repo / "example.el"
        source.write_text("(provide 'example)\n")
        (self.repo / "test").mkdir()
        command = "git add test/../example.el && git commit -m test"
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "deny")

    def test_requires_docs_for_quoted_production_path_with_shell_punctuation(self):
        source = self.repo / "source & support.el"
        source.write_text("(provide 'support)\n")
        command = 'git add "source & support.el" && git commit -m test'
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "deny")


def deny_reason(result: subprocess.CompletedProcess[str]) -> str:
    if not result.stdout.strip():
        return ""
    output = json.loads(result.stdout)
    return output["hookSpecificOutput"].get("permissionDecisionReason", "")


class DocUpdateHookRepoPathTests(unittest.TestCase):
    """Cover a repository whose path contains a space, as ~/My Drive/dotfiles does.

    The other fixtures build their repository directly under `tempfile`, whose
    paths never contain a space, so none of this was exercised before.
    """

    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        # A second repository standing in for the session's own working
        # directory: it has a root README.org and no doc/ directory, so a hook
        # that resolves it instead of the commit's repository refuses.
        self.elsewhere = Path(self.temp_dir.name) / "elsewhere"
        self.elsewhere.mkdir()
        subprocess.run(["git", "init", "-q", str(self.elsewhere)], check=True)
        (self.elsewhere / "README.org").write_text("#+title: Elsewhere\n")
        subprocess.run(
            ["git", "-C", str(self.elsewhere), "add", "README.org"], check=True
        )
        subprocess.run(
            [
                "git", "-C", str(self.elsewhere),
                "-c", "user.name=Test",
                "-c", "user.email=test@example.com",
                "commit", "-qm", "initial",
            ],
            check=True,
        )
        self.repo = Path(self.temp_dir.name) / "My Drive" / "repo"
        self.doc = self.repo / "emacs" / "extras" / "doc"
        self.doc.mkdir(parents=True)
        subprocess.run(["git", "init", "-q", str(self.repo)], check=True)
        # Both a root README.org and a nested doc/ directory, as dotfiles has, so
        # that the two refusal messages can be told apart.
        (self.repo / "README.org").write_text("#+title: Example\n")
        (self.doc / "manual.org").write_text("#+title: Manual\n")
        subprocess.run(["git", "-C", str(self.repo), "add", "-A"], check=True)
        subprocess.run(
            [
                "git", "-C", str(self.repo),
                "-c", "user.name=Test",
                "-c", "user.email=test@example.com",
                "commit", "-qm", "initial",
            ],
            check=True,
        )

    def run_hook(
        self, hook, command_field, command, cwd=None, workdir=None, home=None
    ):
        payload = {
            "tool_name": "exec_command",
            "tool_input": {
                command_field: command,
                "workdir": str(workdir or self.repo),
            },
        }
        env = None
        if home is not None:
            env = {**os.environ, "HOME": str(home)}
        return subprocess.run(
            ["bash", str(hook)],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
            cwd=str(cwd) if cwd else None,
            env=env,
        )

    def test_nested_doc_directory_is_found_when_the_path_has_a_space(self):
        (self.repo / "example.el").write_text("(provide 'example)\n")
        subprocess.run(
            ["git", "-C", str(self.repo), "add", "example.el"], check=True
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, "git commit -m test")
                self.assertEqual(permission_decision(result), "deny")
                self.assertIn("doc/*.org", deny_reason(result))
                self.assertNotIn("README.org is not included", deny_reason(result))

    def test_one_shot_add_and_commit_accepts_a_nested_doc_org(self):
        (self.repo / "example.el").write_text("(provide 'example)\n")
        (self.doc / "manual.org").write_text("#+title: Manual\nUpdated.\n")
        command = "git add example.el emacs/extras/doc/manual.org && git commit -m test"
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_cd_target_is_honoured_when_the_message_uses_command_substitution(self):
        """A commit message built with $(...) must not lose the `cd` target.

        `cd <repo> && git commit -m "$(cat <<EOF ...)"` is the ordinary way to
        write a multi-line message, and the session's own working directory is
        frequently a different repository. Resolving the wrong repository does
        not merely misreport: it evaluates the guard against a tree that has
        neither the staged Elisp nor the staged manual, so it can refuse a
        correct commit and, when the other repository happens to hold a staged
        doc file, wave through one that should be refused.
        """
        (self.repo / "example.el").write_text("(provide 'example)\n")
        (self.doc / "manual.org").write_text("#+title: Manual\nSubstitution.\n")
        command = (
            f'cd "{self.repo}" && git add example.el emacs/extras/doc/manual.org '
            '&& git commit -q -m "$(printf %s subject)"'
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(
                    hook, command_field, command, cwd=self.elsewhere,
                    workdir=self.elsewhere,
                )
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_cd_target_written_through_home_is_resolved(self):
        """`cd "$HOME/..."` is how these paths are normally written.

        The target is extracted by parameter expansion alone, which never
        expands `$HOME`, so the directory test failed on the literal string and
        the session's repository was silently used instead.
        """
        (self.repo / "example.el").write_text("(provide 'example)\n")
        (self.doc / "manual.org").write_text("#+title: Manual\nVia HOME.\n")
        relative = self.repo.relative_to(Path(self.temp_dir.name))
        command = (
            f'cd "$HOME/{relative}" && git add example.el '
            "emacs/extras/doc/manual.org && git commit -q -m subject"
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(
                    hook, command_field, command, cwd=self.elsewhere,
                    workdir=self.elsewhere, home=self.temp_dir.name,
                )
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_a_generated_texi_staged_by_the_same_command_is_accepted(self):
        """The hook runs before the git add it is inspecting.

        Refusing because the generated file is unstaged, when the very command
        being judged stages it, blocks a correct commit.
        """
        manual = self.doc / "manual.org"
        manual.write_text(
            "#+title: Manual\n#+texinfo_filename: manual.info\nBody.\n"
        )
        generated = self.doc / "manual.texi"
        generated.write_text("@node Top\n")
        subprocess.run(
            ["git", "-C", str(self.repo), "add", "-A"], check=True
        )
        subprocess.run(
            [
                "git", "-C", str(self.repo),
                "-c", "user.name=Test", "-c", "user.email=test@example.com",
                "commit", "-qm", "manual",
            ],
            check=True,
        )
        manual.write_text(
            "#+title: Manual\n#+texinfo_filename: manual.info\nBody changed.\n"
        )
        subprocess.run(
            ["git", "-C", str(self.repo), "add", "emacs/extras/doc/manual.org"],
            check=True,
        )
        generated.write_text("@node Top\n@c regenerated\n")
        command = (
            f'cd "{self.repo}" && git add emacs/extras/doc/manual.texi '
            "&& git commit -q -m x"
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertNotIn("generated Texinfo", deny_reason(result))

    def test_an_apostrophe_in_a_heredoc_message_is_not_read_as_arguments(self):
        """Prose in a heredoc is data, not arguments.

        An apostrophe in a commit message ("the hook's own context") makes the
        shell lexer raise, and the parser then fails closed by synthesising a
        .el path, so a commit staging no Elisp at all is refused.
        """
        command = (
            "cat > /tmp/msg.txt <<'EOF'\n"
            "hooks: fix the session's own repository\n"
            "EOF\n"
            f'cd "{self.repo}" && git add README.org && git commit -q -F /tmp/msg.txt'
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_a_heredoc_body_naming_an_el_file_is_not_read_as_arguments(self):
        """A message that talks about Elisp is not a commit of Elisp."""
        command = (
            "cat > /tmp/msg.txt <<'EOF'\n"
            "tests: cover example.el and git commit paths\n"
            "EOF\n"
            f'cd "{self.repo}" && git add README.org && git commit -q -F /tmp/msg.txt'
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_genuinely_unparseable_arguments_still_fail_closed(self):
        """Fail-closed is kept for argument text that cannot be lexed."""
        (self.repo / "example.el").write_text("(provide 'example)\n")
        command = f'cd "{self.repo}" && git add example.el \'unclosed && git commit -q -m x'
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(permission_decision(result), "deny")

    def test_a_cd_after_a_preamble_still_decides_the_repository(self):
        """The cd need not be the first thing in the command.

        Writing a message to a file, or setting a variable, before changing
        directory is ordinary, and the repository the command commits in is the
        same either way.
        """
        (self.repo / "example.el").write_text("(provide 'example)\n")
        (self.doc / "manual.org").write_text("#+title: Manual\nAfter preamble.\n")
        command = (
            "MSG=/tmp/some-message.txt && "
            f'cd "{self.repo}" && git add example.el '
            "emacs/extras/doc/manual.org && git commit -q -F $MSG"
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(
                    hook, command_field, command, cwd=self.elsewhere,
                    workdir=self.elsewhere,
                )
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_the_last_cd_in_the_chain_decides_the_repository(self):
        (self.repo / "example.el").write_text("(provide 'example)\n")
        (self.doc / "manual.org").write_text("#+title: Manual\nLast cd.\n")
        command = (
            f'cd "{self.elsewhere}" && echo staging ; cd "{self.repo}" && '
            "git add example.el emacs/extras/doc/manual.org && git commit -q -m x"
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(
                    hook, command_field, command, cwd=self.elsewhere,
                    workdir=self.elsewhere,
                )
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_an_unresolvable_cd_target_is_not_swapped_for_the_session_repo(self):
        """An unresolvable target must not be answered from a different tree.

        Evaluating the session's repository instead is unsound in both
        directions: it refuses correct commits, and it approves ones it should
        refuse whenever that other tree happens to hold a staged manual.
        """
        (self.repo / "example.el").write_text("(provide 'example)\n")
        command = (
            'cd "$SOME_UNSET_VARIABLE/repo" && git add example.el '
            "&& git commit -q -m subject"
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(
                    hook, command_field, command, cwd=self.elsewhere,
                    workdir=self.elsewhere,
                )
                self.assertNotIn("README.org", deny_reason(result))

    def test_one_shot_add_and_commit_does_not_depend_on_the_hook_cwd(self):
        (self.repo / "example.el").write_text("(provide 'example)\n")
        (self.doc / "manual.org").write_text("#+title: Manual\nUpdated again.\n")
        command = "git add example.el emacs/extras/doc/manual.org && git commit -m test"
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(
                    hook, command_field, command, cwd=self.elsewhere
                )
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )


if __name__ == "__main__":
    unittest.main()
