from __future__ import annotations

import json
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

    def run_hook(self, hook, command_field, command, cwd=None, workdir=None):
        payload = {
            "tool_name": "exec_command",
            "tool_input": {
                command_field: command,
                "workdir": str(workdir or self.repo),
            },
        }
        return subprocess.run(
            ["bash", str(hook)],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
            cwd=str(cwd) if cwd else None,
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
