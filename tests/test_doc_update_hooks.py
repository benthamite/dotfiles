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


if __name__ == "__main__":
    unittest.main()
