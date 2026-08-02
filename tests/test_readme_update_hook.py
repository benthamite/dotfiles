from __future__ import annotations

import json
import subprocess
import tempfile
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
HOOKS = (
    (DOTFILES / "claude/hooks/require-readme-update.sh", "command"),
    (DOTFILES / "codex/hooks/require-readme-update.sh", "cmd"),
)


def permission_decision(result: subprocess.CompletedProcess[str]) -> str:
    if not result.stdout.strip():
        return "allow"
    return json.loads(result.stdout)["hookSpecificOutput"].get(
        "permissionDecision", "allow"
    )


class ReadmeUpdateHookTests(unittest.TestCase):
    """Cover the guard that ties a claude/ change to the Claude overview."""

    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        self.repo = Path(self.temp_dir.name) / "dotfiles"
        for relative in ("claude/skills/example", "claude/bin", "codex/skills/example"):
            (self.repo / relative).mkdir(parents=True)
        subprocess.run(["git", "init", "-q", str(self.repo)], check=True)
        (self.repo / "claude/README.org").write_text("#+title: Claude\n")
        (self.repo / "claude/bin/README.org").write_text("#+title: Commands\n")
        (self.repo / "claude/skills/example/SKILL.md").write_text("---\nname: example\n---\n")
        subprocess.run(["git", "-C", str(self.repo), "add", "-A"], check=True)
        subprocess.run(
            [
                "git", "-C", str(self.repo),
                "-c", "user.name=Test", "-c", "user.email=test@example.com",
                "commit", "-qm", "initial",
            ],
            check=True,
        )

    def run_hook(self, hook: Path, command_field: str, command: str):
        payload = {
            "tool_name": "exec_command",
            "tool_input": {command_field: command, "workdir": str(self.repo)},
        }
        return subprocess.run(
            ["bash", str(hook)],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
        )

    def change_skill(self):
        (self.repo / "claude/skills/example/SKILL.md").write_text(
            "---\nname: example\n---\nChanged.\n"
        )

    def test_a_skill_change_without_the_overview_is_refused(self):
        self.change_skill()
        subprocess.run(
            ["git", "-C", str(self.repo), "add", "claude/skills/example/SKILL.md"],
            check=True,
        )
        for hook, field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, field, "git commit -m test")
                self.assertEqual(permission_decision(result), "deny")

    def test_a_skill_change_with_the_overview_is_allowed(self):
        self.change_skill()
        (self.repo / "claude/README.org").write_text("#+title: Claude\nUpdated.\n")
        subprocess.run(
            [
                "git", "-C", str(self.repo), "add",
                "claude/skills/example/SKILL.md", "claude/README.org",
            ],
            check=True,
        )
        for hook, field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, field, "git commit -m test")
                self.assertEqual(permission_decision(result), "allow")

    def test_a_different_readme_does_not_satisfy_the_requirement(self):
        """Staging some other README must not stand in for the Claude overview.

        The check matched the bare substring README.org anywhere in the command,
        so `git add claude/skills/... claude/bin/README.org && git commit` passed
        the guard and the overview went unwritten.
        """
        self.change_skill()
        command = (
            "git add claude/skills/example/SKILL.md claude/bin/README.org "
            "&& git commit -m test"
        )
        for hook, field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, field, command)
                self.assertEqual(permission_decision(result), "deny")

    def test_the_overview_named_in_a_one_shot_add_is_accepted(self):
        self.change_skill()
        command = (
            "git add claude/skills/example/SKILL.md claude/README.org "
            "&& git commit -m test"
        )
        for hook, field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, field, command)
                self.assertEqual(permission_decision(result), "allow")

    def test_the_overview_is_not_satisfied_by_prose_mentioning_it(self):
        """A commit message that talks about the README is not an update to it."""
        self.change_skill()
        command = (
            "git add claude/skills/example/SKILL.md "
            "&& git commit -m 'note: claude/README.org still needs this'"
        )
        for hook, field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, field, command)
                self.assertEqual(permission_decision(result), "deny")

    def test_a_change_outside_claude_is_not_governed(self):
        (self.repo / "codex/skills/example/SKILL.md").write_text("---\nname: x\n---\n")
        subprocess.run(["git", "-C", str(self.repo), "add", "-A"], check=True)
        for hook, field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, field, "git commit -m test")
                self.assertEqual(permission_decision(result), "allow")


if __name__ == "__main__":
    unittest.main()
