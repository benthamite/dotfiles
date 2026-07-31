"""Behaviour tests for the destructive-command guard.

Nothing covered this hook until 2026-07-31, which is how its git clone branch
stayed an ask-style gate for four months after CLAUDE.md required guards to
allow or deny with a reason and never escalate.

Fixture targets deliberately use example-org/example-repo rather than a real
Epoch repository: block-github-write-command.sh scans raw command text without
the quoted-content masking that block-destructive-command.sh does, so a real
owner name in a fixture trips that guard when this file is edited.
"""

from __future__ import annotations

import json
import subprocess
import unittest
from pathlib import Path

DOTFILES = Path(__file__).resolve().parents[1]
DESTRUCTIVE_GUARDS = (
    DOTFILES / "claude/hooks/block-destructive-command.sh",
    DOTFILES / "codex/hooks/block-destructive-command.sh",
)

CLONE_URL = "https://example.test/example-org/example-repo.git"
CLONE_SLUG = "example-org/example-repo"


def run_hook(script: Path, command: str) -> subprocess.CompletedProcess[str]:
    payload = {"tool_name": "Bash", "tool_input": {"command": command}}
    return subprocess.run(
        ["bash", str(script)],
        input=json.dumps(payload),
        text=True,
        capture_output=True,
        check=False,
    )


class CloneRuleTests(unittest.TestCase):
    @property
    def guards(self):
        return DESTRUCTIVE_GUARDS

    def decide(self, guard: Path, command: str) -> tuple[str, str]:
        result = run_hook(guard, command)
        self.assertEqual(result.returncode, 0, result.stderr)
        if not result.stdout.strip():
            return "allow", ""
        specific = json.loads(result.stdout)["hookSpecificOutput"]
        return (
            specific.get("permissionDecision", "allow"),
            specific.get("permissionDecisionReason", ""),
        )

    def test_no_guard_ever_escalates_a_clone_to_the_user(self):
        """The regression that mattered: a guard must decide, not prompt."""
        for guard in self.guards:
            for command in (
                f"git clone {CLONE_URL}",
                f"ALLOW_CLONE=1 git clone {CLONE_URL}",
            ):
                with self.subTest(guard=guard.name, command=command):
                    decision, _ = self.decide(guard, command)
                    self.assertNotEqual(decision, "ask")

    def test_unrequested_clone_is_denied(self):
        for guard in self.guards:
            for command in (
                f"git clone {CLONE_URL}",
                f"gh repo clone {CLONE_SLUG}",
            ):
                with self.subTest(guard=guard.name, command=command):
                    decision, reason = self.decide(guard, command)
                    self.assertEqual(decision, "deny")
                    self.assertIn("ALLOW_CLONE=1", reason)

    def test_requested_clone_proceeds_with_the_escape_hatch(self):
        """A clone Pablo asked for must not be a dead end."""
        for guard in self.guards:
            for command in (
                f"ALLOW_CLONE=1 git clone {CLONE_URL}",
                f"ALLOW_CLONE=1 gh repo clone {CLONE_SLUG}",
            ):
                with self.subTest(guard=guard.name, command=command):
                    decision, _ = self.decide(guard, command)
                    self.assertEqual(decision, "allow")

    def test_escape_hatch_is_scoped_to_the_clone_rule(self):
        """It must not become a skeleton key for the rest of the hook.

        A global early exit on the variable would have been simpler to write and
        would have waved through every other rule in the file.
        """
        for guard in self.guards:
            for command in (
                "ALLOW_CLONE=1 rm -rf /tmp/example-guard-target",
                "ALLOW_CLONE=1 git reset --hard",
                "ALLOW_CLONE=1 git clean -f",
            ):
                with self.subTest(guard=guard.name, command=command):
                    decision, _ = self.decide(guard, command)
                    self.assertEqual(decision, "deny")

    def test_ordinary_commands_are_untouched(self):
        for guard in self.guards:
            with self.subTest(guard=guard.name):
                decision, _ = self.decide(guard, "git status --short")
                self.assertEqual(decision, "allow")


class DispatcherCloneRuleTests(CloneRuleTests):
    """Claude's live path.

    block-destructive-command.sh is not registered in settings.json at all;
    pretooluse-bash.sh inlines its checks so the Bash matcher emits one
    aggregated decision. Testing only the standalone script would have passed
    while Claude still asked on every clone.
    """

    DISPATCHER = DOTFILES / "claude/hooks/pretooluse-bash.sh"

    def setUp(self):
        if not self.DISPATCHER.exists():
            self.skipTest("dispatcher not present")
        self._guards = (self.DISPATCHER,)

    @property
    def guards(self):
        return self._guards


class NoAskDecisionTests(unittest.TestCase):
    GUARDS = DESTRUCTIVE_GUARDS + (DOTFILES / "claude/hooks/pretooluse-bash.sh",)

    def test_no_guard_can_emit_an_ask_decision(self):
        """Structural, not incidental: no ask emitter may exist at all."""
        for guard in self.GUARDS:
            with self.subTest(guard=guard.name):
                text = guard.read_text()
                self.assertNotIn('"permissionDecision": "ask"', text)
                self.assertNotIn('permissionDecision:"ask"', text)

    def test_dispatcher_has_no_ask_accumulator(self):
        dispatcher = DOTFILES / "claude/hooks/pretooluse-bash.sh"
        self.assertNotIn("ASK_REASONS", dispatcher.read_text())


if __name__ == "__main__":
    unittest.main()
