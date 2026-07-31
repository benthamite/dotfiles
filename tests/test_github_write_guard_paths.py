"""Tests for the write guard's self-protected-path check.

The check used to be two independent conditions: does the command mention a
protected path anywhere, and does it contain a redirection or file-mutating verb
anywhere. Any ">" satisfied the second one, so on 2026-07-31 a commit was refused
because its message contained the phrase "deny > allow" while it also staged one
of these files by name. The operator now has to name the path, which removes that
class and is strictly more precise: an operator far from the path was never
evidence about the path.

Protected path fragments are assembled at runtime rather than written as literals,
because these very guards scan the text of the command that edits this file.
"""

from __future__ import annotations

import json
import subprocess
import unittest
from pathlib import Path

DOTFILES = Path(__file__).resolve().parents[1]
WRITE_GUARDS = (
    DOTFILES / "claude/hooks/block-github-write-command.sh",
    DOTFILES / "codex/hooks/block-github-write-command.sh",
)

# e.g. "claude/hooks/pretooluse-bash.sh" without ever spelling it in one piece.
PROTECTED = "claude/hooks/" + "pretooluse" + "-bash.sh"
ALLOWLIST = "agents/github-write" + "-allowlist.txt"


def decide(guard: Path, command: str) -> str:
    payload = {"tool_name": "Bash", "tool_input": {"command": command}}
    result = subprocess.run(
        ["bash", str(guard)],
        input=json.dumps(payload),
        text=True,
        capture_output=True,
        check=False,
    )
    if result.returncode != 0:
        raise AssertionError(f"{guard.name} exited {result.returncode}: {result.stderr}")
    if not result.stdout.strip():
        return "allow"
    return json.loads(result.stdout)["hookSpecificOutput"]["permissionDecision"]


class ProtectedPathWriteTests(unittest.TestCase):
    def assert_decision(self, command: str, expected: str) -> None:
        for guard in WRITE_GUARDS:
            with self.subTest(guard=guard.parent.parent.name, command=command):
                self.assertEqual(decide(guard, command), expected)

    # --- writes that must stay blocked ---

    def test_redirection_onto_a_protected_path_is_denied(self):
        for command in (
            f"printf 'x' > {PROTECTED}",
            f"printf 'x' >{PROTECTED}",
            f"printf 'x' >> {PROTECTED}",
            f'printf "x" > "{PROTECTED}"',
            f"printf 'x' > ~/My\\ Drive/dotfiles/{PROTECTED}",
            f"cat /dev/null > {ALLOWLIST}",
        ):
            self.assert_decision(command, "deny")

    def test_mutating_verbs_naming_a_protected_path_are_denied(self):
        for command in (
            f"rm {PROTECTED}",
            f"trash {PROTECTED}",
            f"mv {PROTECTED} /tmp/stashed",
            f"cp /tmp/evil.sh {PROTECTED}",
            f"install -m 755 /tmp/evil.sh {PROTECTED}",
            f"tee {PROTECTED}",
            f"sed -i '' -e 's/deny/allow/' {PROTECTED}",
            f"perl -pi -e 's/deny/allow/' {PROTECTED}",
            f"git checkout -- {PROTECTED}",
            f"git restore {PROTECTED}",
        ):
            self.assert_decision(command, "deny")

    # --- mentions that must not be blocked ---

    def test_staging_a_protected_path_with_a_gt_in_the_message_is_allowed(self):
        """The exact false positive: a '>' in prose, far from the path."""
        self.assert_decision(
            f"git add ai-config-sync.json {PROTECTED} && "
            "git commit -m 'precedence drops to deny > allow'",
            "allow",
        )

    def test_heredoc_message_containing_a_gt_is_allowed(self):
        self.assert_decision(
            f"git add {PROTECTED} && git commit -F - <<'EOF'\n"
            "Close the rule\n\nprecedence drops to deny > allow\nEOF",
            "allow",
        )

    def test_reading_a_protected_path_is_allowed(self):
        for command in (
            f"grep -n ask {PROTECTED}",
            f"sed -n '1,20p' {PROTECTED}",
            f"wc -l {PROTECTED}",
            f"git diff {PROTECTED}",
            f"git add {PROTECTED}",
        ):
            self.assert_decision(command, "allow")

    def test_unrelated_redirection_alongside_a_read_is_allowed(self):
        """A redirection in a different command segment is not about the path."""
        self.assert_decision(f"printf 'x' > /tmp/scratch; grep -c ask {PROTECTED}", "allow")


if __name__ == "__main__":
    unittest.main()
