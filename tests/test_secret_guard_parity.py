"""Parity tests for the Claude and Codex secret-leak guards.

Both copies of block-secret-leak.sh must enforce the same 1Password policy:
- direct `op` commands are denied,
- the unbatched `env -u OP_SERVICE_ACCOUNT_TOKEN op ...` form is denied,
- the explicit batched fallback `env -u OP_SERVICE_ACCOUNT_TOKEN bash -c '...'`
  is allowed,
- the `op-automations` and `op-desktop` wrappers are allowed,
- deny messages advise `op-desktop`, not a path the policy blocks.
"""

from __future__ import annotations

import json
import subprocess
import unittest
from pathlib import Path


DOTFILES = Path("/Users/pablostafforini/My Drive/dotfiles")
GUARDS = {
    "claude": DOTFILES / "claude" / "hooks" / "block-secret-leak.sh",
    "codex": DOTFILES / "codex" / "hooks" / "block-secret-leak.sh",
}


def run_guard(guard: Path, command: str) -> dict | None:
    """Run a guard with a synthetic Bash payload; return its decision JSON."""
    payload = json.dumps({"tool_name": "Bash", "tool_input": {"command": command}})
    result = subprocess.run(
        ["bash", str(guard)],
        input=payload,
        capture_output=True,
        text=True,
        check=True,
    )
    if not result.stdout.strip():
        return None
    return json.loads(result.stdout)


def decision(output: dict | None) -> str:
    if output is None:
        return "allow"
    return output["hookSpecificOutput"]["permissionDecision"]


class SecretGuardParityTests(unittest.TestCase):
    def assert_both(self, command: str, expected: str) -> None:
        for tool, guard in GUARDS.items():
            with self.subTest(tool=tool, command=command):
                self.assertEqual(decision(run_guard(guard, command)), expected)

    def test_direct_op_is_denied(self):
        self.assert_both("op read op://Employee/Example/credential", "deny")

    def test_unbatched_env_u_op_is_denied(self):
        self.assert_both(
            "env -u OP_SERVICE_ACCOUNT_TOKEN op read op://Employee/Example/credential",
            "deny",
        )

    def test_batched_fallback_shell_is_allowed(self):
        self.assert_both(
            "env -u OP_SERVICE_ACCOUNT_TOKEN bash -c 'op read op://Employee/Example/credential > /dev/null'",
            "allow",
        )

    def test_op_automations_is_allowed(self):
        self.assert_both(
            "op-automations read op://Automations/Example/credential > /dev/null",
            "allow",
        )

    def test_op_desktop_is_allowed(self):
        self.assert_both(
            "op-desktop read op://Employee/Example/credential > /dev/null",
            "allow",
        )

    def test_deny_message_advises_op_desktop(self):
        for tool, guard in GUARDS.items():
            with self.subTest(tool=tool):
                output = run_guard(guard, "op read op://Employee/Example/credential")
                reason = output["hookSpecificOutput"]["permissionDecisionReason"]
                self.assertIn("op-desktop", reason)
                self.assertNotIn("env -u OP_SERVICE_ACCOUNT_TOKEN op ", reason)


if __name__ == "__main__":
    unittest.main()
