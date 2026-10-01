import json
import sys
import types
import unittest
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parent))
from guard_harness import run_engine, run_guard  # noqa: E402

from agent_guard.core import HookFailure, Result  # noqa: E402
from agent_guard.policies import dispatcher  # noqa: E402


def run_hook(script, payload):
    # wrap-bash-output.sh's rewrite is the dispatcher's last step; production
    # runs it as part of the full Claude Bash chain.
    if script == "wrap-bash-output.sh":
        return run_engine("claude", payload)
    return run_guard("claude/hooks/" + script, payload)


class ClaudeBashPreToolUseTest(unittest.TestCase):
    def assert_rewrite_preserves_bash_input_fields(self, script):
        payload = {
            "tool_name": "Bash",
            "tool_input": {
                "command": "printenv",
                "timeout": 600000,
                "run_in_background": True,
                "description": "long local eval",
            },
        }
        result = run_hook(script, payload)

        self.assertEqual(result.returncode, 0, result.stderr)
        output = json.loads(result.stdout)
        updated_input = output["hookSpecificOutput"]["updatedInput"]

        self.assertIn("redact-secrets.sh", updated_input["command"])
        self.assertEqual(updated_input["timeout"], 600000)
        self.assertIs(updated_input["run_in_background"], True)
        self.assertEqual(updated_input["description"], "long local eval")

    def test_command_rewrite_preserves_bash_input_fields(self):
        self.assert_rewrite_preserves_bash_input_fields("pretooluse-bash.sh")

    def test_standalone_wrap_rewrite_preserves_bash_input_fields(self):
        self.assert_rewrite_preserves_bash_input_fields("wrap-bash-output.sh")

    def assert_benign_command_is_not_rewritten(self, script):
        payload = {
            "tool_name": "Bash",
            "tool_input": {"command": "git status --short"},
        }
        result = run_hook(script, payload)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")

    def test_dispatcher_leaves_benign_command_unchanged(self):
        self.assert_benign_command_is_not_rewritten("pretooluse-bash.sh")

    def test_standalone_wrap_leaves_benign_command_unchanged(self):
        self.assert_benign_command_is_not_rewritten("wrap-bash-output.sh")


class DelegatedGuardFailuresTest(unittest.TestCase):
    """Run the real dispatcher against a disposable delegated policy.

    The shell dispatcher parsed each delegated hook's stdout; in-process a
    policy returns a Result or raises, so the malformed-output, invalid-shape,
    "ask" and empty-stdout cases have no equivalent and were removed.
    """

    def setUp(self):
        # Every other policy the dispatcher delegates to allows, so each test
        # observes the disposable policy alone.
        allow = types.SimpleNamespace(decide=lambda call: Result.allow())
        for name in ("untrusted_execution", "github_write", "elpaca_rebuild", "long_pipeline"):
            patcher = mock.patch.object(dispatcher, name, allow)
            patcher.start()
            self.addCleanup(patcher.stop)

    def run_dispatcher(self, decide):
        payload = {"tool_name": "Bash", "session_id": "disposable-delegate-fixture",
                   "tool_input": {"command": "git status --short"}}
        with mock.patch.object(dispatcher, "secret_leak", types.SimpleNamespace(decide=decide)):
            return run_guard("claude/hooks/pretooluse-bash.sh", payload)

    def assert_denied(self, result):
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(result.stdout.strip(), "dispatcher silently accepted the guard failure")
        output = result.decision()
        self.assertEqual(output["permissionDecision"], "deny")
        self.assertTrue(output["permissionDecisionReason"])
        return output["permissionDecisionReason"]

    def test_crash_denies_without_echoing_guard_output(self):
        marker = "PRIVATE_FIXTURE_DO_NOT_ECHO"

        def crash(call):
            raise HookFailure(marker, status=7)

        result = self.run_dispatcher(crash)
        reason = self.assert_denied(result)
        self.assertIn("block-secret-leak.sh", reason)
        self.assertIn("7", reason)
        self.assertNotIn(marker, result.stdout + result.stderr)

    def test_unexpected_exception_fails_closed(self):
        def broken(call):
            raise RuntimeError("PRIVATE_FIXTURE_DO_NOT_ECHO")

        result = self.run_dispatcher(broken)
        self.assertEqual((result.returncode, result.stdout), (2, ""))
        self.assertNotIn("PRIVATE_FIXTURE_DO_NOT_ECHO", result.stderr)

    def test_policy_denial_is_preserved(self):
        result = self.run_dispatcher(lambda call: Result.deny("Fixture policy denial"))
        self.assertEqual(self.assert_denied(result), "Fixture policy denial")

    def test_denial_without_reason_still_denies(self):
        reason = self.assert_denied(self.run_dispatcher(lambda call: Result.deny("")))
        self.assertIn("block-secret-leak.sh", reason)

    def test_allow_remains_allowed(self):
        result = self.run_dispatcher(lambda call: Result.allow())
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")


if __name__ == "__main__":
    unittest.main()
