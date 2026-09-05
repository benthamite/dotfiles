import json
import shlex
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def run_hook(script, payload):
    return subprocess.run(
        [str(ROOT / "claude" / "hooks" / script)],
        input=json.dumps(payload),
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )


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
    """Run the real dispatcher against a disposable standalone guard."""

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="delegate-test-")
        self.addCleanup(self.temp.cleanup)
        self.directory = Path(self.temp.name)
        for name in ("pretooluse-bash.sh", "lib-heredoc.sh"):
            (self.directory / name).write_bytes((ROOT / "claude/hooks" / name).read_bytes())
        self.guard = self.directory / "block-secret-leak.sh"

    def run_dispatcher(self, stdout="", stderr="", status=0):
        self.guard.write_text(
            "#!/bin/bash\ncat >/dev/null\n"
            f"printf '%s' {shlex.quote(stdout)}\n"
            f"printf '%s' {shlex.quote(stderr)} >&2\nexit {status}\n"
        )
        payload = {"tool_name": "Bash", "session_id": "disposable-delegate-fixture",
                   "tool_input": {"command": "git status --short"}}
        return subprocess.run(["bash", str(self.directory / "pretooluse-bash.sh")],
                              input=json.dumps(payload), text=True, capture_output=True)

    def assert_denied(self, result):
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(result.stdout.strip(), "dispatcher silently accepted the guard failure")
        output = json.loads(result.stdout)["hookSpecificOutput"]
        self.assertEqual(output["permissionDecision"], "deny")
        self.assertTrue(output["permissionDecisionReason"])
        return output["permissionDecisionReason"]

    def test_crash_denies_without_echoing_guard_output(self):
        marker = "PRIVATE_FIXTURE_DO_NOT_ECHO"
        result = self.run_dispatcher(stdout=marker, stderr=marker, status=7)
        reason = self.assert_denied(result)
        self.assertIn("block-secret-leak.sh", reason)
        self.assertIn("7", reason)
        self.assertNotIn(marker, result.stdout + result.stderr)

    def test_compact_json_denial_is_preserved(self):
        result = self.run_dispatcher(json.dumps({"hookSpecificOutput": {
            "permissionDecision": "deny", "permissionDecisionReason": "Fixture policy denial",
        }}, separators=(",", ":")))
        self.assertEqual(self.assert_denied(result), "Fixture policy denial")

    def test_malformed_output_denies_without_echoing_it(self):
        marker = "PRIVATE_FIXTURE_DO_NOT_ECHO"
        result = self.run_dispatcher(marker)
        self.assert_denied(result)
        self.assertNotIn(marker, result.stdout + result.stderr)

    def test_invalid_shapes_and_interactive_decisions_deny(self):
        for response in ("null", "[]", "{}", "{}\n{}",
                         '{"hookSpecificOutput":{"permissionDecision":"ask"}}',
                         '{"hookSpecificOutput":{"permissionDecision":"unknown"}}'):
            with self.subTest(response=response):
                self.assert_denied(self.run_dispatcher(response))

    def test_empty_success_and_structured_allow_remain_allowed(self):
        for response in ("", " \n", '{"hookSpecificOutput":{"permissionDecision":"allow"}}'):
            with self.subTest(response=response):
                result = self.run_dispatcher(response)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "")


if __name__ == "__main__":
    unittest.main()
