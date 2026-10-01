import sys
import unittest
from unittest import mock
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import Call  # noqa: E402
from agent_guard.policies import ahrefs  # noqa: E402


def call(runtime, tool, command):
    return Call.from_payload(runtime, {"tool_name": tool, "tool_input": {"command": command}, "cwd": "/tmp"},
                             env={"HOME": "/Users/tester"})


def decision(runtime, command, tool="Bash"):
    return ahrefs.decide(call(runtime, tool, command)).decision


class FailureTests(unittest.TestCase):
    def test_helper_failures_propagate(self):
        for helper in ("mask_heredoc_bodies", "mask_git_commit_messages", "grep"):
            with self.subTest(helper=helper), mock.patch.object(ahrefs, helper, side_effect=RuntimeError):
                with self.assertRaises(RuntimeError):
                    ahrefs.decide(call("claude", "Bash", "curl api.ahrefs.com/x"))


class AhrefsTests(unittest.TestCase):
    def test_raw_paid_call_denied(self):
        result = ahrefs.decide(call("claude", "Bash", "curl https://api.ahrefs.com/v3/site-explorer/metrics"))
        self.assertEqual((result.decision, result.reason), ("deny", ahrefs.REASON))

    def test_guard_and_free_quota_probe_allowed(self):
        self.assertEqual(decision("claude", "~/bin/ahrefs-api-guard request https://api.ahrefs.com/v3/x"), "allow")
        self.assertEqual(decision("codex", "curl api.ahrefs.com/v3/subscription-info/limits-and-usage",
                                  "exec_command"), "allow")
        self.assertEqual(decision("codex", "curl api.ahrefs.com/v3/subscription-info/limits-and-usage?gsc=1",
                                  "exec_command"), "deny")
        self.assertEqual(decision("claude", "xahrefs-api-guard api.ahrefs.com"), "deny")

    def test_prose_mentions_allowed(self):
        self.assertEqual(decision("claude", "git commit -m 'call api.ahrefs.com via the guard'"), "allow")
        self.assertEqual(decision("claude", "cat <<'EOF' > notes.md\napi.ahrefs.com\nEOF"), "allow")

    def test_shell_tools_by_runtime(self):
        command = "curl api.ahrefs.com/v3/site-audit"
        self.assertEqual(decision("claude", command, "exec_command"), "allow")
        self.assertEqual(decision("codex", command, "functions.exec_command"), "deny")
        self.assertEqual(decision("codex", command, "Read"), "allow")

    def test_dispatcher_check(self):
        self.assertEqual(ahrefs.dispatcher_check(call("claude", "Bash", "curl api.ahrefs.com/x")), [ahrefs.REASON])
        self.assertEqual(ahrefs.dispatcher_check(call("claude", "Bash", "ls")), [])


if __name__ == "__main__":
    unittest.main()
