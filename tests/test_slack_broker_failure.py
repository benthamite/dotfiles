"""The Slack wrapper says how a credential broker lookup failed, without the value."""
import importlib.util
import io
import contextlib
import subprocess
import unittest
from pathlib import Path
from unittest import mock

SOURCE = Path(__file__).resolve().parents[1] / "claude/bin/slack.py"
spec = importlib.util.spec_from_file_location("slack_broker_fixture", SOURCE)
SLACK = importlib.util.module_from_spec(spec)
spec.loader.exec_module(SLACK)
REF = "op://Automations/Slack MCP - Epoch Unofficial/xoxc_token"


class BrokerFailureTests(unittest.TestCase):
    def failure_message(self, **run_behaviour):
        err = io.StringIO()
        with mock.patch.object(SLACK.subprocess, "run", **run_behaviour), \
                contextlib.redirect_stderr(err), self.assertRaises(SystemExit) as raised:
            SLACK._op_read(REF)
        self.assertEqual(raised.exception.code, 1)
        return err.getvalue()

    def test_nonzero_exit_reports_status_and_op_error_text(self):
        completed = subprocess.CompletedProcess(
            ["op-automations", "read", REF], 1, stdout="",
            stderr="[ERROR] 2026/09/10 12:45:17 could not read secret: item does not have a field 'token'\n",
        )
        message = self.failure_message(return_value=completed)
        self.assertIn("(exit 1)", message)
        self.assertIn("does not have a field 'token'", message)

    def test_token_shaped_runs_in_stderr_are_masked(self):
        completed = subprocess.CompletedProcess(
            ["op-automations", "read", REF], 1, stdout="",
            stderr="unexpected: xoxb-ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 leaked\n",
        )
        message = self.failure_message(return_value=completed)
        self.assertNotIn("ABCDEFGHIJKLMNOPQRSTUVWXYZ", message)
        self.assertIn("unexpected:", message)

    def test_timeout_is_named(self):
        message = self.failure_message(side_effect=subprocess.TimeoutExpired(["op-automations"], 30))
        self.assertIn("timed out after 30s", message)

    def test_missing_broker_is_named_without_path_details(self):
        message = self.failure_message(side_effect=FileNotFoundError(2, "no such file", "op-automations"))
        self.assertIn("could not run: FileNotFoundError", message)
        self.assertNotIn("no such file", message)


if __name__ == "__main__":
    unittest.main()
