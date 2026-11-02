"""Offline regression coverage for posting an approved message as Pablo."""

import importlib.util
import io
import json
from pathlib import Path
import sys
from types import SimpleNamespace
import unittest
from unittest import mock


SOURCE = Path(__file__).resolve().parents[1] / "claude/bin/slack.py"
# slack.py imports agent_prefix from its own directory.
sys.path.insert(0, str(SOURCE.parent))
PERMALINK = "https://forethought-ea.slack.com/archives/D0C8CR24NQZ/p1793185462048979"
REPLY_LINK = PERMALINK + "?thread_ts=1793185400.000100&cid=D0C8CR24NQZ"


def send_args(**overrides):
    args = dict(permalink=None, channel=None, thread_ts=None, text="Done.", stdin=False,
                reviewed=None, programmatic=False)
    args.update(overrides)
    return SimpleNamespace(**args)


class SendTests(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location("slack_send_test", SOURCE)
        self.mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.mod)
        self.mod._workspace = "forethought"
        import agent_prefix
        self.enterContext(mock.patch.object(agent_prefix, "FORETHOUGHT", Path.cwd()))
        self.call = self.enterContext(mock.patch.object(self.mod, "call", side_effect=self.fake_call))
        self.clear = self.enterContext(mock.patch.object(self.mod, "_clear_staged_draft",
                                                         return_value="nothing-staged"))
        self.enterContext(mock.patch.object(self.mod, "_tokens", side_effect=AssertionError("No credentials")))
        self.output = self.enterContext(mock.patch("sys.stdout", new_callable=io.StringIO))
        self.errors = self.enterContext(mock.patch("sys.stderr", new_callable=io.StringIO))

    def fake_call(self, method, **params):
        message = {"ts": "1793190000.000200", "text": params["text"]}
        if params.get("thread_ts"):
            message["thread_ts"] = params["thread_ts"]
        return {"ok": True, "channel": params["channel"], "ts": message["ts"], "message": message}

    def posted(self):
        return [c.kwargs for c in self.call.call_args_list if c.args[0] == "chat.postMessage"]

    def receipt(self):
        return json.loads(self.output.getvalue())

    def test_permalink_replies_in_that_messages_thread(self):
        self.mod.cmd_send(send_args(permalink=PERMALINK))
        self.assertEqual(self.posted(), [{"channel": "D0C8CR24NQZ", "text": "[agent] Done.",
                                          "thread_ts": "1793185462.048979"}])
        self.assertEqual(self.receipt()["permalink"], "https://forethought-ea.slack.com/archives/D0C8CR24NQZ/"
                         "p1793190000000200?thread_ts=1793185462.048979&cid=D0C8CR24NQZ")

    def test_permalink_to_a_reply_joins_its_parent_thread(self):
        self.mod.cmd_send(send_args(permalink=REPLY_LINK))
        self.assertEqual(self.posted()[0]["thread_ts"], "1793185400.000100")

    def test_channel_posts_at_top_level(self):
        self.mod.cmd_send(send_args(channel="C0C9VPUQ9SN"))
        self.assertEqual(self.posted(), [{"channel": "C0C9VPUQ9SN", "text": "[agent] Done.", "thread_ts": None}])
        self.assertEqual(self.receipt()["status"], "sent")

    def test_programmatic_send_has_no_prefix(self):
        self.mod.cmd_send(send_args(channel="C0C9VPUQ9SN", programmatic=True))
        self.assertEqual(self.posted()[0]["text"], "Done.")

    def test_stdin_drops_one_trailing_newline(self):
        with mock.patch("sys.stdin", io.StringIO("Line one.\nLine two.\n\n")):
            self.mod.cmd_send(send_args(channel="C0C9VPUQ9SN", text=None, stdin=True))
        self.assertEqual(self.posted()[0]["text"], "[agent] Line one.\nLine two.\n")

    def test_blank_text_sends_nothing(self):
        with self.assertRaises(SystemExit) as raised:
            self.mod.cmd_send(send_args(channel="C0C9VPUQ9SN", text="  \n"))
        self.assertEqual(raised.exception.code, 2)
        self.assertEqual(self.posted(), [])

    def test_permalink_from_another_workspace_sends_nothing(self):
        with self.assertRaises(SystemExit) as raised:
            self.mod.cmd_send(send_args(permalink=PERMALINK.replace("forethought-ea", "epochai")))
        self.assertEqual(raised.exception.code, 2)
        self.assertEqual(self.posted(), [])

    def test_thread_ts_with_permalink_is_refused(self):
        with self.assertRaises(SystemExit):
            self.mod.cmd_send(send_args(permalink=PERMALINK, thread_ts="1793185462.048979"))
        self.assertEqual(self.posted(), [])

    def test_no_response_exits_3_and_is_not_retried(self):
        self.call.side_effect = self.mod._NoResponse(1)
        with self.assertRaises(SystemExit) as raised:
            self.mod.cmd_send(send_args(channel="C0C9VPUQ9SN"))
        self.assertEqual(raised.exception.code, 3)
        self.assertEqual(len(self.posted()), 1)
        self.assertIn("may or may not have been sent", self.errors.getvalue())

    def test_slack_rejection_exits_1(self):
        self.call.side_effect = SystemExit(1)
        with self.assertRaises(SystemExit) as raised:
            self.mod.cmd_send(send_args(channel="C0C9VPUQ9SN"))
        self.assertEqual(raised.exception.code, 1)

    def test_transport_failure_is_reported_as_no_response(self):
        self.call.side_effect = None
        with mock.patch.object(self.mod.urllib.request, "build_opener") as opener:
            opener.return_value.open.side_effect = OSError("connection reset")
            with self.assertRaises(self.mod._NoResponse) as raised:
                self.mod._open_authenticated(object())
        self.assertEqual(raised.exception.code, 1)

    def test_receipt_reports_the_staged_copy_and_clears_its_thread(self):
        self.clear.return_value = "cleared"
        self.mod.cmd_send(send_args(permalink=REPLY_LINK))
        self.clear.assert_called_once_with("D0C8CR24NQZ", "1793185400.000100")
        self.assertEqual(self.receipt()["composer"], "cleared")

    def test_nothing_is_cleared_when_the_send_fails(self):
        self.call.side_effect = SystemExit(1)
        with self.assertRaises(SystemExit):
            self.mod.cmd_send(send_args(channel="C0C9VPUQ9SN"))
        self.clear.assert_not_called()

    def test_clear_targets_the_thread_root_or_the_channel(self):
        spec = importlib.util.spec_from_file_location("slack_clear_test", SOURCE)
        real = importlib.util.module_from_spec(spec)  # setUp's module has _clear_staged_draft mocked
        spec.loader.exec_module(real)
        real._workspace = "forethought"
        done = SimpleNamespace(stdout='{"clear": "absent", "status": "ok"}\n')
        with mock.patch.object(real.subprocess, "run", return_value=done) as run:
            self.assertEqual(real._clear_staged_draft("D0C8CR24NQZ", "1793185462.048979"), "absent")
            self.assertEqual(real._clear_staged_draft("C0C9VPUQ9SN", None), "absent")
        thread_argv, channel_argv = (c.args[0] for c in run.call_args_list)
        self.assertEqual(thread_argv[3:], ["--clear", "--permalink", PERMALINK])
        self.assertEqual(channel_argv[3:], ["--clear", "--channel", "C0C9VPUQ9SN", "--workspace", "forethought"])
        with mock.patch.object(real.subprocess, "run", side_effect=OSError("no emacs")):
            self.assertEqual(real._clear_staged_draft("C0C9VPUQ9SN", None), "uncertain")
        with mock.patch.object(real.subprocess, "run", return_value=SimpleNamespace(stdout="")):
            self.assertEqual(real._clear_staged_draft("C0C9VPUQ9SN", None), "uncertain")

    def test_cli_wiring(self):
        with mock.patch("sys.argv", ["slack.py", "-w", "forethought", "send", "--permalink", PERMALINK,
                                     "--text", "Done."]):
            self.mod.main()
        self.assertEqual(self.mod._workspace, "forethought")
        self.assertEqual(self.receipt()["thread_ts"], "1793185462.048979")


if __name__ == "__main__":
    unittest.main()
