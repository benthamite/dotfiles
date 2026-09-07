"""Offline exact-target and bounded-context Slack permalink regressions."""
import argparse
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import unittest
from unittest import mock

SOURCE = Path(__file__).resolve().parents[1] / "claude/bin/slack.py"
spec = importlib.util.spec_from_file_location("slack_permalink_fixture", SOURCE)
SLACK = importlib.util.module_from_spec(spec)
spec.loader.exec_module(SLACK)
CHANNEL = "C12345678"
TARGET = "1776265509.202259"
PARENT = "1776265426.017059"
URL = f"https://epochai.slack.com/archives/{CHANNEL}/p{TARGET.replace('.', '')}"
THREAD = URL + f"?thread_ts={PARENT}&cid={CHANNEL}"


def tripwire(*args, **kwargs):
    raise AssertionError("unmocked credential or network call")


class PermalinkTests(unittest.TestCase):
    def setUp(self):
        self.stack = contextlib.ExitStack()
        self.addCleanup(self.stack.close)
        for name in ("subprocess.run", "socket.socket", "socket.create_connection"):
            self.stack.enter_context(mock.patch(name, side_effect=tripwire))
        self.stack.enter_context(mock.patch.object(SLACK, "_workspace", "epoch"))
        self.stack.enter_context(mock.patch.object(SLACK, "_user_cache", {}))

    def invoke(self, responses, url=URL, cursor=None, resolve_users=False):
        output = io.StringIO()
        args = argparse.Namespace(url=url, cursor=cursor, limit=200, resolve_users=resolve_users)
        with mock.patch.object(SLACK, "call", side_effect=responses) as call, contextlib.redirect_stdout(output):
            SLACK.cmd_permalink(args)
        return json.loads(output.getvalue()), call.call_args_list

    def test_exact_standalone_message(self):
        target = {"ts": TARGET, "text": "Exact message"}
        out, calls = self.invoke([{"ok": True, "messages": [target], "has_more": True}])
        self.assertEqual(out["target_message"], target)
        self.assertEqual(out["messages"], [target])
        self.assertTrue(out["coverage"]["complete"])
        self.assertEqual(calls, [mock.call("conversations.history", channel=CHANNEL, oldest=TARGET, latest=TARGET, inclusive="true", limit="1")])

    def test_missing_or_neighbor_target_never_succeeds(self):
        for messages in ([], [{"ts": PARENT, "text": "Wrong"}], [{"ts": TARGET}, {"ts": TARGET}]):
            with self.subTest(messages=messages), contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
                self.invoke([{"ok": True, "messages": messages}])

    def test_thread_target_membership_and_partial_first_page(self):
        target = {"ts": TARGET, "thread_ts": PARENT, "text": "Selected reply"}
        parent = {"ts": PARENT, "thread_ts": PARENT, "text": "Parent", "reply_count": 10}
        out, calls = self.invoke([
            {"ok": True, "messages": [target]},
            {"ok": True, "messages": [parent], "has_more": True, "response_metadata": {"next_cursor": "next"}},
        ], THREAD)
        self.assertEqual(calls[0], mock.call("conversations.replies", channel=CHANNEL, ts=PARENT, oldest=TARGET, latest=TARGET, inclusive="true", limit="1"))
        self.assertEqual(len(calls), 2)
        self.assertEqual(out["target_message"], target)
        self.assertEqual(out["messages"], [parent])
        self.assertEqual(out["coverage"], {"complete": False, "has_more": True, "next_cursor": "next", "starts_at_beginning": True, "is_limited": False})

    def test_wrong_thread_hint_and_wrong_context_rows_fail(self):
        target = {"ts": TARGET, "thread_ts": PARENT}
        cases = [
            [{"ok": True, "messages": [{"ts": PARENT, "thread_ts": PARENT}]}],
            [{"ok": True, "messages": [{"ts": TARGET, "thread_ts": "1.000000"}]}],
            [{"ok": True, "messages": [target]}, {"ok": True, "messages": [{"ts": "1.000000", "thread_ts": "1.000000"}]}],
            [{"ok": True, "messages": [target]}, {"ok": True, "messages": []}],
        ]
        for responses in cases:
            with self.subTest(responses=responses), contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
                self.invoke(responses, THREAD)

    def test_later_cursor_page_is_never_whole_thread(self):
        target = {"ts": TARGET, "thread_ts": PARENT}
        out, calls = self.invoke([
            {"ok": True, "messages": [target]},
            {"ok": True, "messages": [target], "has_more": False},
        ], THREAD, cursor="second")
        self.assertEqual(calls[1].kwargs["cursor"], "second")
        self.assertFalse(out["coverage"]["complete"])
        self.assertFalse(out["coverage"]["starts_at_beginning"])

    def test_parent_with_replies_expands_one_page(self):
        target = {"ts": TARGET, "reply_count": 1}
        child = {"ts": "1776265600.000000", "thread_ts": TARGET}
        out, calls = self.invoke([{"ok": True, "messages": [target]}, {"ok": True, "messages": [target, child], "has_more": False}])
        self.assertEqual(len(calls), 2)
        self.assertEqual(out["permalink"]["thread_ts"], TARGET)
        self.assertTrue(out["coverage"]["complete"])

    def test_target_outside_page_gets_enterprise_user_enrichment(self):
        target = {"ts": TARGET, "thread_ts": PARENT, "user": "W12345678"}
        out, _ = self.invoke([
            {"ok": True, "messages": [target]},
            {"ok": True, "messages": [{"ts": PARENT}], "has_more": True},
            {"ok": True, "user": {"id": "W12345678", "real_name": "Exact Person", "profile": {}}},
        ], THREAD, resolve_users=True)
        self.assertEqual(out["target_message"]["user"], "W12345678")
        self.assertEqual(out["target_message"]["user_profile_resolved"]["real_name"], "Exact Person")

    def test_canonical_url_rejections_precede_api_calls(self):
        urls = [
            URL.replace("https:", "http:"), URL.replace("epochai", "other"),
            URL.replace("epochai", "evil@epochai"), URL.replace(".com/", ".com:443/"),
            URL + "/extra", URL + "#fragment", URL + "?thread_ts=bad", URL + "?thread_ts=",
            THREAD + "&thread_ts=" + PARENT, THREAD + "&cid=" + CHANNEL,
            URL + "?cid=C87654321", URL + "?unknown=x", URL + "\n",
            URL.replace("/archives/", "//archives/"), URL.replace("C12345678", "U12345678"),
            URL.replace("p177", "p１７７"),
        ]
        for url in urls:
            with self.subTest(url=url), mock.patch.object(SLACK, "call", side_effect=tripwire), contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as error:
                SLACK.cmd_permalink(argparse.Namespace(url=url, limit=200))
            self.assertEqual(error.exception.code, 2)

    def test_unknown_domain_is_bound_to_selected_account(self):
        with mock.patch.object(SLACK, "_workspace", "trajectory"):
            url = URL.replace("epochai", "verified-example")
            out, calls = self.invoke([
                {"ok": True, "url": "https://verified-example.slack.com/"},
                {"ok": True, "messages": [{"ts": TARGET}]},
            ], url)
            self.assertEqual(calls[0], mock.call("auth.test"))
            self.assertEqual(out["permalink"]["team_domain"], "verified-example")
            for identity_url in ("https://other.slack.com/", "https://evil.test/", "https://user@verified-example.slack.com/"):
                with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
                    self.invoke([{"ok": True, "url": identity_url}], url)

    def test_replies_enforces_parent_and_reports_cursor_coverage(self):
        args = argparse.Namespace(channel=CHANNEL, thread_ts=PARENT, limit=200, cursor="next", resolve_users=False)
        output = io.StringIO()
        with mock.patch.object(SLACK, "call", return_value={"ok": True, "messages": [{"ts": TARGET, "thread_ts": PARENT}], "has_more": False}), contextlib.redirect_stdout(output):
            SLACK.cmd_replies(args)
        self.assertFalse(json.loads(output.getvalue())["coverage"]["complete"])
        with mock.patch.object(SLACK, "call", return_value={"ok": True, "messages": [{"ts": TARGET, "thread_ts": TARGET}]}), contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
            SLACK.cmd_replies(args)

    def test_replies_first_page_requires_parent(self):
        args = argparse.Namespace(channel=CHANNEL, thread_ts=PARENT, limit=200, cursor=None, resolve_users=False)
        with mock.patch.object(SLACK, "call", return_value={"ok": True, "messages": [{"ts": TARGET, "thread_ts": PARENT}]}), contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
            SLACK.cmd_replies(args)

    def test_invalid_pagination_cannot_claim_complete_context(self):
        target = {"ts": TARGET, "thread_ts": PARENT}
        for metadata in ({"response_metadata": []}, {"response_metadata": 0}, {"response_metadata": {"next_cursor": False}}, {"has_more": 1}, {"is_limited": 1}):
            page = {"ok": True, "messages": [{"ts": PARENT}], **metadata}
            with self.subTest(metadata=metadata), contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit):
                self.invoke([{"ok": True, "messages": [target]}, page], THREAD)

    def test_limited_history_never_reports_complete(self):
        out, _ = self.invoke([{"ok": True, "messages": [{"ts": TARGET}], "is_limited": True}])
        self.assertTrue(out["coverage"]["is_limited"])
        self.assertFalse(out["coverage"]["complete"])
        out, _ = self.invoke([
            {"ok": True, "messages": [{"ts": TARGET, "thread_ts": PARENT}]},
            {"ok": True, "messages": [{"ts": PARENT}], "is_limited": True},
        ], THREAD)
        self.assertTrue(out["coverage"]["is_limited"])
        self.assertFalse(out["coverage"]["complete"])

    def test_nonpositive_limit_fails_before_any_account_access(self):
        for limit in (0, -1):
            args = argparse.Namespace(url=URL, channel=CHANNEL, thread_ts=PARENT, limit=limit)
            for command in (SLACK.cmd_permalink, SLACK.cmd_replies):
                with mock.patch.object(SLACK, "call", side_effect=tripwire), contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as error:
                    command(args)
                self.assertEqual(error.exception.code, 2)


if __name__ == "__main__":
    unittest.main()
