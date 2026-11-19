"""Offline regression coverage for explicit search pages and exact unsave targets."""

import importlib.util
import io
import json
from pathlib import Path
from types import SimpleNamespace
import unittest
from unittest import mock


SOURCE = Path(__file__).resolve().parents[1] / "claude/bin/slack.py"


class SavedTests(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location("slack_saved_test", SOURCE)
        self.mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.mod)
        self.call = self.enterContext(mock.patch.object(self.mod, "call"))
        self.enterContext(mock.patch.object(self.mod, "_tokens", side_effect=AssertionError("No credentials")))
        self.enterContext(mock.patch.object(self.mod.subprocess, "run", side_effect=AssertionError("No processes")))
        self.output = self.enterContext(mock.patch("sys.stdout", new_callable=io.StringIO))
        self.enterContext(mock.patch("sys.stderr", new_callable=io.StringIO))

    def test_search_passes_exact_page_once_and_preserves_response(self):
        response = {"ok": True, "messages": {"matches": [], "paging": {"page": 2, "pages": 4}}}
        self.call.return_value = response
        self.mod.cmd_search(SimpleNamespace(query="is:saved", max=100, page=2, sort="timestamp"))
        self.call.assert_called_once_with("search.messages", query="is:saved", count="100", page="2", sort="timestamp")
        self.assertEqual(json.loads(self.output.getvalue()), response)

    def test_search_bounds_reject_before_call(self):
        for name in ("max", "page"):
            for value in (0, -1, 101, True, 1.5, "2"):
                with self.subTest(name=name, value=value):
                    args = SimpleNamespace(query="is:saved", max=20, page=1, sort="score")
                    setattr(args, name, value)
                    with self.assertRaises(SystemExit):
                        self.mod.cmd_search(args)
        self.call.assert_not_called()

    def test_cli_default_and_explicit_page(self):
        for options, page, count in (([], "1", "20"), (["--page", "100", "--max", "1"], "100", "1")):
            with self.subTest(options=options), mock.patch("sys.argv", ["slack.py", "-w", "epoch", "search", "is:saved", *options]):
                self.call.reset_mock()
                self.call.return_value = {}
                self.mod.main()
                self.call.assert_called_once_with("search.messages", query="is:saved", count=count, page=page, sort="timestamp")

    def test_unsave_exact_mapping_and_response(self):
        for channel in ("C12345678", "G12345678", "D12345678"):
            with self.subTest(channel=channel):
                self.call.reset_mock()
                self.output.seek(0)
                self.output.truncate()
                self.call.return_value = {"ok": True, "unchanged": "metadata"}
                self.mod.cmd_unsave(SimpleNamespace(channel=channel, ts="1710000000.000001"))
                self.call.assert_called_once_with("saved.delete", item_id=channel, item_type="message", ts="1710000000.000001")
                self.assertEqual(json.loads(self.output.getvalue()), self.call.return_value)

    def test_unsave_invalid_channel_rejected_before_call(self):
        for channel in ("", "general", "U12345678", "C123", "C12345678,D12345678", " C12345678", "C12345678\n", "c12345678"):
            with self.subTest(channel=channel), self.assertRaises(SystemExit):
                self.mod.cmd_unsave(SimpleNamespace(channel=channel, ts="1710000000.000001"))
        self.call.assert_not_called()

    def test_unsave_invalid_timestamp_rejected_before_call(self):
        for ts in ("", "1710000000", "1710000000.1", "1710000000.0000001", "-1.000000", "1e9.000000", "1710000000.000001\n", "１２３.000001"):
            with self.subTest(ts=ts), self.assertRaises(SystemExit):
                self.mod.cmd_unsave(SimpleNamespace(channel="C12345678", ts=ts))
        self.call.assert_not_called()



class CompleteTodoTests(unittest.TestCase):
    CH, TS = "C12345678", "1710000000.000001"

    def setUp(self):
        spec = importlib.util.spec_from_file_location("slack_todo_test", SOURCE)
        self.mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.mod)
        self.call = self.enterContext(mock.patch.object(self.mod, "call"))
        self.enterContext(mock.patch.object(self.mod, "_tokens", side_effect=AssertionError("No credentials")))
        self.output = self.enterContext(mock.patch("sys.stdout", new_callable=io.StringIO))
        self.enterContext(mock.patch("sys.stderr", new_callable=io.StringIO))

    def item(self, **kw):
        base = {"item_type": "message", "item_id": self.CH, "ts": self.TS,
                "todo_state": "to_do", "date_created": 1700}
        base.update(kw)
        return base

    def pages(self, *pages):
        def fake(method, **params):
            if method == "saved.update":
                return {"ok": True}
            index = int(params.get("cursor") or 0)
            items, nxt = pages[index]
            return {"ok": True, "saved_items": items, "response_metadata": {"next_cursor": nxt}}
        self.call.side_effect = fake

    def run_cmd(self, mark="1700"):
        self.mod.cmd_complete_todo(SimpleNamespace(channel=self.CH, ts=self.TS, mark=mark))
        return json.loads(self.output.getvalue())["status"]

    def updates(self):
        return [c for c in self.call.call_args_list if c.args[0] == "saved.update"]

    def test_completes_same_mark_found_on_a_later_page(self):
        self.pages(([self.item(ts="1.000001")], "1"), ([self.item()], ""))
        self.assertEqual(self.run_cmd(), "completed")
        self.assertEqual(self.updates(), [mock.call("saved.update", item_type="message",
                                                     item_id=self.CH, ts=self.TS, mark="completed")])

    def test_settled_without_write(self):
        for items, status in (([], "absent"), ([self.item(todo_state="saved")], "not-todo"),
                              ([self.item(date_created=1800)], "remarked")):
            with self.subTest(status=status):
                self.call.reset_mock()
                self.output.seek(0)
                self.output.truncate()
                self.pages((items, ""))
                self.assertEqual(self.run_cmd(), status)
                self.assertEqual(self.updates(), [])

    def test_incomplete_listing_fails_without_write(self):
        for pages in ((([self.item()], "1"), ([], "1")),):
            self.pages(*pages)
            with self.assertRaises(SystemExit):
                self.run_cmd()
        self.call.side_effect = lambda method, **p: {"ok": True}
        with self.assertRaises(SystemExit):
            self.run_cmd()
        self.assertEqual(self.updates(), [])


if __name__ == "__main__":
    unittest.main()
