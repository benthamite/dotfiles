"""Offline regression coverage for listing conversations across sparse pages."""

import importlib.util
import io
import json
from pathlib import Path
from types import SimpleNamespace
import unittest
from unittest import mock


SOURCE = Path(__file__).resolve().parents[1] / "claude/bin/slack.py"

# Slack returns sparse pages: an empty page with a cursor is not the end.
PAGES = {
    None: {"ok": True, "channels": [
        {"id": "C11111111", "name": "general", "is_member": True, "num_members": 27,
         "purpose": {"value": "Workspace-wide"}}],
        "response_metadata": {"next_cursor": "p2"}},
    "p2": {"ok": True, "channels": [], "response_metadata": {"next_cursor": "p3"}},
    "p3": {"ok": True, "channels": [
        {"id": "G22222222", "name": "retreat-oct-2026", "is_private": True, "is_member": True,
         "num_members": 23, "purpose": {"value": ""}},
        {"id": "D33333333", "is_im": True, "user": "U44444444"}],
        "response_metadata": {"next_cursor": ""}},
}


class ChannelsTests(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location("slack_channels_test", SOURCE)
        self.mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.mod)
        self.call = self.enterContext(mock.patch.object(self.mod, "call", side_effect=self.fake_call))
        self.enterContext(mock.patch.object(self.mod, "_tokens", side_effect=AssertionError("No credentials")))
        self.output = self.enterContext(mock.patch("sys.stdout", new_callable=io.StringIO))
        self.errors = self.enterContext(mock.patch("sys.stderr", new_callable=io.StringIO))

    def fake_call(self, method, **params):
        self.assertEqual(method, "conversations.list")
        return PAGES[params.get("cursor")]

    def rows(self, query=None, types="public_channel,private_channel,im"):
        self.mod.cmd_channels(SimpleNamespace(query=query, types=types))
        return [json.loads(line) for line in self.output.getvalue().splitlines()]

    def test_follows_cursors_past_empty_pages(self):
        names = [r.get("name") for r in self.rows()]
        self.assertEqual(names, [None, "general", "retreat-oct-2026"])
        self.assertEqual(self.call.call_count, 3)
        self.assertIn("3 of 3 conversations listed", self.errors.getvalue())

    def test_rows_are_compact(self):
        rows = {r["id"]: r for r in self.rows()}
        self.assertEqual(rows["G22222222"], {"id": "G22222222", "name": "retreat-oct-2026", "type": "private",
                                             "is_member": True, "num_members": 23})
        self.assertEqual(rows["D33333333"], {"id": "D33333333", "type": "im", "user": "U44444444"})

    def test_query_filters_name_and_purpose_and_reports_total(self):
        self.assertEqual([r["id"] for r in self.rows("#RETREAT")], ["G22222222"])
        self.assertIn("1 of 3 conversations match '#RETREAT'", self.errors.getvalue())

    def test_query_matches_purpose(self):
        self.assertEqual([r["id"] for r in self.rows("workspace")], ["C11111111"])

    def test_runaway_pagination_fails(self):
        self.call.side_effect = lambda method, **params: {"channels": [], "response_metadata": {"next_cursor": "x"}}
        with self.assertRaises(SystemExit):
            self.rows()
        self.assertEqual(self.output.getvalue(), "")

    def test_cli_defaults_to_channels_only(self):
        with mock.patch("sys.argv", ["slack.py", "-w", "forethought", "channels", "retreat"]):
            self.mod.main()
        self.assertEqual(self.call.call_args.kwargs["types"], "public_channel,private_channel")
        self.assertEqual(self.call.call_args.kwargs["limit"], "1000")


if __name__ == "__main__":
    unittest.main()
