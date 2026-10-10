"""Offline regression coverage for joining public channels by name or ID."""

import copy
import importlib.util
import io
import json
from pathlib import Path
from types import SimpleNamespace
import unittest
from unittest import mock


SOURCE = Path(__file__).resolve().parents[1] / "claude/bin/slack.py"

PAGES = [
    {"ok": True, "channels": [{"id": "C11111111", "name": "general", "is_member": True}],
     "response_metadata": {"next_cursor": "next"}},
    {"ok": True, "channels": [{"id": "C22222222", "name": "outreach", "is_member": False}],
     "response_metadata": {"next_cursor": ""}},
]


class JoinTests(unittest.TestCase):
    def setUp(self):
        self.pages = copy.deepcopy(PAGES)
        spec = importlib.util.spec_from_file_location("slack_join_test", SOURCE)
        self.mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.mod)
        self.call = self.enterContext(mock.patch.object(self.mod, "call", side_effect=self.fake_call))
        self.enterContext(mock.patch.object(self.mod, "_tokens", side_effect=AssertionError("No credentials")))
        self.enterContext(mock.patch.object(self.mod.time, "sleep"))
        self.output = self.enterContext(mock.patch("sys.stdout", new_callable=io.StringIO))
        self.enterContext(mock.patch("sys.stderr", new_callable=io.StringIO))

    def fake_call(self, method, **params):
        if method == "conversations.list":
            return self.pages[0] if params.get("cursor") is None else self.pages[1]
        return {"ok": True}

    def lines(self):
        return [json.loads(line) for line in self.output.getvalue().splitlines()]

    def joins(self):
        return [c.kwargs["channel"] for c in self.call.call_args_list if c.args[0] == "conversations.join"]

    def test_joins_only_non_members_across_pages(self):
        self.mod.cmd_join(SimpleNamespace(channels=["#general", "outreach"], dry_run=False))
        self.assertEqual(self.joins(), ["C22222222"])
        self.assertEqual([l["status"] for l in self.lines()], ["already", "joined"])

    def test_accepts_ids_and_dedupes_repeats(self):
        self.mod.cmd_join(SimpleNamespace(channels=["C22222222", "outreach"], dry_run=False))
        self.assertEqual(self.joins(), ["C22222222"])
        self.assertEqual([l["status"] for l in self.lines()], ["joined", "already"])

    def test_dry_run_makes_no_join(self):
        self.mod.cmd_join(SimpleNamespace(channels=["outreach"], dry_run=True))
        self.assertEqual(self.joins(), [])
        self.assertEqual(self.lines()[0]["status"], "would-join")

    def test_unknown_channel_fails_before_any_join(self):
        with self.assertRaises(SystemExit):
            self.mod.cmd_join(SimpleNamespace(channels=["outreach", "nope"], dry_run=False))
        self.assertEqual(self.joins(), [])

    def test_cli_wiring(self):
        with mock.patch("sys.argv", ["slack.py", "-w", "forethought", "join", "outreach", "--dry-run"]):
            self.mod.main()
        self.assertEqual(self.mod._workspace, "forethought")
        self.assertEqual(self.lines()[0]["status"], "would-join")


if __name__ == "__main__":
    unittest.main()
