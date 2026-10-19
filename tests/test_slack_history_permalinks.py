"""Offline regression coverage for permalinks added to history and replies output."""

import importlib.util
import io
import json
from pathlib import Path
import unittest
from unittest import mock


SOURCE = Path(__file__).resolve().parents[1] / "claude/bin/slack.py"

MESSAGES = [
    {"ts": "1792159116.143729", "thread_ts": "1792159116.143729", "text": "root"},
    {"ts": "1792164037.382979", "thread_ts": "1792159116.143729", "text": "reply"},
]


class HistoryPermalinkTests(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location("slack_history_permalink_test", SOURCE)
        self.mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.mod)
        self.enterContext(mock.patch.object(
            self.mod, "call", side_effect=lambda method, **params: {"ok": True, "messages": [dict(m) for m in MESSAGES]}))
        self.enterContext(mock.patch.object(self.mod, "_tokens", side_effect=AssertionError("No credentials")))
        self.output = self.enterContext(mock.patch("sys.stdout", new_callable=io.StringIO))
        self.enterContext(mock.patch("sys.stderr", new_callable=io.StringIO))

    def run_history(self, workspace):
        with mock.patch("sys.argv", ["slack.py", "-w", workspace, "history", "C0C8BCAN4N5"]):
            self.mod.main()
        return json.loads(self.output.getvalue())["messages"]

    def test_forethought_links_use_workspace_domain_and_thread(self):
        root, reply = self.run_history("forethought")
        self.assertEqual(root["permalink"],
                         "https://forethought-ea.slack.com/archives/C0C8BCAN4N5/p1792159116143729")
        self.assertEqual(reply["permalink"],
                         "https://forethought-ea.slack.com/archives/C0C8BCAN4N5/p1792164037382979"
                         "?thread_ts=1792159116.143729&cid=C0C8BCAN4N5")

    def test_links_round_trip_through_permalink_parser(self):
        for message in self.run_history("forethought"):
            info = self.mod.parse_permalink(message["permalink"])
            self.assertEqual(info["message_ts"], message["ts"])

    def test_workspace_without_domain_gets_no_links(self):
        for message in self.run_history("altruismo-eficaz"):
            self.assertNotIn("permalink", message)


if __name__ == "__main__":
    unittest.main()
