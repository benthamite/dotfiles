"""Offline tests for slack_rich_text: mrkdwn bullets to Slack rich_text lists."""
import importlib.util
import json
from pathlib import Path
import sys
import unittest

BIN = Path(__file__).resolve().parents[1] / "claude/bin"
spec = importlib.util.spec_from_file_location("slack_rich_text", BIN / "slack_rich_text.py")
RICH = importlib.util.module_from_spec(spec)
spec.loader.exec_module(RICH)

PLAN = (
    "*Thursday 5th November*\n\n_Schedule_:\n• 08:00 Call with Peter\n\n"
    "_Sessions:_\n• *S1*: Idea Swipe & co\n• ~*S3–S4*: Write a plan~\n"
    "    ◦ By EOD: file_name kept.\n_Retrospective:_ fine <https://x.com|link>."
)


def text(elements):
    return "".join(e.get("text", "") for e in elements)


class RichTextTests(unittest.TestCase):
    def setUp(self):
        self.elements = RICH.to_blocks(PLAN)[0]["elements"]

    def test_structure_matches_the_composer(self):
        self.assertEqual(
            [(e["type"], e.get("indent")) for e in self.elements],
            [("rich_text_section", None), ("rich_text_list", 0),
             ("rich_text_section", None), ("rich_text_list", 0),
             ("rich_text_list", 1), ("rich_text_section", None)])

    def test_sections_keep_blank_lines_and_end_before_lists(self):
        self.assertEqual(text(self.elements[0]["elements"]),
                         "Thursday 5th November\n\nSchedule:\n")
        self.assertEqual(text(self.elements[2]["elements"]), "\nSessions:\n")

    def test_consecutive_bullets_share_one_list(self):
        self.assertEqual(len(self.elements[3]["elements"]), 2)

    def test_nested_styles_and_plain_underscores(self):
        struck = self.elements[3]["elements"][1]["elements"]
        self.assertEqual(struck[0], {"type": "text", "text": "S3–S4",
                                     "style": {"bold": True, "strike": True}})
        self.assertEqual(struck[1]["style"], {"strike": True})
        nested = self.elements[4]["elements"][0]["elements"]
        self.assertEqual(nested, [{"type": "text", "text": "By EOD: file_name kept."}])

    def test_links_and_unescaping(self):
        last = self.elements[5]["elements"]
        self.assertIn({"type": "link", "url": "https://x.com", "text": "link"}, last)
        blocks = RICH.to_blocks("• A &amp; B")
        self.assertEqual(text(blocks[0]["elements"][0]["elements"][0]["elements"]), "A & B")

    def test_serialises(self):
        json.dumps(RICH.to_blocks(PLAN), ensure_ascii=False)


if __name__ == "__main__":
    unittest.main()
