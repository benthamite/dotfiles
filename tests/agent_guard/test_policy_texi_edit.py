import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import CLAUDE, CODEX, Call  # noqa: E402
from agent_guard.policies import texi_edit  # noqa: E402


def decision(runtime, tool, tool_input):
    return texi_edit.decide(Call.from_payload(runtime, {"tool_name": tool, "tool_input": tool_input})).decision


class TexiTests(unittest.TestCase):
    def test_claude_file_path_then_file_path_camel(self):
        self.assertEqual(decision(CLAUDE, "Edit", {"file_path": "doc/a.texi"}), "deny")
        self.assertEqual(decision(CLAUDE, "Edit", {"file_path": None, "filePath": "a.texi"}), "deny")
        self.assertEqual(decision(CLAUDE, "Edit", {"file_path": "a.org", "filePath": "a.texi"}), "allow")
        self.assertEqual(decision(CLAUDE, "Edit", {"path": "a.texi"}), "allow")

    def test_trailing_newlines_are_dropped(self):
        self.assertEqual(decision(CLAUDE, "Edit", {"file_path": "a.texi\n\n"}), "deny")

    def test_codex_checks_every_changed_path(self):
        patch = "*** Begin Patch\n*** Update File: a.org\n*** Add File: b.texi\n+x\n*** End Patch"
        self.assertEqual(decision(CODEX, "apply_patch", {"patch": patch}), "deny")
        self.assertEqual(decision(CODEX, "Edit", {"path": "a.texi"}), "deny")
        self.assertEqual(decision(CODEX, "Edit", {"file_path": "a.texi.org"}), "allow")

    def test_reason(self):
        call = Call.from_payload(CODEX, {"tool_name": "Edit", "tool_input": {"file_path": "a.texi"}})
        self.assertEqual(texi_edit.decide(call).reason, texi_edit.REASON)


if __name__ == "__main__":
    unittest.main()
