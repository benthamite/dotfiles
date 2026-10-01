import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import CLAUDE, CODEX, Call  # noqa: E402
from agent_guard.policies import elpaca_dotfiles_edit  # noqa: E402


def decide(runtime, tool, tool_input):
    return elpaca_dotfiles_edit.decide(Call.from_payload(runtime, {"tool_name": tool, "tool_input": tool_input}))


class ElpacaDotfilesEditTests(unittest.TestCase):
    def test_canonical_path_strips_through_the_last_clone_segment(self):
        result = decide(CLAUDE, "Edit", {"file_path": "/e/elpaca/sources/dotfiles/a/elpaca/sources/dotfiles/b.el"})
        self.assertEqual(result.decision, "deny")
        self.assertIn("\n  ~/My Drive/dotfiles/b.el\n\n", result.reason)

    def test_other_paths(self):
        self.assertEqual(decide(CLAUDE, "Edit", {"file_path": "/e/elpaca/sources/dotfiles"}).decision, "allow")
        self.assertEqual(decide(CLAUDE, "Edit", {"filePath": "/e/elpaca/sources/dotfiles/a"}).decision, "allow")

    def test_codex_patch_paths(self):
        patch = "*** Begin Patch\n*** Update File: x\n*** Move to: /e/elpaca/sources/dotfiles/y\n*** End Patch"
        result = decide(CODEX, "apply_patch", patch)
        self.assertEqual(result.decision, "deny")
        self.assertIn("~/My Drive/dotfiles/y\n", result.reason)
        self.assertEqual(decide(CODEX, "Edit", {"path": "/e/elpaca/sources/dotfiles/z"}).decision, "deny")


if __name__ == "__main__":
    unittest.main()
