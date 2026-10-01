import json
import locale
import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard import codex_paths as cp  # noqa: E402
from agent_guard.core import HookFailure  # noqa: E402


def setUpModule():
    locale.setlocale(locale.LC_COLLATE, "en_GB.UTF-8")


def payload(tool_name="apply_patch", **tool_input):
    return {"tool_name": tool_name, "tool_input": tool_input}


class FieldTests(unittest.TestCase):
    def test_patch_text_uses_jq_alternative(self):
        self.assertEqual(cp.codex_patch_text(payload(patch="", command="x")), "")
        self.assertEqual(cp.codex_patch_text(payload(patch=False, command=None, input="i")), "i")
        self.assertEqual(cp.codex_patch_text(payload(text=1)), "1")
        self.assertEqual(cp.codex_patch_text({"tool_input": "raw\n\n"}), "raw")
        self.assertEqual(cp.codex_patch_text({"tool_input": json.dumps({"input": "a\0b"})}), "ab")

    def test_direct_file_path(self):
        self.assertEqual(cp.codex_direct_file_path(payload(filePath="/a", path="/b")), "/a")
        self.assertEqual(cp.codex_direct_file_path(payload(notebook_path="/n.ipynb")), "/n.ipynb")
        self.assertEqual(cp.codex_direct_file_path(payload()), "")


class ChangedPathsTests(unittest.TestCase):
    def test_direct_then_sorted_unique_patch_paths(self):
        patch = ("*** Begin Patch\n*** Update File: b\n*** Move to: a\n*** Add File: b\n"
                 "diff --git a/c /dev/null\n*** End Patch")
        self.assertEqual(cp.codex_changed_paths(payload(file_path="z\ny", patch=patch)), ["z", "y", "a", "b", "c"])

    def test_rules_apply_to_the_rewritten_line(self):
        self.assertEqual(cp.codex_changed_paths(payload(patch="*** Add File: *** Move to: diff --git a/m b/n")),
                         ["*** Move to: diff --git a/m b/n", "diff --git a/m b/n", "m", "n"])

    def test_diff_fields_split_on_blanks(self):
        self.assertEqual(cp.codex_changed_paths(payload(patch="diff --git a/q\t\tb/r extra\ndiff --git a/\ndiff --git\ta/s b/t")),
                         ["", "q", "r"])

    def test_order_follows_locale_collation(self):
        patch = "*** Add File: README.md\n*** Add File: package.json"
        self.assertEqual(cp.codex_changed_paths(payload(patch=patch)), ["package.json", "README.md"])

    def test_paths_equal_under_collation_are_both_kept(self):
        patch = "*** Add File: x/\u6587.txt\n*** Add File: x/\u5b57.txt"
        self.assertEqual(sorted(cp.codex_changed_paths(payload(patch=patch))), ["x/\u5b57.txt", "x/\u6587.txt"])


class ContentForScanTests(unittest.TestCase):
    def test_apply_patch_drops_sections_written_to_secret_files(self):
        patch = ("*** Begin Patch\n*** Add File: a/.env.local\n+SECRET\n*** Add File: b\n+y\n"
                 "*** Update File: c\n@@\n-x\n+z\n*** End Patch\n")
        self.assertEqual(cp.codex_patch_content_for_scan(payload(patch=patch)),
                         "*** Add File: b\n+y\n*** Update File: c\n@@\n-x\n+z")

    def test_apply_patch_trims_and_strips_crlf(self):
        patch = "  *** Begin Patch\r\n*** Add File: x\r\n+b\r\n *** End Patch  \n\n"
        self.assertEqual(cp.codex_patch_content_for_scan(payload(patch=patch)), "*** Add File: x\n+b")

    def test_unclassifiable_apply_patch_fails(self):
        for patch in ("", "x", "*** Begin Patch\r*** Add File: q\r+z\r*** End Patch",
                      "*** Begin Patch\n*** Add File: x\nnoplus\n*** End Patch",
                      "*** Begin Patch\n*** Update File: ~/.zshenv-secrets\n*** Move to: plain\n@@\n-a\n+b\n*** End Patch",
                      "*** Begin Patch\n*** Delete File: x\n*** End Patch\nextra"):
            with self.subTest(patch=patch), self.assertRaises(HookFailure):
                cp.codex_patch_content_for_scan(payload(patch=patch))

    def test_native_write_joins_direct_patch_and_new_text(self):
        self.assertEqual(cp.codex_patch_content_for_scan(payload("Edit", file_path="f", content=None, new_string=2)),
                         "f\n\n\n2")
        self.assertEqual(cp.codex_patch_content_for_scan(payload("Write", file_path="f", content="c\n")), "f\n\nc")

    def test_native_write_to_secret_file_is_exempt(self):
        for path in ("x.zshenv-secrets", "a.env.op", ".env.local"):
            self.assertEqual(cp.codex_patch_content_for_scan(payload("Write", file_path=path, content="s")), "")

    def test_native_write_with_object_content_fails_like_jq_join(self):
        with self.assertRaises(HookFailure):
            cp.codex_patch_content_for_scan(payload("Write", file_path="f", content={"a": 1}))


if __name__ == "__main__":
    unittest.main()
