from __future__ import annotations

import filecmp
import json
import unittest
from pathlib import Path


DOTFILES = Path("/Users/pablostafforini/My Drive/dotfiles")
SKILL_DIRS = (
    DOTFILES / "macos/.claude/skills/audit-mac-app",
    DOTFILES / "macos/.codex/skills/audit-mac-app",
)


def relative_files(root: Path) -> set[Path]:
    return {
        path.relative_to(root)
        for path in root.rglob("*")
        if path.is_file() and "node_modules" not in path.parts
    }


class AuditMacAppSupplyChainTests(unittest.TestCase):
    def test_mirrored_skill_trees_are_byte_identical(self):
        claude, codex = SKILL_DIRS
        claude_files = relative_files(claude)
        codex_files = relative_files(codex)
        self.assertEqual(claude_files, codex_files)

        for relative in sorted(claude_files):
            with self.subTest(path=relative):
                self.assertTrue(
                    filecmp.cmp(claude / relative, codex / relative, shallow=False),
                    f"mirrored file differs: {relative}",
                )

    def test_extractor_is_exactly_locked_and_never_uses_npx(self):
        for skill_dir in SKILL_DIRS:
            with self.subTest(skill=skill_dir):
                package_path = skill_dir / "package.json"
                lock_path = skill_dir / "package-lock.json"
                helper = skill_dir / "scripts/extract-asar.sh"
                required = (package_path, lock_path, helper)
                for path in required:
                    self.assertTrue(path.is_file(), f"missing locked extractor file: {path}")
                if not all(path.is_file() for path in required):
                    continue

                package = json.loads(
                    package_path.read_text(encoding="utf-8")
                )
                self.assertEqual(
                    package["dependencies"],
                    {"@electron/asar": "4.2.1"},
                )

                lock = json.loads(
                    lock_path.read_text(encoding="utf-8")
                )
                self.assertEqual(
                    lock["packages"]["node_modules/@electron/asar"]["version"],
                    "4.2.1",
                )
                self.assertIn(
                    "integrity",
                    lock["packages"]["node_modules/@electron/asar"],
                )

                self.assertTrue(helper.stat().st_mode & 0o100)
                helper_text = helper.read_text(encoding="utf-8")
                self.assertTrue(
                    "AUDIT_MAC_APP_CACHE_DIR" in helper_text,
                    "extractor installation should live outside the synced skill tree",
                )
                self.assertFalse(
                    '$SKILL_DIR/node_modules/' in helper_text,
                    "extractor must not install node_modules into Google Drive",
                )

                for relative in relative_files(skill_dir):
                    if relative.suffix not in {".md", ".sh"}:
                        continue
                    path = skill_dir / relative
                    text = path.read_text(encoding="utf-8")
                    self.assertNotRegex(text, r"\bnpx\b[^\n]*\basar\b")


if __name__ == "__main__":
    unittest.main()
