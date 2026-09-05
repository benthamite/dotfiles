from __future__ import annotations

import filecmp
import json
import os
import subprocess
import struct
import tempfile
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
    def test_real_archive_relative_paths_and_unpacked_inputs(self):
        with tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
            root = Path(temp)
            contents = b'{"name":"synthetic-isolation-fixture"}\n'
            unpacked_contents = b"synthetic unpacked asset\n"
            header = json.dumps({"files": {
                "package.json": {"size": len(contents), "offset": "0"},
                "asset.txt": {"size": len(unpacked_contents), "unpacked": True},
            }}).encode()
            padded = header + b"\0" * (-len(header) % 4)
            archive = root / "app.asar"
            archive.write_bytes(struct.pack("<IIII", 4, 8 + len(padded), 4 + len(padded), len(header)) + padded + contents)
            unpacked = root / "app.asar.unpacked"
            unpacked.mkdir()
            (unpacked / "asset.txt").write_bytes(unpacked_contents)
            for index, skill in enumerate(SKILL_DIRS):
                result = subprocess.run(
                    [str(skill / "scripts/extract-asar.sh"), "app.asar", f"output-{index}"],
                    cwd=root, capture_output=True, text=True, timeout=120,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual((root / f"output-{index}/package.json").read_bytes(), contents)
                self.assertEqual((root / f"output-{index}/asset.txt").read_bytes(), unpacked_contents)
            self.assertEqual((unpacked / "asset.txt").read_bytes(), unpacked_contents)

    def test_unpacked_and_parent_symlink_inputs_are_refused(self):
        with tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
            root = Path(temp)
            outside = root / "outside"
            outside.mkdir()
            (outside / "canary").write_text("unchanged")
            bundle = root / "bundle"
            bundle.mkdir()
            (bundle / "app.asar").touch()
            (bundle / "app.asar.unpacked").symlink_to(outside)
            parent_link = root / "parent-link"
            parent_link.symlink_to(outside)
            (outside / "app.asar").touch()
            for skill in SKILL_DIRS:
                for archive in (bundle / "app.asar", parent_link / "app.asar"):
                    destination = root / "must-not-be-created"
                    result = subprocess.run(
                        [str(skill / "scripts/extract-asar.sh"), str(archive), str(destination)],
                        capture_output=True, text=True, timeout=30,
                    )
                    self.assertEqual(result.returncode, 2, result.stderr)
                    self.assertFalse(destination.exists())
            self.assertEqual((outside / "canary").read_text(), "unchanged")

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

    def test_brace_expansion_has_dos_fix(self):
        minimum_safe_version = (5, 0, 9)
        for skill_dir in SKILL_DIRS:
            with self.subTest(skill=skill_dir):
                lock = json.loads(
                    (skill_dir / "package-lock.json").read_text(encoding="utf-8")
                )
                version = lock["packages"]["node_modules/brace-expansion"]["version"]
                self.assertGreaterEqual(
                    tuple(map(int, version.split("."))),
                    minimum_safe_version,
                    "brace-expansion must include the GHSA-rgw5-rvv9-x895 fix",
                )

    def test_cold_cache_install_runs_from_staging_project(self):
        for skill_dir in SKILL_DIRS:
            with self.subTest(skill=skill_dir), tempfile.TemporaryDirectory() as temp:
                temp_dir = Path(temp)
                runtime_dir = temp_dir / "runtime"
                runtime_dir.mkdir()
                node = runtime_dir / "node"
                npm = runtime_dir / "npm"
                node.write_text(
                    """#!/bin/bash
if [ "${1:-}" = "-e" ]; then
    exit 0
fi
if [ "${2:-}" = "extract" ]; then
    printf '{}\\n' >"$4/package.json"
    exit 0
fi
exit 1
""",
                    encoding="utf-8",
                )
                npm.write_text(
                    """#!/bin/bash
set -e
[ "${1:-}" = "ci" ]
for argument in "$@"; do
    [ "$argument" != "--prefix" ]
done
[ -f package.json ]
[ -f package-lock.json ]
mkdir -p node_modules/@electron/asar/bin
printf '%s\\n' 'import{writeFileSync}from"node:fs";writeFileSync("/workspace/package.json","{}");' >node_modules/@electron/asar/bin/asar.mjs
""",
                    encoding="utf-8",
                )
                node.chmod(0o755)
                npm.chmod(0o755)
                archive = temp_dir / "fixture.asar"
                archive.touch()
                destination = temp_dir / "extracted"
                environment = os.environ.copy()
                environment.update(
                    {
                        "AUDIT_MAC_APP_CACHE_DIR": str(temp_dir / "cache"),
                        "AUDIT_MAC_APP_NODE": str(node),
                    }
                )

                result = subprocess.run(
                    [
                        str(skill_dir / "scripts/extract-asar.sh"),
                        str(archive),
                        str(destination),
                    ],
                    capture_output=True,
                    text=True,
                    check=False,
                    env=environment,
                )

                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertTrue((destination / "package.json").is_file())


if __name__ == "__main__":
    unittest.main()
