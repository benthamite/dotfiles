#!/usr/bin/env python3
"""Regression tests for bin/drive-errors."""

from __future__ import annotations

import json
import os
from pathlib import Path
import runpy
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "drive-errors"


class DriveErrorsTests(unittest.TestCase):
    def run_script(
        self, command: str, drive_root: Path, fixture: dict | None = None
    ) -> subprocess.CompletedProcess[str]:
        env = os.environ.copy()
        env["DRIVE_ERRORS_DRIVE_ROOT"] = str(drive_root)
        if fixture is not None:
            env["DRIVE_ERRORS_PANEL_FIXTURE"] = json.dumps(fixture)
        return subprocess.run(
            [str(SCRIPT), command],
            cwd=ROOT,
            env=env,
            text=True,
            capture_output=True,
            check=False,
        )

    def test_list_preserves_duplicate_rows_and_page_order(self) -> None:
        fixture = {
            "pages": [
                {
                    "page": 1,
                    "total": 2,
                    "rows": [
                        {
                            "name": "meeting.gdoc",
                            "reason": "Can’t upload some Google files",
                        },
                        {
                            "name": "node_modules",
                            "reason": "Can’t upload some files",
                        },
                    ],
                },
                {
                    "page": 2,
                    "total": 2,
                    "rows": [
                        {
                            "name": "meeting.gdoc",
                            "reason": "Can’t upload some Google files",
                        }
                    ],
                },
            ]
        }
        with tempfile.TemporaryDirectory() as tmp:
            result = self.run_script("list", Path(tmp), fixture)

        self.assertEqual(result.returncode, 0, result.stderr)
        first = result.stdout.index("1. meeting.gdoc")
        second = result.stdout.index("2. node_modules")
        third = result.stdout.index("3. meeting.gdoc")
        self.assertLess(first, second)
        self.assertLess(second, third)
        self.assertIn("Can’t upload some Google files: 2", result.stdout)
        self.assertIn("Can’t upload some files: 1", result.stdout)

    def test_list_calls_matches_candidates_not_live_or_stale(self) -> None:
        fixture = {
            "pages": [
                {
                    "page": 1,
                    "total": 1,
                    "rows": [
                        {
                            "name": "same-name.gdoc",
                            "reason": "Can’t upload some Google files",
                        },
                        {
                            "name": "missing.gdoc",
                            "reason": "Can’t upload some Google files",
                        },
                    ],
                }
            ]
        }
        with tempfile.TemporaryDirectory() as tmp:
            drive_root = Path(tmp)
            (drive_root / "one").mkdir()
            (drive_root / "two").mkdir()
            (drive_root / "one" / "same-name.gdoc").write_text("one")
            (drive_root / "two" / "same-name.gdoc").write_text("two")
            result = self.run_script("list", drive_root, fixture)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("same-name.gdoc: 2 candidate paths", result.stdout)
        self.assertIn("missing.gdoc: 0 candidate paths", result.stdout)
        self.assertIn("not exact-instance proof", result.stdout)
        self.assertNotIn("LIVE", result.stdout)
        self.assertNotIn("STALE", result.stdout)

    def test_fixture_rejects_broken_pagination(self) -> None:
        fixture = {
            "pages": [
                {
                    "page": 1,
                    "total": 3,
                    "rows": [
                        {"name": "one", "reason": "Can’t upload some files"}
                    ],
                },
                {
                    "page": 3,
                    "total": 3,
                    "rows": [
                        {"name": "three", "reason": "Can’t upload some files"}
                    ],
                },
            ]
        }
        with tempfile.TemporaryDirectory() as tmp:
            result = self.run_script("list", Path(tmp), fixture)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("pagination", result.stderr.lower())

    def test_list_does_not_call_an_open_nonempty_error_list_clean(self) -> None:
        fixture = {"pages": [{"page": 1, "total": 1, "rows": []}]}
        with tempfile.TemporaryDirectory() as tmp:
            result = self.run_script("list", Path(tmp), fixture)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("no rows", result.stderr.lower())
        self.assertNotIn("No error rows found", result.stdout)

    def test_list_rejects_partial_page_after_ax_traversal_failure(self) -> None:
        fixture = {
            "errorCount": 2,
            "pages": [
                {
                    "page": 1,
                    "total": 1,
                    "traversalFailed": True,
                    "rows": [
                        {"name": "partial", "reason": "Can’t upload some files"}
                    ],
                }
            ]
        }
        with tempfile.TemporaryDirectory() as tmp:
            result = self.run_script("list", Path(tmp), fixture)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("expected 2 rows", result.stderr.lower())
        self.assertNotIn("1. partial", result.stdout)

    def test_list_accepts_irrelevant_ax_failure_when_expected_count_is_complete(self) -> None:
        fixture = {
            "errorCount": 1,
            "pages": [
                {
                    "page": 1,
                    "total": 1,
                    "traversalFailed": True,
                    "rows": [
                        {"name": "complete", "reason": "Can’t upload some files"}
                    ],
                }
            ],
        }
        with tempfile.TemporaryDirectory() as tmp:
            result = self.run_script("list", Path(tmp), fixture)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("1. complete", result.stdout)

    def test_reader_recognizes_current_clean_marker_and_closes_stale_error_window(self) -> None:
        namespace = runpy.run_path(str(SCRIPT))
        open_jxa = namespace["OPEN_ERROR_LIST_JXA"]
        self.assertIn('title === "Up to date"', open_jxa)
        self.assertIn('subrole === "AXCloseButton"', open_jxa)
        self.assertIn("waitForWindowAbsent", open_jxa)
        self.assertIn(
            'throw new Error("Existing Google Drive Error list did not close")',
            open_jxa,
        )
        self.assertLess(
            open_jxa.index("waitForWindowAbsent"),
            open_jxa.index("view.click()"),
        )

    def test_list_fails_closed_when_panel_state_is_unrecognized(self) -> None:
        fixture = {"state": "unrecognized"}
        with tempfile.TemporaryDirectory() as tmp:
            result = self.run_script("list", Path(tmp), fixture)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("unrecognized", result.stderr.lower())
        self.assertNotIn("No error rows found", result.stdout)

    def test_locate_finds_symlinks_inside_archive(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            drive_root = Path(tmp)
            archive = drive_root / "repos" / "archive" / "old-project"
            archive.mkdir(parents=True)
            target = drive_root / "outside"
            target.mkdir()
            (archive / "node_modules").symlink_to(target, target_is_directory=True)
            result = self.run_script("locate", drive_root)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("repos/archive/old-project/node_modules", result.stdout)

    def test_locate_rejects_missing_drive_root(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            missing = Path(tmp) / "missing"
            result = self.run_script("locate", missing)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("drive root", result.stderr.lower())

    def test_source_uses_semantic_view_and_reacquired_page_controls(self) -> None:
        source = SCRIPT.read_text()
        self.assertIn('title.match(/^(\\d+) errors?$/)', source)
        self.assertIn('title === "View"', source)
        self.assertIn('title === "Google Drive Error list"', source)
        self.assertIn('title === "Previous page"', source)
        self.assertIn('title === "Next page"', source)
        self.assertIn("read_page", source)
        self.assertIn("next_page", source)

    def test_next_page_is_scoped_to_the_error_list_window(self) -> None:
        namespace = runpy.run_path(str(SCRIPT))
        next_page_jxa = namespace["NEXT_PAGE_JXA"]
        self.assertIn('title === "Google Drive Error list"', next_page_jxa)

    def test_clean_state_requires_error_free_ax_traversal(self) -> None:
        namespace = runpy.run_path(str(SCRIPT))
        open_jxa = namespace["OPEN_ERROR_LIST_JXA"]
        self.assertIn("traversalFailed", open_jxa)
        self.assertIn("sawUpToDate && !traversalFailed", open_jxa)

    def test_clean_detection_fails_closed_on_ax_property_errors(self) -> None:
        namespace = runpy.run_path(str(SCRIPT))
        source = namespace["OPEN_ERROR_LIST_JXA"]
        self.assertIn('catch (_) { traversalFailed = true; return ""; }', source)
        self.assertIn("if (!titleRead || !valueRead) traversalFailed = true;", source)


if __name__ == "__main__":
    unittest.main()
