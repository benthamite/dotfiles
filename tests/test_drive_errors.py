#!/usr/bin/env python3
"""Regression tests for bin/drive-errors."""

from __future__ import annotations

import json
import os
from pathlib import Path
import runpy
import io
import shutil
import subprocess
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout
from types import SimpleNamespace
from unittest import mock


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
            timeout=10,
        )

    def test_list_preserves_duplicate_rows_and_page_order(self) -> None:
        fixture = {
            "state": "errors",
            "traversalFailed": False,
            "errorCount": 3,
            "pages": [
                {
                    "page": 1,
                    "total": 2,
                    "traversalFailed": False,
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
                    "traversalFailed": False,
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
            "state": "errors",
            "traversalFailed": False,
            "errorCount": 2,
            "pages": [
                {
                    "page": 1,
                    "total": 1,
                    "traversalFailed": False,
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
            "state": "errors",
            "traversalFailed": False,
            "errorCount": 3,
            "pages": [
                {
                    "page": 1,
                    "total": 3,
                    "traversalFailed": False,
                    "rows": [
                        {"name": "one", "reason": "Can’t upload some files"}
                    ],
                },
                {
                    "page": 3,
                    "total": 3,
                    "traversalFailed": False,
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
        fixture = {
            "state": "errors", "traversalFailed": False, "errorCount": 1,
            "pages": [{"page": 1, "total": 1, "rows": [], "traversalFailed": False}],
        }
        with tempfile.TemporaryDirectory() as tmp:
            result = self.run_script("list", Path(tmp), fixture)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("no rows", result.stderr.lower())
        self.assertNotIn("No error rows found", result.stdout)

    def test_list_rejects_partial_page_after_ax_traversal_failure(self) -> None:
        fixture = {
            "state": "errors",
            "traversalFailed": False,
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
        self.assertIn("traversal", result.stderr.lower())
        self.assertNotIn("1. partial", result.stdout)

    def test_list_refuses_ax_failure_even_when_expected_count_matches(self) -> None:
        fixture = {
            "state": "errors",
            "traversalFailed": False,
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

        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn("traversal", result.stderr.lower())
        self.assertNotIn("1. complete", result.stdout)

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


class DriveErrorsBoundaryTests(unittest.TestCase):
    def setUp(self) -> None:
        self.module = runpy.run_path(str(SCRIPT))

    @staticmethod
    def fixture() -> dict:
        return {
            "state": "errors", "errorCount": 1, "traversalFailed": False,
            "pages": [{
                "page": 1, "total": 1, "traversalFailed": False,
                "rows": [{"name": "owned-fixture", "reason": "Can’t upload some files"}],
            }],
        }

    def collect(self, fixture: dict) -> list:
        return self.module["collect_rows"](self.module["FixturePanel"](fixture))

    def test_fixture_requires_explicit_known_complete_summary(self) -> None:
        for fixture in ({}, {"state": "unknown"}, {"pages": []},
                        {"state": "errors", "pages": []},
                        {"state": "clean"},
                        {"state": "clean", "traversalFailed": True}):
            with self.subTest(fixture=fixture), self.assertRaises(RuntimeError):
                self.collect(fixture)
        self.assertEqual(self.collect({"state": "clean", "traversalFailed": False}), [])

    def test_fixture_clean_may_not_hide_error_rows_or_counts(self) -> None:
        for extra in ({"pages": self.fixture()["pages"]}, {"errorCount": 1},
                      {"pages": {}}, {"errorCount": False}):
            with self.subTest(extra=extra), self.assertRaises(RuntimeError):
                self.collect({"state": "clean", "traversalFailed": False, **extra})

    def test_count_and_page_metadata_require_positive_integers_not_booleans(self) -> None:
        for field in ("errorCount", "page", "total"):
            for value in (True, False, 0, -1, 1.0, "1", None):
                fixture = self.fixture()
                target = fixture if field == "errorCount" else fixture["pages"][0]
                target[field] = value
                with self.subTest(field=field, value=value), self.assertRaises(RuntimeError):
                    self.collect(fixture)

    def test_failed_missing_or_untyped_traversal_metadata_refuses(self) -> None:
        for level in ("summary", "page"):
            for value in (True, None, 0, "false"):
                fixture = self.fixture()
                target = fixture if level == "summary" else fixture["pages"][0]
                target["traversalFailed"] = value
                with self.subTest(level=level, value=value), self.assertRaises(RuntimeError):
                    self.collect(fixture)
            fixture = self.fixture()
            target = fixture if level == "summary" else fixture["pages"][0]
            del target["traversalFailed"]
            with self.subTest(level=level, missing=True), self.assertRaises(RuntimeError):
                self.collect(fixture)

    def test_malformed_and_exhausted_fixture_pages_raise_controlled_errors(self) -> None:
        for pages in (None, {}, [None], [[]], [{"rows": None}], []):
            fixture = self.fixture()
            fixture["pages"] = pages
            with self.subTest(pages=pages), self.assertRaises(RuntimeError):
                self.collect(fixture)
        fixture = self.fixture()
        fixture["errorCount"] = 2
        fixture["pages"][0]["total"] = 2
        with self.assertRaises(RuntimeError):
            self.collect(fixture)
        fixture = self.fixture()
        fixture["pages"].append(dict(fixture["pages"][0]))
        with self.assertRaises(RuntimeError):
            self.collect(fixture)

    def test_live_protocol_requires_json_object(self) -> None:
        for value in (None, [], True, 1, "state", "private-sentinel"):
            response = SimpleNamespace(returncode=0, stdout=json.dumps(value), stderr="")
            with self.subTest(value=value), mock.patch.object(
                self.module["subprocess"], "run", return_value=response
            ), self.assertRaises(RuntimeError):
                self.module["AccessibilityPanel"]().open_error_list()

    def test_live_summary_uses_same_strict_schema(self) -> None:
        for result in ({"state": []}, {"state": {}}, {"state": "clean"},
                       {"state": "clean", "traversalFailed": True},
                       {"state": "clean", "traversalFailed": False, "errorCount": 3},
                       {"state": "errors", "traversalFailed": False, "errorCount": True}):
            with self.subTest(result=result), mock.patch.object(
                self.module["AccessibilityPanel"], "_run_jxa", return_value=result
            ), self.assertRaises(RuntimeError):
                self.module["AccessibilityPanel"]().open_error_list()

    def test_next_page_requires_successful_complete_click_result(self) -> None:
        for result in ({}, {"clicked": False}, {"clicked": 1},
                       {"clicked": True, "traversalFailed": True}):
            with self.subTest(result=result), mock.patch.object(
                self.module["AccessibilityPanel"], "_run_jxa", return_value=result
            ), self.assertRaises(RuntimeError):
                self.module["AccessibilityPanel"]().next_page()

    def test_jxa_timeout_and_transport_errors_are_bounded_and_sanitized(self) -> None:
        sentinel = "private-sentinel-provider-output"
        failures = (
            subprocess.TimeoutExpired("fixture", 1, output=sentinel, stderr=sentinel),
            OSError(sentinel),
        )
        for failure in failures:
            with self.subTest(failure=type(failure).__name__), mock.patch.object(
                self.module["subprocess"], "run", side_effect=failure
            ) as run, self.assertRaises(RuntimeError) as caught:
                self.module["AccessibilityPanel"]._run_jxa("fixture only")
            self.assertNotIn(sentinel, str(caught.exception))
            self.assertGreater(run.call_args.kwargs["timeout"], 0)
            self.assertLessEqual(run.call_args.kwargs["timeout"], 60)
        for response in (
            SimpleNamespace(returncode=1, stdout=sentinel, stderr=sentinel),
            SimpleNamespace(returncode=0, stdout=sentinel, stderr=""),
        ):
            with mock.patch.object(self.module["subprocess"], "run", return_value=response), \
                    self.assertRaises(RuntimeError) as caught:
                self.module["AccessibilityPanel"]._run_jxa("fixture only")
            self.assertNotIn(sentinel, str(caught.exception))

    def test_count_bounds_stop_collection_before_unbounded_pagination(self) -> None:
        fixture = self.fixture()
        page = fixture["pages"][0]
        for total, rows in ((1000000, page["rows"]),
                            (2, page["rows"] * 3)):
            panel = mock.Mock()
            panel.open_error_list.return_value = 2
            panel.read_page.return_value = {**page, "total": total, "rows": rows}
            with self.subTest(total=total), self.assertRaises(RuntimeError):
                self.module["collect_rows"](panel)
            panel.next_page.assert_not_called()

    def test_unknown_reason_or_missing_filename_refuses_entire_result(self) -> None:
        for row in (None, {}, {"name": "item", "reason": "new reason"},
                    {"name": "", "reason": "Can’t upload some files"}):
            fixture = self.fixture()
            fixture["pages"][0]["rows"] = [row]
            with self.subTest(row=row), self.assertRaises(RuntimeError):
                self.collect(fixture)

    def test_cli_fixture_label_cannot_be_mistaken_for_live_observation(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            result = DriveErrorsTests().run_script("list", Path(tmp), {
                "state": "clean", "traversalFailed": False,
            })
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("FIXTURE", result.stdout)
        self.assertIn("not live", result.stdout)

    def test_cli_live_label_is_scoped_to_observed_panel(self) -> None:
        stdout, stderr = io.StringIO(), io.StringIO()
        with tempfile.TemporaryDirectory() as tmp, mock.patch.dict(
            self.module["main"].__globals__, {"DRIVE_ROOT": Path(tmp)}
        ), mock.patch.dict(os.environ):
            os.environ.pop("DRIVE_ERRORS_PANEL_FIXTURE", None)
            with mock.patch.object(self.module["AccessibilityPanel"], "_run_jxa", return_value={
                "state": "clean", "traversalFailed": False,
            }), redirect_stdout(stdout), redirect_stderr(stderr):
                result = self.module["main"](["drive-errors", "list"])
        self.assertEqual(result, 0, stderr.getvalue())
        self.assertIn("observed", stdout.getvalue())
        self.assertIn("not all accounts", stdout.getvalue())
        self.assertNotIn("FIXTURE", stdout.getvalue())


@unittest.skipUnless(shutil.which("node"), "Node is required for the synthetic JXA runtime")
class DriveErrorsJxaTests(unittest.TestCase):
    """Execute the actual JXA strings with a fake accessibility tree, never macOS APIs."""

    HARNESS = r'''
const vm = require("node:vm");
const input = JSON.parse(require("node:fs").readFileSync(0, "utf8"));
const scenario = input.scenario;
let clicks = 0;
function element(role, title, children = [], click = null, broken = false) {
  return {
    role: () => role, title: () => title, value: () => "", subrole: () => "",
    uiElements: () => { if (broken) throw Error("synthetic inaccessible subtree"); return children; },
    click: () => { clicks++; if (click) click(); },
  };
}
const text = value => element("AXStaticText", value);
const reasons = ["Can’t upload some Google files", "Can’t upload some files"];
const rowReason = scenario === "unknown-reason" ? "Unsupported reason" : reasons[1];
const rows = [
  element("AXGroup", "", [element("AXLink", "duplicate"),
    element("AXGroup", "", [text(rowReason), ...(scenario === "mixed-reasons" ? [text(reasons[0])] : [])])]),
  element("AXGroup", "", [element("AXLink", "duplicate"), element("AXGroup", "", [text(reasons[1])])]),
];
const controls = [element("AXButton", "Previous page"), text("1/1"), element("AXButton", "Next page")];
if (scenario === "mixed-pagination") controls.push(text("1/2"));
const errorChildren = [...controls, ...rows];
if (scenario === "read-fault" || scenario === "next-fault") {
  errorChildren.push(element("AXGroup", "", [], null, true));
}
const errorWindow = element("AXWindow", "Google Drive Error list", errorChildren);
let windows;
if (input.kind === "OPEN_ERROR_LIST_JXA") {
  let children;
  if (scenario === "clean") children = [text("Up to date")];
  else if (scenario === "unrecognized") children = [text("Unrecognized localized state")];
  else {
    children = [element("AXGroup", "2 errors", [
      element("AXButton", "View", [], () => windows.push(errorWindow))])];
    if (scenario === "open-fault") children.push(element("AXGroup", "", [], null, true));
  }
  windows = [element("AXWindow", "Google Drive", children)];
} else windows = [errorWindow];
const proc = {windows: () => windows};
let result = null, error = null;
try {
  const serialized = vm.runInNewContext(input.source, {
    Application: name => {
      if (name !== "System Events") throw Error("unexpected API");
      return {processes: {byName: name => {
        if (name !== "Google Drive") throw Error("unexpected process");
        return proc;
      }}};
    },
    delay: () => {},
  }, {timeout: 1000});
  result = JSON.parse(serialized);
} catch (failure) { error = String(failure.message); }
process.stdout.write(JSON.stringify({result, error, clicks}));
'''

    def run_jxa(self, kind: str, scenario: str = "normal") -> dict:
        module = runpy.run_path(str(SCRIPT))
        result = subprocess.run(
            [shutil.which("node"), "-e", self.HARNESS],
            input=json.dumps({"kind": kind, "source": module[kind], "scenario": scenario}),
            capture_output=True, text=True, check=False, timeout=5,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads(result.stdout)

    def test_summary_results_and_failed_traversal_before_view_click(self) -> None:
        for scenario, state, clicks in (("normal", "errors", 1), ("clean", "clean", 0),
                                       ("unrecognized", "unrecognized", 0)):
            with self.subTest(scenario=scenario):
                result = self.run_jxa("OPEN_ERROR_LIST_JXA", scenario)
                self.assertIsNone(result["error"])
                self.assertEqual(result["result"]["state"], state)
                self.assertIs(result["result"]["traversalFailed"], False)
                self.assertEqual(result["clicks"], clicks)
        failed = self.run_jxa("OPEN_ERROR_LIST_JXA", "open-fault")
        self.assertIn("traversal", failed["error"])
        self.assertEqual(failed["clicks"], 0)

    def test_rows_preserve_duplicates_and_fail_closed_on_unknown_extraction(self) -> None:
        result = self.run_jxa("READ_PAGE_JXA")
        self.assertIsNone(result["error"])
        self.assertEqual([row["name"] for row in result["result"]["rows"]], ["duplicate"] * 2)
        self.assertIs(result["result"]["traversalFailed"], False)
        module = runpy.run_path(str(SCRIPT))
        for scenario in ("unknown-reason", "mixed-reasons", "mixed-pagination", "read-fault"):
            with self.subTest(scenario=scenario):
                result = self.run_jxa("READ_PAGE_JXA", scenario)
                self.assertIsNone(result["error"])
                self.assertIs(result["result"]["traversalFailed"], True)
                panel = mock.Mock()
                panel.open_error_list.return_value = 2
                panel.read_page.return_value = result["result"]
                with self.assertRaises(RuntimeError):
                    module["collect_rows"](panel)

    def test_next_page_does_not_click_after_incomplete_traversal(self) -> None:
        result = self.run_jxa("NEXT_PAGE_JXA")
        self.assertIsNone(result["error"])
        self.assertEqual(result["result"], {"clicked": True, "traversalFailed": False})
        self.assertEqual(result["clicks"], 1)
        failed = self.run_jxa("NEXT_PAGE_JXA", "next-fault")
        self.assertIn("traversal", failed["error"])
        self.assertEqual(failed["clicks"], 0)


if __name__ == "__main__":
    unittest.main()
