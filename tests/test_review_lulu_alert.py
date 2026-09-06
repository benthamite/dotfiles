from __future__ import annotations

import filecmp
import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest import mock
from pathlib import Path

DOTFILES = Path(__file__).resolve().parents[1]
SKILL_DIRS = (
    DOTFILES / "macos/.claude/skills/review-lulu-alert",
    DOTFILES / "macos/.codex/skills/review-lulu-alert",
)
HELPER = SKILL_DIRS[1] / "scripts/inspect-lulu-alert"
FIXTURES = DOTFILES / "tests/fixtures/review-lulu-alert"

# Accessibility calls that would let the helper change LuLu's state. The whole
# point of the skill is that reviewing an alert cannot answer it, so the helper
# must not be able to press a button or set an attribute even by accident.
MUTATING_AX_CALLS = (
    "AXUIElementPerformAction",
    "AXUIElementSetAttributeValue",
    "AXUIElementPostKeyboardEvent",
    "CGEventPost",
    "CGEventCreateMouseEvent",
)

SWIFT = shutil.which("swift")
requires_swift = unittest.skipUnless(SWIFT and sys.platform == "darwin", "macOS Swift is not available")
TEST_ROOT = None


def setUpModule():
    global TEST_ROOT
    TEST_ROOT = tempfile.TemporaryDirectory(prefix="current63-lulu-tests-", dir="/private/tmp" if sys.platform == "darwin" else "/tmp")


def tearDownModule():
    TEST_ROOT.cleanup()


def yaml_scalar(value: str) -> str:
    """Parse the scalar subset used by skill metadata without PyYAML."""
    value = value.strip()
    if value.startswith('"'):
        parsed = json.loads(value)
        if not isinstance(parsed, str):
            raise ValueError("metadata scalar must be a string")
        return parsed
    if value.startswith("'") and value.endswith("'"):
        return value[1:-1].replace("''", "'")
    return value


def frontmatter_scalars(text: str) -> dict[str, str]:
    if not text.startswith("---\n"):
        raise ValueError("missing frontmatter")
    block, separator, _body = text[4:].partition("\n---\n")
    if not separator:
        raise ValueError("unterminated frontmatter")
    result: dict[str, str] = {}
    for line in block.splitlines():
        key, delimiter, value = line.partition(":")
        if not delimiter or not key or key[0].isspace():
            raise ValueError(f"unsupported frontmatter line: {line!r}")
        result[key] = yaml_scalar(value)
    return result


def interface_scalars(text: str) -> dict[str, str]:
    lines = text.splitlines()
    if not lines or lines[0] != "interface:":
        raise ValueError("missing interface mapping")
    result: dict[str, str] = {}
    for line in lines[1:]:
        if not line.startswith("  ") or line.startswith("    "):
            raise ValueError(f"unsupported interface line: {line!r}")
        key, delimiter, value = line.strip().partition(":")
        if not delimiter:
            raise ValueError(f"unsupported interface line: {line!r}")
        result[key] = yaml_scalar(value)
    return result


def run_helper(*args: str) -> subprocess.CompletedProcess[str]:
    if "--fixture" not in args and (not args or "--dump" in args):
        if os.environ.get("REVIEW_LULU_ALERT_LIVE") != "1":
            raise AssertionError("live reads require explicit REVIEW_LULU_ALERT_LIVE=1")
    return subprocess.run(
        [str(HELPER), *args],
        capture_output=True,
        text=True,
        timeout=120,
        cwd=TEST_ROOT.name,
        env=dict(os.environ, CLANG_MODULE_CACHE_PATH=TEST_ROOT.name + "/clang-cache",
                 SWIFT_MODULECACHE_PATH=TEST_ROOT.name + "/swift-cache"),
    )


def parse_fixture(name: str) -> dict:
    result = run_helper("--fixture", str(FIXTURES / name))
    if result.returncode != 0:
        raise AssertionError(f"helper failed on {name}: {result.stderr}")
    return json.loads(result.stdout)


class MirroredSkillTreeTests(unittest.TestCase):
    def test_claude_and_codex_copies_are_byte_identical(self):
        claude, codex = SKILL_DIRS
        claude_files = {p.relative_to(claude) for p in claude.rglob("*") if p.is_file()}
        codex_files = {p.relative_to(codex) for p in codex.rglob("*") if p.is_file()}
        self.assertEqual(claude_files, codex_files)
        self.assertTrue(claude_files, "skill tree is empty")

        for relative in sorted(claude_files):
            with self.subTest(path=relative):
                self.assertTrue(
                    filecmp.cmp(claude / relative, codex / relative, shallow=False),
                    f"mirrored file differs: {relative}",
                )

    def test_both_copies_ship_an_executable_helper(self):
        for skill_dir in SKILL_DIRS:
            helper = skill_dir / "scripts/inspect-lulu-alert"
            with self.subTest(skill=skill_dir.parent.parent.name):
                self.assertTrue(helper.is_file())
                self.assertTrue(helper.stat().st_mode & 0o111, "helper is not executable")

    def test_skill_frontmatter_declares_name_and_triggers(self):
        for skill_dir in SKILL_DIRS:
            text = (skill_dir / "SKILL.md").read_text(encoding="utf-8")
            with self.subTest(skill=skill_dir.parent.parent.name):
                self.assertTrue(text.startswith("---\n"))
                front = frontmatter_scalars(text)
                self.assertEqual("review-lulu-alert", front["name"])
                description = front["description"].lower()
                self.assertIn("lulu", description)
                self.assertLess(len(front["description"]), 1024)

    def test_openai_interface_metadata_is_present(self):
        for skill_dir in SKILL_DIRS:
            interface = interface_scalars(
                (skill_dir / "agents/openai.yaml").read_text(encoding="utf-8")
            )
            with self.subTest(skill=skill_dir.parent.parent.name):
                self.assertTrue(interface["display_name"])
                self.assertTrue(interface["short_description"])
                self.assertIn("review-lulu-alert", interface["default_prompt"])


class HelperIsReadOnlyTests(unittest.TestCase):
    def test_helper_source_contains_no_mutating_accessibility_calls(self):
        source = HELPER.read_text(encoding="utf-8")
        for call in MUTATING_AX_CALLS:
            with self.subTest(call=call):
                self.assertNotIn(call, source)

    @requires_swift
    def test_helper_declares_no_action_flags(self):
        result = run_helper("--help")
        self.assertEqual(0, result.returncode, result.stderr)
        for forbidden in ("--allow", "--block", "--click", "--apply"):
            self.assertNotIn(forbidden, result.stdout)


@requires_swift
class FixtureParsingTests(unittest.TestCase):
    def test_real_capture_parses_completely(self):
        """Anchored on an unedited `--dump` of a real LuLu 4.3.2 alert.

        Only the probe's path was shortened. If a LuLu update moves the detail
        columns or swaps the scope pop-up for something else, this is the test
        that notices.
        """
        report = parse_fixture("alert-real-capture.json")

        self.assertEqual([], report["unreadable_fields"])
        self.assertEqual("is connecting to 1.1.1.1", report["headline"])
        self.assertEqual(
            {
                "name": "lulu-alert-probe",
                "pid": 44067,
                "path": "/tmp/lulu-alert-probe",
                "args": "1.1.1.1 443",
            },
            report["process"],
        )
        self.assertEqual(
            {
                "ip_address": "1.1.1.1",
                "port_protocol": "443 (TCP)",
                "reverse_dns": "one.one.one.one",
            },
            report["connection"],
        )
        self.assertEqual("Process", report["rule"]["scope"])
        self.assertEqual("Process lifetime", report["rule"]["duration"])
        self.assertEqual("13:02:37", report["alert_timestamp"])

    def test_values_are_read_from_the_correct_detail_column(self):
        """LuLu draws each value inside its label's own frame, and puts the
        Connection column to the right of the Process column. Pairing that
        starts past the label's right edge silently reads the wrong column."""
        report = parse_fixture("alert-signed-tool.json")

        self.assertEqual(48213, report["process"]["pid"])
        self.assertEqual("140.82.121.6", report["connection"]["ip_address"])
        self.assertNotEqual(report["process"]["pid"], report["connection"]["ip_address"])

    def test_signed_tool_alert_reports_every_audit_field(self):
        report = parse_fixture("alert-signed-tool.json")

        self.assertTrue(report["alert_present"])
        self.assertEqual("fixture", report["source"])
        self.assertEqual([], report["unreadable_fields"])
        self.assertEqual(
            {
                "name": "gh",
                "pid": 48213,
                "path": "/opt/homebrew/bin/gh",
                "args": "pr list --repo benthamite/dotfiles",
            },
            report["process"],
        )
        self.assertEqual(
            {
                "ip_address": "140.82.121.6",
                "port_protocol": "443 (TCP)",
                "reverse_dns": "lb-140-82-121-6-iad.github.com",
            },
            report["connection"],
        )
        self.assertEqual("Remote Endpoint", report["rule"]["scope"])
        self.assertEqual("Process lifetime", report["rule"]["duration"])
        self.assertEqual("12:41:03", report["alert_timestamp"])
        self.assertEqual(["Block", "Allow"], report["buttons"])

    def test_preselected_broad_rule_is_reported_verbatim(self):
        """"Process" scope covers every endpoint, and "Always" never expires:
        together they are the broadest rule the alert can create."""
        report = parse_fixture("alert-unsigned-suspicious.json")

        self.assertEqual("Process", report["rule"]["scope"])
        self.assertEqual("Always", report["rule"]["duration"])
        self.assertEqual(
            "/Users/pablostafforini/Downloads/SoftwareUpdater", report["process"]["path"]
        )
        self.assertEqual("(unresolved)", report["connection"]["reverse_dns"])

    def test_popup_scope_is_reported_as_the_current_selection(self):
        """A pop-up button IS its selection, so `selected: false` beside a
        matching `rule.scope` would read as a contradiction. Its menu items only
        enter the tree once opened — a click — so the list is not exhaustive."""
        report = parse_fixture("alert-real-capture.json")
        scope_options = report["rule"]["scope_options"]

        self.assertEqual(1, len(scope_options))
        self.assertEqual("AXPopUpButton", scope_options[0]["role"])
        self.assertTrue(scope_options[0]["selected"])
        self.assertEqual(report["rule"]["scope"], scope_options[0]["label"])
        self.assertFalse(report["rule"]["scope_options_complete"])

        # Radio groups are fully readable, so theirs is exhaustive.
        self.assertTrue(report["rule"]["duration_options_complete"])

    def test_collapsed_rule_controls_are_not_claimed_complete(self):
        report = parse_fixture("alert-details-collapsed.json")

        self.assertEqual([], report["rule"]["scope_options"])
        self.assertFalse(report["rule"]["scope_options_complete"])
        self.assertFalse(report["rule"]["duration_options_complete"])

    def test_untitled_checkboxes_are_not_mistaken_for_rule_options(self):
        """LuLu's VirusTotal, signing-info and ancestry buttons are untitled
        check boxes sitting above the rule controls."""
        report = parse_fixture("alert-signed-tool.json")

        for option in report["rule"]["scope_options"] + report["rule"]["duration_options"]:
            self.assertIsNotNone(option["label"])
        self.assertEqual(
            ["Always", "Process lifetime", "Expires in:"],
            [option["label"] for option in report["rule"]["duration_options"]],
        )
        self.assertEqual("AXPopUpButton", report["rule"]["scope_options"][0]["role"])

    def test_collapsed_details_are_named_rather_than_guessed(self):
        report = parse_fixture("alert-details-collapsed.json")

        self.assertTrue(report["alert_present"])
        self.assertEqual("node", report["process"]["name"])
        self.assertIsNone(report["process"]["path"])
        self.assertIsNone(report["rule"]["scope"])
        for field in ("process_id", "process_path", "ip_address", "rule_scope"):
            self.assertIn(field, report["unreadable_fields"])

    def test_multiple_windows_are_counted_without_selecting_an_alert(self):
        result = run_helper("--fixture", str(FIXTURES / "alert-queued-second.json"))
        self.assertEqual(70, result.returncode)
        report = json.loads(result.stdout)
        self.assertEqual(2, report["alert_windows_open"])
        self.assertIsNone(report["alert_present"])
        self.assertEqual("incomplete", report["read_status"])
        self.assertNotIn("process", report)

    def test_no_alert_state_is_a_clean_success(self):
        result = run_helper("--fixture", str(FIXTURES / "no-alert.json"))

        self.assertEqual(0, result.returncode, result.stderr)
        report = json.loads(result.stdout)
        self.assertFalse(report["alert_present"])
        self.assertNotIn("process", report)


@requires_swift
class HelperErrorHandlingTests(unittest.TestCase):
    def test_unknown_argument_is_a_usage_error(self):
        result = run_helper("--allow")

        self.assertEqual(64, result.returncode)
        self.assertEqual("", result.stdout)
        self.assertIn("unknown argument", result.stderr)

    def test_missing_fixture_file_is_a_usage_error(self):
        result = run_helper("--fixture", str(FIXTURES / "does-not-exist.json"))

        self.assertEqual(64, result.returncode)
        self.assertIn("cannot read fixture file", result.stderr)

    def test_fixture_with_wrong_schema_is_rejected(self):
        import tempfile

        with tempfile.NamedTemporaryFile("w", suffix=".json", dir=TEST_ROOT.name, delete=False) as handle:
            json.dump({"schema": "something-else/9", "elements": []}, handle)
            path = handle.name
        self.addCleanup(Path(path).unlink)

        result = run_helper("--fixture", path)

        self.assertEqual(64, result.returncode)
        self.assertIn("fixture schema", result.stderr)


@requires_swift
class SyntheticSafetyTests(unittest.TestCase):
    def fixture(self, changes):
        fixture = json.loads((FIXTURES / "alert-signed-tool.json").read_text())
        changes(fixture)
        with tempfile.NamedTemporaryFile("w", suffix=".json", dir=TEST_ROOT.name, delete=False) as handle:
            json.dump(fixture, handle)
        self.addCleanup(Path(handle.name).unlink)
        return run_helper("--fixture", handle.name)

    def test_missing_left_value_does_not_borrow_connection_data(self):
        result = self.fixture(lambda root: root.__setitem__("elements", [
            item for item in root["elements"] if item["index"] not in (24, 34)]))
        report = json.loads(result.stdout)
        self.assertIsNone(report["process"]["args"])
        self.assertIsNone(report["process"]["path"])
        self.assertIn("process_args", report["unreadable_fields"])
        self.assertIn("process_path", report["unreadable_fields"])

    def test_missing_peer_labels_do_not_remove_the_detail_cell_boundary(self):
        result = self.fixture(lambda root: root.__setitem__("elements", [
            item for item in root["elements"] if item["index"] not in (18, 24, 25)]))
        report = json.loads(result.stdout)
        self.assertIsNone(report["process"]["args"])
        self.assertIn("process_args", report["unreadable_fields"])

    def test_duplicate_labels_are_ambiguous(self):
        def duplicate(root):
            item = dict(next(item for item in root["elements"] if item.get("value") == "pid:"))
            item["index"] = 100
            root["elements"].append(item)
        report = json.loads(self.fixture(duplicate).stdout)
        self.assertIsNone(report["process"]["pid"])
        self.assertIn("process_id", report["unreadable_fields"])

    def test_multiple_selected_durations_are_not_arbitrarily_chosen(self):
        def duplicate(root):
            next(item for item in root["elements"] if item.get("title") == "Always")["number_value"] = 1
        report = json.loads(self.fixture(duplicate).stdout)
        self.assertIsNone(report["rule"]["duration"])
        self.assertIn("rule_duration", report["unreadable_fields"])

    def test_malformed_fixture_does_not_claim_no_alert(self):
        for invalid in ({}, {"schema": "lulu-alert-ax-dump/1", "elements": "not an array"}):
            with self.subTest(invalid=invalid):
                result = self.fixture(lambda root: (root.clear(), root.update(invalid)))
                self.assertEqual(64, result.returncode)
                self.assertEqual("", result.stdout)

    def test_boolean_pid_is_rejected(self):
        result = self.fixture(lambda root: root["lulu"].__setitem__("pid", True))
        self.assertEqual(64, result.returncode)

    def test_duplicate_fixture_option_is_rejected(self):
        result = run_helper("--fixture", str(FIXTURES / "no-alert.json"),
                            "--fixture", str(FIXTURES / "alert-signed-tool.json"))
        self.assertEqual(64, result.returncode)
        self.assertEqual("", result.stdout)

    def test_unknown_control_state_is_not_false_or_complete(self):
        def unknown(root):
            next(item for item in root["elements"] if item.get("title") == "Always")["number_value"] = None
        report = json.loads(self.fixture(unknown).stdout)
        self.assertIsNone(report["rule"]["duration"])
        self.assertIsNone(report["rule"]["duration_options"][0]["selected"])
        self.assertFalse(report["rule"]["duration_options_complete"])

    def test_new_rule_labels_are_read_as_data(self):
        def newer(root):
            popup = next(item for item in root["elements"] if item["role"] == "AXPopUpButton")
            popup["title"] = "Process + Kids"
            selected = next(item for item in root["elements"] if item.get("title") == "Process lifetime")
            selected["title"] = "Once"
        report = json.loads(self.fixture(newer).stdout)
        self.assertEqual("Process + Kids", report["rule"]["scope"])
        self.assertEqual("Once", report["rule"]["duration"])

    def test_unreadable_control_label_cannot_disappear_from_a_complete_group(self):
        def hidden(root):
            control = next(item for item in root["elements"] if item.get("title") == "Always")
            control["title"] = None
            control["number_value"] = 1
        report = json.loads(self.fixture(hidden).stdout)
        self.assertIsNone(report["rule"]["duration"])
        self.assertFalse(report["rule"]["duration_options_complete"])
        self.assertIsNone(report["rule"]["duration_options"][0]["label"])

    def test_unlocated_control_cannot_make_selection_look_complete(self):
        def unlocated(root):
            next(item for item in root["elements"] if item.get("title") == "Always")["x"] = None
        report = json.loads(self.fixture(unlocated).stdout)
        self.assertIsNone(report["rule"]["duration"])
        self.assertFalse(report["rule"]["duration_options_complete"])

    def test_capture_issues_survive_replay_and_are_not_a_no_alert_success(self):
        result = self.fixture(lambda root: (root.__setitem__("read_issues", ["Synthetic AX failure"]),
                                           root.__setitem__("elements", []),
                                           root["window"].update(found=False, alert_window_count=0, matched_by=None)))
        self.assertEqual(70, result.returncode)
        report = json.loads(result.stdout)
        self.assertEqual(["Synthetic AX failure"], report["read_issues"])
        self.assertIsNone(report["alert_present"])
        self.assertNotIn("process", report)

    def test_partial_capture_does_not_claim_complete_radio_options(self):
        result = self.fixture(lambda root: root.__setitem__("read_issues", ["Synthetic truncated tree"]))
        self.assertEqual(70, result.returncode)
        report = json.loads(result.stdout)
        self.assertTrue(report["alert_present"])
        self.assertFalse(report["rule"]["duration_options_complete"])

    def test_capture_time_is_preserved_not_replaced_by_replay_time(self):
        report = json.loads(self.fixture(lambda root: root.__setitem__("captured_at", "2020-01-02T03:04:05Z")).stdout)
        self.assertEqual("2020-01-02T03:04:05Z", report["captured_at"])
        self.assertNotEqual(report["read_at"], report["captured_at"])
        self.assertFalse(report["snapshot_atomic"])

    def test_unsupported_and_contradictory_shapes_are_rejected(self):
        changes = [
            lambda root: root["lulu"].__setitem__("bundle_id", "example.invalid.wrong"),
            lambda root: root["window"].__setitem__("found", False),
            lambda root: root["window"].__setitem__("alert_window_count", True),
            lambda root: root["elements"][1].__setitem__("x", True),
            lambda root: root["elements"][1].__setitem__("index", 0),
            lambda root: root["elements"][1].__setitem__("depth", 41),
            lambda root: root["elements"][1].__setitem__("role", " "),
            lambda root: root["elements"][1].__setitem__("role", None),
            lambda root: root["elements"][1].__setitem__("index", 4000),
            lambda root: root["lulu"].__setitem__("pid", 2147483648),
            lambda root: root["lulu"].__setitem__("pid", None),
            lambda root: root["window"].__setitem__("matched_by", "guessed"),
            lambda root: root.__setitem__("unsupported", "not silently ignored"),
            lambda root: root.__setitem__("read_issues", "wrong type"),
        ]
        for index, change in enumerate(changes):
            with self.subTest(case=index):
                result = self.fixture(change)
                self.assertEqual(64, result.returncode)
                self.assertEqual("", result.stdout)

    def test_non_regular_and_oversized_fixtures_are_rejected_without_blocking(self):
        with tempfile.TemporaryDirectory(dir=TEST_ROOT.name) as staging:
            root = Path(staging)
            fifo = root / "owned.fifo"
            os.mkfifo(fifo)
            large = root / "oversized.json"
            with large.open("wb") as stream:
                stream.truncate(8 * 1024 * 1024 + 1)
            link = root / "owned-link.json"
            link.symlink_to(FIXTURES / "no-alert.json")
            for path in (fifo, root, large, link):
                with self.subTest(kind=path.name):
                    result = run_helper("--fixture", str(path))
                    self.assertEqual(64, result.returncode)

    def test_missing_expiry_fields_do_not_imply_a_known_duration(self):
        def expires(root):
            for item in root["elements"]:
                if item["role"] == "AXRadioButton":
                    item["number_value"] = int(item.get("title") == "Expires in:")
        report = json.loads(self.fixture(expires).stdout)
        self.assertIsNone(report["rule"]["duration"])
        self.assertIn("rule_duration", report["unreadable_fields"])

    def test_oversized_text_is_explicitly_unreadable(self):
        def large(root):
            next(item for item in root["elements"] if item["index"] == 24)["value"] = "x" * (16 * 1024 + 1)
        report = json.loads(self.fixture(large).stdout)
        self.assertIsNone(report["process"]["args"])
        self.assertIn("oversized_text", report["unreadable_fields"])
        self.assertIn("process_args", report["unreadable_fields"])

    def test_invalid_alert_pid_is_unknown_not_an_actionable_process(self):
        for pid in ("0", "-1", "2147483648"):
            with self.subTest(pid=pid):
                def invalid(root):
                    next(item for item in root["elements"] if item["index"] == 20)["value"] = pid
                report = json.loads(self.fixture(invalid).stdout)
                self.assertIsNone(report["process"]["pid"])
                self.assertIn("process_id", report["unreadable_fields"])

    def test_repeated_label_values_have_a_bounded_projection(self):
        def repeated(root):
            root["elements"] = [dict(index=index, depth=1, role="AXStaticText", value=f"custom {index}:",
                                      x=1, y=1, width=10, height=10) for index in range(100)]
            root["elements"].append(dict(index=100, depth=1, role="AXStaticText", value="x" * 16000,
                                         x=10, y=1, width=10, height=10))
        result = self.fixture(repeated)
        self.assertEqual(0, result.returncode, result.stderr)
        report = json.loads(result.stdout)
        self.assertIn("raw_label_pairs_truncated", report["unreadable_fields"])
        self.assertLess(len(result.stdout), 80000)

    def test_opt_in_live_assertions_accept_fixture_backed_complete_and_incomplete_results(self):
        partial = json.loads((FIXTURES / "alert-signed-tool.json").read_text())
        partial["read_issues"] = ["Synthetic AX read failure"]
        with tempfile.NamedTemporaryFile("w", suffix=".json", dir=TEST_ROOT.name, delete=False) as handle:
            json.dump(partial, handle)
        self.addCleanup(Path(handle.name).unlink)
        fixtures = [FIXTURES / name for name in ("no-alert.json", "alert-signed-tool.json", "alert-queued-second.json")]
        fixtures.append(Path(handle.name))
        actual_run = run_helper
        for fixture in fixtures:
            with self.subTest(fixture=fixture.name):
                parsed = actual_run("--fixture", str(fixture))
                report = json.loads(parsed.stdout)
                report["source"] = "live"  # Only the adapter label is replaced; data came from the fixture CLI.
                live_result = subprocess.CompletedProcess([], parsed.returncode, json.dumps(report), parsed.stderr)
                case = LiveReadTests("test_live_read_reports_complete_or_incomplete_alert_evidence")
                with mock.patch(__name__ + ".run_helper", return_value=live_result):
                    case.test_live_read_reports_complete_or_incomplete_alert_evidence()

                dumped = actual_run("--dump", "--fixture", str(fixture))
                def fixture_only(*args):
                    if args == ("--dump",):
                        return dumped
                    if len(args) == 2 and args[0] == "--fixture":
                        return actual_run(*args)
                    raise AssertionError("unexpected live invocation in fixture-backed assertion check")
                case = LiveReadTests("test_live_dump_round_trips_through_the_fixture_parser")
                try:
                    with mock.patch(__name__ + ".run_helper", side_effect=fixture_only):
                        case.test_live_dump_round_trips_through_the_fixture_parser()
                finally:
                    case.doCleanups()


@requires_swift
@unittest.skipUnless(os.environ.get("REVIEW_LULU_ALERT_LIVE") == "1", "live AX reads require explicit opt-in")
class LiveReadTests(unittest.TestCase):
    """Exercises the live Accessibility path against whatever LuLu is doing now."""

    def test_live_read_reports_complete_or_incomplete_alert_evidence(self):
        result = run_helper()

        if result.returncode == 69:
            self.skipTest("LuLu is not running")
        if result.returncode == 77:
            self.skipTest("Accessibility permission is not granted to the test runner")

        self.assertIn(result.returncode, (0, 70), result.stderr)
        report = json.loads(result.stdout)
        if result.returncode == 0:
            self.assertIsInstance(report["alert_present"], bool)
            self.assertEqual("complete", report["read_status"])
            self.assertEqual([], report["read_issues"])
        else:
            self.assertTrue(report["alert_present"] is None or report["alert_present"] is True)
            self.assertEqual("incomplete", report["read_status"])
            self.assertTrue(report["read_issues"])
        self.assertEqual("live", report["source"])
        self.assertEqual("com.objective-see.lulu.app", report["lulu"]["bundle_id"])

    def test_live_dump_round_trips_through_the_fixture_parser(self):
        dumped = run_helper("--dump")

        if dumped.returncode in (69, 77):
            self.skipTest("LuLu is unavailable or Accessibility is denied")
        self.assertIn(dumped.returncode, (0, 70), dumped.stderr)
        raw = json.loads(dumped.stdout)
        self.assertEqual(dumped.returncode == 70, bool(raw["read_issues"]))

        import tempfile

        with tempfile.NamedTemporaryFile("w", suffix=".json", dir=TEST_ROOT.name, delete=False) as handle:
            handle.write(dumped.stdout)
            path = handle.name
        self.addCleanup(Path(path).unlink)

        replayed = run_helper("--fixture", path)
        self.assertEqual(dumped.returncode, replayed.returncode, replayed.stderr)
        report = json.loads(replayed.stdout)
        self.assertEqual("fixture", report["source"])
        self.assertEqual("incomplete" if dumped.returncode == 70 else "complete", report["read_status"])
        self.assertEqual(raw["read_issues"], report["read_issues"])
        self.assertEqual(raw["captured_at"], report["captured_at"])


if __name__ == "__main__":
    unittest.main()
