from __future__ import annotations

import filecmp
import json
import shutil
import subprocess
import unittest
from pathlib import Path

import yaml


DOTFILES = Path("/Users/pablostafforini/My Drive/dotfiles")
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
    "AXUIElementSetMessagingTimeout",
    "AXUIElementPostKeyboardEvent",
    "CGEventPost",
    "CGEventCreateMouseEvent",
)

SWIFT = shutil.which("swift")
requires_swift = unittest.skipUnless(SWIFT, "swift is not available")


def run_helper(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(HELPER), *args],
        capture_output=True,
        text=True,
        timeout=120,
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
                front = yaml.safe_load(text.split("---\n")[1])
                self.assertEqual("review-lulu-alert", front["name"])
                description = front["description"].lower()
                self.assertIn("lulu", description)
                self.assertLess(len(front["description"]), 1024)

    def test_openai_interface_metadata_is_present(self):
        for skill_dir in SKILL_DIRS:
            data = yaml.safe_load((skill_dir / "agents/openai.yaml").read_text(encoding="utf-8"))
            with self.subTest(skill=skill_dir.parent.parent.name):
                interface = data["interface"]
                self.assertTrue(interface["display_name"])
                self.assertTrue(interface["short_description"])
                self.assertIn("review-lulu-alert", interface["default_prompt"])


class HelperIsReadOnlyTests(unittest.TestCase):
    def test_helper_source_contains_no_mutating_accessibility_calls(self):
        source = HELPER.read_text(encoding="utf-8")
        for call in MUTATING_AX_CALLS:
            with self.subTest(call=call):
                self.assertNotIn(call, source)

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

    def test_queued_alerts_are_counted(self):
        report = parse_fixture("alert-queued-second.json")

        self.assertEqual(2, report["alert_windows_open"])
        self.assertEqual("/usr/bin/curl", report["process"]["path"])

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

        with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as handle:
            json.dump({"schema": "something-else/9", "elements": []}, handle)
            path = handle.name
        self.addCleanup(Path(path).unlink)

        result = run_helper("--fixture", path)

        self.assertEqual(64, result.returncode)
        self.assertIn("fixture schema", result.stderr)


@requires_swift
class LiveReadTests(unittest.TestCase):
    """Exercises the live Accessibility path against whatever LuLu is doing now."""

    def test_live_read_succeeds_and_reports_a_boolean_alert_state(self):
        result = run_helper()

        if result.returncode == 69:
            self.skipTest("LuLu is not running")
        if result.returncode == 77:
            self.skipTest("Accessibility permission is not granted to the test runner")

        self.assertEqual(0, result.returncode, result.stderr)
        report = json.loads(result.stdout)
        self.assertIn(report["alert_present"], (True, False))
        self.assertEqual("live", report["source"])
        self.assertEqual("com.objective-see.lulu.app", report["lulu"]["bundle_id"])

    def test_live_dump_round_trips_through_the_fixture_parser(self):
        dumped = run_helper("--dump")

        if dumped.returncode in (69, 77):
            self.skipTest("LuLu is unavailable or Accessibility is denied")
        self.assertEqual(0, dumped.returncode, dumped.stderr)

        import tempfile

        with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as handle:
            handle.write(dumped.stdout)
            path = handle.name
        self.addCleanup(Path(path).unlink)

        replayed = run_helper("--fixture", path)
        self.assertEqual(0, replayed.returncode, replayed.stderr)
        self.assertEqual("fixture", json.loads(replayed.stdout)["source"])


if __name__ == "__main__":
    unittest.main()
