"""Synthetic-only triage validation, ranking, and private report publication."""

import contextlib
import importlib.util
import io
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = [ROOT / side / "skills/overnight-todos/triage.py" for side in ("codex", "claude")]


def load_triage():
    spec = importlib.util.spec_from_file_location("overnight_triage_fixture", SCRIPTS[0])
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class OvernightTodosTriageTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="overnight-triage-tests-", dir="/tmp")
        self.addCleanup(temporary.cleanup)
        self.directory = Path(temporary.name).resolve()
        self.source = self.directory / "dump.json"
        self.report = self.directory / "report.md"
        self.classifications = self.directory / "classifications.json"
        self.triage = load_triage()

    def record(self, **changes):
        record = {"id": "fixture-id", "file": "/nonexistent-fixture/notes.org",
                  "title": "Fix a typo", "todo": "TODO", "priority": None,
                  "effort": None, "tags": [], "olp": []}
        record.update(changes)
        return record

    def dump(self, value):
        self.source.write_text(json.dumps(value), encoding="utf-8")
        self.source.chmod(0o600)

    def arguments(self, *extra):
        return ["--input", str(self.source), "--output", str(self.report),
                "--classifications-out", str(self.classifications), *extra]

    def invoke(self, *extra, error=None):
        before = self.source.read_bytes() if self.source.is_file() else None
        output, errors = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(output), contextlib.redirect_stderr(errors):
            if error is None:
                self.triage.main(self.arguments(*extra))
            else:
                with self.assertRaises(SystemExit) as caught:
                    self.triage.main(self.arguments(*extra))
                self.assertEqual(caught.exception.code, 2)
                self.assertIn(error, errors.getvalue())
                self.assertNotIn("Traceback", errors.getvalue())
                self.assertEqual(output.getvalue(), "")
        if before is not None:
            self.assertEqual(self.source.read_bytes(), before)
        return output.getvalue(), errors.getvalue()

    def no_outputs(self):
        self.assertFalse(self.report.exists())
        self.assertFalse(self.classifications.exists())
        self.assertEqual(list(self.directory.glob(".triage-*")), [])

    def test_paired_cli_uses_private_outputs_and_never_dispatches(self):
        self.assertEqual(SCRIPTS[0].read_bytes(), SCRIPTS[1].read_bytes())
        self.dump([self.record()])
        original = self.source.read_bytes()
        for script in SCRIPTS:
            with self.subTest(script=script):
                self.report = self.directory / f"{script.parents[2].name}-report.md"
                self.classifications = self.directory / f"{script.parents[2].name}-classified.json"
                result = subprocess.run([sys.executable, str(script), *self.arguments("--mode", "act", "--max-tasks", "0")],
                                        cwd=self.directory, capture_output=True, text=True, timeout=10,
                                        env=dict(os.environ, PYTHONDONTWRITEBYTECODE="1"), check=False)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn("DISPATCHED: 0 (classification only)", result.stdout)
                for path in (self.report, self.classifications):
                    self.assertEqual(path.stat().st_mode & 0o777, 0o600)
                text = self.report.read_text()
                self.assertIn("no task was executed", text)
                self.assertIn("not a live-state check", text)
                self.assertNotIn("would be dispatched", text)
                self.assertEqual(json.loads(self.classifications.read_text())["candidate"][0]["id"], "fixture-id")
        self.assertEqual(self.source.read_bytes(), original)

    def test_priority_effort_ranking_and_unknown_effort_remain_heuristic(self):
        self.dump([self.record(id="slow", title="Fix issue", priority="1", effort="3:00"),
                   self.record(id="fast", title="Fix a typo", priority="2", effort="15m"),
                   self.record(id="unknown", title="Update a tag", priority=None, effort="unestimated"),
                   self.record(id="middle", title="Check a service", priority="1", effort="30m")])
        self.invoke()
        records = json.loads(self.classifications.read_text())["candidate"]
        self.assertEqual([record["id"] for record in records], ["middle", "fast", "slow", "unknown"])
        self.assertEqual([(record["_priority"], record["_difficulty"]) for record in records],
                         [(1, 2), (2, 1), (1, 5), (5, 1)])

    def test_project_tags_require_investigation_even_for_title_shortcuts(self):
        for title in ("Fix a typo", "Plan a project"):
            with self.subTest(title=title):
                self.assertEqual(self.triage.classify(self.record(title=title, tags=["project"]))[0], "investigate")
        self.assertEqual(self.triage.classify(self.record(title="Fix a typo"))[0], "candidate")
        self.assertEqual(self.triage.classify(self.record(title="Plan a project"))[0], "blocked")

    def test_ai_exemption_does_not_swallow_real_name_prefixes(self):
        for title in ("Ask Aisha for the date", "Ask Claudia for the date"):
            self.assertEqual(self.triage.classify(self.record(title=title))[0], "blocked")
        for title in ("Ask AI for a summary", "Ask Claude for a summary", "Ask myself about the result"):
            self.assertNotEqual(self.triage.classify(self.record(title=title))[0], "blocked")

    def test_specific_personal_planning_precedes_generic_planning(self):
        self.assertEqual(self.triage.classify(self.record(title="Plan career changes"))[:2], ("blocked", 4))
        self.assertEqual(self.triage.classify(self.record(title="Plan fixture task"))[:2], ("blocked", 3))

    def test_slash_delimited_reading_is_long_form_at_end_or_before_space(self):
        for title in ("Read /Fixture Book/", "Read /Fixture Book/ for context"):
            with self.subTest(title=title):
                self.assertEqual(self.triage.classify(self.record(title=title))[:2], ("blocked", 3))
        self.assertEqual(self.triage.classify(self.record(title="Read fixture paragraph"))[0], "candidate")

    def test_title_file_effort_markup_is_literal_and_json_preserves_metadata(self):
        record = self.record(title='Fix [a link](https://example.invalid)\n\n## COMPLETED\n<img src="https://example.invalid/pixel">',
                             file="/nonexistent-fixture/[download](https://example.invalid).org",
                             effort="unknown\n<script>fixture</script>")
        self.dump([record])
        self.invoke()
        text = self.report.read_text()
        self.assertNotIn("\n## COMPLETED", text)
        self.assertNotIn("<img", text)
        self.assertNotIn("<script>", text)
        self.assertNotIn("](https://", text)
        self.assertIn("&lt;img", text)
        result = json.loads(self.classifications.read_text())["candidate"][0]
        self.assertEqual({key: result[key] for key in record}, record)

    def test_home_abbreviation_requires_a_path_component_boundary(self):
        self.assertEqual(self.triage.fmt_file({"file": "/fixture/home/note.org"}, "/fixture/home"), "~/note.org")
        self.assertEqual(self.triage.fmt_file({"file": "/fixture/home-other/note.org"}, "/fixture/home"),
                         "/fixture/home-other/note.org")

    def test_malformed_roots_fail_before_outputs(self):
        for value in ({}, None, True, "fixture", ["fixture"]):
            with self.subTest(value=value):
                self.dump(value)
                self.invoke(error="input must be a JSON array" if not isinstance(value, list) else "required dump fields")
                self.no_outputs()

    def test_dump_field_types_and_supported_priorities_are_required(self):
        cases = [("id", ""), ("file", "relative.org"), ("title", []), ("todo", ""),
                 ("priority", True), ("priority", 1), ("priority", "0"), ("priority", "10"),
                 ("priority", "A"), ("effort", 20), ("tags", "project"), ("olp", None)]
        for field, value in cases:
            with self.subTest(field=field, value=value):
                self.dump([self.record(**{field: value})])
                self.invoke(error="priority" if field == "priority" else field)
                self.no_outputs()
        value = self.record()
        del value["effort"]
        self.dump([value])
        self.invoke(error="required dump fields")
        self.no_outputs()

    def test_duplicate_identity_refuses_instead_of_dispatching_twice(self):
        self.dump([self.record(), self.record(title="Check another result")])
        self.invoke(error="repeats a TODO identity")
        self.no_outputs()

    def test_bad_json_duplicate_fields_nonfinite_and_surrogates_are_sanitized(self):
        payloads = [('{"private-fixture-secret":', "UTF-8 JSON"),
                    ('[{"id":"one","id":"two"}]', "duplicate field"),
                    (json.dumps([self.record(priority="1")]).replace('"priority": "1"', '"priority": NaN'), "non-finite"),
                    (json.dumps([self.record(title="\ud800")]), "could not be rendered")]
        for payload, reason in payloads:
            with self.subTest(payload=repr(payload)):
                self.source.write_text(payload)
                _, errors = self.invoke(error=reason)
                self.assertNotIn("private-fixture-secret", errors)
                self.no_outputs()

    def test_negative_budget_refuses_and_empty_array_is_a_valid_empty_snapshot(self):
        self.dump([])
        self.invoke("--max-tasks", "-1", error="must be nonnegative")
        self.no_outputs()
        self.invoke("--max-tasks", "0")
        self.assertEqual(json.loads(self.classifications.read_text()), {"blocked": [], "candidate": [], "investigate": []})

    def test_nonregular_input_refuses_without_waiting_for_a_fifo_writer(self):
        os.mkfifo(self.source)
        result = subprocess.run([sys.executable, str(SCRIPTS[0]), *self.arguments()], cwd=self.directory,
                                capture_output=True, text=True, timeout=3, check=False,
                                env=dict(os.environ, PYTHONDONTWRITEBYTECODE="1"))
        self.assertEqual(result.returncode, 2)
        self.assertIn("regular JSON dump", result.stderr)
        self.no_outputs()

    def test_existing_report_classification_and_input_alias_never_overwrite(self):
        self.dump([self.record()])
        for destination in (self.report, self.classifications):
            with self.subTest(destination=destination):
                destination.write_text("existing private artifact")
                self.invoke(error="already exists")
                self.assertEqual(destination.read_text(), "existing private artifact")
                other = self.classifications if destination == self.report else self.report
                self.assertFalse(other.exists())
                destination.unlink()
        self.invoke("--output", str(self.source), error="already exists")
        self.no_outputs()
        self.invoke("--classifications-out", str(self.report.parent / "." / self.report.name), error="distinct")
        self.no_outputs()

    def test_symlink_destination_and_nonprivate_or_missing_parent_refuse(self):
        self.dump([self.record()])
        foreign = self.directory / "foreign.txt"
        foreign.write_text("preserve this fixture")
        self.report.symlink_to(foreign)
        self.invoke(error="already exists")
        self.assertTrue(self.report.is_symlink())
        self.assertEqual(foreign.read_text(), "preserve this fixture")
        self.assertFalse(self.classifications.exists())
        self.report.unlink()
        public = self.directory / "public"
        public.mkdir(mode=0o755)
        public.chmod(0o755)
        self.invoke("--output", str(public / "report.md"), error="mode-0700")
        self.assertEqual(list(public.iterdir()), [])
        self.invoke("--output", str(self.directory / "missing" / "report.md"), error="unavailable")
        self.assertFalse((self.directory / "missing").exists())
        self.no_outputs()

    def test_permissive_umask_cannot_make_outputs_public(self):
        self.dump([self.record()])
        previous = os.umask(0)
        try:
            self.invoke()
        finally:
            os.umask(previous)
        for path in (self.report, self.classifications):
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)
        self.assertEqual(list(self.directory.glob(".triage-*")), [])

    def test_staging_failure_has_no_published_output_or_raw_error(self):
        self.dump([self.record()])
        with mock.patch.object(self.triage.os, "fsync", side_effect=OSError("private-fixture-secret")):
            _, errors = self.invoke(error="publication failed")
        self.assertNotIn("private-fixture-secret", errors)
        self.no_outputs()

    def test_second_publication_failure_removes_only_owned_first_output(self):
        self.dump([self.record()])
        original_link = os.link
        calls = []
        def fail_second(source, target):
            calls.append(target)
            if len(calls) == 2:
                raise OSError("synthetic link failure")
            return original_link(source, target)
        with mock.patch.object(self.triage.os, "link", side_effect=fail_second):
            self.invoke(error="publication failed")
        self.assertEqual(len(calls), 2)
        self.no_outputs()

    def test_publication_performs_then_raises_cleans_exact_owned_output(self):
        self.dump([self.record()])
        original_link = os.link
        def effect_then_fail(source, target):
            original_link(source, target)
            raise OSError("synthetic post-effect failure")
        with mock.patch.object(self.triage.os, "link", side_effect=effect_then_fail):
            self.invoke(error="publication failed")
        self.no_outputs()

    def test_concurrent_replacement_is_not_removed_during_failed_publication(self):
        self.dump([self.record()])
        original_link = os.link
        def replace_then_fail(source, target):
            original_link(source, target)
            target.unlink()
            target.write_text("concurrent replacement")
            raise OSError("synthetic post-effect failure")
        with mock.patch.object(self.triage.os, "link", side_effect=replace_then_fail):
            self.invoke(error="publication failed")
        self.assertEqual(self.report.read_text(), "concurrent replacement")
        self.assertFalse(self.classifications.exists())
        self.assertEqual(list(self.directory.glob(".triage-*")), [])


if __name__ == "__main__":
    unittest.main()
