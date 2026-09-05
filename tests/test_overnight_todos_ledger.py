"""Isolated ledger regressions; never reads notes or runtime state outside fixtures."""
import argparse
import importlib.util
import io
import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from concurrent.futures import ThreadPoolExecutor
from contextlib import redirect_stdout
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "codex/skills/overnight-todos/ledger.py"
SPEC = importlib.util.spec_from_file_location("overnight_ledger", SCRIPT)
ledger = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ledger)


class LedgerTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="overnight-ledger-test-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.state = self.root / "state.json"
        self.note = self.root / "fixture.org"
        self.note.write_text("* TODO Fixture\n:PROPERTIES:\n:ID: fixture-id\n:END:\nContext.\n")
        self.forbidden_history = self.root / "unselected-history.md"
        patcher = mock.patch.object(ledger, "HISTORY_PATH", self.forbidden_history, create=True)
        patcher.start()
        self.addCleanup(patcher.stop)

    def entry(self, timestamp):
        return {"last_verdict": "BLOCKED", "heading_hash": "sha256:fixture",
                "last_attempted_at": timestamp}

    def record_args(self, **changes):
        values = {"ledger": str(self.state), "id": "fixture-id", "file": str(self.note),
                  "title": "Fixture", "verdict": "BLOCKED: Needs a user decision | ease=2",
                  "operation_id": "fixture-operation"}
        values.update(changes)
        return argparse.Namespace(**values)

    def test_corrupt_filter_refuses_without_renaming_or_resetting_state(self):
        self.state.write_bytes(b"{not valid json")
        source = self.root / "classifications.json"
        source.write_text(json.dumps({"blocked": [], "candidate": [], "investigate": []}))
        output = self.root / "filtered.json"
        before = set(self.root.iterdir())
        with self.assertRaises(ValueError), redirect_stdout(io.StringIO()):
            ledger.cmd_filter(argparse.Namespace(classifications=str(source), ledger=str(self.state),
                                                output=str(output), skip_window_days=14))
        self.assertEqual(self.state.read_bytes(), b"{not valid json")
        self.assertEqual(set(self.root.iterdir()), before)

    def test_naive_and_future_attempts_never_suppress_work(self):
        now = datetime.now(timezone.utc)
        for stamp in (now.replace(tzinfo=None).isoformat(), (now + timedelta(days=2)).isoformat()):
            with self.subTest(timestamp=stamp):
                self.assertFalse(ledger.should_skip(self.entry(stamp), "sha256:fixture", 14))

    def test_valid_recent_aware_attempt_still_suppresses_unchanged_work(self):
        stamp = (datetime.now(timezone.utc) - timedelta(hours=1)).isoformat()
        self.assertTrue(ledger.should_skip(self.entry(stamp), "sha256:fixture", 14))

    def test_parent_hash_changes_when_descendant_content_changes(self):
        self.note.write_text("* TODO Parent\n:PROPERTIES:\n:ID: parent\n:END:\nParent.\n"
                             "** Child\nOld child context.\n* Other\nUnrelated.\n")
        before = ledger.heading_hash(str(self.note), "parent")
        self.assertIsNotNone(before)
        self.note.write_text(self.note.read_text().replace("Old child", "New child"))
        self.assertNotEqual(ledger.heading_hash(str(self.note), "parent"), before)

    def test_file_level_id_hashes_file_node(self):
        self.note.write_text(":PROPERTIES:\n:ID: file-node\n:END:\n#+title: Fixture\nBody.\n* Child\nText.\n")
        self.assertIsNotNone(ledger.heading_hash(str(self.note), "file-node"))

    def test_literal_block_id_is_not_an_org_heading_identity(self):
        self.note.write_text("* Real heading\n#+begin_example\n:PROPERTIES:\n:ID: fake\n:END:\n#+end_example\n")
        self.assertIsNone(ledger.heading_hash(str(self.note), "fake"))

    def test_duplicate_ids_are_ambiguous(self):
        self.note.write_text("* One\n:PROPERTIES:\n:ID: duplicate\n:END:\nOne.\n"
                             "* Two\n:PROPERTIES:\n:ID: duplicate\n:END:\nTwo.\n")
        self.assertIsNone(ledger.heading_hash(str(self.note), "duplicate"))

    def test_verdict_rejects_unknown_kind_bad_separator_multiline_and_invalid_ease(self):
        for verdict in ("UNKNOWN: result", "BLOCKED reason | ease=2", "COMPLETED:",
                        "BLOCKED: reason | ease=9", "BLOCKED: reason | ease=0",
                        "BLOCKED: reason", "COMPLETED: okay | ease=99",
                        "FAILED: first\nCOMPLETED: second"):
            with self.subTest(verdict=verdict), self.assertRaises(ValueError):
                ledger.parse_verdict(verdict)

    def test_explicit_ledger_records_history_only_adjacent_to_it(self):
        with redirect_stdout(io.StringIO()):
            ledger.cmd_record(self.record_args())
        self.assertFalse(self.forbidden_history.exists())
        self.assertTrue((self.root / "history.md").exists())

    def legacy(self):
        entry = {"last_verdict": "BLOCKED", "last_ease": 2,
                 "last_reason": "BLOCKED: Earlier user decision | ease=2",
                 "last_attempted_at": datetime.now(timezone.utc).isoformat(),
                 "heading_hash": ledger.heading_hash(str(self.note), "fixture-id"), "attempts": 1}
        self.state.write_text(json.dumps({"version": 1, "updated_at": None, "todos": {"fixture-id": entry}}))

    def record(self, **kwargs):
        with redirect_stdout(io.StringIO()):
            ledger.cmd_record(self.record_args(**kwargs))
        return ledger.load_ledger(str(self.state))

    def test_version_one_is_read_only_until_explicit_migration(self):
        self.legacy()
        before = self.state.read_bytes()
        with self.assertRaisesRegex(ValueError, "explicit migrate"):
            self.record()
        self.assertEqual(self.state.read_bytes(), before)
        self.assertEqual(ledger.load_ledger(str(self.state))["version"], 1)

    def test_migration_preserves_exact_adjacent_legacy_history_bytes(self):
        self.legacy()
        history = self.root / "history.md"
        original = b"# Earlier record\r\nPrivate legacy bytes: \xff without trailing newline"
        history.write_bytes(original)
        self.forbidden_history.write_bytes(b"UNSELECTED GLOBAL HISTORY")
        with redirect_stdout(io.StringIO()):
            ledger.cmd_migrate(argparse.Namespace(ledger=str(self.state)))
        self.assertEqual(history.read_bytes(), original)
        state = self.record()
        self.assertEqual(state["todos"]["fixture-id"]["attempts"], 2)
        self.assertEqual(len(state["events"]), 1)
        self.assertTrue(history.read_bytes().startswith(original + b"\n"))
        self.assertNotIn(b"UNSELECTED", history.read_bytes())

    def test_absent_legacy_history_does_not_fabricate_past_entries(self):
        self.legacy()
        with redirect_stdout(io.StringIO()):
            ledger.cmd_migrate(argparse.Namespace(ledger=str(self.state)))
        history = self.root / "history.md"
        self.assertFalse(history.exists())
        self.assertEqual(ledger.load_ledger(str(self.state))["events"], [])
        self.record()
        self.assertNotIn(b"Earlier user decision", history.read_bytes())

    def test_new_ledger_refuses_to_adopt_even_empty_existing_history(self):
        (self.root / "history.md").touch()
        with self.assertRaisesRegex(ValueError, "explicit migration"):
            self.record()
        self.assertFalse(self.state.exists())

    def test_same_operation_is_idempotent_and_conflicting_reuse_refuses(self):
        self.record()
        before = self.state.read_bytes(), (self.root / "history.md").read_bytes()
        state = self.record()
        self.assertEqual(state["todos"]["fixture-id"]["attempts"], 1)
        self.assertEqual((self.state.read_bytes(), (self.root / "history.md").read_bytes()), before)
        with self.assertRaisesRegex(ValueError, "different request"):
            self.record(verdict="FAILED: Different result")
        self.assertEqual((self.state.read_bytes(), (self.root / "history.md").read_bytes()), before)

    def test_history_failure_is_explicit_and_same_operation_reconciles(self):
        original = ledger._atomic_write
        def fail_history(path, data, expected):
            if Path(path).name == "history.md":
                raise OSError("fixture history failure")
            return original(path, data, expected)
        with mock.patch.object(ledger, "_atomic_write", side_effect=fail_history):
            with self.assertRaisesRegex(ValueError, "verdict is recorded; history/checkpoint is incomplete"):
                self.record()
        state = ledger.load_ledger(str(self.state))
        self.assertEqual(state["history_rendered"], 0)
        self.assertEqual(state["todos"]["fixture-id"]["attempts"], 1)
        self.assertFalse((self.root / "history.md").exists())
        state = self.record()
        self.assertEqual(state["history_rendered"], 1)
        self.assertEqual(state["todos"]["fixture-id"]["attempts"], 1)

    def test_history_publication_then_exception_does_not_duplicate_event(self):
        original = ledger._atomic_write
        def write_then_fail(path, data, expected):
            result = original(path, data, expected)
            if Path(path).name == "history.md":
                raise OSError("publication result lost")
            return result
        with mock.patch.object(ledger, "_atomic_write", side_effect=write_then_fail):
            with self.assertRaisesRegex(ValueError, "history/checkpoint is incomplete"):
                self.record()
        before = (self.root / "history.md").read_bytes()
        state = self.record()
        self.assertEqual((self.root / "history.md").read_bytes(), before)
        self.assertEqual(len(state["events"]), 1)

    def test_state_publication_then_exception_reconciles_same_operation(self):
        original = ledger._atomic_write
        def write_then_fail(path, data, expected):
            result = original(path, data, expected)
            if Path(path) == self.state:
                raise OSError("state publication result lost")
            return result
        with mock.patch.object(ledger, "_atomic_write", side_effect=write_then_fail):
            with self.assertRaisesRegex(ValueError, "publication failed or is uncertain"):
                self.record()
        state = self.record()
        self.assertEqual(len(state["events"]), 1)
        self.assertEqual(state["todos"]["fixture-id"]["attempts"], 1)

    def test_foreign_history_edits_are_never_overwritten(self):
        self.record()
        history = self.root / "history.md"
        history.write_bytes(history.read_bytes() + b"User-owned edit\n")
        before = self.state.read_bytes(), history.read_bytes()
        with self.assertRaisesRegex(ValueError, "external edit retained"):
            self.record(operation_id="another-attempt")
        self.assertEqual((self.state.read_bytes(), history.read_bytes()), before)

    def test_external_state_change_during_history_write_is_not_overwritten(self):
        original = ledger._atomic_write
        def edit_state_after_history(path, data, expected):
            result = original(path, data, expected)
            if Path(path).name == "history.md":
                current = json.loads(self.state.read_text())
                current["operator_annotation"] = "Preserve this independent edit"
                self.state.write_text(json.dumps(current))
            return result
        with mock.patch.object(ledger, "_atomic_write", side_effect=edit_state_after_history):
            with self.assertRaisesRegex(ValueError, "history/checkpoint is incomplete"):
                self.record()
        self.assertEqual(json.loads(self.state.read_text())["operator_annotation"], "Preserve this independent edit")

    def test_parallel_cli_records_preserve_all_attempts_and_history(self):
        def invoke(number):
            return subprocess.run(
                [sys.executable, str(SCRIPT), "record", "--ledger", str(self.state),
                 "--id", "fixture-id", "--file", str(self.note), "--title", "Fixture",
                 "--verdict", "FAILED: Owned synthetic attempt", "--operation-id", f"parallel-{number}"],
                capture_output=True, text=True, timeout=10,
                env={**os.environ, "PYTHONDONTWRITEBYTECODE": "1"})
        with ThreadPoolExecutor(max_workers=4) as pool:
            results = list(pool.map(invoke, range(4)))
        self.assertTrue(all(result.returncode == 0 for result in results), [result.stderr for result in results])
        value = ledger.load_ledger(str(self.state))
        self.assertEqual(value["todos"]["fixture-id"]["attempts"], 4)
        self.assertEqual(len(value["events"]), 4)
        self.assertEqual(value["history_rendered"], 4)
        self.assertEqual((self.root / "history.md").read_text().count('"verdict":'), 4)

    def test_private_permissions_and_no_clobber_filter_output(self):
        self.record()
        for path in (self.state, self.root / "history.md", self.root / "state.json.lock"):
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)
        source = self.root / "classifications.json"
        source.write_text(json.dumps({"blocked": [], "candidate": [], "investigate": []}))
        output = self.root / "filtered.json"
        args = argparse.Namespace(classifications=str(source), ledger=str(self.state),
                                  output=str(output), skip_window_days=14)
        before = self.state.read_bytes(), (self.root / "history.md").read_bytes()
        with redirect_stdout(io.StringIO()):
            ledger.cmd_filter(args)
        self.assertEqual(output.stat().st_mode & 0o777, 0o600)
        with self.assertRaisesRegex(ValueError, "already exists"):
            ledger.cmd_filter(args)
        self.assertEqual((self.state.read_bytes(), (self.root / "history.md").read_bytes()), before)

    def test_filter_never_creates_missing_ledger_or_lock(self):
        source = self.root / "classifications.json"
        source.write_text(json.dumps({"blocked": [], "candidate": [], "investigate": []}))
        with redirect_stdout(io.StringIO()):
            ledger.cmd_filter(argparse.Namespace(classifications=str(source), ledger=str(self.state),
                                                output=str(self.root / "filtered.json"), skip_window_days=14))
        self.assertFalse(self.state.exists())
        self.assertFalse((self.root / "state.json.lock").exists())

    def test_ledger_and_history_symlink_leaves_refuse_without_mutating_target(self):
        target = self.root / "unrelated.json"
        target.write_bytes(b"Unrelated bytes")
        self.state.symlink_to(target)
        with self.assertRaisesRegex(ValueError, "non-symlink"):
            self.record()
        self.assertEqual(target.read_bytes(), b"Unrelated bytes")

    def test_ancestor_and_file_metadata_changes_invalidate_selected_subtree(self):
        original = ("#+filetags: :first:\n* Parent\n:PROPERTIES:\n:ID: parent\n:END:\nParent context.\n"
                    "** TODO Child\n:PROPERTIES:\n:ID: child\n:END:\nChild context.\n"
                    "* Unrelated\nUnrelated content.\n")
        self.note.write_text(original)
        before = ledger.heading_hash(str(self.note), "child")
        for change in (original.replace(":first:", ":second:"), original.replace("Parent context", "Updated context")):
            self.note.write_text(change)
            self.assertNotEqual(ledger.heading_hash(str(self.note), "child"), before)
        self.note.write_text(original.replace("Unrelated content", "Unrelated edited"))
        self.assertEqual(ledger.heading_hash(str(self.note), "child"), before)

    def test_ids_after_prose_or_inside_quote_blocks_are_not_adopted(self):
        for text in ("* Heading\nOrdinary prose.\n:PROPERTIES:\n:ID: wrong\n:END:\n",
                     "* Heading\nCLOSED: ordinary prose\n:PROPERTIES:\n:ID: wrong\n:END:\n",
                     "#+begin_quote\n* Fake\n:PROPERTIES:\n:ID: wrong\n:END:\n#+end_quote\n"):
            self.note.write_text(text)
            self.assertIsNone(ledger.heading_hash(str(self.note), "wrong"))

    def test_real_planning_line_before_properties_is_supported(self):
        self.note.write_text("* TODO Planned\nSCHEDULED: <2026-09-06 Sun> DEADLINE: <2026-09-07 Mon>\n"
                             ":PROPERTIES:\n:ID: planned\n:END:\nContext.\n")
        self.assertIsNotNone(ledger.heading_hash(str(self.note), "planned"))

    def test_invalid_skip_windows_are_refused_even_without_an_entry(self):
        for days in (-1, 10 ** 30):
            with self.subTest(days=days), self.assertRaises(ValueError):
                ledger.should_skip({}, None, days)

    def test_ambiguous_json_and_boolean_schema_are_refused(self):
        for data in (b'{"version":1,"todos":{"lost":{}},"todos":{}}',
                     b'{"version":1,"updated_at":NaN,"todos":{}}',
                     b'{"version":true,"todos":{}}'):
            self.state.write_bytes(data)
            with self.subTest(data=data), self.assertRaises(ValueError):
                ledger.load_ledger(str(self.state))
            self.assertEqual(self.state.read_bytes(), data)

    def test_heading_symlink_never_proves_note_identity(self):
        alias = self.root / "note-alias.org"
        alias.symlink_to(self.note)
        self.assertIsNone(ledger.heading_hash(str(alias), "fixture-id"))
        with self.assertRaisesRegex(ValueError, "identity could not be proved"):
            self.record(file=str(alias))

    def test_heading_changed_during_read_cannot_suppress_work(self):
        original = os.fstat
        count = 0
        def edit_after_read(fd):
            nonlocal count
            result = original(fd)
            count += 1
            if count == 2:
                self.note.write_text("Replacement note without the claimed ID.\n")
            return result
        with mock.patch.object(os, "fstat", side_effect=edit_after_read):
            self.assertIsNone(ledger.heading_hash(str(self.note), "fixture-id"))

    def test_owned_private_directory_checks_uid_and_rejects_symlink_parent(self):
        with self.subTest(case="foreign owner"), mock.patch.object(os, "geteuid", return_value=os.geteuid() + 1):
            with self.assertRaisesRegex(ValueError, "owned|owner"):
                ledger._private_parent(self.state)
        target = self.root / "private"
        target.mkdir(mode=0o700)
        alias = self.root / "private-alias"
        alias.symlink_to(target, target_is_directory=True)
        with self.subTest(case="symlink parent"), self.assertRaisesRegex(ValueError, "symlink|regular|owned"):
            ledger._private_parent(alias / "state.json")

    def test_missing_or_ambiguous_identity_allows_only_non_suppressing_failure_records(self):
        for ambiguous in (False, True):
            if ambiguous:
                self.note.write_text("* First\n:PROPERTIES:\n:ID: fixture-id\n:END:\n"
                                     "* Second\n:PROPERTIES:\n:ID: fixture-id\n:END:\n")
            else:
                self.note.unlink()
            for kind in ("FAILED", "DEFERRED"):
                with self.subTest(kind=kind, ambiguous=ambiguous):
                    state = self.record(verdict=kind + ": Identity could not be proved",
                                        operation_id=f"{kind}-{ambiguous}")
                    entry = state["todos"]["fixture-id"]
                    self.assertIsNone(entry["heading_hash"])
                    self.assertFalse(ledger.should_skip(entry, "sha256:any", 14))
            for verdict in ("COMPLETED: Not verified", "BLOCKED: Not verified | ease=2"):
                with self.subTest(verdict=verdict, ambiguous=ambiguous), self.assertRaisesRegex(ValueError, "identity could not be proved"):
                    self.record(verdict=verdict, operation_id=f"unproved-{verdict.split(':')[0]}-{ambiguous}")

    def test_pending_first_event_does_not_adopt_foreign_empty_history(self):
        original = ledger._atomic_write
        history = self.root / "history.md"
        def create_history_after_state(path, data, expected):
            result = original(path, data, expected)
            if Path(path) == self.state and not history.exists():
                history.touch()
            return result
        with mock.patch.object(ledger, "_atomic_write", side_effect=create_history_after_state):
            with self.assertRaisesRegex(ValueError, "history/checkpoint is incomplete"):
                self.record()
        self.assertEqual(history.read_bytes(), b"")

    def test_new_history_text_neutralizes_html_and_markdown_images(self):
        self.record(title='<img src="https://example.invalid/private"> ![image](https://example.invalid/image)')
        history = (self.root / "history.md").read_text()
        self.assertNotIn("<img", history)
        self.assertNotIn("![image]", history)

    def test_read_refuses_fifo_replacement_without_blocking(self):
        code = r'''
import importlib.util, os, sys
from pathlib import Path
from unittest import mock
spec = importlib.util.spec_from_file_location("ledger", sys.argv[1])
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
path = Path(sys.argv[2])
original = Path.lstat
replaced = False
def replace_after_lstat(selected):
    global replaced
    result = original(selected)
    if selected == path and not replaced:
        replaced = True
        selected.unlink()
        os.mkfifo(selected)
    return result
with mock.patch.object(Path, "lstat", new=replace_after_lstat):
    try:
        module._read(path)
    except (ValueError, OSError):
        sys.exit(0)
sys.exit(1)
'''
        result = subprocess.run([sys.executable, "-c", code, str(SCRIPT), str(self.note)],
                                capture_output=True, timeout=2,
                                env={**os.environ, "PYTHONDONTWRITEBYTECODE": "1"})
        self.assertEqual(result.returncode, 0, result.stderr)

    @unittest.skipUnless(shutil.which("emacs"), "standalone Emacs is unavailable")
    def test_native_org_identity_and_subtree_comparison(self):
        cases = {
            "heading": ("node", 1, True, "* TODO Node\n:PROPERTIES:\n:ID: node\n:END:\nBody.\n"),
            "planning": ("node", 1, True, "* TODO Node\nSCHEDULED: <2026-09-06 Sun> DEADLINE: <2026-09-07 Mon>\n:PROPERTIES:\n:ID: node\n:END:\nBody.\n"),
            "after_blank": ("node", 0, False, "* TODO Node\n\n:PROPERTIES:\n:ID: node\n:END:\nBody.\n"),
            "after_prose": ("node", 0, False, "* TODO Node\nProse.\n:PROPERTIES:\n:ID: node\n:END:\nBody.\n"),
            # Native Org accepts these layouts; our documented conservative subset refuses them.
            "unsupported_planning": ("node", 1, False, "* TODO Node\nCLOSED: ordinary prose\n:PROPERTIES:\n:ID: node\n:END:\nBody.\n"),
            "unsupported_quote_headline": ("node", 1, False, "#+begin_quote\n* Fake\n:PROPERTIES:\n:ID: node\n:END:\n#+end_quote\n"),
            "file": ("node", 1, True, ":PROPERTIES:\n:ID: node\n:END:\n#+title: Node\nBody.\n* Child\nChild body.\n"),
            "file_after_title": ("node", 0, False, "#+title: Node\n:PROPERTIES:\n:ID: node\n:END:\nBody.\n"),
            "file_after_comment": ("node", 1, True, "# A comment\n:PROPERTIES:\n:ID: node\n:END:\nBody.\n"),
            "file_after_blank": ("node", 1, True, "\n\n:PROPERTIES:\n:ID: node\n:END:\nBody.\n"),
            "source": ("node", 0, False, "* Real\n#+begin_src text\n:PROPERTIES:\n:ID: node\n:END:\n#+end_src\n"),
            "example": ("node", 0, False, "* Real\n#+begin_example\n:PROPERTIES:\n:ID: node\n:END:\n#+end_example\n"),
            "comment": ("node", 0, False, "* Real\n#+begin_comment\n:PROPERTIES:\n:ID: node\n:END:\n#+end_comment\n"),
            "parent": ("parent", 1, True, "#+filetags: :one:\n* Parent\n:PROPERTIES:\n:ID: parent\n:END:\nContext.\n** Child\n:PROPERTIES:\n:ID: child\n:END:\nOld.\n* Neighbor\nUnrelated.\n"),
            "child": ("child", 1, True, "#+filetags: :one:\n* Parent\n:PROPERTIES:\n:ID: parent\n:END:\nContext.\n** Child\n:PROPERTIES:\n:ID: child\n:END:\nOld.\n* Neighbor\nUnrelated.\n"),
            "duplicate": ("node", 2, False, "* First\n:PROPERTIES:\n:ID: node\n:END:\n* Second\n:PROPERTIES:\n:ID: node\n:END:\n"),
        }
        paths = []
        for name, (_node, _count, _hashed, content) in cases.items():
            path = self.root / (name + ".org")
            path.write_text(content)
            paths.append(str(path))
        parser = self.root / "native-parser.el"
        parser.write_text(r''';;; native-parser.el --- Owned fixture parser -*- lexical-binding: t; -*-
(setq user-emacs-directory
      (expand-file-name "emacs-state/" (file-name-directory load-file-name)))
(require 'org)
(require 'org-element)
(require 'json)
(let ((paths (delete "--" command-line-args-left))
      (enable-local-variables nil)
      (enable-local-eval nil)
      (org-mode-hook nil)
      (org-inhibit-startup t)
      results)
  (setq command-line-args-left nil)
  (dolist (path paths)
    (with-temp-buffer
      (insert-file-contents path)
      (org-mode)
      (let ((tree (org-element-parse-buffer)) ids)
        (org-element-map tree 'node-property
          (lambda (property)
            (when (equal (upcase (org-element-property :key property)) "ID")
              (let* ((owner (org-element-lineage property '(headline)))
                     (start (if owner (org-element-property :begin owner) (point-min)))
                     (end (if owner (org-element-property :end owner) (point-max))))
                (push `((id . ,(org-element-property :value property))
                        (kind . ,(if owner "headline" "file"))
                        (text . ,(buffer-substring-no-properties start end))) ids)))))
        (push `((file . ,(file-name-nondirectory path))
                (ids . ,(vconcat (nreverse ids)))) results))))
  (princ (json-encode `((emacs . ,emacs-version)
                       (org . ,(org-version))
                       (results . ,(vconcat (nreverse results)))))))
''')
        result = subprocess.run([shutil.which("emacs"), "-Q", "--batch", "-l", str(parser), "--", *paths],
                                capture_output=True, text=True, timeout=30, cwd=self.root)
        self.assertEqual(result.returncode, 0, result.stderr)
        native = json.loads(result.stdout)
        self.assertTrue(native["emacs"])
        self.assertTrue(native["org"])
        for item in native["results"]:
            name = Path(item["file"]).stem
            node_id, count, hashed, _content = cases[name]
            matches = [entry for entry in item["ids"] if entry["id"] == node_id]
            with self.subTest(case=name, emacs=native["emacs"], org=native["org"]):
                self.assertEqual(len(matches), count)
                self.assertEqual(ledger.heading_hash(str(self.root / item["file"]), node_id) is not None, hashed)
                if hashed:
                    self.assertEqual(count, 1)
                if name == "parent":
                    self.assertIn("** Child", matches[0]["text"])
                    self.assertNotIn("* Neighbor", matches[0]["text"])
                if name == "file":
                    self.assertEqual(matches[0]["kind"], "file")

    def test_paired_helpers_remain_identical(self):
        self.assertEqual(SCRIPT.read_bytes(), (ROOT / "claude/skills/overnight-todos/ledger.py").read_bytes())


if __name__ == "__main__":
    unittest.main()
