"""Offline walk-list regressions. Every mutable path is a disposable fixture."""
import contextlib
import importlib.util
import io
import json
import multiprocessing
import os
import shlex
import stat
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


REPO = Path(__file__).resolve().parents[1]
SCRIPT = REPO / "codex/programmatic-skills/walk-list/walk.py"
COPIES = [REPO / tool / family / "walk-list/walk.py"
          for tool in ("claude", "codex")
          for family in ("skills", "programmatic-skills")]


def load_walk(root):
    spec = importlib.util.spec_from_file_location("isolated_walk", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.DATA_ROOT = Path(root) / "data"
    module.REGISTRY_PATH = module.DATA_ROOT / "registry.json"
    module.OUTPUT_ROOT = Path(root) / "out"
    return module


def run_worker(root, command, args, result, entered=None, pause=None, release=None):
    """Independent processes exercise the real flock, never the user's store."""
    walk = load_walk(root)
    if pause is not None:
        function_name = "atomic_json" if command == "record" else "save_registry"
        original = getattr(walk, function_name)

        def paused(*values, **kwargs):
            pause.set()
            if not release.wait(10):
                raise RuntimeError("fixture synchronization timed out")
            return original(*values, **kwargs)

        setattr(walk, function_name, paused)
    output = io.StringIO()
    try:
        if entered is not None:
            entered.set()
        with contextlib.redirect_stdout(output):
            getattr(walk, f"cmd_{command}")(Path(args[0]), *args[1:])
        result.put(("ok", output.getvalue()))
    except BaseException as error:
        result.put(("error", str(error)))


class WalkListTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="walk-list-tests-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        self.walk = load_walk(self.root)
        self.source = self.root / "queue.json"
        self.source.write_text('["first", "second"]', encoding="utf-8")

    def call(self, command, *args):
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            getattr(self.walk, f"cmd_{command}")(*args)
        return output.getvalue()

    def start(self, cap=1, source=None):
        source = source or self.source
        output = self.call("start", source, cap)
        sid, directory = self.walk.resolve_session(source)
        return sid, directory, output

    def state(self, directory):
        return json.loads((directory / "state.json").read_text())

    def complete(self, source=None):
        source = source or self.source
        self.call("next", source, "first verdict")
        self.call("next", source, "second verdict")

    def claim(self, source=None):
        output = self.call("dispatch", source or self.source)
        return output.splitlines()[0].split(": ", 1)[1]

    def outputs(self):
        return sorted(self.walk.OUTPUT_ROOT.glob("*.walk-decisions.json"))

    def test_all_four_copies_match(self):
        self.assertTrue(all(path.read_bytes() == SCRIPT.read_bytes() for path in COPIES))

    def test_exact_path_never_falls_back_to_same_basename(self):
        self.start()
        wrong = self.root / "other" / self.source.name
        with self.assertRaisesRegex(SystemExit, "no active walk"):
            self.call("status", wrong)

    def test_nonowned_or_symlink_inputs_are_never_overwritten(self):
        sid, directory, _ = self.start()
        stub = self.source.read_bytes()
        for replacement in (b"unrelated replacement", json.dumps({
                self.walk.STUB_MARKER: True, "session_id": "other"}).encode()):
            with self.subTest(replacement=replacement):
                self.source.write_bytes(replacement)
                for command in ("restore", "abort"):
                    with self.assertRaises(SystemExit):
                        self.call(command, self.source)
                    self.assertEqual(self.source.read_bytes(), replacement)
                with self.assertRaises(SystemExit):
                    self.call("start", self.source, 1)
                self.assertTrue((directory / "source.json").exists())
        self.source.unlink()
        target = self.root / "target.json"
        target.write_bytes(stub)
        self.source.symlink_to(target)
        with self.assertRaises(SystemExit):
            self.call("abort", self.source)
        self.assertTrue(self.source.is_symlink())
        self.assertEqual(target.read_bytes(), stub)
        self.assertEqual(self.walk.load_registry()[str(self.source.absolute())], sid)

    def test_start_refuses_symlink_without_moving_it(self):
        linked = self.root / "linked.json"
        linked.symlink_to(self.source.name)
        with self.assertRaisesRegex(SystemExit, "symlink"):
            self.call("start", linked, 1)
        self.assertTrue(linked.is_symlink())
        self.assertEqual(self.source.read_text(), '["first", "second"]')
        self.assertEqual(self.walk.load_registry(), {})

    def test_legacy_sequential_state_and_prefix_stub_continue(self):
        _, directory, _ = self.start()
        state = self.state(directory)
        state.pop("sequential_index")
        state["decisions"] = [{"index": 0, "item": "first", "decision": "legacy"}]
        state["cursor"] = 1
        self.walk.atomic_json(directory / "state.json", state)
        stub = json.loads(self.source.read_text())
        stub.pop("session_id")
        self.source.write_text(json.dumps(stub))
        output = self.call("next", self.source, "current verdict")
        self.assertIn("WALK COMPLETE", output)
        self.assertEqual([d["decision"] for d in self.state(directory)["decisions"]],
                         ["legacy", "current verdict"])

    def test_pool_next_and_unshown_mode_switch_are_rejected(self):
        _, directory, _ = self.start(2)
        with self.assertRaisesRegex(SystemExit, "sequential mode"):
            self.call("next", self.source, "unseen verdict")
        self.call("set_max_concurrent", self.source, "1")
        with self.assertRaisesRegex(SystemExit, "not shown"):
            self.call("next", self.source, "still unseen")
        self.assertEqual(self.state(directory)["decisions"], [])
        self.assertIn("first", self.call("start", self.source, 1))
        self.call("next", self.source, "now shown")

    def test_resume_exposes_redispatch_head_even_at_end_of_cursor(self):
        _, directory, _ = self.start(2)
        first = self.claim()
        second = self.claim()
        self.call("record", self.source, second, "second done")
        self.call("release_stale", self.source, "0")
        self.call("set_max_concurrent", self.source, "1")
        with self.assertRaisesRegex(SystemExit, "not shown"):
            self.call("next", self.source, "unseen")
        output = self.call("start", self.source, 1)
        self.assertIn("=== ITEM 1 OF 2 ===\nfirst", output)
        self.assertNotIn("WALK COMPLETE", output)
        self.call("next", self.source, "first done")
        self.assertEqual(sorted(d["index"] for d in self.state(directory)["decisions"]), [0, 1])
        with self.assertRaisesRegex(SystemExit, "unknown"):
            self.call("record", self.source, first, "late")

    def test_mixed_decision_schemas_report_latest_append(self):
        _, directory, _ = self.start()
        state = self.state(directory)
        state["decisions"] = [{"index": 0, "decision": "old sequential"},
                              {"index": 1, "decision": "new pool",
                               "recorded_at": "2026-01-01T00:00:00+00:00"}]
        self.walk.atomic_json(directory / "state.json", state)
        self.assertIn("Last decision (item 2): new pool", self.call("status", self.source))

    def test_json_arrays_jsonl_arrays_objects_and_plaintext(self):
        for raw, expected in [('[1, 2]', [1, 2]), ('[1]\n[2]\n', [[1], [2]]),
                              ('[1]\n{"two":2}', [[1], {"two": 2}]),
                              ('{"one":1}\n{"two":2}', [{"one": 1}, {"two": 2}]),
                              ('first\n\nsecond', ["first", "second"])]:
            with self.subTest(raw=raw):
                self.assertEqual(self.walk.parse_items(raw), expected)
        with self.assertRaisesRegex(SystemExit, "malformed"):
            self.walk.parse_items('[1,\n broken]')

    def test_nonfinite_ages_rejected_without_releasing_claims(self):
        _, directory, _ = self.start(2)
        self.claim()
        original = self.state(directory)
        for age in ("nan", "NaN", "inf", "-inf", "Infinity", "-1"):
            with self.subTest(age=age), self.assertRaises(SystemExit):
                self.call("release_stale", self.source, age)
        self.assertEqual(self.state(directory), original)

    def test_cap_cannot_shrink_below_claims_and_legacy_slots_never_negative(self):
        _, directory, _ = self.start(2)
        self.claim()
        self.claim()
        with self.assertRaisesRegex(SystemExit, "cap cannot"):
            self.call("set_max_concurrent", self.source, "1")
        state = self.state(directory)
        self.assertEqual(state["max_concurrent"], 2)
        state["max_concurrent"] = 1
        self.walk.atomic_json(directory / "state.json", state)
        self.assertEqual(json.loads(self.call("pool_status", self.source))["available_slots"], 0)

    def test_restore_refuses_unfinished_claimed_and_redispatch_work(self):
        _, directory, _ = self.start(2)
        for phase in ("pending", "claimed", "redispatch"):
            if phase == "claimed":
                self.claim()
            if phase == "redispatch":
                self.call("release_stale", self.source, "0")
            with self.subTest(phase=phase), self.assertRaisesRegex(SystemExit, "unfinished|in-flight"):
                self.call("restore", self.source)
            self.assertTrue(self.walk.is_stub(self.source))
            self.assertTrue((directory / "source.json").exists())
            self.assertEqual(self.outputs(), [])

    def test_complete_restore_exports_exact_state_and_original_bytes(self):
        original = self.source.read_bytes()
        sid, directory, _ = self.start()
        self.complete()
        expected = self.state(directory)
        self.call("restore", self.source)
        self.assertEqual(self.source.read_bytes(), original)
        self.assertEqual(json.loads(self.outputs()[0].read_text()), dict(expected, session_id=sid))
        self.assertEqual(self.walk.load_registry(), {})
        self.assertFalse(directory.exists())

    def test_same_stem_exports_and_existing_evidence_are_preserved(self):
        first_original = self.source.read_bytes()
        self.start()
        self.complete()
        self.call("restore", self.source)
        first_export = self.outputs()[0]
        saved = first_export.read_bytes()
        other = self.root / "other" / self.source.name
        other.parent.mkdir()
        other.write_bytes(first_original)
        self.start(source=other)
        self.complete(other)
        self.call("restore", other)
        self.assertEqual(len(self.outputs()), 2)
        self.assertEqual(first_export.read_bytes(), saved)

    def test_abort_is_explicit_and_unknown_session_files_survive(self):
        original = self.source.read_bytes()
        _, directory, _ = self.start(2)
        self.claim()
        with self.assertRaisesRegex(SystemExit, "in-flight"):
            self.call("abort", self.source)
        self.call("release_stale", self.source, "0")
        extra = directory / "user-added.txt"
        extra.write_text("keep this")
        output = self.call("abort", self.source)
        self.assertEqual(self.source.read_bytes(), original)
        self.assertEqual(self.outputs(), [])
        self.assertEqual(extra.read_text(), "keep this")
        self.assertIn("Retained unexpected", output)
        self.assertEqual(sorted(p.name for p in directory.iterdir()), [extra.name])

    def test_start_state_registry_and_stub_failures_preserve_original(self):
        original = self.source.read_bytes()
        self.walk.ensure_root()
        prior = {"/unrelated/path": "untouched"}
        self.walk.save_registry(prior)
        original_atomic = self.walk.atomic_json
        original_exchange = self.walk.exchange_paths

        def fail_state(path, data):
            if path.name == "state.json":
                raise OSError("fixture state failure")
            return original_atomic(path, data)

        def fail_stub(source, target):
            if Path(target) == self.source:
                raise OSError("fixture stub failure")
            return original_exchange(source, target)

        for target, replacement in (("atomic_json", fail_state),
                                    ("save_registry", mock.Mock(side_effect=OSError("fixture registry failure"))),
                                    ("exchange_paths", fail_stub)):
            owner, name = self.walk, target
            with self.subTest(target=target), mock.patch.object(owner, name, replacement):
                with self.assertRaises(OSError):
                    self.call("start", self.source, 1)
            self.assertEqual(self.source.read_bytes(), original)
            self.assertEqual(self.walk.load_registry(), prior)

    def test_state_serialization_or_replace_failure_preserves_claim_and_no_ack(self):
        _, directory, _ = self.start(2)
        first, second = self.claim(), self.claim()
        self.call("record", self.source, first, "first persisted")
        original = (directory / "state.json").read_bytes()
        original_replace = self.walk.os.replace

        def fail_state(source, target):
            if Path(target) == directory / "state.json":
                raise OSError("fixture state replace failure")
            return original_replace(source, target)

        for owner, name, replacement in [
                (self.walk.json, "dumps", mock.Mock(side_effect=TypeError("fixture serialization failure"))),
                (self.walk.os, "replace", fail_state)]:
            captured = io.StringIO()
            with self.subTest(name=name), contextlib.redirect_stdout(captured), mock.patch.object(owner, name, replacement):
                with self.assertRaises((OSError, TypeError)):
                    self.walk.cmd_record(self.source, second, "must not be acknowledged")
            self.assertNotIn("Recorded", captured.getvalue())
            self.assertEqual((directory / "state.json").read_bytes(), original)
        self.call("record", self.source, second, "retry succeeds")

    def test_restore_output_failure_leaves_owned_stub_source_and_state(self):
        _, directory, _ = self.start()
        self.complete()
        stub, state = self.source.read_bytes(), (directory / "state.json").read_bytes()
        with mock.patch.object(self.walk.json, "dump", side_effect=OSError("fixture export failure")):
            with self.assertRaises(OSError):
                self.call("restore", self.source)
        self.assertEqual(self.source.read_bytes(), stub)
        self.assertEqual((directory / "state.json").read_bytes(), state)
        self.assertTrue((directory / "source.json").exists())
        self.assertEqual(self.outputs(), [])

    def test_restore_replace_and_registry_failures_are_retryable(self):
        sid, directory, _ = self.start()
        self.complete()
        stub, state = self.source.read_bytes(), (directory / "state.json").read_bytes()
        original_exchange = self.walk.exchange_paths

        def fail_replace(source, target):
            if Path(target) == self.source:
                raise OSError("fixture restore failure")
            return original_exchange(source, target)

        for owner, name, replacement in [
                (self.walk, "exchange_paths", fail_replace),
                (self.walk, "save_registry", mock.Mock(side_effect=OSError("fixture registry failure")))]:
            with self.subTest(name=name), mock.patch.object(owner, name, replacement):
                with self.assertRaises(OSError):
                    self.call("restore", self.source)
            self.assertEqual(self.source.read_bytes(), stub)
            self.assertEqual((directory / "state.json").read_bytes(), state)
            self.assertEqual(self.walk.resolve_session(self.source)[0], sid)
            self.assertTrue((directory / "source.json").exists())
        earlier = {p: p.read_bytes() for p in self.outputs()}
        self.call("restore", self.source)
        for path, content in earlier.items():
            self.assertEqual(path.read_bytes(), content)

    def test_registry_failure_rollback_never_overwrites_unrelated_replacement(self):
        _, directory, _ = self.start()
        self.complete()

        def replaced_before_failure(registry):
            self.source.write_text("unrelated concurrent replacement")
            raise OSError("fixture registry failure")

        with mock.patch.object(self.walk, "save_registry", replaced_before_failure):
            with self.assertRaises(OSError):
                self.call("restore", self.source)
        self.assertEqual(self.source.read_text(), "unrelated concurrent replacement")
        self.assertTrue((directory / "source.json").exists())
        self.assertTrue((directory / "state.json").exists())

    def test_start_rechecks_ownership_after_registry_preparation(self):
        original_save = self.walk.save_registry
        replaced = False

        def replace_original(registry):
            nonlocal replaced
            original_save(registry)
            if not replaced:
                self.source.write_text("unrelated replacement")
                replaced = True

        with mock.patch.object(self.walk, "save_registry", replace_original):
            with self.assertRaisesRegex(SystemExit, "input changed"):
                self.call("start", self.source, 1)
        self.assertEqual(self.source.read_text(), "unrelated replacement")
        self.assertEqual(self.walk.load_registry(), {})
        copies = list(self.walk.DATA_ROOT.glob("*/source.json"))
        self.assertEqual(len(copies), 1)
        self.assertEqual(copies[0].read_text(), '["first", "second"]')

    def test_failed_sequential_state_write_never_acknowledges_verdict(self):
        _, directory, _ = self.start()
        original = (directory / "state.json").read_bytes()
        captured = io.StringIO()
        with mock.patch.object(self.walk, "atomic_json", side_effect=OSError("fixture write failure")):
            with contextlib.redirect_stdout(captured), self.assertRaises(OSError):
                self.walk.cmd_next(self.source, "not persisted")
        self.assertNotIn("Recorded", captured.getvalue())
        self.assertNotIn("second", captured.getvalue())
        self.assertEqual((directory / "state.json").read_bytes(), original)

    def recovery_directories(self):
        return sorted(self.root.glob(f".{self.source.name}.walk-recovery-*"))

    def publication_race(self, command, *, second_replacement=False):
        original = self.source.read_bytes()
        if command == "restore":
            self.start()
            self.complete()
        native_exchange = self.walk.exchange_paths
        replacements = [b"foreign replacement one\n", b"foreign replacement two\n"]
        calls = []

        def replace_at_exchange(staged, target):
            if not calls or (second_replacement and len(calls) == 1):
                self.source.unlink()
                self.source.write_bytes(replacements[len(calls)])
            calls.append(True)
            return native_exchange(staged, target)

        expected_error = (self.walk.PublicationRecoveryError if second_replacement else SystemExit)
        output = io.StringIO()
        with mock.patch.object(self.walk, "exchange_paths", side_effect=replace_at_exchange):
            with contextlib.redirect_stdout(output), self.assertRaises(expected_error):
                if command == "start":
                    self.walk.cmd_start(self.source, 1)
                else:
                    self.walk.cmd_restore(self.source)
        self.assertEqual(len(calls), 2)
        self.assertEqual(self.source.read_bytes(), replacements[0])
        self.assertNotIn("STARTED", output.getvalue())
        self.assertNotIn("Restored", output.getvalue())
        saved_sources = list(self.walk.DATA_ROOT.glob("*/source.json"))
        self.assertEqual([path.read_bytes() for path in saved_sources], [original])
        if second_replacement:
            retained = self.recovery_directories()
            self.assertEqual(len(retained), 1)
            self.assertEqual(stat.S_IMODE(retained[0].stat().st_mode), 0o700)
            self.assertEqual((retained[0] / "entry").read_bytes(), replacements[1])
            self.assertTrue(self.walk.load_registry())
        else:
            self.assertEqual(self.recovery_directories(), [])
            self.assertEqual(bool(self.walk.load_registry()), command == "restore")

    def test_start_preserves_replacement_at_actual_native_exchange(self):
        self.publication_race("start")

    def test_restore_preserves_replacement_at_actual_native_exchange(self):
        self.publication_race("restore")

    def test_start_retains_both_foreign_files_when_rollback_also_races(self):
        self.publication_race("start", second_replacement=True)

    def test_restore_retains_both_foreign_files_when_rollback_also_races(self):
        self.publication_race("restore", second_replacement=True)

    def test_symlink_replacement_is_restored_without_touching_its_referent(self):
        self.start()
        self.complete()
        target = self.root / "foreign.txt"
        target.write_bytes(b"referent must not change")
        native_exchange = self.walk.exchange_paths
        calls = []

        def insert_link(staged, destination):
            if not calls:
                self.source.unlink()
                self.source.symlink_to(target)
            calls.append(True)
            return native_exchange(staged, destination)

        with mock.patch.object(self.walk, "exchange_paths", side_effect=insert_link):
            with self.assertRaisesRegex(SystemExit, "replacement preserved"):
                self.call("restore", self.source)
        self.assertTrue(self.source.is_symlink())
        self.assertEqual(target.read_bytes(), b"referent must not change")
        self.assertEqual(self.recovery_directories(), [])

    def test_successful_exchange_followed_by_error_is_rolled_back(self):
        original = self.source.read_bytes()
        native_exchange = self.walk.exchange_paths
        calls = []

        def exchange_then_error(staged, target):
            native_exchange(staged, target)
            calls.append(True)
            if len(calls) == 1:
                raise OSError("fixture interrupted after completed native exchange")

        with mock.patch.object(self.walk, "exchange_paths", side_effect=exchange_then_error):
            with self.assertRaisesRegex(OSError, "interrupted"):
                self.call("start", self.source, 1)
        self.assertEqual(len(calls), 2)
        self.assertEqual(self.source.read_bytes(), original)
        self.assertEqual(self.walk.load_registry(), {})
        self.assertEqual(self.recovery_directories(), [])

    def test_unavailable_native_exchange_never_uses_replace_fallback(self):
        original = self.source.read_bytes()
        with mock.patch.object(self.walk.sys, "platform", "unsupported-fixture"):
            with self.assertRaisesRegex(OSError, "unavailable"):
                self.call("start", self.source, 1)
        self.assertEqual(self.source.read_bytes(), original)
        self.assertEqual(self.walk.load_registry(), {})
        self.assertEqual(self.recovery_directories(), [])

    def test_missing_destination_at_exchange_is_not_recreated(self):
        original = self.source.read_bytes()
        native_exchange = self.walk.exchange_paths

        def remove_before_exchange(staged, target):
            self.source.unlink()
            return native_exchange(staged, target)

        with mock.patch.object(self.walk, "exchange_paths", side_effect=remove_before_exchange):
            with self.assertRaisesRegex(self.walk.PublicationRecoveryError, "retained"):
                self.call("start", self.source, 1)
        self.assertFalse(self.source.exists())
        self.assertEqual(len(self.recovery_directories()), 1)
        copies = list(self.walk.DATA_ROOT.glob("*/source.json"))
        self.assertEqual([path.read_bytes() for path in copies], [original])

    def test_private_created_modes_and_original_input_mode_mtime_restore(self):
        previous_mask = os.umask(0o022)
        self.addCleanup(os.umask, previous_mask)
        self.source.chmod(0o640)
        original_times = (1_600_000_000_000_000_000, 1_600_000_001_000_000_000)
        os.utime(self.source, ns=original_times)
        _, directory, _ = self.start()
        for path in (self.walk.DATA_ROOT, directory):
            self.assertEqual(stat.S_IMODE(path.stat().st_mode), 0o700)
        for path in (directory / "source.json", directory / "state.json",
                     self.walk.REGISTRY_PATH, self.walk.DATA_ROOT / "registry.lock"):
            self.assertEqual(stat.S_IMODE(path.stat().st_mode), 0o600)
        self.complete()
        self.assertEqual(stat.S_IMODE((directory / "state.lock").stat().st_mode), 0o600)
        self.call("restore", self.source)
        self.assertEqual(stat.S_IMODE(self.walk.OUTPUT_ROOT.stat().st_mode), 0o700)
        self.assertEqual(stat.S_IMODE(self.outputs()[0].stat().st_mode), 0o600)
        self.assertEqual(stat.S_IMODE(self.source.stat().st_mode), 0o640)
        self.assertEqual(self.source.stat().st_mtime_ns, original_times[1])

    def test_existing_readable_roots_are_not_chmodded_and_new_copies_are_private(self):
        self.walk.ensure_root()
        self.walk.DATA_ROOT.chmod(0o755)
        self.walk.OUTPUT_ROOT.mkdir(mode=0o755)
        previous = self.walk.OUTPUT_ROOT / "existing-evidence.txt"
        previous.write_bytes(b"existing user file")
        previous.chmod(0o644)
        _, directory, _ = self.start()
        self.assertEqual(stat.S_IMODE(directory.stat().st_mode), 0o700)
        self.assertEqual(stat.S_IMODE((directory / "source.json").stat().st_mode), 0o600)
        self.complete()
        self.call("restore", self.source)
        for path in (self.walk.DATA_ROOT, self.walk.OUTPUT_ROOT):
            self.assertEqual(stat.S_IMODE(path.stat().st_mode), 0o755)
        self.assertEqual(stat.S_IMODE(previous.stat().st_mode), 0o644)
        self.assertEqual(previous.read_bytes(), b"existing user file")
        self.assertEqual(stat.S_IMODE(self.outputs()[0].stat().st_mode), 0o600)

    def test_legacy_source_metadata_defaults_restore_without_migration(self):
        _, directory, _ = self.start()
        state = self.state(directory)
        for key in ("source_mode", "source_atime_ns", "source_mtime_ns"):
            state.pop(key)
        self.walk.atomic_json(directory / "state.json", state)
        stored = directory / "source.json"
        stored.chmod(0o640)
        os.utime(stored, ns=(1_500_000_000_000_000_000, 1_500_000_001_000_000_000))
        self.complete()
        self.call("restore", self.source)
        self.assertEqual(stat.S_IMODE(self.source.stat().st_mode), 0o640)
        self.assertEqual(self.source.stat().st_mtime_ns, 1_500_000_001_000_000_000)

    def test_original_mode_comes_from_the_same_captured_preimage(self):
        original_capture = self.walk.capture_entry
        changed = []

        def chmod_at_capture(path, **kwargs):
            if path == self.source and not changed:
                self.source.chmod(0o640)
                changed.append(True)
            return original_capture(path, **kwargs)

        with mock.patch.object(self.walk, "capture_entry", side_effect=chmod_at_capture):
            _, directory, _ = self.start()
        self.assertEqual(self.state(directory)["source_mode"], 0o640)
        self.complete()
        self.call("restore", self.source)
        self.assertEqual(stat.S_IMODE(self.source.stat().st_mode), 0o640)

    def test_symlink_store_root_is_refused_without_chmod_or_writes(self):
        target = self.root / "foreign-root"
        target.mkdir(mode=0o755)
        self.walk.DATA_ROOT.symlink_to(target, target_is_directory=True)
        with self.assertRaisesRegex(SystemExit, "non-symlink"):
            self.call("start", self.source, 1)
        self.assertEqual(list(target.iterdir()), [])
        self.assertEqual(stat.S_IMODE(target.stat().st_mode), 0o755)

    def test_symlink_session_root_is_refused_without_traversal(self):
        sid, directory, _ = self.start()
        retained = self.root / "relocated-session"
        directory.rename(retained)
        directory.symlink_to(retained, target_is_directory=True)
        with self.assertRaisesRegex(SystemExit, "non-symlink"):
            self.call("status", self.source)
        self.assertTrue((retained / "source.json").is_file())
        self.assertEqual(self.walk.load_registry()[str(self.source)], sid)

    def test_unowned_session_directory_is_refused_without_permission_changes(self):
        _, directory, _ = self.start()
        before = directory.stat().st_mode
        foreign_uid = os.getuid() + 1
        with mock.patch.object(self.walk.os, "getuid", return_value=foreign_uid):
            with self.assertRaisesRegex(SystemExit, "owned"):
                self.walk.resolve_session(self.source)
        self.assertEqual(directory.stat().st_mode, before)

    def test_start_reconciles_verified_registry_write_after_reported_error(self):
        original_save = self.walk.save_registry

        def saved_then_failed(registry):
            original_save(registry)
            raise OSError("fixture lost registry acknowledgement")

        errors = io.StringIO()
        with mock.patch.object(self.walk, "save_registry", side_effect=saved_then_failed):
            with contextlib.redirect_stderr(errors):
                _, directory, output = self.start()
        self.assertIn("exact intended bytes", errors.getvalue())
        self.assertIn("STARTED", output)
        self.assertTrue(self.walk.is_stub(self.source))
        self.assertEqual(self.state(directory)["cursor"], 0)
        self.assertEqual(self.recovery_directories(), [])

    def test_restore_reconciles_verified_registry_removal_after_reported_error(self):
        original = self.source.read_bytes()
        _, directory, _ = self.start()
        self.complete()
        original_save = self.walk.save_registry

        def saved_then_failed(registry):
            original_save(registry)
            raise OSError("fixture lost registry acknowledgement")

        errors = io.StringIO()
        with mock.patch.object(self.walk, "save_registry", side_effect=saved_then_failed):
            with contextlib.redirect_stderr(errors):
                output = self.call("restore", self.source)
        self.assertIn("exact intended bytes", errors.getvalue())
        self.assertIn("Restored", output)
        self.assertEqual(self.source.read_bytes(), original)
        self.assertEqual(self.walk.load_registry(), {})
        self.assertFalse(directory.exists())
        self.assertEqual(self.recovery_directories(), [])

    def test_unknown_registry_postimage_is_retained_without_false_rollback(self):
        original = self.source.read_bytes()
        self.walk.ensure_root()
        prior = {"/unrelated": "keep"}
        self.walk.save_registry(prior)
        original_save = self.walk.save_registry

        def unexpected_postimage(registry):
            original_save(dict(registry, **{"/concurrent": "preserve"}))
            raise OSError("fixture unexpected registry postimage")

        output = io.StringIO()
        with mock.patch.object(self.walk, "save_registry", side_effect=unexpected_postimage):
            with contextlib.redirect_stdout(output):
                with self.assertRaisesRegex(self.walk.RegistryRecoveryError, "uncertain"):
                    self.walk.cmd_start(self.source, 1)
        self.assertEqual(self.source.read_bytes(), original)
        observed = self.walk.load_registry()
        self.assertEqual(observed["/unrelated"], "keep")
        self.assertEqual(observed["/concurrent"], "preserve")
        self.assertIn(str(self.source), observed)
        self.assertEqual(len(self.recovery_directories()), 1)
        self.assertEqual(len(list(self.walk.DATA_ROOT.glob("*/source.json"))), 1)
        self.assertNotIn("STARTED", output.getvalue())

    def test_unreadable_registry_error_readback_is_explicit_uncertainty(self):
        original = self.source.read_bytes()
        original_save = self.walk.save_registry
        original_read = self.walk.registry_bytes
        reads = []

        def saved_then_failed(registry):
            original_save(registry)
            raise OSError("fixture write failure")

        def unreadable_after_write():
            reads.append(True)
            if len(reads) > 1:
                raise PermissionError("fixture unavailable registry")
            return original_read()

        with mock.patch.object(self.walk, "save_registry", side_effect=saved_then_failed):
            with mock.patch.object(self.walk, "registry_bytes", side_effect=unreadable_after_write):
                with self.assertRaisesRegex(self.walk.RegistryRecoveryError, "unreadable"):
                    self.call("start", self.source, 1)
        self.assertEqual(self.source.read_bytes(), original)
        self.assertEqual(len(self.recovery_directories()), 1)
        self.assertTrue(self.walk.load_registry())

    def test_start_retains_staging_if_registry_rollback_fails_before_effect(self):
        original = self.source.read_bytes()
        original_save = self.walk.save_registry
        calls = []

        def fail_second_save(registry):
            calls.append(True)
            if len(calls) == 2:
                raise OSError("fixture rollback write failed before effect")
            original_save(registry)

        with mock.patch.object(self.walk, "save_registry", side_effect=fail_second_save):
            with mock.patch.object(self.walk, "exchange_paths", side_effect=OSError("fixture exchange failed")):
                with self.assertRaisesRegex(self.walk.RegistryRecoveryError, "rollback failed"):
                    self.call("start", self.source, 1)
        self.assertEqual(len(calls), 2)
        self.assertEqual(self.source.read_bytes(), original)
        self.assertTrue(self.walk.load_registry())
        self.assertEqual(len(self.recovery_directories()), 1)
        self.assertTrue((self.recovery_directories()[0] / "entry").is_file())
        self.assertEqual(len(list(self.walk.DATA_ROOT.glob("*/source.json"))), 1)

    def test_fifo_registry_lock_refuses_without_waiting_for_a_reader(self):
        original = self.source.read_bytes()
        self.walk.ensure_root()
        os.mkfifo(self.walk.DATA_ROOT / "registry.lock", mode=0o600)
        status, _ = self.result(self.worker("start", [str(self.source), 1]))
        self.assertEqual(status, "error")
        self.assertEqual(self.source.read_bytes(), original)

    def test_fifo_state_lock_refuses_without_waiting_for_a_reader(self):
        _, directory, _ = self.start()
        before = (directory / "state.json").read_bytes()
        os.mkfifo(directory / "state.lock", mode=0o600)
        status, _ = self.result(self.worker("next", [str(self.source), "never recorded"]))
        self.assertEqual(status, "error")
        self.assertEqual((directory / "state.json").read_bytes(), before)

    def test_private_text_files_explicitly_use_utf8_and_binary_mode_stays_binary(self):
        target = self.root / "unicode-evidence.json"
        native_fdopen = self.walk.os.fdopen
        with mock.patch.object(self.walk.os, "fdopen", wraps=native_fdopen) as opened:
            with self.walk.private_file(target, os.O_WRONLY | os.O_CREAT | os.O_EXCL, "w") as stream:
                stream.write("caf\u00e9 \U0001f680")
        self.assertEqual(opened.call_args.kwargs, {"encoding": "utf-8"})
        self.assertEqual(target.read_bytes(), "caf\u00e9 \U0001f680".encode("utf-8"))
        binary = self.root / "binary-copy"
        with self.walk.private_file(binary, os.O_WRONLY | os.O_CREAT | os.O_EXCL, "wb") as stream:
            stream.write(b"\xff\x00")
        self.assertEqual(binary.read_bytes(), b"\xff\x00")

    def test_unicode_export_roundtrips_under_ascii_default_file_encoding(self):
        code = (
            "import contextlib,io,locale,sys\n"
            "from pathlib import Path\n"
            "from test_walk_list import load_walk\n"
            "root=Path(sys.argv[1]); walk=load_walk(root)\n"
            "source=root/'unicode.json'\n"
            "source.write_bytes('[\"caf\\u00e9 \\U0001f680\"]'.encode('utf-8'))\n"
            "with contextlib.redirect_stdout(io.StringIO()):\n"
            " walk.cmd_start(source,1)\n"
            " walk.cmd_next(source,'r\\u00e9sum\\u00e9')\n"
            " walk.cmd_restore(source)\n"
            "print(locale.getencoding())\n"
        )
        root = self.root / "ascii-native"
        root.mkdir()
        result = subprocess.run(
            [sys.executable, "-c", code, str(root)],
            env={"PATH": "/usr/bin:/bin", "TMPDIR": str(self.root),
                 "PYTHONPATH": str(REPO / "tests"), "PYTHONDONTWRITEBYTECODE": "1",
                 "PYTHONUTF8": "0", "PYTHONCOERCECLOCALE": "0",
                 "PYTHONIOENCODING": "utf-8", "LC_ALL": "C"},
            capture_output=True, text=True, encoding="utf-8", timeout=15,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(result.stdout.strip().lower(), {"ascii", "ansi_x3.4-1968", "us-ascii"})
        evidence = next((root / "out").glob("*.walk-decisions.json"))
        data = json.loads(evidence.read_text(encoding="utf-8"))
        self.assertEqual(data["decisions"][0]["item"], "caf\u00e9 \U0001f680")
        self.assertEqual(data["decisions"][0]["decision"], "r\u00e9sum\u00e9")

    def test_actionable_hints_quote_exact_script_path_input_and_placeholders(self):
        source = self.root / "items ;$(printf never) 'quoted'.json"
        source.write_bytes(self.source.read_bytes())
        _, _, output = self.start(source=source)

        def command_after(text, prefix):
            line = next(line for line in text.splitlines() if prefix in line)
            return shlex.split(line.split(prefix, 1)[1].split("  (", 1)[0])

        prefix = ["python", str(SCRIPT)]
        self.assertEqual(command_after(output, "When done: "),
                         prefix + ["next", str(source), "<decision>"])
        next_output = self.call("next", source, "first literal verdict")
        self.assertEqual(command_after(next_output, "When done: "),
                         prefix + ["next", str(source), "<decision>"])
        completed = self.call("next", source, "second literal verdict")
        self.assertEqual(command_after(completed, "Run: "), prefix + ["restore", str(source)])
        self.call("restore", source)
        with self.assertRaises(SystemExit) as error:
            self.call("status", source)
        self.assertEqual(shlex.split(str(error.exception).split("Run: ", 1)[1]),
                         prefix + ["start", str(source)])
        _, _, pool = self.start(cap=2, source=source)
        self.assertEqual(command_after(pool, "Use: "), prefix + ["dispatch", str(source)])

    def test_inflight_hints_include_literal_argv_and_worker_safety_boundary(self):
        self.start()
        self.claim()
        with self.assertRaises(SystemExit) as error:
            self.call("next", self.source, "unrecorded")
        message = str(error.exception)
        hints = message.split("`")
        self.assertEqual(shlex.split(hints[1]),
                         ["python", str(SCRIPT), "record", str(self.source), "<token>", "<decision>"])
        self.assertEqual(shlex.split(hints[3]),
                         ["python", str(SCRIPT), "release-stale", str(self.source), "<age-seconds>"])
        self.assertIn("confirming worker termination", message)
        self.assertIn("reconciling its effects", message)

    def worker(self, command, args, **kwargs):
        ctx = multiprocessing.get_context("spawn")
        result = ctx.Queue()
        process = ctx.Process(target=run_worker,
                              args=(self.root, command, args, result), kwargs=kwargs)
        process.start()

        def cleanup():
            if process.is_alive():
                process.terminate()
            process.join(5)
            result.close()
        self.addCleanup(cleanup)
        return process, result

    def result(self, worker):
        process, result = worker
        value = result.get(timeout=10)
        process.join(10)
        self.assertFalse(process.is_alive(), "fixture worker did not exit")
        self.assertEqual(process.exitcode, 0)
        return value

    def test_concurrent_starts_preserve_different_paths_and_share_same_path(self):
        other = self.root / "other.json"
        other.write_text('["another"]')
        ctx = multiprocessing.get_context("spawn")
        paused, release, entered = ctx.Event(), ctx.Event(), ctx.Event()
        first = self.worker("start", [str(self.source), 1], pause=paused, release=release)
        self.assertTrue(paused.wait(10))
        second = self.worker("start", [str(other), 1], entered=entered)
        self.assertTrue(entered.wait(10))
        release.set()
        self.assertEqual(self.result(first)[0], "ok")
        self.assertEqual(self.result(second)[0], "ok")
        self.assertEqual(set(self.walk.load_registry()), {str(self.source), str(other)})
        original = self.walk.load_registry()
        same1 = self.worker("start", [str(self.source), 1])
        same2 = self.worker("start", [str(self.source), 1])
        self.assertIn("RESUMING", self.result(same1)[1])
        self.assertIn("RESUMING", self.result(same2)[1])
        self.assertEqual(self.walk.load_registry(), original)

    def test_record_before_restore_exports_final_committed_verdict(self):
        self.source.write_text('["only item"]')
        self.start(2)
        token = self.claim()
        ctx = multiprocessing.get_context("spawn")
        paused, release, entered = ctx.Event(), ctx.Event(), ctx.Event()
        record = self.worker("record", [str(self.source), token, "final verdict"], pause=paused, release=release)
        self.assertTrue(paused.wait(10))
        restore = self.worker("restore", [str(self.source)], entered=entered)
        self.assertTrue(entered.wait(10))
        release.set()
        self.assertEqual(self.result(record)[0], "ok")
        self.assertEqual(self.result(restore)[0], "ok")
        snapshot = json.loads(self.outputs()[0].read_text())
        self.assertEqual(snapshot["decisions"][0]["decision"], "final verdict")
        self.assertEqual(snapshot["in_flight"], {})

    def test_abort_before_late_record_refuses_without_orphan_state(self):
        _, directory, _ = self.start(2)
        token = self.claim()
        self.call("release_stale", self.source, "0")
        ctx = multiprocessing.get_context("spawn")
        paused, release, entered = ctx.Event(), ctx.Event(), ctx.Event()
        abort = self.worker("abort", [str(self.source)], pause=paused, release=release)
        self.assertTrue(paused.wait(10))
        record = self.worker("record", [str(self.source), token, "too late"], entered=entered)
        self.assertTrue(entered.wait(10))
        release.set()
        self.assertEqual(self.result(abort)[0], "ok")
        status, message = self.result(record)
        self.assertEqual(status, "error")
        self.assertIn("no active walk", message)
        self.assertFalse(directory.exists())


if __name__ == "__main__":
    unittest.main()
