"""Offline walk-list regressions. Every mutable path is a disposable fixture."""
import contextlib
import importlib.util
import io
import json
import multiprocessing
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
        original_replace = self.walk.os.replace

        def fail_state(path, data):
            if path.name == "state.json":
                raise OSError("fixture state failure")
            return original_atomic(path, data)

        def fail_stub(source, target):
            if Path(target) == self.source:
                raise OSError("fixture stub failure")
            return original_replace(source, target)

        for target, replacement in (("atomic_json", fail_state),
                                    ("save_registry", mock.Mock(side_effect=OSError("fixture registry failure"))),
                                    ("os.replace", fail_stub)):
            owner, name = (self.walk.os, "replace") if target == "os.replace" else (self.walk, target)
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
        original_replace = self.walk.os.replace

        def fail_replace(source, target):
            if Path(target) == self.source:
                raise OSError("fixture restore failure")
            return original_replace(source, target)

        for owner, name, replacement in [
                (self.walk.os, "replace", fail_replace),
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
