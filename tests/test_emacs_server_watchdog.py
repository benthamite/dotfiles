"""Contracts of the Emacs server watchdog, with probe and notifications faked."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import json
import os
from pathlib import Path
import sys
import tempfile
import time
import unittest
from unittest import mock


DOTFILES = Path(__file__).resolve().parents[1]


def load_watchdog():
    loader = importlib.machinery.SourceFileLoader(
        "isolated_emacs_server_watchdog", str(DOTFILES / "bin/emacs-server-watchdog"))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[loader.name] = module
    loader.exec_module(module)
    return module


watchdog = load_watchdog()


def failed(failure, detail="", answer=None):
    return {"answer": answer, "failure": failure, "detail": detail}


class DiagnoseTest(unittest.TestCase):
    def state(self, **fields):
        return {"time": 1000.0, "pid": 42, "recursion_depth": 0,
                "minibuffer_depth": 0, "prompt": None, "debugger": None, **fields}

    def diagnose(self, state, outcome=None, socket="unchanged", now=1010.0):
        return watchdog.diagnose(state, outcome or failed("timeout"), socket, now)

    def test_names_the_debugger_frame(self):
        cause = self.diagnose(self.state(
            recursion_depth=3,
            debugger="Debugger entered--entering a function:\n"
                     "* clear-minibuffer-message()\n  shell-command()"))
        self.assertIn("debugger", cause)
        self.assertIn("clear-minibuffer-message", cause)

    def test_names_the_prompt(self):
        cause = self.diagnose(self.state(minibuffer_depth=1,
                                         prompt="File changed on disk; reread? "))
        self.assertIn("File changed on disk", cause)

    def test_stale_state_means_emacs_is_blocked(self):
        cause = self.diagnose(self.state(), now=1100.0)
        self.assertIn("blocked for 100 s", cause)

    def test_stale_state_wins_over_an_old_debugger_entry(self):
        cause = self.diagnose(self.state(debugger="Debugger entered"), now=1100.0)
        self.assertIn("blocked", cause)

    def test_another_emacs_on_the_socket(self):
        cause = self.diagnose(self.state(), failed("other_emacs", answer=7))
        self.assertIn("pid 7", cause)
        self.assertIn("pid 42", cause)

    # Regression: on October 7 timeouts with a fresh state file and an
    # unchanged socket were reported as a missing or replaced socket.
    def test_a_timeout_with_fresh_state_means_a_slow_emacs(self):
        cause = self.diagnose(self.state(), failed("timeout"))
        self.assertIn("did not answer within 5 s", cause)
        self.assertIn("garbage collection", cause)
        self.assertNotIn("socket", cause)

    def test_a_missing_socket_file(self):
        cause = self.diagnose(self.state(), failed("timeout"), socket="missing")
        self.assertIn("socket file is missing", cause)
        cause = self.diagnose(self.state(), failed("no_socket"), socket="unknown")
        self.assertIn("socket file is missing", cause)

    def test_a_replaced_socket(self):
        cause = self.diagnose(self.state(), failed("timeout"), socket="replaced")
        self.assertIn("replaced", cause)

    def test_a_refused_connection(self):
        cause = self.diagnose(self.state(), failed("refused"))
        self.assertIn("refuses connections", cause)

    def test_another_error_quotes_emacsclient(self):
        cause = self.diagnose(self.state(), failed("error", "exit 1: boom"))
        self.assertIn("boom", cause)


class ProbeTest(unittest.TestCase):
    def run_probe(self, returncode, stdout="", stderr=""):
        result = mock.Mock(returncode=returncode, stdout=stdout, stderr=stderr)
        with mock.patch.object(watchdog.subprocess, "run", return_value=result):
            return watchdog.probe("/sock")

    def test_an_answer(self):
        self.assertEqual(self.run_probe(0, "42\n"),
                         {"answer": 42, "failure": None, "detail": None})

    # The messages below are emacsclient 30's, captured against a missing
    # socket, a socket nobody listens on and one that never replies.
    def test_classifies_a_timeout(self):
        outcome = self.run_probe(1, stderr="\nServer not responding; timed out after 5 seconds")
        self.assertEqual(outcome["failure"], "timeout")
        self.assertEqual(outcome["detail"],
                         "exit 1: Server not responding; timed out after 5 seconds")

    def test_classifies_a_refused_connection(self):
        outcome = self.run_probe(1, stderr=(
            f"{watchdog.EMACSCLIENT}: can't connect to /sock: Connection refused\n"
            f"{watchdog.EMACSCLIENT}: error accessing socket \"/sock\"\n"))
        self.assertEqual(outcome["failure"], "refused")
        self.assertEqual(outcome["detail"], "exit 1: can't connect to /sock: "
                         "Connection refused / error accessing socket \"/sock\"")

    def test_classifies_a_missing_socket(self):
        outcome = self.run_probe(1, stderr=(
            f"{watchdog.EMACSCLIENT}: can't find socket; have you started the server?\n"))
        self.assertEqual(outcome["failure"], "no_socket")

    def test_a_hung_emacsclient_is_a_timeout(self):
        with mock.patch.object(watchdog.subprocess, "run",
                               side_effect=watchdog.subprocess.TimeoutExpired("x", 15)):
            self.assertEqual(watchdog.probe("/sock")["failure"], "timeout")

    def test_detail_is_bounded(self):
        outcome = self.run_probe(1, stderr="x" * 1000)
        self.assertEqual(outcome["failure"], "error")
        self.assertLessEqual(len(outcome["detail"]), watchdog.DETAIL_LENGTH + 10)


class SocketTest(unittest.TestCase):
    def test_compares_with_the_last_answer(self):
        self.assertEqual(watchdog.compare_socket(None, [1, 2]), "missing")
        self.assertEqual(watchdog.compare_socket([1, 2], None), "unknown")
        self.assertEqual(watchdog.compare_socket([1, 2], [1, 2]), "unchanged")
        self.assertEqual(watchdog.compare_socket([3, 2], [1, 2]), "replaced")

    def test_reads_inode_and_mtime(self):
        with tempfile.NamedTemporaryFile() as f:
            identity = watchdog.socket_identity(f.name)
            self.assertEqual(identity[0], Path(f.name).stat().st_ino)
        self.assertIsNone(watchdog.socket_identity("/nonexistent/socket"))


class GcActivityTest(unittest.TestCase):
    def state(self, pid=42, count=100, elapsed=10.0):
        return {"pid": pid, "gcs_done": count, "gc_elapsed": elapsed,
                "gc_cons_threshold": 800000}

    def test_deltas_since_the_previous_run(self):
        previous = watchdog.gc_counters(self.state())
        activity = watchdog.gc_activity(self.state(count=130, elapsed=55.5), previous)
        self.assertEqual(activity, {"gc_count_delta": 30, "gc_seconds_delta": 45.5,
                                    "gc_cons_threshold": 800000})

    def test_no_deltas_across_emacs_restarts_or_without_counters(self):
        previous = watchdog.gc_counters(self.state())
        for state, prev in [(self.state(pid=43, count=130), previous),
                            (self.state(count=5), previous),
                            (self.state(), None),
                            ({"pid": 42}, previous)]:
            activity = watchdog.gc_activity(state, prev)
            self.assertIsNone(activity["gc_count_delta"])
            self.assertIsNone(activity["gc_seconds_delta"])


class MainTest(unittest.TestCase):
    def setUp(self):
        self.dir = Path(tempfile.mkdtemp())
        self.notified = []
        self.logged = []
        patches = [
            mock.patch.object(watchdog, "STATE_FILE", self.dir / "state.json"),
            mock.patch.object(watchdog, "WATCHDOG_FILE", self.dir / "watchdog.json"),
            mock.patch.object(watchdog, "socket_path", lambda: "/sock"),
            mock.patch.object(watchdog, "emacs_running", lambda pid: True),
            mock.patch.object(watchdog, "notify",
                              lambda message, title: self.notified.append(title)),
            mock.patch.object(watchdog, "log", self.logged.append),
            mock.patch.object(watchdog, "SAMPLES_FILE", self.dir / "samples.jsonl"),
            mock.patch.object(watchdog, "socket_identity", lambda socket: [1, 2]),
            mock.patch.object(watchdog, "memory_by_process",
                              lambda: [(42, 3500, 5000, "Emacs"),
                                       (9, 100, 23000, "Google Drive"),
                                       (7, 1600, None, "bztransmit")]),
            mock.patch.object(watchdog, "swap_used_mb", lambda: 36000),
            mock.patch.object(watchdog, "capture_stack", self.fake_capture),
        ]
        self.captures = []
        for patch in patches:
            patch.start()
            self.addCleanup(patch.stop)
        self.write_state(recursion_depth=1, debugger="Debugger entered")

    def fake_capture(self, pid, reason, now):
        self.captures.append((pid, reason))
        return f"/stacks/{reason}.txt"

    def write_state(self, **fields):
        (self.dir / "state.json").write_text(json.dumps(
            {"time": time.time(), "pid": 42, "recursion_depth": 0,
             "minibuffer_depth": 0, "prompt": None, "debugger": None,
             "gcs_done": 100, "gc_elapsed": 10.0, "gc_cons_threshold": 800000,
             **fields}))

    def run_with_answer(self, answer, failure="timeout"):
        outcome = ({"answer": answer, "failure": None, "detail": None}
                   if answer is not None else
                   {"answer": None, "failure": failure, "detail": "exit 1: why"})
        with mock.patch.object(watchdog, "probe", lambda socket: outcome):
            watchdog.main()

    def test_alerts_once_after_two_failures_then_on_recovery(self):
        self.run_with_answer(None)
        self.assertEqual(self.notified, [])
        self.run_with_answer(None)
        self.run_with_answer(None)
        self.assertEqual(self.notified, ["Emacs server not answering"])
        self.run_with_answer(42)
        self.assertEqual(self.notified, ["Emacs server not answering",
                                         "Emacs server answering again"])
        self.assertTrue(self.logged[0].startswith("outage: Server parked in the debugger"))
        self.assertTrue(self.logged[0].endswith("[probe: timeout: exit 1: why]"))

    def test_logs_a_slow_emacs_with_the_probe_failure(self):
        self.write_state()
        self.run_with_answer(42)
        self.run_with_answer(None)
        self.run_with_answer(None)
        (line,) = self.logged
        self.assertIn("did not answer within 5 s", line)
        self.assertIn("[probe: timeout: exit 1: why]", line)

    def test_a_socket_changed_since_the_last_answer_is_reported(self):
        self.write_state()
        self.run_with_answer(42)
        with mock.patch.object(watchdog, "socket_identity", lambda socket: [9, 9]):
            self.run_with_answer(None)
            self.run_with_answer(None)
        self.assertIn("socket was replaced", self.logged[0])
        self.assertEqual(self.samples()[-1]["socket"], "replaced")

    def test_a_healthy_server_stays_silent(self):
        for _ in range(3):
            self.run_with_answer(42)
        self.assertEqual(self.notified, [])

    def test_a_single_failure_is_not_an_outage(self):
        self.run_with_answer(None)
        self.run_with_answer(42)
        self.run_with_answer(None)
        self.assertEqual(self.notified, [])

    def test_a_closed_emacs_is_not_an_outage(self):
        with mock.patch.object(watchdog, "emacs_running", lambda pid: False):
            self.run_with_answer(None)
            self.run_with_answer(None)
        self.assertEqual(self.notified, [])

    def samples(self):
        lines = (self.dir / "samples.jsonl").read_text().splitlines()
        return [json.loads(line) for line in lines]

    def test_each_run_records_a_sample(self):
        self.run_with_answer(42)
        self.run_with_answer(None)
        first, second = self.samples()
        self.assertEqual(first["emacs_pid"], 42)
        self.assertTrue(first["answered"])
        self.assertFalse(second["answered"])
        self.assertIsInstance(first["probe_seconds"], float)
        self.assertEqual(first["emacs_memory_mb"], 5000)
        self.assertEqual(first["emacs_rss_mb"], 3500)
        self.assertEqual(first["swap_used_mb"], 36000)
        self.assertEqual(len(first["load"]), 3)
        self.assertEqual(first["top_memory_mb"],
                         [["Google Drive", 23000, 100], ["Emacs", 5000, 3500]])
        self.assertEqual(first["top_rss_mb"],
                         [["Emacs", 3500], ["bztransmit", 1600], ["Google Drive", 100]])
        self.assertIsNone(first["failure"])
        self.assertEqual(second["failure"], "timeout")
        self.assertEqual(second["failure_detail"], "exit 1: why")
        self.assertEqual(first["socket"], "unknown")
        self.assertEqual(second["socket"], "unchanged")
        self.assertLess(second["state_age_seconds"], 5)
        self.assertEqual(second["gc_cons_threshold"], 800000)

    def test_samples_record_garbage_collection_since_the_last_run(self):
        self.run_with_answer(42)
        self.write_state(gcs_done=112, gc_elapsed=40.25)
        self.run_with_answer(None)
        first, second = self.samples()
        self.assertIsNone(first["gc_count_delta"])
        self.assertEqual(second["gc_count_delta"], 12)
        self.assertEqual(second["gc_seconds_delta"], 30.25)

    def test_a_closed_emacs_still_records_the_machine(self):
        with mock.patch.object(watchdog, "emacs_running", lambda pid: False):
            self.run_with_answer(None)
        (sample,) = self.samples()
        self.assertIsNone(sample["emacs_pid"])
        self.assertIsNone(sample["answered"])
        self.assertEqual(sample["swap_used_mb"], 36000)

    def test_a_failed_probe_captures_a_stack_named_in_the_sample(self):
        self.write_state()
        self.run_with_answer(42)
        self.run_with_answer(None)
        self.assertEqual(self.captures, [(42, "timeout")])
        first, second = self.samples()
        self.assertNotIn("stack_file", first)
        self.assertEqual(second["stack_file"], "/stacks/timeout.txt")

    def test_captures_are_throttled_during_a_long_outage(self):
        self.write_state()
        self.run_with_answer(None)
        self.run_with_answer(None)
        self.assertEqual(len(self.captures), 1)
        state = json.loads((self.dir / "watchdog.json").read_text())
        state["last_capture"] -= watchdog.CAPTURE_INTERVAL
        (self.dir / "watchdog.json").write_text(json.dumps(state))
        self.run_with_answer(None)
        self.assertEqual(len(self.captures), 2)

    def test_a_gc_storm_captures_a_stack_even_when_the_server_answers(self):
        self.write_state()
        self.run_with_answer(42)
        self.write_state(gcs_done=100 + watchdog.GC_STORM - 1)
        self.run_with_answer(42)
        self.assertEqual(self.captures, [])
        self.write_state(gcs_done=100 + 2 * watchdog.GC_STORM)
        self.run_with_answer(42)
        self.assertEqual(self.captures, [(42, "gc-storm")])

    def test_old_samples_are_trimmed_once_past_the_slack(self):
        now = time.time()
        day = 86400
        path = self.dir / "samples.jsonl"
        recent = json.dumps({"time": round(now - day)}) + "\n"
        path.write_text(json.dumps({"time": round(now - 7.5 * day)}) + "\n" + recent)
        watchdog.trim_samples(now)
        self.assertEqual(len(path.read_text().splitlines()), 2)
        path.write_text(json.dumps({"time": round(now - 8.5 * day)}) + "\n" + recent)
        watchdog.trim_samples(now)
        self.assertEqual(path.read_text(), recent)


class CaptureStackTest(unittest.TestCase):
    def setUp(self):
        self.dir = Path(tempfile.mkdtemp())
        patch = mock.patch.object(watchdog, "STACKS_DIR", self.dir / "stacks")
        patch.start()
        self.addCleanup(patch.stop)

    def test_samples_into_a_private_directory_and_trims_old_reports(self):
        stacks = self.dir / "stacks"
        stacks.mkdir()
        old = stacks / "old.txt"
        old.write_text("x")
        os.utime(old, (0, 0))
        with mock.patch.object(watchdog.subprocess, "Popen") as run:
            path = watchdog.capture_stack(42, "timeout", time.time())
        self.assertFalse(old.exists())
        self.assertTrue(path.endswith("-timeout.txt"))
        args = run.call_args.args[0]
        self.assertEqual(args[4:8], [str(watchdog.SAMPLE_DEADLINE),
                                     watchdog.SAMPLE, "42", "3"])
        self.assertEqual(args[-1], path)
        self.assertTrue(run.call_args.kwargs["start_new_session"])
        self.assertEqual(stacks.stat().st_mode & 0o777, 0o700)

    def test_a_sample_that_cannot_start_is_logged_not_raised(self):
        logged = []
        with mock.patch.object(watchdog.subprocess, "Popen",
                               side_effect=FileNotFoundError("sample")), \
             mock.patch.object(watchdog, "log", logged.append):
            self.assertIsNone(watchdog.capture_stack(42, "timeout", time.time()))
        self.assertTrue(logged[0].startswith("stack capture failed"))

    def test_samples_a_live_process(self):
        path = Path(watchdog.capture_stack(os.getpid(), "test", time.time()))
        deadline = time.monotonic() + 120
        while not (path.exists() and "Call graph" in path.read_text()):
            self.assertLess(time.monotonic(), deadline, "no report after 120 s")
            time.sleep(1)

    # Regression: samplers hung for up to 2 h under load, and while
    # attached they made every dlopen in Emacs wait.
    def test_a_hung_sample_is_stopped_at_the_deadline(self):
        hung = self.dir / "hung-sample"
        hung.write_text("#!/bin/sh\necho $$ > \"$4.pid\"\nexec sleep 300\n")
        hung.chmod(0o700)
        with mock.patch.object(watchdog, "SAMPLE", str(hung)), \
             mock.patch.object(watchdog, "SAMPLE_DEADLINE", 1):
            path = watchdog.capture_stack(42, "timeout", time.time())
        pid_file = Path(path + ".pid")
        deadline = time.monotonic() + 10
        while not pid_file.exists():
            self.assertLess(time.monotonic(), deadline, "sampler never started")
            time.sleep(0.1)
        pid = int(pid_file.read_text())
        while True:
            try:
                os.kill(pid, 0)
            except ProcessLookupError:
                break
            self.assertLess(time.monotonic(), deadline, "sampler not stopped")
            time.sleep(0.1)


class SystemReadingTest(unittest.TestCase):
    def test_reads_this_machine(self):
        processes = watchdog.memory_by_process()
        self.assertTrue(processes)
        self.assertTrue(all("/" not in name for *_, name in processes))
        self.assertGreaterEqual(watchdog.swap_used_mb(), 0)

    # Regression: Google Drive held 23 GB, nearly all swapped out, and never
    # appeared in a ranking by resident size.
    def test_reads_the_footprint_of_own_processes(self):
        own = watchdog.footprint_mb(os.getpid())
        self.assertGreater(own, 0)
        self.assertLess(own, 1024)
        self.assertIsNone(watchdog.footprint_mb(1))


if __name__ == "__main__":
    unittest.main()
