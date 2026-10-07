"""Contracts of the Emacs server watchdog, with probe and notifications faked."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import json
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
                              lambda: [(9, 23000, 100, "Google Drive"),
                                       (42, 5000, 3500, "Emacs")]),
            mock.patch.object(watchdog, "swap_used_mb", lambda: 36000),
        ]
        for patch in patches:
            patch.start()
            self.addCleanup(patch.stop)
        self.write_state(recursion_depth=1, debugger="Debugger entered")

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


TOP_OUTPUT = """Processes: 891 total, 12 running, 875 sleeping, 7114 threads
2026/10/07 10:05:09
Load Avg: 17.01, 21.84, 24.10

PID    MEM
27547  23G
50375  8719M+
55316  5066M-
1      6977K
2      512B
"""


class SystemReadingTest(unittest.TestCase):
    # Regression: Google Drive held 23 GB, nearly all swapped out, and never
    # appeared in a ranking by resident size.
    def test_parses_top_footprints(self):
        self.assertEqual(watchdog.parse_top_memory(TOP_OUTPUT),
                         {27547: 23552, 50375: 8719, 55316: 5066, 1: 7, 2: 0})

    def test_rejects_unexpected_top_lines(self):
        with self.assertRaises(ValueError):
            watchdog.parse_top_memory(TOP_OUTPUT + "3  lots\n")

    def test_reads_this_machine(self):
        processes = watchdog.memory_by_process()
        self.assertTrue(processes)
        self.assertEqual(processes, sorted(processes, key=lambda p: p[1], reverse=True))
        self.assertTrue(all("/" not in name for *_, name in processes))
        self.assertGreaterEqual(watchdog.swap_used_mb(), 0)


if __name__ == "__main__":
    unittest.main()
