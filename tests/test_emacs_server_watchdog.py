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


class DiagnoseTest(unittest.TestCase):
    def state(self, **fields):
        return {"time": 1000.0, "pid": 42, "recursion_depth": 0,
                "minibuffer_depth": 0, "prompt": None, "debugger": None, **fields}

    def test_names_the_debugger_frame(self):
        cause = watchdog.diagnose(self.state(
            recursion_depth=3,
            debugger="Debugger entered--entering a function:\n"
                     "* clear-minibuffer-message()\n  shell-command()"), None, 1010.0)
        self.assertIn("debugger", cause)
        self.assertIn("clear-minibuffer-message", cause)

    def test_names_the_prompt(self):
        cause = watchdog.diagnose(self.state(minibuffer_depth=1,
                                             prompt="File changed on disk; reread? "),
                                  None, 1010.0)
        self.assertIn("File changed on disk", cause)

    def test_stale_state_means_emacs_is_blocked(self):
        cause = watchdog.diagnose(self.state(), None, 1100.0)
        self.assertIn("blocked for 100 s", cause)

    def test_stale_state_wins_over_an_old_debugger_entry(self):
        cause = watchdog.diagnose(self.state(debugger="Debugger entered"), None, 1100.0)
        self.assertIn("blocked", cause)

    def test_another_emacs_on_the_socket(self):
        cause = watchdog.diagnose(self.state(), 7, 1010.0)
        self.assertIn("pid 7", cause)
        self.assertIn("pid 42", cause)


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
            mock.patch.object(watchdog, "memory_by_process",
                              lambda: [(42, 3500, "Emacs"), (7, 1600, "bztransmit")]),
            mock.patch.object(watchdog, "swap_used_mb", lambda: 36000),
        ]
        for patch in patches:
            patch.start()
            self.addCleanup(patch.stop)
        (self.dir / "state.json").write_text(json.dumps(
            {"time": time.time(), "pid": 42, "recursion_depth": 1,
             "minibuffer_depth": 0, "prompt": None, "debugger": "Debugger entered"}))

    def run_with_answer(self, answer):
        with mock.patch.object(watchdog, "probe", lambda socket: answer):
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
        self.assertEqual(first["emacs_rss_mb"], 3500)
        self.assertEqual(first["swap_used_mb"], 36000)
        self.assertEqual(len(first["load"]), 3)
        self.assertEqual(first["top_rss_mb"], [["Emacs", 3500], ["bztransmit", 1600]])

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


class SystemReadingTest(unittest.TestCase):
    def test_reads_this_machine(self):
        processes = watchdog.memory_by_process()
        self.assertTrue(processes)
        self.assertEqual(processes, sorted(processes, key=lambda p: p[1], reverse=True))
        self.assertTrue(all("/" not in name for _, _, name in processes))
        self.assertGreaterEqual(watchdog.swap_used_mb(), 0)


if __name__ == "__main__":
    unittest.main()
