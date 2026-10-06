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


if __name__ == "__main__":
    unittest.main()
