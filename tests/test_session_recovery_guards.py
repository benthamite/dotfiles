"""Emacs IPC regressions, independent of workflow fixtures.

Manual-reconciliation receipts are covered with real transcripts in
test_request_review.py and test_orchestrate_review.py, which also keep the
Claude and Codex helper copies identical.
"""
import importlib.util
import os
from pathlib import Path
import subprocess
import tempfile
import time
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]


def load(relative):
    spec = importlib.util.spec_from_file_location("recovery_guard_test", ROOT / relative)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class SessionRecoveryGuards(unittest.TestCase):
    def test_blocked_client_is_actually_terminated_within_bound(self):
        module = load("lib/python/agent_session_lib.py")
        # Only this owned substitute is launched; the active Emacs is untouched.
        with tempfile.TemporaryDirectory(prefix="blocked-emacsclient-") as directory:
            client = Path(directory) / "emacsclient"
            client.write_text("#!/bin/sh\nexec /bin/sleep 60\n")
            client.chmod(0o700)
            with mock.patch.dict(os.environ, {"PATH": directory}):
                started = time.monotonic()
                with self.assertRaisesRegex(module.EmacsClientError, "outcome is unknown"):
                    module.run_emacs_eval("nil")
                elapsed = time.monotonic() - started
            self.assertGreaterEqual(elapsed, 14)
            self.assertLess(elapsed, 20)

    def test_emacs_timeout_is_bounded_and_reports_ambiguous_outcome(self):
        module = load("lib/python/agent_session_lib.py")
        with mock.patch.object(module.subprocess, "run", side_effect=
                               subprocess.TimeoutExpired("emacsclient", 15)) as run:
            with self.assertRaisesRegex(module.EmacsClientError, "outcome is unknown"):
                module.run_emacs_eval("nil")
        self.assertEqual(run.call_args.kwargs["timeout"], 15)

    def test_successful_emacs_reply_is_preserved(self):
        module = load("lib/python/agent_session_lib.py")
        reply = subprocess.CompletedProcess([], 0, b'"ready"\n', b"")
        with mock.patch.object(module.subprocess, "run", return_value=reply):
            self.assertEqual(module.run_emacs_eval("nil"), "ready")


if __name__ == "__main__":
    unittest.main()
