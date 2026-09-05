"""IPC and manual reconciliation regressions, independent of workflow fixtures."""
import contextlib
import importlib.util
import io
import os
from pathlib import Path
import subprocess
import tempfile
import time
from types import SimpleNamespace
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

    def test_manual_delivered_requires_receipt_in_both_helpers_and_mirrors(self):
        for side in ("claude", "codex"):
            for skill in ("request-review", "orchestrate-review"):
                for delivered in (False, True):
                    with self.subTest(side=side, skill=skill, receipt=delivered):
                        module = load(f"{side}/skills/{skill}/scripts/"
                                      + skill.replace("-", "_") + ".py")
                        pending = {"actor": "agent1", "phase": "spec", "transcript_offset": 42}
                        state = {"pending_submission": pending,
                                 "reviewer": {"transcript": "/review.jsonl"},
                                 "agent1": {"transcript": "/author.jsonl"},
                                 "plan": {"commit": "a" * 40}, "stage": "one"}
                        request = skill == "request-review"
                        args = SimpleNamespace(run_file=Path("unused"), delivered=True)
                        with contextlib.ExitStack() as stack:
                            stack.enter_context(mock.patch.object(module, "review_lock" if request else "run_lock",
                                                                 return_value=contextlib.nullcontext()))
                            stack.enter_context(mock.patch.object(module, "load_review" if request else "load_run",
                                                                 return_value=state))
                            save = stack.enter_context(mock.patch.object(module, "save_review" if request else "save_run"))
                            receipt = stack.enter_context(mock.patch.object(module.session, "_marker_delivered",
                                                                            return_value=delivered))
                            if not request:
                                stack.enter_context(mock.patch.object(module, "_finalize_pending"))
                            stack.enter_context(contextlib.redirect_stdout(io.StringIO()))
                            if delivered:
                                module.reconcile_submission(args)
                                save.assert_called_once()
                            else:
                                with self.assertRaisesRegex(SystemExit, "receipt is missing"):
                                    module.reconcile_submission(args)
                                save.assert_not_called()
                                self.assertIs(state["pending_submission"], pending)
                            self.assertEqual(receipt.call_args.args[1], 42)


if __name__ == "__main__":
    unittest.main()
