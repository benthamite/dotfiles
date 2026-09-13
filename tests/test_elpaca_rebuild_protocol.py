"""Check the rebuild helper's generated protocol with native batch Emacs.

Only synthetic registry/status functions run; no live server is contacted and
no real package is loaded, rebuilt or reconfigured.
"""

import base64
import importlib.machinery
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch


HELPER = Path(__file__).resolve().parents[1] / "claude/bin/elpaca-rebuild-wait"
EMACS = shutil.which("emacs")


@unittest.skipUnless(EMACS, "Native batch Emacs is required")
class ElpacaRebuildProtocolTests(unittest.TestCase):
    def setUp(self):
        loader = importlib.machinery.SourceFileLoader("rebuild_protocol_fixture", str(HELPER))
        spec = importlib.util.spec_from_loader(loader.name, loader)
        self.helper = importlib.util.module_from_spec(spec)
        loader.exec_module(self.helper)
        self.temporary = tempfile.TemporaryDirectory(prefix="elpaca-protocol-", dir="/tmp")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name).resolve()
        self.source = self.root / 'source with ñ and "quotes"'
        self.profile = self.root / "profile with spaces"
        self.source.mkdir()
        self.profile.mkdir()
        self.status_package = "fixture-package"
        self.status_state = "finished"
        self.queued = False
        self.calls = 0
        self.outputs = []
        self.helper.command = self.native_command

    def native_command(self, arguments, timeout=10):
        self.assertEqual(arguments[:2], ["emacsclient", "-e"])
        self.calls += 1
        prelude = f"""(progn
          (setq user-emacs-directory {json.dumps(str(self.profile))}
                before-init-time '(1 2 3 4))
          (defun emacs-pid () 123456)
          (defun elpaca-get (package)
            (and (eq package 'fixture-package) 'fixture-entry))
          (defun elpaca<-source-dir (_entry) {json.dumps(str(self.source))})
          (defun elpaca-source-dir (_entry) (error "Unexpected alternate source"))
          (defun elpaca--status (_entry) 'finished)
          (setq elpaca-extras--build-reload-statuses (make-hash-table :test 'equal))
          (puthash "fixture-token" (list :package 'fixture-package
            :state '{'queued' if self.queued else 'finished'})
            elpaca-extras--build-reload-statuses)
          (defun elpaca-extras-rebuild-and-reload (package)
            (unless (eq package 'fixture-package) (error "Unexpected package"))
            "fixture-package-token-1")
          (defun elpaca-extras-build-reload-status (_token)
            (list :package (intern {json.dumps(self.status_package)})
                  :state (intern {json.dumps(self.status_state)})
                  :message "fixture-private-diagnostic")))"""
        result = subprocess.run(
            [EMACS, "--batch", "-Q", "--eval", prelude,
             "--eval", f"(prin1 {arguments[-1]})"],
            text=True, capture_output=True, check=False, timeout=timeout,
        )
        if result.returncode:
            raise self.helper.RebuildError("synthetic native protocol refused")
        self.outputs.append(result.stdout)
        return result.stdout.strip()

    def identity(self):
        return self.helper.validate_runtime(
            self.helper.emacs_json(self.helper.runtime_form("fixture-package")),
            "fixture-package",
        )

    def test_native_runtime_identity_round_trips_paths_and_types(self):
        self.assertEqual(self.identity(), {
            "package": "fixture-package", "pid": 123456, "start": "(1 2 3 4)",
            "profile": str(self.profile), "source": str(self.source),
        })

    def test_native_request_compares_identity_before_returning_token(self):
        token = self.helper.request("fixture-package", self.identity())
        self.assertEqual(token, "fixture-package-token-1")

    def test_native_request_refuses_changed_runtime(self):
        identity = self.identity()
        identity["pid"] += 1
        with self.assertRaises(self.helper.RebuildError):
            self.helper.request("fixture-package", identity)

    def test_native_token_status_binds_package_without_private_message(self):
        identity = self.identity()
        for state in ("queued", "finished", "failed"):
            self.status_state = state
            self.assertEqual(self.helper.token_status(
                "fixture-package", "fixture-package-token-1", identity), state)
        decoded = [base64.b64decode(json.loads(output)).decode("utf-8")
                   for output in self.outputs]
        self.assertTrue(all("fixture-private-diagnostic" not in output for output in decoded))

    def test_native_token_from_other_package_is_refused(self):
        identity = self.identity()
        self.status_package = "other-package"
        with self.assertRaises(self.helper.RebuildError):
            self.helper.token_status("fixture-package", "fixture-package-token-1", identity)

    def test_invalid_token_never_reaches_emacs(self):
        identity = self.identity()
        calls = self.calls
        with self.assertRaises(self.helper.RebuildError):
            self.helper.token_status("fixture-package", '\") (error "injected")', identity)
        self.assertEqual(self.calls, calls)

    def lost_request(self):
        initial = {"runtime": self.identity(), "source": "fixture", "active": "fixture"}
        owner = self.root / "fixture-package.json"
        status = self.root / "fixture-package.status"
        with patch.object(self.helper, "request", side_effect=self.helper.RebuildError("lost reply")):
            with self.assertRaises(self.helper.RebuildError):
                self.helper.start_operation("fixture-package", initial, owner, status)
        operation = self.helper.read_operation(owner)
        self.assertEqual(operation["state"], "requesting")
        self.assertNotIn("token", operation)
        return initial, owner, status, operation

    def reconcile(self, initial, owner, status, operation):
        with patch.object(self.helper, "context", return_value=initial):
            self.helper.reconcile_tokenless_request(
                "fixture-package", self.source, False, initial, operation, owner, status)

    def test_lost_reply_idle_package_is_archived_without_certifying_completion(self):
        initial, owner, status, operation = self.lost_request()
        self.reconcile(initial, owner, status, operation)
        archives = list(self.root.glob("fixture-package.abandoned.*.json"))
        self.assertEqual(len(archives), 1)
        self.assertEqual(json.loads(archives[0].read_text())["state"], "abandoned")
        self.assertEqual(self.helper.read_operation(owner), operation)
        self.assertEqual(self.helper.read_status(status), "pending")
        self.assertFalse(list(self.root.glob("*.receipt.json")))

    def test_lost_reply_still_queued_is_not_abandoned(self):
        initial, owner, status, operation = self.lost_request()
        self.queued = True
        with self.assertRaisesRegex(self.helper.RebuildError, "not confirmed idle"):
            self.reconcile(initial, owner, status, operation)
        self.assertFalse(list(self.root.glob("*.abandoned.*.json")))

    def test_lost_reply_changed_runtime_is_not_abandoned(self):
        initial, owner, status, operation = self.lost_request()
        initial["runtime"]["pid"] += 1
        with self.assertRaises(self.helper.RebuildError):
            self.reconcile(initial, owner, status, operation)
        self.assertFalse(list(self.root.glob("*.abandoned.*.json")))

    def test_replaced_status_is_not_abandoned(self):
        initial, owner, status, operation = self.lost_request()
        self.helper.atomic_write(status, "pending:another producer\n")
        with self.assertRaises(self.helper.RebuildError):
            self.reconcile(initial, owner, status, operation)

    def test_returned_token_uses_normal_resume_not_reconciliation(self):
        initial, owner, status, operation = self.lost_request()
        operation.update(state="pending", token="fixture-package-token-1")
        with self.assertRaises(self.helper.RebuildError):
            self.reconcile(initial, owner, status, operation)


if __name__ == "__main__":
    unittest.main()
