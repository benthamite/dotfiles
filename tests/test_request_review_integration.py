"""Request-review lifecycle through real batch Emacs evaluation, with fake backends.

Only the emacsclient transport is replaced. Each evaluation uses a fresh -Q
batch Emacs, owned HOME and files, and synthetic agent buffers/functions. The
real helper, emitted identity guard, prompt transfer, JSONL evidence readers,
and persisted run lifecycle remain in use. No personal server, model, package
startup, credential store, or network is contacted; this does not establish a
real reviewer round trip or live package activation.
"""

import hashlib
import importlib.util
import io
import json
import os
import subprocess
import tempfile
import unittest
from contextlib import contextmanager, redirect_stdout
from pathlib import Path
from types import SimpleNamespace
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
HELPER = ROOT / "codex/skills/request-review/scripts/request_review.py"
PAIRED_HELPER = ROOT / "claude/skills/request-review/scripts/request_review.py"
SHARED = ROOT / "lib/python/agent_session_lib.py"
EMACS = Path(os.environ.get("EMACS_TEST_BINARY", "/opt/homebrew/bin/emacs"))


def load_helper():
    spec = importlib.util.spec_from_file_location("request_review_native", HELPER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class NativeFixture:
    def __init__(self, root, backend):
        self.root = root.resolve()
        self.backend = backend
        self.repo = self.root / "repo"
        self.repo.mkdir()
        self.home = self.root / "home"
        self.home.mkdir()
        self.env = {"HOME": str(self.home), "TMPDIR": str(self.root),
                    "PATH": "/usr/bin:/bin", "LANG": "en_US.UTF-8",
                    "GIT_CONFIG_NOSYSTEM": "1"}
        self.transcript = self.root / "review.jsonl"
        self.transcript.write_bytes(b"")
        self.run_file = self.root / "run.json"
        self.dispatch_log = self.root / "dispatch.jsonl"
        self.lifecycle_event = self.root / "lifecycle-event.txt"
        self.buffer = "*request-review-owned-fixture*"
        self.session_id = "00000000-0000-4000-8000-000000000049"
        self.review = load_helper()
        self.receipt = True
        self.initial_state = "waiting"
        self.dispatch_override = {}
        self.bootstrap_override = {}
        self.git("init", "--quiet")
        (self.repo / "plan.md").write_text("# Owned synthetic plan\n", encoding="utf-8")
        self.git("add", "plan.md")
        self.git("-c", "user.name=Fixture", "-c", "user.email=fixture@example.invalid",
                 "commit", "--quiet", "-m", "fixture")
        self.commit = self.git("rev-parse", "HEAD").strip()

    def git(self, *args):
        return subprocess.run(["/usr/bin/git", "-C", str(self.repo), *args],
                              env=self.env, capture_output=True, text=True,
                              timeout=15, check=True).stdout

    def prelude(self, dispatch, bootstrap):
        identity = {"backend": self.backend, "session_id": self.session_id,
                    "directory": str(self.repo), "transcript": str(self.transcript)
                    if self.transcript.exists() else None, "state": "waiting"
                    if self.lifecycle_event.exists() else self.initial_state}
        if dispatch:
            identity.update(self.dispatch_override)
        if bootstrap:
            identity.update(self.bootstrap_override)
        q = self.review.session.elisp_string
        path = "nil" if identity["transcript"] is None else q(identity["transcript"])
        return f'''
          ;; This is an interpreted guard fixture, not a compiler acceptance
          ;; test. Defining blocked process subrs must not launch a compiler.
          (setq native-comp-enable-subr-trampolines nil
                native-comp-jit-compilation nil)
          (require 'cl-lib)
          (require 'json)
          (defvar agent-claude-submit-retries 3)
          (defvar fixture-backend '{identity["backend"]})
          (defvar fixture-session-id {q(identity["session_id"])})
          (defvar fixture-transcript {path})
          (defvar fixture-state '{identity["state"]})
          (defun make-network-process (&rest _) (error "fixture network forbidden"))
          (defun open-network-stream (&rest _) (error "fixture network forbidden"))
          (defun start-process (&rest _) (error "fixture subprocess forbidden"))
          (defun make-process (&rest _) (error "fixture subprocess forbidden"))
          (defun get-buffer-process (&rest _) 'fixture-owned-process)
          (defun process-live-p (process) (eq process 'fixture-owned-process))
          (defun agent--detect-backend (&rest _) fixture-backend)
          (defun agent-session-display-state (&rest _) fixture-state)
          (defun agent-session-event (_buffer event)
            (unless (eq event 'blocked) (error "unexpected synthetic lifecycle event"))
            (setq fixture-state 'waiting)
            (with-temp-file {q(str(self.lifecycle_event))} (insert "blocked")))
          (defun codex-session-identity (&rest _)
            (list :session-id fixture-session-id))
          (defun codex--find-session-transcript (&rest _) fixture-transcript)
          (defun agent-claude--parse-status-file ()
            (list :session_id fixture-session-id :transcript_path fixture-transcript))
          (defun fixture-write (path record)
            (let ((coding-system-for-write 'utf-8-unix))
              (with-temp-buffer
                (insert (json-encode record) "\\n")
                (write-region (point-min) (point-max) path t 'silent))))
          (defun agent-send-return (target)
            (fixture-write {q(str(self.dispatch_log))} '((kind . "return")))
            target)
          (defun agent-submit (prompt target)
            (fixture-write {q(str(self.dispatch_log))}
              `((kind . "submit") (prompt . ,prompt)
                (retries . ,agent-claude-submit-retries)))
            (when {"t" if self.receipt else "nil"}
              (fixture-write {q(str(self.transcript))}
                (if (eq fixture-backend 'codex)
                    `((type . "response_item")
                      (payload . ((type . "message") (role . "user")
                                  (content . [((type . "input_text") (text . ,prompt))]))))
                  `((type . "user") (message . ((role . "user") (content . ,prompt)))))))
            target)
          (with-current-buffer (get-buffer-create {q(self.buffer)})
            (setq default-directory {q(identity["directory"] + "/")}))
        '''

    def native_rpc(self, expression):
        # A race override is applied only after Python preflight, in the exact
        # native evaluation which will dispatch if its guard accepts.
        dispatch = "(agent-submit\n" in expression
        bootstrap = "(agent-session-event " in expression
        program = f'''(progn {self.prelude(dispatch, bootstrap)}
          (condition-case fixture-error
              (let ((value {expression})) (when (null value) (princ "nil")))
            (error (princ (error-message-string fixture-error)) (kill-emacs 27))))'''
        result = subprocess.run([str(EMACS), "-Q", "--batch", "--eval", program],
                                cwd=self.root, env=self.env, text=True,
                                capture_output=True, timeout=15)
        if result.returncode:
            raise self.review.EmacsClientError(
                "owned batch Emacs refused evaluation: " + result.stdout + result.stderr)
        return result.stdout.strip()

    def call(self, operation, **kwargs):
        output = io.StringIO()
        with redirect_stdout(output):
            getattr(self.review, operation)(SimpleNamespace(run_file=str(self.run_file), **kwargs))
        return output.getvalue()

    def init(self):
        return self.call("init_review", repo=str(self.repo), plan_path="plan.md",
                         plan_commit=self.commit, caller_backend="claude-code"
                         if self.backend == "codex" else "codex",
                         reviewer_buffer=self.buffer, reviewer_backend=self.backend,
                         reviewer_transcript=str(self.transcript)
                         if self.transcript.exists() else None)

    def submit(self):
        return self.call("submit_review", context_file=None)

    def state(self):
        value = self.review.load_review(self.run_file)
        if value != json.loads(self.run_file.read_text(encoding="utf-8")):
            raise AssertionError("loaded state differs from persisted bytes")
        return value

    def dispatches(self):
        if not self.dispatch_log.exists():
            return []
        return [json.loads(line) for line in self.dispatch_log.read_text().splitlines()]

    def append(self, *records):
        with self.transcript.open("a", encoding="utf-8") as stream:
            for record in records:
                stream.write(json.dumps(record, ensure_ascii=False) + "\n")

    def user(self, text):
        return {"type": "response_item", "payload": {"type": "message", "role": "user",
                "content": [{"type": "input_text", "text": text}]}}

    def returned(self, text, *, commentary=False):
        if self.backend == "codex":
            return {"type": "response_item", "payload": {"type": "message",
                    "role": "assistant", "channel": "commentary" if commentary else "final",
                    "content": [{"type": "output_text", "text": text}]}}
        return {"type": "assistant", "message": {"role": "assistant", "stop_reason": "end_turn",
                "content": [{"type": "text", "text": text}]}}

    def marker(self):
        # The exact contract is the final line actually dispatched, not a
        # separately reconstructed marker that could hide a prompt mismatch.
        return self.dispatches()[0]["prompt"].splitlines()[-1]


@unittest.skipUnless(EMACS.is_file(), "standalone Emacs test binary unavailable")
class RequestReviewNativeIntegrationTests(unittest.TestCase):
    @contextmanager
    def fixture(self, backend="codex"):
        before = {path: hashlib.sha256(path.read_bytes()).hexdigest()
                  for path in (HELPER, PAIRED_HELPER, SHARED)}
        with tempfile.TemporaryDirectory(prefix="request-review-native-", dir="/tmp") as directory:
            fixture = NativeFixture(Path(directory), backend)
            with mock.patch.dict(os.environ, fixture.env, clear=True), \
                 mock.patch.object(tempfile, "tempdir", str(fixture.root)), \
                 mock.patch.object(fixture.review.session, "run_emacs_eval", side_effect=fixture.native_rpc), \
                 mock.patch.object(fixture.review.session, "DELIVERY_INITIAL_WAIT_SECONDS", 0):
                yield fixture
        self.assertFalse(Path(directory).exists())
        self.assertEqual(before, {path: hashlib.sha256(path.read_bytes()).hexdigest()
                                  for path in before}, "source changed during native acceptance")

    def test_both_backends_complete_exact_review_and_persist_readback(self):
        for backend in ("codex", "claude-code"):
            with self.subTest(backend=backend), self.fixture(backend) as fixture:
                if backend == "claude-code":
                    fixture.initial_state = "unknown"
                fixture.init()
                fixture.submit()
                fixture.append(fixture.returned("Owned finding: café.\n" + fixture.marker()))
                output = fixture.call("finish_review")
                self.assertIn("REVIEW OUTCOME: complete", output)
                self.assertEqual(fixture.state()["status"], "review-returned")
                self.assertEqual(fixture.run_file.stat().st_mode & 0o777, 0o600)
                self.assertEqual([(row["kind"], row["retries"]) for row in fixture.dispatches()],
                                 [("submit", 0)])
                if backend == "claude-code":
                    self.assertEqual(fixture.lifecycle_event.read_text(), "blocked")

    def test_fresh_codex_adopts_only_its_receipt_created_transcript(self):
        with self.fixture() as fixture:
            fixture.transcript.unlink()
            fixture.init()
            fixture.submit()
            self.assertEqual(fixture.state()["reviewer"]["transcript"], str(fixture.transcript))
            self.assertEqual(fixture.state()["status"], "review-active")
            self.assertEqual(len(fixture.dispatches()), 1)

    def test_same_evaluation_identity_and_lifecycle_races_send_nothing(self):
        for override in ({"session_id": "different-session"}, {"backend": "claude-code"},
                         {"state": "busy"}, {"directory": "/tmp"},
                         {"transcript": "/tmp/owned-never-read-transcript.jsonl"}):
            with self.subTest(override=override), self.fixture() as fixture:
                fixture.init()
                fixture.dispatch_override = override
                with self.assertRaises(fixture.review.EmacsClientError):
                    fixture.submit()
                self.assertEqual(fixture.dispatches(), [])
                self.assertIsNotNone(fixture.state()["pending_submission"])
        with self.fixture("claude-code") as fixture:
            fixture.initial_state = "unknown"
            fixture.bootstrap_override = {"session_id": "reused-at-bootstrap"}
            with self.assertRaises(fixture.review.EmacsClientError):
                fixture.init()
            self.assertEqual(fixture.dispatches(), [])
            self.assertFalse(fixture.lifecycle_event.exists())
            self.assertFalse(fixture.run_file.exists())

    def test_successful_dispatch_without_receipt_stays_pending_without_return(self):
        with self.fixture() as fixture:
            fixture.receipt = False
            fixture.init()
            with self.assertRaises(fixture.review.EmacsClientError):
                fixture.submit()
            pending = fixture.run_file.read_bytes()
            with self.assertRaises((SystemExit, fixture.review.EmacsClientError)):
                fixture.call("reconcile_submission", delivered=False)
            self.assertEqual(fixture.run_file.read_bytes(), pending)
            self.assertIsNotNone(fixture.state()["pending_submission"])
            self.assertEqual([row["kind"] for row in fixture.dispatches()], ["submit"])

    def test_commentary_is_not_final_and_markerless_final_is_incomplete(self):
        with self.fixture() as fixture:
            fixture.init()
            fixture.submit()
            fixture.append(fixture.returned("Interim.\n" + fixture.marker(), commentary=True))
            with self.assertRaises((SystemExit, fixture.review.EmacsClientError)):
                fixture.call("finish_review")
            self.assertEqual(fixture.state()["status"], "review-active")
            fixture.append(fixture.returned("Terminal findings without the completion marker."))
            fixture.call("finish_review")
            self.assertEqual(fixture.state()["status"], "review-incomplete")
            self.assertEqual(len(fixture.dispatches()), 1)

    def test_later_unrelated_turn_cannot_certify_original_review(self):
        with self.fixture() as fixture:
            fixture.init()
            fixture.submit()
            marker = fixture.marker()
            fixture.append(fixture.returned("Original answer.\n" + marker),
                           fixture.user("An unrelated later prompt."),
                           fixture.returned("Unrelated answer.\n" + marker))
            with self.assertRaises((SystemExit, fixture.review.EmacsClientError)):
                fixture.call("finish_review")
            self.assertEqual(fixture.state()["status"], "review-active")
            self.assertEqual(len(fixture.dispatches()), 1)


if __name__ == "__main__":
    unittest.main()
