"""Offline CLI/state integration through native Emacs with synthetic actors only.

Only run_emacs_eval is redirected.  No emacsclient, live package, user session,
or agent service is accessed; every actor and transcript belongs to /tmp.
"""

import ast
import hashlib
import importlib.util
import io
import json
import os
import shutil
import subprocess
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "codex/skills/orchestrate-review/scripts/orchestrate_review.py"
EMACS = os.environ.get("EMACS_TEST_BINARY") or shutil.which("emacs")

# This is deliberately a backend boundary fixture, not an agent.el emulator.
# The real Python code supplies and evaluates all identity/dispatch forms.
FAKE_BACKEND = r'''
(require 'json)
(require 'subr-x)
(defvar fixture-registry-file (getenv "ORCHESTRATE_TEST_REGISTRY"))
(defvar fixture-dispatch-file (getenv "ORCHESTRATE_TEST_DISPATCH"))
(defvar fixture-actors
  (let ((json-array-type 'list)) (json-read-file fixture-registry-file)))
(defvar-local fixture-actor nil)
(defvar agent-claude-submit-retries 3)
(dolist (actor fixture-actors)
  (with-current-buffer (get-buffer-create (alist-get 'buffer actor))
    (setq fixture-actor actor
          default-directory (file-name-as-directory (alist-get 'directory actor)))
    (setq-local codex--session-transcript-file (alist-get 'transcript actor))))
(defun fixture-save ()
  "Persist only the disposable actor registry."
  (with-temp-file fixture-registry-file (insert (json-encode fixture-actors))))
(defun fixture-append (path record)
  "Append synthetic RECORD to the fixture PATH."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (concat (json-encode record) "\n") nil path t 'silent)))
(defun agent--detect-backend (&optional buffer)
  "Return the synthetic backend in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (intern (alist-get 'backend fixture-actor))))
(defun agent-session-display-state (&optional buffer)
  "Return the synthetic lifecycle state in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (intern (alist-get 'state fixture-actor))))
(defun agent-claude--parse-status-file ()
  "Return the selected fake Claude identity."
  (list :session_id (alist-get 'session_id fixture-actor)
        :transcript_path (alist-get 'transcript fixture-actor)))
(defun codex-session-identity (&optional buffer)
  "Return the fake Codex identity for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (list :session-id (alist-get 'session_id fixture-actor))))
(defun codex-prompt-input (&optional buffer)
  "Return the disposable composer text in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (or (alist-get 'composer fixture-actor) "")))
(defun agent-send-return (_buffer)
  "Refuse an unexpected Return retry in this no-retransmission fixture."
  (error "Unexpected Return retry"))
(defun agent-submit (prompt target)
  "Record PROMPT for synthetic TARGET, optionally simulating lost transport."
  (with-current-buffer target
    (fixture-append fixture-dispatch-file
                    `((buffer . ,(buffer-name)) (prompt . ,prompt)
                      (session_id . ,(alist-get 'session_id fixture-actor))
                      (backend_retries . ,agent-claude-submit-retries)))
    (when (equal (alist-get 'transport fixture-actor) "error-before-receipt")
      (error "Fixture transport failed before receipt"))
    (fixture-append
     (alist-get 'transcript fixture-actor)
     (if (eq (agent--detect-backend) 'codex)
         `((type . "response_item")
           (payload . ((type . "message") (role . "user")
                       (content . [((type . "input_text") (text . ,prompt))]))))
       `((type . "user") (message . ((role . "user") (content . ,prompt))))))
    (setf (alist-get 'state fixture-actor) "busy")
    (fixture-save)
    (when (equal (alist-get 'transport fixture-actor) "error-after-receipt")
      (error "Fixture transport failed after receipt"))
    target))
'''


@unittest.skipUnless(EMACS, "standalone Emacs is unavailable")
class OrchestrateReviewIntegrationTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="orchestrate-integration-", dir="/tmp")
        self.addCleanup(self.temporary.cleanup)
        self.directory = Path(self.temporary.name).resolve()
        self.repo = self.directory / 'repository with spaces and "quotes"'
        self.repo.mkdir()
        self.registry = self.directory / "actors.json"
        self.dispatch = self.directory / "dispatch.jsonl"
        self.backend = self.directory / "fake-backend.el"
        self.backend.write_text(FAKE_BACKEND, encoding="utf-8")
        self.run = self.directory / "run.json"
        self.context = self.directory / "phase.txt"
        self.context.write_text('Review the complete stage, including "quoted" paths.\n', encoding="utf-8")
        self.context.chmod(0o600)
        self.actors = []
        self.before_dispatch = None
        spec = importlib.util.spec_from_file_location("orchestrate_integration", SCRIPT)
        self.module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.module)
        patcher = mock.patch.object(self.module.session, "run_emacs_eval", side_effect=self.native_rpc)
        patcher.start()
        self.addCleanup(patcher.stop)

    def native_rpc(self, expression):
        if self.before_dispatch and "(agent-submit" in expression:
            callback, self.before_dispatch = self.before_dispatch, None
            callback()
        # Emulate only emacsclient's printed result.  The production forms
        # themselves still write/read their real one-shot private JSON files.
        wrapped = f'''(condition-case err
          (let ((result (let ((standard-output (get-buffer-create " *fixture-rpc-output*")))
                          {expression})))
            (fixture-save)
            (prin1 result))
          (error (fixture-save)
                 (princ (error-message-string err) 'external-debugging-output)
                 (kill-emacs 3)))'''
        env = dict(os.environ, ORCHESTRATE_TEST_REGISTRY=str(self.registry),
                   ORCHESTRATE_TEST_DISPATCH=str(self.dispatch))
        result = subprocess.run([EMACS, "-Q", "--batch", "--load", str(self.backend),
                                 "--eval", wrapped], cwd=self.directory, env=env,
                                text=True, capture_output=True, timeout=15, check=False)
        if result.returncode:
            raise self.module.EmacsClientError("offline fixture RPC refused: " + result.stderr.strip())
        value = result.stdout.strip()
        return ast.literal_eval(value) if value.startswith('"') else value

    def cli(self, command, *arguments, error=None):
        output, errors = io.StringIO(), io.StringIO()
        with redirect_stdout(output), redirect_stderr(errors):
            if error is None:
                self.assertEqual(self.module.main([command, "--run-file", str(self.run), *arguments]), 0)
            else:
                with self.assertRaises(SystemExit) as caught:
                    self.module.main([command, "--run-file", str(self.run), *arguments])
                self.assertIn(error, str(caught.exception) + errors.getvalue())
        return output.getvalue()

    def save_actors(self):
        self.registry.write_text(json.dumps(self.actors), encoding="utf-8")

    def update_actor(self, number, **changes):
        self.actors = json.loads(self.registry.read_text())
        self.actors[number - 1].update(changes)
        self.save_actors()

    def init_run(self, reverse=False, adopt=False):
        backends = ("codex", "claude-code") if reverse else ("claude-code", "codex")
        for number, backend in enumerate(backends, 1):
            transcript = self.directory / f"{self.run.stem}-actor-{number}.jsonl"
            transcript.touch()
            actor = {"buffer": f"*fixture:{number}*", "backend": backend,
                     "directory": str(self.repo), "session_id": f"fixture-session-{number}",
                     "transcript": str(transcript), "state": "waiting"}
            self.actors.append(actor)
            if backend == "codex":
                self.append_record(number, {"type": "session_meta", "payload": {
                    "id": actor["session_id"], "cwd": str(self.repo)}})
        self.save_actors()
        arguments = ["--repo", str(self.repo), "--stage", "36"]
        for number, actor in enumerate(self.actors, 1):
            for field in ("buffer", "backend", "transcript"):
                arguments.extend([f"--agent{number}-{field}", actor[field]])
        if adopt:
            arguments.extend(["--adopt-implementation", "--spec-commit", "a" * 40,
                              "--plan-commit", "b" * 40, "--reviews-complete"])
        self.cli("init-run", *arguments)
        self.assertEqual(self.run.stat().st_mode & 0o777, 0o600)

    def state(self):
        return json.loads(self.run.read_text())

    def dispatches(self):
        return [json.loads(line) for line in self.dispatch.read_text().splitlines()] if self.dispatch.exists() else []

    def append_record(self, number, record):
        with Path(self.actors[number - 1]["transcript"]).open("a", encoding="utf-8") as stream:
            stream.write(json.dumps(record) + "\n")

    def return_text(self, phase, text=None, *, waiting=True):
        number = 2 if phase.endswith("-review") else 1
        actor = self.actors[number - 1]
        marker = "STAGE COMPLETE: 36" if phase == "implementation" else f"PHASE COMPLETE: {phase}"
        text = text or f"Verified synthetic {phase} outcome.\n{marker}"
        if actor["backend"] == "codex":
            record = {"type": "response_item", "payload": {
                "type": "message", "role": "assistant", "phase": "final_answer", "channel": "final",
                "content": [{"type": "output_text", "text": text}]}}
        else:
            record = {"type": "assistant", "message": {
                "role": "assistant", "stop_reason": "end_turn", "content": [{"type": "text", "text": text}]}}
        self.append_record(number, record)
        self.update_actor(number, state="waiting" if waiting else "busy")
        return text

    def submit(self, phase, **options):
        return self.cli("submit", "--phase", phase, "--prompt-file", str(self.context), **options)

    def finish(self, phase, **options):
        return self.cli("finish-phase", "--phase", phase, **options)

    def complete(self, returned):
        evidence = self.directory / "verification.txt"
        evidence.write_text(returned, encoding="utf-8")
        evidence.chmod(0o600)
        self.cli("complete-stage", "--evidence-file", str(evidence))
        self.assertEqual(self.state()["status"], "complete")
        self.assertEqual(self.state()["acceptance_evidence"]["sha256"], hashlib.sha256(evidence.read_bytes()).hexdigest())

    def test_full_five_phase_flow_and_whole_role_reversal(self):
        for reverse in (False, True):
            with self.subTest(reverse=reverse):
                # A second owned run uses separate state and actor files.
                if reverse:
                    self.run = self.directory / "reversed-run.json"
                    self.dispatch = self.directory / "reversed-dispatch.jsonl"
                    self.actors = []
                self.init_run(reverse=reverse)
                for phase in self.module.PHASES:
                    self.submit(phase)
                    self.assertEqual(self.state()["active_phase"], phase)
                    self.assertIsNone(self.state()["pending_submission"])
                    returned = self.return_text(phase)
                    output = self.finish(phase)
                    if phase == "implementation":
                        self.assertIn(returned, output)
                self.complete(returned)
                deliveries = self.dispatches()
                self.assertEqual([item["buffer"] for item in deliveries],
                                 ["*fixture:1*", "*fixture:2*", "*fixture:1*", "*fixture:2*", "*fixture:1*"])
                self.assertEqual(len({item["prompt"].splitlines()[0] for item in deliveries}), 5)
                self.assertTrue(all(item["backend_retries"] == 0 for item in deliveries))
                self.assertEqual(len(self.state()["completions"]), 5)
                self.assertNotIn("Verified synthetic", self.run.read_text())

    def test_same_rpc_actor_replacement_refuses_before_backend_dispatch(self):
        self.init_run()
        self.before_dispatch = lambda: self.update_actor(1, session_id="replacement-session")
        self.submit("spec", error="actor identity or lifecycle changed before dispatch")
        self.assertEqual(self.dispatches(), [])
        self.assertEqual(self.state()["pending_submission"]["identity"]["session_id"], "fixture-session-1")
        self.assertEqual(self.state()["submissions"], [])
        self.cli("reconcile-submission", "--not-delivered", error="session identity changed")

    def test_ambiguous_restart_reconciles_receipt_without_second_phase_send(self):
        self.init_run()
        self.submit("spec")
        old_transcript = Path(self.actors[0]["transcript"])
        old_bytes = old_transcript.read_bytes()
        fresh = self.directory / "fresh-author.jsonl"
        fresh.touch()
        self.update_actor(1, session_id="fresh-session", transcript=str(fresh),
                          state="waiting", transport="error-after-receipt")
        self.cli("restart-phase", "--prompt-file", str(self.context), error="failed after receipt")
        self.assertEqual(self.state()["pending_submission"]["kind"], "restart")
        self.assertEqual(self.state()["agent1"]["transcript"], str(old_transcript))
        self.cli("reconcile-submission", "--not-delivered", error="positively acknowledged")
        self.cli("reconcile-submission", "--delivered")
        self.return_text("spec")
        self.finish("spec")
        self.assertEqual(self.state()["agent1"]["transcript"], str(fresh))
        self.assertEqual(len(self.state()["submissions"]), 1)
        self.assertEqual(len(self.dispatches()), 2)
        self.assertEqual(old_transcript.read_bytes(), old_bytes)
        self.assertNotEqual(self.dispatches()[0]["prompt"].splitlines()[0],
                            self.dispatches()[1]["prompt"].splitlines()[0])

    def steering_file(self):
        path = self.directory / "steering.txt"
        path.write_text("Obstacle: The synthetic stage returned without collecting its final report.\n"
                        "Resolution: The already approved fixture result is available in the stage context.\n"
                        "Whole-stage direction: Resume ownership and finish the complete stage verification.\n")
        path.chmod(0o600)
        return path

    def test_ambiguous_steering_receipt_resumes_exactly_once(self):
        self.init_run(reverse=True, adopt=True)
        self.submit("implementation")
        self.return_text("implementation", "Missing the final fixture report; stage remains incomplete.")
        self.cli("stage-return")
        steering = self.steering_file()
        self.update_actor(1, transport="error-after-receipt")
        self.cli("steer-stage", "--prompt-file", str(steering), error="failed after receipt")
        self.assertEqual(self.state()["pending_submission"]["kind"], "steering")
        self.cli("steer-stage", "--prompt-file", str(steering), error="requires reconciliation")
        self.cli("reconcile-submission", "--not-delivered", error="positively acknowledged")
        self.cli("reconcile-submission", "--delivered")
        returned = self.return_text("implementation")
        self.assertIn(returned, self.finish("implementation"))
        self.complete(returned)
        self.assertEqual(len(self.dispatches()), 2)
        self.assertEqual(len(self.state()["steering_prompts"]), 1)
        self.assertEqual({item["buffer"] for item in self.dispatches()}, {"*fixture:1*"})

    def test_completion_rejects_busy_actor_and_unrelated_user_turn(self):
        self.init_run(reverse=True)
        self.submit("spec")
        self.return_text("spec", waiting=False)
        self.finish("spec", error="completion requires awaiting input")
        self.update_actor(1, state="waiting")
        self.append_record(1, {"type": "response_item", "payload": {
            "type": "message", "role": "user", "content": [{"type": "input_text", "text": "An unrelated user turn."}]}})
        self.return_text("spec")
        self.finish("spec", error="no returned assistant message")
        self.assertEqual(self.state()["completions"], [])
        self.assertEqual(len(self.dispatches()), 1)

    def test_unacknowledged_implementation_never_retries_or_reopens(self):
        self.init_run(adopt=True)
        self.update_actor(1, transport="error-before-receipt")
        self.submit("implementation", error="failed before receipt")
        self.cli("retry-delivery", error="cannot be retried")
        self.cli("reconcile-submission", "--delivered", error="receipt is missing")
        self.cli("reconcile-submission", "--not-delivered")
        self.assertEqual(self.state()["stop_evidence"]["kind"], "delivery-ambiguous")
        self.cli("steer-stage", "--prompt-file", str(self.steering_file()), error="cannot be steered")
        self.submit("implementation", error="cannot be steered or resubmitted")
        self.assertEqual(len(self.dispatches()), 1)
        self.assertEqual(self.state()["completions"][-1]["phase"], "plan-review")


if __name__ == "__main__":
    unittest.main()
