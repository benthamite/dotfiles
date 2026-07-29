import importlib.machinery
import importlib.util
import json
import pathlib
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "claude" / "bin" / "copy-slack-draft"
THREAD_TS = "1710000000.000000"
MESSAGE_TS = "1710000000.000100"
THREAD_PERMALINK = (
    f"https://epochai.slack.com/archives/C123/p1710000000000100"
    f"?thread_ts={THREAD_TS}&cid=C123"
)
MESSAGE_PERMALINK = (
    "https://epochai.slack.com/archives/C123/p1710000000000100"
)


def load_module():
    loader = importlib.machinery.SourceFileLoader(
        "copy_slack_draft", str(SCRIPT)
    )
    spec = importlib.util.spec_from_loader("copy_slack_draft", loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules["copy_slack_draft"] = module
    spec.loader.exec_module(module)
    return module


class CopySlackDraftTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()

    def capture_prefill_expression(self, permalink, draft_path):
        captured = []
        with mock.patch.object(
            self.mod,
            "run_emacs_eval",
            side_effect=lambda expr: captured.append(expr) or '"captured"',
        ):
            self.mod.prefill_slack_reply(permalink, draft_path)
        self.assertEqual(len(captured), 1)
        return captured[0]

    def evaluate_prefill(
        self,
        permalink,
        *,
        thread_ts,
        target_input="",
        other_input="UNRELATED UNSENT",
        fetch_delay=0,
        fetch_error=False,
        corrupt_target_after_insert=False,
        force_timeout=False,
    ):
        draft = "First line\nSecond line\n"
        target_ts = thread_ts or MESSAGE_TS
        target_name = f"*slack-thread: target - {target_ts}"
        with tempfile.TemporaryDirectory() as temp_dir:
            draft_path = pathlib.Path(temp_dir) / "draft.txt"
            draft_path.write_text(draft)
            expression = self.capture_prefill_expression(permalink, draft_path)
            thread_ts_form = "nil" if thread_ts is None else json.dumps(thread_ts)
            corrupt_hook = ""
            if corrupt_target_after_insert:
                corrupt_hook = """
                  (with-current-buffer test-target-buffer
                    (add-hook
                     'after-change-functions
                     (lambda (&rest _)
                       (let ((inhibit-modification-hooks t))
                         (goto-char (point-max))
                         (insert "!")))
                     nil t))
                """
            time_binding = ""
            if force_timeout:
                time_binding = """
                    ((symbol-function 'float-time)
                     (lambda (&optional _)
                       (prog1 (* test-float-time-calls 31)
                         (setq test-float-time-calls
                               (1+ test-float-time-calls)))))
                """
            program = f"""
              (progn
                (require 'cl-lib)
                (require 'json)
                (require 'seq)
                (require 'subr-x)
                (defvar test-target-buffer nil)
                (defvar test-other-buffer nil)
                (defvar test-thread-cache
                  (make-hash-table :test 'equal))
                (defvar test-room-messages nil)
                (defvar test-fetch-room nil)
                (defvar test-fetch-team nil)
                (defvar test-fetch-ts nil)
                (defvar test-fetch-callback nil)
                (defvar test-fetch-error-callback nil)
                (defvar test-fetch-delay {fetch_delay})
                (defvar test-fetch-error {"t" if fetch_error else "nil"})
                (defvar test-float-time-calls 0)
                (defvar test-room-mutation-count 0)
                (defvar test-thread-create-count 0)
                (defvar test-display-count 0)
                (cl-defstruct test-message ts)
                (cl-defstruct test-thread
                  room team ts has-more buffer)
                (defun slack-browse-url (&rest _) nil)
                (defun slack-permalink-to-info (_)
                  (list :team-domain "epochai"
                        :room-id "C123"
                        :ts {json.dumps(MESSAGE_TS)}
                        :thread-ts {thread_ts_form}))
                (defun slack-team-find-by-domain (_) 'epoch-team)
                (defun slack-team-connectedp (_) t)
                (defun slack-room-find (_ _) 'target-room)
                (defun slack-start (&rest _) nil)
                (defun test-finish-fetch ()
                  (if test-fetch-error
                      (funcall
                       test-fetch-error-callback
                       :error-thrown '(error http 500)
                       :symbol-status 'error
                       :response
                       '(:settings
                         (:headers
                          (("Authorization"
                            . "Bearer TEST-SLACK-SECRET"))))
                       :data '(:ok nil :error "channel_not_found"))
                    (funcall
                     test-fetch-callback
                     (list (make-test-message :ts test-fetch-ts))
                     "next-cursor"
                     t)))
                (cl-defun slack-conversations-replies
                    (room ts team
                          &key after-success on-error &allow-other-keys)
                  (setq test-fetch-room room
                        test-fetch-team team
                        test-fetch-ts ts
                        test-fetch-callback after-success
                        test-fetch-error-callback on-error)
                  (if (= test-fetch-delay 0)
                      (test-finish-fetch)
                    (run-at-time test-fetch-delay nil
                                 #'test-finish-fetch)))
                (defun slack-room-set-messages (_ messages _)
                  (setq test-room-messages messages
                        test-room-mutation-count
                        (1+ test-room-mutation-count)))
                (defun slack-message-set-replies (&rest _)
                  (setq test-room-mutation-count
                        (1+ test-room-mutation-count)))
                (defun slack-room-find-message (_ ts)
                  (seq-find
                   (lambda (message)
                     (equal (test-message-ts message) ts))
                   test-room-messages))
                (defun slack-create-thread-message-buffer
                    (room team ts &optional has-more)
                  (setq test-thread-create-count
                        (1+ test-thread-create-count))
                  (let* ((key (cons room ts))
                         (existing (gethash key test-thread-cache)))
                    (or existing
                        (let ((thread
                               (make-test-thread
                                :room room
                                :team team
                                :ts ts
                                :has-more has-more
                                :buffer test-target-buffer)))
                          (puthash key thread test-thread-cache)
                          thread))))
                (defun slack-thread--sync-buffer (&rest _) nil)
                (defun slack-buffer-buffer (thread)
                  (test-thread-buffer thread))
                (defun slack-buffer-display (thread)
                  (setq test-display-count (1+ test-display-count))
                  (switch-to-buffer (test-thread-buffer thread)))
                ;; Model the inherited implementation's generic opener.  Its
                ;; target appears after the fixed wait has already selected a
                ;; different thread buffer.
                (defun test-finish-legacy-open ()
                  (switch-to-buffer test-target-buffer))
                (defun slack-open-message (&rest _)
                  (run-at-time 0.02 nil #'test-finish-legacy-open))
                (defun test-input-buffer (name text)
                  (let ((buffer (get-buffer-create name)))
                    (with-current-buffer buffer
                      (erase-buffer)
                      (setq-local lui-input-marker
                                  (copy-marker (point-min)))
                      (insert text))
                    buffer))
                (setq test-target-buffer
                      (test-input-buffer
                       {json.dumps(target_name)}
                       {json.dumps(target_input)})
                      test-other-buffer
                      (test-input-buffer
                       "*slack-thread: other - 1700000000.000000"
                       {json.dumps(other_input)}))
                ;; A same-timestamp thread in another room must not match.
                (puthash
                 (cons 'other-room {json.dumps(target_ts)})
                 (make-test-thread
                  :room 'other-room
                  :team 'epoch-team
                  :ts {json.dumps(target_ts)}
                  :has-more nil
                  :buffer test-other-buffer)
                 test-thread-cache)
                {corrupt_hook}
                (switch-to-buffer test-other-buffer)
                (let (result failure)
                  (cl-letf (((symbol-function 'sit-for)
                             (lambda (&rest _)
                               (accept-process-output nil 0.005)))
                            {time_binding})
                    (condition-case err
                        (setq result {expression})
                      (error
                       (setq failure (error-message-string err)))))
                  ;; Let deliberately late fetch/open callbacks run so the
                  ;; test observes side effects after the caller returned.
                  (accept-process-output nil 0.06)
                  (princ
                   (json-encode
                    `((result . ,result)
                      (failure . ,failure)
                      (target . ,(with-current-buffer
                                     test-target-buffer
                                   (buffer-string)))
                      (other . ,(with-current-buffer
                                    test-other-buffer
                                  (buffer-string)))
                      (visible
                       . ,(buffer-name
                           (window-buffer (selected-window))))
                      (fetch-room . ,test-fetch-room)
                      (fetch-team . ,test-fetch-team)
                      (fetch-ts . ,test-fetch-ts)
                      (room-mutations . ,test-room-mutation-count)
                      (thread-creations . ,test-thread-create-count)
                      (displays . ,test-display-count)
                      (target-has-more
                       . ,(let ((thread
                                 (gethash
                                  (cons 'target-room
                                        {json.dumps(target_ts)})
                                  test-thread-cache)))
                            (and thread
                                 (test-thread-has-more thread)))))))))
            """
            result = subprocess.run(
                ["emacs", "-Q", "--batch", "--eval", program],
                text=True,
                capture_output=True,
            )
        self.assertEqual(result.returncode, 0, result.stderr)
        return draft, json.loads(result.stdout)

    def assert_unchanged_on_failure(self, payload, *, target):
        self.assertIsNotNone(payload["failure"])
        self.assertEqual(payload["target"], target)
        self.assertEqual(payload["other"], "UNRELATED UNSENT")
        self.assertEqual(
            payload["visible"],
            "*slack-thread: other - 1700000000.000000",
        )

    def test_targets_exact_room_and_thread_instead_of_first_buffer(self):
        draft, payload = self.evaluate_prefill(
            THREAD_PERMALINK,
            thread_ts=THREAD_TS,
        )

        self.assertIsNone(payload["failure"])
        self.assertEqual(payload["other"], "UNRELATED UNSENT")
        self.assertEqual(payload["target"], draft)
        self.assertEqual(payload["fetch-room"], "target-room")
        self.assertEqual(payload["fetch-team"], "epoch-team")
        self.assertEqual(payload["fetch-ts"], THREAD_TS)
        self.assertEqual(
            payload["visible"],
            f"*slack-thread: target - {THREAD_TS}",
        )

    def test_waits_for_async_unloaded_thread_before_staging(self):
        draft, payload = self.evaluate_prefill(
            THREAD_PERMALINK,
            thread_ts=THREAD_TS,
            fetch_delay=0.02,
        )

        self.assertIsNone(payload["failure"])
        self.assertEqual(payload["target"], draft)
        self.assertEqual(payload["fetch-ts"], THREAD_TS)
        self.assertGreater(payload["room-mutations"], 0)
        self.assertEqual(payload["thread-creations"], 1)
        self.assertTrue(payload["target-has-more"])

    def test_plain_message_fetches_thread_rooted_at_message_timestamp(self):
        draft, payload = self.evaluate_prefill(
            MESSAGE_PERMALINK,
            thread_ts=None,
            fetch_delay=0.02,
        )

        self.assertIsNone(payload["failure"])
        self.assertEqual(payload["target"], draft)
        self.assertEqual(payload["fetch-ts"], MESSAGE_TS)
        self.assertEqual(
            payload["visible"],
            f"*slack-thread: target - {MESSAGE_TS}",
        )

    def test_refuses_to_overwrite_existing_target_input(self):
        payload = self.evaluate_prefill(
            THREAD_PERMALINK,
            thread_ts=THREAD_TS,
            target_input="TARGET UNSENT",
        )[1]

        self.assert_unchanged_on_failure(payload, target="TARGET UNSENT")
        self.assertIn("already contains unsent text", payload["failure"])

    def test_corrupt_insertion_is_rejected_and_rolled_back(self):
        payload = self.evaluate_prefill(
            THREAD_PERMALINK,
            thread_ts=THREAD_TS,
            corrupt_target_after_insert=True,
        )[1]

        self.assert_unchanged_on_failure(payload, target="")
        self.assertIn("does not exactly match", payload["failure"])

    def test_late_callback_after_timeout_has_no_side_effects(self):
        payload = self.evaluate_prefill(
            THREAD_PERMALINK,
            thread_ts=THREAD_TS,
            fetch_delay=0.02,
            force_timeout=True,
        )[1]

        self.assertEqual(
            payload["visible"],
            "*slack-thread: other - 1700000000.000000",
        )
        self.assertEqual(payload["target"], "")
        self.assertEqual(payload["other"], "UNRELATED UNSENT")
        self.assertIsNotNone(payload["failure"])
        self.assertIn("Timed out waiting", payload["failure"])
        self.assertEqual(payload["room-mutations"], 0)
        self.assertEqual(payload["thread-creations"], 0)
        self.assertEqual(payload["displays"], 0)

    def test_fetch_error_has_no_side_effects(self):
        payload = self.evaluate_prefill(
            THREAD_PERMALINK,
            thread_ts=THREAD_TS,
            fetch_error=True,
        )[1]

        self.assert_unchanged_on_failure(payload, target="")
        self.assertIn("channel_not_found", payload["failure"])
        self.assertNotIn("TEST-SLACK-SECRET", payload["failure"])
        self.assertEqual(payload["room-mutations"], 0)
        self.assertEqual(payload["thread-creations"], 0)
        self.assertEqual(payload["displays"], 0)

    def test_emacsclient_stdout_error_is_not_success(self):
        completed = subprocess.CompletedProcess(
            ["emacsclient"], 0, stdout='*ERROR*: (error "broken")\n', stderr=""
        )
        with mock.patch.object(
            self.mod.subprocess, "run", return_value=completed
        ):
            with self.assertRaisesRegex(RuntimeError, "broken"):
                self.mod.run_emacs_eval("(error \"broken\")")


if __name__ == "__main__":
    unittest.main()
