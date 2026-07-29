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
THREAD_PERMALINK = (
    "https://epochai.slack.com/archives/C123/p1710000000000100"
    "?thread_ts=1710000000.000000&cid=C123"
)
MESSAGE_PERMALINK = (
    "https://epochai.slack.com/archives/C123/p1710000000000100"
)


def load_module():
    loader = importlib.machinery.SourceFileLoader("copy_slack_draft", str(SCRIPT))
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
        corrupt_target_after_insert=False,
        force_timeout=False,
    ):
        draft = "First line\nSecond line\n"
        with tempfile.TemporaryDirectory() as temp_dir:
            draft_path = pathlib.Path(temp_dir) / "draft.txt"
            draft_path.write_text(draft)
            expression = self.capture_prefill_expression(permalink, draft_path)
            thread_ts_form = "nil" if thread_ts is None else json.dumps(thread_ts)
            target_ts = thread_ts or "1710000000.000100"
            target_buffer_name = f"*slack-thread: target - {target_ts}"
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
                (defvar test-created-room nil)
                (defvar test-created-team nil)
                (defvar test-created-thread-ts nil)
                (defvar test-thread-cache
                  (make-hash-table :test 'equal))
                (defvar test-parent-loaded nil)
                (defvar test-open-room nil)
                (defvar test-open-team nil)
                (defvar test-open-thread-ts nil)
                (defvar test-open-callback nil)
                (defvar test-channel-callback nil)
                (defvar test-channel-buffer nil)
                (defvar test-callback-delay
                  {"0.02" if force_timeout else "0"})
                (defvar test-float-time-calls 0)
                (cl-defstruct test-thread
                  room team ts has-more buffer)
                (defun slack-browse-url (&rest _) nil)
                (defun slack-permalink-to-info (_)
                  (list :team-domain "epochai"
                        :room-id "C123"
                        :ts "1710000000.000100"
                        :thread-ts {thread_ts_form}))
                (defun slack-team-find-by-domain (_) 'epoch-team)
                (defun slack-team-connectedp (_) t)
                (defun slack-room-find (_ _) 'target-room)
                (defun slack-create-thread-message-buffer
                    (room team thread-ts &optional has-more)
                  (setq test-created-room room
                        test-created-team team
                        test-created-thread-ts thread-ts)
                  (let* ((key (cons room thread-ts))
                         (existing (gethash key test-thread-cache)))
                    (or existing
                        (let ((thread
                               (make-test-thread
                                :room room
                                :team team
                                :ts thread-ts
                                :has-more has-more
                                :buffer test-target-buffer)))
                          (puthash key thread test-thread-cache)
                          thread))))
                (defun slack-buffer-buffer (thread)
                  (test-thread-buffer thread))
                (defun slack-buffer-display (thread)
                  (switch-to-buffer (test-thread-buffer thread)))
                (defun test-finish-thread-open ()
                  (let* ((key (cons test-open-room
                                    test-open-thread-ts))
                         (existing (gethash key test-thread-cache))
                         (thread
                          (slack-create-thread-message-buffer
                           test-open-room test-open-team
                           test-open-thread-ts t)))
                    (unless existing
                      (setq test-parent-loaded t))
                    (slack-buffer-display thread)
                    (when test-open-callback
                      (funcall test-open-callback))))
                (defun test-finish-channel-open ()
                  (setq test-parent-loaded t)
                  (slack-room-display
                   test-open-room test-open-team
                   test-channel-callback))
                (defun slack-open-message
                    (team room ts thread-ts &rest _)
                  (setq test-open-room room
                        test-open-team team
                        test-open-thread-ts (or thread-ts ts)
                        test-open-callback nil
                        test-channel-callback nil)
                  (run-at-time
                   test-callback-delay nil
                   (if thread-ts
                       #'test-finish-thread-open
                     #'test-finish-channel-open)))
                (defun slack-open-message--open-thread
                    (room thread-ts team callback _)
                  (setq test-open-room room
                        test-open-team team
                        test-open-thread-ts thread-ts
                        test-open-callback callback)
                  (run-at-time
                   test-callback-delay nil
                   #'test-finish-thread-open))
                (defun slack-open-message--open-channel
                    (_ room team callback after-success)
                  (setq test-open-room room
                        test-open-team team
                        test-channel-callback
                        (or after-success callback))
                  (run-at-time
                   test-callback-delay nil
                   #'test-finish-channel-open))
                (defun slack-room-display (_ _ &optional callback)
                  (switch-to-buffer test-channel-buffer)
                  (when callback
                    (funcall callback)))
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
                       {json.dumps(target_buffer_name)}
                       {json.dumps(target_input)})
                      test-other-buffer
                      (test-input-buffer
                       "*slack-thread: other - 1700000000.000000"
                       {json.dumps(other_input)})
                      test-channel-buffer
                      (get-buffer-create "*slack-channel: target*"))
                {corrupt_hook}
                (switch-to-buffer test-other-buffer)
                (let (result failure)
                  (cl-letf (((symbol-function 'sit-for)
                             (lambda (&rest _) nil))
                            {time_binding})
                    (condition-case err
                        (setq result {expression})
                      (error
                       (setq failure (error-message-string err)))))
                  (accept-process-output nil 0.05)
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
                      (parent-loaded . ,test-parent-loaded)
                      (target-has-more
                       . ,(let ((thread
                                 (gethash
                                  (cons 'target-room
                                        (or {thread_ts_form}
                                            "1710000000.000100"))
                                  test-thread-cache)))
                            (and thread
                                 (test-thread-has-more thread))))
                      (created-room . ,test-created-room)
                      (created-team . ,test-created-team)
                      (created-thread-ts . ,test-created-thread-ts))))))
            """
            result = subprocess.run(
                ["emacs", "-Q", "--batch", "--eval", program],
                text=True,
                capture_output=True,
            )
        self.assertEqual(result.returncode, 0, result.stderr)
        return draft, json.loads(result.stdout)

    def test_prefill_targets_exact_permalink_thread(self):
        draft, payload = self.evaluate_prefill(
            THREAD_PERMALINK,
            thread_ts="1710000000.000000",
        )

        self.assertIsNone(payload["failure"])
        self.assertEqual(payload["target"], draft)
        self.assertEqual(payload["other"], "UNRELATED UNSENT")
        self.assertTrue(payload["parent-loaded"])
        self.assertTrue(payload["target-has-more"])
        self.assertEqual(
            payload["visible"],
            "*slack-thread: target - 1710000000.000000",
        )
        self.assertEqual(payload["created-room"], "target-room")
        self.assertEqual(payload["created-team"], "epoch-team")
        self.assertEqual(
            payload["created-thread-ts"],
            "1710000000.000000",
        )
        self.assertEqual(
            payload["result"],
            "*slack-thread: target - 1710000000.000000",
        )

    def test_plain_message_permalink_targets_new_thread_at_message(self):
        draft, payload = self.evaluate_prefill(
            MESSAGE_PERMALINK,
            thread_ts=None,
        )

        self.assertIsNone(payload["failure"])
        self.assertEqual(payload["target"], draft)
        self.assertEqual(payload["other"], "UNRELATED UNSENT")
        self.assertTrue(payload["parent-loaded"])
        self.assertEqual(
            payload["visible"],
            "*slack-thread: target - 1710000000.000100",
        )
        self.assertEqual(
            payload["created-thread-ts"],
            "1710000000.000100",
        )

    def test_prefill_refuses_to_overwrite_existing_target_input(self):
        _, payload = self.evaluate_prefill(
            THREAD_PERMALINK,
            thread_ts="1710000000.000000",
            target_input="TARGET UNSENT",
        )

        self.assertIsNotNone(payload["failure"])
        self.assertIn("already contains unsent text", payload["failure"])
        self.assertEqual(payload["target"], "TARGET UNSENT")
        self.assertEqual(payload["other"], "UNRELATED UNSENT")

    def test_prefill_detects_non_exact_target_content(self):
        _, payload = self.evaluate_prefill(
            THREAD_PERMALINK,
            thread_ts="1710000000.000000",
            other_input="",
            corrupt_target_after_insert=True,
        )

        self.assertIsNotNone(payload["failure"])
        self.assertIn("does not exactly match", payload["failure"])

    def test_prefill_timeout_prevents_late_insertion(self):
        _, payload = self.evaluate_prefill(
            THREAD_PERMALINK,
            thread_ts="1710000000.000000",
            force_timeout=True,
        )

        self.assertIsNotNone(payload["failure"])
        self.assertIn("Timed out waiting", payload["failure"])
        self.assertEqual(payload["target"], "")
        self.assertEqual(payload["other"], "UNRELATED UNSENT")


if __name__ == "__main__":
    unittest.main()
