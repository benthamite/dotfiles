import importlib.machinery
import importlib.util
import io
import json
import os
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
    f"https://epochai.slack.com/archives/C12345678/p1710000000000100"
    f"?thread_ts={THREAD_TS}&cid=C12345678"
)
MESSAGE_PERMALINK = (
    "https://epochai.slack.com/archives/C12345678/p1710000000000100"
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
            side_effect=lambda expr, socket: captured.append(expr) or '"captured"',
        ):
            self.mod.prefill_slack_reply(permalink, draft_path, '/tmp/synthetic-socket')
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
        open_only=False,
        disconnected=False,
        missing_linked_message=False,
        replace_during_display=False,
    ):
        draft = "First line\nSecond line\n"
        target_ts = thread_ts or MESSAGE_TS
        target_name = f"*slack-thread: target - {target_ts}"
        with tempfile.TemporaryDirectory() as temp_dir:
            draft_path = pathlib.Path(temp_dir) / "draft.txt"
            draft_path.write_text(draft)
            expression = self.capture_prefill_expression(permalink, None if open_only else draft_path)
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
            display_hook = ""
            if replace_during_display:
                display_hook = """(with-current-buffer test-target-buffer
                        (erase-buffer) (insert "NEW USER INPUT"))"""
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
                        :room-id "C12345678"
                        :ts {json.dumps(MESSAGE_TS)}
                        :thread-ts {thread_ts_form}))
                (defun slack-team-find-by-domain (_) 'epoch-team)
                (defun slack-team-connectedp (_) {"nil" if disconnected else "t"})
                (defun slack-room-find (_ _) 'target-room)
                (defun slack-start (&rest _) (error "MUST NOT CONNECT"))
                (defun slack-ts (message) (test-message-ts message))
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
                     (append (list (make-test-message :ts test-fetch-ts))
                             {"nil" if missing_linked_message else '(list (make-test-message :ts ' + json.dumps(MESSAGE_TS) + '))'})
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
                  {display_hook}
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
                timeout=10,
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
            with self.assertRaisesRegex(RuntimeError, "not acknowledged"):
                self.mod.run_emacs_eval("(error \"broken\")", "/tmp/synthetic-socket")

    def test_open_only_preserves_existing_thread_input(self):
        _, payload = self.evaluate_prefill(THREAD_PERMALINK, thread_ts=THREAD_TS,
                                          target_input="UNSENT", open_only=True)
        self.assertIsNone(payload["failure"])
        self.assertEqual(payload["target"], "UNSENT")
        self.assertEqual(payload["result"], "draft-opened")

    def test_disconnected_team_is_not_started(self):
        _, payload = self.evaluate_prefill(THREAD_PERMALINK, thread_ts=THREAD_TS,
                                          disconnected=True)
        self.assert_unchanged_on_failure(payload, target="")
        self.assertEqual(payload["room-mutations"], 0)

    def test_linked_message_must_be_in_fetched_thread(self):
        _, payload = self.evaluate_prefill(THREAD_PERMALINK, thread_ts=THREAD_TS,
                                          missing_linked_message=True)
        self.assert_unchanged_on_failure(payload, target="")
        self.assertEqual(payload["room-mutations"], 0)

    def test_display_change_does_not_erase_new_user_input(self):
        _, payload = self.evaluate_prefill(THREAD_PERMALINK, thread_ts=THREAD_TS,
                                          replace_during_display=True)
        self.assertIsNotNone(payload["failure"])
        self.assertEqual(payload["target"], "NEW USER INPUT")

    def batch(self, expression):
        result = subprocess.run(["emacs", "-Q", "--batch", "--eval", expression],
                                capture_output=True, text=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads(result.stdout)

    def test_ring_isolated_exact_and_transform_aware(self):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory) / "unicode-😀.txt"
            path.write_bytes("Draft 😀\r\n".encode())
            captured = []
            with mock.patch.object(self.mod, "run_emacs_eval", side_effect=lambda e, s: captured.append(e) or "draft-ring-staged"):
                self.mod.copy_to_kill_ring(path, "/tmp/socket")
            for reject in (False, True, "mutate"):
                with self.subTest(reject=reject):
                    output = self.batch(f'''(progn (require 'json)
                    (let* ((kill-ring '("OLD")) (kill-ring-yank-pointer kill-ring)
                           (copied nil) (read nil)
                           (save-interprogram-paste-before-kill t)
                           (interprogram-cut-function (lambda (&rest _) (setq copied t)))
                           (interprogram-paste-function (lambda () (setq read t) "CLIPBOARD"))
                           (kill-transform-function {"(lambda (s) (aset s 0 ?X) s)" if reject == "mutate" else "(lambda (_) nil)" if reject else "nil"})
                           (result {captured[0]}))
                      (princ (json-encode (list result copied read (car kill-ring))))))''')
                    self.assertEqual(output[0], "draft-ring-failed" if reject else "draft-ring-staged")
                    self.assertEqual(output[1:3], [None, None])
                    self.assertEqual(output[3], "OLD" if reject else "Draft 😀\r\n")

    def channel(self, *, existing="", corrupt=False, open_only=False, display_change=False):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory) / "draft"
            path.write_text("DRAFT")
            captured = []
            with mock.patch.object(self.mod, "run_emacs_eval", side_effect=lambda e, s: captured.append(e) or "draft-composer-staged"):
                self.mod.prefill_channel_message("C12345678", None if open_only else path, "/tmp/socket")
            display_snippet = '(erase-buffer) (insert "NEW USER INPUT")' if display_change else ""
            corrupt_snippet = ""
            if corrupt:
                corrupt_snippet = """(add-hook 'after-change-functions
                     (lambda (&rest _) (let ((inhibit-modification-hooks t))
                       (goto-char (point-max)) (insert "CORRUPT"))) nil t)"""
            return self.batch(f'''(progn (require 'cl-lib) (require 'json)
              (defvar slack-debug nil)
              (defun slack-team-find-by-domain (domain)
                (unless (equal domain "epochai") (error "Wrong workspace")) 'team)
              (defun slack-room-find (&rest _) 'room)
              (defun slack-team-connectedp (_) t)
              (defun slack-create-message-buffer (_ cursor _)
                (unless (equal cursor "") (error "Wrong cursor")) 'object)
              (defun slack-buffer-buffer (_) (get-buffer-create "target"))
              (defun slack-buffer-display (_)
                (unless slack-debug (error "Unsafe display error handler"))
                (switch-to-buffer "target")
                {display_snippet})
              (with-current-buffer (get-buffer-create "target")
                (setq-local lui-input-marker (copy-marker (point-min)))
                (insert {json.dumps(existing)})
                {corrupt_snippet})
              (let (result failure)
                (condition-case nil (setq result {captured[0]}) (error (setq failure t)))
                (princ (json-encode
                  (list result failure (with-current-buffer "target" (buffer-string)))))))''')

    def test_channel_exact_and_no_overwrite(self):
        self.assertEqual(self.channel(), ["draft-composer-staged", None, "DRAFT"])
        self.assertEqual(self.channel(existing="DRAFT"), ["draft-composer-staged", None, "DRAFT"])
        self.assertEqual(self.channel(existing="UNSENT"), [None, True, "UNSENT"])

    def test_channel_corruption_rolls_back(self):
        self.assertEqual(self.channel(corrupt=True), [None, True, ""])

    def test_channel_display_change_preserves_new_input(self):
        self.assertEqual(self.channel(display_change=True), [None, True, "NEW USER INPUT"])

    def test_channel_open_only_preserves_input(self):
        self.assertEqual(self.channel(existing="UNSENT", open_only=True), ["draft-opened", None, "UNSENT"])

    def test_client_is_bounded_and_rejects_untrusted_output(self):
        for output in ("nil", '"PRIVATE DRAFT"', '*ERROR* PRIVATE DRAFT'):
            with mock.patch.object(self.mod.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, output, "SECRET")) as run:
                with self.assertRaises(self.mod.UncertainResult) as caught:
                    self.mod.run_emacs_eval("(ignore)", "/tmp/explicit-socket")
                self.assertNotIn("PRIVATE", str(caught.exception))
                self.assertEqual(run.call_args.kwargs["timeout"], 65)
                self.assertEqual(run.call_args.args[0][1:5], ["--socket-name", "/tmp/explicit-socket", "--alternate-editor", "false"])

    def test_private_snapshot_preserves_bytes_and_freezes_source(self):
        original_mkdtemp = tempfile.mkdtemp
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory) / "draft😀"
            path.write_bytes("original 😀\r\n".encode())
            path.chmod(0o600)
            with mock.patch.object(self.mod.tempfile, "mkdtemp", side_effect=lambda **_: original_mkdtemp(dir=directory)):
                with self.mod.draft_snapshot(str(path)) as snapshot:
                    self.assertTrue(snapshot.is_absolute())
                    self.assertEqual(snapshot.stat().st_mode & 0o777, 0o600)
                    path.write_text("changed")
                    self.assertEqual(snapshot.read_bytes(), "original 😀\r\n".encode())
                self.assertFalse(snapshot.exists())

    def test_client_timeout_and_decode_errors_are_uncertain_and_safe(self):
        for error in (subprocess.TimeoutExpired("emacsclient", 65, output="PRIVATE"),
                      UnicodeDecodeError("utf8", b"SECRET\xff", 6, 7, "bad")):
            with mock.patch.object(self.mod.subprocess, "run", side_effect=error):
                with self.assertRaises((TimeoutError, self.mod.UncertainResult)) as caught:
                    self.mod.run_emacs_eval("(ignore)", "/tmp/socket")
                self.assertNotIn("SECRET", str(caught.exception))
                self.assertNotIn("PRIVATE", str(caught.exception))

    def test_socket_is_explicit_existing_owned_socket(self):
        import stat
        with self.assertRaises(ValueError):
            self.mod.validate_socket("relative")
        with mock.patch.object(pathlib.Path, "stat", return_value=mock.Mock(st_mode=stat.S_IFREG | 0o600, st_uid=os.getuid())):
            with self.assertRaises(ValueError):
                self.mod.validate_socket("/tmp/not-socket")
        with mock.patch.object(pathlib.Path, "stat", return_value=mock.Mock(st_mode=stat.S_IFSOCK | 0o600, st_uid=os.getuid())):
            self.assertEqual(self.mod.validate_socket("/tmp/socket"), "/tmp/socket")

    def test_snapshot_refuses_public_symlink_and_invalid_utf8(self):
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory) / "draft"
            path.write_bytes(b"draft")
            path.chmod(0o644)
            with self.assertRaises(ValueError):
                with self.mod.draft_snapshot(str(path)): self.fail("Accepted public file")
            path.chmod(0o600)
            link = pathlib.Path(directory) / "link"
            link.symlink_to(path)
            with self.assertRaises(OSError):
                with self.mod.draft_snapshot(str(link)): self.fail("Accepted symlink")
            path.write_bytes(b"\xff")
            with self.assertRaises(UnicodeError):
                with self.mod.draft_snapshot(str(path)): self.fail("Accepted invalid UTF8")

    def test_snapshot_survives_uncertain_outcome(self):
        original_mkdtemp = tempfile.mkdtemp
        with tempfile.TemporaryDirectory() as directory:
            path = pathlib.Path(directory) / "draft"
            path.write_text("DRAFT")
            path.chmod(0o600)
            with mock.patch.object(self.mod.tempfile, "mkdtemp", side_effect=lambda **_: original_mkdtemp(dir=directory)), mock.patch("sys.stderr", new_callable=io.StringIO) as err:
                with self.assertRaises(TimeoutError):
                    with self.mod.draft_snapshot(str(path)) as snapshot:
                        raise TimeoutError()
                self.assertTrue(snapshot.exists())
                self.assertIn(str(snapshot), err.getvalue())

    def test_invalid_target_does_not_touch_ring_or_client(self):
        for url in ("https://evil.slack.com/archives/C12345678/p1710000000000000", THREAD_PERMALINK + "&cid=OTHER"):
            with mock.patch.object(sys, "argv", ["copy", "--socket", "/tmp/socket", "--file", "/missing", "--permalink", url]), mock.patch.object(self.mod, "copy_to_kill_ring") as ring, mock.patch.object(self.mod, "run_emacs_eval") as client, mock.patch("sys.stdout", new_callable=io.StringIO):
                self.assertEqual(self.mod.main(), 1)
                ring.assert_not_called()
                client.assert_not_called()

    def test_open_only_main_never_reads_file_or_stages_ring(self):
        with mock.patch.object(sys, "argv", ["copy", "--socket", "/tmp/socket", "--channel", "C12345678", "--open-only"]), mock.patch.object(self.mod, "validate_socket", return_value="/tmp/socket"), mock.patch.object(self.mod, "draft_snapshot") as snapshot, mock.patch.object(self.mod, "copy_to_kill_ring") as ring, mock.patch.object(self.mod, "prefill_channel_message", return_value="draft-opened") as stage, mock.patch("sys.stdout", new_callable=io.StringIO) as output:
            self.assertEqual(self.mod.main(), 0)
            snapshot.assert_not_called()
            ring.assert_not_called()
            stage.assert_called_once_with("C12345678", None, "/tmp/socket")
            self.assertFalse(json.loads(output.getvalue())["composer"])

    def test_composer_success_ring_failure_is_partial(self):
        from contextlib import nullcontext
        with mock.patch.object(sys, "argv", ["copy", "--socket", "/tmp/socket", "--channel", "C12345678", "--file", "/synthetic"]), mock.patch.object(self.mod, "validate_socket", return_value="/tmp/socket"), mock.patch.object(self.mod, "draft_snapshot", return_value=nullcontext(pathlib.Path("/snapshot"))), mock.patch.object(self.mod, "prefill_channel_message", return_value="draft-composer-staged"), mock.patch.object(self.mod, "copy_to_kill_ring", side_effect=self.mod.UncertainResult()), mock.patch("sys.stdout", new_callable=io.StringIO) as output:
            self.assertEqual(self.mod.main(), 1)
            result = json.loads(output.getvalue())
            self.assertTrue(result["composer"])
            self.assertFalse(result["ring"])
            self.assertEqual((result["status"], result["stage"]), ("uncertain", "ring"))


    def test_stdin_draft_is_private_exact_and_strips_one_newline(self):
        import stat as stat_module
        stdin = mock.Mock()
        stdin.buffer = io.BytesIO("draft ñ line\n".encode("utf-8"))
        with mock.patch.object(sys, "stdin", stdin):
            with self.mod.stdin_draft() as source:
                path = pathlib.Path(source)
                self.assertEqual(path.read_bytes(), "draft ñ line".encode("utf-8"))
                self.assertEqual(stat_module.S_IMODE(path.stat().st_mode), 0o600)
                self.assertEqual(stat_module.S_IMODE(path.parent.stat().st_mode), 0o700)
                with self.mod.draft_snapshot(source) as snapshot:
                    self.assertEqual(snapshot.read_bytes(), "draft ñ line".encode("utf-8"))
        self.assertFalse(path.exists())

    def test_main_uses_stdin_and_default_socket(self):
        stdin = mock.Mock()
        stdin.buffer = io.BytesIO(b"draft\n")
        seen = []

        def fake_stage(channel, snapshot, socket):
            seen.append((channel, snapshot.read_bytes(), socket))
            return "draft-composer-staged"

        with mock.patch.object(sys, "argv", ["copy", "--stdin", "--channel", "C12345678"]), mock.patch.object(sys, "stdin", stdin), mock.patch.object(self.mod, "default_socket", return_value="/tmp/default-socket"), mock.patch.object(self.mod, "validate_socket") as validate, mock.patch.object(self.mod, "prefill_channel_message", side_effect=fake_stage), mock.patch.object(self.mod, "copy_to_kill_ring") as ring, mock.patch("sys.stdout", new_callable=io.StringIO) as output:
            self.assertEqual(self.mod.main(), 0)
            validate.assert_not_called()
            self.assertEqual(seen, [("C12345678", b"draft", "/tmp/default-socket")])
            ring.assert_called_once()
            self.assertEqual(json.loads(output.getvalue())["status"], "ok")

    def test_main_without_server_or_with_both_sources_fails_before_client(self):
        stdin = mock.Mock()
        stdin.buffer = io.BytesIO(b"draft")
        for argv in (["copy", "--stdin", "--channel", "C12345678"], ["copy", "--stdin", "--file", "/x", "--channel", "C12345678"]):
            with self.subTest(argv=argv), mock.patch.object(sys, "argv", argv), mock.patch.object(sys, "stdin", stdin), mock.patch.object(self.mod, "default_socket", return_value=None), mock.patch.object(self.mod, "run_emacs_eval") as client, mock.patch("sys.stdout", new_callable=io.StringIO) as output:
                self.assertEqual(self.mod.main(), 1)
                client.assert_not_called()
                self.assertEqual(json.loads(output.getvalue())["stage"], "validation")


if __name__ == "__main__":
    unittest.main()
