;;; doom-modeline-extras-test.el --- Tests for doom-modeline-extras -*- lexical-binding: t -*-

;; Tests for doom-modeline-extras.el.

;;; Code:

(require 'ert)
(require 'agent)
(require 'doom-modeline-extras)

(defvar ghub-default-host-alist)

;;;; GitHub notifications

(ert-deftest doom-modeline-extras-test-github-fetch-count-form-returns-count ()
  "GitHub fetch count form returns a count instead of the payload."
  (require 'async)
  (let ((ghub-default-host-alist '((github . "api.github.com")))
        (orig-require (symbol-function 'require))
        calls)
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (if (eq feature 'ghub)
                     t
                   (funcall orig-require feature filename noerror))))
              ((symbol-function 'run-hooks)
               (lambda (&rest _) nil))
              ((symbol-function 'ghub--username)
               (lambda (host)
                 (push (list :username host) calls)
                 "user"))
              ((symbol-function 'ghub--token)
               (lambda (&rest args)
                 (push (cons :token args) calls)
                 "token"))
              ((symbol-function 'ghub-get)
               (lambda (&rest args)
                 (push (cons :get args) calls)
                 '((id . "1") (id . "2")))))
      (should (= (funcall (doom-modeline-extras--github-fetch-count-form)) 2))
      (should (assq :get calls)))))

(ert-deftest doom-modeline-extras-test-github-fetch-disables-password-prompt ()
  "GitHub fetch disables async password prompting and skips overlap."
  (require 'async)
  (let ((doom-modeline-github t)
        (doom-modeline-extras--github-fetch-process nil)
        (doom-modeline-extras--github-fetch-started nil)
        (orig-require (symbol-function 'require))
        captured-finish
        captured-prompt
        (start-count 0))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (funcall orig-require feature filename noerror)))
              ((symbol-function 'process-live-p)
               (lambda (proc) (eq proc 'live-process)))
              ((symbol-function 'async-start)
               (lambda (_start finish)
                 (cl-incf start-count)
                 (setq captured-finish finish
                       captured-prompt async-prompt-for-password)
                 'live-process)))
      (doom-modeline-extras--github-fetch-notifications)
      (doom-modeline-extras--github-fetch-notifications)
      (should (= start-count 1))
      (should-not captured-prompt)
      (funcall captured-finish 7)
      (should-not doom-modeline-extras--github-fetch-process))))

(ert-deftest doom-modeline-extras-test-github-fetch-reaps-dead-child ()
  "GitHub fetch restarts when the previous child died without reporting."
  (require 'async)
  (let ((doom-modeline-github t)
        (doom-modeline-extras--github-fetch-process 'dead-process)
        (doom-modeline-extras--github-fetch-started (current-time))
        (orig-require (symbol-function 'require))
        (start-count 0))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (funcall orig-require feature filename noerror)))
              ((symbol-function 'process-live-p)
               (lambda (proc) (eq proc 'live-process)))
              ((symbol-function 'process-buffer)
               (lambda (_proc) nil))
              ((symbol-function 'async-start)
               (lambda (_start _finish)
                 (cl-incf start-count)
                 'live-process)))
      (doom-modeline-extras--github-fetch-notifications)
      (should (= start-count 1))
      (should (eq doom-modeline-extras--github-fetch-process 'live-process)))))

(ert-deftest doom-modeline-extras-test-github-fetch-reaps-hung-child ()
  "GitHub fetch kills and replaces a child that exceeded the timeout."
  (require 'async)
  (let ((doom-modeline-github t)
        (doom-modeline-extras--github-fetch-process 'live-process)
        (doom-modeline-extras--github-fetch-started
         (time-subtract (current-time)
                        (seconds-to-time
                         (1+ doom-modeline-extras--github-fetch-timeout))))
        (orig-require (symbol-function 'require))
        deleted
        (start-count 0))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (funcall orig-require feature filename noerror)))
              ((symbol-function 'process-live-p)
               (lambda (proc) (eq proc 'live-process)))
              ((symbol-function 'process-buffer)
               (lambda (_proc) nil))
              ((symbol-function 'delete-process)
               (lambda (proc) (setq deleted proc)))
              ((symbol-function 'async-start)
               (lambda (_start _finish)
                 (cl-incf start-count)
                 'new-process)))
      (doom-modeline-extras--github-fetch-notifications)
      (should (eq deleted 'live-process))
      (should (= start-count 1))
      (should (eq doom-modeline-extras--github-fetch-process 'new-process)))))

;;;; AI session status

(ert-deftest doom-modeline-extras-test-format-codex-status-fields-shows-effort ()
  "Format Codex modeline fields with model and reasoning effort."
  (cl-letf (((symbol-function 'agent-codex-status-model)
             (lambda () "gpt-5.5"))
            ((symbol-function 'agent-codex-status-effort)
             (lambda () "high")))
    (with-temp-buffer
      (agent--set-session (current-buffer)
                          (agent-session-create :backend 'codex
                                                :account "epoch"
                                                :directory "~/project/"))
      (should (equal (doom-modeline-extras--format-codex-status-fields)
                     " | gpt-5.5 | high | epoch")))))

(ert-deftest doom-modeline-extras-test-format-claude-status-fields-shows-effort ()
  "Format Claude modeline fields with model and effort."
  (cl-letf (((symbol-function 'agent-claude-status-model)
             (lambda () "Opus 4.8"))
            ((symbol-function 'agent-claude-status-effort)
             (lambda () "high")))
    (with-temp-buffer
      (agent--set-session (current-buffer)
                          (agent-session-create :backend 'claude-code
                                                :account nil
                                                :directory "~/project/"))
      (should (equal (doom-modeline-extras--format-claude-status-fields)
                     " | Opus 4.8 | high")))))

(ert-deftest doom-modeline-extras-test-format-state-summary-escapes-percent ()
  "Show the latest published status with percent signs escaped."
  (with-temp-buffer
    (setq-local agent--session-published-status "Coverage at 80% now")
    (should (equal (doom-modeline-extras--format-state-summary)
                   " | Coverage at 80%% now"))))

(ert-deftest doom-modeline-extras-test-format-state-summary-absent ()
  "Show nothing when the agent has published no status."
  (with-temp-buffer
    (should-not (doom-modeline-extras--format-state-summary))))

(provide 'doom-modeline-extras-test)
;;; doom-modeline-extras-test.el ends here
