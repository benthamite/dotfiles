;;; doom-modeline-extras-test.el --- Tests for doom-modeline-extras -*- lexical-binding: t -*-

;; Tests for pure formatting and threshold functions in doom-modeline-extras.el.

;;; Code:

(require 'ert)
(require 'agent)
(require 'doom-modeline-extras)

(defvar ghub-default-host-alist)

;;;; Humanize tokens

(ert-deftest doom-modeline-extras-test-humanize-tokens-millions ()
  "Humanize tokens formats numbers >= 1M with one decimal and M suffix."
  (should (equal (doom-modeline-extras--humanize-tokens 1000000) "1.0M"))
  (should (equal (doom-modeline-extras--humanize-tokens 1500000) "1.5M"))
  (should (equal (doom-modeline-extras--humanize-tokens 2300000) "2.3M"))
  (should (equal (doom-modeline-extras--humanize-tokens 10000000) "10.0M")))

(ert-deftest doom-modeline-extras-test-humanize-tokens-thousands ()
  "Humanize tokens formats numbers >= 1k but < 1M with one decimal and k suffix."
  (should (equal (doom-modeline-extras--humanize-tokens 1000) "1.0k"))
  (should (equal (doom-modeline-extras--humanize-tokens 1200) "1.2k"))
  (should (equal (doom-modeline-extras--humanize-tokens 45000) "45.0k"))
  (should (equal (doom-modeline-extras--humanize-tokens 999999) "1000.0k")))

(ert-deftest doom-modeline-extras-test-humanize-tokens-small ()
  "Humanize tokens formats numbers < 1k as plain integers."
  (should (equal (doom-modeline-extras--humanize-tokens 0) "0"))
  (should (equal (doom-modeline-extras--humanize-tokens 1) "1"))
  (should (equal (doom-modeline-extras--humanize-tokens 999) "999"))
  (should (equal (doom-modeline-extras--humanize-tokens 500) "500")))

(ert-deftest doom-modeline-extras-test-humanize-tokens-boundary ()
  "Humanize tokens handles exact boundary values."
  (should (equal (doom-modeline-extras--humanize-tokens 999) "999"))
  (should (equal (doom-modeline-extras--humanize-tokens 1000) "1.0k"))
  (should (equal (doom-modeline-extras--humanize-tokens 999999) "1000.0k"))
  (should (equal (doom-modeline-extras--humanize-tokens 1000000) "1.0M")))

;;;; Humanize duration

(ert-deftest doom-modeline-extras-test-humanize-duration-hours ()
  "Humanize duration formats times >= 1 hour as XhYm."
  (should (equal (doom-modeline-extras--humanize-duration 3600000) "1h0m"))
  (should (equal (doom-modeline-extras--humanize-duration 7380000) "2h3m"))
  (should (equal (doom-modeline-extras--humanize-duration 5400000) "1h30m")))

(ert-deftest doom-modeline-extras-test-humanize-duration-minutes ()
  "Humanize duration formats times >= 1 minute but < 1 hour as XmYs."
  (should (equal (doom-modeline-extras--humanize-duration 60000) "1m0s"))
  (should (equal (doom-modeline-extras--humanize-duration 90000) "1m30s"))
  (should (equal (doom-modeline-extras--humanize-duration 2700000) "45m0s")))

(ert-deftest doom-modeline-extras-test-humanize-duration-seconds ()
  "Humanize duration formats times < 1 minute as Xs."
  (should (equal (doom-modeline-extras--humanize-duration 1000) "1s"))
  (should (equal (doom-modeline-extras--humanize-duration 45000) "45s"))
  (should (equal (doom-modeline-extras--humanize-duration 59000) "59s")))

(ert-deftest doom-modeline-extras-test-humanize-duration-zero ()
  "Humanize duration formats 0 ms as 0s."
  (should (equal (doom-modeline-extras--humanize-duration 0) "0s")))

(ert-deftest doom-modeline-extras-test-humanize-duration-sub-second ()
  "Humanize duration formats sub-second durations as 0s."
  (should (equal (doom-modeline-extras--humanize-duration 500) "0s"))
  (should (equal (doom-modeline-extras--humanize-duration 999) "0s")))

;;;; Context face

(ert-deftest doom-modeline-extras-test-context-face-urgent ()
  "Context face returns urgent for percentage >= 80."
  (should (eq (doom-modeline-extras--context-face 80) 'doom-modeline-urgent))
  (should (eq (doom-modeline-extras--context-face 95) 'doom-modeline-urgent))
  (should (eq (doom-modeline-extras--context-face 100) 'doom-modeline-urgent)))

(ert-deftest doom-modeline-extras-test-context-face-warning ()
  "Context face returns warning for percentage >= 60 and < 80."
  (should (eq (doom-modeline-extras--context-face 60) 'doom-modeline-warning))
  (should (eq (doom-modeline-extras--context-face 70) 'doom-modeline-warning))
  (should (eq (doom-modeline-extras--context-face 79) 'doom-modeline-warning)))

(ert-deftest doom-modeline-extras-test-context-face-info ()
  "Context face returns info for percentage < 60."
  (should (eq (doom-modeline-extras--context-face 0) 'doom-modeline-info))
  (should (eq (doom-modeline-extras--context-face 30) 'doom-modeline-info))
  (should (eq (doom-modeline-extras--context-face 59) 'doom-modeline-info)))

;;;; Cache face

(ert-deftest doom-modeline-extras-test-cache-face-info ()
  "Cache face returns info for percentage >= 80."
  (should (eq (doom-modeline-extras--cache-face 80) 'doom-modeline-info))
  (should (eq (doom-modeline-extras--cache-face 95) 'doom-modeline-info))
  (should (eq (doom-modeline-extras--cache-face 100) 'doom-modeline-info)))

(ert-deftest doom-modeline-extras-test-cache-face-warning ()
  "Cache face returns warning for percentage >= 50 and < 80."
  (should (eq (doom-modeline-extras--cache-face 50) 'doom-modeline-warning))
  (should (eq (doom-modeline-extras--cache-face 65) 'doom-modeline-warning))
  (should (eq (doom-modeline-extras--cache-face 79) 'doom-modeline-warning)))

(ert-deftest doom-modeline-extras-test-cache-face-urgent ()
  "Cache face returns urgent for percentage < 50."
  (should (eq (doom-modeline-extras--cache-face 0) 'doom-modeline-urgent))
  (should (eq (doom-modeline-extras--cache-face 25) 'doom-modeline-urgent))
  (should (eq (doom-modeline-extras--cache-face 49) 'doom-modeline-urgent)))

;;;; Usage deviation face

(ert-deftest doom-modeline-extras-test-usage-deviation-face-urgent ()
  "Deviation face returns urgent for positive deviation (over budget)."
  (should (eq (doom-modeline-extras--usage-deviation-face 1) 'doom-modeline-urgent))
  (should (eq (doom-modeline-extras--usage-deviation-face 50) 'doom-modeline-urgent))
  (should (eq (doom-modeline-extras--usage-deviation-face 100) 'doom-modeline-urgent)))

(ert-deftest doom-modeline-extras-test-usage-deviation-face-info ()
  "Deviation face returns info for zero or negative deviation (under budget)."
  (should (eq (doom-modeline-extras--usage-deviation-face 0) 'doom-modeline-info))
  (should (eq (doom-modeline-extras--usage-deviation-face -25) 'doom-modeline-info))
  (should (eq (doom-modeline-extras--usage-deviation-face -100) 'doom-modeline-info)))

;;;; Humanize reset

(ert-deftest doom-modeline-extras-test-humanize-reset-nil ()
  "Humanize reset returns nil for nil input."
  (should-not (doom-modeline-extras--humanize-reset nil)))

(ert-deftest doom-modeline-extras-test-humanize-reset-invalid ()
  "Humanize reset returns nil for invalid input."
  (should-not (doom-modeline-extras--humanize-reset "not-a-date")))

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
        (doom-modeline-extras--github-fetch-active nil)
        (orig-require (symbol-function 'require))
        captured-finish
        captured-prompt
        (start-count 0))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &optional filename noerror)
                 (funcall orig-require feature filename noerror)))
              ((symbol-function 'async-start)
               (lambda (_start finish)
                 (cl-incf start-count)
                 (setq captured-finish finish
                       captured-prompt async-prompt-for-password)
                 'process)))
      (doom-modeline-extras--github-fetch-notifications)
      (doom-modeline-extras--github-fetch-notifications)
      (should (= start-count 1))
      (should-not captured-prompt)
      (funcall captured-finish 7)
      (should-not doom-modeline-extras--github-fetch-active))))

(ert-deftest doom-modeline-extras-test-format-codex-status-fields-shows-effort ()
  "Format Codex modeline fields with model and reasoning effort."
  (cl-letf (((symbol-function 'agent-codex-status-model)
             (lambda () "gpt-5.5"))
            ((symbol-function 'agent-codex-status-effort)
             (lambda () "high"))
            ((symbol-function 'agent-codex-status-duration-ms)
             (lambda () nil)))
    (with-temp-buffer
      (agent--set-session (current-buffer)
                          (agent-session-create :backend 'codex
                                                :account "epoch"
                                                :directory "~/project/"))
      (should (equal (doom-modeline-extras--format-codex-status-fields)
                     " | gpt-5.5 | high | epoch")))))

(ert-deftest doom-modeline-extras-test-format-claude-status-fields-shows-effort ()
  "Format Claude modeline fields with model and effort."
  (let ((doom-modeline-extras-claude-code-usage nil)
        (doom-modeline-extras-claude-code-tokens nil)
        (doom-modeline-extras-claude-code-lines-changed nil)
        (doom-modeline-extras-claude-code-cache-efficiency nil))
    (cl-letf (((symbol-function 'agent-claude-status-model)
               (lambda () "Opus 4.8"))
              ((symbol-function 'agent-claude-status-effort)
               (lambda () "high"))
              ((symbol-function 'agent-claude-status-session-usage)
               (lambda () nil))
              ((symbol-function 'agent-claude-status-weekly-usage)
               (lambda () nil))
              ((symbol-function 'agent-claude-status-session-reset)
               (lambda () nil))
              ((symbol-function 'agent-claude-status-weekly-reset)
               (lambda () nil))
              ((symbol-function 'agent-claude-status-token-count)
               (lambda () nil))
              ((symbol-function 'agent-claude-status-cost)
               (lambda () nil))
              ((symbol-function 'agent-claude-status-context-percent)
               (lambda () nil))
              ((symbol-function 'agent-claude-status-lines-added)
               (lambda () nil))
              ((symbol-function 'agent-claude-status-lines-removed)
               (lambda () nil))
              ((symbol-function 'agent-claude-status-duration-ms)
               (lambda () nil))
              ((symbol-function 'agent-claude-status-cache-read-tokens)
               (lambda () nil))
              ((symbol-function 'agent-claude-status-cache-total-tokens)
               (lambda () nil)))
      (with-temp-buffer
        (agent--set-session (current-buffer)
                            (agent-session-create :backend 'claude-code
                                                  :account nil
                                                  :directory "~/project/"))
        (should (equal (doom-modeline-extras--format-claude-status-fields)
                       " | Opus 4.8 | high"))))))

(provide 'doom-modeline-extras-test)
;;; doom-modeline-extras-test.el ends here
