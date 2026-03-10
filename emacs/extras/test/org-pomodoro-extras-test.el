;;; org-pomodoro-extras-test.el --- Tests for org-pomodoro-extras -*- lexical-binding: t -*-

;; Tests for timer formatting in org-pomodoro-extras.el.

;;; Code:

(require 'ert)
(require 'org-pomodoro-extras)

;;;; format-timer

(ert-deftest org-pomodoro-extras-test-format-timer-zero-count ()
  "Format timer with zero pomodoro count."
  (let ((org-pomodoro-count 0)
        (org-pomodoro-format nil))
    (org-extras-pomodoro-format-timer)
    (should (string-match-p "%s" org-pomodoro-format))
    (should (string-match-p "|0" org-pomodoro-format))))

(ert-deftest org-pomodoro-extras-test-format-timer-positive-count ()
  "Format timer with a positive pomodoro count."
  (let ((org-pomodoro-count 5)
        (org-pomodoro-format nil))
    (org-extras-pomodoro-format-timer)
    (should (string-match-p "|5" org-pomodoro-format))))

(ert-deftest org-pomodoro-extras-test-format-timer-large-count ()
  "Format timer with a large pomodoro count."
  (let ((org-pomodoro-count 42)
        (org-pomodoro-format nil))
    (org-extras-pomodoro-format-timer)
    (should (string-match-p "|42" org-pomodoro-format))))

(ert-deftest org-pomodoro-extras-test-format-timer-contains-tomato ()
  "Format timer includes the tomato emoji."
  (let ((org-pomodoro-count 1)
        (org-pomodoro-format nil))
    (org-extras-pomodoro-format-timer)
    (should (string-prefix-p "\U0001F345" org-pomodoro-format))))

(provide 'org-pomodoro-extras-test)
;;; org-pomodoro-extras-test.el ends here
