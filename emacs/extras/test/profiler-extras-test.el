;;; profiler-extras-test.el --- Tests for profiler-extras -*- lexical-binding: t -*-

;; Tests for profiler toggle state management
;; in profiler-extras.el.

;;; Code:

(require 'ert)
(require 'profiler-extras)

;;;; Toggle state

(ert-deftest profiler-extras-test-toggle-flips-state ()
  "Profiler-toggle flips the toggle variable."
  (let ((profiler-extras-profiler-toggle nil))
    (profiler-extras-profiler-toggle)
    (should profiler-extras-profiler-toggle)
    ;; Toggle again: now it should report + stop, and flip back
    (profiler-extras-profiler-toggle)
    (should-not profiler-extras-profiler-toggle)))

(ert-deftest profiler-extras-test-toggle-starts-profiler ()
  "Profiler-toggle starts the profiler when toggle is nil."
  (let ((profiler-extras-profiler-toggle nil))
    (profiler-extras-profiler-toggle)
    (should (profiler-running-p))
    ;; Clean up
    (profiler-stop)
    (setq profiler-extras-profiler-toggle nil)))

(provide 'profiler-extras-test)
;;; profiler-extras-test.el ends here
