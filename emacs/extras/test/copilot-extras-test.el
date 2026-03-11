;;; copilot-extras-test.el --- Tests for copilot-extras -*- lexical-binding: t -*-

;; Tests for pure logic functions in copilot-extras.el.
;; copilot-extras depends on copilot, which is unavailable in CI,
;; so we extract and inline the pure logic for testing.

;;; Code:

(require 'ert)

;;;; Inlined pure logic

(defun copilot-extras-test--should-enable-p (buffer-read-only major-mode excluded-modes this-command)
  "Return non-nil when copilot should be enabled.
Checks BUFFER-READ-ONLY, MAJOR-MODE against EXCLUDED-MODES, and THIS-COMMAND."
  (not (or buffer-read-only
           (memq major-mode excluded-modes)
           (null this-command))))

;;;; should-enable-p

(ert-deftest copilot-extras-test-should-enable-nil-when-read-only ()
  "Return nil when buffer is read-only."
  (should-not (copilot-extras-test--should-enable-p
               t 'emacs-lisp-mode '() 'self-insert-command)))

(ert-deftest copilot-extras-test-should-enable-nil-when-excluded-mode ()
  "Return nil when major-mode is in the excluded list."
  (should-not (copilot-extras-test--should-enable-p
               nil 'org-mode '(org-mode text-mode) 'self-insert-command)))

(ert-deftest copilot-extras-test-should-enable-nil-when-no-command ()
  "Return nil when this-command is nil (non-interactive context)."
  (should-not (copilot-extras-test--should-enable-p
               nil 'emacs-lisp-mode '() nil)))

(ert-deftest copilot-extras-test-should-enable-t-when-all-pass ()
  "Return t when buffer is writable, mode is not excluded, and command is non-nil."
  (should (copilot-extras-test--should-enable-p
           nil 'emacs-lisp-mode '(org-mode) 'self-insert-command)))

(ert-deftest copilot-extras-test-should-enable-t-with-empty-excluded ()
  "Return t when excluded list is empty."
  (should (copilot-extras-test--should-enable-p
           nil 'python-mode '() 'self-insert-command)))

(ert-deftest copilot-extras-test-should-enable-nil-multiple-exclusions ()
  "Return nil when mode matches any entry in a multi-mode excluded list."
  (should-not (copilot-extras-test--should-enable-p
               nil 'text-mode '(org-mode text-mode special-mode) 'self-insert-command)))

(ert-deftest copilot-extras-test-should-enable-t-mode-not-in-excluded ()
  "Return t when mode is not in the excluded list even with multiple entries."
  (should (copilot-extras-test--should-enable-p
           nil 'rust-mode '(org-mode text-mode special-mode) 'self-insert-command)))

(provide 'copilot-extras-test)
;;; copilot-extras-test.el ends here
