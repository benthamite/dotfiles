;;; vterm-extras-test.el --- Tests for vterm-extras -*- lexical-binding: t -*-

;; Tests for terminal key-sending helpers in vterm-extras.el.

;;; Code:

(require 'ert)
(require 'vterm-extras)

;;;; Variables

(ert-deftest vterm-extras-test-keymap-exceptions-is-non-empty-list ()
  "Variable `vterm-extras-keymap-exceptions' is a non-empty list."
  (should (consp vterm-extras-keymap-exceptions))
  (should (> (length vterm-extras-keymap-exceptions) 0)))

(ert-deftest vterm-extras-test-keymap-exceptions-contains-strings ()
  "Every element in `vterm-extras-keymap-exceptions' is a string."
  (dolist (key vterm-extras-keymap-exceptions)
    (should (stringp key))))

;;;; Send functions - argument delegation

(ert-deftest vterm-extras-test-send-forward-word-args ()
  "Sending forward-word delegates (\"f\" nil t nil) to `vterm-send-key'."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'vterm-send-key)
               (lambda (&rest args) (setq captured-args args))))
      (vterm-extras-send-forward-word)
      (should (equal captured-args '("f" nil t nil))))))

(ert-deftest vterm-extras-test-send-backward-word-args ()
  "Sending backward-word delegates (\"b\" nil t nil) to `vterm-send-key'."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'vterm-send-key)
               (lambda (&rest args) (setq captured-args args))))
      (vterm-extras-send-backward-word)
      (should (equal captured-args '("b" nil t nil))))))

(ert-deftest vterm-extras-test-send-kill-word-args ()
  "Sending kill-word delegates (\"d\" nil t nil) to `vterm-send-key'."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'vterm-send-key)
               (lambda (&rest args) (setq captured-args args))))
      (vterm-extras-send-kill-word)
      (should (equal captured-args '("d" nil t nil))))))

(ert-deftest vterm-extras-test-send-kill-line-args ()
  "Sending kill-line delegates (\"k\" nil nil t) to `vterm-send-key'."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'vterm-send-key)
               (lambda (&rest args) (setq captured-args args))))
      (vterm-extras-send-kill-line)
      (should (equal captured-args '("k" nil nil t))))))

;;;; Functions are defined

(ert-deftest vterm-extras-test-functions-are-defined ()
  "All public vterm-extras functions are defined."
  (should (fboundp 'vterm-extras-send-forward-word))
  (should (fboundp 'vterm-extras-send-backward-word))
  (should (fboundp 'vterm-extras-send-kill-word))
  (should (fboundp 'vterm-extras-send-kill-line))
  (should (fboundp 'vterm-extras-setup-keymap)))

;;;; Send functions - interactivity

(ert-deftest vterm-extras-test-send-functions-are-interactive ()
  "All send functions are interactive commands."
  (should (commandp #'vterm-extras-send-forward-word))
  (should (commandp #'vterm-extras-send-backward-word))
  (should (commandp #'vterm-extras-send-kill-word))
  (should (commandp #'vterm-extras-send-kill-line)))

(provide 'vterm-extras-test)
;;; vterm-extras-test.el ends here
