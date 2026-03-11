;;; eat-extras-test.el --- Tests for eat-extras -*- lexical-binding: t -*-

;; Tests for terminal key-sending helpers in eat-extras.el.

;;; Code:

(require 'ert)
(require 'eat-extras)

;;;; Variables

(ert-deftest eat-extras-test-non-bound-keys-is-non-empty-list ()
  "Variable `eat-extras-non-bound-keys' is a non-empty list."
  (should (consp eat-extras-non-bound-keys))
  (should (> (length eat-extras-non-bound-keys) 0)))

(ert-deftest eat-extras-test-non-bound-keys-contains-vectors ()
  "Every element in `eat-extras-non-bound-keys' is a vector."
  (dolist (key eat-extras-non-bound-keys)
    (should (vectorp key))))

(ert-deftest eat-extras-test-emacs-passthrough-keys-is-non-empty-list ()
  "Variable `eat-extras-emacs-passthrough-keys' is a non-empty list."
  (should (consp eat-extras-emacs-passthrough-keys))
  (should (> (length eat-extras-emacs-passthrough-keys) 0)))

(ert-deftest eat-extras-test-emacs-passthrough-keys-contains-strings ()
  "Every element in `eat-extras-emacs-passthrough-keys' is a string."
  (dolist (key eat-extras-emacs-passthrough-keys)
    (should (stringp key))))

;;;; Send functions - escape sequence verification

(ert-deftest eat-extras-test-send-forward-word-escape ()
  "Sending forward-word delegates ESC-f to `eat-term-send-string'."
  (let ((captured-string nil))
    (cl-letf (((symbol-function 'eat-term-send-string)
               (lambda (_terminal str) (setq captured-string str))))
      (let ((eat-terminal 'dummy))
        (eat-extras-send-forward-word)
        (should (equal captured-string "\ef"))))))

(ert-deftest eat-extras-test-send-backward-word-escape ()
  "Sending backward-word delegates ESC-b to `eat-term-send-string'."
  (let ((captured-string nil))
    (cl-letf (((symbol-function 'eat-term-send-string)
               (lambda (_terminal str) (setq captured-string str))))
      (let ((eat-terminal 'dummy))
        (eat-extras-send-backward-word)
        (should (equal captured-string "\eb"))))))

(ert-deftest eat-extras-test-send-kill-word-escape ()
  "Sending kill-word delegates ESC-d to `eat-term-send-string'."
  (let ((captured-string nil))
    (cl-letf (((symbol-function 'eat-term-send-string)
               (lambda (_terminal str) (setq captured-string str))))
      (let ((eat-terminal 'dummy))
        (eat-extras-send-kill-word)
        (should (equal captured-string "\ed"))))))

(ert-deftest eat-extras-test-send-kill-line-escape ()
  "Sending kill-line delegates C-k to `eat-term-send-string'."
  (let ((captured-string nil))
    (cl-letf (((symbol-function 'eat-term-send-string)
               (lambda (_terminal str) (setq captured-string str))))
      (let ((eat-terminal 'dummy))
        (eat-extras-send-kill-line)
        (should (equal captured-string "\C-k"))))))

(ert-deftest eat-extras-test-send-backspace-escape ()
  "Sending backspace delegates DEL (0x7f) to `eat-term-send-string'."
  (let ((captured-string nil))
    (cl-letf (((symbol-function 'eat-term-send-string)
               (lambda (_terminal str) (setq captured-string str))))
      (let ((eat-terminal 'dummy))
        (eat-extras-send-backspace)
        (should (equal captured-string "\x7f"))))))

(ert-deftest eat-extras-test-send-meta-backspace-escape ()
  "Sending meta-backspace delegates ESC-DEL to `eat-term-send-string'."
  (let ((captured-string nil))
    (cl-letf (((symbol-function 'eat-term-send-string)
               (lambda (_terminal str) (setq captured-string str))))
      (let ((eat-terminal 'dummy))
        (eat-extras-send-meta-backspace)
        (should (equal captured-string "\e\x7f"))))))

(ert-deftest eat-extras-test-send-undo-escape ()
  "Sending undo delegates C-_ to `eat-term-send-string'."
  (let ((captured-string nil))
    (cl-letf (((symbol-function 'eat-term-send-string)
               (lambda (_terminal str) (setq captured-string str))))
      (let ((eat-terminal 'dummy))
        (eat-extras-send-undo)
        (should (equal captured-string "\C-_"))))))

;;;; Send functions - interactivity

(ert-deftest eat-extras-test-send-functions-are-interactive ()
  "All send functions are interactive commands."
  (should (commandp #'eat-extras-send-forward-word))
  (should (commandp #'eat-extras-send-backward-word))
  (should (commandp #'eat-extras-send-kill-word))
  (should (commandp #'eat-extras-send-kill-line))
  (should (commandp #'eat-extras-send-backspace))
  (should (commandp #'eat-extras-send-meta-backspace))
  (should (commandp #'eat-extras-send-undo)))

;;;; sync-semi-char-mode-map

(ert-deftest eat-extras-test-sync-semi-char-mode-map-updates-cdr ()
  "Syncing semi-char mode map replaces the cdr in `minor-mode-map-alist'."
  (let* ((old-map (make-sparse-keymap))
         (new-map (make-sparse-keymap))
         (minor-mode-map-alist (list (cons 'eat--semi-char-mode old-map)))
         (eat-semi-char-mode-map new-map))
    (eat-extras-sync-semi-char-mode-map)
    (should (eq (cdr (assq 'eat--semi-char-mode minor-mode-map-alist))
                new-map))))

(ert-deftest eat-extras-test-sync-semi-char-mode-map-no-entry ()
  "Syncing when no entry exists in alist does not signal an error."
  (let ((minor-mode-map-alist nil)
        (eat-semi-char-mode-map (make-sparse-keymap)))
    (eat-extras-sync-semi-char-mode-map)
    (should (null minor-mode-map-alist))))

(provide 'eat-extras-test)
;;; eat-extras-test.el ends here
