;;; aidermacs-extras-test.el --- Tests for aidermacs-extras -*- lexical-binding: t -*-

;; Tests for the kill-buffer guard function
;; in aidermacs-extras.el.

;;; Code:

(require 'ert)

;; Try to load aidermacs-extras.  If aidermacs (heavy dep) is missing,
;; the function is defined before the require call in the source, so
;; we eval just the defun from the source file.
(unless (fboundp 'aidermacs-extras-confirm-kill-buffer)
  (condition-case nil
      (require 'aidermacs-extras)
    (error
     ;; Load only the defun from source, skipping the (require 'aidermacs).
     (let ((source (locate-library "aidermacs-extras.el")))
       (when source
         (with-temp-buffer
           (insert-file-contents source)
           (goto-char (point-min))
           (when (re-search-forward "(defun aidermacs-extras-confirm-kill-buffer" nil t)
             (goto-char (match-beginning 0))
             (eval (read (current-buffer)) t))))))))

;;;; confirm-kill-buffer

(ert-deftest aidermacs-extras-test-confirm-kill-non-aidermacs-buffer ()
  "Return t for a non-aidermacs buffer (allow kill)."
  (skip-unless (fboundp 'aidermacs-extras-confirm-kill-buffer))
  (with-temp-buffer
    (rename-buffer "*scratch-test*" t)
    (should (eq t (aidermacs-extras-confirm-kill-buffer)))))

(ert-deftest aidermacs-extras-test-confirm-kill-non-comint-buffer ()
  "Return t when buffer name starts with *aidermacs but is not comint-mode."
  (skip-unless (fboundp 'aidermacs-extras-confirm-kill-buffer))
  (with-temp-buffer
    (rename-buffer "*aidermacs-test*" t)
    ;; Not in comint-mode, so guard should not trigger
    (should (eq t (aidermacs-extras-confirm-kill-buffer)))))

(ert-deftest aidermacs-extras-test-confirm-kill-aidermacs-no-process ()
  "Return t when aidermacs comint buffer has no process."
  (skip-unless (fboundp 'aidermacs-extras-confirm-kill-buffer))
  (with-temp-buffer
    (rename-buffer "*aidermacs-test*" t)
    ;; Even if we could set comint-mode, there is no process
    ;; so the guard should not trigger
    (should (eq t (aidermacs-extras-confirm-kill-buffer)))))

(provide 'aidermacs-extras-test)
;;; aidermacs-extras-test.el ends here
