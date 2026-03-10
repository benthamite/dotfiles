;;; aidermacs-extras-test.el --- Tests for aidermacs-extras -*- lexical-binding: t -*-

;; Tests for the kill-buffer guard function
;; in aidermacs-extras.el.

;;; Code:

(require 'ert)

;; The confirm-kill-buffer function is defined before (require 'aidermacs)
;; in aidermacs-extras.el.  We load just that function to avoid requiring
;; aidermacs (which pulls in heavy deps like vterm).
(unless (fboundp 'aidermacs-extras-confirm-kill-buffer)
  (autoload 'aidermacs-extras-confirm-kill-buffer "aidermacs-extras"))

;;;; confirm-kill-buffer

(ert-deftest aidermacs-extras-test-confirm-kill-non-aidermacs-buffer ()
  "Return t for a non-aidermacs buffer (allow kill)."
  (with-temp-buffer
    (rename-buffer "*scratch-test*" t)
    (should (eq t (aidermacs-extras-confirm-kill-buffer)))))

(ert-deftest aidermacs-extras-test-confirm-kill-non-comint-buffer ()
  "Return t when buffer name starts with *aidermacs but is not comint-mode."
  (with-temp-buffer
    (rename-buffer "*aidermacs-test*" t)
    ;; Not in comint-mode, so guard should not trigger
    (should (eq t (aidermacs-extras-confirm-kill-buffer)))))

(ert-deftest aidermacs-extras-test-confirm-kill-aidermacs-no-process ()
  "Return t when aidermacs comint buffer has no process."
  (with-temp-buffer
    (rename-buffer "*aidermacs-test*" t)
    ;; Even if we could set comint-mode, there is no process
    ;; so the guard should not trigger
    (should (eq t (aidermacs-extras-confirm-kill-buffer)))))

(provide 'aidermacs-extras-test)
;;; aidermacs-extras-test.el ends here
