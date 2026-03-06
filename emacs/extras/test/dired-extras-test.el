;;; dired-extras-test.el --- Tests for dired-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in dired-extras.el, including the
;; y-or-n-p advice and hide-details-mode toggle logic.

;;; Code:

(require 'ert)
(require 'dired-extras)

;;;; y-or-n-p-just-yes

(ert-deftest dired-extras-test-y-or-n-p-just-yes-kill-dired-buffer ()
  "Return t for prompts starting with \"Kill Dired buffer\"."
  (should (eq t (dired-extras-y-or-n-p-just-yes #'ignore "Kill Dired buffer `foo'?"))))

(ert-deftest dired-extras-test-y-or-n-p-just-yes-kill-buffer-of ()
  "Return t for prompts starting with \"Kill buffer of\"."
  (should (eq t (dired-extras-y-or-n-p-just-yes #'ignore "Kill buffer of /some/path?"))))

(ert-deftest dired-extras-test-y-or-n-p-just-yes-delegates-other-prompts ()
  "Delegate to the original function for non-Dired prompts."
  (let ((original-called nil))
    (dired-extras-y-or-n-p-just-yes
     (lambda (prompt)
       (setq original-called prompt)
       'original-result)
     "Save file?")
    (should (equal original-called "Save file?"))))

(ert-deftest dired-extras-test-y-or-n-p-just-yes-returns-original-result ()
  "Return the value from the original function for non-Dired prompts."
  (should (eq 'yes-result
              (dired-extras-y-or-n-p-just-yes
               (lambda (_) 'yes-result)
               "Continue?"))))

(ert-deftest dired-extras-test-y-or-n-p-just-yes-does-not-delegate-for-dired ()
  "Do not call the original function for Dired kill prompts."
  (let ((original-called nil))
    (dired-extras-y-or-n-p-just-yes
     (lambda (_) (setq original-called t))
     "Kill Dired buffer `downloads'?")
    (should-not original-called)))

(ert-deftest dired-extras-test-y-or-n-p-just-yes-partial-match ()
  "Delegate to original function when prompt merely contains but does not start with the keyword."
  (let ((original-called nil))
    (dired-extras-y-or-n-p-just-yes
     (lambda (_) (setq original-called t) nil)
     "Really Kill Dired buffer?")
    (should original-called)))

;;;; clean-up-after-deletion-quietly

(ert-deftest dired-extras-test-clean-up-after-deletion-quietly-auto-answers-dired ()
  "During the wrapped call, y-or-n-p auto-answers t for Dired kill prompts."
  (let ((dired-prompt-result nil))
    (dired-extras-clean-up-after-deletion-quietly
     (lambda ()
       (setq dired-prompt-result (y-or-n-p "Kill Dired buffer `test'?"))))
    (should (eq dired-prompt-result t))))

(ert-deftest dired-extras-test-clean-up-after-deletion-quietly-removes-advice-on-error ()
  "The quiet wrapper removes advice even if the wrapped function signals an error."
  (ignore-errors
    (dired-extras-clean-up-after-deletion-quietly
     (lambda (&rest _) (error "Test error"))))
  ;; After the error, y-or-n-p should no longer be advised, so a
  ;; non-Dired prompt would delegate to the real y-or-n-p (we cannot
  ;; easily test that without a tty, so we just verify no error)
  (should-not (advice-member-p #'dired-extras-y-or-n-p-just-yes 'y-or-n-p)))

(provide 'dired-extras-test)
;;; dired-extras-test.el ends here
