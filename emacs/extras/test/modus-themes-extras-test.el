;;; modus-themes-extras-test.el --- Tests for modus-themes-extras -*- lexical-binding: t -*-

;; Tests for theme toggle logic in modus-themes-extras.el.

;;; Code:

(require 'ert)
(require 'modus-themes-extras)

;;;; toggle

(ert-deftest modus-themes-extras-test-toggle-from-light-to-dark ()
  "Toggle calls load-theme with dark theme when light is active."
  (let ((modus-themes-extras-light-theme 'modus-operandi)
        (modus-themes-extras-dark-theme 'modus-vivendi)
        (custom-enabled-themes '(modus-operandi))
        (loaded-theme nil))
    (cl-letf (((symbol-function 'modus-themes-load-theme)
               (lambda (theme) (setq loaded-theme theme))))
      (modus-themes-extras-toggle)
      (should (eq loaded-theme 'modus-vivendi)))))

(ert-deftest modus-themes-extras-test-toggle-from-dark-to-light ()
  "Toggle calls load-theme with light theme when dark is active."
  (let ((modus-themes-extras-light-theme 'modus-operandi)
        (modus-themes-extras-dark-theme 'modus-vivendi)
        (custom-enabled-themes '(modus-vivendi))
        (loaded-theme nil))
    (cl-letf (((symbol-function 'modus-themes-load-theme)
               (lambda (theme) (setq loaded-theme theme))))
      (modus-themes-extras-toggle)
      (should (eq loaded-theme 'modus-operandi)))))

(ert-deftest modus-themes-extras-test-toggle-no-theme-loads-light ()
  "Toggle loads light theme when no theme is active."
  (let ((modus-themes-extras-light-theme 'modus-operandi)
        (modus-themes-extras-dark-theme 'modus-vivendi)
        (custom-enabled-themes nil)
        (loaded-theme nil))
    (cl-letf (((symbol-function 'modus-themes-load-theme)
               (lambda (theme) (setq loaded-theme theme))))
      (modus-themes-extras-toggle)
      (should (eq loaded-theme 'modus-operandi)))))

;;;; load-theme-emacs-plus

(ert-deftest modus-themes-extras-test-load-theme-emacs-plus-light ()
  "Load light theme for light appearance."
  (let ((modus-themes-extras-light-theme 'modus-operandi)
        (modus-themes-extras-dark-theme 'modus-vivendi)
        (loaded-theme nil))
    (cl-letf (((symbol-function 'modus-themes-load-theme)
               (lambda (theme) (setq loaded-theme theme))))
      (modus-themes-extras-load-theme-emacs-plus 'light)
      (should (eq loaded-theme 'modus-operandi)))))

(ert-deftest modus-themes-extras-test-load-theme-emacs-plus-dark ()
  "Load dark theme for dark appearance."
  (let ((modus-themes-extras-light-theme 'modus-operandi)
        (modus-themes-extras-dark-theme 'modus-vivendi)
        (loaded-theme nil))
    (cl-letf (((symbol-function 'modus-themes-load-theme)
               (lambda (theme) (setq loaded-theme theme))))
      (modus-themes-extras-load-theme-emacs-plus 'dark)
      (should (eq loaded-theme 'modus-vivendi)))))

;;;; follow-appearance-emacs-plus

;; Only Emacs Plus defines these variables.  Elsewhere a `let' of a variable
;; unknown to this file would bind it lexically, out of reach of the code
;; under test.
(defvar ns-system-appearance)
(defvar ns-system-appearance-change-functions)

(ert-deftest modus-themes-extras-test-apply-appearance-change-loads-once ()
  "Load the matching theme on a change and nothing while it stays the same."
  (let ((modus-themes-extras-light-theme 'modus-operandi)
        (modus-themes-extras-dark-theme 'modus-vivendi)
        (modus-themes-extras--applied-appearance 'dark)
        (ns-system-appearance 'light)
        (loaded nil))
    (cl-letf (((symbol-function 'modus-themes-load-theme)
               (lambda (theme) (push theme loaded))))
      (modus-themes-extras--apply-appearance-change)
      (modus-themes-extras--apply-appearance-change)
      (should (equal loaded '(modus-operandi)))
      (setq ns-system-appearance 'dark)
      (modus-themes-extras--apply-appearance-change)
      (should (equal loaded '(modus-vivendi modus-operandi))))))

(ert-deftest modus-themes-extras-test-follow-appearance-keeps-hook-empty ()
  "Following the appearance removes the hook function and starts one timer."
  (let ((ns-system-appearance-change-functions
         (list #'modus-themes-extras-load-theme-emacs-plus))
        (ns-system-appearance 'dark)
        (modus-themes-extras--appearance-timer nil)
        (modus-themes-extras--applied-appearance nil)
        (loaded nil))
    (cl-letf (((symbol-function 'modus-themes-load-theme)
               (lambda (theme) (push theme loaded))))
      (unwind-protect
          (progn
            (modus-themes-extras-follow-appearance-emacs-plus)
            (let ((first modus-themes-extras--appearance-timer))
              (modus-themes-extras-follow-appearance-emacs-plus)
              (should-not (memq first timer-list)))
            (should (null ns-system-appearance-change-functions))
            (should (memq modus-themes-extras--appearance-timer timer-list))
            (should (equal loaded (list modus-themes-extras-dark-theme
                                        modus-themes-extras-dark-theme))))
        (when (timerp modus-themes-extras--appearance-timer)
          (cancel-timer modus-themes-extras--appearance-timer))))))

(provide 'modus-themes-extras-test)
;;; modus-themes-extras-test.el ends here
