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

(provide 'modus-themes-extras-test)
;;; modus-themes-extras-test.el ends here
