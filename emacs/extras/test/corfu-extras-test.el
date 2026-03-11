;;; corfu-extras-test.el --- Tests for corfu-extras -*- lexical-binding: t -*-

;; Tests for completion helpers in corfu-extras.el.

;;; Code:

(require 'ert)
(require 'corfu-extras)

;;;; corfu-continue-commands membership

(ert-deftest corfu-extras-test-move-to-minibuffer-in-continue-commands ()
  "Function `corfu-extras-move-to-minibuffer' is in `corfu-continue-commands'."
  (should (memq 'corfu-extras-move-to-minibuffer corfu-continue-commands)))

;;;; enable-always-in-minibuffer

(ert-deftest corfu-extras-test-enable-in-minibuffer-calls-corfu-mode-when-vertico-unbound ()
  "When `vertico--input' is unbound, `corfu-mode' is called."
  (let ((corfu-mode-called nil))
    (cl-letf (((symbol-function 'corfu-mode)
               (lambda (&rest _) (setq corfu-mode-called t))))
      (when (boundp 'vertico--input)
        (makunbound 'vertico--input))
      (corfu-extras-enable-always-in-minibuffer)
      (should corfu-mode-called))))

(ert-deftest corfu-extras-test-enable-in-minibuffer-calls-corfu-mode-when-vertico-nil ()
  "When `vertico--input' is bound but nil, `corfu-mode' is called."
  (let ((corfu-mode-called nil))
    (defvar vertico--input)
    (let ((vertico--input nil))
      (cl-letf (((symbol-function 'corfu-mode)
                 (lambda (&rest _) (setq corfu-mode-called t))))
        (corfu-extras-enable-always-in-minibuffer)
        (should corfu-mode-called)))))

(ert-deftest corfu-extras-test-enable-in-minibuffer-skips-when-vertico-active ()
  "When `vertico--input' is bound and truthy, `corfu-mode' is NOT called."
  (let ((corfu-mode-called nil))
    (defvar vertico--input)
    (let ((vertico--input t))
      (cl-letf (((symbol-function 'corfu-mode)
                 (lambda (&rest _) (setq corfu-mode-called t))))
        (corfu-extras-enable-always-in-minibuffer)
        (should-not corfu-mode-called)))))

;;;; Interactivity

(ert-deftest corfu-extras-test-move-to-minibuffer-is-interactive ()
  "Function `corfu-extras-move-to-minibuffer' is interactive."
  (should (commandp #'corfu-extras-move-to-minibuffer)))

(ert-deftest corfu-extras-test-enable-always-is-not-interactive ()
  "Function `corfu-extras-enable-always-in-minibuffer' is not interactive."
  (should-not (commandp #'corfu-extras-enable-always-in-minibuffer)))

;;;; Functions are defined

(ert-deftest corfu-extras-test-functions-are-defined ()
  "All public corfu-extras functions are defined."
  (should (fboundp 'corfu-extras-move-to-minibuffer))
  (should (fboundp 'corfu-extras-enable-always-in-minibuffer)))

(provide 'corfu-extras-test)
;;; corfu-extras-test.el ends here
