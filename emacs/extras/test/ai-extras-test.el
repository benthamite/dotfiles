;;; ai-extras-test.el --- Tests for ai-extras -*- lexical-binding: t -*-

;; Tests for pure and near-pure helper functions in ai-extras.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ai-extras)

;;;; Theme sync

(ert-deftest ai-extras-test-sync-theme-dispatches-to-backends ()
  "Dispatch theme sync to all registered backend handlers."
  (let ((ai-extras-backends nil)
        (seen nil))
    (ai-extras-register-backend
     'one
     (list :sync-theme (lambda (theme) (push (cons 'one theme) seen))))
    (ai-extras-register-backend
     'two
     (list :sync-theme (lambda (theme) (push (cons 'two theme) seen))))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame param)
                 (when (eq param 'background-mode) 'dark))))
      (ai-extras--do-sync-theme t)
      (should (equal (sort seen (lambda (a b)
                                  (string< (symbol-name (car a))
                                           (symbol-name (car b)))))
                     '((one . "dark") (two . "dark")))))))

(ert-deftest ai-extras-test-sync-theme-before-start-respects-toggle ()
  "Do not sync immediately when `ai-extras-sync-theme' is disabled."
  (let ((ai-extras-sync-theme nil)
        (called nil))
    (cl-letf (((symbol-function 'ai-extras--do-sync-theme)
               (lambda () (setq called t))))
      (ai-extras-sync-theme-now)
      (should-not called))))

(provide 'ai-extras-test)
;;; ai-extras-test.el ends here
