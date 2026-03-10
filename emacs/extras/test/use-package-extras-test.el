;;; use-package-extras-test.el --- Tests for use-package-extras -*- lexical-binding: t -*-

;; Tests for use-feature and lambda! macros
;; in use-package-extras.el.

;;; Code:

(require 'ert)
(require 'use-package-extras)

;;;; use-feature macro

(ert-deftest use-package-extras-test-use-feature-expands-with-ensure-nil ()
  "Use-feature expands to use-package with :ensure nil."
  (let ((expansion (macroexpand-1 '(use-feature test-pkg :init (message "hi")))))
    (should (eq (car expansion) 'use-package))
    (should (eq (cadr expansion) 'test-pkg))
    (should (memq :ensure (cddr expansion)))
    (let ((ensure-pos (cl-position :ensure (cddr expansion))))
      (should (eq (nth (1+ ensure-pos) (cddr expansion)) nil)))))

(ert-deftest use-package-extras-test-use-feature-passes-args ()
  "Use-feature passes additional arguments to use-package."
  (let ((expansion (macroexpand-1 '(use-feature test-pkg :defer t))))
    (should (memq :defer (cddr expansion)))))

(ert-deftest use-package-extras-test-use-feature-no-extra-args ()
  "Use-feature works with no extra arguments."
  (let ((expansion (macroexpand-1 '(use-feature test-pkg))))
    (should (eq (car expansion) 'use-package))
    (should (eq (cadr expansion) 'test-pkg))
    (should (memq :ensure (cddr expansion)))))

;;;; lambda! macro

(ert-deftest use-package-extras-test-lambda-bang-creates-interactive-lambda ()
  "Lambda! expands to an interactive lambda."
  (let ((expansion (macroexpand-1 '(lambda! (message "hi")))))
    (should (eq (car expansion) 'lambda))
    (should (equal (cadr expansion) '()))
    (should (equal (caddr expansion) '(interactive)))
    (should (equal (cadddr expansion) '(message "hi")))))

(ert-deftest use-package-extras-test-lambda-bang-callable ()
  "Lambda! produces a callable function."
  (let ((fn (lambda! (+ 1 2))))
    (should (functionp fn))
    (should (= (funcall fn) 3))))

(ert-deftest use-package-extras-test-lambda-bang-multiple-body-forms ()
  "Lambda! handles multiple body forms."
  (let ((fn (lambda! (setq test-var-1 1) (setq test-var-2 2) (+ test-var-1 test-var-2))))
    (should (functionp fn))
    (should (= (funcall fn) 3))))

(provide 'use-package-extras-test)
;;; use-package-extras-test.el ends here
