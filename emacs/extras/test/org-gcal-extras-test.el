;;; org-gcal-extras-test.el --- Tests for org-gcal-extras -*- lexical-binding: t -*-

;; Tests for calendar sync helpers in org-gcal-extras.el.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; org-gcal may not be available in CI; skip all tests if so.
(defvar org-gcal-extras-test--loadable
  (condition-case nil
      (progn (require 'org-gcal-extras) t)
    (error nil))
  "Non-nil when org-gcal-extras loaded successfully.")

;;;; inhibit-modification-hooks

(ert-deftest org-gcal-extras-test-inhibit-hooks-calls-orig-fun ()
  "Advice wrapper calls orig-fun with the provided args."
  (skip-unless org-gcal-extras-test--loadable)
  (let (called-with)
    (cl-letf (((symbol-function 'org-gcal-extras--reset-element-cache)
               #'ignore))
      (org-gcal-extras--inhibit-modification-hooks
       (lambda (&rest args) (setq called-with args) 'ok)
       'arg1 'arg2))
    (should (equal '(arg1 arg2) called-with))))

(ert-deftest org-gcal-extras-test-inhibit-hooks-sets-inhibit-t ()
  "Variable `inhibit-modification-hooks' is t inside the advice wrapper."
  (skip-unless org-gcal-extras-test--loadable)
  (let (hooks-value)
    (cl-letf (((symbol-function 'org-gcal-extras--reset-element-cache)
               #'ignore))
      (org-gcal-extras--inhibit-modification-hooks
       (lambda (&rest _args) (setq hooks-value inhibit-modification-hooks))))
    (should (eq t hooks-value))))

(ert-deftest org-gcal-extras-test-inhibit-hooks-passes-through-return ()
  "Return value of orig-fun is passed through by the advice wrapper."
  (skip-unless org-gcal-extras-test--loadable)
  (cl-letf (((symbol-function 'org-gcal-extras--reset-element-cache)
             #'ignore))
    (let ((result (org-gcal-extras--inhibit-modification-hooks
                   (lambda (&rest _args) 42))))
      (should (equal 42 result)))))

(ert-deftest org-gcal-extras-test-inhibit-hooks-passes-through-nil ()
  "A nil return value from orig-fun is passed through correctly."
  (skip-unless org-gcal-extras-test--loadable)
  (cl-letf (((symbol-function 'org-gcal-extras--reset-element-cache)
             #'ignore))
    (let ((result (org-gcal-extras--inhibit-modification-hooks
                   (lambda (&rest _args) nil))))
      (should (null result)))))

;;;; reset-element-cache

(ert-deftest org-gcal-extras-test-reset-cache-noop-outside-org-mode ()
  "Does nothing when not in `org-mode'."
  (skip-unless org-gcal-extras-test--loadable)
  (let (cache-reset-called)
    (cl-letf (((symbol-function 'org-element-cache-reset)
               (lambda () (setq cache-reset-called t))))
      (with-temp-buffer
        (fundamental-mode)
        (org-gcal-extras--reset-element-cache))
      (should-not cache-reset-called))))

(ert-deftest org-gcal-extras-test-reset-cache-noop-when-fboundp-missing ()
  "Does not error when `org-element--cache-active-p' is not available."
  (skip-unless org-gcal-extras-test--loadable)
  (let (cache-reset-called)
    (with-temp-buffer
      (org-mode)
      ;; Enter org-mode first, then set up mocks so org-mode init does
      ;; not interfere with our tracking flag.
      (let ((saved-fn (symbol-function 'org-element--cache-active-p)))
        (unwind-protect
            (progn
              (fmakunbound 'org-element--cache-active-p)
              (cl-letf (((symbol-function 'org-element-cache-reset)
                         (lambda () (setq cache-reset-called t))))
                (org-gcal-extras--reset-element-cache)))
          (fset 'org-element--cache-active-p saved-fn))))
    (should-not cache-reset-called)))

(ert-deftest org-gcal-extras-test-reset-cache-called-when-active ()
  "Calls `org-element-cache-reset' when cache is active in org-mode."
  (skip-unless org-gcal-extras-test--loadable)
  (let (cache-reset-called)
    (with-temp-buffer
      (org-mode)
      ;; Set up mocks after org-mode init to avoid false positives.
      (cl-letf (((symbol-function 'org-element--cache-active-p)
                 (lambda () t))
                ((symbol-function 'org-element-cache-reset)
                 (lambda () (setq cache-reset-called t))))
        (org-gcal-extras--reset-element-cache)))
    (should cache-reset-called)))

(ert-deftest org-gcal-extras-test-reset-cache-skipped-when-inactive ()
  "Does not call `org-element-cache-reset' when cache is inactive."
  (skip-unless org-gcal-extras-test--loadable)
  (let (cache-reset-called)
    (with-temp-buffer
      (org-mode)
      ;; Set up mocks after org-mode init to avoid false positives.
      (cl-letf (((symbol-function 'org-element--cache-active-p)
                 (lambda () nil))
                ((symbol-function 'org-element-cache-reset)
                 (lambda () (setq cache-reset-called t))))
        (org-gcal-extras--reset-element-cache)))
    (should-not cache-reset-called)))

(provide 'org-gcal-extras-test)
;;; org-gcal-extras-test.el ends here
