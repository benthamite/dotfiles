;;; ace-link-extras-test.el --- Tests for ace-link-extras -*- lexical-binding: t -*-

;; Tests for ace-link-extras.el, focusing on the patched
;; `ace-link--mu4e-action' function.

;;; Code:

(require 'ert)
(require 'ace-link-extras)

;;;; ace-link--mu4e-action

(ert-deftest ace-link-extras-test-mu4e-action-no-properties ()
  "Calling `ace-link--mu4e-action' on text without link properties is a no-op."
  (with-temp-buffer
    (insert "plain text")
    (should-not (ace-link--mu4e-action 1))))

(ert-deftest ace-link-extras-test-mu4e-action-non-number ()
  "Calling `ace-link--mu4e-action' with a non-number argument is a no-op."
  (should-not (ace-link--mu4e-action nil))
  (should-not (ace-link--mu4e-action t)))

(ert-deftest ace-link-extras-test-mu4e-action-shr-url ()
  "Links with `shr-url' property dispatch to `shr-browse-url'."
  (with-temp-buffer
    (insert (propertize "click here" 'shr-url "https://example.com"))
    (let ((called nil))
      (cl-letf (((symbol-function 'shr-browse-url)
                 (lambda (&rest _) (setq called t))))
        (ace-link--mu4e-action 1)
        (should called)))))

(ert-deftest ace-link-extras-test-mu4e-action-mu4e-url ()
  "Links with `mu4e-url' property dispatch to `mu4e--view-browse-url-from-binding'."
  (with-temp-buffer
    (insert (propertize "click here" 'mu4e-url "https://example.com"))
    (let ((called nil))
      (cl-letf (((symbol-function 'mu4e--view-browse-url-from-binding)
                 (lambda (&rest _) (setq called t))))
        (ace-link--mu4e-action 1)
        (should called)))))

(ert-deftest ace-link-extras-test-mu4e-action-shr-url-takes-precedence ()
  "When both `shr-url' and `mu4e-url' are present, `shr-url' wins."
  (with-temp-buffer
    (insert (propertize "click here" 'shr-url "https://shr.example.com"
                        'mu4e-url "https://mu4e.example.com"))
    (let ((shr-called nil)
          (mu4e-called nil))
      (cl-letf (((symbol-function 'shr-browse-url)
                 (lambda (&rest _) (setq shr-called t)))
                ((symbol-function 'mu4e--view-browse-url-from-binding)
                 (lambda (&rest _) (setq mu4e-called t))))
        (ace-link--mu4e-action 1)
        (should shr-called)
        (should-not mu4e-called)))))

;;;; ace-link-extras-mu4e dispatcher

(defvar mm-text-html-renderer)
(ert-deftest ace-link-extras-test-mu4e-dispatches-on-renderer ()
  "When `mm-text-html-renderer' is not w3m, delegates to `ace-link-mu4e'."
  (let ((mm-text-html-renderer 'shr)
        (called nil))
    (cl-letf (((symbol-function 'ace-link-mu4e)
               (lambda (&rest _) (setq called t))))
      (ace-link-extras-mu4e)
      (should called))))

(provide 'ace-link-extras-test)
;;; ace-link-extras-test.el ends here
