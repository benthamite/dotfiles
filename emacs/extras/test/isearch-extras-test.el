;;; isearch-extras-test.el --- Tests for isearch-extras -*- lexical-binding: t -*-

;; Tests for isearch advice function
;; in isearch-extras.el.

;;; Code:

(require 'ert)
(require 'isearch-extras)

;;;; Use selection

(ert-deftest isearch-extras-test-use-selection-no-region ()
  "Use-selection calls orig-fun directly when no region is active."
  (with-temp-buffer
    (insert "hello world")
    (let ((called-with nil))
      (isearch-extras-use-selection
       (lambda (&rest args) (setq called-with args))
       t)
      (should (equal called-with '(t))))))

(ert-deftest isearch-extras-test-use-selection-passes-args ()
  "Use-selection passes all args to orig-fun when no region."
  (with-temp-buffer
    (insert "hello world")
    (let ((called-with nil))
      (isearch-extras-use-selection
       (lambda (&rest args) (setq called-with args))
       nil "extra")
      (should (equal called-with '(nil "extra"))))))

(provide 'isearch-extras-test)
;;; isearch-extras-test.el ends here
