;;; consult-extras-test.el --- Tests for consult-extras -*- lexical-binding: t -*-

;; Tests for the ripgrep multiline toggle function
;; in consult-extras.el.

;;; Code:

(require 'ert)
(require 'consult-extras)

;;;; Toggle multiline

(ert-deftest consult-extras-test-toggle-multiline-adds-flags ()
  "Toggle-multiline appends multiline flags when absent."
  (let ((consult-ripgrep-args "rg --null --line-buffered"))
    (consult-extras-toggle-multiline)
    (should (string-match-p "--multiline" consult-ripgrep-args))
    (should (string-match-p "--multiline-dotall" consult-ripgrep-args))))

(ert-deftest consult-extras-test-toggle-multiline-removes-flags ()
  "Toggle-multiline removes multiline flags when present."
  (let ((consult-ripgrep-args "rg --null --line-buffered --multiline --multiline-dotall"))
    (consult-extras-toggle-multiline)
    (should-not (string-match-p "--multiline" consult-ripgrep-args))))

(ert-deftest consult-extras-test-toggle-multiline-round-trip ()
  "Toggle-multiline round-trips: add then remove restores original."
  (let* ((original "rg --null --line-buffered")
         (consult-ripgrep-args original))
    (consult-extras-toggle-multiline) ; add
    (should (string-match-p "--multiline" consult-ripgrep-args))
    (consult-extras-toggle-multiline) ; remove
    (should (equal consult-ripgrep-args original))))

(provide 'consult-extras-test)
;;; consult-extras-test.el ends here
