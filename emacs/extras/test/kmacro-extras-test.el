;;; kmacro-extras-test.el --- Tests for kmacro-extras -*- lexical-binding: t -*-

;; Tests for keyboard macro counter toggle between numeric and
;; alphabetical in kmacro-extras.el.

;;; Code:

(require 'ert)
(require 'kmacro-extras)

;;;; Counter toggle format

(ert-deftest kmacro-extras-test-toggle-to-alpha-format ()
  "Toggle from numeric sets format to %c."
  (let ((kmacro-counter-format "%d"))
    (kmacro-extras-counter-toggle-alpha-number)
    (should (equal kmacro-counter-format "%c"))))

(ert-deftest kmacro-extras-test-toggle-to-numeric-format ()
  "Toggle from alphabetical sets format to %d."
  (let ((kmacro-counter-format "%c"))
    (kmacro-extras-counter-toggle-alpha-number)
    (should (equal kmacro-counter-format "%d"))))

(ert-deftest kmacro-extras-test-toggle-format-round-trip ()
  "Toggle round-trips format back to %d after two toggles."
  (let ((kmacro-counter-format "%d"))
    (kmacro-extras-counter-toggle-alpha-number)
    (should (equal kmacro-counter-format "%c"))
    (kmacro-extras-counter-toggle-alpha-number)
    (should (equal kmacro-counter-format "%d"))))

(provide 'kmacro-extras-test)
;;; kmacro-extras-test.el ends here
