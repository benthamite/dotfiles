;;; org-ref-extras-test.el --- Tests for org-ref-extras -*- lexical-binding: t -*-

;; Tests for org-ref patches in org-ref-extras.el.

;;; Code:

(require 'ert)
(require 'org-ref-extras)

;;;; Feature and function existence

(ert-deftest org-ref-extras-test-feature-provided ()
  "The `org-ref-extras' feature should be provided."
  (should (featurep 'org-ref-extras)))

(ert-deftest org-ref-extras-test-sort-bibtex-entry-defined ()
  "The patched `org-ref-sort-bibtex-entry' function should be defined."
  (should (fboundp 'org-ref-sort-bibtex-entry)))

(ert-deftest org-ref-extras-test-isbn-to-bibtex-defined ()
  "The patched `isbn-to-bibtex' function should be defined."
  (should (fboundp 'isbn-to-bibtex)))

(provide 'org-ref-extras-test)
;;; org-ref-extras-test.el ends here
