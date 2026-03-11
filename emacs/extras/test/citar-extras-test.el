;;; citar-extras-test.el --- Tests for citar-extras -*- lexical-binding: t -*-

;; Tests for citation management helpers in citar-extras.el.

;;; Code:

(require 'ert)
(require 'citar-extras)

;;;; Feature

(ert-deftest citar-extras-test-feature-provided ()
  "The `citar-extras' feature should be provided."
  (should (featurep 'citar-extras)))

;;;; Indicator variables

(ert-deftest citar-extras-test-indicator-files-icons-defined ()
  "The `citar-extras-indicator-files-icons' variable should be a `citar-indicator'."
  (should (boundp 'citar-extras-indicator-files-icons))
  (should (citar-indicator-p citar-extras-indicator-files-icons)))

(ert-deftest citar-extras-test-indicator-links-icons-defined ()
  "The `citar-extras-indicator-links-icons' variable should be a `citar-indicator'."
  (should (boundp 'citar-extras-indicator-links-icons))
  (should (citar-indicator-p citar-extras-indicator-links-icons)))

(ert-deftest citar-extras-test-indicator-notes-icons-defined ()
  "The `citar-extras-indicator-notes-icons' variable should be a `citar-indicator'."
  (should (boundp 'citar-extras-indicator-notes-icons))
  (should (citar-indicator-p citar-extras-indicator-notes-icons)))

(ert-deftest citar-extras-test-indicator-cited-icons-defined ()
  "The `citar-extras-indicator-cited-icons' variable should be a `citar-indicator'."
  (should (boundp 'citar-extras-indicator-cited-icons))
  (should (citar-indicator-p citar-extras-indicator-cited-icons)))

;;;; Function definitions

(ert-deftest citar-extras-test-open-in-ebib-defined ()
  "The `citar-extras-open-in-ebib' function should be defined."
  (should (fboundp 'citar-extras-open-in-ebib)))

(ert-deftest citar-extras-test-goto-bibtex-entry-defined ()
  "The `citar-extras-goto-bibtex-entry' function should be defined."
  (should (fboundp 'citar-extras-goto-bibtex-entry)))

(ert-deftest citar-extras-test-refresh-bibliography-defined ()
  "The `citar-extras-refresh-bibliography' function should be defined."
  (should (fboundp 'citar-extras-refresh-bibliography)))

(ert-deftest citar-extras-test-refresh-all-bibliographies-defined ()
  "The `citar-extras-refresh-all-bibliographies' function should be defined."
  (should (fboundp 'citar-extras-refresh-all-bibliographies)))

(provide 'citar-extras-test)
;;; citar-extras-test.el ends here
