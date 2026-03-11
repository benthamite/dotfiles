;;; paths-test.el --- Tests for paths -*- lexical-binding: t -*-

;; Tests for path variable definitions in paths.el.

;;; Code:

(require 'ert)
(require 'paths)

;;;; Package loading

(ert-deftest paths-test-loads-without-error ()
  "The paths package loads without error."
  (should (featurep 'paths)))

;;;; Directory variables

(ert-deftest paths-test-dir-dotemacs-is-string ()
  "The dotemacs directory is a string."
  (should (stringp paths-dir-dotemacs)))

(ert-deftest paths-test-dir-dotemacs-ends-with-slash ()
  "The dotemacs directory ends with a slash."
  (should (string-suffix-p "/" paths-dir-dotemacs)))

(ert-deftest paths-test-dir-dotemacs-resolves-from-home ()
  "The dotemacs directory starts from HOME."
  (should (string-prefix-p (getenv "HOME") paths-dir-dotemacs)))

(ert-deftest paths-test-dir-google-drive-contains-my-drive ()
  "The Google Drive directory contains \"My Drive\"."
  (should (string-match-p "My Drive" paths-dir-google-drive)))

(ert-deftest paths-test-dir-google-drive-ends-with-slash ()
  "The Google Drive directory ends with a slash."
  (should (string-suffix-p "/" paths-dir-google-drive)))

(ert-deftest paths-test-dir-pdf-library-derived-from-google-drive ()
  "The PDF library is derived from the Google Drive directory."
  (should (string-prefix-p paths-dir-google-drive paths-dir-pdf-library)))

(ert-deftest paths-test-dir-pdf-library-ends-with-slash ()
  "The PDF library directory ends with a slash."
  (should (string-suffix-p "/" paths-dir-pdf-library)))

(ert-deftest paths-test-dir-html-library-derived-from-google-drive ()
  "The HTML library is derived from the Google Drive directory."
  (should (string-prefix-p paths-dir-google-drive paths-dir-html-library)))

(ert-deftest paths-test-dir-html-library-ends-with-slash ()
  "The HTML library directory ends with a slash."
  (should (string-suffix-p "/" paths-dir-html-library)))

(ert-deftest paths-test-dir-downloads-ends-with-slash ()
  "The downloads directory ends with a slash."
  (should (string-suffix-p "/" paths-dir-downloads)))

(ert-deftest paths-test-dir-dotfiles-ends-with-slash ()
  "The dotfiles directory ends with a slash."
  (should (string-suffix-p "/" paths-dir-dotfiles)))

;;;; List variables

(ert-deftest paths-test-dir-all-repos-is-list ()
  "The all repos variable is a list."
  (should (listp paths-dir-all-repos)))

(ert-deftest paths-test-dir-all-repos-contains-strings ()
  "Each entry in the all repos list is a string."
  (dolist (repo paths-dir-all-repos)
    (should (stringp repo))))

(ert-deftest paths-test-dir-all-repos-nonempty ()
  "The all repos list is not empty."
  (should (> (length paths-dir-all-repos) 0)))

;;;; File variables

(ert-deftest paths-test-file-config-ends-with-org ()
  "The config file ends with .org."
  (should (string-suffix-p ".org" paths-file-config)))

(ert-deftest paths-test-file-ledger-ends-with-ledger ()
  "The ledger file ends with .ledger."
  (should (string-suffix-p ".ledger" paths-file-ledger)))

(ert-deftest paths-test-file-ledger-db-ends-with-pricedb ()
  "The ledger database file ends with .pricedb."
  (should (string-suffix-p ".pricedb" paths-file-ledger-db)))

(ert-deftest paths-test-file-notes-ends-with-org ()
  "The notes file ends with .org."
  (should (string-suffix-p ".org" paths-file-notes)))

(ert-deftest paths-test-file-karabiner-edn-ends-with-edn ()
  "The Karabiner edn file ends with .edn."
  (should (string-suffix-p ".edn" paths-file-karabiner-edn)))

(ert-deftest paths-test-file-init-tangle-flags-ends-with-el ()
  "The tangle flags file ends with .el."
  (should (string-suffix-p ".el" paths-file-init-tangle-flags-sans-directory)))

(provide 'paths-test)
;;; paths-test.el ends here
