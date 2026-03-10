;;; abbrev-extras-test.el --- Tests for abbrev-extras -*- lexical-binding: t -*-

;; Tests for mode listing, abbrev table creation, and abbrev addition
;; in abbrev-extras.el.

;;; Code:

(require 'ert)
(require 'abbrev-extras)

;;;; Get sorted modes

(ert-deftest abbrev-extras-test-get-sorted-modes-returns-list ()
  "Get-sorted-modes returns a non-empty list."
  (let ((modes (abbrev-extras-get-sorted-modes)))
    (should (listp modes))
    (should (> (length modes) 0))))

(ert-deftest abbrev-extras-test-get-sorted-modes-contains-known-modes ()
  "Get-sorted-modes includes well-known modes."
  (let ((modes (abbrev-extras-get-sorted-modes)))
    (should (member "emacs-lisp-mode" modes))
    (should (member "fundamental-mode" modes))
    (should (member "text-mode" modes))))

(ert-deftest abbrev-extras-test-get-sorted-modes-sorted ()
  "Get-sorted-modes returns modes in alphabetical order."
  (let ((modes (abbrev-extras-get-sorted-modes)))
    (should (equal modes (sort (copy-sequence modes) #'string<)))))

(ert-deftest abbrev-extras-test-get-sorted-modes-all-end-in-mode ()
  "Get-sorted-modes returns only symbols ending in '-mode'."
  (let ((modes (abbrev-extras-get-sorted-modes)))
    (dolist (mode modes)
      (should (string-match-p "-mode\\'" mode)))))

(ert-deftest abbrev-extras-test-get-sorted-modes-all-strings ()
  "Get-sorted-modes returns strings, not symbols."
  (let ((modes (abbrev-extras-get-sorted-modes)))
    (dolist (mode modes)
      (should (stringp mode)))))

;;;; Maybe create abbrev table

(ert-deftest abbrev-extras-test-maybe-create-abbrev-table-new ()
  "Maybe-create-abbrev-table creates a new table when it does not exist."
  (let ((sym (make-symbol "test-nonexistent-mode-abbrev-table")))
    (abbrev-extras-maybe-create-abbrev-table sym)
    (should (boundp sym))
    (should (abbrev-table-p (symbol-value sym)))))

(ert-deftest abbrev-extras-test-maybe-create-abbrev-table-existing ()
  "Maybe-create-abbrev-table does not overwrite an existing table."
  (let ((sym (make-symbol "test-existing-mode-abbrev-table")))
    (define-abbrev-table sym '(("tst" "test-expansion")))
    (let ((original-table (symbol-value sym)))
      (abbrev-extras-maybe-create-abbrev-table sym)
      (should (eq (symbol-value sym) original-table))
      (should (abbrev-expansion "tst" (symbol-value sym))))))

;;;; Add abbrev to table

(ert-deftest abbrev-extras-test-add-abbrev-to-table ()
  "Add-abbrev-to-table defines the abbreviation."
  (let ((sym (make-symbol "test-add-abbrev-table")))
    (define-abbrev-table sym '())
    (abbrev-extras-add-abbrev-to-table "tst" sym "test-expansion")
    (should (equal (abbrev-expansion "tst" (symbol-value sym)) "test-expansion"))))

(ert-deftest abbrev-extras-test-add-abbrev-to-table-creates-if-needed ()
  "Add-abbrev-to-table creates the table if it does not exist."
  (let ((sym (make-symbol "test-auto-create-abbrev-table")))
    (abbrev-extras-add-abbrev-to-table "tst" sym "test-expansion")
    (should (boundp sym))
    (should (equal (abbrev-expansion "tst" (symbol-value sym)) "test-expansion"))))

(provide 'abbrev-extras-test)
;;; abbrev-extras-test.el ends here
