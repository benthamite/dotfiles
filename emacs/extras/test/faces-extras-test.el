;;; faces-extras-test.el --- Tests for faces-extras -*- lexical-binding: t -*-

;; Tests for face attribute predicates and face attribute management
;; in faces-extras.el.

;;; Code:

(require 'ert)
(require 'faces-extras)

;;;; Unspecified-p predicate

(ert-deftest faces-extras-test-unspecified-p-symbol ()
  "Unspecified-p returns t for the symbol `unspecified'."
  (should (faces-extras--unspecified-p 'unspecified)))

(ert-deftest faces-extras-test-unspecified-p-other-symbol ()
  "Unspecified-p returns nil for other symbols."
  (should-not (faces-extras--unspecified-p 'bold))
  (should-not (faces-extras--unspecified-p nil))
  (should-not (faces-extras--unspecified-p t)))

(ert-deftest faces-extras-test-unspecified-p-list-containing ()
  "Unspecified-p returns t for lists containing `unspecified'."
  (should (faces-extras--unspecified-p '(bold unspecified italic)))
  (should (faces-extras--unspecified-p '(unspecified))))

(ert-deftest faces-extras-test-unspecified-p-list-not-containing ()
  "Unspecified-p returns nil for lists without `unspecified'."
  (should-not (faces-extras--unspecified-p '(bold italic)))
  (should-not (faces-extras--unspecified-p '(1 2 3))))

(ert-deftest faces-extras-test-unspecified-p-nested-list ()
  "Unspecified-p detects `unspecified' in nested lists."
  (should (faces-extras--unspecified-p '(bold (unspecified) italic)))
  (should (faces-extras--unspecified-p '((nested (deeply unspecified))))))

(ert-deftest faces-extras-test-unspecified-p-string ()
  "Unspecified-p returns nil for string values."
  (should-not (faces-extras--unspecified-p "unspecified"))
  (should-not (faces-extras--unspecified-p "bold")))

(ert-deftest faces-extras-test-unspecified-p-number ()
  "Unspecified-p returns nil for numeric values."
  (should-not (faces-extras--unspecified-p 12))
  (should-not (faces-extras--unspecified-p 0.8)))

(ert-deftest faces-extras-test-unspecified-p-empty-list ()
  "Unspecified-p returns nil for an empty list."
  (should-not (faces-extras--unspecified-p '())))

(provide 'faces-extras-test)
;;; faces-extras-test.el ends here
