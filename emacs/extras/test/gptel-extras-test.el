;;; gptel-extras-test.el --- Tests for gptel-extras -*- lexical-binding: t -*-

;; Tests for pure and near-pure helper functions in gptel-extras.el.

;;; Code:

(require 'ert)
(require 'gptel-extras)

;;;; Generate next heading

(ert-deftest gptel-extras-test-generate-next-heading-no-number ()
  "Append \" 2\" when heading has no numeric suffix."
  (should (equal (gptel-extras--generate-next-heading "My conversation")
                 "My conversation 2")))

(ert-deftest gptel-extras-test-generate-next-heading-increment ()
  "Increment the trailing number when present."
  (should (equal (gptel-extras--generate-next-heading "My conversation 2")
                 "My conversation 3")))

(ert-deftest gptel-extras-test-generate-next-heading-large-number ()
  "Increment a large trailing number correctly."
  (should (equal (gptel-extras--generate-next-heading "Chat 99")
                 "Chat 100")))

(ert-deftest gptel-extras-test-generate-next-heading-number-one ()
  "Increment from 1 to 2."
  (should (equal (gptel-extras--generate-next-heading "Session 1")
                 "Session 2")))

(ert-deftest gptel-extras-test-generate-next-heading-number-in-middle ()
  "A number in the middle of the heading is not treated as a suffix."
  (should (equal (gptel-extras--generate-next-heading "Chapter 3 notes")
                 "Chapter 3 notes 2")))

(ert-deftest gptel-extras-test-generate-next-heading-empty-string ()
  "Empty string gets \" 2\" appended."
  (should (equal (gptel-extras--generate-next-heading "")
                 " 2")))

;;;; Normalize whitespace

(ert-deftest gptel-extras-test-normalize-whitespace-basic ()
  "Collapse multiple spaces to a single space."
  (should (equal (gptel-extras-normalize-whitespace "hello   world")
                 "hello world")))

(ert-deftest gptel-extras-test-normalize-whitespace-tabs-newlines ()
  "Collapse tabs and newlines into a single space."
  (should (equal (gptel-extras-normalize-whitespace "hello\t\n\rworld")
                 "hello world")))

(ert-deftest gptel-extras-test-normalize-whitespace-trims ()
  "Trim leading and trailing whitespace."
  (should (equal (gptel-extras-normalize-whitespace "  hello world  ")
                 "hello world")))

(ert-deftest gptel-extras-test-normalize-whitespace-already-normal ()
  "Return string unchanged when already normalized."
  (should (equal (gptel-extras-normalize-whitespace "already normal")
                 "already normal")))

(ert-deftest gptel-extras-test-normalize-whitespace-empty ()
  "Return empty string for whitespace-only input."
  (should (equal (gptel-extras-normalize-whitespace "   ") "")))

(ert-deftest gptel-extras-test-normalize-whitespace-single-word ()
  "Return single word unchanged."
  (should (equal (gptel-extras-normalize-whitespace "word") "word")))

;;;; Create fuzzy regex

(ert-deftest gptel-extras-test-create-fuzzy-regex-simple ()
  "Fuzzy regex matches the original string literally."
  (let ((regex (gptel-extras-create-fuzzy-regex "hello world")))
    (should (string-match-p regex "hello world"))))

(ert-deftest gptel-extras-test-create-fuzzy-regex-exact-whitespace ()
  "Fuzzy regex matches the exact whitespace in the original string.
The fuzzy regex only flexes escaped whitespace from `regexp-quote',
so a plain space matches exactly one space."
  (let ((regex (gptel-extras-create-fuzzy-regex "hello world")))
    (should (string-match-p regex "hello world"))
    ;; Two spaces do not match because the regex has a single literal space.
    (should-not (string-match-p regex "hello  world"))))

(ert-deftest gptel-extras-test-create-fuzzy-regex-no-false-match ()
  "Fuzzy regex does not match unrelated strings."
  (let ((regex (gptel-extras-create-fuzzy-regex "hello world")))
    (should-not (string-match-p regex "goodbye world"))))

(ert-deftest gptel-extras-test-create-fuzzy-regex-special-chars ()
  "Fuzzy regex properly escapes regex special characters."
  (let ((regex (gptel-extras-create-fuzzy-regex "foo.bar")))
    (should (string-match-p regex "foo.bar"))
    (should-not (string-match-p regex "fooXbar"))))

(ert-deftest gptel-extras-test-create-fuzzy-regex-multiple-spaces ()
  "Fuzzy regex preserves multiple spaces literally.
`regexp-quote' does not escape spaces, so multiple spaces remain
literal in the pattern."
  (let ((regex (gptel-extras-create-fuzzy-regex "a  b")))
    (should (string-match-p regex "a  b"))
    (should-not (string-match-p regex "a b"))))

;;;; Create flexible regex

(ert-deftest gptel-extras-test-create-flexible-regex-matches-exact ()
  "Flexible regex matches the original string."
  (let ((regex (gptel-extras-create-flexible-regex "abc")))
    (should (string-match-p regex "abc"))))

(ert-deftest gptel-extras-test-create-flexible-regex-matches-with-spaces ()
  "Flexible regex matches with whitespace between characters."
  (let ((regex (gptel-extras-create-flexible-regex "abc")))
    (should (string-match-p regex "a b c"))
    (should (string-match-p regex "a  b  c"))))

(ert-deftest gptel-extras-test-create-flexible-regex-no-false-match ()
  "Flexible regex does not match when characters differ."
  (let ((regex (gptel-extras-create-flexible-regex "abc")))
    (should-not (string-match-p regex "xyz"))))

(ert-deftest gptel-extras-test-create-flexible-regex-special-chars ()
  "Flexible regex escapes regex metacharacters."
  (let ((regex (gptel-extras-create-flexible-regex "a.b")))
    (should (string-match-p regex "a.b"))
    ;; The dot is escaped, so it should not match arbitrary chars,
    ;; but flexible regex inserts optional whitespace between chars
    ;; so "aXb" should still not match since X is not whitespace or dot.
    (should-not (string-match-p regex "aXb"))))

;;;; Extract parts text

(ert-deftest gptel-extras-test-extract-parts-text-strings ()
  "Return strings from a list of plain string parts."
  (should (equal (gptel-extras--extract-parts-text '("hello" "world"))
                 '("hello" "world"))))

(ert-deftest gptel-extras-test-extract-parts-text-hash-tables ()
  "Extract text from hash-table parts."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "text" "from hash" ht)
    (should (equal (gptel-extras--extract-parts-text (list ht))
                   '("from hash")))))

(ert-deftest gptel-extras-test-extract-parts-text-mixed ()
  "Handle a mix of strings, hash-tables, and non-text items."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "text" "hash-text" ht)
    (let ((ht2 (make-hash-table :test 'equal)))
      (puthash "image" "data" ht2)
      (should (equal (gptel-extras--extract-parts-text
                      (list "plain" ht ht2 42))
                     '("plain" "hash-text" nil nil))))))

(ert-deftest gptel-extras-test-extract-parts-text-empty ()
  "Return nil for an empty parts list."
  (should (null (gptel-extras--extract-parts-text '()))))

(provide 'gptel-extras-test)
;;; gptel-extras-test.el ends here
