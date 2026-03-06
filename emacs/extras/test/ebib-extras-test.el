;;; ebib-extras-test.el --- Tests for ebib-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in ebib-extras.el.

;;; Code:

(require 'ert)

;; Stub tlon variables that ebib-extras references at load time.
(defvar tlon-file-fluid "/tmp/test-fluid.bib")
(defvar tlon-file-stable "/tmp/test-stable.bib")
(defvar tlon-file-db "/tmp/test-db.bib")

(require 'ebib-extras)

;;;; ebib-extras-isbn-p

(ert-deftest ebib-extras-test-isbn-p-isbn13-no-hyphens ()
  "Matches a plain 13-digit ISBN."
  (should (ebib-extras-isbn-p "9780262045841")))

(ert-deftest ebib-extras-test-isbn-p-isbn13-with-hyphens ()
  "Matches a 13-digit ISBN with hyphens."
  (should (ebib-extras-isbn-p "978-0-262-04584-1")))

(ert-deftest ebib-extras-test-isbn-p-isbn10-no-hyphens ()
  "Matches a plain 10-digit ISBN."
  (should (ebib-extras-isbn-p "0262045842")))

(ert-deftest ebib-extras-test-isbn-p-isbn10-with-hyphens ()
  "Matches a 10-digit ISBN with hyphens."
  (should (ebib-extras-isbn-p "0-262-04584-2")))

(ert-deftest ebib-extras-test-isbn-p-isbn10-with-x-check ()
  "Matches a 10-digit ISBN ending in X."
  (should (ebib-extras-isbn-p "0-306-40615-X")))

(ert-deftest ebib-extras-test-isbn-p-with-prefix ()
  "Matches a string with ISBN-13 prefix label."
  (should (ebib-extras-isbn-p "ISBN-13 978-0-262-04584-1")))

(ert-deftest ebib-extras-test-isbn-p-with-isbn-prefix ()
  "Matches a string with ISBN prefix and colon."
  (should (ebib-extras-isbn-p "ISBN: 9780262045841")))

(ert-deftest ebib-extras-test-isbn-p-too-short ()
  "Rejects a string that is too short to be an ISBN."
  (should-not (ebib-extras-isbn-p "12345")))

(ert-deftest ebib-extras-test-isbn-p-empty-string ()
  "Rejects an empty string."
  (should-not (ebib-extras-isbn-p "")))

(ert-deftest ebib-extras-test-isbn-p-alphabetic ()
  "Rejects a purely alphabetic string."
  (should-not (ebib-extras-isbn-p "notanisbn")))

;;;; ebib-extras-key-is-valid-p

(ert-deftest ebib-extras-test-key-is-valid-p-standard ()
  "Accepts a standard BibTeX key like `author2023keyword'."
  (should (ebib-extras-key-is-valid-p "smith2023cognition")))

(ert-deftest ebib-extras-test-key-is-valid-p-with-hyphens ()
  "Accepts a key with hyphens."
  (should (ebib-extras-key-is-valid-p "van-der-berg2021ethics")))

(ert-deftest ebib-extras-test-key-is-valid-p-with-underscores ()
  "Accepts a key with underscores."
  (should (ebib-extras-key-is-valid-p "smith_jones2020results")))

(ert-deftest ebib-extras-test-key-is-valid-p-too-short-prefix ()
  "Rejects a key with fewer than 2 chars before the year."
  (should-not (ebib-extras-key-is-valid-p "s2023x")))

(ert-deftest ebib-extras-test-key-is-valid-p-no-year ()
  "Rejects a key that lacks a 4-digit year component."
  (should-not (ebib-extras-key-is-valid-p "smithcognition")))

(ert-deftest ebib-extras-test-key-is-valid-p-too-short-suffix ()
  "Rejects a key with fewer than 2 chars after the year."
  (should-not (ebib-extras-key-is-valid-p "smith2023x")))

(ert-deftest ebib-extras-test-key-is-valid-p-special-chars ()
  "Rejects a key with characters outside [_[:alnum:]-]."
  (should-not (ebib-extras-key-is-valid-p "smith@2023!cognition")))

;;;; ebib-extras-get-file-in-string

(ert-deftest ebib-extras-test-get-file-in-string-single-pdf ()
  "Returns the file when a single PDF is present."
  (let ((result (ebib-extras-get-file-in-string "/library/smith2023.pdf" "pdf")))
    (should (string-suffix-p "smith2023.pdf" result))))

(ert-deftest ebib-extras-test-get-file-in-string-multiple-files ()
  "Returns the correct file from a semicolon-separated list."
  (let ((result (ebib-extras-get-file-in-string
                 "/lib/smith2023.pdf; /lib/smith2023.html" "html")))
    (should (string-suffix-p "smith2023.html" result))))

(ert-deftest ebib-extras-test-get-file-in-string-no-match ()
  "Returns nil when no file has the requested extension."
  (should-not (ebib-extras-get-file-in-string "/lib/smith2023.pdf" "html")))

(ert-deftest ebib-extras-test-get-file-in-string-nil-input ()
  "Returns nil when FILES is nil."
  (should-not (ebib-extras-get-file-in-string nil "pdf")))

(ert-deftest ebib-extras-test-get-file-in-string-first-match-wins ()
  "Returns the first file matching the extension."
  (let ((result (ebib-extras-get-file-in-string
                 "/a/first.pdf; /b/second.pdf" "pdf")))
    (should (string-suffix-p "first.pdf" result))))

;;;; ebib-extras--rename-and-abbreviate-file

(ert-deftest ebib-extras-test-rename-and-abbreviate-with-extension ()
  "Constructs path with extension appended to key."
  (let ((result (ebib-extras--rename-and-abbreviate-file "/tmp/library" "smith2023" "pdf")))
    (should (string-suffix-p "smith2023.pdf" result))
    (should (string-prefix-p "/tmp/library" result))))

(ert-deftest ebib-extras-test-rename-and-abbreviate-without-extension ()
  "Constructs path using the key alone when extension is nil."
  (let ((result (ebib-extras--rename-and-abbreviate-file "/tmp/library" "smith2023" nil)))
    (should (string-suffix-p "smith2023" result))
    (should-not (string-match-p "\\." (file-name-nondirectory result)))))

(ert-deftest ebib-extras-test-rename-and-abbreviate-abbreviates-home ()
  "Abbreviates the home directory in the result."
  (let ((result (ebib-extras--rename-and-abbreviate-file
                 (expand-file-name "~/some/path") "key2023word" "pdf")))
    (should (string-prefix-p "~/" result))))

;;;; ebib-extras-get-authors-list

(ert-deftest ebib-extras-test-get-authors-list-single ()
  "Parses a single author in `Last, First' format."
  (let ((result (ebib-extras-get-authors-list "Smith, John")))
    (should (equal result '("John Smith")))))

(ert-deftest ebib-extras-test-get-authors-list-multiple ()
  "Parses multiple authors separated by ` and '."
  (let ((result (ebib-extras-get-authors-list "Smith, John and Doe, Jane")))
    (should (equal result '("John Smith" "Jane Doe")))))

(ert-deftest ebib-extras-test-get-authors-list-braced ()
  "Preserves braced author names without reversal."
  (let ((result (ebib-extras-get-authors-list "{World Health Organization}")))
    (should (equal result '("World Health Organization")))))

(ert-deftest ebib-extras-test-get-authors-list-single-name ()
  "Handles a single-name author without comma."
  (let ((result (ebib-extras-get-authors-list "Aristotle")))
    (should (equal result '("Aristotle")))))

(ert-deftest ebib-extras-test-get-authors-list-mixed ()
  "Handles a mix of braced and normal authors."
  (let ((result (ebib-extras-get-authors-list "Smith, John and {WHO}")))
    (should (equal result '("John Smith" "WHO")))))

;;;; ebib-extras-format-authors

(ert-deftest ebib-extras-test-format-authors-single ()
  "Formats a single author."
  (should (equal (ebib-extras-format-authors '("John Smith")) "John Smith")))

(ert-deftest ebib-extras-test-format-authors-two ()
  "Formats two authors with default separator."
  (should (equal (ebib-extras-format-authors '("John Smith" "Jane Doe"))
                 "John Smith & Jane Doe")))

(ert-deftest ebib-extras-test-format-authors-three ()
  "Formats three authors (at the default max)."
  (should (equal (ebib-extras-format-authors '("A" "B" "C"))
                 "A & B & C")))

(ert-deftest ebib-extras-test-format-authors-exceeds-max ()
  "Uses `et al' when authors exceed the max."
  (should (equal (ebib-extras-format-authors '("A" "B" "C" "D"))
                 "A et al")))

(ert-deftest ebib-extras-test-format-authors-custom-separator ()
  "Uses a custom separator."
  (should (equal (ebib-extras-format-authors '("A" "B") ", ")
                 "A, B")))

(ert-deftest ebib-extras-test-format-authors-custom-max ()
  "Respects a custom max."
  (should (equal (ebib-extras-format-authors '("A" "B" "C") nil 2)
                 "A et al")))

;;;; ebib-extras-unbrace

(ert-deftest ebib-extras-test-unbrace-simple ()
  "Removes outermost braces."
  (should (equal (ebib-extras-unbrace "{Hello}") "Hello")))

(ert-deftest ebib-extras-test-unbrace-nested ()
  "Removes all braces, including nested ones."
  (should (equal (ebib-extras-unbrace "{The {GNU} Project}") "The GNU Project")))

(ert-deftest ebib-extras-test-unbrace-no-braces ()
  "Returns string unchanged when no braces are present."
  (should (equal (ebib-extras-unbrace "Hello World") "Hello World")))

(ert-deftest ebib-extras-test-unbrace-empty ()
  "Returns empty string for empty input."
  (should (equal (ebib-extras-unbrace "") "")))

;;;; ebib-extras-valid-key-regexp (constant)

(ert-deftest ebib-extras-test-valid-key-regexp-basic-match ()
  "The regexp matches a typical valid key."
  (should (string-match-p ebib-extras-valid-key-regexp "smith2023cognition")))

(ert-deftest ebib-extras-test-valid-key-regexp-rejects-spaces ()
  "The regexp rejects keys with spaces."
  (should-not (string-match-p ebib-extras-valid-key-regexp "smith 2023cognition")))

;;;; ebib-extras-book-like-entry-types (constant)

(ert-deftest ebib-extras-test-book-like-entry-types-contains-both-cases ()
  "The list includes both lowercase and capitalized forms."
  (should (member "book" ebib-extras-book-like-entry-types))
  (should (member "Book" ebib-extras-book-like-entry-types))
  (should (member "incollection" ebib-extras-book-like-entry-types))
  (should (member "Incollection" ebib-extras-book-like-entry-types)))

(provide 'ebib-extras-test)
;;; ebib-extras-test.el ends here
