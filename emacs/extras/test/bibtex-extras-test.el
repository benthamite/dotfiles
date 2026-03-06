;;; bibtex-extras-test.el --- Tests for bibtex-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in bibtex-extras.el.

;;; Code:

(require 'ert)
(require 'bibtex-extras)

;;;; bibtex-extras-get-field

(defconst bibtex-extras-test--sample-entry
  "@Article{smith2023cognition,
  author       = {Smith, John and Doe, Jane},
  title        = {On the Nature of Cognition},
  journaltitle = {Journal of Philosophy},
  date         = {2023},
  volume       = {42},
  pages        = {1--20},
  doi          = {10.1234/example},
}
"
  "A sample BibTeX entry for testing.")

(ert-deftest bibtex-extras-test-get-field-author ()
  "Extracts the author field from a BibTeX entry."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (let ((author (bibtex-extras-get-field "author")))
      (should author)
      (should (string-match-p "Smith" author))
      (should (string-match-p "Doe" author)))))

(ert-deftest bibtex-extras-test-get-field-title ()
  "Extracts the title field from a BibTeX entry."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "title")
                   "On the Nature of Cognition"))))

(ert-deftest bibtex-extras-test-get-field-volume ()
  "Extracts a numeric field from a BibTeX entry."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "volume") "42"))))

(ert-deftest bibtex-extras-test-get-field-nonexistent ()
  "Returns nil for a field that does not exist."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (should-not (bibtex-extras-get-field "abstract"))))

(ert-deftest bibtex-extras-test-get-field-doi ()
  "Extracts the DOI field."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "doi") "10.1234/example"))))

;;;; bibtex-extras-get-field-in-string

(ert-deftest bibtex-extras-test-get-field-in-string-author ()
  "Extracts author from a BibTeX string."
  (let ((author (bibtex-extras-get-field-in-string
                 bibtex-extras-test--sample-entry "author")))
    (should author)
    (should (string-match-p "Smith" author))))

(ert-deftest bibtex-extras-test-get-field-in-string-title ()
  "Extracts title from a BibTeX string."
  (should (equal (bibtex-extras-get-field-in-string
                  bibtex-extras-test--sample-entry "title")
                 "On the Nature of Cognition")))

(ert-deftest bibtex-extras-test-get-field-in-string-nonexistent ()
  "Returns nil for a nonexistent field in a BibTeX string."
  (should-not (bibtex-extras-get-field-in-string
               bibtex-extras-test--sample-entry "abstract")))

;;;; bibtex-extras-get-key

(ert-deftest bibtex-extras-test-get-key ()
  "Extracts the key from a BibTeX entry."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-key) "smith2023cognition"))))

(ert-deftest bibtex-extras-test-get-key-different-type ()
  "Extracts the key from a Book entry."
  (with-temp-buffer
    (insert "@Book{russell1945history,
  author    = {Russell, Bertrand},
  title     = {A History of Western Philosophy},
  publisher = {Simon \\& Schuster},
  date      = {1945},
}
")
    (bibtex-mode)
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-key) "russell1945history"))))

;;;; bibtex-extras-lessp

(ert-deftest bibtex-extras-test-lessp-alphabetical-no-crossref ()
  "Sorts alphabetically when neither entry has crossref."
  (should (bibtex-extras-lessp '("alpha" nil nil) '("beta" nil nil)))
  (should-not (bibtex-extras-lessp '("beta" nil nil) '("alpha" nil nil))))

(ert-deftest bibtex-extras-test-lessp-crossref-before-no-crossref ()
  "Entries with crossref sort before entries without."
  (should (bibtex-extras-lessp '("zeta" t nil) '("alpha" nil nil)))
  (should-not (bibtex-extras-lessp '("alpha" nil nil) '("zeta" t nil))))

(ert-deftest bibtex-extras-test-lessp-both-crossref-reverse-alpha ()
  "When both have crossref, sorts in reverse alphabetical order."
  (should (bibtex-extras-lessp '("beta" t nil) '("alpha" t nil)))
  (should-not (bibtex-extras-lessp '("alpha" t nil) '("beta" t nil))))

(ert-deftest bibtex-extras-test-lessp-equal-keys-no-crossref ()
  "Equal keys without crossref are not less than each other."
  (should-not (bibtex-extras-lessp '("same" nil nil) '("same" nil nil))))

(ert-deftest bibtex-extras-test-lessp-equal-keys-with-crossref ()
  "Equal keys with crossref are not less than each other."
  (should-not (bibtex-extras-lessp '("same" t nil) '("same" t nil))))

;;;; bibtex-extras-replace-element-by-name

(ert-deftest bibtex-extras-test-replace-element-by-name ()
  "Replaces the correct element in the list."
  (let ((list (list '("a" 1) '("b" 2) '("c" 3))))
    (bibtex-extras-replace-element-by-name list "b" '("b" 99))
    (should (equal (cadr list) '("b" 99)))))

(ert-deftest bibtex-extras-test-replace-element-by-name-no-match ()
  "Leaves the list unchanged when target is not found."
  (let ((list (list '("a" 1) '("b" 2))))
    (bibtex-extras-replace-element-by-name list "z" '("z" 99))
    (should (equal list '(("a" 1) ("b" 2))))))

;;;; bibtex-extras-get-entry-as-string

(ert-deftest bibtex-extras-test-get-entry-as-string-whole ()
  "Returns the full entry as a string."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (let ((entry (bibtex-extras-get-entry-as-string)))
      (should (string-match-p "@Article" entry))
      (should (string-match-p "smith2023cognition" entry))
      (should (string-match-p "author" entry)))))

(ert-deftest bibtex-extras-test-get-entry-as-string-field ()
  "Returns a specific field value when FIELD is provided."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (let ((title (bibtex-extras-get-entry-as-string nil "title")))
      (should (equal title "On the Nature of Cognition")))))

;;;; bibtex-extras-biblatex-fields (constant)

(ert-deftest bibtex-extras-test-biblatex-fields-contains-common ()
  "The fields list contains common BibLaTeX fields."
  (should (member "author" bibtex-extras-biblatex-fields))
  (should (member "title" bibtex-extras-biblatex-fields))
  (should (member "date" bibtex-extras-biblatex-fields))
  (should (member "doi" bibtex-extras-biblatex-fields))
  (should (member "url" bibtex-extras-biblatex-fields))
  (should (member "isbn" bibtex-extras-biblatex-fields)))

(provide 'bibtex-extras-test)
;;; bibtex-extras-test.el ends here
