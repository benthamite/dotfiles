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

;;;; bibtex-extras-set-field

(ert-deftest bibtex-extras-test-set-field-create-new ()
  "Creates a new field when it does not exist."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-set-field "abstract" "An abstract")
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "abstract") "An abstract"))))

(ert-deftest bibtex-extras-test-set-field-replace-existing ()
  "Replaces an existing field value."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-set-field "volume" "99")
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "volume") "99"))))

(ert-deftest bibtex-extras-test-set-field-replace-title ()
  "Replaces the title field with a new value."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-set-field "title" "New Title")
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "title") "New Title"))))

;;;; bibtex-extras-add-or-update-field

(ert-deftest bibtex-extras-test-add-or-update-field-add-new ()
  "Adds a new field when it does not exist."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-add-or-update-field "keywords" "philosophy")
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "keywords") "philosophy"))))

(ert-deftest bibtex-extras-test-add-or-update-field-update-existing ()
  "Updates an existing field value."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-add-or-update-field "volume" "100")
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "volume") "100"))))

;;;; bibtex-extras-kill-field

(ert-deftest bibtex-extras-test-kill-field-removes-field ()
  "Removes an existing field from the entry."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-kill-field "doi")
    (widen)
    (goto-char (point-min))
    (should-not (bibtex-extras-get-field "doi"))))

(ert-deftest bibtex-extras-test-kill-field-preserves-other-fields ()
  "Other fields remain intact after removing a field."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-kill-field "doi")
    (widen)
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "title") "On the Nature of Cognition"))
    (should (equal (bibtex-extras-get-field "volume") "42"))))

(ert-deftest bibtex-extras-test-kill-field-nonexistent ()
  "Killing a nonexistent field does not error."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    ;; Should not signal an error
    (bibtex-extras-kill-field "abstract")
    (widen)
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "title") "On the Nature of Cognition"))))

;;;; bibtex-extras-append-to-field

(ert-deftest bibtex-extras-test-append-to-field-existing ()
  "Appends to an existing field with the default semicolon delimiter."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    ;; First add a keywords field
    (bibtex-extras-add-or-update-field "keywords" "philosophy")
    (goto-char (point-min))
    (bibtex-extras-append-to-field "keywords" "cognition")
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "keywords") "philosophy;cognition"))))

(ert-deftest bibtex-extras-test-append-to-field-custom-delimiter ()
  "Appends to a field with a custom delimiter."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-add-or-update-field "keywords" "philosophy")
    (goto-char (point-min))
    (bibtex-extras-append-to-field "keywords" "cognition" ", ")
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "keywords") "philosophy, cognition"))))

(ert-deftest bibtex-extras-test-append-to-field-empty ()
  "Appending to a nonexistent field creates it with just the value."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-append-to-field "keywords" "philosophy")
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "keywords") "philosophy"))))

;;;; bibtex-extras-escape-special-characters

(ert-deftest bibtex-extras-test-escape-special-characters-ampersand ()
  "Escapes an unescaped ampersand in a field value."
  (with-temp-buffer
    (insert "@Article{test2023,
  author = {Smith & Jones},
  title  = {A Title},
}
")
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-escape-special-characters)
    (goto-char (point-min))
    (should (string-match-p "Smith \\\\& Jones"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest bibtex-extras-test-escape-special-characters-dollar ()
  "Escapes an unescaped dollar sign in a field value."
  (with-temp-buffer
    (insert "@Article{test2023,
  title  = {A $100 Prize},
}
")
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-escape-special-characters)
    (goto-char (point-min))
    (should (string-match-p "\\\\\\$100"
                            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest bibtex-extras-test-escape-special-characters-skips-url ()
  "Does not escape special characters in URL fields."
  (with-temp-buffer
    (insert "@Article{test2023,
  title = {A Title},
  url   = {https://example.com/path?a=1&b=2},
}
")
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-escape-special-characters)
    (goto-char (point-min))
    (should (string-match-p "a=1&b=2"
                            (buffer-substring-no-properties (point-min) (point-max))))))

;;;; bibtex-extras-entry-sorter

(ert-deftest bibtex-extras-test-entry-sorter-no-crossref ()
  "Returns the key and nil crossref for an entry without crossref."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (let ((result (bibtex-extras-entry-sorter)))
      (should (equal (nth 0 result) "smith2023cognition"))
      (should-not (nth 1 result))
      (should-not (nth 2 result)))))

(ert-deftest bibtex-extras-test-entry-sorter-with-crossref ()
  "Returns the key and non-nil crossref flag for an entry with crossref."
  (with-temp-buffer
    (insert "@InCollection{chapter2023,
  author   = {Doe, Jane},
  title    = {A Chapter},
  crossref = {book2023},
}
")
    (bibtex-mode)
    (goto-char (point-min))
    (let ((result (bibtex-extras-entry-sorter)))
      (should (equal (nth 0 result) "chapter2023"))
      (should (nth 1 result))
      (should-not (nth 2 result)))))

;;;; bibtex-extras-get-field-name

(ert-deftest bibtex-extras-test-get-field-name-title ()
  "Returns the field name when point is on the title field."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    ;; Move to the title field text
    (re-search-forward "On the Nature")
    (should (equal (bibtex-extras-get-field-name) "title"))))

(ert-deftest bibtex-extras-test-get-field-name-author ()
  "Returns the field name when point is on the author field."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (re-search-forward "Doe, Jane")
    (goto-char (match-beginning 0))
    (should (equal (bibtex-extras-get-field-name) "author"))))

(ert-deftest bibtex-extras-test-get-field-name-doi ()
  "Returns the field name when point is on the doi field."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (goto-char (point-min))
    (re-search-forward "10\\.1234")
    (should (equal (bibtex-extras-get-field-name) "doi"))))

;;;; bibtex-extras-convert-titleaddon-to-journaltitle

(ert-deftest bibtex-extras-test-convert-titleaddon-to-journaltitle ()
  "Converts titleaddon field name to journaltitle."
  (with-temp-buffer
    (insert "@Article{test2023,
  author     = {Author, Test},
  title      = {A Title},
  titleaddon = {Some Journal},
}
")
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-convert-titleaddon-to-journaltitle)
    (goto-char (point-min))
    (should (string-match-p "journaltitle = "
                            (buffer-substring-no-properties (point-min) (point-max))))
    (should-not (string-match-p "titleaddon = "
                                (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest bibtex-extras-test-convert-titleaddon-to-journaltitle-no-titleaddon ()
  "Does nothing when there is no titleaddon field."
  (with-temp-buffer
    (insert bibtex-extras-test--sample-entry)
    (bibtex-mode)
    (let ((before (buffer-string)))
      (goto-char (point-min))
      (bibtex-extras-convert-titleaddon-to-journaltitle)
      (should (equal (buffer-string) before)))))

(ert-deftest bibtex-extras-test-convert-titleaddon-to-journaltitle-preserves-value ()
  "The field value is preserved after conversion."
  (with-temp-buffer
    (insert "@Article{test2023,
  author     = {Author, Test},
  title      = {A Title},
  titleaddon = {The Atlantic},
}
")
    (bibtex-mode)
    (goto-char (point-min))
    (bibtex-extras-convert-titleaddon-to-journaltitle)
    (goto-char (point-min))
    (should (equal (bibtex-extras-get-field "journaltitle") "The Atlantic"))))

(provide 'bibtex-extras-test)
;;; bibtex-extras-test.el ends here
