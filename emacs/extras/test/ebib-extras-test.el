;;; ebib-extras-test.el --- Tests for ebib-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in ebib-extras.el.

;;; Code:

(require 'ert)

;; Stub tlon variables that ebib-extras references at load time.
(defvar tlon-file-fluid "/tmp/test-fluid.bib")
(defvar tlon-file-stable "/tmp/test-stable.bib")
(defvar tlon-file-db "/tmp/test-db.bib")

(require 'ebib-extras)
(require 'tlon-core)

;;;; Database save synchronization

(defun ebib-extras-test--with-save-database (function)
  "Call FUNCTION with an isolated real database and its file."
  (let* ((file (make-temp-file "ebib-save-test-" nil ".bib"))
         (db (ebib-db-new-database))
         (ebib--cur-db db)
         (ebib--databases (list db)))
    (unwind-protect
        (with-temp-buffer
          (let ((ebib--buffer-alist `((index . ,(current-buffer)))))
            (ebib-db-set-filename file db)
            (ebib-db-set-backup nil db)
            (ebib-db-set-entry "Author2020Paper"
                               '(("=type=" . "article") ("title" . "Paper")) db)
            (set-file-times file (time-subtract (current-time) 60))
            (ebib-db-set-modtime (ebib--get-file-modtime file) db)
            (funcall function db file)))
      (delete-file file))))

(ert-deftest ebib-extras-test-own-saves-refresh-modtime ()
  "Attachment and timer saves must permit a subsequent save without prompts."
  (dolist (operation '(attachment autosave))
    (ebib-extras-test--with-save-database
     (lambda (db file)
       (let ((other-db (ebib-db-new-database))
             (ebib--needs-update nil))
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (&rest _) (ert-fail "Spurious overwrite prompt")))
                   ((symbol-function 'run-with-timer) #'ignore))
           (let ((ebib--cur-db other-db))
             (if (eq operation 'attachment)
                 (ebib-extras--update-file-field-contents
                  "Author2020Paper" "/tmp/author-paper.pdf" db)
               (ebib-db-set-modified t db)
               (ebib-extras-auto-save-databases))
             (should (eq ebib--cur-db other-db)))
           (should (equal (ebib-db-get-modtime db) (ebib--get-file-modtime file)))
           (ebib-save-current-database t)
           (should-not (ebib-db-modified-p db))
           (with-temp-buffer
             (insert-file-contents file)
             (should (search-forward "Author2020Paper" nil t))
             (when (eq operation 'attachment)
               (should (search-forward "/tmp/author-paper.pdf" nil t))))))))))

(ert-deftest ebib-extras-test-own-saves-preserve-external-edits ()
  "Refusing an external-file conflict preserves disk and unsaved database edits."
  (dolist (operation '(attachment autosave))
    (ebib-extras-test--with-save-database
     (lambda (db file)
       (let ((external "@Misc{External2026Entry, title = {External change}}\n")
             (ebib--needs-update nil)
             prompted)
         (with-temp-file file (insert external))
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (_prompt) (setq prompted t) nil))
                   ((symbol-function 'run-with-timer) #'ignore))
           (should-error
            (if (eq operation 'attachment)
                (ebib-extras--update-file-field-contents
                 "Author2020Paper" "/tmp/author-paper.pdf" db)
              (ebib-db-set-modified t db)
              (ebib-extras-auto-save-databases)))
           (should prompted)
           (should (ebib-db-modified-p db))
           (with-temp-buffer
             (insert-file-contents file)
             (should (equal (buffer-string) external)))))))))

;;;; Ebib return bindings

(ert-deftest ebib-extras-test-return-edits-current-field ()
  "Bind RET to edit the current field in an Ebib entry buffer."
  (should (eq (lookup-key ebib-entry-mode-map (kbd "RET"))
              #'ebib-edit-current-field)))

(ert-deftest ebib-extras-test-return-edits-selected-entry ()
  "Bind RET to edit the selected entry in an Ebib index buffer."
  (should (eq (lookup-key ebib-index-mode-map (kbd "RET"))
              #'ebib-edit-entry)))

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

;;;; ebib-extras-attach-files

(ert-deftest ebib-extras-test-attach-files-capitalized-online-type ()
  "Generate both attachments for a capitalized Online entry type."
  (let (attached)
    (cl-letf (((symbol-function 'ebib-extras-get-field)
               (lambda (field &optional _key)
                 (pcase field
                   ("url" "https://example.com/article")
                   ("=type=" "Online")
                   (_ nil))))
              ((symbol-function 'ebib-extras-url-to-pdf-attach)
               (lambda (key &rest _) (push (list "pdf" key) attached)))
              ((symbol-function 'ebib-extras-url-to-html-attach)
               (lambda (key &rest _) (push (list "html" key) attached))))
      (ebib-extras-attach-files "Ngo2026WhatJustHappened"))
    (should (equal (nreverse attached)
                   '(("pdf" "Ngo2026WhatJustHappened")
                     ("html" "Ngo2026WhatJustHappened"))))))

;;;; ebib-extras--extension-directories

(ert-deftest ebib-extras-test-extension-directories-pdf ()
  "Return the PDF library directory for the \"pdf\" extension."
  (let ((paths-dir-pdf-library "/test/pdf-library/"))
    (should (equal (ebib-extras--extension-directories "pdf")
                   "/test/pdf-library/"))))

(ert-deftest ebib-extras-test-extension-directories-html ()
  "Return the HTML library directory for the \"html\" extension."
  (let ((paths-dir-html-library "/test/html-library/"))
    (should (equal (ebib-extras--extension-directories "html")
                   "/test/html-library/"))))

(ert-deftest ebib-extras-test-extension-directories-valid-media-extension ()
  "Return the media library directory for extensions in valid-file-extensions."
  (let ((paths-dir-media-library "/test/media-library/"))
    (should (equal (ebib-extras--extension-directories "mp3")
                   "/test/media-library/"))
    (should (equal (ebib-extras--extension-directories "webm")
                   "/test/media-library/"))))

(ert-deftest ebib-extras-test-extension-directories-unknown-extension ()
  "Signal a user-error for an unknown file extension."
  (should-error (ebib-extras--extension-directories "xyz")
                :type 'user-error))

;;;; ebib-extras-check-valid-key

(ert-deftest ebib-extras-test-check-valid-key-valid ()
  "Do not signal an error for a valid BibTeX key."
  (should-not (ebib-extras-check-valid-key "smith2023cognition")))

(ert-deftest ebib-extras-test-check-valid-key-invalid ()
  "Signal user-error for an invalid BibTeX key."
  (should-error (ebib-extras-check-valid-key "bad")
                :type 'user-error))

;;;; ebib-extras-set-rating

(ert-deftest ebib-extras-test-set-rating-searches-letterboxd-without-slug ()
  "Search Letterboxd directly when the film has no stored slug."
  (let ((ebib--cur-db 'test-db)
	opened-url searched-title set-fields)
    (cl-letf (((symbol-function 'ebib-extras-choose-rating)
	       (lambda () "7"))
	      ((symbol-function 'ebib-extras-get-supertype)
	       (lambda () "film"))
	      ((symbol-function 'ebib-extras-get-field)
	       (lambda (field &optional _key)
		 (pcase field
		   ("title" "Blue Moon")
		   ("url" "https://www.imdb.com/title/tt32536228/")
		   ("letterboxd" nil))))
	      ((symbol-function 'ebib--get-key-at-point)
	       (lambda () "linklater2025bluemoon"))
	      ((symbol-function 'ebib-set-field-value)
	       (lambda (field value _key _db &optional _action)
		 (push (cons field value) set-fields)))
	      ((symbol-function 'ebib-extras-update-entry-buffer) #'ignore)
	      ((symbol-function 'browse-url)
	       (lambda (url &rest _args)
		 (setq opened-url url)))
	      ((symbol-function 'ebib-extras-search-letterboxd)
	       (lambda (title)
		 (setq searched-title title)))
	      ((symbol-function 'bib-search-letterboxd)
	       (lambda (&rest _args)
		 (error "Private Letterboxd lookup should not run"))))
      (ebib-extras-set-rating))
    (should (equal searched-title "Blue Moon"))
    (should (equal opened-url "https://www.imdb.com/title/tt32536228/"))
    (should (equal set-fields '(("rating" . "7"))))))

;;;; arXiv identifiers

(ert-deftest ebib-extras-test-arxiv-id-p-new-style ()
  "Recognize modern arXiv identifiers."
  (should (ebib-extras-arxiv-id-p "2401.01234"))
  (should (ebib-extras-arxiv-id-p "2401.01234v2")))

(ert-deftest ebib-extras-test-arxiv-id-p-old-style ()
  "Recognize legacy arXiv identifiers."
  (should (ebib-extras-arxiv-id-p "math/0309136"))
  (should (ebib-extras-arxiv-id-p "hep-th/9901001v1")))

(ert-deftest ebib-extras-test-arxiv-id-p-rejects-doi ()
  "Do not treat DOI strings as arXiv identifiers."
  (should-not (ebib-extras-arxiv-id-p "10.1000/example")))

;;;; PDF postprocessing

(require 'cl-lib)
(require 'ebib-extras)
(require 'files-extras)
(defvar tlon-languages-properties)
(defvar pdf-view-mode-hook)

(ert-deftest ebib-extras-pdf-keeps-target-across-buffer-switch ()
  "Postprocessing uses the entry PDF and language across metadata yields."
  (dolist (mode '(ebib-entry-mode ebib-index-mode fundamental-mode))
    (let* ((db (ebib-db-new-database))
           (ebib--cur-db db)
           (key "Author2020Paper")
           (tlon-languages-properties nil)
           (pdf-view-mode-hook nil)
           (pdf "/tmp/verified-paper.pdf")
           command opened)
      (ebib-db-set-entry key `(("file" . ,pdf)) db)
      (should-not (ebib-get-field-value "langid" key db 'noerror))
      ;; The agent verifies the PDF's language before starting attachment.
      (ebib-set-field-value "langid" "english" key db 'overwrite)
      (with-temp-buffer
        (setq major-mode mode)
        (cl-letf (((symbol-function 'ebib-extras-set-pdf-metadata)
                   (lambda (&rest _)
                     (setq major-mode 'pdf-view-mode)
                     (setq buffer-file-name "/tmp/unrelated-paper.pdf")
                     (setq ebib--cur-db (ebib-db-new-database))))
                  ((symbol-function 'executable-find) (lambda (_) "/usr/bin/ocrmypdf"))
                  ((symbol-function 'tlon-lookup)
                   (lambda (_table result _property language)
                     (should (equal language "english"))
                     (if (eq result :standard) "english" "eng")))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) (ert-fail "Unexpected language prompt")))
                  ((symbol-function 'tlon-select-language)
                   (lambda (&rest _) (ert-fail "Unexpected language prompt")))
                  ((symbol-function 'start-process-shell-command)
                   (lambda (_name _buffer text) (setq command text) :process))
                  ((symbol-function 'set-process-filter) #'ignore)
                  ((symbol-function 'find-file) (lambda (file) (setq opened file))))
          (ebib-extras--af-postprocess-pdf key)
          (should (string-match-p "-l eng" command))
          (should (string-match-p (regexp-quote pdf) command))
          (should (equal opened pdf)))))))

(ert-deftest ebib-extras-pdf-refuses-multiple-pdfs-before-prompting ()
  "Multiple attached PDFs must not select an arbitrary OCR target."
  (let ((ebib--cur-db (ebib-db-new-database)))
    (cl-letf (((symbol-function 'ebib-extras-get-field)
               (lambda (&rest _) "first.pdf;second.pdf"))
              ((symbol-function 'ebib--split-files)
               (lambda (_) '("first.pdf" "second.pdf")))
              ((symbol-function 'ebib-extras-get-or-set-language)
               (lambda (&rest _) (ert-fail "Language requested before PDF validation"))))
      (should-error (ebib-extras--af-postprocess-pdf "Author2020Paper")
                    :type 'user-error))))

(ert-deftest ebib-extras-test-crossref-allows-inherited-parent-fields ()
  "Crossref validation rejects local duplication, not inherited values."
  (let* ((db (ebib-db-new-database))
         (ebib--cur-db db)
         (ebib--databases (list db))
         (key "Author2021Paper"))
    (ebib-db-set-entry "Editor2021Proceedings"
                       '(("=type=" . "proceedings")
                         ("publisher" . "{PMLR}") ("date" . "{2021}")) db)
    (ebib-db-set-entry key '(("=type=" . "inproceedings")
                             ("crossref" . "{Editor2021Proceedings}")) db)
    (should (equal (ebib-extras-get-field "publisher" key) "PMLR"))
    (should-not (ebib-db-get-field-value "publisher" key db 'noerror))
    (should-not (ebib-extras-check-crossref key))
    (ebib-set-field-value "publisher" "PMLR" key db 'overwrite)
    (should-error (ebib-extras-check-crossref key) :type 'user-error)
    (ebib-set-field-value "crossref" nil key db 'overwrite)
    (should-error (ebib-extras-check-crossref key) :type 'user-error)))

(ert-deftest ebib-extras-test-new-language-is-passed-to-pdf-processing ()
  "An intentional language selection returns its value, not setter status."
  (let* ((db (ebib-db-new-database))
         (ebib--cur-db db)
         (key "Author2020Paper")
         (tlon-languages-properties nil)
         (pdf-view-mode-hook nil)
         prompted command)
    (ebib-db-set-entry key '(("file" . "/tmp/verified-paper.pdf")) db)
    (cl-letf (((symbol-function 'ebib--get-key-at-point) (lambda () key))
              ((symbol-function 'tlon-lookup)
               (lambda (_table result _property language)
                 (when language
                   (should (equal language "english"))
                   (if (eq result :standard) "english" "eng"))))
              ((symbol-function 'tlon-lookup-all) (lambda (&rest _) '("english")))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (setq prompted t) "english"))
              ((symbol-function 'ebib-extras-set-field)
               (lambda (field value)
                 (ebib-set-field-value field value key db 'overwrite) t))
              ((symbol-function 'ebib-extras-set-pdf-metadata) #'ignore)
              ((symbol-function 'executable-find) (lambda (_) "/usr/bin/ocrmypdf"))
              ((symbol-function 'start-process-shell-command)
               (lambda (_name _buffer text) (setq command text) :process))
              ((symbol-function 'set-process-filter) #'ignore)
              ((symbol-function 'find-file) #'ignore))
      (ebib-extras--af-postprocess-pdf key db)
      (should prompted)
      (should (equal (ebib-extras-get-field "langid" key) "english"))
      (should (string-match-p "-l eng" command)))))

(ert-deftest ebib-extras-test-abstract-uses-selected-database ()
  "Preserve the selected entry's abstract without searching BibTeX buffers."
  (let* ((db (ebib-db-new-database))
         (ebib--cur-db db)
         fetched)
    (ebib-db-set-entry "Author2020Paper"
                       '(("=type=" . "article") ("abstract" . "Existing")) db)
    (cl-letf (((symbol-function 'ebib-extras-open-key) #'ignore)
              ((symbol-function 'bibtex-extras-get-entry-as-string)
               (lambda (&rest _) (ert-fail "Wrong BibTeX-buffer lookup")))
              ((symbol-function 'tlon-get-abstract-with-or-without-ai)
               (lambda (&rest args) (setq fetched args))))
      (ebib-extras-set-abstract "Author2020Paper")
      (should-not fetched)
      (dolist (empty '("" "{}" "\"\"" "{  }"))
        (setq fetched nil)
        (ebib-db-set-field-value "abstract" empty "Author2020Paper" db 'overwrite)
        (ebib-extras-set-abstract "Author2020Paper")
        (should (equal fetched '(nil t)))))))


;;;; Explicit asynchronous attachment ownership

(defun ebib-extras-test--operation-databases ()
  "Return two real databases with distinct entries sharing one key."
  (let ((a (ebib-db-new-database)) (b (ebib-db-new-database)))
    (dolist (db (list a b))
      (ebib-db-set-entry "Author2020Paper"
                         (copy-tree '(("=type=" . "article") ("title" . "Paper")
                                      ("doi" . "10.1234/paper")
                                      ("langid" . "english")
                                      ("abstract" . "Existing abstract"))) db))
    (list a b)))

(ert-deftest ebib-extras-test-delayed-attachment-retains-database-and-policy ()
  "A late download updates only its original DB after navigation."
  (pcase-let* ((`(,a ,b) (ebib-extras-test--operation-databases))
               (key "Author2020Paper")
               (operation (ebib-extras-make-operation key a t))
               (dir (make-temp-file "ebib-owned-attachment-" t))
               (src (expand-file-name "source.pdf" dir))
               (dest (expand-file-name (concat key ".pdf") dir))
               (paths-dir-pdf-library dir)
               (ebib--cur-db a)
               (ebib--databases (list b a))
               (tlon-languages-properties '((:name "english" :standard "english")))
               (callback nil) (saved nil))
    (unwind-protect
        (progn
          (with-temp-file src (insert "%PDF-1.4 test"))
          (cl-letf (((symbol-function 'annas-archive-download)
                     (lambda (_id complete noninteractive-p)
                       (should noninteractive-p) (setq callback complete)))
                    ((symbol-function 'ebib-extras--save-database)
                     (lambda (db) (push db saved)))
                    ((symbol-function 'ebib-extras--af-postprocess-pdf) #'ignore)
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "Prompt")))
                    ((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "Prompt"))))
            (ebib-extras--annas-archive-download "10.1234/paper" key a operation)
            (should (= (ebib-extras-operation-pending operation) 1))
            (setq ebib--cur-db b)
            (funcall callback 'complete src nil)
            (funcall callback 'complete src nil)
            (should (eq ebib--cur-db b))
            (should (file-exists-p dest))
            (should-not (file-exists-p src))
            (should (equal (expand-file-name
                            (ebib-unbrace (ebib-db-get-field-value "file" key a))) dest))
            (should-not (ebib-db-get-field-value "file" key b 'noerror))
            (should (equal (ebib-db-get-field-value "abstract" key a) "Existing abstract"))
            (should (equal saved (list a)))
            (should (eq (ebib-extras-operation-status operation) 'complete))
            (should (zerop (ebib-extras-operation-pending operation)))))
      (delete-directory dir t))))

(ert-deftest ebib-extras-test-delayed-attachment-blocks-replaced-entry ()
  "A late result cannot attach to a replacement using the original key."
  (let* ((db (car (ebib-extras-test--operation-databases)))
         (ebib--databases (list db))
         (operation (ebib-extras-make-operation "Author2020Paper" db t))
         (callback (ebib-extras--attachment-callback operation)))
    (ebib-db-set-entry "Author2020Paper" '(("title" . "Replacement")) db 'overwrite)
    (cl-letf (((symbol-function 'ebib-extras-attach-file)
               (lambda (&rest _) (ert-fail "Wrong entry attached"))))
      (funcall callback 'complete "/tmp/nonexistent-owned.pdf" nil))
    (should (eq (ebib-extras-operation-status operation) 'blocked))
    (should (string-match-p "replaced" (car (ebib-extras-operation-errors operation))))))

(ert-deftest ebib-extras-test-headless-collision-and-language-never-prompt ()
  "Unknown language and existing destinations fail before moving a file."
  (let* ((db (car (ebib-extras-test--operation-databases)))
         (key "Author2020Paper")
         (ebib--databases (list db))
         (operation (ebib-extras-make-operation key db t))
         (dir (make-temp-file "ebib-collision-" t))
         (paths-dir-pdf-library dir)
         (src (expand-file-name "source.pdf" dir))
         (dest (expand-file-name (concat key ".pdf") dir))
         (tlon-languages-properties '((:name "english" :standard "english"))))
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "Prompt")))
                  ((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "Prompt"))))
          (with-temp-file src (insert "source"))
          (with-temp-file dest (insert "existing"))
          (should-error (ebib-extras-attach-file src key t db operation) :type 'user-error)
          (should (file-exists-p src))
          (with-temp-buffer (insert-file-contents dest) (should (equal (buffer-string) "existing")))
          (ebib-db-set-field-value "langid" nil key db t)
          (should-error (ebib-extras-get-or-set-language key db operation) :type 'user-error))
      (delete-directory dir t))))

(ert-deftest ebib-extras-test-url-callback-has-independent-operation ()
  "URL rendering carries DB identity and reports failure without another prompt."
  (pcase-let* ((`(,a ,b) (ebib-extras-test--operation-databases))
               (key "Author2020Paper")
               (operation (ebib-extras-make-operation key a t))
               (ebib--cur-db a)
               (ebib--databases (list a b))
               (paths-dir-downloads temporary-file-directory)
               (stages nil)
               (complete nil) (failed nil))
    (ebib-db-set-field-value "url" "https://example.com/paper" key a t)
    (cl-letf (((symbol-function 'eww-extras-url-to-file)
               (lambda (_type _url callback _key failure stage)
                 (push stage stages)
                 (setq complete callback failed failure))))
      (ebib-extras-url-to-file-attach "pdf" key a operation))
    (setq ebib--cur-db b)
    (funcall failed "Renderer exited with status 1")
    (funcall complete "/tmp/late.pdf" key)
    (should (eq (ebib-extras-operation-status operation) 'blocked))
    (should (zerop (ebib-extras-operation-pending operation)))
    (should-not (ebib-db-get-field-value "file" key b 'noerror))
    (mapc #'delete-file stages)))

(ert-deftest ebib-extras-test-operation-rejects-closed-and-dirty-database ()
  "Callbacks cannot write closed databases or save unrelated user edits."
  (let* ((db (car (ebib-extras-test--operation-databases)))
         (key "Author2020Paper")
         (ebib--databases (list db))
         (operation (ebib-extras-make-operation key db t)))
    (ebib-db-set-modified t db)
    (should-error (ebib-extras--operation-check operation) :type 'user-error)
    (should (ebib-db-modified-p db))
    (ebib-db-set-modified nil db)
    (setq ebib--databases nil)
    (should-error (ebib-extras--operation-check operation) :type 'user-error)))

(ert-deftest ebib-extras-test-replaced-target-never-reaches-pdf-metadata ()
  "Postprocessing validates identity before looking up or changing a PDF."
  (let* ((db (car (ebib-extras-test--operation-databases)))
         (key "Author2020Paper")
         (ebib--databases (list db))
         (operation (ebib-extras-make-operation key db t)))
    (ebib-db-set-entry key '(("title" . "Replacement")) db 'overwrite)
    (cl-letf (((symbol-function 'ebib-extras-set-pdf-metadata)
               (lambda (&rest _) (ert-fail "Metadata reached replacement"))))
      (should-error (ebib-extras--af-postprocess-pdf key db operation) :type 'user-error))))

(ert-deftest ebib-extras-test-url-stages-distinguish-same-key-databases ()
  "Two render requests cannot overwrite each other's temporary output."
  (let* ((dbs (ebib-extras-test--operation-databases))
         (ebib--databases dbs)
         (paths-dir-downloads temporary-file-directory)
         stages callbacks)
    (unwind-protect
        (cl-letf (((symbol-function 'eww-extras-url-to-file)
                   (lambda (_type _url _callback _key failure stage)
                     (push stage stages) (push failure callbacks))))
          (dolist (db dbs)
            (ebib-db-set-field-value "url" "https://example.com/paper" "Author2020Paper" db t)
            (ebib-extras-url-to-file-attach
             "pdf" "Author2020Paper" db (ebib-extras-make-operation "Author2020Paper" db t)))
          (should (= (length (delete-dups (copy-sequence stages))) 2))
          (dolist (callback callbacks) (funcall callback "Controlled failure")))
      (mapc #'delete-file stages))))

(ert-deftest ebib-extras-test-ocr-completion-tracks-terminal-status-once ()
  "OCR remains pending until exit, including nonfatal already-OCR exit 6."
  (dolist (code '(0 6 2))
    (let* ((db (car (ebib-extras-test--operation-databases)))
           (ebib--databases (list db))
           (operation (ebib-extras-make-operation "Author2020Paper" db t))
           (status 'run) sentinel)
      (cl-letf (((symbol-function 'process-sentinel) (lambda (_) nil))
                ((symbol-function 'set-process-sentinel) (lambda (_ callback) (setq sentinel callback)))
                ((symbol-function 'process-status) (lambda (_) status))
                ((symbol-function 'process-exit-status) (lambda (_) code)))
        (ebib-extras--track-ocr-process 'owned-process operation)
        (funcall sentinel 'owned-process "continued")
        (should (= (ebib-extras-operation-pending operation) 1))
        (setq status 'exit)
        (funcall sentinel 'owned-process "finished")
        (funcall sentinel 'owned-process "duplicate")
        (should (zerop (ebib-extras-operation-pending operation)))
        (should (eq (ebib-extras-operation-status operation)
                    (if (memq code '(0 6)) 'complete 'blocked)))))))

(ert-deftest ebib-extras-test-abstract-cancellation-finishes-owned-task ()
  "Cancellation during fetch or callback save leaves no pending abstract task."
  (dolist (phase '(fetch save))
    (let* ((db (car (ebib-extras-test--operation-databases)))
           (ebib--databases (list db))
           (key "Author2020Paper")
           (operation (ebib-extras-make-operation key db t)))
      (ebib-db-set-field-value "abstract" nil key db t)
      (cl-letf (((symbol-function 'tlon-get-abstract-with-or-without-ai)
                 (lambda (_key _preserve target)
                   (if (eq phase 'fetch) (signal 'quit nil)
                     (funcall (plist-get target :callback) 'complete))))
                ((symbol-function 'ebib-extras--save-database)
                 (lambda (_) (signal 'quit nil))))
        (condition-case nil
            (ebib-extras-set-abstract key db operation "/tmp/verified.pdf")
          (quit nil)))
      (should (zerop (ebib-extras-operation-pending operation)))
      (should (eq (ebib-extras-operation-status operation) 'blocked)))))

(provide 'ebib-extras-test)
;;; ebib-extras-test.el ends here
