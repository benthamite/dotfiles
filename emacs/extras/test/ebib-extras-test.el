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

(defconst ebib-extras-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

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
                               (copy-tree '(("=type=" . "article") ("title" . "Paper"))) db)
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

(ert-deftest ebib-extras-test-update-file-field-without-existing-field ()
  "A first attachment must not fail when the entry has no file field yet."
  (ebib-extras-test--with-save-database
   (lambda (db _file)
     (cl-letf (((symbol-function 'run-with-timer) #'ignore))
       (should-not (ebib-db-get-field-value "file" "Author2020Paper" db 'noerror))
       (ebib-extras--update-file-field-contents
        "Author2020Paper" "~/library/Author2020Paper.pdf" db)
       (should (equal (ebib-unbrace
                       (ebib-db-get-field-value "file" "Author2020Paper" db))
                      "~/library/Author2020Paper.pdf"))))))

(ert-deftest ebib-extras-test-save-database-without-ebib-buffers ()
  "Saving must work headlessly, before Ebib has created its index buffer."
  (let* ((file (make-temp-file "ebib-headless-save-" nil ".bib"))
         (db (ebib-db-new-database))
         (ebib--buffer-alist nil)
         (ebib--cur-db nil)
         (ebib--databases (list db)))
    (unwind-protect
        (progn
          (ebib-db-set-filename file db)
          (ebib-db-set-backup nil db)
          (ebib-db-set-entry "Author2020Paper"
                             '(("=type=" . "article") ("title" . "Paper")) db)
          (ebib-db-set-modtime (ebib--get-file-modtime file) db)
          (ebib-db-set-modified t db)
          (ebib-extras--save-database db)
          (should-not (ebib-db-modified-p db))
          (should (equal (ebib-db-get-modtime db) (ebib--get-file-modtime file)))
          (with-temp-buffer
            (insert-file-contents file)
            (should (search-forward "Author2020Paper" nil t))))
      (delete-file file))))

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

;;;; File notification reloads

(defun ebib-extras-test--with-watched-database (function)
  "Call FUNCTION with an isolated saved database and its watched file."
  (let* ((file (make-temp-file "ebib-watch-test-" nil ".bib"))
         (db (ebib-db-new-database))
         (index (generate-new-buffer " *ebib-watch-index*"))
         (log (generate-new-buffer " *ebib-watch-log*"))
         (ebib--cur-db db)
         (ebib--databases (list db))
         (ebib--buffer-alist `((index . ,index) (log . ,log)))
         (ebib-extras-last-reload-times (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (ebib-db-set-filename file db)
          (ebib-db-set-buffer index db)
          (ebib-db-set-backup nil db)
          (ebib-db-set-entry "Says2010OnKeepingLogbook"
                             (copy-tree '(("=type=" . "online")
                                          ("title" . "{On keeping a logbook}"))) db)
          (with-current-buffer index
            (insert (propertize "Says2010OnKeepingLogbook" 'ebib-key
                                "Says2010OnKeepingLogbook"))
            (goto-char (point-min)))
          (ebib-db-set-modtime (ebib--get-file-modtime file) db)
          (ebib-db-set-modified t db)
          (ebib-extras--save-database db)
          (funcall function db file))
      (when-let ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (kill-buffer index)
      (kill-buffer log)
      (delete-file file))))

(defun ebib-extras-test--write-external-change (file)
  "Write a distinguishable external bibliography change to FILE."
  (with-temp-file file
    (insert "@online{Says2010OnKeepingLogbook, title = {External title}}\n"))
  (set-file-times file (time-add (current-time) 2)))

(ert-deftest ebib-extras-test-own-save-notification-preserves-operation ()
  "A renamed entry's own save must not invalidate its pending attachment."
  (ebib-extras-test--with-watched-database
   (lambda (db file)
     (let ((key "Kleon2010OnKeepingLogbook"))
       (ebib-db-change-key "Says2010OnKeepingLogbook" key db)
       (ebib-db-set-modified t db)
       (ebib-extras--save-database db)
       (let* ((entry (ebib-db-get-entry key db))
              (operation (ebib-extras-make-operation key db t)))
         (ebib-extras--operation-start operation)
         (ebib-extras--auto-reload-callback (list nil 'changed file) 0 db file)
         (should (eq entry (ebib-db-get-entry key db)))
         (ebib-extras--operation-check operation)
         (ebib-extras--operation-finish operation 'complete)
         (should (eq (ebib-extras-operation-status operation) 'complete))
         (should (zerop (ebib-extras-operation-pending operation)))
         (should-not (gethash file ebib-extras-last-reload-times)))))))

(ert-deftest ebib-extras-test-external-notification-reloads-current-file ()
  "An external change refreshes the visiting buffer before reparsing it."
  (ebib-extras-test--with-watched-database
   (lambda (db file)
     (let ((other (ebib-db-new-database)))
       (ebib-extras-test--write-external-change file)
       (let ((ebib--cur-db other))
         (ebib-extras--auto-reload-callback (list nil 'changed file) 0 db file)
         (should (eq ebib--cur-db other)))
       (should (equal (ebib-unbrace
                       (ebib-db-get-field-value "title"
                                                "Says2010OnKeepingLogbook" db))
                      "External title"))
       (should (equal (ebib-db-get-modtime db) (ebib--get-file-modtime file)))
       (should (gethash file ebib-extras-last-reload-times))))))

(ert-deftest ebib-extras-test-external-notification-preserves-dirty-state ()
  "Background notifications never discard or prompt about unsaved edits."
  (dolist (dirty '(database buffer))
    (ebib-extras-test--with-watched-database
     (lambda (db file)
       (let ((entry (ebib-db-get-entry "Says2010OnKeepingLogbook" db))
             (saved-modtime (ebib-db-get-modtime db)))
         (if (eq dirty 'database)
             (progn
               (ebib-db-set-field-value "title" "Local title"
                                        "Says2010OnKeepingLogbook" db t)
               (ebib-db-set-modified t db))
           (with-current-buffer (find-buffer-visiting file)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert "\n% Unsaved local edit\n"))))
         (ebib-extras-test--write-external-change file)
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (&rest _) (ert-fail "Background reload prompted"))))
           (ebib-extras--auto-reload-callback (list nil 'changed file) 0 db file))
         (should (eq entry (ebib-db-get-entry "Says2010OnKeepingLogbook" db)))
         (should (equal saved-modtime (ebib-db-get-modtime db)))
         (should-not (gethash file ebib-extras-last-reload-times))
         (if (eq dirty 'database)
             (progn
               (should (ebib-db-modified-p db))
               (should (equal (ebib-db-get-field-value "title"
                                                      "Says2010OnKeepingLogbook" db)
                              "Local title")))
           (with-current-buffer (find-buffer-visiting file)
             (should (buffer-modified-p))
             (should (string-match-p "Unsaved local edit" (buffer-string))))))))))

(ert-deftest ebib-extras-test-external-notification-retains-cooldown ()
  "A recent successful reload still suppresses rapid repeated notifications."
  (ebib-extras-test--with-watched-database
   (lambda (db file)
     (let ((entry (ebib-db-get-entry "Says2010OnKeepingLogbook" db))
           (last-reload (current-time)))
       (puthash file last-reload ebib-extras-last-reload-times)
       (ebib-extras-test--write-external-change file)
       (ebib-extras--auto-reload-callback (list nil 'changed file) 0 db file)
       (should (eq entry (ebib-db-get-entry "Says2010OnKeepingLogbook" db)))
       (should (equal last-reload (gethash file ebib-extras-last-reload-times)))))))

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

(ert-deftest ebib-extras-test-headless-books-require-reviewed-attachments ()
  "Unattached books and ISBN entries cannot acquire unreviewed downloads."
  (dolist (fields '(("book" nil nil) ("Book" nil "10.1234/book")
                    ("misc" "9780262045841" nil)))
    (let* ((db (apply #'ebib-extras-test--book-database fields))
           (ebib--databases (list db))
           (key "Author2020Book")
           (operation (ebib-extras-make-operation key db t))
           (tlon-languages-properties '((:name "english" :standard "english")))
           downloaded)
      (cl-letf (((symbol-function 'annas-archive-download)
                 (lambda (&rest _) (setq downloaded t)))
                ((symbol-function 'read-string)
                 (lambda (&rest _) (ert-fail "Headless book prompted"))))
        (should (equal (ebib-extras-process-entry key db operation) key)))
      (should-not downloaded)
      (should (eq (ebib-extras-operation-status operation) 'blocked))
      (should (zerop (ebib-extras-operation-pending operation)))
      (should (equal (ebib-extras-operation-errors operation)
                     '("Book attachment requires a reviewed local PDF")))
      (should-not (ebib-db-get-field-value "file" key db 'noerror)))))

(ert-deftest ebib-extras-test-direct-headless-book-download-is-blocked ()
  "Calling the legacy book command directly cannot bypass the review guard."
  (let* ((db (ebib-extras-test--book-database "book"))
         (ebib--databases (list db))
         (operation (ebib-extras-make-operation "Author2020Book" db t))
         downloaded)
    (cl-letf (((symbol-function 'annas-archive-download)
               (lambda (&rest _) (setq downloaded t))))
      (ebib-extras-book-attach "Author2020Book" db operation))
    (should-not downloaded)
    (should (eq (ebib-extras-operation-status operation) 'blocked))
    (should (zerop (ebib-extras-operation-pending operation)))))

(ert-deftest ebib-extras-test-attached-headless-book-still-processes ()
  "An existing reviewed attachment follows ordinary metadata processing."
  (let* ((db (ebib-extras-test--book-database "book" nil "10.1234/book"))
         (ebib--databases (list db))
         (key "Author2020Book")
         (operation (ebib-extras-make-operation key db t))
         (tlon-languages-properties '((:name "english" :standard "english")))
         (file (make-temp-file "ebib-reviewed-book-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (progn
          (ebib-db-set-field-value "file" file key db t)
          (cl-letf (((symbol-function 'annas-archive-download)
                     (lambda (&rest _) (ert-fail "Existing book downloaded again"))))
            (should (equal (ebib-extras-process-entry key db operation) key)))
          (should (eq (ebib-extras-operation-status operation) 'complete))
          (should (zerop (ebib-extras-operation-pending operation)))
          (should (equal (ebib-db-get-field-value "file" key db) file))
          (should (equal (ebib-db-get-field-value "abstract" key db)
                         "Original abstract")))
      (delete-file file))))

(ert-deftest ebib-extras-test-blocked-book-still-checks-crossref ()
  "Blocking acquisition does not skip the final metadata checks."
  (let* ((db (ebib-extras-test--book-database "incollection"))
         (ebib--databases (list db))
         (key "Author2020Book")
         (operation (ebib-extras-make-operation key db t))
         (tlon-languages-properties '((:name "english" :standard "english"))))
    (ebib-db-set-field-value "publisher" "Hardcoded parent publisher" key db t)
    (cl-letf (((symbol-function 'annas-archive-download) #'ignore))
      (let ((error (should-error (ebib-extras-process-entry key db operation)
                                 :type 'user-error)))
        (should (string-match-p "should use.*crossref" (error-message-string error)))))
    (should (eq (ebib-extras-operation-status operation) 'blocked))
    (should (zerop (ebib-extras-operation-pending operation)))))

(ert-deftest ebib-extras-test-manual-book-download-and-doi-precedence ()
  "Manual books retain their search prompt and existing DOI preference."
  (let* ((db (ebib-extras-test--book-database "book" "9780262045841"))
         (ebib--databases (list db))
         (key "Author2020Book")
         prompted downloaded doi-requested)
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt initial &rest _)
                 (setq prompted initial) "Chosen search"))
              ((symbol-function 'ebib-extras--annas-archive-download)
               (lambda (id &rest _) (setq downloaded id)))
              ((symbol-function 'ebib-extras-doi-attach)
               (lambda (&rest _) (setq doi-requested t))))
      (ebib-extras-attach-files key db)
      (should (equal prompted "9780262045841"))
      (should (equal downloaded "Chosen search"))
      (setq prompted nil downloaded nil)
      (ebib-db-set-field-value "doi" "10.1234/book" key db t)
      (ebib-extras-attach-files key db)
      (should doi-requested)
      (should-not prompted)
      (should-not downloaded))))

(defun ebib-extras-test--book-database (type &optional isbn doi)
  "Return a book fixture with TYPE and optional ISBN and DOI."
  (let ((db (ebib-db-new-database)))
    (ebib-db-set-entry "Author2020Book"
                       (list (cons "=type=" type) '("title" . "Book")
                             '("langid" . "english") '("abstract" . "Original abstract")) db)
    (when isbn (ebib-db-set-field-value "isbn" isbn "Author2020Book" db t))
    (when doi (ebib-db-set-field-value "doi" doi "Author2020Book" db t))
    db))

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

(ert-deftest ebib-extras-test-failed-url-render-discards-stage-and-reports ()
  "A failed render removes its empty staging file and tells an interactive user."
  (dolist (noninteractive-p '(nil t))
    (let* ((db (car (ebib-extras-test--operation-databases)))
           (key "Author2020Paper")
           (operation (ebib-extras-make-operation key db noninteractive-p))
           (ebib--cur-db db)
           (ebib--databases (list db))
           (paths-dir-downloads temporary-file-directory)
           (stage nil) (failed nil) (messages nil))
      (ebib-db-set-field-value "url" "https://example.com/paper" key db t)
      (cl-letf (((symbol-function 'eww-extras-url-to-file)
                 (lambda (_type _url _callback _key failure path)
                   (setq stage path failed failure)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (ebib-extras-url-to-file-attach "html" key db operation)
        (should (file-regular-p stage))
        (funcall failed "verification: unresolved consent blocker"))
      (should-not (file-exists-p stage))
      (should (equal (ebib-extras-operation-errors operation)
                     '("verification: unresolved consent blocker")))
      (if noninteractive-p
          (should-not messages)
        (should (equal messages
                       '("Author2020Paper: verification: unresolved consent blocker")))))))

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

(defmacro ebib-extras-test-with-ocr-attachment (&rest body)
  "Run BODY with an owned PDF, saved database and attachment operation."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "ebib-ocr-order-" t))
          (source (expand-file-name "source.pdf" directory))
          (bibfile (expand-file-name "fixture.bib" directory))
          (databases (ebib-extras-test--operation-databases))
          (db (car databases)) (other (cadr databases))
          (key "Author2020Paper")
          (ebib--databases databases) (ebib--cur-db db)
          (ebib--buffer-alist nil) (ebib--needs-update nil)
          (paths-dir-pdf-library directory) (paths-dir-html-library directory)
          (ebib-extras--operations (make-hash-table :test #'equal))
          (tlon-languages-properties '((:name "english" :standard "english"
                                        :iso-639-2 "eng")))
          (ocr-buffer (generate-new-buffer " *ebib-ocr-order*"))
          ocr-process operation)
     (unwind-protect
         (progn
           (copy-file (expand-file-name "fixtures/files-extras-ocr-mixed.pdf"
                                       ebib-extras-test--directory) source)
           (write-region "" nil bibfile nil 'silent)
           (ebib-db-set-filename bibfile db)
           (ebib-db-set-backup nil db)
           (ebib-db-set-field-value "abstract" nil key db t)
           (ebib-db-set-field-value "author" "{Author, Example}" key db t)
           (ebib-db-set-modtime (ebib--get-file-modtime bibfile) db)
           (ebib-db-set-modified t db)
           (ebib-extras--save-database db)
           (setq operation (ebib-extras-make-operation key db t))
           ,@body)
       (when (processp ocr-process)
         (set-process-sentinel ocr-process #'ignore)
         (when (process-live-p ocr-process) (delete-process ocr-process)))
       (kill-buffer ocr-buffer)
       (when-let ((buffer (find-buffer-visiting bibfile)))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-directory directory t))))

(ert-deftest ebib-extras-test-image-only-abstract-waits-for-owned-ocr ()
  "Extract real OCR text only after completion, despite intervening processing."
  (dolist (command '("qpdf" "pdftotext" "ocrmypdf" "pdftk"))
    (skip-unless (executable-find command)))
  (ebib-extras-test-with-ocr-attachment
    (let ((image-only (expand-file-name "image-only.pdf" directory))
          (start (symbol-function 'start-process-shell-command))
          target text pending-at-request)
      (should (zerop (process-file "qpdf" nil nil nil source "--pages" "." "2"
                                   "--" image-only)))
      (setq source image-only)
      (with-temp-buffer
        (should (zerop (process-file "pdftotext" nil t nil source "-")))
        (should (equal (buffer-string) "\f")))
      (cl-letf (((symbol-function 'start-process-shell-command)
                 (lambda (name _buffer command)
                   (let ((process-connection-type nil))
                     (setq ocr-process
                           (funcall start name ocr-buffer
                                    (concat "read task_ready; " command))))))
                ((symbol-function 'tlon-get-abstract-with-or-without-ai)
                 (lambda (_interactive _preserve captured)
                   (should-not target)
                   (setq target captured
                         pending-at-request (ebib-extras-operation-pending operation)
                         text (with-temp-buffer
                                (should (zerop (process-file
                                                 "pdftotext" nil t nil
                                                 (plist-get captured :file) "-")))
                                (buffer-string))))))
        (ebib-extras-attach-file source key t db operation)
        (should-not target)
        (should (= (ebib-extras-operation-pending operation) 1))
        (ebib-extras-process-entry key db operation)
        (should-not target)
        (setq ebib--cur-db other)
        (process-send-string ocr-process "ready\n")
        (ebib-extras-test--wait-for-ocr ocr-process)
        (should (zerop (process-exit-status ocr-process)))
        (should (string-match-p "SCANNED ONLY PAGE" text))
        (should (eq (plist-get target :db) db))
        (should (equal (plist-get target :key) key))
        (should (= pending-at-request 2))
        (should (= (ebib-extras-operation-pending operation) 1))
        (ebib-db-set-field-value "abstract" "{Fixture abstract}" key db t)
        (ebib-db-set-modified t db)
        (funcall (plist-get target :callback) 'complete)
        (funcall (plist-get target :callback) 'complete)
        (should (zerop (ebib-extras-operation-pending operation)))
        (should (eq (ebib-extras-operation-status operation) 'complete))
        (should (equal (ebib-db-get-field-value "abstract" key other)
                       "Existing abstract"))))))

(defun ebib-extras-test--wait-for-ocr (process)
  "Drain owned PROCESS until exit, bounded to 20 seconds."
  (let ((deadline (+ (float-time) 20)))
    (while (and (process-live-p process) (< (float-time) deadline))
      (accept-process-output process 0.1)))
  (accept-process-output process 0.01)
  (should-not (process-live-p process)))

(ert-deftest ebib-extras-test-ocr-failure-never-starts-dependent-abstract ()
  "OCR startup, exit and retained-target failures cannot leak abstract work."
  (dolist (failure '(metadata no-process startup exit replaced closed dirty sentinel))
    (ebib-extras-test-with-ocr-attachment
      (let (requested)
        (cl-letf (((symbol-function 'ebib-extras-set-pdf-metadata)
                   (lambda (&rest _) (when (eq failure 'metadata) (error "Metadata failed"))))
                  ((symbol-function 'files-extras-ocr-pdf)
                   (lambda (&rest _)
                     (pcase failure
                       ('no-process nil)
                       ('startup (error "OCR startup failed"))
                       (_ (setq ocr-process
                                (make-process :name "owned-ocr-failure" :buffer ocr-buffer
                                  :command (list "sh" "-c"
                                    (format "read task_ready; exit %d"
                                            (if (eq failure 'exit) 2 0)))
                                  :connection-type 'pipe :noquery t))
                          (when (eq failure 'sentinel)
                            (set-process-sentinel ocr-process
                              (lambda (&rest _) (error "Original sentinel failed"))))
                          ocr-process))))
                  ((symbol-function 'tlon-get-abstract-with-or-without-ai)
                   (lambda (&rest _) (setq requested t))))
          (if (memq failure '(metadata no-process startup))
              (should-error (ebib-extras-attach-file source key t db operation))
            (ebib-extras-attach-file source key t db operation)
            (should-not requested)
            (pcase failure
              ('replaced (ebib-db-set-entry key '(("title" . "Replacement")) db 'overwrite))
              ('closed (setq ebib--databases (list other)))
              ('dirty (ebib-db-set-modified t db)))
            (process-send-string ocr-process "ready\n")
            (ebib-extras-test--wait-for-ocr ocr-process))
          (should-not requested)
          (should (zerop (ebib-extras-operation-pending operation)))
          (should (eq (ebib-extras-operation-status operation) 'blocked))
          (when (memq failure '(metadata no-process startup exit sentinel))
            (ebib-extras-process-entry key db operation)
            (should-not requested)))))))

(ert-deftest ebib-extras-test-existing-or-late-abstract-survives-ocr ()
  "Existing abstracts and fields filled during OCR suppress generation."
  (dolist (late '(nil t))
    (ebib-extras-test-with-ocr-attachment
      (unless late
        (ebib-db-set-field-value "abstract" "{Keep this abstract}" key db t)
        (ebib-db-set-modified t db)
        (ebib-extras--save-database db))
      (cl-letf (((symbol-function 'ebib-extras-set-pdf-metadata) #'ignore)
                ((symbol-function 'files-extras-ocr-pdf)
                 (lambda (&rest _)
                   (setq ocr-process (make-process :name "owned-ocr-preserve"
                     :buffer ocr-buffer :command '("cat") :connection-type 'pipe :noquery t))))
                ((symbol-function 'tlon-get-abstract-with-or-without-ai)
                 (lambda (&rest _) (ert-fail "An existing abstract was ignored"))))
        (ebib-extras-attach-file source key t db operation)
        (when late
          (ebib-db-set-field-value "abstract" "{Keep this abstract}" key db t)
          (ebib-db-set-modified t db)
          (ebib-extras--save-database db))
        (process-send-eof ocr-process)
        (ebib-extras-test--wait-for-ocr ocr-process)
        (should (equal (ebib-unbrace (ebib-db-get-field-value "abstract" key db))
                       "Keep this abstract"))
        (should (eq (ebib-extras-operation-status operation) 'complete))
        (should (zerop (ebib-extras-operation-pending operation)))))))

(ert-deftest ebib-extras-test-attachments-without-ocr-process-abstract-immediately ()
  "PDF opt-out and non-PDF attachments keep immediate abstract processing."
  (dolist (type '(pdf html))
    (ebib-extras-test-with-ocr-attachment
      (when (eq type 'html)
        (setq source (expand-file-name "source.html" directory))
        (with-temp-file source (insert "<html><body>Source text</body></html>")))
      (let (requested)
        (cl-letf (((symbol-function 'files-extras-ocr-pdf)
                   (lambda (&rest _) (ert-fail "Unexpected OCR")))
                  ((symbol-function 'tlon-get-abstract-with-or-without-ai)
                   (lambda (_interactive _preserve target)
                     (setq requested t)
                     (funcall (plist-get target :callback) 'complete))))
          (ebib-extras-attach-file source key (eq type 'html) db operation)
          (should requested)
          (should (eq (ebib-extras-operation-status operation) 'complete))
          (should (zerop (ebib-extras-operation-pending operation))))))))

(ert-deftest ebib-extras-test-pdf-ocr-preserves-pending-html-abstract ()
  "Completing PDF OCR cannot replace an earlier HTML abstract request."
  (ebib-extras-test-with-ocr-attachment
    (let ((html (expand-file-name "source.html" directory))
          (requests 0) target)
      (with-temp-file html (insert "<html><body>Source text</body></html>"))
      (cl-letf (((symbol-function 'ebib-extras-set-pdf-metadata) #'ignore)
                ((symbol-function 'files-extras-ocr-pdf)
                 (lambda (&rest _)
                   (setq ocr-process (make-process :name "owned-ocr-html-first"
                     :buffer ocr-buffer :command '("cat") :connection-type 'pipe :noquery t))))
                ((symbol-function 'tlon-get-abstract-with-or-without-ai)
                 (lambda (_interactive _preserve captured)
                   (cl-incf requests)
                   (setq target captured))))
        (ebib-extras-attach-file html key t db operation)
        (should (= requests 1))
        (should (= (ebib-extras-operation-pending operation) 1))
        (ebib-extras-attach-file source key t db operation)
        (should (eq (ebib-extras-operation-abstract-started operation) t))
        (should (= (ebib-extras-operation-pending operation) 2))
        (process-send-eof ocr-process)
        (ebib-extras-test--wait-for-ocr ocr-process)
        (should (= requests 1))
        (should (= (ebib-extras-operation-pending operation) 1))
        (ebib-db-set-field-value "abstract" "{HTML abstract}" key db t)
        (ebib-db-set-modified t db)
        (funcall (plist-get target :callback) 'complete)
        (should (eq (ebib-extras-operation-status operation) 'complete))
        (should (zerop (ebib-extras-operation-pending operation)))))))

(ert-deftest ebib-extras-test-ocr-continuation-finishes-once ()
  "Terminal OCR and continuation errors finish once without replaying sentinels."
  (dolist (scenario '((exit 0 nil) (exit 6 nil) (exit 2 nil) (signal 6 nil)
                      (exit 0 error) (exit 0 quit)))
    (pcase-let* ((`(,status ,code ,failure) scenario)
                 (db (car (ebib-extras-test--operation-databases)))
                 (ebib--databases (list db))
                 (operation (ebib-extras-make-operation "Author2020Paper" db t))
                 (calls 0) (original-calls 0) (sentinel nil))
      (cl-letf (((symbol-function 'process-sentinel)
                 (lambda (_) (lambda (&rest _) (cl-incf original-calls))))
                ((symbol-function 'set-process-sentinel)
                 (lambda (_ callback) (setq sentinel callback)))
                ((symbol-function 'process-status) (lambda (_) status))
                ((symbol-function 'process-exit-status) (lambda (_) code)))
        (condition-case nil
            (ebib-extras--track-ocr-process
             'finished-process operation
             (lambda ()
               (cl-incf calls)
               (when failure (signal failure '("Continuation failed")))))
          (quit nil))
        (funcall sentinel 'finished-process "duplicate")
        (should (zerop original-calls))
        (should (= calls (if (and (eq status 'exit) (memq code '(0 6))) 1 0)))
      (should (zerop (ebib-extras-operation-pending operation)))
      (should (eq (ebib-extras-operation-status operation)
                  (if (and (eq status 'exit) (memq code '(0 6)) (not failure))
                      'complete 'blocked)))))))

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

(ert-deftest ebib-extras-test-postprocessing-displays-only-manual-operations ()
  "Headless attachment retains metadata and OCR without entering PDF view."
  (dolist (headless '(nil t))
    (let* ((db (car (ebib-extras-test--operation-databases)))
           (ebib--databases (list db))
           (key "Author2020Paper")
           (file "/tmp/Author2020Paper.pdf")
           (pdf-view-mode-hook nil)
           (operation (ebib-extras-make-operation key db headless))
           metadata ocr opened)
      (ebib-set-field-value "file" file key db 'overwrite)
      (cl-letf (((symbol-function 'ebib-extras-set-pdf-metadata)
                 (lambda (actual-key actual-db)
                   (setq metadata (list actual-key actual-db))))
                ((symbol-function 'files-extras-ocr-pdf)
                 (lambda (_force actual-file _parameters language)
                   (setq ocr (list actual-file language)) nil))
                ((symbol-function 'find-file)
                 (lambda (actual-file) (setq opened actual-file))))
        (ebib-extras--af-postprocess-pdf key db operation))
      (should (equal metadata (list key db)))
      (should (equal ocr (list file "english")))
      (should (equal opened (unless headless file))))))

(ert-deftest ebib-extras-test-pdf-metadata-private-process-and-cleanup ()
  "Metadata runs without a shell/display and cleans success/failure artifacts."
  (dolist (exit-status '(0 7))
    (let* ((db (car (ebib-extras-test--operation-databases)))
           (key "Author2020Paper")
           (dir (make-temp-file "ebib-metadata-" t))
           (file (expand-file-name "quoted ' paper.pdf" dir))
           artifacts diagnostics)
      (unwind-protect
          (progn
            (with-temp-file file (insert "original PDF"))
            (ebib-set-field-value "file" file key db 'overwrite)
            (ebib-set-field-value "author" "Author, Alice" key db 'overwrite)
            (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/pdftk"))
                      ((symbol-function 'shell-command)
                       (lambda (&rest _) (ert-fail "Interactive shell path reached")))
                      ((symbol-function 'process-file)
                       (lambda (program input destination display &rest arguments)
                         (should (equal program "/usr/bin/pdftk"))
                         (should-not input)
                         (should-not display)
                         (should (equal (car arguments) file))
                         (setq diagnostics (car destination)
                               artifacts (list (nth 2 arguments) (nth 4 arguments)))
                         (should (buffer-live-p diagnostics))
                         (with-current-buffer diagnostics (insert "Controlled failure"))
                         (with-temp-file (nth 4 arguments) (insert "rewritten PDF"))
                         exit-status)))
              (if (zerop exit-status)
                  (ebib-extras-set-pdf-metadata key db)
                (should-error (ebib-extras-set-pdf-metadata key db))))
            (with-temp-buffer
              (insert-file-contents file)
              (should (equal (buffer-string)
                             (if (zerop exit-status) "rewritten PDF" "original PDF"))))
            (should (= (length artifacts) 2))
            (should-not (seq-some #'file-exists-p artifacts))
            (should-not (buffer-live-p diagnostics)))
        (dolist (artifact artifacts)
          (when (file-exists-p artifact) (delete-file artifact)))
        (delete-directory dir t)))))

(provide 'ebib-extras-test)
;;; ebib-extras-test.el ends here
