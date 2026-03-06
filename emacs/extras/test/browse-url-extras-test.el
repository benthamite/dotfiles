;;; browse-url-extras-test.el --- Tests for browse-url-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in browse-url-extras.el, including
;; URL handler setup and URL file writing.

;;; Code:

(require 'ert)
(require 'browse-url-extras)

;;;; set-handler

(ert-deftest browse-url-extras-test-set-handler-adds-urls ()
  "URLs from a file are added to browse-url-handlers."
  (let ((browse-url-handlers nil)
        (temp-file (make-temp-file "browse-url-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "example.com\ngithub.com\nreddit.com"))
          (browse-url-extras-set-handler temp-file 'browse-url-default-browser)
          (should (= (length browse-url-handlers) 3))
          ;; Each entry should be a cons of (regexp-quoted-url . handler)
          (should (equal (cdar browse-url-handlers) 'browse-url-default-browser))
          ;; All entries should use the same handler
          (dolist (entry browse-url-handlers)
            (should (equal (cdr entry) 'browse-url-default-browser))))
      (delete-file temp-file))))

(ert-deftest browse-url-extras-test-set-handler-regexp-quotes-urls ()
  "URLs are regexp-quoted so dots are literal."
  (let ((browse-url-handlers nil)
        (temp-file (make-temp-file "browse-url-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "example.com"))
          (browse-url-extras-set-handler temp-file 'browse-url-default-browser)
          (let ((pattern (caar browse-url-handlers)))
            ;; The dot should be escaped to literal dot
            (should (string-match-p (regexp-quote ".") pattern))
            ;; Should match the literal URL
            (should (string-match-p pattern "example.com"))
            ;; Should NOT match "exampleXcom" if properly escaped
            (should-not (string-match-p pattern "exampleXcom"))))
      (delete-file temp-file))))

(ert-deftest browse-url-extras-test-set-handler-empty-file ()
  "An empty file should not add any handlers."
  (let ((browse-url-handlers nil)
        (temp-file (make-temp-file "browse-url-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert ""))
          (browse-url-extras-set-handler temp-file 'browse-url-default-browser)
          (should (= (length browse-url-handlers) 0)))
      (delete-file temp-file))))

(ert-deftest browse-url-extras-test-set-handler-appends-to-existing ()
  "New handlers are pushed onto existing browse-url-handlers."
  (let ((browse-url-handlers '(("existing\\.com" . browse-url-firefox)))
        (temp-file (make-temp-file "browse-url-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "new.com"))
          (browse-url-extras-set-handler temp-file 'browse-url-default-browser)
          (should (= (length browse-url-handlers) 2))
          ;; The new entry is pushed to the front
          (should (equal (cdar browse-url-handlers) 'browse-url-default-browser))
          ;; The old entry is still present
          (should (equal (cdr (cadr browse-url-handlers)) 'browse-url-firefox)))
      (delete-file temp-file))))

(ert-deftest browse-url-extras-test-set-handler-skips-empty-lines ()
  "Empty lines in the URL file are ignored (s-split with omit-nulls)."
  (let ((browse-url-handlers nil)
        (temp-file (make-temp-file "browse-url-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "example.com\n\ngithub.com\n\n"))
          (browse-url-extras-set-handler temp-file 'browse-url-default-browser)
          (should (= (length browse-url-handlers) 2)))
      (delete-file temp-file))))

(ert-deftest browse-url-extras-test-set-handler-multiple-handlers ()
  "Different files can set different handlers."
  (let ((browse-url-handlers nil)
        (default-file (make-temp-file "browse-url-default"))
        (firefox-file (make-temp-file "browse-url-firefox")))
    (unwind-protect
        (progn
          (with-temp-file default-file
            (insert "example.com"))
          (with-temp-file firefox-file
            (insert "github.com"))
          (browse-url-extras-set-handler default-file 'browse-url-default-browser)
          (browse-url-extras-set-handler firefox-file 'browse-url-firefox)
          (should (= (length browse-url-handlers) 2))
          ;; Most recently pushed is github.com with firefox
          (should (equal (cdar browse-url-handlers) 'browse-url-firefox))
          ;; The other entry is example.com with default browser
          (should (equal (cdr (cadr browse-url-handlers)) 'browse-url-default-browser)))
      (delete-file default-file)
      (delete-file firefox-file))))

;;;; write-url-to-file

(ert-deftest browse-url-extras-test-write-url-to-file-appends ()
  "Writing a URL appends it to the file."
  (let ((temp-file (make-temp-file "browse-url-write")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "existing.com"))
          (browse-url-extras-write-url-to-file "new.com" temp-file)
          (let ((content (with-temp-buffer
                           (insert-file-contents temp-file)
                           (buffer-string))))
            (should (string-match-p "existing\\.com" content))
            (should (string-match-p "new\\.com" content))))
      (delete-file temp-file))))

(ert-deftest browse-url-extras-test-write-url-to-file-sorts ()
  "URLs in the file are sorted alphabetically after writing."
  (let ((temp-file (make-temp-file "browse-url-write")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "zebra.com\napple.com"))
          (browse-url-extras-write-url-to-file "mango.com" temp-file)
          (let ((lines (with-temp-buffer
                         (insert-file-contents temp-file)
                         (split-string (buffer-string) "\n" t))))
            ;; Should be sorted
            (should (equal lines (sort (copy-sequence lines) #'string<)))))
      (delete-file temp-file))))

(ert-deftest browse-url-extras-test-write-url-to-file-deduplicates ()
  "Duplicate URLs are removed after writing."
  (let ((temp-file (make-temp-file "browse-url-write")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "example.com"))
          (browse-url-extras-write-url-to-file "example.com" temp-file)
          (let ((lines (with-temp-buffer
                         (insert-file-contents temp-file)
                         (split-string (buffer-string) "\n" t))))
            ;; Should have only one occurrence
            (should (= (length (seq-filter
                                (lambda (l) (equal l "example.com"))
                                lines))
                       1))))
      (delete-file temp-file))))

(provide 'browse-url-extras-test)
;;; browse-url-extras-test.el ends here
