;;; eww-extras-test.el --- Tests for eww-extras -*- lexical-binding: t -*-

;; Tests for URL command construction and readable exceptions
;; in eww-extras.el.

;;; Code:

(require 'ert)
(require 'eww-extras)

;;;; URL-to-file command construction

(ert-deftest eww-extras-test-make-command-pdf ()
  "Make-command constructs a PDF download command."
  (let ((browse-url-chrome-program "/usr/bin/chrome"))
    (let ((cmd (eww-extras-url-to-file-make-command
                "https://example.com" "/tmp/out.pdf" "pdf")))
      (should (listp cmd))
      (should (= (length cmd) 3))
      (should (string-match-p "print-to-pdf" (nth 2 cmd)))
      (should (string-match-p "example.com" (nth 2 cmd))))))

(ert-deftest eww-extras-test-make-command-html ()
  "Make-command constructs an HTML download command."
  (let ((browse-url-chrome-program "/usr/bin/chrome"))
    (let ((cmd (eww-extras-url-to-file-make-command
                "https://example.com" "/tmp/out.html" "html")))
      (should (listp cmd))
      (should (string-match-p "dump-dom" (nth 2 cmd))))))

(ert-deftest eww-extras-test-make-command-invalid-type ()
  "Make-command signals error for invalid type."
  (let ((browse-url-chrome-program "/usr/bin/chrome"))
    (should-error (eww-extras-url-to-file-make-command
                   "https://example.com" "/tmp/out.xyz" "xyz"))))

;;;; Run callback

(ert-deftest eww-extras-test-run-callback-with-fn ()
  "Run-callback calls the function when provided."
  (let ((called nil))
    (eww-extras-run-callback
     (lambda (file key) (setq called (list file key)))
     "/tmp/test.pdf" "smith2024")
    (should (equal called '("/tmp/test.pdf" "smith2024")))))

(ert-deftest eww-extras-test-run-callback-nil ()
  "Run-callback does nothing when callback is nil."
  (should-not (eww-extras-run-callback nil "/tmp/test.pdf" "key")))

;;;; Readable exceptions from file

(ert-deftest eww-extras-test-set-readable-exceptions-from-file ()
  "Set-readable-exceptions-from-file populates the exception list."
  (let* ((tmpfile (make-temp-file "eww-test-exceptions"))
         (eww-extras-readable-exceptions-file tmpfile)
         (eww-extras-readable-exceptions nil))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "example.com\ngithub.com\n"))
          (eww-extras-set-readable-exceptions-from-file)
          (should (= (length eww-extras-readable-exceptions) 2))
          ;; Each entry should be regexp-quoted
          (should (cl-every #'stringp eww-extras-readable-exceptions)))
      (delete-file tmpfile))))

(ert-deftest eww-extras-test-set-readable-exceptions-missing-file ()
  "Set-readable-exceptions-from-file does nothing when file is missing."
  (let ((eww-extras-readable-exceptions-file "/nonexistent/file")
        (eww-extras-readable-exceptions '("existing")))
    (eww-extras-set-readable-exceptions-from-file)
    ;; Should not have changed the variable
    (should (equal eww-extras-readable-exceptions '("existing")))))

;;;; Readable autoview

(ert-deftest eww-extras-test-readable-autoview-exception ()
  "Readable-autoview skips readability for URLs in exception list."
  (let ((eww-extras-readable-exceptions '("example\\.com")))
    (with-temp-buffer
      ;; Simulate eww buffer with URL
      (let ((eww-data (list :url "https://example.com/page" :source "<p>test</p>")))
        ;; Should not error, should skip eww-readable
        (eww-extras-readable-autoview)))))

(provide 'eww-extras-test)
;;; eww-extras-test.el ends here
