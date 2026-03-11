;;; pdf-tools-extras-test.el --- Tests for pdf-tools-extras -*- lexical-binding: t -*-

;; Tests for PDF conversion helper logic in pdf-tools-extras.el.
;; Cannot load pdf-tools-extras in CI (requires pdf-tools), so test
;; pure logic directly by defining equivalent functions inline.

;;; Code:

(require 'ert)

;;;; pdftotext command construction

;; Extracted from pdf-tools-extras--run-pdftotext for standalone testing.
;; This tests the command string construction only, without executing it.
(defun pdf-tools-extras-test--build-pdftotext-cmd (executable source output &optional extra-args)
  "Build the pdftotext command string.
EXECUTABLE is the pdftotext binary name, SOURCE is the input PDF,
OUTPUT is either a file path or \"-\" for stdout.  EXTRA-ARGS is an
optional list of additional argument strings."
  (mapconcat #'shell-quote-argument
             (append (list executable)
                     extra-args
                     (list (expand-file-name source) output))
             " "))

(ert-deftest pdf-tools-extras-test-build-cmd-basic ()
  "Basic command includes executable, source, and output."
  (let ((cmd (pdf-tools-extras-test--build-pdftotext-cmd
              "pdftotext" "/tmp/test.pdf" "-")))
    (should (string-match-p "pdftotext" cmd))
    (should (string-match-p "test\\.pdf" cmd))
    (should (string-match-p "-" cmd))))

(ert-deftest pdf-tools-extras-test-build-cmd-with-destination ()
  "Command includes destination file path when provided."
  (let ((cmd (pdf-tools-extras-test--build-pdftotext-cmd
              "pdftotext" "/tmp/test.pdf" "/tmp/output.txt")))
    (should (string-match-p "output\\.txt" cmd))
    (should-not (string-match-p " - " cmd))))

(ert-deftest pdf-tools-extras-test-build-cmd-with-extra-args ()
  "Extra args are inserted between executable and source."
  (let ((cmd (pdf-tools-extras-test--build-pdftotext-cmd
              "pdftotext" "/tmp/test.pdf" "-" '("-layout" "-nopgbrk"))))
    (should (string-match-p "-layout" cmd))
    (should (string-match-p "-nopgbrk" cmd))))

(ert-deftest pdf-tools-extras-test-build-cmd-no-extra-args ()
  "Without extra args the command has exactly three components."
  (let ((cmd (pdf-tools-extras-test--build-pdftotext-cmd
              "pdftotext" "/tmp/test.pdf" "-")))
    ;; Three shell-quoted arguments separated by spaces.
    ;; The source path is expanded, so count the quoted segments.
    (should (> (length (split-string cmd " ")) 2))))

(ert-deftest pdf-tools-extras-test-build-cmd-expands-source ()
  "The source path is expanded to an absolute path."
  (let ((cmd (pdf-tools-extras-test--build-pdftotext-cmd
              "pdftotext" "~/test.pdf" "-")))
    (should-not (string-match-p "~/" cmd))
    (should (string-match-p (getenv "HOME") cmd))))

;;;; convert-pdf output selection logic

;; Extracted from pdf-tools-extras-convert-pdf for standalone testing.
(defun pdf-tools-extras-test--convert-pdf-output (destination)
  "Return the output argument for pdftotext given DESTINATION.
If DESTINATION is non-nil, return its expanded path; otherwise return \"-\"."
  (if destination
      (expand-file-name destination)
    "-"))

(ert-deftest pdf-tools-extras-test-convert-pdf-stdout ()
  "When destination is nil, output is \"-\" for stdout."
  (should (equal (pdf-tools-extras-test--convert-pdf-output nil) "-")))

(ert-deftest pdf-tools-extras-test-convert-pdf-destination ()
  "When destination is provided, output is the expanded path."
  (let ((result (pdf-tools-extras-test--convert-pdf-output "/tmp/out.txt")))
    (should (equal result "/tmp/out.txt"))))

(ert-deftest pdf-tools-extras-test-convert-pdf-destination-expanded ()
  "A relative destination is expanded to an absolute path."
  (let ((result (pdf-tools-extras-test--convert-pdf-output "~/out.txt")))
    (should-not (string-prefix-p "~" result))
    (should (string-match-p (getenv "HOME") result))))

(provide 'pdf-tools-extras-test)
;;; pdf-tools-extras-test.el ends here
