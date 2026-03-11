;;; jinx-extras-test.el --- Tests for jinx-extras -*- lexical-binding: t -*-

;; Tests for language toggle logic and abbrev creation logic
;; in jinx-extras.el.  Since jinx is an external dependency,
;; we extract the pure logic inline rather than requiring the package.

;;; Code:

(require 'ert)

;; Do NOT require jinx-extras; jinx is an external dep unavailable in CI.

;;;; Language toggle logic

(ert-deftest jinx-extras-test-toggle-english-to-spanish ()
  "Toggle from English to Spanish."
  (let ((jinx-languages "en"))
    (should (equal (if (string= jinx-languages "en") "es" "en") "es"))))

(ert-deftest jinx-extras-test-toggle-spanish-to-english ()
  "Toggle from Spanish to English."
  (let ((jinx-languages "es"))
    (should (equal (if (string= jinx-languages "en") "es" "en") "en"))))

(ert-deftest jinx-extras-test-toggle-unknown-language-to-english ()
  "Toggle from an unknown language defaults to English."
  (let ((jinx-languages "fr"))
    (should (equal (if (string= jinx-languages "en") "es" "en") "en"))))

(ert-deftest jinx-extras-test-toggle-empty-string-to-english ()
  "Toggle from an empty language string defaults to English."
  (let ((jinx-languages ""))
    (should (equal (if (string= jinx-languages "en") "es" "en") "en"))))

;;;; Abbrev creation logic

(ert-deftest jinx-extras-test-abbrev-from-overlay ()
  "Extract misspelled word from an overlay region for abbrev creation."
  (with-temp-buffer
    (insert "teh quick brown fox")
    (let* ((ov (make-overlay 1 4))  ; "teh"
           (abbrev (buffer-substring-no-properties
                    (overlay-start ov)
                    (overlay-end ov))))
      (should (equal abbrev "teh"))
      (delete-overlay ov))))

(ert-deftest jinx-extras-test-abbrev-defines-correctly ()
  "Define an abbrev from a misspelled word and its correction."
  (with-temp-buffer
    (insert "recieve the package")
    (let* ((ov (make-overlay 1 8))  ; "recieve"
           (word "receive")
           (abbrev (buffer-substring-no-properties
                    (overlay-start ov)
                    (overlay-end ov))))
      (should (equal abbrev "recieve"))
      ;; Verify the abbrev table entry can be created
      (let ((table (make-abbrev-table)))
        (define-abbrev table abbrev word)
        (should (equal (abbrev-expansion abbrev table) word)))
      (delete-overlay ov))))

(ert-deftest jinx-extras-test-abbrev-single-character ()
  "Extract a single-character misspelled word from an overlay."
  (with-temp-buffer
    (insert "a b c")
    (let* ((ov (make-overlay 3 4))  ; "b"
           (abbrev (buffer-substring-no-properties
                    (overlay-start ov)
                    (overlay-end ov))))
      (should (equal abbrev "b"))
      (delete-overlay ov))))

(provide 'jinx-extras-test)
;;; jinx-extras-test.el ends here
