;;; zotra-extras-test.el --- Tests for zotra-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in zotra-extras.el, primarily the
;; octal sequence replacement logic.

;;; Code:

(require 'ert)
(require 'zotra-extras)

;;;; fix-octal-sequences

(ert-deftest zotra-extras-test-fix-octal-sequences-latin-accents ()
  "Replace octal sequences for common Latin accented characters."
  (with-temp-buffer
    ;; \303\251 is the octal/UTF-8 byte sequence for \u00e9 (e with acute).
    ;; `unibyte-string' produces raw bytes that, once inserted into a
    ;; multibyte buffer, appear as the two-character sequence that
    ;; `search-forward' in `zotra-extras-fix-octal-sequences' will find.
    (insert (unibyte-string #o303 #o251))
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) "\u00e9"))))

(ert-deftest zotra-extras-test-fix-octal-sequences-n-tilde ()
  "Replace octal sequences for n with tilde (\u00f1)."
  (with-temp-buffer
    (insert (unibyte-string #o303 #o261))
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) "\u00f1"))))

(ert-deftest zotra-extras-test-fix-octal-sequences-u-umlaut ()
  "Replace octal sequences for u with umlaut (\u00fc)."
  (with-temp-buffer
    (insert (unibyte-string #o303 #o274))
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) "\u00fc"))))

(ert-deftest zotra-extras-test-fix-octal-sequences-no-change ()
  "Leave text without octal sequences unchanged."
  (with-temp-buffer
    (insert "plain ASCII text with no octal sequences")
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) "plain ASCII text with no octal sequences"))))

(ert-deftest zotra-extras-test-fix-octal-sequences-mixed-content ()
  "Handle buffer with both octal sequences and normal text."
  (with-temp-buffer
    (insert "Author: Mu" (unibyte-string #o303 #o261) "oz")
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) "Author: Mu\u00f1oz"))))

(ert-deftest zotra-extras-test-fix-octal-sequences-empty-buffer ()
  "Handle empty buffer without error."
  (with-temp-buffer
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) ""))))

(ert-deftest zotra-extras-test-fix-octal-sequences-multiple-occurrences ()
  "Replace multiple octal sequences in the same buffer."
  (with-temp-buffer
    (insert (unibyte-string #o303 #o251) " and " (unibyte-string #o303 #o251))
    (zotra-extras-fix-octal-sequences)
    (should (equal (buffer-string) "\u00e9 and \u00e9"))))

(ert-deftest zotra-extras-test-fix-octal-sequences-preserves-point ()
  "Point is preserved after fixing octal sequences (via save-excursion)."
  (with-temp-buffer
    (insert "before " (unibyte-string #o303 #o251) " after")
    (goto-char 4)
    (let ((pos (point)))
      (zotra-extras-fix-octal-sequences)
      (should (= (point) pos)))))

(provide 'zotra-extras-test)
;;; zotra-extras-test.el ends here
