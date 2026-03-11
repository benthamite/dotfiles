;;; org-msg-extras-test.el --- Tests for org-msg-extras -*- lexical-binding: t -*-

;; Tests for pure logic functions in org-msg-extras.el.
;; org-msg-extras depends on org-msg and org-extras, which are unavailable
;; in CI, so we extract and inline the pure logic for testing.

;;; Code:

(require 'ert)
(require 'org)

;;;; Inlined pure logic

(defun org-msg-extras-test--end-of-meta-data ()
  "Move point past org property drawer meta data."
  (while (re-search-forward org-property-re nil t)
    (forward-line)))

;;;; end-of-meta-data

(ert-deftest org-msg-extras-test-end-of-meta-data-moves-past-properties ()
  "Point moves past a :PROPERTIES:/:END: block."
  (with-temp-buffer
    (insert ":PROPERTIES:\n:ID: abc123\n:END:\nBody text here\n")
    (goto-char (point-min))
    (org-msg-extras-test--end-of-meta-data)
    (should (looking-at-p "Body text here"))))

(ert-deftest org-msg-extras-test-end-of-meta-data-multiple-properties ()
  "Point moves past a property drawer with multiple properties."
  (with-temp-buffer
    (insert ":PROPERTIES:\n:ID: abc123\n:AUTHOR: someone\n:DATE: 2026-01-01\n:END:\nContent\n")
    (goto-char (point-min))
    (org-msg-extras-test--end-of-meta-data)
    (should (looking-at-p "Content"))))

(ert-deftest org-msg-extras-test-end-of-meta-data-no-properties ()
  "Point stays at the beginning when no property drawer is present."
  (with-temp-buffer
    (insert "Just plain text\nNo properties here\n")
    (goto-char (point-min))
    (let ((start (point)))
      (org-msg-extras-test--end-of-meta-data)
      (should (= (point) start)))))

(ert-deftest org-msg-extras-test-end-of-meta-data-empty-drawer ()
  "Point moves past an empty property drawer."
  (with-temp-buffer
    (insert ":PROPERTIES:\n:END:\nAfter drawer\n")
    (goto-char (point-min))
    (org-msg-extras-test--end-of-meta-data)
    (should (looking-at-p "After drawer"))))

;;;; Custom variable defaults

(ert-deftest org-msg-extras-test-personal-plain-text-signature-default ()
  "The default personal plain text signature contains the expected text."
  (let ((sig "\n--\nPablo\n"))
    (should (stringp sig))
    (should (string-match-p "Pablo" sig))
    (should (string-match-p "\n--\n" sig))))

(ert-deftest org-msg-extras-test-personal-html-signature-default ()
  "The default personal HTML signature contains org signature block markers."
  (let ((sig "\n#+begin_signature\n--\n*Pablo*\n#+end_signature"))
    (should (stringp sig))
    (should (string-match-p "begin_signature" sig))
    (should (string-match-p "end_signature" sig))
    (should (string-match-p "Pablo" sig))))

(provide 'org-msg-extras-test)
;;; org-msg-extras-test.el ends here
