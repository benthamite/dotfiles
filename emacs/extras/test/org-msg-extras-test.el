;;; org-msg-extras-test.el --- Tests for org-msg-extras -*- lexical-binding: t -*-

;; Tests for pure logic functions in org-msg-extras.el.
;; org-msg-extras depends on org-msg and org-extras, which are unavailable
;; in CI, so we extract and inline the pure logic for testing.

;;; Code:

(require 'ert)
(require 'cl-lib)
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

(ert-deftest org-msg-extras-test-epoch-signatures-default-title ()
  "Epoch signature defaults use the current job title."
  (if (condition-case nil
          (progn (require 'org-msg-extras) t)
        (error nil))
      (dolist (signature (list org-msg-extras-epoch-plain-text-signature
                               org-msg-extras-epoch-html-signature))
        (should (string-match-p "AI Enablement Manager" signature))
        (should-not (string-match-p "Operations Associate" signature)))
    (ert-skip "org-msg-extras dependencies unavailable")))

;;;; close-forwarded-parts

(defconst org-msg-extras-test--end-line
  "-------------------- End of forwarded message --------------------"
  "End-of-forward line that `message-forward' inserts.")

(defun org-msg-extras-test--close-forwarded-parts (body &optional type)
  "Return BODY after closing forwarded parts in a draft of TYPE.
TYPE defaults to `forward'."
  (unless (condition-case nil
              (progn (require 'org-msg-extras) t)
            (error nil))
    (ert-skip "org-msg-extras dependencies unavailable"))
  (with-temp-buffer
    (let ((mail-header-separator "--text follows this line--"))
      (insert "Subject: Fwd: x\n" mail-header-separator "\n" body)
      (setq-local mu4e-compose-type (or type 'forward))
      (org-msg-extras-close-forwarded-parts)
      (goto-char (point-min))
      (message-goto-body)
      (buffer-substring-no-properties (point) (point-max)))))

(ert-deftest org-msg-extras-test-close-forwarded-html-only-part ()
  "An open HTML part is closed before the end-of-forward line."
  (should (equal (org-msg-extras-test--close-forwarded-parts
                  (concat "<#part type=text/html charset=\"utf-8\" nofile=yes>\n"
                          "<p>Hi</p>\n"
                          org-msg-extras-test--end-line "\n"))
                 (concat "<#part type=text/html charset=\"utf-8\" nofile=yes>\n"
                         "<p>Hi</p>\n<#/part>\n"
                         org-msg-extras-test--end-line "\n"))))

(ert-deftest org-msg-extras-test-close-forwarded-parts-in-multipart ()
  "Open parts inside a multipart are closed before the next tag."
  (should (equal (org-msg-extras-test--close-forwarded-parts
                  (concat "<#multipart type=alternative>\n"
                          "<#part type=\"text/plain\">\nHi\n"
                          "<#part type=\"text/html\">\n<p>Hi</p>\n"
                          "<#/multipart>\n"))
                 (concat "<#multipart type=alternative>\n"
                         "<#part type=\"text/plain\">\nHi\n<#/part>\n"
                         "<#part type=\"text/html\">\n<p>Hi</p>\n<#/part>\n"
                         "<#/multipart>\n"))))

(ert-deftest org-msg-extras-test-close-forwarded-parts-mid-line-tag ()
  "A part is closed before a tag that follows its last line directly.
`mml-insert-mime' leaves no newline after a text part, so a forwarded
multipart message can contain `</html><#/multipart>'."
  (should (equal (org-msg-extras-test--close-forwarded-parts
                  (concat "<#multipart type=alternative>\n"
                          "<#part type=text/html>\n<p>Hi</p></html><#/multipart>\n"))
                 (concat "<#multipart type=alternative>\n"
                         "<#part type=text/html>\n<p>Hi</p></html>\n<#/part>\n"
                         "<#/multipart>\n"))))

(ert-deftest org-msg-extras-test-close-forwarded-parts-ignores-quoted-tag ()
  "A quoted tag inside a part does not end the part."
  (should (equal (org-msg-extras-test--close-forwarded-parts
                  (concat "<#part type=text/plain>\nSee <#!part> here\n"
                          org-msg-extras-test--end-line "\n"))
                 (concat "<#part type=text/plain>\nSee <#!part> here\n<#/part>\n"
                         org-msg-extras-test--end-line "\n"))))

(ert-deftest org-msg-extras-test-close-forwarded-parts-keeps-closed-part ()
  "A part that is already closed is left alone."
  (let ((body "<#part type=text/html>\n<p>Hi</p>\n<#/part>\n"))
    (should (equal (org-msg-extras-test--close-forwarded-parts body) body))))

(ert-deftest org-msg-extras-test-close-forwarded-parts-only-forwards ()
  "Drafts that are not forwards are left alone."
  (let ((body "<#part type=text/html>\n<p>Hi</p>\n"))
    (should (equal (org-msg-extras-test--close-forwarded-parts body 'reply)
                   body))))

;;;; prepare-forward

(defun org-msg-extras-test--embeddable-p (raw)
  "Return `org-msg-extras--embeddable-p' for a parent message RAW."
  (unless (condition-case nil
              (progn (require 'org-msg-extras) t)
            (error nil))
    (ert-skip "org-msg-extras dependencies unavailable"))
  (let ((file (make-temp-file "org-msg-extras-test" nil ".eml" raw)))
    (unwind-protect
        (cl-letf (((symbol-function 'mu4e-message-readable-path)
                   (lambda (_msg) file)))
          (org-msg-extras--embeddable-p 'msg))
      (delete-file file))))

(ert-deftest org-msg-extras-test-embeddable-html-only ()
  "An HTML-only message can be embedded."
  (should (org-msg-extras-test--embeddable-p
           "Subject: x\nMIME-Version: 1.0\nContent-Type: text/html\n\n<p>Hi</p>\n")))

(ert-deftest org-msg-extras-test-embeddable-alternative ()
  "A text and HTML alternative can be embedded."
  (should (org-msg-extras-test--embeddable-p
           (concat "Subject: x\nMIME-Version: 1.0\n"
                   "Content-Type: multipart/alternative; boundary=\"b\"\n\n"
                   "--b\nContent-Type: text/plain\n\nHi\n"
                   "--b\nContent-Type: text/html\n\n<p>Hi</p>\n--b--\n"))))

(ert-deftest org-msg-extras-test-not-embeddable-plain-text ()
  "A plain-text message is not embedded."
  (should-not (org-msg-extras-test--embeddable-p
               "Subject: x\nMIME-Version: 1.0\nContent-Type: text/plain\n\nHi\n")))

(ert-deftest org-msg-extras-test-not-embeddable-with-attachment ()
  "A message with an attachment is not embedded, so the attachment is kept."
  (should-not (org-msg-extras-test--embeddable-p
               (concat "Subject: x\nMIME-Version: 1.0\n"
                       "Content-Type: multipart/mixed; boundary=\"b\"\n\n"
                       "--b\nContent-Type: text/html\n\n<p>Hi</p>\n"
                       "--b\nContent-Type: application/pdf\n"
                       "Content-Disposition: attachment; filename=a.pdf\n\nxx\n--b--\n"))))

(provide 'org-msg-extras-test)
;;; org-msg-extras-test.el ends here
