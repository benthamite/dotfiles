;;; markdown-mode-extras-test.el --- Tests for markdown-mode-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in markdown-mode-extras.el, including
;; link deletion and section extraction.

;;; Code:

(require 'ert)
(require 'markdown-mode-extras)

;;;; delete-link

(ert-deftest markdown-mode-extras-test-delete-link-returns-name ()
  "Delete inline link syntax and return its description text."
  (with-temp-buffer
    (markdown-mode)
    (insert "before [Example](https://example.com) after")
    (goto-char 10) ; inside "Example"
    (let ((name (markdown-mode-extras-delete-link)))
      (should (equal name "Example")))))

(ert-deftest markdown-mode-extras-test-delete-link-removes-entire-syntax ()
  "The entire link syntax is removed from the buffer."
  (with-temp-buffer
    (markdown-mode)
    (insert "before [Example](https://example.com) after")
    (goto-char 10)
    (markdown-mode-extras-delete-link)
    ;; delete-link removes the whole link markup without inserting anything back
    (should (equal (buffer-string) "before  after"))))

(ert-deftest markdown-mode-extras-test-delete-link-not-on-link ()
  "Return nil when point is not on a link."
  (with-temp-buffer
    (markdown-mode)
    (insert "no link here")
    (goto-char (point-min))
    (should-not (markdown-mode-extras-delete-link))
    (should (equal (buffer-string) "no link here"))))

(ert-deftest markdown-mode-extras-test-delete-link-at-beginning-of-buffer ()
  "Handle link at the very start of the buffer."
  (with-temp-buffer
    (markdown-mode)
    (insert "[First](https://first.com) rest")
    (goto-char 2)
    (let ((name (markdown-mode-extras-delete-link)))
      (should (equal name "First"))
      (should (equal (buffer-string) " rest")))))

(ert-deftest markdown-mode-extras-test-delete-link-at-end-of-buffer ()
  "Handle link at the very end of the buffer."
  (with-temp-buffer
    (markdown-mode)
    (insert "start [Last](https://last.com)")
    (goto-char 10)
    (let ((name (markdown-mode-extras-delete-link)))
      (should (equal name "Last"))
      (should (equal (buffer-string) "start ")))))

;;;; remove-url-in-link

(ert-deftest markdown-mode-extras-test-remove-url-in-link-basic ()
  "Remove the URL from a markdown link, leaving only the description."
  (with-temp-buffer
    (markdown-mode)
    (insert "See [example](https://example.com) here")
    (goto-char 8) ; inside the link
    (markdown-mode-extras-remove-url-in-link nil)
    (should (equal (buffer-string) "See example here"))))

(ert-deftest markdown-mode-extras-test-remove-url-in-link-not-on-link ()
  "Do nothing when point is not on a link."
  (with-temp-buffer
    (markdown-mode)
    (insert "plain text")
    (goto-char 3)
    (markdown-mode-extras-remove-url-in-link nil)
    (should (equal (buffer-string) "plain text"))))

;;;; get-section

(ert-deftest markdown-mode-extras-test-get-section-by-name ()
  "Extract a named section from a markdown buffer."
  (with-temp-buffer
    (markdown-mode)
    (insert "# Introduction\n\nIntro text.\n\n# Details\n\nDetail text.\n\n# Conclusion\n\nEnd.")
    (let ((section (markdown-mode-extras-get-section "Details")))
      (should (string-match-p "Detail text\\." section))
      ;; Should not contain other sections
      (should-not (string-match-p "Intro text" section)))))

(provide 'markdown-mode-extras-test)
;;; markdown-mode-extras-test.el ends here
