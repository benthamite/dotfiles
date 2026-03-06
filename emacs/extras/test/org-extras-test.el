;;; org-extras-test.el --- Tests for org-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in org-extras.el including line
;; counting, babel evaluation, link manipulation, and string operations.

;;; Code:

(require 'ert)
(require 'org-extras)

;;;; count-lines-with-expression

(ert-deftest org-extras-test-count-lines-with-expression-basic-match ()
  "Count lines matching a simple regexp in a multiline string."
  (should (= (org-extras-count-lines-with-expression
              "foo\nbar\nfoo again" "foo")
             2)))

(ert-deftest org-extras-test-count-lines-with-expression-no-match ()
  "Return 0 when no lines match the regexp."
  (should (= (org-extras-count-lines-with-expression
              "foo\nbar\nbaz" "qux")
             0)))

(ert-deftest org-extras-test-count-lines-with-expression-all-match ()
  "Count all lines when every line matches."
  (should (= (org-extras-count-lines-with-expression
              "abc\nabc\nabc" "abc")
             3)))

(ert-deftest org-extras-test-count-lines-with-expression-empty-string ()
  "Handle empty string input (one empty line from split)."
  (should (= (org-extras-count-lines-with-expression "" "foo")
             0)))

(ert-deftest org-extras-test-count-lines-with-expression-single-line ()
  "Handle single-line string with match."
  (should (= (org-extras-count-lines-with-expression "hello" "hello")
             1)))

(ert-deftest org-extras-test-count-lines-with-expression-regexp ()
  "Match with a real regexp pattern, not just a literal string."
  (should (= (org-extras-count-lines-with-expression
              "abc123\ndef456\nghi" "[0-9]+")
             2)))

(ert-deftest org-extras-test-count-lines-with-expression-partial-match ()
  "Regexp matches partial line content."
  (should (= (org-extras-count-lines-with-expression
              "foobar\nbarbaz\nqux" "bar")
             2)))

;;;; confirm-babel-evaluate

(ert-deftest org-extras-test-confirm-babel-evaluate-safe-language ()
  "Return non-nil for languages in the safe list."
  (let ((org-extras-confirm-babel-evaluate-languages '("emacs-lisp" "python")))
    (should (org-extras-confirm-babel-evaluate "emacs-lisp" nil))
    (should (org-extras-confirm-babel-evaluate "python" nil))))

(ert-deftest org-extras-test-confirm-babel-evaluate-unsafe-language ()
  "Return nil for languages not in the safe list."
  (let ((org-extras-confirm-babel-evaluate-languages '("emacs-lisp" "python")))
    (should-not (org-extras-confirm-babel-evaluate "shell" nil))
    (should-not (org-extras-confirm-babel-evaluate "ruby" nil))))

(ert-deftest org-extras-test-confirm-babel-evaluate-empty-list ()
  "Return nil for any language when the safe list is empty."
  (let ((org-extras-confirm-babel-evaluate-languages nil))
    (should-not (org-extras-confirm-babel-evaluate "emacs-lisp" nil))))

(ert-deftest org-extras-test-confirm-babel-evaluate-ignores-body ()
  "The second argument (body) is ignored; result depends only on language."
  (let ((org-extras-confirm-babel-evaluate-languages '("python")))
    (should (org-extras-confirm-babel-evaluate "python" "print('hello')"))
    (should (org-extras-confirm-babel-evaluate "python" nil))))

;;;; remove-link

(ert-deftest org-extras-test-remove-link-with-description ()
  "Remove link syntax and keep the description text."
  (with-temp-buffer
    (org-mode)
    (insert "before [[https://example.com][Example]] after")
    (goto-char 10) ; inside the link
    (org-extras-remove-link)
    (should (equal (buffer-string) "before Example after"))))

(ert-deftest org-extras-test-remove-link-without-description ()
  "Remove link syntax and keep the URL when no description is present."
  (with-temp-buffer
    (org-mode)
    (insert "before [[https://example.com]] after")
    (goto-char 10) ; inside the link
    (org-extras-remove-link)
    (should (equal (buffer-string) "before https://example.com after"))))

(ert-deftest org-extras-test-remove-link-not-on-link ()
  "Do nothing when point is not on a link."
  (with-temp-buffer
    (org-mode)
    (insert "no link here")
    (goto-char (point-min))
    (org-extras-remove-link)
    (should (equal (buffer-string) "no link here"))))

;;;; link-get-thing-at-point / link-get-url-at-point / link-get-description-at-point

(ert-deftest org-extras-test-link-get-url-at-point-extracts-url ()
  "Extract URL from an org link at point via kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "[[https://example.com][description]]")
    (goto-char 5) ; inside the link
    (org-extras-link-get-url-at-point)
    (should (equal (current-kill 0) "https://example.com"))))

(ert-deftest org-extras-test-link-get-url-at-point-adds-to-kill-ring ()
  "Verify the extracted URL is added to the kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "[[https://example.com][click here]]")
    (goto-char 5)
    (org-extras-link-get-url-at-point)
    (should (equal (current-kill 0) "https://example.com"))))

(ert-deftest org-extras-test-link-get-description-at-point-extracts-description ()
  "Extract description from an org link at point via kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "[[https://example.com][My Description]]")
    (goto-char 5)
    (org-extras-link-get-description-at-point)
    (should (equal (current-kill 0) "My Description"))))

(ert-deftest org-extras-test-link-get-url-at-point-no-link ()
  "Return nil when point is not on an org link."
  (with-temp-buffer
    (org-mode)
    (insert "just plain text")
    (goto-char 5)
    (should-not (org-extras-link-get-url-at-point))))

(ert-deftest org-extras-test-link-get-url-at-point-no-description ()
  "Extract URL from a link without a description via kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "[[https://example.com]]")
    (goto-char 5)
    (org-extras-link-get-url-at-point)
    (should (equal (current-kill 0) "https://example.com"))))

(ert-deftest org-extras-test-link-get-link-at-point-extracts-full-link ()
  "Extract the full link syntax from an org link at point via kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "[[https://example.com][desc]]")
    (goto-char 5)
    (org-extras-link-get-link-at-point)
    (let ((result (current-kill 0)))
      (should (string-match-p "https://example.com" result))
      (should (string-match-p "desc" result)))))

;;;; get-heading-contents

(ert-deftest org-extras-test-get-heading-contents-returns-body ()
  "Extract body text from an org heading, excluding sub-headings."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody text line one\nBody text line two\n** Sub-heading\nSub content\n")
    (goto-char (point-min))
    (let ((contents (org-extras-get-heading-contents)))
      (should (string-match-p "Body text line one" contents))
      (should-not (string-match-p "Sub content" contents)))))

(ert-deftest org-extras-test-get-heading-contents-empty-heading ()
  "Return empty string for a heading with no body text."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n** Sub-heading\n")
    (goto-char (point-min))
    (let ((contents (org-extras-get-heading-contents)))
      (should (stringp contents))
      (should (string= contents "")))))

(ert-deftest org-extras-test-get-heading-contents-from-inside-body ()
  "Extract body text when point is inside the heading body, not on the heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody text here\n** Sub-heading\n")
    (goto-char (point-min))
    (forward-line 1) ; point is on "Body text here"
    (let ((contents (org-extras-get-heading-contents)))
      (should (string-match-p "Body text here" contents)))))

(ert-deftest org-extras-test-get-heading-contents-before-first-heading ()
  "Return a message when point is before the first heading."
  (with-temp-buffer
    (org-mode)
    (insert "Some preamble\n* Heading\nBody\n")
    (goto-char (point-min))
    (should (equal (org-extras-get-heading-contents)
                   "Not in or on an org heading"))))

;;;; copy-heading-name

(ert-deftest org-extras-test-copy-heading-name-copies-to-kill-ring ()
  "Copy the current heading name to the kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "* My Important Heading\nSome body\n")
    (goto-char (point-min))
    (org-extras-copy-heading-name)
    (should (equal (current-kill 0) "My Important Heading"))))

(ert-deftest org-extras-test-copy-heading-name-with-todo ()
  "Copy heading name excluding TODO keyword."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO My Task\nSome body\n")
    (goto-char (point-min))
    (org-extras-copy-heading-name)
    (should (equal (current-kill 0) "My Task"))))

;;;; suppress-track-changes-assertion

(ert-deftest org-extras-test-suppress-track-changes-assertion-normal-function ()
  "Allow normal function calls through without interference."
  (let ((result (org-extras--suppress-track-changes-assertion
                 (lambda (x y) (+ x y))
                 3 4)))
    (should (equal result 7))))

(ert-deftest org-extras-test-suppress-track-changes-assertion-suppresses-assertion ()
  "Suppress cl-assertion-failed errors and return nil."
  (let ((result (org-extras--suppress-track-changes-assertion
                 (lambda ()
                   (signal 'cl-assertion-failed '(nil)))
                 )))
    (should-not result)))

(ert-deftest org-extras-test-suppress-track-changes-assertion-propagates-other-errors ()
  "Do not suppress errors other than cl-assertion-failed."
  (should-error
   (org-extras--suppress-track-changes-assertion
    (lambda () (error "Some other error")))
   :type 'error))

(provide 'org-extras-test)
;;; org-extras-test.el ends here
