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

(provide 'org-extras-test)
;;; org-extras-test.el ends here
