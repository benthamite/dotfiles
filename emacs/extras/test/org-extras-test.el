;;; org-extras-test.el --- Tests for org-extras -*- lexical-binding: t -*-

;; Tests for pure helper functions in org-extras.el including line
;; counting, babel evaluation, link manipulation, and string operations.

;;; Code:

(require 'ert)
(require 'org-extras)
(require 'org-clock)

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

;;;; insert-subheading

(ert-deftest org-extras-test-insert-subheading-creates-child ()
  "Insert a subheading as a child of the current heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nSome body text\n")
    (goto-char (point-min))
    (org-extras-insert-subheading)
    (should (looking-back "^\\*\\* " nil))
    ;; The buffer should have a level-2 heading
    (should (string-match-p "^\\*\\* " (buffer-string)))))

(ert-deftest org-extras-test-insert-subheading-before-existing-children ()
  "Insert a subheading above existing sub-headings."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nBody text\n** Existing child\nChild body\n")
    (goto-char (point-min))
    (org-extras-insert-subheading)
    ;; Point should be on the newly inserted heading, which is before
    ;; the existing child
    (let ((new-pos (point)))
      (should (string-match-p "^\\*\\* Existing child"
                              (buffer-substring new-pos (point-max)))))))

;;;; copy-heading-contents

(ert-deftest org-extras-test-copy-heading-contents-to-kill-ring ()
  "Copy heading body text to the kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody line one\nBody line two\n** Sub\n")
    (goto-char (point-min))
    (org-extras-copy-heading-contents)
    (should (string-match-p "Body line one" (current-kill 0)))))

(ert-deftest org-extras-test-copy-heading-contents-empty-heading ()
  "Display message when heading has no body text."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n** Sub\n")
    (goto-char (point-min))
    (should (equal (org-extras-copy-heading-contents) "Heading is empty."))))

;;;; count-words

(ert-deftest org-extras-test-count-words-basic ()
  "Count words in a subtree body and return a summary message."
  (with-temp-buffer
    (org-mode)
    (insert "* Test heading\nOne two three four five\n")
    (goto-char (point-min))
    (let ((result (org-extras-count-words)))
      (should (stringp result))
      (should (string-match-p "5 words" result)))))

(ert-deftest org-extras-test-count-words-with-children ()
  "Count words across subtree including children."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nOne two\n** Child\nThree four five\n")
    (goto-char (point-min))
    (let ((result (org-extras-count-words)))
      (should (stringp result))
      (should (string-match-p "5 words" result)))))

(ert-deftest org-extras-test-count-words-empty-body ()
  "Count zero words in a heading with no body."
  (with-temp-buffer
    (org-mode)
    (insert "* Empty heading\n")
    (goto-char (point-min))
    (let ((result (org-extras-count-words)))
      (should (stringp result))
      (should (string-match-p "0 words" result)))))

(ert-deftest org-extras-test-count-words-copies-to-kill-ring ()
  "Verify that word count number is copied to the kill ring."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nAlpha beta gamma\n")
    (goto-char (point-min))
    (org-extras-count-words)
    (should (equal (current-kill 0) "3"))))

;;;; jump-to-first-heading

(ert-deftest org-extras-test-jump-to-first-heading-basic ()
  "Move point to the first heading in the buffer."
  (with-temp-buffer
    (org-mode)
    (insert "Preamble text\n* First heading\n* Second heading\n")
    (goto-char (point-max))
    (org-extras-jump-to-first-heading)
    (should (org-at-heading-p))
    (should (string-match-p "First heading" (org-get-heading t t t t)))))

(ert-deftest org-extras-test-jump-to-first-heading-widens ()
  "Widen the buffer before jumping to the first heading."
  (with-temp-buffer
    (org-mode)
    (insert "Preamble\n* First heading\nBody\n* Second heading\nBody\n")
    (goto-char (point-min))
    (org-next-visible-heading 2) ; on second heading
    (org-narrow-to-subtree)
    (org-extras-jump-to-first-heading)
    ;; Buffer should be widened and point on first heading
    (should (= (point-min) 1))
    (should (string-match-p "First heading" (org-get-heading t t t t)))))

(ert-deftest org-extras-test-jump-to-first-heading-from-end ()
  "Jump to the first heading from the end of the buffer."
  (with-temp-buffer
    (org-mode)
    (insert "Some preamble\n* First heading\nBody\n* Second heading\n")
    (goto-char (point-max))
    (org-extras-jump-to-first-heading)
    (should (org-at-heading-p))
    (should (string-match-p "First heading" (org-get-heading t t t t)))))

;;;; narrow-to-entry-and-children

(ert-deftest org-extras-test-narrow-to-entry-and-children-basic ()
  "Narrow to the full subtree including children."
  (with-temp-buffer
    (org-mode)
    (insert "* First\nBody 1\n** Child\nChild body\n* Second\nBody 2\n")
    (goto-char (point-min))
    (org-extras-narrow-to-entry-and-children)
    ;; The narrowed region should include the child but not the second heading
    (should (string-match-p "Child body" (buffer-string)))
    (should-not (string-match-p "Second" (buffer-string)))))

(ert-deftest org-extras-test-narrow-to-entry-and-children-excludes-siblings ()
  "Narrowing excludes sibling headings at the same level."
  (with-temp-buffer
    (org-mode)
    (insert "* H1\nBody A\n** Child A\n* H2\nBody B\n")
    (goto-char (point-min))
    (org-extras-narrow-to-entry-and-children)
    (should (string-match-p "Body A" (buffer-string)))
    (should (string-match-p "Child A" (buffer-string)))
    (should-not (string-match-p "H2" (buffer-string)))))

;;;; narrow-to-entry-no-children

(ert-deftest org-extras-test-narrow-to-entry-no-children-basic ()
  "Narrow to entry excluding child subtrees."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nParent body\n** Child\nChild body\n")
    (goto-char (point-min))
    (org-extras-narrow-to-entry-no-children)
    (should (string-match-p "Parent body" (buffer-string)))
    (should-not (string-match-p "Child" (buffer-string)))))

(ert-deftest org-extras-test-narrow-to-entry-no-children-no-body ()
  "Narrow to entry with no body and children."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** Child\nChild body\n")
    (goto-char (point-min))
    (org-extras-narrow-to-entry-no-children)
    ;; Should only contain the heading line
    (should (string-match-p "Parent" (buffer-string)))
    (should-not (string-match-p "Child" (buffer-string)))))

(ert-deftest org-extras-test-narrow-to-entry-no-children-excludes-siblings ()
  "Narrowing excludes both children and sibling headings."
  (with-temp-buffer
    (org-mode)
    (insert "* H1\nBody\n** Child\n* H2\nOther\n")
    (goto-char (point-min))
    (org-extras-narrow-to-entry-no-children)
    (should (string-match-p "Body" (buffer-string)))
    (should-not (string-match-p "Child" (buffer-string)))
    (should-not (string-match-p "H2" (buffer-string)))))

;;;; goto-beginning-of-heading-text

(ert-deftest org-extras-test-goto-beginning-of-heading-text-plain ()
  "Position point at the start of heading text (after stars and space)."
  (with-temp-buffer
    (org-mode)
    (insert "* Simple heading\n")
    (goto-char (point-min))
    (org-extras-goto-beginning-of-heading-text)
    (should (looking-at "Simple heading"))))

(ert-deftest org-extras-test-goto-beginning-of-heading-text-with-todo ()
  "Position point after TODO keyword."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO My task\n")
    (goto-char (point-min))
    (org-extras-goto-beginning-of-heading-text)
    (should (looking-at "My task"))))

(ert-deftest org-extras-test-goto-beginning-of-heading-text-with-priority ()
  "Position point after priority cookie."
  (with-temp-buffer
    (org-mode)
    (insert "* [#A] Important thing\n")
    (goto-char (point-min))
    (org-extras-goto-beginning-of-heading-text)
    (should (looking-at "Important thing"))))

(ert-deftest org-extras-test-goto-beginning-of-heading-text-with-todo-and-priority ()
  "Position point after both TODO keyword and priority cookie."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO [#B] Another task\n")
    (goto-char (point-min))
    (org-extras-goto-beginning-of-heading-text)
    (should (looking-at "Another task"))))

(ert-deftest org-extras-test-goto-beginning-of-heading-text-not-on-heading ()
  "Do nothing when point is not on a heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody text\n")
    (goto-char (point-min))
    (forward-line 1) ; on body text
    (let ((pos (point)))
      (org-extras-goto-beginning-of-heading-text)
      (should (= (point) pos)))))

;;;; remove-trailing-heading

(ert-deftest org-extras-test-remove-trailing-heading-empty ()
  "Remove an empty heading at the end of the buffer."
  (with-temp-buffer
    (org-mode)
    (insert "* Real heading\nBody\n* \n")
    (org-extras-remove-trailing-heading)
    (should-not (string-match-p "^\\* $" (buffer-string)))
    (should (string-match-p "Real heading" (buffer-string)))))

(ert-deftest org-extras-test-remove-trailing-heading-non-empty ()
  "Do not remove a non-empty trailing heading."
  (with-temp-buffer
    (org-mode)
    (insert "* First\nBody\n* Second\nMore body\n")
    (org-extras-remove-trailing-heading)
    (should (string-match-p "Second" (buffer-string)))))

(ert-deftest org-extras-test-remove-trailing-heading-preserves-content ()
  "Preserve all content before the empty trailing heading."
  (with-temp-buffer
    (org-mode)
    (insert "* H1\nBody one\n* H2\nBody two\n* \n")
    (org-extras-remove-trailing-heading)
    (should (string-match-p "Body one" (buffer-string)))
    (should (string-match-p "Body two" (buffer-string)))
    (should (string-match-p "H2" (buffer-string)))))

;;;; clocktable-sorter

(ert-deftest org-extras-test-clocktable-sorter-sorts-descending ()
  "Sort clocktable entries by time in descending order."
  (let* ((sorted-tables nil)
         (org-clock-clocktable-formatter
          (lambda (_ipos tables _params)
            (setq sorted-tables tables)))
         (tables (list '("File A" 100 nil)
                       '("File C" 300 nil)
                       '("File B" 200 nil))))
    (org-extras-clocktable-sorter 0 tables nil)
    (should (equal (mapcar #'cadr sorted-tables) '(300 200 100)))))

(ert-deftest org-extras-test-clocktable-sorter-already-sorted ()
  "Handle tables that are already in descending order."
  (let* ((sorted-tables nil)
         (org-clock-clocktable-formatter
          (lambda (_ipos tables _params)
            (setq sorted-tables tables)))
         (tables (list '("A" 300 nil)
                       '("B" 200 nil)
                       '("C" 100 nil))))
    (org-extras-clocktable-sorter 0 tables nil)
    (should (equal (mapcar #'cadr sorted-tables) '(300 200 100)))))

(ert-deftest org-extras-test-clocktable-sorter-single-table ()
  "Handle a single-element table list."
  (let* ((sorted-tables nil)
         (org-clock-clocktable-formatter
          (lambda (_ipos tables _params)
            (setq sorted-tables tables)))
         (tables (list '("Only" 42 nil))))
    (org-extras-clocktable-sorter 0 tables nil)
    (should (equal (mapcar #'cadr sorted-tables) '(42)))))

(ert-deftest org-extras-test-clocktable-sorter-equal-times ()
  "Handle tables with equal time values."
  (let* ((sorted-tables nil)
         (org-clock-clocktable-formatter
          (lambda (_ipos tables _params)
            (setq sorted-tables tables)))
         (tables (list '("A" 100 nil)
                       '("B" 100 nil)
                       '("C" 100 nil))))
    (org-extras-clocktable-sorter 0 tables nil)
    (should (= (length sorted-tables) 3))
    (should (cl-every (lambda (t1) (= (cadr t1) 100)) sorted-tables))))

;;;; reset-checkbox-state-subtree

(ert-deftest org-extras-test-reset-checkbox-state-subtree-basic ()
  "Reset checked checkboxes in a subtree to unchecked."
  (with-temp-buffer
    (org-mode)
    (insert "* Task list\n- [X] Item one\n- [X] Item two\n- [ ] Item three\n")
    (goto-char (point-min))
    (org-extras-reset-checkbox-state-subtree)
    (should-not (string-match-p "\\[X\\]" (buffer-string)))
    (should (string-match-p "\\[ \\]" (buffer-string)))))

(ert-deftest org-extras-test-reset-checkbox-state-subtree-preserves-text ()
  "Preserve checkbox text while resetting state."
  (with-temp-buffer
    (org-mode)
    (insert "* Tasks\n- [X] Do laundry\n- [X] Buy groceries\n")
    (goto-char (point-min))
    (org-extras-reset-checkbox-state-subtree)
    (should (string-match-p "Do laundry" (buffer-string)))
    (should (string-match-p "Buy groceries" (buffer-string)))))

(ert-deftest org-extras-test-reset-checkbox-state-subtree-already-unchecked ()
  "Handle subtree where all checkboxes are already unchecked."
  (with-temp-buffer
    (org-mode)
    (insert "* Tasks\n- [ ] Item one\n- [ ] Item two\n")
    (goto-char (point-min))
    (org-extras-reset-checkbox-state-subtree)
    (should-not (string-match-p "\\[X\\]" (buffer-string)))
    ;; Still has unchecked checkboxes
    (should (string-match-p "\\[ \\]" (buffer-string)))))

(provide 'org-extras-test)
;;; org-extras-test.el ends here
