;;; org-noter-extras-test.el --- Tests for org-noter-extras -*- lexical-binding: t -*-

;; Tests for dehyphenation and page offset functions
;; in org-noter-extras.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-noter-extras)

;;;; Dehyphenate

(ert-deftest org-noter-extras-test-dehyphenate-basic ()
  "Dehyphenate removes hyphens splitting words across lines."
  (with-temp-buffer
    (insert "This is a hyphen- ated word.")
    (goto-char (point-min))
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (org-noter-extras-dehyphenate)
    (should (equal (buffer-string) "This is a hyphenated word."))))

(ert-deftest org-noter-extras-test-dehyphenate-multiple ()
  "Dehyphenate removes multiple hyphens in text."
  (with-temp-buffer
    (insert "con- nected and dis- connected")
    (goto-char (point-min))
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (org-noter-extras-dehyphenate)
    (should (equal (buffer-string) "connected and disconnected"))))

(ert-deftest org-noter-extras-test-dehyphenate-preserves-normal-hyphens ()
  "Dehyphenate does not remove hyphens that are not word-splits."
  (with-temp-buffer
    (insert "well-known fact")
    (goto-char (point-min))
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (org-noter-extras-dehyphenate)
    (should (equal (buffer-string) "well-known fact"))))

(ert-deftest org-noter-extras-test-dehyphenate-no-hyphens ()
  "Dehyphenate leaves text unchanged when there are no hyphens."
  (with-temp-buffer
    (insert "no hyphens here")
    (goto-char (point-min))
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (org-noter-extras-dehyphenate)
    (should (equal (buffer-string) "no hyphens here"))))

;;;; Highlight offset

(ert-deftest org-noter-extras-test-highlight-offset-positive ()
  "Highlight-offset subtracts positive offset from page numbers."
  (with-temp-buffer
    (insert "** Highlight on page 220\n** Highlight on page 230\n")
    (org-noter-extras-highlight-offset 10)
    (should (string-match-p "Highlight on page 210" (buffer-string)))
    (should (string-match-p "Highlight on page 220" (buffer-string)))))

(ert-deftest org-noter-extras-test-highlight-offset-zero ()
  "Highlight-offset with zero offset leaves page numbers unchanged."
  (with-temp-buffer
    (insert "** Highlight on page 100\n")
    (org-noter-extras-highlight-offset 0)
    (should (string-match-p "Highlight on page 100" (buffer-string)))))

(ert-deftest org-noter-extras-test-highlight-offset-negative ()
  "Highlight-offset with negative offset adds to page numbers."
  (with-temp-buffer
    (insert "** Highlight on page 10\n")
    (org-noter-extras-highlight-offset -5)
    (should (string-match-p "Highlight on page 15" (buffer-string)))))

(ert-deftest org-noter-extras-test-highlight-offset-no-match ()
  "Highlight-offset does not modify text without page headings."
  (with-temp-buffer
    (let ((text "** Some other heading\n"))
      (insert text)
      (org-noter-extras-highlight-offset 10)
      (should (equal (buffer-string) text)))))

;;;; Cleanup

(ert-deftest org-noter-extras-test-cleanup-annotation-abbreviated-page ()
  "Cleanup accepts abbreviated org-noter highlight headings."
  (with-temp-buffer
    (org-mode)
    (insert "* Work\n"
            ":PROPERTIES:\n"
            ":Custom_ID: Sebo2025MoralCircleWho\n"
            ":END:\n"
            "** Skeleton\n"
            "*** Highlight on p. 47\n"
            ":PROPERTIES:\n"
            ":NOTER_PAGE: (47 . 0.7512626666666666)\n"
            ":END:\n"
            "**** Contents\n"
            "As the history of our treatment of nonhuman animals illustrates.\n")
    (goto-char (point-min))
    (search-forward "**** Contents")
    (cl-letf (((symbol-function 'org-noter-sync-current-note) #'ignore)
              ((symbol-function 'other-window) #'ignore))
      (org-noter-extras-cleanup-annotation "Moral circle"))
    (should (string-match-p "\\*\\*\\* Moral circle" (buffer-string)))
    (should (string-match-p "#\\+begin_quote\nAs the history" (buffer-string)))
    (should (string-match-p "\\[cite:@Sebo2025MoralCircleWho, p\\. 47\\]" (buffer-string)))))

(ert-deftest org-noter-extras-test-cleanup-annotation-chapter ()
  "Cleanup formats chapter locators in citations."
  (with-temp-buffer
    (org-mode)
    (insert "* Work\n"
            ":PROPERTIES:\n"
            ":Custom_ID: Sebo2025MoralCircleWho\n"
            ":END:\n"
            "** Skeleton\n"
            "*** Highlight on chapter 2\n"
            "**** Contents\n"
            "The moral circle can expand.\n")
    (goto-char (point-min))
    (search-forward "**** Contents")
    (cl-letf (((symbol-function 'org-noter-sync-current-note) #'ignore)
              ((symbol-function 'other-window) #'ignore))
      (org-noter-extras-cleanup-annotation "Expansion"))
    (should (string-match-p "\\[cite:@Sebo2025MoralCircleWho, chap\\. 2\\]" (buffer-string)))))

;;;; Regexp matching

(ert-deftest org-noter-extras-test-highlight-heading-regexp ()
  "Highlight-heading-regexp matches org-noter highlight headings."
  (should (string-match org-noter-highlight-heading-regexp "Highlight on page 42"))
  (should (equal (match-string 1 "Highlight on page 42") "42")))

(ert-deftest org-noter-extras-test-highlight-heading-regexp-pages ()
  "Highlight-heading-regexp matches multi-page headings."
  (should (string-match org-noter-highlight-heading-regexp "Highlight on pages 42-43"))
  (should (equal (match-string 1 "Highlight on pages 42-43") "42-43")))

(ert-deftest org-noter-extras-test-highlight-heading-regexp-abbreviated-page ()
  "Highlight-heading-regexp matches abbreviated org-noter headings."
  (should (string-match org-noter-highlight-heading-regexp "Highlight on p. 47"))
  (should (equal (match-string 1 "Highlight on p. 47") "47")))

(provide 'org-noter-extras-test)
;;; org-noter-extras-test.el ends here
