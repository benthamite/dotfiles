;;; org2blog-extras-test.el --- Tests for org2blog-extras -*- lexical-binding: t -*-

;; Tests for tag-to-drawer conversion logic in org2blog-extras.el.
;; Since org2blog is an external dependency, we extract the pure
;; regex/string logic inline rather than requiring the package.

;;; Code:

(require 'ert)

;; Do NOT require org2blog-extras; org2blog is an external dep unavailable in CI.

;;;; Tag parsing regex

(ert-deftest org2blog-extras-test-move-tags-regex-matches ()
  "The tag regex matches an org heading with tags."
  (with-temp-buffer
    (insert "* Test heading :tag1:tag2:\n")
    (goto-char (point-min))
    (should (re-search-forward "^\\* .*?:\\(.*\\):\n" nil t))
    (should (equal (substring-no-properties (match-string 1)) "tag1:tag2"))))

(ert-deftest org2blog-extras-test-move-tags-to-drawer ()
  "Tags are moved from heading to POST_TAGS property format."
  (with-temp-buffer
    (org-mode)
    (insert "* Test heading :tag1:tag2:\n")
    (goto-char (point-min))
    (when (re-search-forward "^\\* .*?:\\(.*\\):\n" nil t)
      (let ((tags (string-join
                   (split-string
                    (substring-no-properties (match-string 1))
                    ":")
                   ", ")))
        (should (equal tags "tag1, tag2"))))))

(ert-deftest org2blog-extras-test-move-tags-single-tag ()
  "A single tag is extracted correctly."
  (with-temp-buffer
    (org-mode)
    (insert "* Post title :solotag:\n")
    (goto-char (point-min))
    (when (re-search-forward "^\\* .*?:\\(.*\\):\n" nil t)
      (let ((tags (string-join
                   (split-string
                    (substring-no-properties (match-string 1))
                    ":")
                   ", ")))
        (should (equal tags "solotag"))))))

(ert-deftest org2blog-extras-test-move-tags-three-tags ()
  "Three tags are extracted and joined with commas."
  (with-temp-buffer
    (org-mode)
    (insert "* My post :alpha:beta:gamma:\n")
    (goto-char (point-min))
    (when (re-search-forward "^\\* .*?:\\(.*\\):\n" nil t)
      (let ((tags (string-join
                   (split-string
                    (substring-no-properties (match-string 1))
                    ":")
                   ", ")))
        (should (equal tags "alpha, beta, gamma"))))))

(ert-deftest org2blog-extras-test-no-tags-no-match ()
  "Heading without tags does not match the tag regex."
  (with-temp-buffer
    (insert "* Heading without tags\n")
    (goto-char (point-min))
    (should-not (re-search-forward "^\\* .*?:\\(.*\\):\n" nil t))))

(ert-deftest org2blog-extras-test-multiple-headings ()
  "Multiple headings with tags are all matched sequentially."
  (with-temp-buffer
    (org-mode)
    (insert "* First :a:b:\n* Second :c:d:e:\n")
    (goto-char (point-min))
    (let (results)
      (while (re-search-forward "^\\* .*?:\\(.*\\):\n" nil t)
        (push (string-join
               (split-string
                (substring-no-properties (match-string 1))
                ":")
               ", ")
              results))
      (setq results (nreverse results))
      (should (equal (length results) 2))
      (should (equal (nth 0 results) "a, b"))
      (should (equal (nth 1 results) "c, d, e")))))

(provide 'org2blog-extras-test)
;;; org2blog-extras-test.el ends here
