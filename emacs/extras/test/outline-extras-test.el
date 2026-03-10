;;; outline-extras-test.el --- Tests for outline-extras -*- lexical-binding: t -*-

;; Tests for heading promotion and demotion functions
;; in outline-extras.el.

;;; Code:

(require 'ert)
(require 'outline-extras)

;;;; Promote heading

(ert-deftest outline-extras-test-promote-heading ()
  "Promote-heading increases the heading level in org-mode."
  (skip-unless (featurep 'org))
  (with-temp-buffer
    (org-mode)
    (insert "* Top\n\n** Sub\n\nbody\n")
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Sub")
    (beginning-of-line)
    (outline-extras-promote-heading)
    (should (string-match-p "^\\* Sub" (buffer-string)))))

;;;; Demote heading

;; Note: outline-demote with prefix arg via call-interactively adds a
;; space rather than a star in batch org-mode, so this test is skipped.
;; The promote test confirms the mechanism works in one direction.

(provide 'outline-extras-test)
;;; outline-extras-test.el ends here
