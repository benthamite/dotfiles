;;; org-noter-extras.el --- Extensions for org-noter -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/org-noter-extras.el
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions for `org-noter'.

;;; Code:

(require 'org-noter)

;;;; Functions

;; All the functions below are super messy; revise

(defun org-noter-extras-cleanup-annotation (title)
  "Cleanup the annotation at point.
Replaces the generic \"Highlight on page X\" generated by
`org-noter-create-skeleton' with a custom TITLE and encloses the content of the
heading in a quote."
  (interactive "sTitle: ")
  (require 'unfill)
  (unless (derived-mode-p 'org-mode)
    (error "Not in an Org buffer"))
  (let ((initial-heading (org-get-heading t t t t))
        (highlight-heading-regexp "Highlight on page \\(.*\\)"))
    (save-restriction
      (save-excursion
        (cond
         ((string-match highlight-heading-regexp initial-heading)
          (org-back-to-heading)
          (org-next-visible-heading 1))
         ((string-match "Contents" initial-heading)
          (org-back-to-heading))
         (t
          (user-error "You must be either in a \"contents\" heading or in a \"highlight on page\" Heading")))
        (org-narrow-to-subtree)
        (org-end-of-meta-data t)
        (narrow-to-region (point) (point-max))
        (unfill-region (point) (point-max))
        (org-noter-extras-dehyphenate)
        (let ((content (buffer-substring-no-properties (point) (point-max))))
          (widen)
          (org-cut-subtree)
          (org-previous-visible-heading 1)
          (let* ((highlight-heading (org-get-heading t t t t))
                 (page (progn
                         (string-match highlight-heading-regexp highlight-heading)
                         (match-string 1 highlight-heading))))
            (org-edit-headline (format "%s, p. %s" title page))
            (org-narrow-to-subtree)
            (goto-char (point-max))
            (insert "\n#+begin_quote\n")
            (insert content)
            (insert "#+end_quote\n")
            (file-extras-remove-extra-blank-lines)
            (org-next-visible-heading 1)))))))

;; TODO: find `org-noter' hook to run this automatically
(defun org-noter-extras-dehyphenate ()
  "Remove leftover hyphens in hyphenated text.
Operate on the current paragraph, or the region if active."
  (interactive)
  (let ((start (if (use-region-p)
                   (region-beginning)
                 (save-excursion (backward-paragraph) (point))))
        (end (if (use-region-p)
                 (region-end)
               (save-excursion (forward-paragraph) (point)))))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\([[:alpha:]]\\)- \\([[:alpha:]]\\)" end t)
        (replace-match "\\1\\2")))))

;; TODO: find `org-noter' hook to run this automatically
(defun org-noter-extras-highlight-offset (offset)
  "Fix numbering mismatch between PDF and book/article pages.
OFFSET is the difference between the page number in the PDF and
the page number in the book or article."
  (interactive "nOffset: ")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(Highlight on page \\)\\(.*$\\)" nil t)
      (let* ((num (number-to-string (+ (string-to-number (match-string 2)) offset)))
             (replacement (concat (match-string 1) num)))
        (replace-match replacement)))))

(provide 'org-noter-extras)
;;; org-noter-extras.el ends here
