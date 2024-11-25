;;; org-ref-extras.el --- Extensions for org-ref -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/org-ref-extras.el
;; Version: 0.2
;; Package-Requires: ((org-ref "3.1") (el-patch "1.1"))

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

;; Extensions for `org-ref'.

;;; Code:

(require 'el-patch)
(require 'org-ref)
(require 'org-ref-isbn)

;;;; Functions

;;;;; Patched functions

;; Remove `bibtex-fill-entry'
(el-patch-defun org-ref-sort-bibtex-entry ()
  "Sort fields of entry in standard order."
  (interactive)
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
	 (entry-fields)
	 (other-fields)
	 (type (cdr (assoc "=type=" entry)))
	 (key (cdr (assoc "=key=" entry)))
	 (field-order (cdr (assoc (if type (downcase type))
				  org-ref-bibtex-sort-order))))

    ;; these are the fields we want to order that are in this entry
    (setq entry-fields (mapcar (lambda (x) (car x)) entry))
    ;; we do not want to reenter these fields
    (setq entry-fields (remove "=key=" entry-fields))
    (setq entry-fields (remove "=type=" entry-fields))

    ;;these are the other fields in the entry, and we sort them alphabetically.
    (setq other-fields
	  (sort (-remove (lambda(x) (member x field-order)) entry-fields)
		'string<))

    (save-restriction
      (bibtex-kill-entry)
      (insert
       (concat "@" type "{" key ",\n"
	       (mapconcat
		(lambda (field)
		  (when (member field entry-fields)
		    (format "%s = %s,"
			    field
			    (cdr (assoc field entry)))))
		field-order "\n")
	       ;; now add the other fields
	       (mapconcat
		(lambda (field)
		  (cl-loop for (f . v) in entry concat
			   (when (string= f field)
			     (format "%s = %s,\n" f v))))
		(-uniq other-fields) "\n")
	       "\n}"))
      (bibtex-search-entry key)
      (el-patch-remove (bibtex-fill-entry))
      (bibtex-clean-entry))))

;; Remove redundant and conflicting `Year' field
(declare-function org-ref-isbn-clean-bibtex-entry "org-ref-isbn")
(el-patch-defun isbn-to-bibtex (isbn bibfile)
  "Get bibtex entry for ISBN and insert it into BIBFILE.
Nothing happens if an entry with the generated key already exists
in the file. Data comes from www.ebook.de."
  (interactive
   (list
    (read-string
     "ISBN: "
     ;; now set initial input
     (cond
      ;; If region is active and it starts with a number, we use it
      ((and  (region-active-p)
	     (s-match "^[0-9]" (buffer-substring (region-beginning) (region-end))))
       (buffer-substring (region-beginning) (region-end)))
      ;; if first entry in kill ring starts with a number assume it is an isbn
      ;; and use it as the guess
      ((stringp (car kill-ring))
       (when (s-match "^[0-9]" (car kill-ring))
	 (car kill-ring)))
      ;; type or paste it in
      (t
       nil)))
    (completing-read "Bibfile: " (org-ref-possible-bibfiles))))

  (let* ((url (format "https://www.ebook.de/de/tools/isbn2bibtex?isbn=%s" isbn))
	 (entry))
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char (point-min))
      (when (re-search-forward "@[a-zA-Z]+{.+\\(\n\s+[^\n]+\\)+}$" nil t)
	(setq entry (match-string 0))
	(el-patch-add (setq entry (s-replace-regexp "^  Year =.*" "" entry)))))

    (if (not entry)
	(message "Nothing found.")
      (find-file bibfile)
      (goto-char (point-max))
      (insert (with-temp-buffer
		(insert (concat entry "\n}"))
		(goto-char (point-min))
		;; [2020-06-06 Sat] I got a report that ottobib returns entries
		;; with ,, in the first line. here if we find one, I eliminate
		;; one of them.
		(when (re-search-forward ",," nil t)
		  (delete-char -1))
		(org-ref-isbn-clean-bibtex-entry)
		(org-ref-clean-bibtex-entry)
		(bibtex-fill-entry)
		(s-trim (buffer-string))))
      (save-buffer))))

(provide 'org-ref-extras)
;;; org-ref-extras.el ends here

