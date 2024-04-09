;;; zotra-extras.el --- Extensions for zotra -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/zotra-extras.el
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

;; Extensions for `zotra'.

;;; Code:

(require 'bibtex-extras)
(require 'doi-utils)
(require 'ebib-utils)
(require 'eww-extras)
(require 'paths)
(require 'tlon-babel-tts)
(require 'zotra)

;;;; Variables

(defvar zotra-extras-most-recent-bibfile nil
  "The bibfile of the most recently added entry.")

(defvar zotra-extras-most-recent-bibkey nil
  "The bibkey of the most recently added entry.")

;;;; Functions

(defun zotra-extras-add-entry (&optional url-or-search-string entry-format bibfile)
  "Like `zotra-extras-add-entry', but set BIBFILE and open in Ebib.
Pass URL-OR-SEARCH-STRING and ENTRY-FORMAT to `zotra-get-entry'
to get the entry.
`zotra-add-entry'."
  (interactive)
  (let* ((bibfile (or bibfile
		      (setq zotra-extras-most-recent-bibfile (zotra-extras-set-bibfile)))))
    (zotra-add-entry url-or-search-string entry-format bibfile)
    (zotra-extras-open-in-ebib zotra-extras-most-recent-bibkey)))

(defun zotra-extras-url-full-capture (&optional url)
  "Add URL to bibfile and generate associated PDF and HTML files."
  (interactive)
  (let ((url (or url
		 (read-string "Add entry from URL or search identifier: " (current-kill 0)))))
    (unless ebib--cur-db
      (ebib))
    (zotra-add-entry url)
    (eww-extras-url-to-pdf url)
    (eww-extras-url-to-html url)))

;;;;; Bibfile

(defvar tlon-babel-refs-file-fluid)
(defun zotra-extras-set-bibfile ()
  "Prompt the user to select a value for `org-cite-global-bibliography'."
  (completing-read "Bibfile" (list
                              tlon-babel-refs-file-fluid
                              paths-file-personal-bibliography-new)))

;;;;; Ebib

(defun zotra-extras-open-in-ebib (bibkey)
  "Open BIBKEY in Ebib after adding entry via `zotra-add-entry'."
  (ebib-switch-to-database-nth (ebib-extras-get-db-number zotra-extras-most-recent-bibfile))
  (ebib-extras-open-or-switch)
  (ebib-extras-reload-database-no-confirm ebib--cur-db)
  (ebib--update-buffers)
  (ebib zotra-extras-most-recent-bibfile bibkey)
  (ebib--index-sort "Timestamp" 'descend)
  (goto-char (point-min))
  (ebib-extras-open-key bibkey)
  ;; we add this so that the latest entry is sorted in the bibtex
  ;; file, instead of remaining at the end of it
  (ebib-save-current-database t)
  (if (y-or-n-p "Are the `type', `author', `date' and `title' fields correct? ")
      (ebib-extras-process-entry)
    (message "Fix the `type', `author', `date' and `title' fields, then run `ebib-extras-process-entry'.")))

;;;;; Cleanup

(declare-function org-ref-clean-bibtex-entry "org-ref-bibtex")
(defun zotra-extras-after-add-process-bibtex ()
  "Process newly added bibtex entry."
  ;; TODO: check that there are no unsaved changes in
  ;; `zotra-extras-most-recent-bibfile'
  (goto-char (point-max))
  (bibtex-extras-convert-titleaddon-to-journaltitle)
  (bibtex-set-field "timestamp" (format-time-string ebib-timestamp-format nil "GMT"))
  (zotra-extras-fix-octal-sequences)
  (bibtex-clean-entry)
  (org-ref-clean-bibtex-entry)
  (setq zotra-extras-most-recent-bibkey (bibtex-extras-get-key)))

(defun zotra-extras-fix-octal-sequences ()
  "Replace octal sequences with corresponding characters.
Remarkably, this is needed because Emacs can't decode certain octal sequences in
Zotero-imported bibtex entries."
  (save-excursion
    (dotimes (i 79)
      (dolist  (pattern '("\"\\302\\%o\""
                          "\"\\303\\%o\""
                          "\"\\314\\%o\""
                          "\"\\342\\200\\%o\""))
        (let* ((octal (read (format pattern (+ #o200 i))))
               (char (decode-coding-string octal 'utf-8)))
          (goto-char (point-min))
          (while (search-forward octal nil t)
            (replace-match char)))))))

;;;;; Misc

(defun zotra-extras-fetch-field (field url-or-search-string &optional ignore-errors)
  "Get FIELD value in bibliographic entry for URL-OR-SEARCH-STRING.
If IGNORE-ERRORS is non-nil, handle error thrown by `zotra-get-entry-1'
gracefully."
  (let* ((query-result (zotra-query-url-or-search-string url-or-search-string))
	 (data (car query-result))
	 (endpoint (cdr query-result))
	 (entry (if ignore-errors
                    (condition-case nil
			(zotra-get-entry-1 data zotra-default-entry-format endpoint)
		      (error nil))
                  (zotra-get-entry-1 data zotra-default-entry-format endpoint))))
    (when entry
      (with-temp-buffer
        (insert entry)
        (bibtex-mode)
        (let ((value (bibtex-autokey-get-field field)))
          (if (string-empty-p value)
              nil
            value))))))

(provide 'zotra-extras)
;;; zotra-extras.el ends here
