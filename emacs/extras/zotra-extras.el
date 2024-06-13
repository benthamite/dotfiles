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

(require 'paths)
(require 'zotra)

;;;; Variables

(defvar zotra-extras-most-recent-bibfile nil
  "The bibfile of the most recently added entry.")

(defvar zotra-extras-most-recent-bibkey nil
  "The bibkey of the most recently added entry.")

(defconst zotra-extras-add-multiple-urls-filename
  (file-name-concat paths-dir-downloads "zotra-add-multiple-urls.txt")
  "Default file for `zotra-extras-add-multiple-urls'.")

;;;; Functions

(defvar ebib--cur-db)
(declare-function ebib "ebib")
(declare-function elfeed-extras-kill-link-url-of-entry "elfeed-extras")
(declare-function eww-copy-page-url "eww")
(defun zotra-extras-add-entry (&optional url-or-search-string entry-format bibfile)
  "Like `zotra-extras-add-entry', but set BIBFILE and open in Ebib.
Pass URL-OR-SEARCH-STRING and ENTRY-FORMAT to `zotra-get-entry'
to get the entry.
`zotra-add-entry'."
  (interactive)
  (let* ((bibfile (or bibfile
		      (setq zotra-extras-most-recent-bibfile (zotra-extras-set-bibfile)))))
    (pcase major-mode
      ('elfeed-show-mode (elfeed-extras-kill-link-url-of-entry))
      ('eww-mode (eww-copy-page-url)))
    (zotra-add-entry url-or-search-string entry-format bibfile)
    (zotra-extras-open-in-ebib zotra-extras-most-recent-bibkey)))

;;;;; Bibfile

(defvar tlon-file-fluid)
(defun zotra-extras-set-bibfile ()
  "Prompt the user to select a value for `org-cite-global-bibliography'."
  (completing-read "Bibfile" (list
                              tlon-file-fluid
                              paths-file-personal-bibliography-new)))

;;;;; Ebib

(declare-function ebib-switch-to-database-nth "ebib")
(declare-function ebib-save-current-database "ebib")
(declare-function ebib--update-buffers "ebib")
(declare-function ebib-extras-open-or-switch "ebib-extras")
(declare-function ebib-extras-get-db-number "ebib-extras")
(declare-function ebib-extras-reload-database-no-confirm "ebib-extras")
(declare-function ebib-extras-sort "ebib-extras")
(declare-function ebib-extras-open-key "ebib-extras")
(declare-function ebib-extras-process-entry "ebib-extras")
(defun zotra-extras-open-in-ebib (bibkey)
  "Open BIBKEY in Ebib after adding entry via `zotra-add-entry'."
  (ebib)
  (ebib-switch-to-database-nth (ebib-extras-get-db-number zotra-extras-most-recent-bibfile))
  (ebib-extras-open-or-switch)
  (ebib-extras-reload-database-no-confirm ebib--cur-db)
  (ebib--update-buffers)
  (ebib zotra-extras-most-recent-bibfile bibkey)
  (ebib-extras-sort 'Timestamp)
  (goto-char (point-min))
  (ebib-extras-open-key bibkey)
  ;; we add this so that the latest entry is sorted in the bibtex
  ;; file, instead of remaining at the end of it
  (ebib-save-current-database t)
  (if (y-or-n-p "Are the `type' and `=key=' fields correct? ")
      (ebib-extras-process-entry)
    (message "Fix the relevant fields, then run `ebib-extras-process-entry'.")))

;;;;; Cleanup

(defvar ebib-timestamp-format)
(declare-function org-ref-clean-bibtex-entry "org-ref-bibtex")
(declare-function bibtex-set-field "doi-utils")
(declare-function bibtex-extras-convert-titleaddon-to-journaltitle "bibtex-extras")
(declare-function bibtex-extras-get-key "bibtex-extras")
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

;;;;; Protocol

;; Just like `zotra-protocol' but with a call to `zotra-extras-add-entry' rather
;; than `zotra-add-entry'
(defun zotra-extras-protocol (info)
  (let ((url (plist-get info :url))
	(bibfile (plist-get info :bibfile))
	(entry-format (plist-get info :format))
	(zotra-multiple-item-strategy zotra-protocol-multiple-item-strategy))
    (message "Zotra received: `%s' to be saved in `%s'"
             url (or bibfile "zotra-default-bibliography"))
    (zotra-extras-add-entry url entry-format bibfile)
    nil))

(add-to-list 'org-protocol-protocol-alist
	     '("zotra-protocol"
	       :protocol "zotra"
	       :function zotra-extras-protocol))

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

(declare-function ebib-save-all-databases "ebib")
(declare-function ebib-extras-sort "ebib-extras")
(declare-function files-extras-lines-to-list "files-extras")
(defun zotra-extras-add-multiple-urls (file)
  "Prompt the user to select a FILE with a list of URLs and add them."
  (interactive (list (read-file-name "File with URLs (one URL per line): " paths-dir-downloads
				     zotra-extras-add-multiple-urls-filename nil nil)))
  (let ((urls (files-extras-lines-to-list file)))
    (ebib-save-all-databases)
    (dolist (url urls)
      (zotra-add-entry url nil tlon-file-fluid))
    (ebib tlon-file-fluid)
    (ebib-extras-sort 'Timestamp)))

(provide 'zotra-extras)
;;; zotra-extras.el ends here
