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
;; (require 'ebib-extras) ; recusrive 
(require 'ebib-utils)
(require 'eww-extras)
(require 'paths)
(require 'tlon-babel-refs)
;; (require 'zotra) ; recursive

;;;; Functions

(defvar zotra-extras-most-recent-bibliography-file nil
  "The most recent bibliography file to which an entry was added.")

(defun zotra-extras-set-bibfile ()
  "Prompt the user to select a value for `org-cite-global-bibliography'."
  (require 'tlon-babel)
  (completing-read "Bibfile" (list
                              tlon-babel-refs-file-fluid
                              paths-file-personal-bibliography-new)))

(defun zotra-extras-add-entry-set-bibfile (orig-fun &optional
						    url entry-format bibfile)
  "Advice to set `org-cite-global-bibliography' before Zotra add commands.
ORIG-FUN, URL, ENTRY-FORMAT, and BIBFILE are arguments passed to
`zotra-add-entry'."
  (setq zotra-extras-most-recent-bibliography-file (zotra-extras-set-bibfile))
  (funcall orig-fun url entry-format zotra-extras-most-recent-bibliography-file))

(advice-add 'zotra-add-entry :around #'zotra-extras-add-entry-set-bibfile)

(defun zotra-after-get-bibtex-entry-hook-function ()
  "Function to trigger with `zotra-after-add-entry-hook'."
  ;; (revert-buffer nil t)
  (goto-char (point-max))
  (bibtex-extras-convert-titleaddon-to-journaltitle)
  (bibtex-set-field "timestamp" (format-time-string ebib-timestamp-format nil "GMT"))
  ;; (bibtex-extras-kill-field "keywords")
  (zotra-extras-fix-octal-sequences)
  (bibtex-clean-entry)
  (org-ref-clean-bibtex-entry)
  ;; (save-buffer)
  (ebib-switch-to-database-nth (ebib-extras-get-db-number zotra-extras-most-recent-bibliography-file))
  (let ((citekey (bibtex-extras-get-key)))
    (ebib-extras-open-or-switch)
    (ebib-extras-reload-database-no-confirm ebib--cur-db)
    (ebib--update-buffers)
    (ebib zotra-extras-most-recent-bibliography-file citekey)
    (ebib--index-sort "Timestamp" 'descend)
    (goto-char (point-min))
    (ebib-extras-open-key citekey)
    ;; we add this so that the latest entry is sorted in the bibtex
    ;; file, instead of remaining at the end of it
    (ebib-save-current-database t)))

(defun zotra-extras-url-full-capture (&optional url)
  "Add URL to bibfile and generate associated PDF and HTML files."
  (interactive)
  (require 'eww-extras)
  (let ((url (simple-extras-get-url url)))
    (unless ebib--cur-db
      (ebib))
    (zotra-add-entry url)
    (eww-extras-url-to-pdf url)
    (eww-extras-url-to-html url)))

;; Remarkably, this is needed because Emacs can't decode certain octal
;; sequences in Zotero-imported bibtex entries
(defun zotra-extras-fix-octal-sequences ()
  "Replace octal sequences with corresponding characters."
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

(provide 'zotra-extras)
;;; zotra-extras.el ends here
