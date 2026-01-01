;;; anki-editor-extras.el --- Extensions for anki-editor -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/anki-editor-extras.el
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

;; Extensions for `anki-editor'.

;;; Code:

(require 'anki-editor)
(require 'gptel)

;;;; User options

(defgroup anki-editor-extras ()
  "Extensions for `anki-editor'."
  :group 'anki-editor)

;;;; Functions

;;;;; set card position

;;;###autoload
(defun anki-editor-extras-set-card-position (card-id position)
  "Set CARD-ID's new-card position to POSITION via AnkiConnect.
CARD-ID and POSITION must be integers (or numeric strings).  POSITION is
1-indexed, matching Anki's `due' position for new cards. For example:

\\=(anki-editor-extras-set-card-position 1764877783576 1)"
  (let ((cid (if (stringp card-id) (string-to-number card-id) card-id))
        (pos (if (stringp position) (string-to-number position) position)))
    (unless (and (integerp cid) (> cid 0))
      (user-error "Invalid card id: %S" card-id))
    (unless (and (integerp pos) (> pos 0))
      (user-error "Invalid position: %S" position))
    (anki-editor-api-call-result
     'setSpecificValueOfCard
     :card cid
     :keys (vconcat '("due"))
     :newValues (vector pos))))

;;;;; plot summaries

(declare-function ebib-extras-get-field "ebib-extras")
(declare-function ebib-extras-key-is-valid-p "ebib-extras")
;;;###autoload
(defun anki-editor-extras-insert-film-plot-summary ()
  "Insert a one-paragraph plot summary for the film referenced by the current file.
When the current buffer is in `org-mode', this command uses the base file name
\\=(without extension) as a BibTeX key. If the key is valid per
`ebib-extras-key-is-valid-p', fetch the entry title via
`ebib-extras-get-field' and request a plot summary from gptel. The summary is
inserted at the end of the buffer under a level-2 heading \"Plot summary\".

After inserting the summary, prompt to optionally push the note to Anki."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an `org-mode' buffer"))
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))
  (let* ((key (file-name-base (buffer-file-name))))
    (if (ebib-extras-key-is-valid-p key)
	(anki-editor-extras--insert-plot-summary-for-key key)
      (user-error "Invalid BibTeX key: %s" key))))

(defun anki-editor-extras--insert-plot-summary-for-key (key)
  "Insert a one-paragraph plot summary for the film with BibTeX KEY.
After inserting, ask whether to push the note to Anki."
  (let* ((title (ebib-extras-get-field "title" key))
	 (date (ebib-extras-get-field "date" key))
	 (year (and date (substring date 0 4)))
	 (title-with-year (if year
			      (format "%s (%s)" title year)
			    title))
	 (author (ebib-extras-get-field "author" key))
	 (directors (anki-editor-extras--ankify-film-directors author))
	 (anki-front (format "What is the plot of %s (%s, %s)?" title directors year)))
    (unless title
      (user-error "Cannot determine title for BibTeX key %s" key))
    (unless year
      (user-error "Cannot determine year for BibTeX key %s" key))
    (unless directors
      (user-error "Cannot determine director last name(s) for BibTeX key %s" key))
    (save-excursion
      (goto-char (point-max))
      (unless (bolp)
	(insert "\n"))
      (insert "\n** Plot summary\n:PROPERTIES:\n:ANKI_FORMAT: nil\n:ANKI_DECK: Main::Started::Plot summaries\n:ANKI_NOTE_TYPE: Basic\n:ANKI_TAGS: film anki-editor\n:ANKI_FIELD_FRONT: " anki-front "\n:END:\n\n")
      (gptel-request
	  (format "Write a single-paragraph summary of the film's plot.\n\nTitle: %s\n\nConstraints:\n- One paragraph only.\n- Since the summary is for my personal reference, to remind myself of the plot of film’s I’ve seen, the summary can include spoilers.\n- Do not use bullet points.\n- Do not include a heading, title, or any prefatory text; output only the paragraph.\n -Do not mention the film’s title in the summary."
		  title-with-year)
	:buffer (current-buffer)
	:position (point)
	:in-place t))
    (when (y-or-n-p "Push plot summary note to Anki now? ")
      (save-excursion
        (goto-char (point-max))
        (org-back-to-heading t)
        (anki-editor-push-note-at-point)))))

(defun anki-editor-extras--ankify-film-directors (author-field)
  "Return formatted director last names from AUTHOR-FIELD.
AUTHOR-FIELD is expected to be a string of one or more directors, each in the
form \"LAST, FIRST\", separated by semicolons."
  (let* ((authors (split-string (or author-field "") ";" t "[ \t\n\r]+"))
	 (last-names (delq nil
			   (mapcar (lambda (a)
				     (let* ((a (string-trim a))
					    (parts (split-string a "," t "[ \t\n\r]+")))
				       (car parts)))
				   authors))))
    (pcase (length last-names)
      (0 nil)
      (1 (car last-names))
      (2 (format "%s & %s" (nth 0 last-names) (nth 1 last-names)))
      (_ (format "%s, %s & %s"
		 (mapconcat #'identity (butlast last-names 2) ", ")
		 (nth (- (length last-names) 2) last-names)
		 (car (last last-names)))))))

(provide 'anki-editor-extras)
;;; anki-editor-extras.el ends here
