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
(require 'cl-lib)
(require 'gptel)
(require 'org)
(require 'subr-x)

;;;; User options

(defgroup anki-editor-extras ()
  "Extensions for `anki-editor'."
  :group 'anki-editor)

;;;; Functions

;;;;; patch

;; https://github.com/anki-editor/anki-editor/issues/117
(defun anki-editor-note-at-point-a (orig-fn &rest args)
  "Advice around `anki-editor-note-at-point' to patch Basic field mapping.
ORIG-FN is the original function being advised.
ARGS are the arguments passed to the original function.

This ensures that for Basic notes we always produce exactly two fields
(\"Front\" and \"Back\"), even if Org/Anki field mapping would otherwise
fail."
  (anki-editor--patch-basic-note
   (apply orig-fn args)))

(defun anki-editor--patch-basic-note (note)
  "Rewrite NOTE fields for \"Basic\" notes to ensure a valid mapping.

For Basic notes, anki-editor expects exactly two fields: \"Front\" and
\"Back\".  If the user uses properties like ANKI_FIELD_FRONT and/or body
text instead of \"** Front\" / \"** Back\" subheadings, mapping can fail
with: \"Cannot map note fields: more than two fields missing\".

NOTE is an `anki-editor-note' struct."
  (let ((model (anki-editor-note-model note)))
    (when (and (stringp model) (string= model "Basic"))
      (with-current-buffer (marker-buffer (anki-editor-note-marker note))
        (save-excursion
          (goto-char (anki-editor-note-marker note))
          (let* ((heading (substring-no-properties (org-get-heading t t t t)))
                 (front-prop (org-entry-get nil "ANKI_FIELD_FRONT"))
                 (front (cond ((and (stringp front-prop)
                                    (not (string-blank-p front-prop)))
                               front-prop)
                              ((and (stringp heading)
                                    (not (string-blank-p heading)))
                               heading)
                              (t nil)))
                 (body (anki-editor--note-contents-before-subheading))
                 (back (and (stringp body)
                            (not (string-blank-p (string-trim body)))
                            body)))
            (unless front
              (user-error "Cannot determine Front for Basic note at point"))
            (unless back
              (user-error "Cannot determine Back for Basic note at point"))
            (setf (anki-editor-note-fields note)
                  (list (cons "Front" front)
                        (cons "Back" back)))))))
    note))

(advice-add 'anki-editor-note-at-point :around #'anki-editor-note-at-point-a)

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
  (let* ((request (anki-editor-extras--plot-summary-request-for-key key))
	 (request-buffer (current-buffer))
	 (request-position (point-marker)))
    (anki-editor-extras--insert-plot-summary-note-skeleton request)
    (set-marker request-position (point) request-buffer)
    (gptel-request
	(anki-editor-extras--plot-summary-prompt request)
      :buffer request-buffer
      :position request-position
      :in-place t
      :callback (anki-editor-extras--plot-summary-callback
		 request-buffer request-position))))

(defun anki-editor-extras--plot-summary-request-for-key (key)
  "Build a plot-summary request plist for BibTeX KEY.
The returned plist contains :title, :year, :title-with-year, and
:anki-front."
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
    (list :title title
	  :year year
	  :title-with-year title-with-year
	  :anki-front anki-front)))

(defun anki-editor-extras--insert-plot-summary-note-skeleton (request)
  "Insert the plot summary note skeleton described by REQUEST.
REQUEST is a plist created by
`anki-editor-extras--plot-summary-request-for-key'."
  (save-excursion
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert "\n** Plot summary\n:PROPERTIES:\n:ANKI_FORMAT: nil\n:ANKI_DECK: Main::Started::Plot summaries\n:ANKI_NOTE_TYPE: Basic\n:ANKI_TAGS: film anki-editor\n:ANKI_FIELD_FRONT: "
	    (plist-get request :anki-front)
	    "\n:END:\n\n")))

(defun anki-editor-extras--plot-summary-prompt (request)
  "Return a gptel prompt string for REQUEST.
REQUEST is a plist created by
`anki-editor-extras--plot-summary-request-for-key'."
  (format "Write a single-paragraph summary of the film's plot.\n\nTitle: %s\n\nConstraints:\n- One paragraph only.\n- Since the summary is for my personal reference, to remind myself of the plot of film’s I’ve seen, the summary can include spoilers.\n- Do not use bullet points.\n- Do not include a heading, title, or any prefatory text; output only the paragraph.\n -Do not mention the film’s title in the summary."
	  (plist-get request :title-with-year)))

(defun anki-editor-extras--plot-summary-callback (request-buffer request-position)
  "Return a gptel callback for inserting plot summary and optionally pushing.
REQUEST-BUFFER is the buffer where the note is being inserted.
REQUEST-POSITION is a marker pointing at the insertion position."
  (lambda (response info)
    (when (eq response 'abort)
      (set-marker request-position nil)
      (message "Aborted plot summary request"))
    (when (and (null response) (plist-get info :status))
      (set-marker request-position nil)
      (user-error "Failed to fetch plot summary: %s" (plist-get info :status)))
    (when (eq response t)
      (set-marker request-position nil)
      (with-current-buffer request-buffer
	(save-excursion
	  (goto-char (point-max))
	  (org-back-to-heading t)
	  (when (y-or-n-p "Push plot summary note to Anki now? ")
	    (anki-editor-extras-push-plot-summary)))))))

;;;###autoload
(defun anki-editor-extras-push-plot-summary ()
  "Push the Anki note at point and set its new-card position to 1."
  (interactive)
  (anki-editor-push-note-at-point)
  (anki-editor-extras-set-card-position (org-entry-get nil "ANKI_NOTE_ID") 1))

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
