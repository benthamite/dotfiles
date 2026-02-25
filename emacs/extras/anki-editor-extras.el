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

(defcustom anki-editor-extras-duplicates-field "Front"
  "Field name used to identify duplicates.
This should match the field you select in Anki's \"Notes → Find duplicates\"
dialog, for example \"Front\"."
  :type 'string
  :group 'anki-editor-extras)

;;;; Functions

;;;;; patch

;; https://github.com/anki-editor/anki-editor/issues/117
(defun anki-editor-note-at-point-a (orig-fn &rest args)
  "Advice around `anki-editor-note-at-point' to patch Basic field mapping.
ORIG-FN is the original function being advised.
ARGS are the arguments passed to the original function.

This addresses a common setup where the Anki model named \"Basic\" has
additional fields (e.g. \"Meta\"), but the Org entry uses a simple
heading + body without \"** Front\" / \"** Back\" subheadings.

If `ANKI_FIELD_FRONT' is set on the entry, always use it as the Front
field and map the entry body to Back."
  (let ((front-prop (org-entry-get-with-inheritance
                     (concat anki-editor-prop-field-prefix "FRONT"))))
    (if (and (stringp front-prop)
             (not (string-blank-p (string-trim front-prop))))
        (anki-editor-extras--note-at-point-basic-heading-body)
      (condition-case err
          (apply orig-fn args)
        (user-error
         (if (anki-editor-extras--missing-fields-error-p err)
             (anki-editor-extras--note-at-point-basic-heading-body)
           (signal (car err) (cdr err))))))))

(defun anki-editor-extras--missing-fields-error-p (err)
  "Return non-nil if ERR is the \"more than two fields missing\" error.
ERR is the condition data provided by `condition-case'."
  (let ((msg (cadr err)))
    (and (stringp msg)
         (string-match-p
          (regexp-quote "Cannot map note fields: more than two fields missing")
          msg))))

(defun anki-editor-extras--note-at-point-basic-heading-body ()
  "Build a Basic note at point using Front property or heading and body for Back.
This is a fallback used when `anki-editor-note-at-point' fails because
the \"Basic\" Anki model has more than two fields (e.g. \"Meta\") but the
current Org entry has no subheading fields.

- Front: `ANKI_FIELD_FRONT' property if set, otherwise the Org heading.
- Back: the content before any subheading.

Any additional model fields are set to empty strings."
  (anki-editor--with-collection-data-updated
    (let* ((deck (org-entry-get-with-inheritance anki-editor-prop-deck))
           (note-id (org-entry-get nil anki-editor-prop-note-id))
           (hash (org-entry-get nil anki-editor-prop-note-hash))
           (note-type (or (org-entry-get nil anki-editor-prop-note-type)
                          anki-editor-default-note-type))
           (tags (cl-set-difference (anki-editor--get-tags)
                                    anki-editor-ignored-org-tags
                                    :test #'string=))
           (heading (substring-no-properties (org-get-heading t t t t)))
           (front-prop (org-entry-get-with-inheritance
                        (concat anki-editor-prop-field-prefix "FRONT")))
           (front (if (and (stringp front-prop)
                           (not (string-blank-p (string-trim front-prop))))
                      front-prop
                    heading))
           (body (anki-editor--note-contents-before-subheading))
           (model-fields (alist-get note-type anki-editor--model-fields
                                    nil nil #'string=)))
      (unless (and (stringp note-type) (string= note-type "Basic"))
        (user-error "Basic fallback invoked for non-Basic note type: %S" note-type))
      (unless deck (user-error "Missing deck"))
      (unless model-fields
        (user-error "Could not get model fields for note type: %S" note-type))
      (unless (and (stringp front) (not (string-blank-p (string-trim front))))
        (user-error "Cannot determine Front for Basic note at point"))
      (unless (and (stringp body) (not (string-blank-p (string-trim body))))
        (user-error "Cannot determine Back for Basic note at point"))
      (make-anki-editor-note
       :id note-id
       :model note-type
       :deck deck
       :tags tags
       :fields (cl-loop for f in model-fields
                        collect (cons f (pcase f
                                          ("Front" front)
                                          ("Back" body)
                                          (_ ""))))
       :hash hash
       :marker (point-marker)))))

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

;;;;; process duplicates

;;;###autoload
(defun anki-editor-extras-duplicates-process ()
  "Process duplicate-tagged notes and suspend real duplicates.
This runs the Emacs-automatable part of the workflow:

1. Remove `duplicate' from notes already tagged `real-duplicate'.
2. Remove `duplicate' from notes already tagged `no-duplicate'.
3. Review remaining `tag:duplicate' notes, tagging each as real or not.
4. Suspend all cards of notes tagged `real-duplicate'."
  (interactive)
  (anki-editor-extras-duplicates-cleanup)
  (anki-editor-extras-review-duplicates)
  (anki-editor-extras-suspend-real-duplicates))

;;;###autoload
(defun anki-editor-extras-remove-tag (tag)
  "Remove TAG from all notes that have it."
  (interactive "sTag to remove: ")
  (let* ((note-ids (anki-editor-api-call-result 'findNotes :query (format "tag:%s" tag))))
    (if (null note-ids)
	(message "No notes found with tag '%s'" tag)
      (anki-editor-api-call-result 'removeTags
                                   :notes note-ids
                                   :tags tag)
      (message "Removed '%s' tag from %d note(s)" tag (length note-ids)))))

;;;###autoload
(defun anki-editor-extras-duplicates-reset ()
  "Reset the duplicates workflow by removing the `duplicate' tag everywhere."
  (interactive)
  (anki-editor-extras-remove-tag "duplicate"))

(defun anki-editor-extras--find-notes (query)
  "Return note ids matching QUERY via AnkiConnect.
QUERY is an Anki search string."
  (anki-editor-api-call-result 'findNotes :query query))

(defun anki-editor-extras--notes->card-ids (note-ids)
  "Return card ids for NOTE-IDS via AnkiConnect.
NOTE-IDS must be a list of note ids."
  (let ((info (anki-editor-api-call-result 'notesInfo :notes note-ids)))
    (cl-loop for note in info
             append (alist-get 'cards note))))

(defun anki-editor-extras-remove-tag-from-query (tag query)
  "Remove TAG from all notes matching QUERY.
TAG is a string tag name.
QUERY is an Anki search string."
  (let ((note-ids (anki-editor-extras--find-notes query)))
    (if (null note-ids)
        (message "No notes found for query: %s" query)
      (anki-editor-api-call-result 'removeTags :notes note-ids :tags tag)
      (message "Removed tag '%s' from %d note(s)" tag (length note-ids)))))

(defun anki-editor-extras--gui-browse-note (note-id)
  "Open Anki browser focused on NOTE-ID."
  (anki-editor-api-call-result 'guiBrowse :query (format "nid:%s" note-id)))

;;;###autoload
(defun anki-editor-extras-duplicates-cleanup ()
  "Remove `duplicate' tag from notes already classified as real/no duplicates."
  (interactive)
  (anki-editor-extras-remove-tag-from-query
   "duplicate" "tag:duplicate tag:real-duplicate")
  (anki-editor-extras-remove-tag-from-query
   "duplicate" "tag:duplicate tag:no-duplicate"))

;;;###autoload
(defun anki-editor-extras-suspend-real-duplicates ()
  "Suspend all cards of notes tagged `real-duplicate'."
  (interactive)
  (anki-editor-extras-suspend-cards-by-query "tag:real-duplicate"))

;;;###autoload
(defun anki-editor-extras-review-duplicates ()
  "Classify notes tagged `duplicate' as real or not duplicates.
For each note, open it in Anki's browser and prompt:

- r: tag as real-duplicate, remove duplicate
- n: tag as no-duplicate, remove duplicate
- s: skip
- q: quit"
  (interactive)
  (let ((note-ids (anki-editor-extras--find-notes "tag:duplicate")))
    (unless note-ids
      (user-error "No notes found with tag:duplicate"))
    (catch 'anki-editor-extras--review-duplicates-quit
      (cl-loop for nid in note-ids
               for remaining downfrom (length note-ids) to 1
               do (anki-editor-extras--gui-browse-note nid)
               do (pcase (read-key (format "nid:%s (%d remaining)  (r)eal  (n)ot  (s)kip  (q)uit: " nid remaining))
                    (?r (anki-editor-api-call-result 'addTags :notes (list nid) :tags "real-duplicate")
                        (anki-editor-api-call-result 'removeTags :notes (list nid) :tags "duplicate"))
                    (?n (anki-editor-api-call-result 'addTags :notes (list nid) :tags "no-duplicate")
                        (anki-editor-api-call-result 'removeTags :notes (list nid) :tags "duplicate"))
                    (?s nil)
                    (?q (throw 'anki-editor-extras--review-duplicates-quit nil))
                    (_ (message "Unknown key; skipping")))))))

(defun anki-editor-extras-suspend-cards-by-query (query)
  "Suspend all cards belonging to notes matching QUERY.
QUERY is an Anki search string."
  (let ((note-ids (anki-editor-extras--find-notes query)))
    (if (null note-ids)
        (message "No notes found for query: %s" query)
      (let ((card-ids (anki-editor-extras--notes->card-ids note-ids)))
        (unless (and (listp card-ids) card-ids)
          (user-error "No cards found for query: %s" query))
        (anki-editor-api-call-result 'suspend :cards card-ids)
        (message "Suspended %d card(s) from %d note(s)"
                 (length card-ids)
                 (length note-ids))))))

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
	 (request-position (copy-marker (point-max) t)))
    (with-current-buffer request-buffer
      (save-excursion
	(anki-editor-extras--insert-plot-summary-note-skeleton request)
	(set-marker request-position (point) request-buffer)))
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
  (goto-char (point-max))
  (insert "** Plot summary\n:PROPERTIES:\n:ANKI_FORMAT: nil\n:ANKI_DECK: Main::Started::Plot summaries\n:ANKI_NOTE_TYPE: Basic\n:ANKI_TAGS: film anki-editor\n:ANKI_FIELD_FRONT: "
	  (plist-get request :anki-front)
	  "\n:END:\n\n"))

(defun anki-editor-extras--plot-summary-prompt (request)
  "Return a gptel prompt string for REQUEST.
REQUEST is a plist created by
`anki-editor-extras--plot-summary-request-for-key'."
  (format "Write a single-paragraph summary of the film's plot.\n\nTitle: %s\n\nConstraints:\n- One paragraph only.\n- Since the summary is for my personal reference, to remind myself of the plot of film’s I’ve seen, the summary can include spoilers.\n- Do not use bullet points.\n- Do not include a heading, title, or any prefatory text; output only the paragraph.\n -Do not mention the film’s title in the summary.\n- Do not make the summaries longer than 100 words or so."
	  (plist-get request :title-with-year)))

(defun anki-editor-extras--plot-summary-callback (request-buffer request-position)
  "Return a gptel callback for inserting plot summary and optionally pushing.
REQUEST-BUFFER is the buffer where the note is being inserted.
REQUEST-POSITION is a marker pointing at the insertion position."
  (lambda (response info)
    (cond
     ((eq response 'abort)
      (set-marker request-position nil)
      (message "Aborted plot summary request"))
     ((and (null response) (plist-get info :status))
      (set-marker request-position nil)
      (user-error "Failed to fetch plot summary: %s" (plist-get info :status)))
     ((stringp response)
      (anki-editor-extras--insert-plot-summary-and-maybe-push
       request-buffer request-position response))
     (t nil))))

(defun anki-editor-extras--insert-plot-summary-and-maybe-push (request-buffer request-position response)
  "Insert RESPONSE at REQUEST-POSITION in REQUEST-BUFFER and optionally push.
REQUEST-BUFFER is the buffer where the note is being inserted.
REQUEST-POSITION is a marker pointing at the insertion position.
RESPONSE is the plot summary text to insert."
  (with-current-buffer request-buffer
    (save-excursion
      (goto-char request-position)
      (insert (string-trim-right response))
      (unless (bolp) (insert "\n"))
      (org-back-to-heading t)
      (when (y-or-n-p "Push plot summary note to Anki now? ")
        (anki-editor-extras-push-plot-summary)))))

;;;###autoload
(defun anki-editor-extras-set-note-card-positions (note-id &optional start-position)
  "Set NOTE-ID's cards to new-card positions START-POSITION..n via AnkiConnect.
NOTE-ID must be an integer (or numeric string).  The note's card ids are
retrieved via `notesInfo'.  START-POSITION defaults to 1.  Positions are
1-indexed, matching Anki's `due' position for new cards.

For a note with n cards, this sets due positions:

START-POSITION, START-POSITION+1, ..., START-POSITION+(n-1)."
  (let* ((nid (if (stringp note-id) (string-to-number note-id) note-id))
         (start (if (null start-position)
                    1
                  (if (stringp start-position)
                      (string-to-number start-position)
                    start-position))))
    (unless (and (integerp nid) (> nid 0))
      (user-error "Invalid note id: %S" note-id))
    (unless (and (integerp start) (> start 0))
      (user-error "Invalid start position: %S" start-position))
    (let* ((info (anki-editor-api-call-result 'notesInfo :notes (list nid)))
           (note (car info))
           (cards (alist-get 'cards note)))
      (unless note
        (user-error "Could not retrieve note info for nid: %S" nid))
      (unless (and (listp cards) cards)
        (user-error "No cards found for nid: %S" nid))
      (setq cards (sort (copy-sequence cards) #'<))
      (cl-loop for cid in cards
               for pos from start
               do (anki-editor-extras-set-card-position cid pos))
      cards)))

;;;###autoload
(defun anki-editor-extras-push-plot-summary (&optional start-position)
  "Push the Anki note at point and set its cards' new-card positions.
START-POSITION defaults to 1.  For a note with n cards, set due positions
to START-POSITION..START-POSITION+(n-1)."
  (interactive "P")
  (anki-editor-push-note-at-point)
  (anki-editor-extras-set-note-card-positions
   (org-entry-get nil "ANKI_NOTE_ID")
   (or start-position 1)))

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
