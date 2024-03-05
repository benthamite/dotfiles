;;; bibtex-extras.el --- Extensions for bibtex -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/bibtex-extras.el
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

;; Extensions for `bibtex'.

;;; Code:

(require 'bibtex)
(require 'doi-utils)
(require 'el-patch)
(require 'ebib)
;; (require 'ebib-extras)

;;;; Variables

(defconst bibtex-extras-biblatex-fields
  '("author"
    "editor"
    "title"
    "booktitle"
    "date"
    "journaltitle"
    "volume"
    "number"
    "issue"
    "edition"
    "series"
    "pages"
    "isbn"
    "issn"
    "doi"
    "url"
    "urldate"
    "publisher"
    "location"
    "chapter"
    "note"
    "addendum"
    "pubstate"
    "eprint"
    "eprinttype"
    "eprintclass"
    "file"
    "abstract"
    "keywords"
    "language"
    "langid"
    "translator"
    "annotator"
    "commentator"
    "introduction"
    "foreword"
    "afterword"
    "venue"
    "eventtitle"
    "eventdate"
    "venue"
    "organization"
    "institution"
    "school"
    "library"
    "archive"
    "index"
    "part"
    "version"
    "crossref"
    "related"
    "relatedtype"
    "entrysubtype"
    "nameaddon"
    "type"
    "maintitle"
    "mainsubtitle"
    "maintitleaddon"
    "booksubtitle"
    "booktitleaddon"
    "journaltitleaddon"
    "journalsubtitle"
    "issuetitle"
    "issuesubtitle"
    "issuetitleaddon"
    "editora"
    "editorb"
    "editorc"
    "translator"
    "commentator"
    "annotator"
    "introduction"
    "foreword"
    "afterword"
    "subtitle"
    "titleaddon"
    "editoratype"
    "editorbtype"
    "editorctype"
    "label"
    "options"
    "shorthand"
    "shorthandintro"
    "xref"
    "entryset"
    "execute"
    "howpublished"
    "sortkey")
  "A list of valid BibLaTeX fields.")

;;;; Functions

(defun bibtex-extras-replace-element-by-name (list target-name new-element)
  "Replace the element in LIST whose name matches TARGET-NAME with NEW-ELEMENT."
  (let ((current list))
    (while current
      (when (equal (caar current) target-name)
        (setcar current new-element))
      (setq current (cdr current))))
  list)

;; Adapted from xahlee.info/emacs/emacs/emacs_zap_gremlins.html
(defun bibtex-extras-asciify-text (&optional begin end)
  "Remove accents in some letters. e.g. café → cafe.
Change European language characters into equivalent ASCII ones.
When called interactively, work on current line or text selection.

Optionally, remove accents in region from BEGIN to END."
  (interactive)
  (let (($charMap
         [
          ["ß" "ss"]
          ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
          ["æ" "ae"]
          ["ç\\|č\\|ć" "c"]
          ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
          ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
          ["ñ\\|ň\\|ń" "n"]
          ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" "o"]
          ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự"     "u"]
          ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
          ["þ" "th"]
          ["ď\\|ð\\|đ" "d"]
          ["ĩ" "i"]
          ["ľ\\|ĺ\\|ł" "l"]
          ["ř\\|ŕ" "r"]
          ["š\\|ś" "s"]
          ["ť" "t"]
          ["ž\\|ź\\|ż" "z"]
	  ["­" ""]       ; soft hyphen
          [" " " "]       ; thin space
          ["–" "-"]       ; dash
          ["—\\|一" "--"] ; em dash etc
	  ["¿" ""]
	  ["¡" ""]
	  ["…" ""]
          ])
        ($p1 (if begin begin
               (if (region-active-p)
                   (region-beginning)
                 (line-beginning-position))))
        ($p2 (if end end
               (if (region-active-p)
                   (region-end)
                 (line-end-position)))))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region $p1 $p2)
        (mapc
         (lambda ($pair)
           (goto-char (point-min))
           (while (re-search-forward (elt $pair 0) (point-max) t)
             (replace-match (elt $pair 1))))
         $charMap)))))

(defun bibtex-extras-asciify-string (string)
  "Return a new STRING e.g. café → cafe."
  (with-temp-buffer
    (insert string)
    (bibtex-extras-asciify-text (point-min) (point-max))
    (buffer-string)))

(defun bibtex-extras-get-key ()
  "Return the key of the current BibTeX entry."
  (save-excursion
    (save-restriction
      (bibtex-narrow-to-entry)
      (goto-char (point-min))
      (if (re-search-forward "@\\w+{\\([^,]+\\),")
          (match-string-no-properties 1)
        (user-error "Not on a BibTeX entry")))))

(defun bibtex-extras-add-or-update-field (field value)
  "Add or update FIELD with VALUE in the current BibTeX entry."
  (bibtex-beginning-of-entry)
  ;; Check if FIELD exists
  (unless (bibtex-search-forward-field field)
    (bibtex-beginning-of-entry)
    (bibtex-make-field field t t))
  ;; Update the value of FIELD
  (when (bibtex-autokey-get-field field)
    (bibtex-set-field field value)))

(defun bibtex-extras-convert-titleaddon-to-journaltitle ()
  "Convert field `titleaddon' to `journaltitle' in entry at point.
When items are imported with the Zotero translation server, the online
publication venue is recorded in the `titleaddon' field. The correct Biblatex
field for this information is `journaltitle', so we move it there."
  (save-excursion
    (save-restriction
      (bibtex-narrow-to-entry)
      (bibtex-beginning-of-entry)
      (when (bibtex-autokey-get-field "titleaddon")
	(while (re-search-forward "titleaddon = " nil t)
          (replace-match "journaltitle = " nil nil))))))

(defun bibtex-extras-kill-field (field)
  "Kill FIELD in bibtex entry at point."
  (save-excursion
    (bibtex-narrow-to-entry)
    (bibtex-beginning-of-entry)
    (when-let ((pos (cadr (bibtex-search-forward-field field t))))
      (goto-char pos)
      (bibtex-kill-field))))

(defun bibtex-extras-move-entry (key target)
  "Move entry with KEY to TARGET bibliography file."
  (unless ebib--cur-db
    (user-error "Ebib does not seem to be open; please open it first"))
  (let ((source (ebib-extras-get-file-of-key key)))
    (when (string= source target)
      (user-error "`%s' is already in `%s'!" key target))
    (with-current-buffer (find-file-noselect source)
      (bibtex-search-entry key)
      (bibtex-kill-entry))
    (with-current-buffer (find-file-noselect target)
      (widen)
      (goto-char (point-max))
      (bibtex-yank)
      (save-buffer))
    (message "Moved entry %s to %s" key target)))

(defun bibtex-extras-append-to-field (field value &optional delimiter)
  "Append VALUE to FIELD.
If DELIMITER is nil, use a semicolon."
  (let* ((delimiter (or delimiter ";"))
	 (current-value (bibtex-autokey-get-field field))
	 (new-value (if (string-empty-p current-value)
			value
		      (concat current-value delimiter value))))
    (bibtex-extras-add-or-update-field field new-value)))

(defun bibtex-extras-open-in-ebib ()
  "Open the current BibTeX entry in Ebib."
  (interactive)
  (let ((file (buffer-file-name))
        (key (bibtex-extras-get-key)))
    (save-buffer)
    ;; If ebib is already open, we reload the database to insure it
    ;; reflects the current entry state.
    (when ebib--cur-db
      (ebib-switch-to-database-nth (ebib-extras-get-db-number file))
      (ebib-extras-reload-database-no-confirm ebib--cur-db))
    (ebib file key)
    (ebib--pop-to-buffer (ebib--buffer 'entry))))

;;;;; getters

(defun bibtex-extras-get-entry-as-string ()
  "Return the bibtex entry at point as a string."
  (save-excursion
    (save-restriction
      (bibtex-narrow-to-entry)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun bibtex-extras-get-field (field)
  "Return the value of FIELD in the current BibTeX entry."
  (save-excursion
    (save-restriction
      (bibtex-narrow-to-entry)
      (bibtex-beginning-of-entry)
      (let* ((bibtex-autokey-use-crossref nil)
	     (value (bibtex-autokey-get-field field)))
	(unless (string-empty-p value)
	  (replace-regexp-in-string "[\n\t ]+" " " value))))))

(defun bibtex-extras-get-field-name ()
  "Return the name of the field at point."
  (save-excursion
    (let* ((case-fold-search t)
           (bounds (bibtex-enclosing-field))
           (end (bibtex-end-of-field bounds)))
      (goto-char end)
      (bibtex-name-in-field bounds))))

(defun bibtex-extras-get-field-in-string (string field)
  "Return the value of FIELD in STRING."
  (save-window-excursion
    (with-temp-buffer
      (insert string)
      (bibtex-extras-get-field field))))

;;;;; setters

(defun bibtex-extras-set-field ()
  "Set FIELD to VALUE, creating it if necessary."
  (interactive)
  (let* ((field (completing-read "Field: " bibtex-extras-biblatex-fields))
	 (value (read-string "Value: " (bibtex-extras-get-field field))))
    (bibtex-set-field field (substring-no-properties value))))

;;;;; sorting of bibtex buffer

;; the two functions below are used to sort the bibtex files, via the user
;; option `bibtex-maintain-sorted-entries'
(defun bibtex-extras-entry-sorter ()
  "Return a list of the bibtex key and the `crossref' field of the entry at point."
  (list (bibtex-extras-get-key)
	(not (string-empty-p (bibtex-autokey-get-field "crossref")))
	nil))

(defun bibtex-extras-lessp (index1 index2)
  "Predicate for sorting BibTeX entries with indices INDEX1 and INDEX2.
Entries will be first be sorted as follows. Those with a `crossref' field will
be placed above the rest, and will be sorted in reverse alphabetical order. The
rest will be sorted alphabetically.

This sorting criterion replicates the Ebib criterion. That the entries with a
`crossref' field are sorted in reverse alphabetical order rather than
alphabetically appears to be a bug. But we replicate it for consistency’s sake:
this way when either Ebib or `bibtex' sorts the buffer, it won't be later
re-sorted by the other."
  (let ((key1 (nth 0 index1))
	(key2 (nth 0 index2))
	(crossref1 (nth 1 index1))
	(crossref2 (nth 1 index2)))
    (cond ((xor crossref1 crossref2)
	   crossref1)
	  ((and crossref1 crossref2)
	   (string< key2 key1))
	  (t
	   (string< key1 key2)))))

;;;;; attach downloads

(defun bibtex-extras-url-to-file-attach (type)
  "Generate PDF of file of TYPE."
  (when (bibtex-extras-get-field "url")
    (eww-extras-url-to-file type nil #'bibtex-extras-attach-file-to-entry)))

(defun bibtex-extras-url-to-pdf-attach ()
  "Generate PDF of URL."
  (interactive)
  (bibtex-extras-url-to-file-attach "pdf"))

(defun bibtex-extras-url-to-html-attach ()
  "Generate HTML of URL."
  (interactive)
  (bibtex-extras-url-to-file-attach "html"))

(defun bibtex-extras-attach-file-to-entry (&optional file bibtex-file)
  "Attach FILE to the relevant entry in BIBTEX-FILE.
The relevant entry is the entry in BIBTEX-FILE whose key equals the name of FILE
sans its extension."
  (let ((key (file-name-nondirectory (file-name-sans-extension file))))
    (save-excursion
      (with-current-buffer (find-file-noselect bibtex-file)
	(bibtex-search-entry key)
	(ebib-extras-attach-file file)
	(message "Attached `%s' to %s" file key)))))

;;;;; Patches

;; tweak function so that `bibtex-autokey-get-field' looks up `urldate' field
(el-patch-defun bibtex-autokey-get-year ()
  "Return year field contents as a string obeying `bibtex-autokey-year-length'."
  (let* ((str (bibtex-autokey-get-field '("date" "year" (el-patch-add "urldate")))) ; possibly ""
	 (year (or (and (iso8601-valid-p str)
			(let ((year (decoded-time-year (iso8601-parse str))))
			  (and year (number-to-string year))))
		   ;; BibTeX permits a year field "(about 1984)", where only
		   ;; the last four nonpunctuation characters must be numerals.
		   (and (string-match "\\([0-9][0-9][0-9][0-9]\\)[^[:alnum:]]*\\'" str)
			(match-string 1 str))
		   (user-error "Year or date field `%s' invalid" str))))
    (substring year (max 0 (- (length year) bibtex-autokey-year-length)))))

;; make `page-dashes' add an extra dash if missing, rather than remove it
(el-patch-defun bibtex-format-entry ()
  "Helper function for `bibtex-clean-entry'.
Formats current entry according to variable `bibtex-entry-format'."
  ;; initialize `bibtex-field-braces-opt' if necessary
  (if (and bibtex-field-braces-alist (not bibtex-field-braces-opt))
      (setq bibtex-field-braces-opt
	    (bibtex-field-re-init bibtex-field-braces-alist 'braces)))
  ;; initialize `bibtex-field-strings-opt' if necessary
  (if (and bibtex-field-strings-alist (not bibtex-field-strings-opt))
      (setq bibtex-field-strings-opt
	    (bibtex-field-re-init bibtex-field-strings-alist 'strings)))

  (let ((case-fold-search t)
	(format (if (eq bibtex-entry-format t)
		    '(realign opts-or-alts required-fields numerical-fields
			      page-dashes whitespace inherit-booktitle
			      last-comma delimiters unify-case braces
			      strings sort-fields)
		  bibtex-entry-format))
	(left-delim-re (regexp-quote (bibtex-field-left-delimiter)))
	bounds crossref-key req-field-list opt-field-list
	default-field-list field-list
	num-alt alt-fields idx error-field-name)
    (unwind-protect
	;; formatting (undone if error occurs)
	(atomic-change-group
	  (save-excursion
	    (save-restriction
	      (bibtex-narrow-to-entry)

	      ;; There are more elegant high-level functions for several tasks
	      ;; done by `bibtex-format-entry'.  However, they contain some
	      ;; redundancy compared with what we need to do anyway.
	      ;; So for speed-up we avoid using them.
	      ;; (`bibtex-format-entry' is called often by `bibtex-reformat'.)

	      ;; identify entry type
	      (goto-char (point-min))
	      (or (re-search-forward bibtex-entry-type nil t)
		  (user-error "Not inside a BibTeX entry"))
	      (let* ((beg-type (1+ (match-beginning 0)))
		     (end-type (match-end 0))
		     (entry-list (assoc-string (buffer-substring-no-properties
						beg-type end-type)
					       bibtex-entry-alist t)))

		;; unify case of entry type
		(when (memq 'unify-case format)
		  (delete-region beg-type end-type)
		  (insert (funcall bibtex-unify-case-function (car entry-list))))

		;; update left entry delimiter
		(when (memq 'delimiters format)
		  (goto-char end-type)
		  (skip-chars-forward " \t\n")
		  (delete-char 1)
		  (insert (bibtex-entry-left-delimiter)))

		;; Do we have a crossref key?
		(goto-char (point-min))
		(if (setq bounds (bibtex-search-forward-field
				  "\\(OPT\\)?crossref"))
		    (let ((text (bibtex-text-in-field-bounds bounds t)))
		      (unless (equal "" text)
			(setq crossref-key text))))

		;; list of required fields appropriate for an entry with
		;; or without crossref key.
		(setq req-field-list (append (nth 2 entry-list)
					     (unless crossref-key
					       (nth 3 entry-list)))
		      opt-field-list (append (if crossref-key
						 (nth 3 entry-list))
					     (nth 4 entry-list)
					     bibtex-user-optional-fields)
		      ;; default list of fields that may appear in this entry
		      default-field-list (append req-field-list opt-field-list)
		      ;; number of ALT fields we may find
		      num-alt (let ((n 0))
				(mapc (lambda (x)
					(if (nth 3 x)
					    (setq n (max n (abs (nth 3 x))))))
				      default-field-list)
				(1+ n))
		      ;; ALT fields of respective groups
		      alt-fields (make-vector num-alt nil))

		(when (memq 'sort-fields format)
		  (goto-char (point-min))
		  (let ((beg-fields (save-excursion (bibtex-beginning-first-field)))
			(fields-alist (bibtex-parse-entry
				       nil (not (memq 'opts-or-alts format))))
			bibtex-help-message elt)
		    (delete-region beg-fields (point))
		    (dolist (field default-field-list)
		      (when (setq elt (assoc-string (car field) fields-alist t))
			(setq fields-alist (delete elt fields-alist))
			(bibtex-make-field (list (car elt) nil (cdr elt)) nil nil t)))
		    (dolist (field fields-alist)
		      (unless (member (car field) '("=key=" "=type="))
			(bibtex-make-field (list (car field) nil (cdr field)) nil nil t))))))

	      ;; process all fields
	      (bibtex-beginning-first-field (point-min))
	      (while (setq bounds (bibtex-parse-field))
		(let* ((beg-field (copy-marker (bibtex-start-of-field bounds)))
		       (end-field (copy-marker (bibtex-end-of-field bounds) t))
		       (beg-name  (copy-marker (bibtex-start-of-name-in-field bounds)))
		       (end-name  (copy-marker (bibtex-end-of-name-in-field bounds)))
		       (beg-text  (copy-marker (bibtex-start-of-text-in-field bounds)))
		       (end-text  (copy-marker (bibtex-end-of-text-in-field bounds) t))
		       (empty-field (equal "" (bibtex-text-in-field-bounds bounds t)))
		       (field-name (buffer-substring-no-properties beg-name end-name))
		       (opt-alt   (and (memq 'opts-or-alts format)
				       (string-match "\\`\\(OPT\\|ALT\\)" field-name)
				       (not (and bibtex-no-opt-remove-re
						 (string-match bibtex-no-opt-remove-re
							       field-name)))))
		       deleted)
		  (if opt-alt (setq field-name (substring field-name 3)))

		  ;; keep track of alternatives
		  (if (and (not empty-field)
			   (setq idx (nth 3 (assoc-string field-name default-field-list t))))
		      (bibtex-vec-push alt-fields (abs idx) field-name))

		  (if (memq 'opts-or-alts format)
		      ;; delete empty optional and alternative fields
		      ;; (but keep empty required fields)
		      (cond ((and empty-field
				  (or opt-alt
				      (let ((field (assoc-string
						    field-name req-field-list t)))
					(or (not field) ; OPT field
					    (nth 3 field))))) ; ALT field
			     (delete-region beg-field end-field)
			     (setq deleted t))
			    ;; otherwise nonempty field: delete "OPT" or "ALT"
			    (opt-alt
			     (goto-char beg-name)
			     (delete-char 3))))

		  (unless deleted
		    (push field-name field-list)

		    ;; Remove whitespace at beginning and end of field.
		    ;; We do not look at individual parts of the field
		    ;; as {foo } # bar # { baz} is a fine field.
		    (when (memq 'whitespace format)
		      (goto-char beg-text)
		      (if (looking-at "\\([{\"]\\)[ \t\n]+")
			  (replace-match "\\1"))
		      (goto-char end-text)
		      (if (looking-back "[ \t\n]+\\([}\"]\\)" beg-text t)
			  (replace-match "\\1")))

		    ;; remove delimiters from purely numerical fields
		    (when (and (memq 'numerical-fields format)
			       (progn (goto-char beg-text)
				      (looking-at "\"[0-9]+\"\\|{[0-9]+}")))
		      (goto-char end-text)
		      (delete-char -1)
		      (goto-char beg-text)
		      (delete-char 1))

		    ;; update delimiters
		    (when (memq 'delimiters format)
		      (goto-char beg-text)
		      ;; simplified from `bibtex-parse-field-text', as we
		      ;; already checked that the field format is correct
		      (while (< (point) end-text)
			(if (looking-at bibtex-field-const)
			    (goto-char (match-end 0))
			  (let ((boundaries (bibtex-parse-field-string)))
			    (if (looking-at left-delim-re)
				(goto-char (cdr boundaries))
			      (delete-char 1)
			      (insert (bibtex-field-left-delimiter))
			      (goto-char (1- (cdr boundaries)))
			      (delete-char 1)
			      (insert (bibtex-field-right-delimiter)))))
			(if (looking-at "[ \t\n]*#[ \t\n]*")
			    (goto-char (match-end 0)))))

		    ;; update page dashes
		    (if (and (memq 'page-dashes format)
			     (string-equal-ignore-case field-name "pages")
			     (progn (goto-char beg-text)
				    (looking-at
				     (el-patch-swap "\\([\"{][0-9]+\\)[ \t\n]*--?[ \t\n]*\\([0-9]+[\"}]\\)"
						    "\\([\"{][0-9]+\\)[ \t\n]*-?[ \t\n]*\\([0-9]+[\"}]\\)"))))
			(replace-match (el-patch-swap "\\1-\\2" "\\1--\\2")))

		    ;; enclose field text by braces according to
		    ;; `bibtex-field-braces-alist'.
		    (let (case-fold-search temp) ; Case-sensitive search
		      (when (and (memq 'braces format)
				 (setq temp (cdr (assoc-string field-name
							       bibtex-field-braces-opt t))))
			(goto-char beg-text)
			(while (re-search-forward temp end-text t)
			  (let ((beg (match-beginning 0))
				(bounds (bibtex-find-text-internal nil t)))
			    (unless (or (nth 4 bounds) ; string constant
					;; match already surrounded by braces
					;; (braces are inside field delimiters)
					(and (< (point) (1- (nth 2 bounds)))
					     (< (1+ (nth 1 bounds)) beg)
					     (looking-at "}")
					     (save-excursion (goto-char (1- beg))
							     (looking-at "{"))))
			      (insert "}")
			      (goto-char beg)
			      (insert "{")))))

		      ;; replace field text by BibTeX string constants
		      ;; according to `bibtex-field-strings-alist'.
		      (when (and (memq 'strings format)
				 (setq temp (cdr (assoc-string field-name
							       bibtex-field-strings-opt t))))
			(goto-char beg-text)
			(dolist (re temp)
			  (while (re-search-forward (car re) end-text t)
			    (let ((bounds (save-match-data
					    (bibtex-find-text-internal nil t))))
			      (unless (nth 4 bounds)
				;; if match not at right subfield boundary...
				(if (< (match-end 0) (1- (nth 2 bounds)))
				    (insert " # " (bibtex-field-left-delimiter))
				  (delete-char 1))
				(replace-match (cdr re))
				(goto-char (match-beginning 0))
				;; if match not at left subfield boundary...
				(if (< (1+ (nth 1 bounds)) (match-beginning 0))
				    (insert (bibtex-field-right-delimiter) " # ")
				  (delete-char -1))))))))

		    ;; use book title of crossref'd entry
		    (if (and (memq 'inherit-booktitle format)
			     empty-field
			     (string-equal-ignore-case field-name "booktitle")
			     crossref-key)
			(let ((title (save-excursion
				       (save-restriction
					 (widen)
					 (if (bibtex-search-entry crossref-key t)
					     (bibtex-text-in-field "title"))))))
			  (when title
			    (setq empty-field nil)
			    (goto-char (1+ beg-text))
			    (insert title))))

		    ;; if empty field is a required field, complain
		    (when (and empty-field
			       (memq 'required-fields format)
			       (assoc-string field-name req-field-list t))
		      (setq error-field-name field-name)
		      (user-error "Mandatory field `%s' is empty" field-name))

		    ;; unify case of field name
		    (when (memq 'unify-case format)
		      (let ((fname (car (assoc-string field-name
						      default-field-list t)))
			    (curname (buffer-substring beg-name end-name)))
			(delete-region beg-name end-name)
			(goto-char beg-name)
			(insert (funcall bibtex-unify-case-function
					 (or fname curname)))))

		    ;; update point
		    (goto-char end-field))))

	      ;; check whether all required fields are present
	      (when (memq 'required-fields format)
		(let ((alt-expect (make-vector num-alt nil)))
		  (dolist (fname req-field-list)
		    (cond ((nth 3 fname)
			   ;; t if required field has alternative flag
			   (setq idx (abs (nth 3 fname)))
			   (bibtex-vec-push alt-expect idx (car fname)))
			  ((not (member-ignore-case (car fname) field-list))
			   (setq error-field-name (car fname))
			   (user-error "Mandatory field `%s' is missing"
				       (car fname)))))
		  (dotimes (idx num-alt)
		    (cond ((and (aref alt-expect idx)
				(not (aref alt-fields idx)))
			   (setq error-field-name
				 (car (last (aref alt-fields idx))))
			   (user-error "Alternative mandatory fields `%s' are missing"
				       (mapconcat 'identity
						  (reverse
						   (aref alt-expect idx))
						  ", ")))
			  ((nth 1 (aref alt-fields idx))
			   (setq error-field-name
				 (car (last (aref alt-fields idx))))
			   (user-error "Fields `%s' are alternatives"
				       (mapconcat 'identity
						  (reverse
						   (aref alt-fields idx))
						  ", ")))))))

	      ;; update comma after last field
	      (if (memq 'last-comma format)
		  (cond ((and bibtex-comma-after-last-field
			      (not (looking-at ",")))
			 (insert ","))
			((and (not bibtex-comma-after-last-field)
			      (looking-at ","))
			 (delete-char 1))))

	      ;; update right entry delimiter
	      (if (looking-at ",")
		  (forward-char))
	      (when (memq 'delimiters format)
		(skip-chars-forward " \t\n")
		(delete-char 1)
		(insert (bibtex-entry-right-delimiter)))

	      ;; realign and fill entry
	      (if (memq 'realign format)
		  (bibtex-fill-entry)))))

      ;; Unwindform: move point to location where error occurred if possible
      (if error-field-name
	  (let (bounds)
	    (when (save-excursion
		    (bibtex-beginning-of-entry)
		    (setq bounds
			  (bibtex-search-forward-field
			   ;; If we use the crossref field, a required field
			   ;; can have the OPT prefix
			   (concat "\\(OPT\\|ALT\\)?" error-field-name) t)))
	      (goto-char (bibtex-start-of-text-in-field bounds))
	      (bibtex-find-text)))))))

(provide 'bibtex-extras)
;;; bibtex-extras.el ends here

