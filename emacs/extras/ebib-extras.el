;;; ebib-extras.el --- Extensions for ebib -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.2
;; Package-Requires: ((annas-archive "0.1") (el-patch "1.1") (ebib "2.0") (paths "0.1") (shut-up "0.3.1"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Extensions for `ebib'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'el-patch)
(require 'ebib)
(require 'paths)
(require 'shut-up)
(require 'annas-archive)

;;;; User options

(defgroup ebib-extras ()
  "Provide extensions for `ebib', the Emacs BibTeX database manager."
  :group 'ebib)

(defcustom ebib-extras-download-use-vpn nil
  "Determine whether to use a VPN when downloading content.
If non-nil, functions that download content might attempt to use a VPN."
  :type 'boolean
  :group 'ebib-extras)

;;;; Variables

(defconst ebib-extras-sort-states
  '(Timestamp Author Title)
  "List of states for sorting the Ebib index buffer.
These states correspond to the criteria by which the Ebib index can be sorted.")

(defvar ebib-extras-sort-state
  (car ebib-extras-sort-states)
  "Current state for sorting the Ebib index buffer.
This variable holds the active sorting criterion, taken from
`ebib-extras-sort-states'.")

(defvar ebib-extras-last-reload-times (make-hash-table :test 'equal)
  "A hash table mapping database file paths to their last reload time.")

;; The variable ebib-extras-attach-file-key was removed as it's unreliable
;; in async contexts. The annas-archive package might need updating if it
;; still references this variable internally.

(defconst ebib-extras-valid-key-regexp
  "^[_[:alnum:]-]\\{2,\\}[[:digit:]]\\{4\\}[_[:alnum:]]\\{2,\\}$"
  "Regular expression defining the structure of valid BibTeX keys.")

(defconst ebib-extras-valid-file-extensions
  '("pdf" "html" "webm" "flac" "mp3" "md" "srt" "vtt")
  "List of valid file extensions used by `ebib-extras-open-file-dwim'.
This list defines the preference order when opening files.")

(defconst ebib-extras-valid-text-file-extensions
  '("html" "pdf" "srt" "vtt")
  "List of valid file extensions considered as text files.
Used by `ebib-extras-get-text-file'.")

(defvar tlon-file-fluid) ; Assuming this is defined elsewhere or is a placeholder
(defconst ebib-extras-auto-save-files
  `(,paths-file-personal-bibliography-new
    ,tlon-file-fluid)
  "List of database files that should be auto-saved.
The large files containing older bibliographic entries are typically excluded to
avoid frequent writes.")

;;;; Functions

(defvar window-extras-frame-split-width-threshold)
(declare-function window-extras-split-if-unsplit "window-extras")
(declare-function winum-select-window-2 "winum")
(declare-function winum-select-window-3 "winum")
;;;###autoload
(defun ebib-extras-open-or-switch ()
  "Open Ebib in an appropriate window or switch to it if already open.
This function splits the current window if necessary and selects a target window
\\=(typically the second or third) before invoking `ebib'."
  (interactive)
  (window-extras-split-if-unsplit)
  (if (> (frame-width) window-extras-frame-split-width-threshold)
      (winum-select-window-3)
    (winum-select-window-2))
  (ebib))

;;;###autoload
(defun ebib-extras-reload-database-no-confirm (db)
  "Reload the Ebib database DB from disk without asking for confirmation.
If the database is modified, it will still prompt unless `yes-or-no-p' returns
true. It preserves the current entry key and updates buffers."
  (ebib--execute-when
    (entries
     (unless (and (ebib-db-modified-p db)
		  (not (yes-or-no-p "Database modified.  Really reload from file? ")))
       (ebib-db-set-current-entry-key (ebib--get-key-at-point) db)
       (ebib--reload-database db)
       (ebib--set-modified nil db)
       ;; (ebib--update-buffers) 
       ))
    (default
     (beep))))

(defconst ebib-extras-isbn-p
  "\\(ISBN-*\\(1[03]\\)* *\\(: \\)?\\)*\\(\\([0-9Xx][ -]*\\)\\{13\\}\\|\\([0-9Xx][ -]*\\)\\{10\\}\\)"
  "Regular expression to identify ISBN numbers (10 or 13 digits).")

(defun ebib-extras-isbn-p (string)
  "Return t if STRING matches the ISBN pattern `ebib-extras-isbn-p'."
  (string-match ebib-extras-isbn-p string))

(declare-function bibtex-extras-get-field "bibtex-extras")
(declare-function s-replace "s")
(defun ebib-extras-get-isbn (&optional key)
  "Return the ISBN for the entry with KEY, or the current entry if KEY is nil.
The ISBN is retrieved from the \"isbn\" field, hyphens are removed, and only the
first part (if multiple ISBNs are listed) is returned."
  (let ((target-key (or key (ebib--get-key-at-point))))
    ;; Use ebib-extras-get-field directly as it now handles the key
    (when-let* ((isbn (ebib-extras-get-field "isbn" target-key)))
      (car (split-string
            (s-replace "-"
                       ""
                       (substring-no-properties
                        isbn))
            " ")))))

(defun ebib-extras-video-p (string)
  "Return t if STRING appears to be a video URL from supported sites.
Currently, this checks for YouTube URLs.
;; TODO: Add more video sites."
  (string-match
   "https?://\\(www\\.\\)?\\(youtube\\.com/watch\\?v=\\|youtu.be/\\)\\([a-zA-Z0-9_-]+\\)"
   string))

(defun ebib-extras--update-file-field-contents (key file-name)
  "Update the `file' field of entry KEY with FILE-NAME.
This function operates directly on the Ebib database and avoids UI interaction,
making it suitable for asynchronous callbacks. If FILE-NAME is already present
in the field, no change is made. The database is marked as modified."
  (let* ((db (catch 'found
	       (dolist (d ebib--databases)
		 (when (ebib-db-has-key key d)
		   (throw 'found d)))
	       nil))
	 (field "file"))
    (unless db
      (error "Cannot find database containing key %s" key))
    (let* ((entry (ebib-db-get-entry key db))
	   (file-field-contents (alist-get field entry nil nil #'string-equal)))
      (unless (and file-field-contents
		   (catch 'file-exists
		     (dolist (file (ebib--split-files file-field-contents))
		       (when (string= (string-trim file) file-name)
			 (throw 'file-exists file)))))
	(ebib-set-field-value field file-name key db ";")
	(ebib--set-modified t db)
	(setq ebib--needs-update t)
	(ebib--save-database db '(16))))))

(defconst ebib-extras-book-like-entry-types
  (let ((lowercase '("book" "collection" "mvbook" "inbook" "incollection"
                     "bookinbook" "suppbook")))
    (append lowercase (mapcar (lambda (entry)
				(concat (upcase (substring entry 0 1))
					(substring entry 1)))
			      lowercase)))
  "List of BibTeX entry types considered as books or book-like entities.
Includes both lowercase and capitalized versions of types like \"book\",
\"collection\", etc.")

(defconst ebib-extras-article-like-entry-types
  (let ((lowercase '("article")))
    (append lowercase (mapcar (lambda (entry)
				(concat (upcase (substring entry 0 1))
					(substring entry 1)))
			      lowercase)))
  "List of BibTeX entry types considered as articles or article-like entities.
Includes both lowercase and capitalized versions of types like \"article\".")

(defconst ebib-extras-video-websites
  '("youtube\\.com" "youtu\\.be")
  "List of regular expressions matching hostnames of video websites.
Used to identify video URLs, e.g., for subtitle download logic.")

(defconst ebib-extras-film-like-entry-types
  (let ((lowercase '("movie" "video" "tvepisode")))
    (append lowercase (mapcar (lambda (entry)
				(concat (upcase (substring entry 0 1))
					(substring entry 1)))
			      lowercase)))
  "List of BibTeX entry types considered as films or film-like entities.
Includes both lowercase and capitalized versions of types like \"movie\",
\"video\", etc.")

(defun ebib-extras-copy-current-field-contents ()
  "Copy the contents of the current field at point to the kill ring.
If the field contains a value from a cross-referenced entry, that resolved value
is copied. This is a wrapper around `ebib-copy-field-contents'."
  (interactive)
  (ebib-copy-field-contents (ebib--current-field)))

;;;###autoload
(defun ebib-extras-get-file (extension)
  "Return the path of the file with EXTENSION in the entry at point.
The function determines the correct way to get field values based on the current
major mode (`ebib-entry-mode' or `bibtex-mode'). A file path is returned if a
file with the given EXTENSION is uniquely found in the \"file\" field of the
entry. Returns nil otherwise."
  (let ((get-field (pcase major-mode
		     ('ebib-entry-mode #'ebib-extras-get-field)
		     ('bibtex-mode #'bibtex-extras-get-field))))
    (when-let* ((files (funcall get-field "file")))
      (ebib-extras-get-file-in-string files extension))))

(defun ebib-extras-get-file-in-string (files extension)
  "Return the path of the file with EXTENSION in FILES."
  (catch 'tag
    (mapc
     (lambda (file)
       (when (equal (file-name-extension file) extension)
	 (throw 'tag (expand-file-name file))))
     (ebib--split-files files))
    nil))

(defun ebib-extras-get-text-file ()
  "Return the path of the first text file found for the entry at point.
It iterates through `ebib-extras-valid-text-file-extensions' and returns the
first file matching an extension found via `ebib-extras-get-file'."
  (catch 'tag
    (dolist (extension ebib-extras-valid-text-file-extensions)
      (when-let* ((file (ebib-extras-get-file extension)))
	(throw 'tag file)))))

(defun ebib-extras-open-file (extension)
  "Open the file with EXTENSION associated with the entry at point.
A file will be opened using `find-file' if it uniquely exists (found via
`ebib-extras-get-file'). Otherwise, a user error is signaled.
EXTENSION is a string representing the file extension."
  (interactive "sFile extension: ")
  (if-let* ((file-name (ebib-extras-get-file extension)))
      (find-file file-name)
    (user-error (format "No (unique) `%s' file found" extension))))

(defun ebib-extras-open-file-externally (extension)
  "Open the file with EXTENSION for the entry at point externally.
The file is opened via `shell-command' with \"open\" if it uniquely exists
\\=(found via `ebib-extras-get-file'). Otherwise, a user error is signaled.
EXTENSION is a string representing the file extension."
  (interactive "sFile extension: ")
  (if-let* ((file-name (expand-file-name (ebib-extras-get-file extension))))
      (shell-command (format "open '%s'" file-name))
    (user-error (format "No (unique) `%s' file found" extension))))

(defun ebib-extras-open-pdf-file ()
  "Open the PDF file associated with the entry at point, if it uniquely exists."
  (interactive)
  (ebib-extras-open-file "pdf"))

(defun ebib-extras-open-md-file ()
  "Open the Markdown (md) file for the entry at point, if it uniquely exists."
  (interactive)
  (ebib-extras-open-file "md"))

(defun ebib-extras-open-srt-file ()
  "Open the SRT subtitle file for the entry at point, if it uniquely exists."
  (interactive)
  (ebib-extras-open-file "srt"))

(defun ebib-extras-open-vtt-file ()
  "Open the VTT subtitle file for the entry at point, if it uniquely exists."
  (interactive)
  (ebib-extras-open-file "vtt"))

(defun ebib-extras-open-pdf-file-externally ()
  "Open the PDF file for the entry at point externally, if it uniquely exists."
  (interactive)
  (ebib-extras-open-file-externally "pdf"))

(defun ebib-extras-open-webm-file ()
  "Open WebM video file for the entry at point externally, if it uniquely exists."
  (interactive)
  (ebib-extras-open-file-externally "webm"))

(defun ebib-extras-open-mp3-file ()
  "Open MP3 audio file for the entry at point externally, if it uniquely exists."
  (interactive)
  (ebib-extras-open-file-externally "mp3"))

(defvar browse-url-handlers) ; Should be declared if it's a special variable
(defun ebib-extras-open-html-file ()
  "Open the HTML file for the entry at point in EWW, if it uniquely exists.
The original HTML buffer is killed after rendering in EWW."
  (interactive)
  (ebib-extras-open-file "html")
  (let ((html-buffer (buffer-name))
	(browse-url-handlers nil)
	(browse-url-browser-function #'eww-browse-url))
    (browse-url-of-buffer)
    (kill-buffer html-buffer)))

(defun ebib-extras-open-html-file-externally ()
  "Open the HTML file for the entry at point externally, if it uniquely exists."
  (interactive)
  (ebib-extras-open-file-externally "html"))

(defun ebib-extras-open-file-dwim ()
  "Open an associated file for the entry at point \\(Do What I Mean).
If the entry has multiple files, it uses the preference ordering defined in
`ebib-extras-valid-file-extensions' and calls the corresponding
`ebib-extras-open-EXT-file' function for the first one found."
  (interactive)
  (if-let* ((extension
	    (catch 'tag
	      (dolist (extension ebib-extras-valid-file-extensions)
		(when (ebib-extras-get-file extension)
		  (throw 'tag extension))))))
      (call-interactively
       (intern
	(concat "ebib-extras-open-" extension "-file")))
    (user-error "No file found")))

(defun ebib-extras-rename-files ()
  "Rename attached files for the entry at point to match its BibTeX key.
Files listed in the \"file\" field are renamed to `KEY.extension' and moved to
the appropriate directory based on `ebib-extras--extension-directories'.
The \"file\" field is updated with the new paths. The entry's key must be valid."
  (interactive)
  (ebib--execute-when
    (entries
     (let* ((field "file")
	    (key (ebib--get-key-at-point))
	    (file-list (split-string (ebib-extras-get-field field) ";")))
       (ebib-extras-check-valid-key key)
       (when file-list
	 (ebib-delete-field-contents field t)
	 (dolist (filename file-list)
	   (let ((extension (file-name-extension filename)))
	     (let ((new-filename
		    (ebib-extras--rename-and-abbreviate-file
		     (ebib-extras--extension-directories extension)
		     key extension)))
	       (cond ((file-exists-p filename)
		      (rename-file filename new-filename 'ok-if-already-exists))
		     ((file-exists-p new-filename))
		     (t (user-error "File `%s' does not exist" filename)))
	       (setq filename new-filename))
	     (ebib-set-field-value field filename key ebib--cur-db ";")))
	 (ebib--redisplay-field field)
	 (ebib--redisplay-index-item field))))
    (default
     (beep))))

(defun ebib-extras-validate-file-stem ()
  "Check that the stem of each attached file equals the entry's unique key.
If any file's base name (without extension) does not match the current
entry's key, a user error is signaled."
  (when-let* ((files (ebib-extras-get-field "file")))
    (when (catch 'tag
	    (mapc
	     (lambda (file)
	       (unless (equal
			(file-name-base file)
			(ebib--get-key-at-point))
		 (throw 'tag file)))
	     (ebib--split-files files))
	    nil)
      (user-error "Invalid file stem"))))

(defun ebib-extras--rename-and-abbreviate-file (directory key &optional extension)
  "Construct a new file path in DIRECTORY, named after KEY, with EXTENSION.
The DIRECTORY path is abbreviated using `abbreviate-file-name'. If EXTENSION is
non-nil, it's appended to KEY. This function primarily constructs the target
path for renaming/moving files."
  (file-name-concat
   (abbreviate-file-name
    directory)
   (if extension
       (file-name-with-extension key extension)
     key)))

(defun ebib-extras-key-is-valid-p (&optional key)
  "Return t if KEY (or current entry's key) is a valid BibTeX key.
Validity is checked against `ebib-extras-valid-key-regexp'."
  (let ((key (or key (ebib--get-key-at-point))))
    (string-match ebib-extras-valid-key-regexp key)))

(defun ebib-extras-check-valid-key (&optional key)
  "Check that KEY (or current entry's key) is a valid BibTeX entry key.
If the key is not valid according to `ebib-extras-key-is-valid-p', signal a
user error prompting to regenerate it."
  (let ((key (or key (ebib--get-key-at-point))))
    (unless (ebib-extras-key-is-valid-p key)
      (user-error "Ebib entry has an invalid key; please regenerate it"))))

(defun ebib-extras--extension-directories (extension)
  "Return the library directory associated with file EXTENSION.
Known extensions are \"pdf\", \"html\", and others listed in
`ebib-extras-valid-file-extensions' (which map to `paths-dir-media-library').
Signals an error for unknown extensions."
  (cond ((string= extension "pdf")
	 paths-dir-pdf-library)
	((string= extension "html")
	 paths-dir-html-library)
	((member extension ebib-extras-valid-file-extensions)
	 paths-dir-media-library)
	(t
	 (user-error "Invalid file extension"))))

;;;;; process invalid files
;; These functions are meant to be run sporadically, to clean up the library.
;; First call `ebib-extras-list-invalid-files', wait for a few minutes for the
;; processing to finish, then call `ebib-extras-rename-next-invalid-file' to
;; rename the next invalid file in line. You may have to do some manual
;; processing. Repeat until all files are renamed.

(defvar ebib-extras-invalid-files nil
  "List of invalidly named files found by `ebib-extras-list-invalid-files'.
An invalid file is one whose base name does not correspond to a known BibTeX
key.")

(autoload 'tlon-bibliography-lookup "tlon-bib")
(defun ebib-extras-file-is-valid-p (file)
  "Check if FILE has a valid base name (slug) corresponding to a BibTeX key.
Validity is determined by looking up the file's base name as a key using
`tlon-bibliography-lookup'."
  (let ((slug (file-name-base file)))
    (stringp (tlon-bibliography-lookup "=key=" slug))))

(defun ebib-extras-list-invalid-files (&optional dirs)
  "Populate `ebib-extras-invalid-files' with files from DIRS with invalid names.
Invalid names are those whose base name isn't a known BibTeX key. If DIRS is
nil, searches `paths-dir-pdf-library' and potentially others commented out in
the original code. This can be slow. DIRS should be a list of directory paths."
  (interactive "P") ; Allow prefix for optional DIRS, or prompt.
  (let* ((dirs (or (if (listp dirs) dirs nil) ; Handle prefix arg for DIRS
                   (list
                    paths-dir-html-library
                    paths-dir-pdf-library
                    paths-dir-media-library)))
	 (invalid-files (seq-filter
			 (lambda (file)
			   (message "Processing %s" file)
			   (null (ebib-extras-file-is-valid-p file)))
			 (apply #'append
				(mapcar
				 (lambda (dir)
				   (directory-files dir t directory-files-no-dot-files-regexp))
				 dirs)))))
    (setq ebib-extras-invalid-files invalid-files)))

(defun ebib-extras-rename-invalid-file (file)
  "Rename an invalid FILE to match its corresponding BibTeX key.
Removes FILE from `ebib-extras-invalid-files'. Looks up the BibTeX key
associated with FILE, opens the entry in Ebib, and calls
`ebib-extras-rename-files' if the entry is not cross-referenced."
  (setq ebib-extras-invalid-files (delete file ebib-extras-invalid-files))
  (when-let* ((key (tlon-bibliography-lookup "file" (file-name-nondirectory file) "=key=" t))
	      (bibfile (ebib-extras-get-file-of-key key)))
    (ebib bibfile key)
    (ebib-edit-entry)
    (unless (ebib-extras-get-field "crossref")
      (ebib-extras-rename-files))))

(defun ebib-extras-rename-next-invalid-file ()
  "Rename the next file from `ebib-extras-invalid-files'.
This function processes the *first* file in `ebib-extras-invalid-files' by
calling `ebib-extras-rename-invalid-file'. The processed file is removed from
the list. Call this repeatedly to process all invalid files one by one."
  (interactive)
  (when ebib-extras-invalid-files
    (ebib-extras-rename-invalid-file (car ebib-extras-invalid-files))))

;;;;; process entries

(declare-function tlon-deepl-translate-abstract "tlon-deepl")
(defun ebib-extras-process-entry ()
  "Process the BibTeX entry at point comprehensively.
This function performs several actions:

1. Generates a BibTeX key if the current one is invalid or if prompted.

2. Sets or confirms the entry's language using
`ebib-extras-get-or-set-language'.

3. Attaches relevant files using `ebib-extras-attach-files'.

It assumes that fields like `type', `author', `date', and `title' are
correctly set."
  (interactive)
  (let (entry-key)
    (when (or (not (ebib-extras-key-is-valid-p))
	      (y-or-n-p "Regenerate key? "))
      (ebib-generate-autokey))
    (setq entry-key (ebib--get-key-at-point))
    (ebib-extras-get-or-set-language)
    (ebib-extras-attach-files entry-key)))

(defun ebib-extras-set-abstract (&optional key)
  "Set the abstract for KEY if it's currently empty.
If KEY is nil, use the current entry's key.

Attempt to fetch the abstract using `tlon-get-abstract-with-or-without-ai'."
  (interactive)
  (ebib-extras-open-key key)
  (unless (if key
	      (bibtex-extras-get-entry-as-string key "abstract")
	    (ebib-extras-get-field "abstract"))
    (tlon-get-abstract-with-or-without-ai)))

;;;;; attach downloads

(declare-function files-extras-newest-file "files-extras")
(declare-function bibtex-extras-get-key "bibex-extras")
(declare-function tlon-get-abstract-with-or-without-ai "tlon-ai")
;;;###autoload
(defun ebib-extras-attach-file (&optional file key open)
  "Attach FILE to the BibTeX entry specified by KEY.
If FILE is a string, attach that file. If FILE is the symbol `most-recent' (or
any symbol), attach the newest file from `paths-dir-downloads'. If FILE is nil,
prompt the user for a file to attach.
KEY defaults to the key of the entry at point. The key must be valid.
The file is renamed to match KEY, moved to the appropriate library directory,
and its path is added to the entry's \"file\" field.
If OPEN is non-nil and the attached file is a PDF, it is opened.
Interactive use:
  - With no prefix: prompts for file, uses current entry's key.
  - With \\[universal-argument] prefix: FILE is `most-recent', uses current
    entry's key.
  - To specify KEY or OPEN programmatically, call non-interactively.
;; TODO: Handle abstract setting, PDF metadata, and OCR for the target KEY,
;; especially in asynchronous contexts or when KEY is not the current entry."
  (interactive (list (if current-prefix-arg 'most-recent nil) nil nil))
  (let ((target-key (or key (ebib--get-key-at-point))))
    (unless target-key
      (user-error "No BibTeX key provided or found at point for attaching file"))
    (ebib-extras-check-valid-key target-key)
    (let* ((file-to-attach
	    (cond ((not file) ; Prompting interactively
		   (let ((initial-folder
			  (completing-read "Select folder: "
					   (list
					    paths-dir-downloads
					    paths-dir-pdf-library
					    paths-dir-html-library
					    paths-dir-media-library))))
		     (read-file-name
		      "File to attach: "
		      (if (catch 'found ; Default to key-based file if it exists
			    (dolist (extension ebib-extras-valid-file-extensions)
			      (when (file-regular-p (file-name-concat
						     initial-folder
						     (file-name-with-extension target-key extension)))
				(throw 'found extension))))
			  (file-name-concat initial-folder target-key)
			initial-folder))))
		  ((and file (symbolp file)) ; `most-recent' or any symbol
		   (files-extras-newest-file paths-dir-downloads))
		  (t file))) ; FILE is a string
	   (extension (file-name-extension file-to-attach))
	   (destination-folder (ebib-extras--extension-directories extension))
	   (file-name (ebib-extras--rename-and-abbreviate-file
		       destination-folder target-key extension)))
      (when (or (not (file-regular-p file-name))
		(y-or-n-p (format "File %s exists. Overwrite? "
				  (file-name-nondirectory file-name))))
	(rename-file file-to-attach file-name t))
      (ebib-extras--update-file-field-contents target-key file-name)
      (ebib-extras-set-abstract target-key)
      (when (and (string= (file-name-extension file-name) "pdf") open)
	(ebib-extras-set-pdf-metadata target-key)
	(ebib-extras-ocr-pdf target-key)
	(ebib-extras-open-pdf-file)))))

(defun ebib-extras-attach-most-recent-file ()
  "Attach the most recent file from `paths-dir-downloads' to the current entry.
This is a convenience wrapper around `ebib-extras-attach-file'."
  (interactive)
  (ebib-extras-attach-file 'most-recent))

;;;;; File attachment

(declare-function eww-extras-url-to-file "eww-extras")
(defun ebib-extras-url-to-file-attach (type &optional key)
  "Generate a file of TYPE (e.g., \"pdf\", \"html\") from the URL of entry KEY.
The generated file is then attached to the entry. If KEY is nil, uses the entry
at point. Uses `eww-extras-url-to-file' for generation and
`ebib-extras-attach-file-to-entry' as a callback for attachment.
TYPE is a string. KEY is an optional BibTeX key string."
  (let ((target-key (or key (ebib--get-key-at-point))))
    (when-let* ((url (ebib-extras-get-field "url" target-key)))
      (eww-extras-url-to-file type url
                              (lambda (file &optional _status)
                                (ebib-extras-attach-file-to-entry file target-key))))))

(defun ebib-extras-url-to-pdf-attach (&optional key)
  "Generate a PDF from the URL of entry KEY and attach it.
If KEY is nil, uses the entry at point. This is a wrapper around
`ebib-extras-url-to-file-attach' with TYPE \"pdf\".
KEY is an optional BibTeX key string, passed interactively as nil."
  (interactive (list nil))
  (ebib-extras-url-to-file-attach "pdf" key))

(defun ebib-extras-url-to-html-attach (&optional key)
  "Generate an HTML file from the URL of entry KEY and attach it.
If KEY is nil, uses the entry at point. This is a wrapper around
`ebib-extras-url-to-file-attach' with TYPE \"html\".
KEY is an optional BibTeX key string, passed interactively as nil."
  (interactive (list nil))
  (ebib-extras-url-to-file-attach "html" key))

(defvar eww-extras-download-subtitles) ; Should be declared if special
(defun ebib-extras-url-to-srt-attach (&optional key)
  "Download SRT subtitles for the URL of the entry with KEY.
If KEY is nil, uses the entry at point. Downloads to `paths-dir-downloads' using
a shell command formatted by `eww-extras-download-subtitles'.
Attachment of the downloaded SRT file must be done manually.
KEY is an optional BibTeX key string, passed interactively as nil."
  (interactive (list nil))
  (let ((target-key (or key (ebib--get-key-at-point))))
    (when-let* ((url (ebib-extras-get-field "url" target-key))
		(default-directory paths-dir-downloads))
      (message "Downloading subtitles for entry %s (URL: %s).... Attach manually when ready." target-key url)
      ;; Assuming eww-extras-download-subtitles is a format string taking the URL
      (shell-command (format eww-extras-download-subtitles url)))))

(defun ebib-extras-book-attach (&optional key)
  "Attempt to download and attach a PDF for the book-type entry with KEY.
If KEY is nil, uses the entry at point. It uses the entry's ISBN or title (if a
book-like type) to search and download via `annas-archive-download'. A hook is
added to `annas-archive-post-download-hook' to automatically attach the
downloaded file using `ebib-extras-attach-file' if the download was internal;
otherwise, it messages to attach manually.
KEY is an optional BibTeX key string, passed interactively as nil."
  (interactive (list nil))
  (let ((target-key (or key (ebib--get-key-at-point))))
    ;; Use target-key when getting fields
    (when-let* ((id (or (ebib-extras-get-isbn target-key)
			(let ((type (ebib-extras-get-field "=type=" target-key)))
			  (and (member type ebib-extras-book-like-entry-types)
			       (ebib-extras-get-field "title" target-key))))))
      ;; Use a hook to capture the key and call attach-file when download finishes.
      ;; This hook (`annas-archive-post-download-hook') receives (url path) if
      ;; downloaded via eww, or just (url) if downloaded externally.
      ;; Use cl-labels to define a function that can refer to itself for removal.
      (cl-labels ((attach-and-remove-hook (url &optional path)
                   (if path
                       (progn
                         (message "Annas Archive download finished for %s, attaching file %s" target-key path)
                         (ebib-extras-attach-file path target-key))
                     (message "Annas Archive download initiated externally for %s (URL: %s). Attach file manually." target-key url))
                   ;; Remove this specific function instance from the hook after it runs.
                   (remove-hook 'annas-archive-post-download-hook #'attach-and-remove-hook)))
        ;; Use the actual hook name from annas-archive.el
        ;; Add hook globally (nil) instead of locally (t), append=nil (add to front)
        (add-hook 'annas-archive-post-download-hook #'attach-and-remove-hook nil nil)
        (annas-archive-download id)))))

(defun ebib-extras-doi-attach (&optional key)
  "Attempt to download and attach a PDF for the entry with KEY using its DOI.
If KEY is nil, uses the entry at point. It retrieves the DOI, then uses
`scihub-download' with `ebib-extras-attach-file-to-entry' as a callback for
attachment.
KEY is an optional BibTeX key string, passed interactively as nil."
  (interactive (list nil))
  (let ((target-key (or key (ebib--get-key-at-point))))
    (when-let* ((doi (ebib-extras-get-field "doi" target-key)))
      (scihub-download doi
                       (lambda (file &optional _status)
                         (ebib-extras-attach-file-to-entry file target-key))))))

(defun ebib-extras-attach-file-to-entry (&optional file key)
  "Attach FILE to the BibTeX entry with KEY.
This function is primarily intended as a callback for asynchronous downloaders.
It calls `ebib-extras-attach-file' and messages success.
FILE is the path to the file. KEY is the BibTeX key string."
  ;; Do not switch buffers here, pass the key directly to attach-file
  (ebib-extras-attach-file file key)
  (message "Attached `%s' to %s" file key))

(declare-function annas-archive-download "annas-archive")
(defun ebib-extras-attach-files (&optional key)
  "Attach files appropriate for the entry with KEY (Do What I Mean).
If KEY is nil, uses the entry at point. If a file is already attached (i.e.,
the \"file\" field is non-empty), it attempts to set the abstract. Otherwise, it
tries to attach files based on available identifiers and entry type:
- DOI: Uses `ebib-extras-doi-attach'.
- ISBN or book-like type: Uses `ebib-extras-book-attach'.
- Video URL: Uses `ebib-extras-url-to-srt-attach' (for subtitles).
- Online type with URL: Uses `ebib-extras-url-to-pdf-attach' and
  `ebib-extras-url-to-html-attach'.
KEY is an optional BibTeX key string, passed interactively as nil.
;; TODO: Ensure `ebib-extras-set-abstract' works correctly for TARGET-KEY if
;; it's not the currently displayed Ebib entry."
  (interactive (list nil))
  (let ((target-key (or key (ebib--get-key-at-point))))
    (cl-destructuring-bind (doi url isbn type file)
        (mapcar (lambda (field) (ebib-extras-get-field field target-key))
                '("doi" "url" "isbn" "=type=" "file"))
      (if file
          (ebib-extras-set-abstract) ; May need adjustment for target-key context
        (cond (doi (ebib-extras-doi-attach target-key))
              ((or isbn (member type ebib-extras-book-like-entry-types))
               (ebib-extras-book-attach target-key))
              ((and url (cl-some (lambda (regexp) (string-match regexp url))
                                 ebib-extras-video-websites))
               (ebib-extras-url-to-srt-attach target-key))
              ((and url (string-match "online" type)) ; Assuming "online" is a type
               (ebib-extras-url-to-pdf-attach target-key)
               (ebib-extras-url-to-html-attach target-key)))))))

;;;;; ?

(autoload 'files-extras-ocr-pdf "files-extras")
(defvar tlon-languages-properties) ; Should be declared if special
(defun ebib-extras-ocr-pdf (&optional force)
  "Perform OCR on the PDF file associated with the current Ebib entry.
This function calls `files-extras-ocr-pdf'. If FORCE is non-nil (e.g., when
called with a prefix argument), OCR is forced even if text is already present."
  (interactive "P")
  (files-extras-ocr-pdf force))

(declare-function tlon-lookup "tlon-core")
(declare-function tlon-lookup-all "tlon-core")
(declare-function bibtex-set-field "bibtex-extras")
(defun ebib-extras-get-or-set-language ()
  "Return the language of the current entry, prompting to set it if needed.
Determines the appropriate field access functions based on `major-mode'
\\=(`ebib-entry-mode' or `bibtex-mode'). If the \"langid\" field is empty or
contains an invalid language (checked against `tlon-languages-properties'),
prompts the user to select a language using `completing-read' and sets the
field. Returns the (potentially newly set) language."
  (cl-destructuring-bind (get-field set-field)
      (pcase major-mode
        ('ebib-entry-mode '(ebib-extras-get-field ebib-extras-set-field))
        ('bibtex-mode '(bibtex-extras-get-field bibtex-set-field)))
    (let* ((get-lang (lambda () (funcall get-field "langid")))
           (set-lang (lambda (lang) (funcall set-field "langid" lang)))
           (lang (funcall get-lang))
           (valid-lang (tlon-lookup tlon-languages-properties :standard :name lang)))
      (or valid-lang
          (funcall set-lang
                   (completing-read "Select language: "
                                    (tlon-lookup-all tlon-languages-properties :standard)
                                    nil t "english"))))))

(defconst ebib-extras-library-genesis
  '("Library Genesis"
    "https://libgen.li/index.php?req=%s" "&columns%5B%5D=t&columns%5B%5D=a&columns%5B%5D=s&columns%5B%5D=y&columns%5B%5D=p&columns%5B%5D=i&objects%5B%5D=f&objects%5B%5D=e&objects%5B%5D=s&objects%5B%5D=a&objects%5B%5D=p&objects%5B%5D=w&topics%5B%5D=l&res=25&filesuns=all"))

(defconst ebib-extras-amazon
  '("Amazon"
    "https://smile.amazon.com/s?k="
    "&i=stripbooks"))

(defconst ebib-extras-worldcat
  '("Worldcat"
    "https://www.worldcat.org/search?q="
    "&itemType=book&limit=50&offset=1"))

(defconst ebib-extras-internet-archive
  '("Internet Archive"
    "https://archive.org/search.php?query="
    ""))

(defconst ebib-extras-university-of-toronto
  '("University of Toronto"
    "https://librarysearch.library.utoronto.ca/discovery/search?query=any,contains,"
    "&tab=Everything&search_scope=UTL_AND_CI&vid=01UTORONTO_INST:UTORONTO&offset=0"))

(defconst ebib-extras-university-of-california-berkeley
  '("University of California, Berkeley"
    "https://search.library.berkeley.edu/discovery/search?query=any,contains,"
    "&tab=Default_UCLibrarySearch&search_scope=DN_and_CI&vid=01UCS_BER:UCB&offset=0"))

(defconst ebib-extras-hathitrust
  '("HathiTrust"
    "https://babel.hathitrust.org/cgi/ls?q1="
    "&field1=ocr;a=srchls;lmt=ft;sz=100"))

(defconst ebib-extras-connected-papers
  '("Connected Papers"
    "https://www.connectedpapers.com/search?q="
    ""))

(defconst ebib-extras-google-scholar
  '("Google Scholar"
    "https://scholar.google.com/scholar?q="
    ""))

(defconst ebib-extras-wikipedia
  '("Google Scholar"
    "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&profile=default&search="
    "&fulltext=Search"))

(defconst ebib-extras-goodreads
  '("Goodreads"
    "https://www.goodreads.com/search?q="
    ""))

(defconst ebib-extras-audible
  '("Audible"
    "https://www.audible.com/search?keywords="
    ""))

(defconst ebib-extras-audiobookbay
  '("Audiobook Bay"
    "https://theaudiobookbay.se/?s="
    "&tt=1"))

(defconst ebib-extras-imdb
  '("IMDB"
    "https://www.imdb.com/find/?q="
    ""))

(defconst ebib-extras-letterboxd
  '("Letterboxd"
    "https://letterboxd.com/search/films/"
    ""))

(defconst ebib-extras-metacritic
  '("Metacritic"
    "https://www.metacritic.com/search/all/"
    "/results"))

(defconst ebib-extras-search-book-functions
  '(ebib-extras-search-goodreads
    ebib-extras-search-hathitrust
    ebib-extras-search-university-of-california-berkeley
    ebib-extras-search-internet-archive
    ebib-extras-search-library-genesis
    ebib-extras-search-amazon)
  "List of functions that search for books.")

(defconst ebib-extras-download-book-functions
  '(ebib-extras-search-goodreads
    ebib-extras-search-hathitrust
    ebib-extras-search-university-of-california-berkeley
    ebib-extras-search-internet-archive
    ebib-extras-search-library-genesis
    ;; ebib-extras-search-amazon
    ebib-extras-search-audible
    ebib-extras-search-audiobookbay
    )
  "List of functions that download books.")

(defconst ebib-extras-search-article-functions
  '(ebib-extras-search-connected-papers
    ebib-extras-search-google-scholar)
  "List of functions that search for articles.")

(defconst ebib-extras-download-article-functions
  '(ebib-extras-search-article-functions)
  "List of functions that download articles.")

(defconst ebib-extras-search-film-functions
  '(ebib-extras-search-imdb
    ebib-extras-search-letterboxd
    ebib-extras-search-metacritic)
  "List of functions that search for films.")

(defconst ebib-extras-download-film-functions
  ebib-extras-search-film-functions
  "List of functions that search for films.")

(defconst ebib-extras-search-engines '()
  "List of search engine symbols.
Used by the `ebib-extras-generate-search-commands' macro.")

(dolist (engine (delete-dups (append ebib-extras-search-book-functions
				     ebib-extras-search-article-functions
				     ebib-extras-search-film-functions
				     ebib-extras-download-book-functions)))
  (let* ((engine-name (symbol-name engine))
	 (engine-symbol (intern (replace-regexp-in-string "ebib-extras-search-" "" engine-name))))

    (add-to-list 'ebib-extras-search-engines engine-symbol)))

;;;;; Search functions

(defun ebib-extras-generate-search-commands ()
  "Generate search commands for search engines in `ebib-extras-search-engines'."
  (mapc
   (lambda (engine)
     (let* ((name (capitalize (replace-regexp-in-string "-" " " (symbol-name engine))))
	    (function-name (intern (concat "ebib-extras-search-" (symbol-name engine))))
	    (docstring (format "Run a search on %s." name))
	    (query-prompt `(,(format "sSearch %s: " name)))
	    (search-engine (intern (concat "ebib-extras-" (symbol-name engine)))))
       (fset function-name
	     `(lambda (&optional query)
		,docstring
		(interactive ,query-prompt)
		(ebib-extras-search ,search-engine query)))))
   ebib-extras-search-engines))

(ebib-extras-generate-search-commands)

(defun ebib-extras-search (search-engine query)
  "Search for QUERY with SEARCH-ENGINE."
  (let* ((prefix (nth 1 search-engine))
	 (suffix (nth 2 search-engine))
	 (query (url-hexify-string query)))
    (browse-url (concat prefix query suffix))))

(declare-function mullvad-connect-to-website "mullvad")
(defun ebib-extras--search-multi (query functions)
  "Search for QUERY with each function in FUNCTIONS."
  (dolist (fun functions)
    (when (and ebib-extras-download-use-vpn
	       (string-match "-download-" (symbol-name fun)))
      (mullvad-connect-to-website "Library Genesis" 5))
    (funcall fun query)))

(defun ebib-extras-search-dwim ()
  "Run a search on the current entry based on the field at point.
If field at point is `title', run a search with its value, else
use identifier.

The list of websites for the search query is defined by the
variable `ebib-extras-search-book'."
  (interactive)
  (ebib-extras--search-or-download 'search))

(defun ebib-extras-get-supertype ()
  "Return the supertype of the current entry."
  (let* ((type (ebib-extras-get-field "=type="))
	 (supertype (cond
		     ((member type ebib-extras-book-like-entry-types)
		      "book")
		     ((member type ebib-extras-article-like-entry-types)
		      "article")
		     ((member type ebib-extras-film-like-entry-types)
		      "film"))))
    supertype))

(defun ebib-extras-get-query-field ()
  "Return the field based on whose value the search query will be run."
  (let ((query (if (string= (ebib--current-field) "title")
		   "title"
		 "identifier")))
    query))

(defun ebib-extras--search-or-download (action)
  "Perform ACTION on the current entry based on the field at point.
If point is on field `title', perform ACTION based on its value.
Else perform ACTION based on the value of its identifier (`doi'
or `isbn').

Action can be `search' or `download'."
  (let* ((supertype (ebib-extras-get-supertype))
	 (query (ebib-extras-get-query-field))
	 (fun (intern (format "ebib-extras-%s-%s-by-%s" action supertype query))))
    (funcall fun)))

(defun ebib-extras-search-book (query)
  "Search for QUERY with the relevant book search functions.
The list of book search functions is specified by
`ebib-extras-search-book-functions'."
  (ebib-extras--search-multi query ebib-extras-search-book-functions))

;; getting the value of the book title field is slightly more involved
;; because two different fields may contain this value. so we create a
;; special function
(defun ebib-extras--get-title-at-point ()
  "Get the title of the entry at point."
  (let ((title (catch 'found
		 (dolist (field '("title" "booktitle"))
		   (when-let* ((title
			       (ebib-extras-get-field field)))
		     (throw 'found title))))))
    title))

(defun ebib-extras-search-book-by-title ()
  "Search for the book's title with the relevant book search functions.
The list of book search functions is specified by
`ebib-extras-search-book-functions'."
  (interactive)
  (ebib--execute-when
    (entries
     (if-let* ((title (ebib-extras--get-title-at-point)))
	 (ebib-extras-search-book title)
       (user-error "Title field is empty")))
    (default
     (beep))))

(defun ebib-extras-search-article (query)
  "Search for QUERY with the relevant article search functions.
The list of article search functions is specified by
`ebib-extras-search-article-functions'."
  (ebib-extras--search-multi query ebib-extras-search-article-functions))

(defun ebib-extras-search-article-by-title ()
  "Search for the article's title with the relevant article search functions.
The list of article search functions is specified by
`ebib-extras-search-article-functions'."
  (interactive)
  (ebib--execute-when
    (entries
     (if-let* ((title (ebib-extras-get-field "title")))
	 (ebib-extras-search-article title)
       (user-error "Title field is empty")))
    (default
     (beep))))

(defun ebib-extras-search-film (query)
  "Search for QUERY with the relevant film search functions.
The list of film search functions is specified by
`ebib-extras-search-film-functions'."
  (ebib-extras--search-multi query ebib-extras-search-film-functions))

(defun ebib-extras-search-film-by-title ()
  "Search for the film's title with the relevant film search functions.
The list of film search functions is specified by
`ebib-extras-search-film-functions'."
  (interactive)
  (ebib--execute-when
    (entries
     (if-let* ((title (ebib-extras-get-field "title")))
	 (ebib-extras-search-film title)
       (user-error "Title field is empty")))
    (default
     (beep))))

(declare-function scihub-is-doi-p "scihub")
(defun ebib-extras-search-by-identifier ()
  "Search for a book or article by the ISBN or DOI of the entry at point."
  (interactive)
  ;; TODO: Add support for arXiv
  (let ((id (or (ebib-extras-get-isbn)
		(ebib-extras-get-field "doi")
		(read-string "Enter ISBN or DOI: "))))
    (cond ((ebib-extras-isbn-p id)
	   (ebib-extras-search-book id))
	  ((scihub-is-doi-p id)
	   (ebib-extras-search-article id))
	  (t
	   (user-error "Identifier does not appear to be an ISBN or DOI")))))

(defalias 'ebib-extras-search-book-by-identifier 'ebib-extras-search-by-identifier)
(defalias 'ebib-extras-search-article-by-identifier 'ebib-extras-search-by-identifier)
;; no film identifier; we just search for it
(defalias 'ebib-extras-search-film-by-identifier 'ebib-extras-search-film-by-title)

(defun ebib-extras-download-book (query)
  "Search for QUERY with the relevant book download functions.
The list of book download functions is specified by
`ebib-extras-download-book-functions'."
  (ebib-extras--search-multi query ebib-extras-download-book-functions))

(defun ebib-extras-download-book-by-title ()
  "Search for the book's title with the relevant book download functions.
The list of book download functions is specified by
`ebib-extras-download-book-functions'."
  (interactive)
  (ebib--execute-when
    (entries
     (if-let* ((title (ebib-extras--get-title-at-point)))
	 (ebib-extras-download-book title)
       (user-error "Title field is empty")))
    (default
     (beep))))

(defun ebib-extras-download-article (query)
  "Search for QUERY with the relevant article download functions.
The list of article download functions is specified by
`ebib-extras-download-article-functions'."
  (ebib-extras--search-multi query ebib-extras-download-article-functions))

(defun ebib-extras-download-article-by-title ()
  "Search for the article's title with the relevant article download functions.
The list of article download functions is specified by
`ebib-extras-download-article-functions'."
  (interactive)
  (ebib--execute-when
    (entries
     (if-let* ((title (ebib-extras-get-field "title")))
	 (ebib-extras-download-article title)
       (user-error "Title field is empty")))
    (default
     (beep))))

(declare-function scihub-download "scihub")
(declare-function eww-extras-url-to-pdf "eww-extras")
(defun ebib-extras-download-pdf ()
  "Download and attach a PDF of the work at point based on its DOI, URL or ISBN."
  (interactive)
  (let ((get-field (pcase major-mode
		     ('ebib-entry-mode #'ebib-extras-get-field)
		     ('bibtex-mode #'bibtex-extras-get-field))))
    (or (when-let* ((doi (funcall get-field "doi")))
	  (scihub-download doi))
	(when-let* ((url (funcall get-field "url")))
	  (eww-extras-url-to-pdf url))
	(when-let* ((isbn (ebib-extras-get-isbn)))
	  (ebib-extras-download-book isbn)))))

;; all we want is to search for a film, and there is no film
;; identifier, so we map all functions to
;; `ebib-extras-search-film-by-title'
(defalias 'ebib-extras-download-film-by-title 'ebib-extras-search-film-by-title)
(defalias 'ebib-extras-download-film-by-identifier 'ebib-extras-search-film-by-title)

(defun ebib-extras-download-video (id)
  "Download video with id ID using `yt-dlp'."
  (unless (executable-find "yt-dlp")
    (user-error "Please install `yt-dlp' (e.g. 'brew install yt-dlp')"))
  (ebib--execute-when
    (entries
     (let* ((key (ebib--get-key-at-point))
	    (file-name
	     (ebib-extras--rename-and-abbreviate-file
	      paths-dir-media-library key "webm")))
       (async-shell-command (format "yt-dlp --output '%s' '%s'" file-name id))
       (message (format "Downloading video from '%s'" (substring-no-properties id)))
       (ebib-extras--update-file-field-contents key file-name)))
    (default
     (beep))))

(defun ebib-extras-sentence-case ()
  "Convert the current field to sentence case."
  (interactive)
  (ebib--execute-when
    (entries
     (let* ((field (ebib--current-field))
            (value (ebib-extras-get-field field))
            (words (split-string value)))
       (setq words
             (mapcar (lambda (word)
                       (if (string-match "\\$\\|{\\|}\\|\\\\" word)
                           word
                         (downcase word)))
                     words))
       (setf (car words) (capitalize (car words)))
       (setq value (mapconcat 'identity words " "))
       (ebib-set-field-value field value (ebib--get-key-at-point) ebib--cur-db 'overwrite 'unbraced)
       (ebib--store-multiline-text (current-buffer))
       (ebib--redisplay-field field)
       (ebib--redisplay-index-item field)
       (ebib-save-current-database nil)))
    (default (beep))))

(declare-function bibtex-extras-get-entry-as-string "bibtex-extras")
(defun ebib-extras-get-or-open-entry ()
  "Get or open the BibTeX entry, depending on how the function was called.
If called interactively, open the entry. Otherwise, return it as a string."
  (interactive)
  (when-let* ((file (ebib-db-get-filename ebib--cur-db))
	     (key (ebib--get-key-at-point))
	     (fun (if (called-interactively-p 'any) #'find-file #'find-file-noselect)))
    (with-current-buffer (funcall fun file)
      (bibtex-search-entry key)
      (unless (called-interactively-p 'any) (bibtex-extras-get-entry-as-string)))))

;;;###autoload
(defun ebib-extras-get-file-of-key (key)
  "Return the bibliographic file in which the entry with KEY is found."
  (unless ebib--databases
    (user-error "Please launch Ebib first"))
  (let ((result (catch 'found
		  ;; taken from `ebib--find-and-set-key'
		  (mapc (lambda (file)
			  (let ((db (ebib--get-db-from-filename file)))
			    (if (and db (member key (ebib-db-list-keys db)))
				(throw 'found db))))
			paths-files-bibliography-all)
		  nil)))
    (alist-get 'filename result)))

;;;###autoload
(defun ebib-extras-open-key (key)
  "Open the entry for KEY in Ebib."
  (when-let* ((file (ebib-extras-get-file-of-key key)))
    (ebib file key)
    (sleep-for 0.01)
    (ebib-edit-entry)))

(autoload 'simple-extras-get-next-element "simple-extras")
;;;###autoload
(defun ebib-extras-sort (&optional state)
  "Sort Ebib index buffer by STATE.
If STATE is nil, toggle between the relevant states."
  (interactive)
  (ebib--execute-when
    (entries
     (let* ((order 'ascend)
	    (state (or state ebib-extras-sort-state))
	    (next (simple-extras-get-next-element state ebib-extras-sort-states)))
       (when (string= state "Timestamp")
	 (setq order 'descend))
       (ebib--index-sort (symbol-name state) order)
       (goto-char (point-min))
       (message (format "Sorting by %s" state))
       (setq ebib-extras-sort-state next)))
    (default
     (beep))))

(defun ebib-extras-end-of-index-buffer ()
  "Move to the end of the index buffer."
  (interactive)
  (when (derived-mode-p 'ebib-index-mode)
    (goto-char (point-max))
    (forward-line -1)))

(declare-function zotra-extras-set-bibfile "zotra-extras")
(defun ebib-extras-duplicate-entry ()
  "Duplicate the current entry."
  (interactive)
  (ebib--execute-when
    (entries
     (let* ((key (ebib--get-key-at-point))
	    (new-key (if ebib-uniquify-keys
			 (ebib-db-uniquify-key (ebib--get-key-at-point) ebib--cur-db)
		       key))
	    (file (progn
		    (zotra-extras-set-bibfile)))
	    entry)
       (with-temp-buffer
	 (ebib--format-entry key ebib--cur-db)
	 (goto-char (point-min))
	 (while (re-search-forward (concat "\\(^@[[:alpha:]]*?{\\)" (regexp-quote key) ",") nil t)
	   (replace-match (concat "\\1" new-key ",")))
	 (setq entry (buffer-substring-no-properties (point-min) (point-max))))
       (with-current-buffer (find-file-noselect file)
	 (widen)
	 (goto-char (point-max))
	 (insert entry)
	 (bibtex-set-field "timestamp" (format-time-string ebib-timestamp-format nil "GMT"))
	 (save-buffer)
	 (ebib-extras-reload-database-no-confirm ebib--cur-db)
	 (ebib file new-key)
	 (ebib--index-sort "Timestamp" 'descend)
	 (goto-char (point-min))
	 (ebib-extras-open-key new-key))))
    (default
     (beep))))

(declare-function citar-open-notes "citar")
(defun ebib-extras-citar-open-notes ()
  "Open note for the entry at point using `citar-open-notes'.
This command replaces the native `ebib-popup-note'. The
replacement ensures that, when a note doesn't already exist, it
is created following the same schema as notes created with
`citar'."
  (interactive)
  (ebib--execute-when
    (entries
     (when-let* ((citekey (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)))
       (citar-open-notes (list citekey))))
    (default
     (beep))))

;;;;; Saving and reloading databases

;; `ebib-extras-auto-save-databases' saves a database to its file when it
;; detects that the db has been modified. `ebib-extras-auto-reload-databases'
;; reloads a database from its file detects that the file has been modified.

;;;;;; saving databases

(defun ebib-extras-auto-save-databases ()
  "Check if any Ebib database has been modified and save it to its file if so."
  (dolist (db ebib--databases)
    (when (ebib-db-modified-p db)
      (ebib--save-database db '(16))))
  (run-with-timer 1 nil #'ebib-extras-auto-save-databases))

(ebib-extras-auto-save-databases)

;;;;;; reloading databases

(defvar ebib-extras-file-notify-descriptors nil
  "List of file-notify descriptors for auto-reloading databases.
Elements in this list are used to remove file-notify watches.")

(autoload 'file-notify-add-watch "filenotify")
;;;###autoload
(defun ebib-extras-auto-reload-database (nth)
  "Monitor the NTH db file for modifications and reload its Ebib db when so."
  (let* ((db (nth nth ebib--databases))
	 (db-file (ebib-db-get-filename db)))
    (add-to-list 'ebib-extras-file-notify-descriptors
		 (file-notify-add-watch
		  db-file
		  '(change)
		  (lambda (event)
		    (ebib-extras--auto-reload-callback event nth db db-file))))))

(defun ebib-extras--auto-reload-callback (_event _nth db db-file)
  "Handle file change notifications for Ebib databases.
This function is called from the callback in
`ebib-extras-auto-reload-database'.  It checks if a reload is necessary based on
a 5-second cooldown. _EVENT is the file notification event. _NTH is the database
index. DB is the ebib database object. DB-FILE is the path to the database
file."
  (let ((now (current-time))
        (last-reload (gethash db-file ebib-extras-last-reload-times)))
    (when (or (not last-reload)
              (> (time-to-seconds (time-subtract now last-reload)) 5))
      (puthash db-file now ebib-extras-last-reload-times)
      (ebib-extras-reload-database-no-confirm db))))

(declare-function file-notify-rm-watch "filenotify")
(defun ebib-extras-remove-file-notify-watchers ()
  "Remove all file-notify watchers for Ebib databases."
  (when ebib-extras-file-notify-descriptors
    (dolist (descriptor ebib-extras-file-notify-descriptors)
      (file-notify-rm-watch descriptor))
    (setq ebib-extras-file-notify-descriptors nil)))

;;;###autoload
(defun ebib-extras-auto-reload-databases ()
  "Monitor each db file for modifications and reload its Ebib db when so."
  (ebib-extras-remove-file-notify-watchers)
  (dotimes (n (length ebib--databases))
    (ebib-extras-auto-reload-database n)))

(defun ebib-extras-no-db-modified-p ()
  "Return t iff no databases are modified."
  (null (cl-some #'ebib-db-modified-p ebib--databases)))

;;;###autoload
(defun ebib-extras-reload-all-databases ()
  "Reload all databases, if none are modified."
  (unless ebib--initialized
    (ebib-init)
    (setq ebib--needs-update t)
    (ebib-check-notes-config))
  (when (ebib-extras-no-db-modified-p)
    (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
    (dolist (db ebib--databases)
      (ebib--reload-database db)
      (ebib--set-modified nil db))
    (ebib--update-buffers)))

;;;;; <new section>

(defvar tlon-file-stable)
(defvar tlon-file-db)
(defconst ebib-extras-db-numbers
  `((,paths-file-personal-bibliography-new . 1)
    (,paths-file-personal-bibliography-old . 2)
    (,tlon-file-fluid . 3)
    (,tlon-file-stable . 4)
    (,tlon-file-db . 5))
  "Association list of database files and their numbers.")

;;;###autoload
(defun ebib-extras-get-db-number (file)
  "Get database number for FILE."
  (cdr (assoc file ebib-extras-db-numbers)))

(defvar ebib-extras-existing-authors nil
  "List of all authors in the current database.")

(defun ebib-extras-create-list-of-existing-authors ()
  "Create a list of all authors in the current database."
  (setq ebib-extras-existing-authors (ebib--create-author/editor-collection)))

(defun ebib-extras-check-author-exists ()
  "Check if the author of the given entry exists in the current database."
  (interactive)
  (let* ((author-to-check (ebib-extras-get-field "author")))
    (if (member author-to-check ebib-extras-existing-authors)
	(message "Author found in the database!")
      (message "Warning: Author not found in the database!"))))

(declare-function ebib-extras-search-goodreads "ebib-extras")
(declare-function ebib-extras-search-imdb "ebib-extras")
(declare-function ebib-extras-search-letterboxd "ebib-extras")
(declare-function bib-search-letterboxd "bib")
(defun ebib-extras-set-rating ()
  "Set rating of current entry.
If applicable, open external website to set rating there as well."
  (interactive)
  (let ((rating (ebib-extras-choose-rating))
	(supertype (ebib-extras-get-supertype))
	(title (ebib-extras-get-field "title"))
	(db ebib--cur-db))
    ;; TODO: open rating websites based on supertype
    (pcase supertype
      ("book" (ebib-extras-search-goodreads title))
      ("film"
       ;; IMDb – open stored URL when available, otherwise search
       (let ((url (ebib-extras-get-field "url")))
         (if (and url (string-match "imdb\\.com" url))
             (browse-url url)
           (ebib-extras-search-imdb title)))
       ;; Letterboxd – open stored slug or fetch it, store, then open
       (let ((lbx-slug (ebib-extras-get-field "letterboxd")))
         (if (and lbx-slug (not (string-empty-p lbx-slug)))
             (browse-url (format bib-letterboxd-url lbx-slug))
           (let ((slug (bib-search-letterboxd title)))
             (when slug
               (ebib-set-field-value "letterboxd" slug (ebib--get-key-at-point)
                                     ebib--cur-db 'overwrite)
               (ebib-extras-update-entry-buffer ebib--cur-db)
               (browse-url (format bib-letterboxd-url slug))))))))
    (ebib-set-field-value "rating" rating (ebib--get-key-at-point) ebib--cur-db 'overwrite)
    (ebib-extras-update-entry-buffer db)))

;; TODO: find way to do this without moving point
(defun ebib-extras-update-entry-buffer (db)
  "Update the entry buffer with the current entry in DB."
  (ebib--update-entry-buffer)
  (set-buffer-modified-p nil)
  (ebib--set-modified t db t nil))

(defun ebib-extras-choose-rating ()
  "Prompt for a rating from 1 to 10 and return the choice."
  (let* ((ratings '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))
	 (rating (char-to-string (read-char-choice "Rating (1–10): " ratings))))
    (when (string= rating "0")
      (setq rating "10"))
    rating))

(defun ebib-extras-no-translation-found ()
  "Log a note indicating that no translation for this work was found."
  (interactive)
  (let ((key (ebib--get-key-at-point))
	(db ebib--cur-db))
    (when (ebib-extras-get-field "note")
      (user-error "Note field is not empty"))
    (ebib-set-field-value "note"
			  (format "No translation found on %s." (format-time-string "%Y-%m-%d"))
			  key db)
    (ebib-extras-update-entry-buffer db)))

;;;###autoload
(defun ebib-extras-get-field (field &optional key)
  "Get the value of FIELD for the entry with KEY, or entry at point if KEY is nil.
Convenience function that calls `ebib-get-field-value' with
sensible defaults and remove line breaks and empty spaces."
  (let ((target-key (or key (ebib--get-key-at-point))))
    (when-let* ((value (ebib-get-field-value field target-key ebib--cur-db t t t)))
      (replace-regexp-in-string "[\n\t ]+" " " value))))

(defun ebib-extras-get-id-or-url ()
  "Get the ID or URL of the entry at point."
  (let ((get-field (pcase major-mode
		     ('ebib-entry-mode #'ebib-extras-get-field)
		     ('bibtex-mode #' (bibtex-extras-get-field)))))
    (when-let* ((id-or-url (catch 'found
			    (dolist (field '("doi" "isbn" "url"))
			      (when-let* ((value (funcall get-field field)))
				(throw 'found value))))))
      id-or-url)))

(declare-function bib-search-isbn "bib")
(declare-function bib-search-crossref "bib")
(declare-function bib-search-imdb "bib")
(declare-function bib-translate-title-to-english "bib")
(defun ebib-extras-fetch-id-or-url ()
  "Fetch the ID or URL of the entry at point.
Fetching is done using `bib'."
  (let* ((get-field (pcase major-mode
		      ('ebib-entry-mode #'ebib-extras-get-field)
		      ('bibtex-mode #' (bibtex-extras-get-field))))
	 (title (funcall get-field "title"))
	 (author (funcall get-field "author")))
    (pcase (ebib-extras-get-supertype)
      ("book" (bib-search-isbn (format "%s %s" title author)))
      ("article" (bib-search-crossref title author))
      ("film" (bib-search-imdb
	       (bib-translate-title-to-english title)))
      (_ (funcall get-field "url")))))

(defun ebib-extras-get-or-fetch-id-or-url ()
  "Get the ID or URL of the entry at point, or fetch it if missing."
  (or (ebib-extras-get-id-or-url)
      (ebib-extras-fetch-id-or-url)))

(defun ebib-extras-browse-url-or-doi ()
  "Browse the URL or DOI of the entry at point."
  (interactive)
  (when-let* ((type (cond ((ebib-extras-get-field "url") 'url)
			 ((ebib-extras-get-field "doi") 'doi))))
    (pcase type
      ('url (ebib-browse-url))
      ('doi (ebib-browse-doi)))))

(defun ebib-extras-set-id (&optional id)
  "Add an ID to the current entry, if missing."
  (interactive)
  (let ((key (ebib--get-key-at-point))
	(db ebib--cur-db)
	(field (pcase (ebib-extras-get-supertype)
		 ("book" "isbn")
		 ("article" "doi")
		 ("film" "url")))
	(id (or id (ebib-extras-fetch-id-or-url))))
    (when (ebib-extras-get-field field)
      (user-error "ID field is not empty"))
    (ebib-set-field-value field id key db)
    (ebib-extras-update-entry-buffer db)))

(defun ebib-extras-bibtex-command (command)
  "Execute a `bibtex' COMMAND with point on the current entry."
  (when-let* ((file (ebib-db-get-filename ebib--cur-db)))
    (with-current-buffer (find-file-noselect file)
      (funcall command))))

(defun ebib-extras-move-entry (direction)
  "Move to the previous or next entry in the current database.
DIRECTION can be `prev' or `next'."
  (let ((fun (pcase direction
	       ('prev #'ebib-prev-entry)
	       ('next #'ebib-next-entry))))
    (pcase major-mode
      ('ebib-index-mode (funcall fun))
      ('ebib-entry-mode
       (ebib-extras-open-or-switch)
       (funcall fun)
       (ebib-edit-entry)
       ;; hack: we do this twice to ensure the index buffer is updated
       (ebib-extras-open-or-switch)
       (ebib-edit-entry)))))

(defun ebib-extras-next-entry ()
  "Move to the next entry in the current database."
  (interactive)
  (ebib-extras-move-entry 'next))

(defun ebib-extras-prev-entry ()
  "Move to the next entry in the current database."
  (interactive)
  (ebib-extras-move-entry 'prev))

(defun ebib-extras-set-field (field value)
  "Set the value of FIELD to VALUE for the entry at point."
  (let* ((key (ebib--get-key-at-point))
	 (init-contents (ebib-get-field-value field key ebib--cur-db 'noerror)))
    (ebib-set-field-value field value (ebib--get-key-at-point) ebib--cur-db 'overwrite (ebib-unbraced-p init-contents))
    (ebib-extras-update-entry-buffer ebib--cur-db)))

(declare-function ebib-extras-fetch-field-value "ebib-extras")
(defun ebib-extras-fetch-keywords ()
  "Fetch keywords for the entry at point and put them in the associated org file."
  (interactive)
  (ebib-extras-fetch-field-value "keywords"))

(declare-function org-extras-sort-keywords "org-extras")
(declare-function org-extras-insert-subheading "org-extras")
(declare-function org-extras-linkify-elements "org-extras")
(defun ebib-extras-export-keywords ()
  "Export keywords in entry at point to its associated note file."
  (interactive)
  (when-let* ((key (ebib--get-key-at-point))
	      (field "keywords")
	      ;; unfill field contents
	      (keywords-raw (replace-regexp-in-string
			     "\n                  "
			     " "
			     (ebib-extras-get-field field)))
	      ;; turn into bullet-separated links
	      (keywords (org-extras-linkify-elements
			 (split-string keywords-raw ", "))))
    (ebib-db-remove-field-value field key ebib--cur-db)
    (ebib--redisplay-field field)
    (ebib--redisplay-index-item field)
    (ebib--set-modified t ebib--cur-db (seq-filter (lambda (dependent)
						     (ebib-db-has-key key dependent))
						   (ebib--list-dependents ebib--cur-db)))
    (ebib-extras-citar-open-notes)
    (org-extras-insert-subheading)
    (insert (concat field "\n" keywords))
    (org-extras-sort-keywords)))

;;;;; pdf metadata

(defun ebib-extras-set-pdf-metadata (&optional key)
  "Set the metadata of the PDF associated with KEY.
If KEY is nil, use the entry at point."
  (interactive)
  (unless (executable-find "exiftool")
    (user-error "Please install `exiftool' (e.g. 'brew install exiftool'"))
  (unless (derived-mode-p 'ebib-entry-mode 'bibtex-mode)
    (user-error "Not in `ebib-entry-mode' or 'bibtex-mode'"))
  (when-let* ((get-field (pcase major-mode
			   ('ebib-entry-mode #'ebib-extras-get-field)
			   ('bibtex-mode #'bibtex-extras-get-field)))
	      (file (if key
			(let ((files (bibtex-extras-get-entry-as-string key "file" )))
			  (ebib-extras-get-file-in-string files "pdf"))
		      (ebib-extras-get-file "pdf")))
	      (author (or (funcall get-field "author")
			  (funcall get-field "editor"))))
    (let* ((file-absolute (expand-file-name file))
	   (author-list (ebib-extras-get-authors-list author))
	   (author-string (ebib-extras-unbrace
			   (ebib-extras-format-authors author-list ", " most-positive-fixnum)))
	   (title (ebib-extras-unbrace (funcall get-field  "title")))
	   (author-arg (format "-Author=\"%s\" " author-string))
	   (title-arg (format "-Title=\"%s\" " title)))
      (when (or author-arg title-arg)
	(message (shell-command-to-string (concat "exiftool -overwrite_original "
						  (when author-arg author-arg)
						  (when title-arg title-arg)
						  (shell-quote-argument file-absolute))))))))

(defun ebib-extras-get-authors-list (authors)
  "Split AUTHORS into a list of authors, reversing the first and last names.
Authors enclosed in braces are left untouched, but the braces are removed."
  (let* ((authors-split (split-string authors " and "))
	 (authors-formatted
	  (mapcar (lambda (author)
		    (cond ((string-match "{\\(.*\\)}" author)
			   (match-string 1 author))
			  ((string-match "\\(.*\\), \\(.*\\)" author)
			   (format "%s %s"
				   (match-string 2 author)
				   (match-string 1 author)))
			  (t author)))
		  authors-split)))
    authors-formatted))

(defun ebib-extras-format-authors (authors &optional separator max)
  "Format AUTHORS as a string.
The authors are separated by SEPARATOR, which defaults to \" & \". A maximum of
MAX authors are included in the string, which defaults to three. When MAX is
exceeded, only the first author will be listed, followed by \" et al\"."
  (let ((separator (or separator " & "))
	(max (or max 3)))
    (if (> (length authors) max)
	(format "%s et al" (car authors))
      (mapconcat 'identity authors separator))))

(autoload 'zotra-extras-get-field "zotra-extras")
(defun ebib-extras-update-field (&optional field keep-braces)
  "Update FIELD in entry at point.
If FIELD is nil, update the field at point. If KEEP-BRACES is non-nil, do not
remove braces from the field value."
  (interactive)
  (when-let* ((id-or-url (ebib-extras-get-id-or-url))
	      (field (or field (ebib--current-field)))
	      (value (zotra-extras-get-field field id-or-url keep-braces)))
    (ebib-set-field-value
     field value (ebib--get-key-at-point) ebib--cur-db 'overwrite)
    (ebib-extras-update-entry-buffer ebib--cur-db)))

(defun ebib-extras-unbrace (string)
  "Remove braces from STRING.
Unlike `ebib-unbrace', this function removes all braces, not just the outermost."
  (replace-regexp-in-string "[{}]" "" string))

;;;;; Patched functions

(defvar index-window)
;; prevent unnecessary vertical window splits
(el-patch-defun ebib--setup-windows ()
  "Create Ebib's window configuration.
If the index buffer is already visible in some frame, select its
window and make the frame active,"
  (let ((index-window (get-buffer-window (ebib--buffer 'index) t))
	(old-frame (selected-frame)))
    (if index-window
	(progn (select-window index-window t)
	       (unless (eq (window-frame) old-frame)
		 (select-frame-set-input-focus (window-frame))
		 (setq ebib--frame-before old-frame)))
      (setq ebib--saved-window-config (current-window-configuration))
      (setq ebib--frame-before nil)
      (cond
       ((eq ebib-layout 'full)
	(delete-other-windows))
       ((eq ebib-layout 'custom)
	(setq ebib--window-before (selected-window))
	(delete-other-windows)
	(let ((width (cond
		      ((integerp ebib-width)
		       (- (window-total-width) ebib-width))
		      ((floatp ebib-width)
		       (- (window-total-width) (truncate (* (window-total-width) ebib-width)))))))
	  (select-window (split-window (selected-window) width t)))))
      (let* ((index-window (selected-window))
	     (entry-window (el-patch-swap
			     (split-window index-window ebib-index-window-size
					   ebib-window-vertical-split)
			     (selected-window))))
	(switch-to-buffer (ebib--buffer 'index))
	(unless (eq ebib-layout 'index-only)
	  (set-window-buffer entry-window (ebib--buffer 'entry)))
	(el-patch-remove
	  (set-window-dedicated-p index-window t))
	(if (eq ebib-layout 'custom)
	    (set-window-dedicated-p entry-window t)))))
  (if (buffer-local-value 'ebib--dirty-index-buffer (ebib--buffer 'index))
      (setq ebib--needs-update t)))

;; pass custom arguments to `format-time-string'
(el-patch-defun ebib--store-entry (entry-key fields db &optional timestamp if-exists)
  "Store the entry defined by ENTRY-KEY and FIELDS into DB.
Optional argument TIMESTAMP indicates whether a timestamp is to
be added to the entry.  Note that for a timestamp to be added,
`ebib-use-timestamp' must also be set to T. IF-EXISTS is as for
`ebib-db-set-entry'.

If storing the entry was successful, return the key under which
the entry is actually stored (which, if IF-EXISTS is `uniquify',
may differ from ENTRY-KEY); otherwise return nil.  Depending on
the value of IF-EXISTS, storing an entry may also result in an
error."
  (let ((actual-key (ebib-db-set-entry entry-key fields db if-exists)))
    (when (and actual-key timestamp ebib-use-timestamp)
      (ebib-set-field-value "timestamp" (format-time-string ebib-timestamp-format (el-patch-add nil "GMT")) actual-key db 'overwrite))
    actual-key))

(el-patch-defun ebib-reload-current-database ()
  "Reload the current database from disk."
  (interactive)
  (ebib--execute-when
    (entries
     (when (or (and (ebib-db-modified-p ebib--cur-db)
		    (yes-or-no-p "Database modified.  Really reload from file? "))
	       (y-or-n-p "Reload current database from file? "))
       (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
       (ebib--reload-database ebib--cur-db)
       (ebib--set-modified nil ebib--cur-db)
       ;; make point not disappear
       (el-patch-swap (ebib--update-buffers)
		      (save-window-excursion (ebib--update-buffers)))
       (message "Database reloaded")))
    (default
     (beep))))

;; [hack] hitting `RET' in 'ebib-index-mode' always edits the entry at point
(el-patch-defun ebib-edit-entry ()
  "Edit the current BibTeX entry."
  (interactive)
  (ebib--execute-when
    (entries
     (el-patch-add
       (if (string= (what-line) "Line 1")
	   (progn
	     (ebib-next-entry)
	     (ebib-prev-entry))
	 (ebib-prev-entry)
	 (ebib-next-entry)))
     (ebib--edit-entry-internal))
    (default
     (beep))))

(declare-function f-file-p "f")
;; when field contains a file, copy absolute file path
(el-patch-defun ebib-copy-field-contents (field)
  "Copy the contents of FIELD to the kill ring.
If the field contains a value from a cross-referenced entry, that
value is copied to the kill ring."
  (unless (or (not field)
	      (string= field "=type="))
    (el-patch-swap (let ((contents (ebib-get-field-value field (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)))
		     (if (stringp contents)
			 (progn (kill-new contents)
				(message "Field contents copied."))
		       (error "Cannot copy an empty field")))
		   (let* ((raw-contents (ebib-extras-get-field field))
			  (contents (if (f-file-p raw-contents)
					(file-truename raw-contents)
				      raw-contents)))
		     (if (stringp contents)
			 (progn (kill-new contents)
				(message "Field contents copied."))
		       (user-error "Cannot copy an empty field"))))))

(provide 'ebib-extras)
;;; ebib-extras.el ends here
