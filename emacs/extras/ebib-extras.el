;;; ebib-extras.el --- Extensions for ebib -*- lexical-binding: t -*-

;; Author: Pablo Stafforini
;; Maintainer: Pablo Stafforini
;; Version: 0.1
;; Homepage: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/ebib-extras.el

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
(require 'bibtex)
(require 'el-patch)
(require 'citar)
(require 'ebib)
(require 'filenotify)
(require 'mullvad)
(require 'paths)
(require 's)
(require 'scihub)
(require 'tlon-babel-tex)
(require 'tlon-babel-tts)
(require 'zotra)
(require 'window-extras)

;;;; Functions

(defun ebib-extras-open-or-switch ()
  "Open ebib in the right window or switch to it if already open."
  (interactive)
  (window-extras-split-if-unsplit)
  (if (> (frame-width) window-extras-frame-split-width-threshold)
      (winum-select-window-3)
    (winum-select-window-2))
  (ebib))

(defun ebib-extras-reload-database-no-confirm (db)
  "Reload the database DB from disk, without asking for confirmation."
  (ebib--execute-when
    (entries
     (unless (and (ebib-db-modified-p db)
		  (not (yes-or-no-p "Database modified.  Really reload from file? ")))
       (ebib-db-set-current-entry-key (ebib--get-key-at-point) db)
       (ebib--reload-database db)
       (ebib--set-modified nil db)
       (message "Database reloaded")))
    (default
     (beep))))

(defvar ebib-extras-isbn-p
  "\\(ISBN-*\\(1[03]\\)* *\\(: \\)?\\)*\\(\\([0-9Xx][ -]*\\)\\{13\\}\\|\\([0-9Xx][ -]*\\)\\{10\\}\\)")

(defun ebib-extras-isbn-p (string)
  "Return t if STRING is an ISBN."
  (string-match ebib-extras-isbn-p string))

(defun ebib-extras-get-isbn ()
  "Return ISBN for the current entry, if it exists."
  (when-let ((get-field (pcase major-mode
			  ('ebib-entry-mode #'ebib-extras-get-field)
			  ('bibtex-mode #'bibtex-extras-get-field)))
	     (isbn
	      (funcall get-field "isbn")))
    (car (split-string
	  (s-replace "-"
		     ""
		     (substring-no-properties
		      isbn))
	  " "))))

(defun ebib-extras-video-p (string)
  "Return t if STRING is a video URL."
  ;; TODO: Add more video sites
  (string-match
   "https?://\\(www\\.\\)?\\(youtube\\.com/watch\\?v=\\|youtu.be/\\)\\([a-zA-Z0-9_-]+\\)"
   string))

(defun ebib-extras--update-file-field-contents (key file-name)
  "Update contents of FILE-NAME in field `file' for entry KEY."
  (let* ((get-field (pcase major-mode
		      ('ebib-entry-mode #'ebib-extras-get-field)
		      ('bibtex-mode #'bibtex-extras-get-field)))
	 (field "file")
	 (file-field-contents (funcall get-field field)))
    (unless (and
	     file-field-contents
	     (catch 'file-exists
	       (dolist (file (ebib--split-files file-field-contents))
		 (when (string= file file-name)
		   (throw 'file-exists file)))))
      (pcase major-mode
	('ebib-entry-mode (ebib-set-field-value field file-name key ebib--cur-db ";"))
	('bibtex-mode (bibtex-extras-append-to-field field file-name)))
      (when (eq major-mode 'ebib-entry-mode)
	(ebib--store-multiline-text (current-buffer))
	(ebib--redisplay-field field)
	(ebib--redisplay-index-item field)
	(ebib-save-current-database t)))))

(defvar ebib-extras-book-like-entry-types
  (let ((lowercase '("book" "collection" "mvbook" "inbook" "incollection" "bookinbook" "suppbook")))
    (append lowercase (mapcar (lambda (entry)
				(concat (upcase (substring entry 0 1))
					(substring entry 1)))
			      lowercase)))
  "Entry types for books and book-like entities.
The entry types are included in both lowercase and sentence case.")

(defvar ebib-extras-article-like-entry-types
  (let ((lowercase '("article")))
    (append lowercase (mapcar (lambda (entry)
				(concat (upcase (substring entry 0 1))
					(substring entry 1)))
			      lowercase)))
  "Entry types for articles and article-like entities.
The entry types are included in both lowercase and sentence case.")

(defvar ebib-extras-film-like-entry-types
  (let ((lowercase '("movie" "video" "tvepisode")))
    (append lowercase (mapcar (lambda (entry)
				(concat (upcase (substring entry 0 1))
					(substring entry 1)))
			      lowercase)))
  "Entry types for articles and article-like entities.
The entry types are included in both lowercase and sentence case.")

(defun ebib-extras-copy-current-field-contents ()
  "Copy the contents of the current field to the kill ring.
If the field contains a value from a cross-referenced entry, that
value is copied to the kill ring."
  (interactive)
  (ebib-copy-field-contents (ebib--current-field)))

(defun ebib-extras-get-file (extension)
  "Return the file with EXTENSION in entry at point.
A file will be returned if it uniquely exists."
  (let ((get-field (pcase major-mode
		     ('ebib-entry-mode #'ebib-extras-get-field)
		     ('bibtex-mode #'bibtex-extras-get-field))))
    (when-let ((files (funcall get-field "file")))
      (catch 'tag
	(mapc
	 (lambda (file)
	   (when (equal (file-name-extension file) extension)
	     (throw 'tag (expand-file-name file))))
	 (ebib--split-files files))
	nil))))

(defun ebib-extras-open-file (extension)
  "Open file with EXTENSION in entry at point.
A file will be opened if it uniquely exists."
  (interactive)
  (if-let ((file-name (ebib-extras-get-file extension)))
      (find-file file-name)
    (user-error (format "No (unique) `%s' file found" extension))))

(defun ebib-extras-open-file-externally (extension)
  "Open file with EXTENSION in entry at point, if it (uniquely)
exists."
  (interactive)
  (if-let ((file-name (expand-file-name (ebib-extras-get-file extension))))
      (shell-command (format "open '%s'" file-name))
    (user-error (format "No (unique) `%s' file found" extension))))

(defun ebib-extras-open-pdf-file ()
  "Open `pdf' file in entry at point, if it (uniquely) exists."
  (interactive)
  (ebib-extras-open-file "pdf"))

(defun ebib-extras-open-md-file ()
  "Open `md' file in entry at point, if it (uniquely) exists."
  (interactive)
  (ebib-extras-open-file "md"))

(defun ebib-extras-open-pdf-file-externally ()
  "Open `pdf' file in entry at point externally, if it (uniquely) exists."
  (interactive)
  (ebib-extras-open-file-externally "pdf"))

(defun ebib-extras-open-webm-file ()
  "Open `webm' file in entry at point, if it (uniquely) exists."
  (interactive)
  (ebib-extras-open-file-externally "webm"))

(defun ebib-extras-open-mp3-file ()
  "Open `webm' file in entry at point, if it (uniquely) exists."
  (interactive)
  (ebib-extras-open-file-externally "mp3"))

(defun ebib-extras-open-html-file ()
  "Open `html' file in entry at point, if it (uniquely) exists."
  (interactive)
  (ebib-extras-open-file "html")
  (let ((html-buffer (buffer-name))
	(browse-url-handlers nil)
	(browse-url-browser-function #'eww-browse-url))
    (browse-url-of-buffer)
    (kill-buffer html-buffer)))

(defun ebib-extras-open-html-file-externally ()
  "Open `html' file in entry at point externally, if it (uniquely) exists."
  (interactive)
  (ebib-extras-open-file-externally "html"))

(defvar ebib-extras-valid-file-extensions
  '("pdf" "html" "webm" "flac" "mp3" "md")
  "List of valid file extensions for `ebib-extras-open-file-dwim'.")

(defun ebib-extras-open-file-dwim ()
  "Open file in entry at point.
If the entry contains more than one file, use the preference
ordering defined in `ebib-extras-valid-file-extensions'."
  (interactive)
  (if-let ((extension
	    (catch 'tag
	      (dolist (extension ebib-extras-valid-file-extensions)
		(when (ebib-extras-get-file extension)
		  (throw 'tag extension))))))
      (call-interactively
       (intern
	(concat "ebib-extras-open-" extension "-file")))
    (user-error "No file found")))

(defun ebib-extras-rename-files ()
  "Rename files in entry at point to match its key."
  (interactive)
  (ebib--execute-when
    (entries
     (let* ((field "file")
	    (key (ebib--get-key-at-point))
	    (file-list (split-string
			(ebib-extras-get-field field)
			";")))
       (ebib-extras-check-valid-key key)
       (when file-list
	 (ebib-delete-field-contents field t)
	 (dolist (filename file-list)
	   (let ((stem (file-name-base filename))
		 (extension (file-name-extension filename)))
	     (unless (equal stem key)
	       (let ((new-filename
		      (ebib-extras--rename-and-abbreviate-file
		       (ebib-extras--extension-directories extension)
		       key
		       extension)))
		 (rename-file filename new-filename)
		 (setq filename new-filename)))
	     (ebib-set-field-value field filename key ebib--cur-db ";")))
	 (ebib--redisplay-field field)
	 (ebib--redisplay-index-item field))))
    ;; (ebib-save-current-database nil))))
    (default
     (beep))))

(defun ebib-extras-validate-file-stem ()
  "Check that stem of each attached file equals entry's unique key."
  (when-let ((files (ebib-extras-get-field "file")))
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
  "Move file to DIRECTORY, rename it to its KEY and abbreviate its path.
If EXTENSION is non-nil, set its extension to its value."
  (file-name-concat
   (abbreviate-file-name
    directory)
   (if extension
       (file-name-with-extension key extension)
     key)))

(defun ebib-extras-check-valid-key (&optional key)
  "Check that KEY is a valid entry key; if not, throw an error."
  (setq key (or key (ebib--get-key-at-point)))
  (unless (string-match
	   "^[_[:alnum:]-]\\{2,\\}[[:digit:]]\\{4\\}[_[:alnum:]]\\{2,\\}$"
	   key)
    (user-error "Ebib entry has an invalid key; please regenerate it")))

(defun ebib-extras--extension-directories (extension)
  "Return directory associated with EXTENSION."
  (cond ((string= extension "pdf")
	 paths-dir-pdf-library)
	((string= extension "html")
	 paths-dir-html-library)
	((or (string= extension "webm")
	     (string= extension "mp3")
	     (string= extension "flac"))
	 paths-dir-media-library)
	(t
	 (user-error "Invalid file extension"))))

;;;;; attach downloads

(defun ebib-extras-attach-file (&optional file most-recent open)
  "Attach FILE to the current entry.
If FILE is nil, prompt the user for one. If MOST-RECENT is non-nil, attach the
most recent file in the `paths-dir-downloads' directory. If OPEN is non-nil,
open FILE."
  (interactive)
  (let* ((get-key (pcase major-mode
		    ('ebib-entry-mode #'ebib--get-key-at-point)
		    ('bibtex-mode #'bibtex-extras-get-key)))
	 (key (funcall get-key)))
    (ebib-extras-check-valid-key key)
    (let* ((file-to-attach
	    (or
	     (when most-recent
	       (files-extras-newest-file paths-dir-downloads))
	     file
	     (let ((initial-folder
		    (completing-read "Select folder: "
				     (list
				      paths-dir-downloads
				      paths-dir-pdf-library
				      paths-dir-html-library
				      paths-dir-media-library))))
	       (read-file-name
		"File to attach: "
		;; Use key as default selection if key-based file exists
		;; else default to `initial-folder'
		(if (catch 'found
		      (dolist (extension ebib-extras-valid-file-extensions)
			(when (file-regular-p (file-name-concat
					       initial-folder
					       (file-name-with-extension key extension)))
			  (throw 'found extension))))
		    (file-name-concat initial-folder key)
		  initial-folder)))))
	   (extension (file-name-extension file-to-attach))
	   (destination-folder (ebib-extras--extension-directories extension))
	   (file-name (ebib-extras--rename-and-abbreviate-file
		       destination-folder key extension)))
      (when (or (not (file-regular-p file-name))
		(y-or-n-p "File exists. Overwrite? "))
	(rename-file file-to-attach file-name t))
      (shut-up
	(ebib-extras--update-file-field-contents key file-name))
      (when (string= (file-name-extension file-name) "pdf")
	(ebib-extras-set-pdf-metadata)
	;; open the pdf to make sure it displays the web page correctly
	(ebib-extras-ocr-pdf)
	(when open
	  (ebib-extras-open-pdf-file))))))

(defun ebib-extras-attach-most-recent-file ()
  "Attach the most recent file in `paths-dir-downloads' to the current entry."
  (interactive)
  (ebib-extras-attach-file nil 'most-recent))

(defun ebib-extras-url-to-file-attach (type)
  "Generate PDF of file of TYPE."
  (when (ebib-extras-get-field "url")
    (eww-extras-url-to-file type nil #'ebib-extras-attach-file-to-entry)))

(defun ebib-extras-url-to-pdf-attach ()
  "Generate PDF of URL."
  (interactive)
  (ebib-extras-url-to-file-attach "pdf"))

(defun ebib-extras-url-to-html-attach ()
  "Generate HTML of URL."
  (interactive)
  (ebib-extras-url-to-file-attach "html"))

(defun ebib-extras-attach-file-to-entry (&optional file _)
  "Attach FILE to the relevant entry.
The relevant entry is the entry whose key equals the name of FILE sans its
extension."
  (let ((key (file-name-nondirectory (file-name-sans-extension file))))
    (save-excursion
      (ebib-extras-open-key key)
      (ebib-extras-attach-file file)
      (message "Attached `%s' to %s" file key))))

;;;;; ?

(defvar ebib-extras-iso-639-2
  '(("english" . "eng")
    ("american" . "eng")
    ("french" . "fra")
    ("german" . "deu")
    ("italian" . "ita")
    ("spanish" . "spa")
    ("portuguese" . "por")
    ("russian" . "rus")
    ("chinese" . "zho")
    ("japanese" . "jpn")
    ("korean" . "kor")
    ("arabic" . "ara")
    ("latin" . "lat")
    ("greek" . "ell"))
  "Alist of languages and their ISO 639-2 codes.")

(defun ebib-extras-ocr-pdf (&optional force)
  "OCR the PDF file in the current entry.
If FORCE is non-nil, force OCR even if the file is already"
  (interactive)
  (let* ((get-field (pcase major-mode
		      ('ebib-entry-mode #'ebib-extras-get-field)
		      ('bibtex-mode #'bibtex-extras-get-field)))
	 (set-field (pcase major-mode
		      ('ebib-entry-mode #'ebib-extras-set-field)
		      ('bibtex-mode #'bibtex-set-field)))
	 (lang (or (funcall get-field "langid")
		   (completing-read "Select language: " ebib-extras-iso-639-2 nil t "english"))))
    (funcall set-field "langid" lang)
    (let ((file-name (ebib-extras-get-file "pdf")))
      (files-extras-ocr-pdf nil nil
			    (format (concat (when force "--force-ocr ") "--deskew -l %s \"%s\" \"%s\"")
				    (alist-get lang ebib-extras-iso-639-2 nil nil 'string=)
				    (expand-file-name file-name)
				    (expand-file-name file-name))))))

(defvar ebib-extras-download-use-vpn nil
  "Whether to use a VPN when downloading content.")

(defvar ebib-extras-library-genesis
  '("Library Genesis"
    "https://libgen.li/index.php?req=%s" "&columns%5B%5D=t&columns%5B%5D=a&columns%5B%5D=s&columns%5B%5D=y&columns%5B%5D=p&columns%5B%5D=i&objects%5B%5D=f&objects%5B%5D=e&objects%5B%5D=s&objects%5B%5D=a&objects%5B%5D=p&objects%5B%5D=w&topics%5B%5D=l&res=25&filesuns=all"))

(defvar ebib-extras-amazon
  '("Amazon"
    "https://smile.amazon.com/s?k="
    "&i=stripbooks"))

(defvar ebib-extras-worldcat
  '("Worldcat"
    "https://www.worldcat.org/search?q="
    "&itemType=book&limit=50&offset=1"))

(defvar ebib-extras-internet-archive
  '("Internet Archive"
    "https://archive.org/search.php?query="
    ""))

(defvar ebib-extras-university-of-toronto
  '("University of Toronto"
    "https://librarysearch.library.utoronto.ca/discovery/search?query=any,contains,"
    "&tab=Everything&search_scope=UTL_AND_CI&vid=01UTORONTO_INST:UTORONTO&offset=0"))

(defvar ebib-extras-university-of-california-berkeley
  '("University of California, Berkeley"
    "https://search.library.berkeley.edu/discovery/search?query=any,contains,"
    "&tab=Default_UCLibrarySearch&search_scope=DN_and_CI&vid=01UCS_BER:UCB&offset=0"))

(defvar ebib-extras-hathitrust
  '("HathiTrust"
    "https://babel.hathitrust.org/cgi/ls?q1="
    "&field1=ocr;a=srchls;lmt=ft;sz=100"))

(defvar ebib-extras-connected-papers
  '("Connected Papers"
    "https://www.connectedpapers.com/search?q="
    ""))

(defvar ebib-extras-google-scholar
  '("Google Scholar"
    "https://scholar.google.com/scholar?q="
    ""))

(defvar ebib-extras-wikipedia
  '("Google Scholar"
    "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&profile=default&search="
    "&fulltext=Search"))

(defvar ebib-extras-goodreads
  '("Goodreads"
    "https://www.goodreads.com/search?q="
    ""))

(defvar ebib-extras-audible
  '("Audible"
    "https://www.audible.com/search?keywords="
    ""))

(defvar ebib-extras-audiobookbay
  '("Audiobook Bay"
    "https://theaudiobookbay.se/?s="
    "&tt=1"))

(defvar ebib-extras-imdb
  '("IMDB"
    "https://www.imdb.com/find/?q="
    ""))

(defvar ebib-extras-letterboxd
  '("Letterboxd"
    "https://letterboxd.com/search/films/"
    ""))

(defvar ebib-extras-metacritic
  '("Metacritic"
    "https://www.metacritic.com/search/all/"
    "/results"))

(defvar ebib-extras-search-book-functions
  '(ebib-extras-search-goodreads
    ebib-extras-search-hathitrust
    ebib-extras-search-university-of-california-berkeley
    ebib-extras-search-internet-archive
    ebib-extras-search-library-genesis
    ebib-extras-search-amazon)
  "List of functions that search for books.")

(defvar ebib-extras-download-book-functions
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

(defvar ebib-extras-search-article-functions
  '(ebib-extras-search-connected-papers
    ebib-extras-search-google-scholar)
  "List of functions that search for articles.")

(defvar ebib-extras-download-article-functions
  '(ebib-extras-search-article-functions)
  "List of functions that download articles.")

(defvar ebib-extras-search-film-functions
  '(ebib-extras-search-imdb
    ebib-extras-search-letterboxd
    ebib-extras-search-metacritic)
  "List of functions that search for films.")

(defvar ebib-extras-download-film-functions
  ebib-extras-search-film-functions
  "List of functions that search for films.")

(defvar ebib-extras-search-engines '()
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

(defmacro ebib-extras-generate-search-commands ()
  "Generate search commands for search engines in `ebib-extras-search-engines'."
  `(progn
     ,@(mapcar (lambda (engine)
		 `(defun ,(intern (concat "ebib-extras-search-" (symbol-name engine))) (&optional query)
		    ,(let ((name (capitalize (replace-regexp-in-string "-" " " (symbol-name engine)))))
		       (format "Run a search on %s." name)
		       `(interactive ,(format "sSearch %s: " name)))
		    (ebib-extras-search
		     ,(intern (concat "ebib-extras-" (symbol-name engine)))
		     query)))
	       ebib-extras-search-engines)))

;; (ebib-extras-generate-search-commands)

(defun ebib-extras-search (search-engine query)
  "Search for QUERY with SEARCH-ENGINE."
  (let* ((prefix (nth 1 search-engine))
	 (suffix (nth 2 search-engine))
	 (query (url-hexify-string query)))
    (browse-url (concat prefix query suffix))))

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
		   (when-let ((title
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
     (if-let ((title (ebib-extras--get-title-at-point)))
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
     (if-let ((title (ebib-extras-get-field "title")))
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
     (if-let ((title (ebib-extras-get-field "title")))
	 (ebib-extras-search-film title)
       (user-error "Title field is empty")))
    (default
     (beep))))

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

  ;;; download functions
(defun ebib-extras-download-dwim ()
  "Try to download the current entry based on the field at point.
If field at point is `title', run a search with its value, else
use identifier.

The list of websites for the search query is defined by the
variable `ebib-extras-download-book'"
  (interactive)
  (ebib-extras--search-or-download 'download))

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
     (if-let ((title (ebib-extras--get-title-at-point)))
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
     (if-let ((title (ebib-extras-get-field "title")))
	 (ebib-extras-download-article title)
       (user-error "Title field is empty")))
    (default
     (beep))))

(defun ebib-extras-download-pdf ()
  "Download and attach a PDF of the work at point based on its DOI, URL or ISBN."
  (interactive)
  (let ((get-field (pcase major-mode
		     ('ebib-entry-mode #'ebib-extras-get-field)
		     ('bibtex-mode #'bibtex-extras-get-field))))
    (or (when-let ((doi (funcall get-field "doi")))
	  (scihub-download doi))
	(when-let ((url (funcall get-field "url")))
	  (eww-extras-url-to-pdf url))
	(when-let ((isbn (ebib-extras-get-isbn)))
	  (ebib-extras-download-book isbn)))))

;; all we want is to search for a film, and there is no film
;; identifier, so we map all functions to
;; `ebib-extras-search-film-by-title'
(defalias 'ebib-extras-download-film-by-title 'ebib-extras-search-film-by-title)
(defalias 'ebib-extras-download-film-by-identifier 'ebib-extras-search-film-by-title)

(defun ebib-extras-download-video (id)
  "Download video with id ID using `yt-dlp'."
  (unless (executable-find "yt-dlp")
    (user-error "Please install `yt-dlp' (e.g. `brew install yt-dlp')"))
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
       (setq words (mapcar
		    (lambda (word)
		      (if
			  ;; match words containing {} or \ which are probably
			  ;; LaTeX or protected words
			  (string-match "\\$\\|{\\|}\\|\\\\" word)
			  word
			(s-downcase word)))
		    words))
       ;; capitalize first word
       (setf (car words) (s-capitalize (car words)))
       (setq value (mapconcat 'identity words " "))
       (ebib-set-field-value field value (ebib--get-key-at-point) ebib--cur-db 'overwrite 'unbraced)
       (ebib--store-multiline-text (current-buffer))
       (ebib--redisplay-field field)
       (ebib--redisplay-index-item field)
       (ebib-save-current-database nil)))
    (default
     (beep))))

(defun ebib-extras-get-or-open-entry ()
  "Get or open the BibTeX entry, depending on how the function was called.
If called interactively, open the entry. Otherwise, return it as a string."
  (interactive)
  (when-let ((file (ebib-db-get-filename ebib--cur-db))
	     (key (ebib--get-key-at-point))
	     (fun (if (called-interactively-p 'any) #'find-file #'find-file-noselect)))
    (with-current-buffer (funcall fun file)
      (bibtex-search-entry key)
      (unless (called-interactively-p 'any) (bibtex-extras-get-entry-as-string)))))

(defun ebib-extras-get-file-of-key (key)
  "Return the bibliographic file in which the entry with KEY is found."
  (unless ebib--databases
    (user-error "Please launch Ebib first"))
  (let ((file (catch 'found
		(dotimes (i (length ebib--databases))
		  (when (member key (ebib-db-list-keys (nth i ebib--databases)))
		    (throw 'found
			   (ebib-db-get-filename (nth i ebib--databases))))))))
    (message file)))

(defun ebib-extras-open-key (key)
  "Open the entry for KEY in Ebib."
  (when-let ((file (ebib-extras-get-file-of-key key)))
    (ebib file key)
    (ebib-edit-entry)))

(defvar ebib-extras-sort-toggle 'Title)

(defun ebib-extras-sort-toggle ()
  "Toggle between sorting by timestamp, author, and title."
  (interactive)
  (ebib--execute-when
    (entries
     (let ((order 'ascend))
       (pcase ebib-extras-sort-toggle
	 ('Timestamp
	  (setq ebib-extras-sort-toggle 'Author))
	 ('Author
	  (setq ebib-extras-sort-toggle 'Title))
	 ('Title
	  (setq ebib-extras-sort-toggle 'Timestamp)
	  (setq order 'descend)))
       (ebib--index-sort (symbol-name ebib-extras-sort-toggle) order)
       (goto-char (point-min))
       (message (format "Sorting by %s" ebib-extras-sort-toggle))))
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
	    (file (zotra-extras-set-bibfile))
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

(defun ebib-extras-citar-open-notes ()
  "Open note for the entry at point using `citar-open-notes'.
This command replaces the native `ebib-popup-note'. The
replacement ensures that, when a note doesn't already exist, it
is created following the same schema as notes created with
`citar'."
  (interactive)
  (ebib--execute-when
    (entries
     (when-let ((citekey (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)))
       (citar-open-notes (list citekey))))
    (default
     (beep))))

(defvar ebib-extras-auto-save-files
  `(,paths-file-personal-bibliography-new
    ,tlon-babel-refs-file-fluid)
  "List of database files that should be auto-saved.
The big files containing the `old' bibliographic entries are excluded.")

(defun ebib-extras-auto-save-databases ()
  "Check if any Ebib database has been modified and save it to its file if so.
The list of files to be watched is defined in `ebib-extras-auto-save-files'."
  (dolist (db ebib--databases)
    (let ((db-file (ebib-db-get-filename db)))
      (when (and (member db-file ebib-extras-auto-save-files)
		 (ebib-db-modified-p db))
	(ebib--save-database db '(16)))))
  (run-with-timer 1 nil #'ebib-extras-auto-save-databases))

(run-with-timer 1 nil #'ebib-extras-auto-save-databases)

(defun ebib-extras-auto-reload-databases ()
  "Check if any db file has been modified and reload its Ebib database if so.
The list of files to be watched is defined in `ebib-extras-auto-save-files'."
  (dolist (db ebib--databases)
    (let ((db-file (ebib-db-get-filename db)))
      (when (member db-file ebib-extras-auto-save-files)
	;; (file-notify-rm-all-watches)
	(file-notify-add-watch
	 db-file
	 '(change attribute-change)
	 (lambda (event)
	   (message "reloading database")
	   (ebib-extras-reload-database-no-confirm db)))))))

(defvar ebib-extras-db-numbers
  `((,paths-file-personal-bibliography-new . 1)
    (,paths-file-personal-bibliography-old . 2)
    (,tlon-babel-refs-file-fluid . 3)
    (,tlon-babel-refs-file-stable . 4))
  "Association list of database files and their numbers.")

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
  (let* ((db ebib--cur-db)
	 (author-to-check (ebib-extras-get-field "author")))
    (if (member author-to-check ebib-extras-existing-authors)
	(message "Author found in the database!")
      (message "Warning: Author not found in the database!"))))

(defun ebib-extras-set-rating ()
  "Set rating of current entry.
If applicable, open external website to set rating there as well."
  (interactive)
  (let ((rating (ebib-extras-choose-rating))
	(supertype (ebib-extras-get-supertype))
	(title (ebib-extras-get-field "title"))
	(key (ebib--get-key-at-point))
	(db ebib--cur-db))
    ;; TODO: open rating websites based on supertype
    (pcase supertype
      ("book" (ebib-extras-search-goodreads title))
      ("film" (ebib-extras-search-imdb title) (ebib-extras-search-letterboxd title)))
    (ebib-set-field-value "rating" rating (ebib--get-key-at-point) ebib--cur-db 'overwrite)
    (ebib-extras-update-entry-buffer db)))

;; TODO: find way to do this without moving point
(defun ebib-extras-update-entry-buffer (db)
  "Update the entry buffer with the current entry in DB."
  (ebib--update-entry-buffer)
  (set-buffer-modified-p nil)
  (ebib--set-modified t db t (seq-filter (lambda (dependent)
					   (ebib-db-has-key key dependent))
					 (ebib--list-dependents db))))

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

(defun ebib-extras-move-entry-to-tlon ()
  "Move bibliographic entry associated with the key at point to the Tlön bibliography."
  (interactive)
  (let ((key (ebib--db-get-current-entry-key ebib--cur-db)))
    (citar-extras-goto-bibtex-entry key)
    (bibtex-extras-move-entry-to-tlon)
    (ebib tlon-babel-refs-file-fluid key)
    (ebib-extras-open-key key)))

(defun ebib-extras-get-field (field)
  "Get the value of FIELD for the entry at point.
Convenience function that calls `ebib-get-field-value' with
sensible defaults and remove line breaks and empty spaces."
  (when-let* ((key (ebib--get-key-at-point))
	      (value (ebib-get-field-value field key ebib--cur-db t t t)))
    (replace-regexp-in-string "[\n\t ]+" " " value)))

(defun ebib-extras-get-id-or-url ()
  "Get the ID or URL of the entry at point."
  (let ((get-field (pcase major-mode
		     ('ebib-entry-mode #'ebib-extras-get-field)
		     ('bibtex-mode #' (bibtex-extras-get-field)))))
    (when-let ((id-or-url (catch 'found
			    (dolist (field '("doi" "isbn" "url"))
			      (when-let ((value (funcall get-field field)))
				(throw 'found value))))))
      id-or-url)))

(defun ebib-extras-fetch-id-or-url ()
  "Fetch the ID or URL of the entry at point.
Fetching is done using `bib'."
  (let* ((get-field (pcase major-mode
		      ('ebib-entry-mode #'ebib-extras-get-field)
		      ('bibtex-mode #' (bibtex-extras-get-field))))
	 (title (get-field "title"))
	 (author (get-field "author")))
    (pcase (ebib-extras-get-supertype)
      ("book" (bib-search-isbn (format "% %" title author)))
      ("article" (bib-search-crossref title author))
      ("film" (bib-search-imdb
	       (bib-translate-title-to-english title)))
      (_ (get-field "url")))))

(defun ebib-extras-get-or-fetch-id-or-url ()
  "Get the ID or URL of the entry at point, or fetch it if missing."
  (or (ebib-extras-get-id-or-url)
      (ebib-extras-fetch-id-or-url)))

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
  (when-let ((file (ebib-db-get-filename ebib--cur-db)))
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
  (ebib-set-field-value field value (ebib--get-key-at-point) ebib--cur-db 'overwrite)
  (ebib-extras-update-entry-buffer ebib--cur-db))

(defun ebib-extras-fetch-keywords ()
  "Fetch keywords for the entry at point and put them in the associated org file."
  (interactive)
  (ebib-extras-fetch-field-value "keywords"))

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

(defun ebib-extras-create-section-entry (&optional title)
  "Create a BibTeX entry for the section of the current entry.
Prompt the user for a title, unless TITLE is non-nil."
  (interactive)
  (let* ((fields `(("title" . ,(or title (read-string "Section title: ")))
		   ("eventtitle" . ,(ebib-extras-get-field "title"))
		   ("url" . ,(read-string "URL: " (ebib-extras-get-field "url")))
		   ("crossref" . ,(ebib--get-key-at-point))
		   ("author" . ,(ebib-extras-get-field "author"))
		   ("date" . ,(ebib-extras-get-field "")))))
    (tlon-babel--create-entry-from-current fields)))


;;;;; pdf metadata

(defun ebib-extras-set-pdf-metadata ()
  "Set the metadata of the PDF associated with the current entry."
  (interactive)
  (unless (executable-find "exiftool")
    (user-error "Please install `exiftool' (e.g. `brew install exiftool'"))
  (unless (derived-mode-p 'ebib-entry-mode 'bibtex-mode)
    (user-error "Not in `ebib-entry-mode' or `bibtex-mode'"))
  (when-let ((get-field (pcase major-mode
			  ('ebib-entry-mode #'ebib-extras-get-field)
			  ('bibtex-mode #'bibtex-extras-get-field)))
	     (file (ebib-extras-get-file "pdf"))
	     (author (or (funcall get-field "author")
			 (funcall get-field "editor"))))
    (let* ((file-absolute (expand-file-name file))
	   (author-list (ebib-extras-get-authors-list author))
	   (author-string (ebib-extras-format-authors author-list))
	   (title (funcall get-field  "title"))
	   (author-arg (format "-Author='%s' " author-string))
	   (title-arg (format "-Title='%s' " title)))
      (when (or author-arg title-arg)
	(shell-command (concat "exiftool -overwrite_original "
			       (when author-arg author-arg)
			       (when title-arg title-arg)
			       (shell-quote-argument file-absolute))))
      (message "Set metadata for PDF file `%s'" file))))

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

(declare-function zotra-extras-get-field "zotra-extras")
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

;;;;; Patched functions

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

;; keep focus in current entry when the database is saved or reloaded.
(el-patch-defun ebib--save-database (db &optional force)
  "Save the database DB.
The FORCE argument is used as in `ebib-save-current-database'."
  ;; See if we need to make a backup.
  (when (and (ebib-db-backup-p db)
	     (file-exists-p (ebib-db-get-filename db)))
    (ebib--make-backup (ebib-db-get-filename db))
    (ebib-db-set-backup nil db))

  ;; Check if the file has changed on disk.
  (let ((db-modtime (ebib-db-get-modtime db))
	(file-modtime (ebib--get-file-modtime (ebib-db-get-filename db))))
    ;; If the file to be saved has been newly created, both modtimes are nil.
    (when (and db-modtime file-modtime
	       (time-less-p db-modtime file-modtime))
      (unless (or (and (listp force)
		       (eq 16 (car force)))
		  (yes-or-no-p (format "File `%s' changed on disk.  Overwrite? " (ebib-db-get-filename db))))
	(error "[Ebib] File not saved"))))

  ;; Now save the database.
  (el-patch-swap
    (with-temp-buffer
      (ebib--format-database-as-bibtex db)
      (write-region (point-min) (point-max) (ebib-db-get-filename db)))
    (let ((buf (current-buffer)))
      (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
      (with-temp-buffer
	(ebib--format-database-as-bibtex db)
	(write-region (point-min) (point-max) (ebib-db-get-filename db)))))
  (ebib--set-modified nil db))

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

;; [hack] hitting `RET' in `ebib-index-mode' always edits the entry at point
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

;; ask user before uniquifying key
(el-patch-defun ebib-db-set-entry (key data db &optional if-exists)
  "Set KEY to DATA in database DB.
DATA is an alist of (FIELD . VALUE) pairs.

IF-EXISTS defines what to do when the key already exists in DB.
If it is `overwrite', replace the existing entry.  If it is `uniquify',
generate a unique key by appending a letter `b', `c', etc., to it.
If it is `noerror', a duplicate key is not stored and the function
returns nil.  If it is nil (or any other value), a duplicate key
triggers an error.

In order to delete an entry, DATA must be nil and IF-EXISTS must be
`overwrite'.

If storing/updating/deleting the entry is successful, return its key.

Note that this function should not be used to add an entry to a
dependent database.  The entry will be added to the main database
instead.  Use `ebib-db-add-entries-to-dependent' instead."
  (let ((exists (gethash key (ebib-db-val 'entries db))))
    (when exists
      (cond
       ;;  If so required, make the entry unique:
       (el-patch-swap
	 ((eq if-exists 'uniquify)
	  (setq key (ebib-db-uniquify-key key db))
	  (setq exists nil))
	 ((eq if-exists 'uniquify)
	  (when (y-or-n-p
		 (format "[Ebib] Key `%s' exists in database `%s'; uniquify? "
			 key (ebib-db-get-filename db 'shortened)))
	    (setq key (ebib-db-uniquify-key key db))
	    (setq exists nil))))
       ;; If the entry is an update, we simply pretend the key does not exist:
       ((eq if-exists 'overwrite)
	(setq exists nil))
       ;; Otherwise signal an error, if so requested:
       ((not (eq if-exists 'noerror))
	(error "[Ebib] Key `%s' exists in database `%s'; cannot overwrite" key (ebib-db-get-filename db 'shortened)))))
    (unless exists
      (if data
	  (puthash key data (ebib-db-val 'entries db))
	(remhash key (ebib-db-val 'entries db)))
      key)))

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
