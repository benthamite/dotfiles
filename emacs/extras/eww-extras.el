;;; eww-extras.el --- Extensions for eww -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/eww-extras.el
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

;; Extensions for `eww'.

;;; Code:

(require 'eww)
(require 'paths)
(require 'simple-extras)

;;;; Variables

;;;;; Chrome headless

(defconst eww-extras-convert-to-pdf
  "'%s' --headless --user-data-dir='%s' --no-pdf-header-footer '%s' --print-to-pdf='%s'"
  "Command to convert a URL to a PDF file.
The placeholders `%s' are replaced by the Chrome program, the Chrome cookie data
directory, the URL, and the output file.")

(defconst eww-extras-convert-to-html
  "'%s' --headless --user-data-dir='%s' --dump-dom '%s' > %s"
  "Command to convert a URL to an HTML file.
The placeholders `%s' are replaced by the Chrome program, the Chrome cookie data
directory, the URL, and the output file.")

;;;;; Annas Archive

(defvar eww-extras-annas-archive-callback nil
  "Callback function to run by `eww-extras-annas-archive-download-file'.")

(defvar eww-extras-annas-archive-bibtex-key nil
  "BibTeX key of the book being downloaded.")

(defconst eww-extras-annas-archive-home-url
  "https://annas-archive.gs/"
  "URL to Anna’s Archive.")

(defconst eww-extras-annas-archive-auth-url
  (concat eww-extras-annas-archive-home-url "account/")
  "URL to authenticate with Anna’s Archive.")

;;;;; Subtitles

(defconst eww-extras-download-subtitles
  "yt-dlp --skip-download --write-auto-subs '%s'"
  "Command to download subtitles from a URL in `srt' format.
The placeholders are replaced by the URL.")

;;;; User options

(defgroup eww-extras ()
  "Extensions for `eww'."
  :group 'eww)

(defcustom eww-extras-readable-exceptions '()
  "List of URLs for which `eww-readable' should not be used by default."
  :type '(repeat string)
  :group 'eww-extras)

(defcustom eww-extras-readable-exceptions-file
  (file-name-concat paths-dir-dotemacs "etc/eww-readable-exceptions.txt")
  "File containing the URLs for which `eww-readable' should not be used by default."
  :type 'file
  :group 'eww-extras)

(defcustom eww-extras-chrome-data-dir
  (expand-file-name "~/Library/Application Support/Google/Chrome/")
  "The directory where Chrome data is stored."
  :type 'directory
  :group 'eww-extras)

(defcustom eww-extras-chrome-data-dir-copy
  (expand-file-name "~/.chrome-data/")
  "A copy of the directory where Chrome data is stored.
A headless Chrome session will fail to authenticate if Chrome is running,
because the database will be locked. So we make a copy of the relevant
directory by running `eww-extras-chrome-copy-data-dir'."
  :type 'directory
  :group 'eww-extras)

(defcustom eww-extras-rsync-command
  "rsync -av '%s' '%s'"
  "The `rsync' command to make a copy of the Chrome data directory.
The placeholders `%s' are replaced by with the source and destination
directories."
  :type 'string
  :group 'eww-extras)

;;;; Functions

(defvar ebib--cur-db)
(declare-function bibtex-extras-get-key "bibtex-extras")
(declare-function ebib-extras-get-field "ebib-extras")
(declare-function ebib-db-get-filename "ebib-db")
(declare-function org-web-tools-extras-org-title-for-url "org-web-tools-extras")
(defun eww-extras-url-to-file (type &optional url callback)
  "Generate file of TYPE for URL and run CALLBACK function.
CALLBACK is a function called when the process concludes. The function takes two
arguments: the file to attach and the BibTeX key of the entry from which this
function was called, if any."
  (let* ((url (simple-extras-get-url url))
	 (bibtex-key (pcase major-mode
		       ('bibtex-mode (bibtex-extras-get-key))
		       ((or 'ebib-entry-mode 'ebib-index-mode)
			(ebib-extras-get-field "=key="))))
	 (title (pcase major-mode
		  ((or 'bibtex-mode 'ebib-entry-mode 'ebib-index-mode) bibtex-key)
		  (_ (pcase type
		       ("pdf" (buffer-name))
		       ("html" (simple-extras-slugify (org-web-tools-extras-org-title-for-url url)))))))
	 (file-name (file-name-with-extension title type))
	 (output-file (file-name-concat paths-dir-downloads file-name))
	 (process (make-process
		   :name (format "url-to-%s" type)
		   :buffer "*URL-to-File-Process*"
		   :command (list shell-file-name shell-command-switch
				  (format
				   (pcase type ("pdf" eww-extras-convert-to-pdf) ("html" eww-extras-convert-to-html))
				   browse-url-chrome-program eww-extras-chrome-data-dir-copy url output-file)))))
    (message "Getting %s file..." type)
    (set-process-sentinel process
			  (lambda (_proc event)
			    (if (string= event "finished\n")
				(progn
				  (message "File downloaded.")
				  (eww-extras-run-callback callback output-file bibtex-key))
			      (user-error "Could not get file"))))))

(defun eww-extras-run-callback (callback file key)
  "When CALLBACK is non-nil, run it with FILE and KEY as arguments.
FILE is the file to attach and KEY is the BibTeX key of the associated entry."
  (when callback
    (funcall callback file key)))

(defun eww-extras-chrome-copy-data-dir ()
  "Make a copy of the Chrome data directory.
This command needs to be run to make an initial copy of the Chrome data
directory, and then every once in a while to keep the directory updated. The
initial copy may take a while if the data directory is very big, but subsequent
updates should be fast."
  (interactive)
  (when (y-or-n-p "Make sure you closed all instances of Chrome and ensured that `eww-extras-chrome-data-dir' and `eww-extras-chrome-data-dir-copy' point to the right directories. Also, if you are running this command for the first time—i.e. if there is currently no copy of the Chrome data directory in your system—note that Emacs will become unresponsive for a few minutes. Proceed? ")
    (shell-command (format eww-extras-rsync-command
			   eww-extras-chrome-data-dir
			   eww-extras-chrome-data-dir-copy))))

(defun eww-extras-url-to-html (&optional url callback)
  "Generate HTML of URL, then run CALLBACK function."
  (interactive)
  (eww-extras-url-to-file "html" url callback))

(defun eww-extras-url-to-pdf (&optional url callback)
  "Generate PDF of URL, then run CALLBACK function."
  (interactive)
  (eww-extras-url-to-file "pdf" url callback))

(defun eww-extras-readable-autoview ()
  "Display the \"readable\" parts of the current web page by default.
The exceptions are listed in `eww-extras-readable-exceptions'."
  (let* ((current-url (eww-current-url))
	 (exception
	  (catch 'exception
	    (dolist (url eww-extras-readable-exceptions)
	      (when (string-match-p url current-url)
		(throw 'exception t))))))
    (unless (or exception
		;; if `:source' is nil, `eww-readable' will throw an error
		(not (plist-get eww-data :source)))
      (eww-readable))))

(add-hook 'eww-after-render-hook #'eww-extras-readable-autoview)

(declare-function ffap-url-p "ffap")
(declare-function browse-url-extras-write-url-to-file "browse-url-extras")
(defun eww-extras-add-domain-to-readable-exceptions ()
  "Prompt for a URL and add its domain to the list of `eww-readable' exceptions.
If buffer is visiting a URL or if there is a URL in the kill ring, use its
domain as the initial prompt input."
  (interactive)
  (let* ((url (or (eww-current-url) (ffap-url-p (current-kill 0))))
	 (domain (when url (url-domain (url-generic-parse-url url))))
	 (file eww-extras-readable-exceptions-file)
	 (selection (read-string (format "Add to `%s': " (file-name-nondirectory file)) domain)))
    (browse-url-extras-write-url-to-file selection file)
    (eww-extras-set-readable-exceptions-from-file)
    (eww-reload)))

(defun eww-extras-set-readable-exceptions-from-file ()
  "Set `eww-readable' exceptions from file of exception URLs."
  (when (file-exists-p eww-extras-readable-exceptions-file)
    (with-temp-buffer
      (insert-file-contents eww-extras-readable-exceptions-file)
      (setq eww-extras-readable-exceptions (split-string (regexp-quote (buffer-string)) "\n" t)))))

(eww-extras-set-readable-exceptions-from-file)

;; The following four commands copied from
;; github.com/gopar/.emacs.d#eww
(defun eww-extras-edit-current-url (&optional arg)
  "Edit current URL or new search.
With prefix ARG is passed, open in new EWW buffer."
  (interactive)
  (let* ((url (eww-copy-page-url))
	 (uris (eww-suggested-uris)))
    (setq url (read-string "Edit URL or new search: " url 'eww-promt-history uris))
    (setq url (eww--dwim-expand-url url))
    (eww url (if arg 4 nil))))

(defun eww-extras-open-with-recent-kill-ring (&optional arg)
  "Open current EWW with most recent item in kill ring.
With prefix ARG is passed, open in new EWW buffer."
  (interactive "P")
  (if arg
      (with-current-buffer
	  (if (derived-mode-p 'eww-mode) (clone-buffer)
	    (generate-new-buffer "*eww*"))
	(eww-mode)
	(eww (current-kill 0)))
    (eww (current-kill 0))))

(declare-function s-split "s")
(declare-function s-join "s")
(defun eww-extras-go-up-url-hierarchy ()
  "Go up the URL hierarchy."
  (interactive)
  (let* ((url (url-generic-parse-url (eww-current-url)))
	 (filepath (url-filename url))
	 (paths (s-split "/" filepath))
	 (new-path (s-join "/" (butlast paths 1)))
	 (new-url nil))
    (setq new-url (url-parse-make-urlobj
		   (url-type url)
		   (url-user url)
		   (url-password url)
		   (url-host url)
		   (url-port url)
		   new-path
		   (url-target url)
		   nil
		   (url-fullness url)))
    (eww-browse-url (url-recreate-url new-url))))

(defun eww-extras-go-to-root-url-hierarchy ()
  "Go to root of current URL hierarchy."
  (interactive)
  (let* ((url (url-generic-parse-url (eww-current-url)))
	 (new-url nil))
    (setq new-url (url-parse-make-urlobj
		   (url-type url)
		   (url-user url)
		   (url-password url)
		   (url-host url)
		   (url-port url)
		   ""
		   (url-target url)
		   nil
		   (url-fullness url)))
    (eww-browse-url (url-recreate-url new-url))))

(declare-function elfeed-tube-fetch "elfeed-tube")
(declare-function macos-open-app "macos")
(declare-function macos-app-is-open-p "macos")
(defun eww-extras-browse-youtube (url &optional player)
  "For YouTube URLs, show its transcript and open video with PLAYER.
If PLAYER is nil, default to `mpv'."
  (when (string-match "youtube.com" url)
    (let ((player (or player "mpv")))
      (kill-buffer)
      (unless (macos-app-is-open-p player)
	(macos-open-app player 'background))
      (elfeed-tube-fetch url))))

(defun eww-extras-get-url-in-link (title)
  "Return the URL of the link whose title is TITLE."
  (interactive)
  (let (found-url)
    (save-excursion
      (goto-char (point-min))
      (while (and (not found-url) (not (eobp)))
	(when-let* ((url (get-text-property (point) 'shr-url))
		    (link-title (buffer-substring-no-properties
				 (point)
				 (or (next-single-property-change (point) 'shr-url)
				     (point-max)))))
	  (when (string-match-p (regexp-quote title) link-title)
	    (setq found-url url)))
	(goto-char (or (next-single-property-change (point) 'shr-url)
		       (point-max)))))
    found-url))

(declare-function zotra-extras-add-entry "zotra-extras")
(defun eww-extras-add-entry ()
  "Add current URL to bibfile and generate associated PDF and HTML files."
  (interactive)
  (when (derived-mode-p 'eww-mode)
    (zotra-extras-add-entry (plist-get eww-data :url))))

(defun eww-extras-browse-file (&optional file)
  "Browse File in `eww'.
If FILE is nil, use the file at point, the file visited by the current buffer,
or prompt the user for a file."
  (interactive)
  (let ((file (or file
		  (thing-at-point 'filename)
		  (buffer-file-name)
		  (read-file-name "File: " nil nil t))))
    (if (file-exists-p file)
	(eww-open-file file)
      (user-error "No file found at point"))))

;; TODO: move the section below to separate package, like I did with `scihub'
;;;;;; Get buffer elements

(defun eww-extras-collect-links-in-buffer ()
  "Get an alist of link titles and URLs for all links in the current `eww' buffer."
  (save-excursion
    (goto-char (point-min))
    (let (beg end candidates)
      (setq end
	    (if (get-text-property (point) 'shr-url)
		(point)
	      (text-property-any (point) (point-max) 'shr-url nil)))
      (while (setq beg (text-property-not-all end (point-max) 'shr-url nil))
	(goto-char beg)
	;; Skip newlines which might precede the link text
	(skip-chars-forward "\n")
	(setq beg (point))
	(if (get-text-property (point) 'shr-url)
	    (progn
	      (setq end (next-single-property-change (point) 'shr-url nil (point-max)))
	      ;; Handle when link is at the end of the buffer
	      (if (eq end nil)
		  (setq end (point-max)))
	      (push (cons (buffer-substring-no-properties beg end) (get-text-property beg 'shr-url))
		    candidates))
	  (setq end (next-single-property-change (point) 'shr-url)))
	(goto-char (max end (1+ (point)))))  ;; ensure progress by moving at least one character forward
      (nreverse candidates))))

;;;;;; Anna's Archive

(defun eww-extras-annas-archive-download (&optional string confirm callback)
  "Search Anna’s Archive for STRING and download the selected item.
If STRING is nil, prompt for a search string. If both STRING and CONFIRM are
non-nil, prompt the user for confirmation to use STRING as the search string.

CALLBACK is a function called when the process concludes. The function takes two
arguments: the file to attach and the BibTeX key of the entry from which this
function was called, if any.

Requires a paid subscription and authentication. (Yes, you can authenticate with
eww!)"
  (interactive)
  (save-window-excursion
    (let* ((string (cond ((and string confirm)
			  (read-string "Search string: " string))
			 (string string)
			 (t (read-string "Search string: "))))
	   (url (format "%ssearch?index=&page=1&q=%s&ext=pdf&sort="
			eww-extras-annas-archive-home-url string)))
      (when callback (setq eww-extras-annas-archive-callback callback))
      (add-hook 'eww-after-render-hook #'eww-extras-annas-archive-select-and-open-url)
      (eww url))))

(defun eww-extras-annas-archive-select-and-open-url ()
  "Get the download URLs from the Annas Archive search results buffer."
  (remove-hook 'eww-after-render-hook #'eww-extras-annas-archive-select-and-open-url)
  (save-window-excursion
    (let (links)
      (dolist (cons (eww-extras-collect-links-in-buffer))
	(when (string-match-p "\\.pdf" (car cons))
	  (push cons links)))
      (if links
	  (let* ((selection (completing-read "Select a link: " links nil t))
		 (url (alist-get selection links nil nil 'string=)))
	    (add-hook 'eww-after-render-hook #'eww-extras-annas-archive-proceed-to-download-page)
	    (eww url))
	(message "No results found.")))))

(defun eww-extras-annas-archive-proceed-to-download-page ()
  "Proceed to the Annas Archive download page."
  (remove-hook 'eww-after-render-hook #'eww-extras-annas-archive-proceed-to-download-page)
  (save-window-excursion
    (let ((url (eww-extras-get-url-in-link "Fast Partner Server")))
      (add-hook 'eww-after-render-hook #'eww-extras-annas-archive-download-file)
      (eww url))))

(defvar ebib-extras-attach-file-key)
(defun eww-extras-annas-archive-download-file ()
  "Download the file from the Annas Archive download page."
  (remove-hook 'eww-after-render-hook 'eww-extras-annas-archive-download-file)
  (let* ((bibtex-key ebib-extras-attach-file-key)
	 (url (eww-extras-annas-archive-get-download-url))
	 (raw-file (file-name-nondirectory url))
	 (sans-extension (file-name-sans-extension raw-file))
	 (extension (file-name-extension raw-file))
	 (filename (file-name-with-extension (substring sans-extension 0 100) extension))
	 (final-path (file-name-concat paths-dir-downloads filename))
	 (temp-path (file-name-with-extension final-path ".tmp"))
	 (callback eww-extras-annas-archive-callback))
    (setq eww-extras-annas-archive-callback nil)
    (let ((process (start-process "download-file" "*download-output*" "curl" "-o" temp-path "-L" url)))
      (set-process-sentinel process
			    (lambda (_proc event)
			      (cond ((string= event "finished\n")
				     (rename-file temp-path final-path 'ok-if-already-exists)
				     (message "Downloaded `%s' to `%s`" filename final-path)
				     (eww-extras-run-callback callback final-path bibtex-key))
				    ((string-prefix-p "exited abnormally" event)
				     (let ((error-code (string-match "code \\([0-9]+\\)" event)))
				       (message "Download failed with error code %s for `%s`"
						(match-string 1 event) filename)))
				    (t
				     (message "Unexpected process event: %s" event))))))
    (message "Downloading `%s'..." filename)))

(defun eww-extras-annas-archive-get-download-url ()
  "Get the download URL from the Annas Archive download page."
  (or (eww-extras-get-url-in-link "Download now")
      (let ((generic-error "Could not find download link")
	    (quota-exceeded "You’ve run out of fast downloads today"))
	(goto-char (point-min))
	(user-error (cond ((re-search-forward quota-exceeded nil t)
			   quota-exceeded)
			  (t generic-error))))))

;;;;;;; Authentication

(defun eww-extras-annas-archive-authenticate ()
  "Authenticate with Anna’s Archive."
  (interactive)
  (save-window-excursion
    (add-hook 'eww-after-render-hook #'eww-extras-annas-archive-get-authentication-details)
    (eww eww-extras-annas-archive-auth-url)))

(defun eww-extras-annas-archive-get-authentication-details ()
  "Return user authentication details from Anna’s Archive."
  (remove-hook 'eww-after-render-hook #'eww-extras-annas-archive-get-authentication-details)
  (let (id key)
    (goto-char (point-min))
    (re-search-forward "Account ID: \\(.*\\)" nil t)
    (setq id (match-string 1))
    (re-search-forward "Secret key (don’t share!): show\\(.*\\)" nil t)
    (setq key (match-string 1))
    (if (and id key)
	(message "You are authenticated.\nAccount ID: %s\nSecret key: %s" id key)
      (eww eww-extras-annas-archive-auth-url)
      (message "You don't seem to be authenticated. Please enter your key in the `eww' buffer."))))

(provide 'eww-extras)

;;; eww-extras.el ends here
