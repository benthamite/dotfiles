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

(require 'browse-url-extras)
(require 'eww)
(require 'prot-eww)
(require 'ffap)
(require 'f)
(require 'org-web-tools-extras)
(require 'paths)
(require 'simple-extras)

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

;;;; Functions

(defun eww-extras-url-to-file (type &optional url)
  "Generate file of TYPE for URL."
  (let* ((url (simple-extras-get-url url))
	 (title (pcase type
		  ("pdf" (or (pcase major-mode
			       ('bibtex-mode (bibtex-extras-get-field "title"))
			       ((or 'ebib-entry-mode 'ebib-index-mode) (ebib-extras-get-field "title")))
			     (buffer-name)))
		  ("html" (org-web-tools-extras-org-title-for-url url))))
	 (slug (prot-eww--sluggify title))
	 (file-name (file-name-with-extension slug type))
	 (output-file (file-name-concat paths-dir-downloads file-name)))
    (async-shell-command
     (format
      (pcase type
	("pdf" "'%s' --print-to-pdf --no-pdf-header-footer --headless %s --print-to-pdf='%s'")
	("html" "'%s' --headless --dump-dom '%s' > %s"))
      browse-url-chrome-program url output-file))))

(defun eww-extras-url-to-html (&optional url)
  "Generate HTML of URL."
  (interactive)
  (eww-extras-url-to-file "html" url))

(defun eww-extras-url-to-pdf (&optional url)
  "Generate PDF of URL."
  (interactive)
  (eww-extras-url-to-file "pdf" url))

(defun eww-extras-readable-autoview ()
  "Display the \"readable\" parts of the current web page by default.
The exceptions are listed in `eww-extras-readable-exceptions'."
  (let* ((current-url (eww-current-url))
	 (exception
	  (catch 'exception
	    (dolist (url eww-extras-readable-exceptions)
	      (when (string-match-p url current-url)
		(throw 'exception t))))))
    (unless exception
      (eww-readable))))

(add-hook 'eww-after-render-hook #'eww-extras-readable-autoview)

(defun eww-extras-add-domain-to-readable-exceptions ()
  "Prompt for a URL and add its domain to the list of `eww-readable' exceptions.
If buffer is visiting a URL or if there is a URL in the kill ring, use its
domain as the initial prompt input."
  (interactive)
  (let* ((url (or (eww-current-url) (ffap-url-p (current-kill 0))))
	 (domain (when url (url-domain (url-generic-parse-url url))))
	 (file eww-extras-readable-exceptions-file)
	 (selection (read-string (format "Add to `%s': " (file-name-nondirectory file)) domain)))
    (browse-url-extras--write-url-to-file selection file)
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

(provide 'eww-extras)

;;; eww-extras.el ends here
