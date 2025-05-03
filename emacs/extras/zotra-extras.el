;;; zotra-extras.el --- Extensions for zotra -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/zotra-extras.el
;; Version: 0.2
;; Package-Requires: ((paths "0.1") (zotra "1.0"))

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

(defgroup zotra-extras ()
  "Extensions for `zotra'."
  :group 'zotra)

(defcustom zotra-extras-use-mullvad-p nil
  "If non-nil, connect via Mullvad temporarily when fetching data for IMDb URLs.
When adding an entry using `zotra-extras-add-entry', if this option is non-nil
*and* the URL or search string provided matches \"imdb.com\", attempt to connect
to the Mullvad VPN server associated with the city defined for \"IMDb\" in
`mullvad-websites-and-cities'.

The Internet Movie Database (IMDb) may return data based on the geographic
location of the request. Connecting via a specific Mullvad server (e.g., New
York for English results) can help control the language or region of the fetched
data.

This option requires the `mullvad' package, and the user options
`mullvad-websites-and-cities' and `mullvad-cities-and-servers' to be set
accordingly. Specifically, `mullvad-websites-and-cities' must include a cons
cell whose car is `\"IMDb\"' and whose cdr is the city you want `mullvad' to
connect to (e.g. `(\"IMDb\" . \"New York\")'), and `mullvad-cities-and-servers'
must include a cons cell whose car is this city and whose cdr is a Mullvad
server for this city (e.g. `(\"New York\" . \"us-nyc-wg-601\")'). Here is a
sample configuration:

\(setq mullvad-cities-and-servers
      \\='((\"London\" . \"gb-lon-wg-001\")
	(\"Madrid\" . \"es-mad-wg-101\")
	(\"Malmö\" . \"se-sto-wg-001\")
	(\"Frankfurt\" . \"de-fra-wg-001\")
	(\"New York\" . \"us-nyc-wg-601\")
	(\"San José\" . \"us-sjc-wg-001\")
	(\"São Paulo\" . \"br-sao-wg-001\")))

\(setq mullvad-websites-and-cities
      \\='((\"Betfair\" . \"London\")
	 (\"Criterion Channel\" . \"New York\")
	 (\"Gemini\" . \"New York\")
	 (\"HathiTrust\" . \"San José\")
	 (\"IMDb\" . \"New York\")
	 (\"Library Genesis\" . \"Malmö\")
	 (\"Pirate Bay\" . \"Malmö\")
	 (\"UC Berkeley\" . \"San José\")
	 (\"Wise\" . \"Madrid\")))

Refer to the `mullvad' package documentation for details."
  :type 'boolean
  :group 'zotra-extras)

(defvar zotra-extras-most-recent-bibfile nil
  "The bibfile of the most recently added entry.")

(defvar zotra-extras-most-recent-bibkey nil
  "The bibkey of the most recently added entry.")

(defconst zotra-extras-add-multiple-urls-from-file-filename
  (file-name-concat paths-dir-downloads "zotra-add-multiple-urls.txt")
  "Default file for `zotra-extras-add-multiple-urls-from-file'.")

;;;; Functions

(defvar ebib--cur-db)
(declare-function ebib "ebib")
(declare-function mullvad-connect-to-website "mullvad")
(autoload 'eww-copy-page-url "eww")
(autoload 'elfeed-extras-kill-link-url-of-entry "elfeed-extras")
;;;###autoload
(defun zotra-extras-add-entry (&optional url-or-search-string entry-format bibfile do-not-open)
  "Like `zotra-add-entry', but with various customizations.
Pass URL-OR-SEARCH-STRING and ENTRY-FORMAT to `zotra-get-entry' to get the
entry. BIBFILE is the file where the BibTeX entry should be saved; if nil,
prompt the user to select it. If DO-NOT-OPEN is non-nil, do not open the entry
in Ebib after adding it."
  (interactive)
  (let* ((bibfile (or bibfile
		      (setq zotra-extras-most-recent-bibfile (zotra-extras-set-bibfile))))
	 (url-or-search-string (or url-or-search-string
				   (read-string "URL or search string: " (ignore-errors (current-kill 0 t))))))
    (pcase major-mode
      ('elfeed-show-mode (elfeed-extras-kill-link-url-of-entry))
      ('eww-mode (eww-copy-page-url)))
   (when (and zotra-extras-use-mullvad-p
              (string-match-p "imdb\\.com" url-or-search-string))
     (message "IMDb URL detected and `zotra-extras-use-mullvad-p' is set. Connecting via Mullvad...")
     (mullvad-connect-to-website "IMDb" 1 'silently))
   (condition-case err
(zotra-extras--add-and-maybe-open url-or-search-string entry-format bibfile do-not-open)
      (error
       (if (string-match-p "JSON parse error: Internal Server Error" (error-message-string err))
	   (let ((zotra-backend 'citoid))
             (message "Request with main backend failed. Retrying with `citoid'...")
             (zotra-extras--add-and-maybe-open url-or-search-string entry-format bibfile do-not-open))
	 (signal (car err) (cdr err)))))))

(defun zotra-extras--add-and-maybe-open (url-or-search-string entry-format bibfile &optional do-not-open)
  "Add entry using `zotra-add-entry' and, by default, open it in Ebib.
Pass URL-OR-SEARCH-STRING and ENTRY-FORMAT to `zotra-get-entry' to get the
entry. BIBFILE is the file where the BibTeX entry should be saved. If
DO-NOT-OPEN is non-nil, do not open the entry in Ebib after adding it."
  (zotra-add-entry url-or-search-string entry-format bibfile)
  (unless do-not-open
    (zotra-extras-open-in-ebib zotra-extras-most-recent-bibkey)))

;;;;; Bibfile

(defvar tlon-file-fluid)
;;;###autoload
(defun zotra-extras-set-bibfile ()
  "Prompt the user to select a value for `org-cite-global-bibliography'."
  (completing-read "Bibfile" (list
                              tlon-file-fluid
                              paths-file-personal-bibliography-new)))

;;;;; Ebib

(autoload 'ebib-switch-to-database-nth "ebib")
(autoload 'ebib-save-current-database "ebib")
(autoload 'ebib--update-buffers "ebib")
(autoload 'ebib-extras-process-entry "ebib-extras")
(declare-function ebib-extras-open-key "ebib-extras")
(declare-function ebib-extras-sort "ebib-extras")
(declare-function ebib-extras-open-or-switch "ebib-extras")
(declare-function ebib-extras-get-db-number "ebib-extras")
(declare-function ebib-extras-reload-database-no-confirm "ebib-extras")
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
(declare-function bibtex-extras-get-key "bibtex-extras")
(declare-function tlon-cleanup-eaf-replace-urls "tlon-cleanup")
(autoload 'bibtex-extras-convert-titleaddon-to-journaltitle "bibtex-extras")
(defun zotra-extras-after-add-process-bibtex ()
  "Process newly added bibtex entry."
  ;; TODO: check that there are no unsaved changes in
  ;; `zotra-extras-most-recent-bibfile'
  (goto-char (point-max))
  (bibtex-extras-convert-titleaddon-to-journaltitle)
  (bibtex-set-field "timestamp" (format-time-string ebib-timestamp-format nil "GMT"))
  (zotra-extras-fix-octal-sequences)
  (org-ref-clean-bibtex-entry)
  (tlon-cleanup-eaf-replace-urls)
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

(defun zotra-extras-protocol (info)
  "Like `zotra-protocol' but with a call to `zotra-extras-add-entry'.
INFO is a plist with the following keys:
- `:url': URL of the page to be saved.
- `:bibfile': Bibfile where the entry should be saved.
- `:format': Format of the entry.
- `:title': Title of the page."
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

(defun zotra-extras-fetch-field (field url-or-search-string &optional ignore-errors timeout)
  "Get FIELD value in bibliographic entry for URL-OR-SEARCH-STRING.
If IGNORE-ERRORS is non-nil, handle error thrown by `zotra-get-entry-1'
gracefully. IF TIMEOUT is non-nil, give up after that many seconds; otherwise,
use the default."
  (let* ((query-result (zotra-query-url-or-search-string url-or-search-string))
	 (data (car query-result))
	 (endpoint (cdr query-result))
	 (zotra-url-retrieve-timeout (or timeout zotra-url-retrieve-timeout))
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
;;;###autoload
(defun zotra-extras-add-multiple-urls-from-file (file bibfile)
  "Prompt the user to select a FILE with a list of URLs and add them to BIBFILE."
  (interactive (list (read-file-name "File with URLs (one URL per line): " paths-dir-downloads
				     zotra-extras-add-multiple-urls-from-file-filename nil nil)))
  (let ((urls (delete-dups (files-extras-lines-to-list file))))
    (zotra-extras-add-multiple-urls urls bibfile)))

(defun zotra-extras-add-multiple-urls (urls bibfile)
  "Add URLS to BIBFILE."
  (let ((urls urls))
    (ebib-save-all-databases)
    (dolist (url urls)
      (message "Adding entry for %s..." url)
      (zotra-extras-add-entry url nil bibfile 'do-not-open))
    (ebib bibfile)
    (ebib-extras-sort 'Timestamp)))

(provide 'zotra-extras)
;;; zotra-extras.el ends here
