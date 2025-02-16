;;; browse-url-extras.el --- Extensions for browse-url -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/browse-url-extras.el
;; Version: 0.2
;; Package-Requires: ((paths "0.1") (f "0.10.0") (s "1.5.0"))

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

;; Extensions for `browse-url'.

;;; Code:

(require 'browse-url)
(require 'eww)
(require 'paths)

;;;; User options

(defgroup browse-url-extras ()
  "Extensions for `browse-url'."
  :group 'browse-url)

(defcustom browse-url-extras-browse-url-default-file
  (file-name-concat paths-dir-dotemacs "etc/browse-url-default.txt")
  "Path to the `browse-url-default.txt' file."
  :type 'file
  :group 'browse-url-extras)

(defcustom browse-url-extras-browse-url-firefox-file
  (file-name-concat paths-dir-dotemacs "etc/browse-url-firefox.txt")
  "Path to the `browse-url-firefox.txt' file."
  :type 'file
  :group 'browse-url-extras)

;;;; Functions

(autoload 'f-read "f")
(autoload 's-split "s")
(defun browse-url-extras-set-handler (urls-file handler)
  "Set the URL HANDLER from a URLS-FILE."
  (dolist (url (s-split "\n" (f-read urls-file) t))
    (push (cons (regexp-quote url) handler) browse-url-handlers)))

(defun browse-url-extras-set-domains-to-open-externally ()
  "Set the domains to open externally.
Read the files `browse-url-extras-browse-url-default-file' and
`browse-url-extras-browse-url-firefox-file', and set `browse-url-handlers' to
open the domains in those files with the default browser and with Firefox,
respectively."
  (interactive)
  (setq browse-url-handlers '())
  (browse-url-extras-set-handler browse-url-extras-browse-url-default-file 'browse-url-default-browser)
  (browse-url-extras-set-handler browse-url-extras-browse-url-firefox-file 'browse-url-firefox))

;;;###autoload
(defun browse-url-extras-of-dired-file-externally ()
  "In Dired, open the file at point in an external browser."
  (interactive)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (browse-url-of-dired-file)))

(declare-function ffap-url-p "f")
(defun browse-url-extras-add-domain-to-open-externally (arg)
  "Prompt for a URL and add its domain to the list of URLs to open externally.
If buffer is visiting a URL or if there is a URL in the kill ring,
use its domain as the initial prompt input.

By default, the command adds the URL to the list of domains to open with the
deafult browser. If called with prefix argument ARG, add the domain to the list
of URLs to open in Firefox instead."
  (interactive "P")
  (let* ((url (or (eww-current-url) (ffap-url-p (current-kill 0))))
         (domain (when url (url-domain (url-generic-parse-url url))))
         (file (if arg
                   browse-url-extras-browse-url-firefox-file
                 browse-url-extras-browse-url-default-file))
         (selection (read-string (format "Add to `%s': " (file-name-nondirectory file)) domain)))
    (browse-url-extras-write-url-to-file selection file)
    (browse-url-extras-set-domains-to-open-externally)
    (browse-url url)))

(defun browse-url-extras-write-url-to-file (url file)
  "Write URL to FILE containing list of URLs to open externally."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-max))
    (insert (format "\n%s" url))
    (goto-char (point-min))
    (flush-lines "^$")
    (delete-duplicate-lines (point-min) (point-max))
    (sort-lines nil (point-min) (point-max))
    (save-buffer)))

(browse-url-extras-set-domains-to-open-externally)

(defun browse-url-open-externally-in-background (url)
  "Open URL externally in the background.
If URL is nil, prompt for a URL."
  (interactive "sURL: ")
  (start-process (concat "open " url) nil "open" "-a" "Firefox" "--background" url))

(provide 'browse-url-extras)
;;; browse-url-extras.el ends here

