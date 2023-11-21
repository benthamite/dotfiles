;;; browse-url-extras.el --- Extensions for browse-url -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/browse-url-extras.el
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

;; Extensions for `browse-url'.

;;; Code:

(require 'browse-url)
(require 'paths)

;;;; User options

(defgroup browse-url-extras ()
  "Extensions for `browse-url'."
  :group 'browse-url)

(defcustom browse-url-extras-urls-default-file
  (file-name-concat paths-dir-dotemacs "etc/urls-default.txt")
  "Path to the `urls-default.txt' file."
  :type 'file
  :group 'path)

(defcustom browse-url-extras-urls-firefox-file
  (file-name-concat paths-dir-dotemacs "etc/urls-firefox.txt")
  "Path to the `urls-firefox.txt' file."
  :type 'file
  :group 'path)

;;;; Functions

(defun browse-url-extras-set-domains-to-open-externally ()
  "Set the domains to open externally.
Read the files `browse-url-extras-urls-default-file' and
`browse-url-extras-urls-firefox-file', and set `browse-url-handlers' to open
the domains in those files with the default browser and with Firefox,
respectively."
  (require 'f)
  (require 's)
  (dolist (url (s-split "\n" (f-read browse-url-extras-urls-default-file) t))
    (push (cons (regexp-quote url) 'browse-url-default-browser) browse-url-handlers))
  (dolist (url (s-split "\n" (f-read browse-url-extras-urls-firefox-file) t))
    (push (cons (regexp-quote url) 'browse-url-firefox) browse-url-handlers)))

(defun browse-url-extras-of-dired-file-externally ()
  "In Dired, open the file at point in an external browser."
  (interactive)
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (browse-url-of-dired-file)))

(defun browse-url-extras-add-domain-to-open-externally (arg)
  "Prompt for a URL and add its domain to the list of URLs to open externally.
If buffer is visiting a URL or if there is a URL in the kill ring,
use its domain as the initial prompt input.

If called with prefix argument ARG, add the domain to the list of URLs to open
in Firefox."
  (interactive "P")
  (require 'eww)
  (require 'f)
  (let* ((url (or (eww-current-url) (ffap-url-p (current-kill 0))))
         (domain (when url (url-domain (url-generic-parse-url url))))
         (file (if arg
                   browse-url-extras-urls-firefox-file
                 browse-url-extras-urls-default-file))
         (selection (read-string (format "Add to `%s': " (file-name-nondirectory file)) domain)))
    (browse-url-extras--write-url-to-file selection file)
    (browse-url-extras-set-domains-to-open-externally)
    (browse-url url)))

(defun browse-url-extras--write-url-to-file (url file)
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

(provide 'browse-url-extras)
;;; browse-url-extras.el ends here

