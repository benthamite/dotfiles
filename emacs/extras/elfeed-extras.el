;;; elfeed-extras.el --- Extensions for elfeed -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/elfeed-extras.el
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

;; Extensions for `elfeed'.

;;; Code:

(require 'elfeed)
(require 'elfeed-show)

;;;; Variables

(defvar elfeed-extras-update-timer nil
  "Timer for updating elfeed.")

;;;; Functions

;; Borrowed from Prot
(defun elfeed-extras-show-visit-in-eww (&optional link)
  "Browse current entry's link or optional LINK in `eww'.
Only show the readable part once the website loads. This can fail on
poorly-designed websites."
  (interactive)
  (let* ((entry (if (derived-mode-p 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (or link (elfeed-entry-link entry))))
    (eww link)
    (add-hook 'eww-after-render-hook 'eww-readable nil t)))

(defun elfeed-extras-mark-all-as-read ()
  "Mark all entries as read."
  (interactive)
  (call-interactively 'mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun elfeed-extras-kill-link-url-of-entry ()
  "Add link of current entry to kill ring."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (message "Copied link: %s" link)
      (kill-new link))))

(defun elfeed-extras-filter-tags (tags)
  "Restrict entries to those with TAGS, or to all entries if already restricted."
  (elfeed-search-set-filter tags)
  (if (string= tags "")
      (message "Showing everything")
    (message (concat "Showing " tags))))

(defvar elfeed-extras-toggle-read-entries t)
(defun elfeed-extras-toggle-read-entries ()
  "Toggle between showing and hiding read entries."
  (interactive)
  (if elfeed-extras-toggle-read-entries
      (elfeed-extras-filter-tags "")
    (elfeed-extras-filter-tags "+unread"))
  (setq elfeed-extras-toggle-read-entries (not elfeed-extras-toggle-read-entries)))

(defvar elfeed-extras-toggle-wiki-entries t)
(defun elfeed-extras-toggle-wiki-entries ()
  "Toggle between showing all, or only `wiki', unread entries."
  (interactive)
  (if elfeed-extras-toggle-wiki-entries
      (elfeed-extras-filter-tags "+unread +wiki")
    (elfeed-extras-filter-tags "+unread -wiki"))
  (setq elfeed-extras-toggle-wiki-entries (not elfeed-extras-toggle-wiki-entries)))

(declare-function zotra-extras-url-full-capture "zotra-extras")
(defun elfeed-extras-url-full-capture ()
  "Add current URL to bibfile and generate associated PDF and HTML files."
  (interactive)
  (when (derived-mode-p 'elfeed-show-mode)
    (zotra-extras-url-full-capture (elfeed-entry-link elfeed-show-entry))))

(defun elfeed-extras-auto-update ()
  "Automatically update `elfeed' every 15 minutes of idleness."
  (cancel-timer elfeed-extras-update-timer)
  (setq elfeed-extras-update-timer
	(run-with-idle-timer (* 15 60) t #'elfeed-update)))

(provide 'elfeed-extras)
;;; elfeed-extras.el ends here

