;;; elfeed-extras.el --- Extensions for elfeed -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/elfeed-extras.el
;; Version: 0.2
;; Package-Requires: ((elfeed))

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

(declare-function zotra-extras-add-entry "zotra-extras")
(defun elfeed-extras-add-entry ()
  "Add current URL to bibfile and generate associated PDF and HTML files."
  (interactive)
  (when (derived-mode-p 'elfeed-show-mode)
    (zotra-extras-add-entry (elfeed-entry-link elfeed-show-entry))))

(defun elfeed-extras-jump-to-next-link ()
  "Jump to next link after point in the current entry."
  (interactive)
  (elfeed-show-next-link)
  (recenter))

(defun elfeed-extras-update ()
  "Update all feeds in `elfeed-feeds'.
Unlike `elfeed-update', this function will update the database even if `ellfeed'
isn’t open."
  ;; Ensure database is loaded
  (elfeed-db-ensure)
  (with-current-buffer (elfeed-search-buffer)
    (unless (eq major-mode 'elfeed-search-mode)
      (elfeed-search-mode)))
  ;; Run the standard update process
  (elfeed-log 'info "Elfeed update: %s"
              (format-time-string "%B %e %Y %H:%M:%S %Z"))
  (let ((elfeed--inhibit-update-init-hooks nil))
    (run-hooks 'elfeed-update-init-hooks)
    (mapc #'elfeed-update-feed (elfeed--shuffle (elfeed-feed-list))))
  ;; Save the database when complete
  (elfeed-db-save)
  (message "Elfeed update started in background."))

(defun elfeed-extras-disable-undo ()
  "Disable undo in the *elfeed-search* buffer."
  (when (eq major-mode 'elfeed-search-mode)
    (setq-local buffer-undo-list nil)))

;;;;; elfeed ‘follow mode’

;; this implements an analogy of `org-agenda-follow-mode': as point is moved
;; across the `elfeed' search buffer, the corresponding entry is shown in the
;; other window
(defun elfeed-extras-display-buffer (buffer)
  "Display BUFFER in the other window, without focusing on it."
  (pop-to-buffer buffer 'window-extras-switch-to-last-window))

(defun elfeed-extras-follow-entry (lines)
  "Move LINES down and display the corresponding entry in the other window."
  (forward-line lines)
  (recenter)
  (call-interactively #'elfeed-search-show-entry)
  (select-window (previous-window))
  (unless elfeed-search-remain-on-entry (forward-line -1)))

(defun elfeed-extras-follow-next ()
  "Move point to the next entry and display it in the other window."
  (interactive)
  (elfeed-extras-follow-entry 1))

(defun elfeed-extras-follow-previous ()
  "Move point to the previous entry and display it in the other window."
  (interactive)
  (elfeed-extras-follow-entry -1))

(provide 'elfeed-extras)
;;; elfeed-extras.el ends here

