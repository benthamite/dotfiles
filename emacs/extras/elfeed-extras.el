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
(require 'paths)
(require 'elfeed-show)

;;;; Functions

;; Borrowed from Prot
(defun elfeed-extras-show-visit-in-eww (&optional link)
  "Browse current entry's link or optional LINK in `eww'.
Only show the readable part once the website loads. This can fail on
poorly-designed websites."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
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

(defun elfeed-extras-full-update ()
  "*Really* update feeds!"
  (interactive)
  (require 'elfeed-org)
  (let ((elfeed-search-buffer "*elfeed-search*"))
    (when (and (get-buffer elfeed-search-buffer)
               (not (equal (buffer-name) elfeed-search-buffer)))
      (kill-buffer elfeed-search-buffer)))
  (elfeed-org)
  (elfeed-unjam)
  (elfeed-update))

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

(defun elfeed-extras-toggle-fixed-pitch ()
  "Toggle between fixed pitch and variable pitch."
  (interactive)
  (if shr-use-fonts
      (setq shr-use-fonts nil)
    (setq shr-use-fonts t))
  (elfeed-show-refresh))

;;;###autoload
(defun elfeed-extras-toggle-session ()
  "Start or end an `elfeed' session."
  (interactive)
  (if (or
       (equal major-mode 'elfeed-search-mode)
       (equal major-mode 'elfeed-show-mode))
      (progn
        (kill-matching-buffers "^\*elfeed\-*\*" nil t))
    (elfeed)
    (when (< elfeed-search-last-update
             (time-to-seconds (time-subtract (current-time) (seconds-to-time (* 60 60 2)))))
      (elfeed-update))))

;; This only works in Firefox due to a Chrome limitation
;; xenodium.com/open-emacs-elfeed-links-in-background/
(defun elfeed-extras-search-browse-background-url ()
  "Open current `elfeed' entry (or region entries) in browser without losing focus."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (mapc (lambda (entry)
            (cl-assert (memq system-type '(darwin)) t "open command is macOS only")
            (start-process (concat "open " (elfeed-entry-link entry))
                           nil "open" "--background" (elfeed-entry-link entry))
            (elfeed-untag entry 'unread)
            (elfeed-search-update-entry entry))
          entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))

(defun elfeed-extras-url-full-capture ()
  "Add current URL to bibfile and generate associated PDF and HTML files."
  (interactive)
  (when (eq major-mode 'elfeed-show-mode)
    (zotra-extras-url-full-capture (elfeed-entry-link elfeed-show-entry))))

;;;;; Performance improvemenets

;; Everything in this section copied verbatim from github.com/skeeto/elfeed/issues/293

(defvar elfeed-extras-update-complete-hook nil
  "Functions called with no arguments when `elfeed-update' is finished.")

(defvar elfeed-extras-updates-in-progress 0
  "Number of feed updates in-progress.")

(defvar elfeed-extras-search-update-filter nil
  "The filter when `elfeed-update' is called.")

(defun elfeed-extras-update-complete-hook (&rest ignore)
  "When update queue is empty, run `elfeed-extras-update-complete-hook' functions."
  (when (= 0 elfeed-extras-updates-in-progress)
    (run-hooks 'elfeed-extras-update-complete-hook)))

(add-hook 'elfeed-update-hooks #'elfeed-extras-update-complete-hook)

(defun elfeed-extras-update-message-completed (&rest _ignore)
  (message "Feeds updated"))

(add-hook 'elfeed-extras-update-complete-hook #'elfeed-extras-update-message-completed)

(defun elfeed-extras-search-update-restore-filter (&rest ignore)
  "Restore filter after feeds update."
  (when elfeed-extras-search-update-filter
    (elfeed-search-set-filter elfeed-extras-search-update-filter)
    (setq elfeed-extras-search-update-filter nil)))

(add-hook 'elfeed-extras-update-complete-hook #'elfeed-extras-search-update-restore-filter)

(defun elfeed-extras-search-update-save-filter (&rest ignore)
  "Save and change the filter while updating."
  (setq elfeed-extras-search-update-filter elfeed-search-filter)
  (setq elfeed-search-filter "#0"))

;; NOTE: It would be better if this hook were run before starting the feed updates, but in
;; `elfeed-update', it happens afterward.
(add-hook 'elfeed-update-init-hooks #'elfeed-extras-search-update-save-filter)

(defun elfeed-extras-update-counter-inc (&rest ignore)
  (cl-incf elfeed-extras-updates-in-progress))

(advice-add #'elfeed-update-feed :before #'elfeed-extras-update-counter-inc)

(defun elfeed-extras-update-counter-dec (&rest ignore)
  (cl-decf elfeed-extras-updates-in-progress)
  (when (< elfeed-extras-updates-in-progress 0)
    ;; Just in case
    (setq elfeed-extras-updates-in-progress 0)))

(add-hook 'elfeed-update-hooks #'elfeed-extras-update-counter-dec)

(provide 'elfeed-extras)
;;; elfeed-extras.el ends here

