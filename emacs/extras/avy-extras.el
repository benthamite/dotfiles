;;; avy-extras.el --- Extensions for avy -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/avy-extras.el
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

;; Extensions for `avy'.

;;; Code:

(require 'avy)

;;;; Functions

(defun avy-extras-goto-word-in-line ()
  "Jump to a word start between start and end of visual line."
  (interactive)
  (avy-with avy-goto-word-0
    (avy-goto-word-0 nil
                     (save-excursion (beginning-of-visual-line))
                     (save-excursion (end-of-visual-line) (point)))))

(defun avy-extras-goto-word-in-line-behind ()
  "Jump to a word start between start of visual line and point."
  (interactive)
  (avy-with avy-goto-word-0
    (avy-goto-word-0 nil
                     (save-excursion (beginning-of-visual-line))
                     (point))))

(defun avy-extras-goto-word-in-line-ahead ()
  "Jump to a word start between point and end of visual line."
  (interactive)
  (avy-with avy-goto-word-0
    (avy-goto-word-0 nil
                     (point)
                     (save-excursion (end-of-visual-line) (point)))))

(defun avy-extras-goto-end-of-line-above (&optional offset bottom-up)
  "Goto visible end of line above the cursor.
OFFSET changes the distance between the closest key to the cursor and the
cursor. When BOTTOM-UP is non-nil, display avy candidates from top to bottom."
  (interactive)
  (call-interactively (lambda! (avy-goto-line-above offset bottom-up)))
  (end-of-line))

(defun avy-extras-goto-end-of-line-below (&optional offset bottom-up)
  "Goto visible end of line below the cursor.
OFFSET changes the distance between the closest key to the cursor and the
cursor. When BOTTOM-UP is non-nil, display avy candidates from top to bottom."
  (interactive)
  (call-interactively (lambda! (avy-goto-line-below offset bottom-up)
  (end-of-line))

(defun avy-extras-dired-find-file ()
  "In Dired, visit the file or directory in selected line."
  (interactive)
  (avy-goto-line)
  (dired-find-alternate-file))

(defun avy-extras-ebib-view-entry ()
  "In Ebib, view the entry in selected line."
  (interactive)
  (avy-goto-line)
  (ebib-edit-entry))

(defun avy-extras-headers-view-message ()
  "In mu4e, view the message in selected line."
  (interactive)
  (avy-goto-line)
  (mu4e-headers-view-message))

(defun avy-extras-telega-view-message ()
  "In Telega, view the message in selected line."
  (interactive)
  (avy-goto-line)
  (push-button)) ; not sure what the actual command to open a chat is

(defun avy-extras-elfeed-search-show-entry ()
  "In Elfeed, display the item in selected line."
  (interactive)
  (avy-goto-line)
  (call-interactively 'elfeed-search-show-entry))

;; karthinks.com/software/avy-can-do-anything/#mark-the-region-from-point-to-a-candidate
(defun avy-extras-action-mark-to-char (pt)
  "Mark the region from point to PT."
  (activate-mark)
  (goto-char pt))

(provide 'avy-extras)
;;; avy-extras.el ends here

