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
(require 'el-patch)
(require 'simple-extras)
(require 'use-package-extras)

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
  (call-interactively (lambda! (avy-goto-line-below offset bottom-up)))
  (end-of-line))

(declare-function dired-find-alternate-file "dired")
(defun avy-extras-dired-find-file ()
  "In Dired, visit the file or directory in selected line."
  (interactive)
  (require 'dired)
  (avy-goto-line)
  (dired-find-alternate-file))

(declare-function ebib-edit-entry "ebib")
(defun avy-extras-ebib-view-entry ()
  "In Ebib, view the entry in selected line."
  (interactive)
  (require 'ebib)
  (avy-goto-line)
  (ebib-edit-entry))

(declare-function mu4e-headers-view-message "mu4e-headers")
(defun avy-extras-headers-view-message ()
  "In mu4e, view the message in selected line."
  (interactive)
  (require 'mu4e-headers)
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

;;;; Patched functions

;; Launch dispatcher with `;' rather than `?'
(el-patch-defun avy-handler-default (char)
  "The default handler for a bad CHAR."
  (let (dispatch)
    (cond ((setq dispatch (assoc char avy-dispatch-alist))
	   (unless (eq avy-style 'words)
	     (setq avy-action (cdr dispatch)))
	   (throw 'done 'restart))
	  ((memq char avy-escape-chars)
	   ;; exit silently
	   (throw 'done 'abort))
	  ((el-patch-swap
	     (eq char ??)
	     (eq char ?\;))
	   (avy-show-dispatch-help)
	   (throw 'done 'restart))
	  ((mouse-event-p char)
	   (signal 'user-error (list "Mouse event not handled" char)))
	  (t
	   (message "No such candidate: %s, hit `C-g' to quit."
		    (if (characterp char) (string char) char))))))

(provide 'avy-extras)
;;; avy-extras.el ends here

