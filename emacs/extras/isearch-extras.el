;;; isearch-extras.el --- Extensions for isearch -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/isearch-extras.el
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

;; Extensions for `isearch'.

;;; Code:

(require 'isearch)

;;;; Functions

(defun isearch-extras-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(defun isearch-extras-copy-match ()
  "Send the first isearch match to the kill ring."
  (interactive)
  (kill-new (buffer-substring (point) isearch-other-end))
  (isearch-done))

;; reddit.com/r/emacs/comments/8aepnk/comment/dwybodv
(defun isearch-extras-yank-kill-literally ()
  "Pull string from kill ring into search string literally."
  (interactive)
  (setq isearch-yank-flag t)
  (let ((string (current-kill 0)))
    (isearch-process-search-string
     string
     (mapconcat 'isearch-text-char-description string ""))))

(provide 'isearch-extras)
;;; isearch-extras.el ends here
