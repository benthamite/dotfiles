;;; outline-extras.el --- Extensions for outline -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/outline-extras.el
;; Version: 0.2

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

;; Extensions for `outline'.

;;; Code:

(require 'outline)

;;;; Functions

(defun outline-extras-promote-heading ()
  "Promote the current heading higher up the tree."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'outline-promote))
  (outline-previous-heading))

(defun outline-extras-demote-heading ()
  "Demote the current heading higher up the tree."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'outline-demote))
  (outline-previous-heading))

(provide 'outline-extras)
;;; outline-extras.el ends here

