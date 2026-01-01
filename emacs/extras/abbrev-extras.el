;;; abbrev-extras.el --- Extensions for abbrev -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/abbrev-extras.el
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

;; Extensions for `abbrev'.

;;; Code:

(require 'abbrev)

;;;; User options

(defgroup abbrev-extras ()
  "Extensions for `abbrev'."
  :group 'abbrev)

(defcustom abbrev-extras-write-file-p t
  "Whether to write abbrevs to file after adding a new abbrev."
  :type 'boolean
  :group 'abbrev-extras)

;;;; Functions

;;;###autoload
(defun abbrev-extras-add-mode-abbrev ()
  "Interactively add a mode-specific abbrev.
If write the abbrev file."
  (interactive)
  (let* ((mode-list (abbrev-extras-get-sorted-modes))
         (mode (completing-read "Major mode: " mode-list nil t))
         (abbrev (read-string "Abbrev: "))
         (expansion (read-string "Expansion: "))
         (table-symbol (intern (concat mode "-abbrev-table"))))
    (abbrev-extras-add-abbrev-to-table abbrev table-symbol expansion)
    (when abbrev-extras-write-file-p (abbrev-extras-write-abbrevs))))

(defun abbrev-extras-get-sorted-modes ()
  "Return a sorted list of mode names for functions ending in \"-mode\"."
  (sort
   (let (modes)
     (mapatoms
      (lambda (s)
        (when (and (functionp s)
                   (string-match-p "-mode\\'" (symbol-name s)))
          (push (symbol-name s) modes)))
      obarray)
     modes)
   #'string<))

(defun abbrev-extras-add-abbrev-to-table (abbrev table-symbol expansion)
  "Add ABBREV to TABLE-SYMBOL with EXPANSION."
  (abbrev-extras-maybe-create-abbrev-table table-symbol)
  (define-abbrev (symbol-value table-symbol) abbrev expansion)
  (message "Added abbrev '%s' -> \"%s\" to %s" abbrev expansion table-symbol))

(defun abbrev-extras-maybe-create-abbrev-table (table-symbol)
  "Create TABLE-SYMBOL if it doesn't exist."
  (unless (boundp table-symbol)
    (define-abbrev-table table-symbol '())))

(defun abbrev-extras-write-abbrevs ()
  "Write abbrevs to file."
  (interactive)
  (write-abbrev-file abbrev-file-name)
  (message "Abbrevs written to %s" abbrev-file-name))

(provide 'abbrev-extras)
;;; abbrev-extras.el ends here
