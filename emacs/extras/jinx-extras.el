;;; jinx-extras.el --- Extensions for jinx -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/jinx-extras.el
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

;; Extensions for `jinx'.

;;; Code:

(require 'jinx)

;;;; Functions

;; github.com/minad/jinx/wiki#save-misspelling-and-correction-as-abbreviation
(defun jinx-extras--add-to-abbrev (overlay word)
  "Add abbreviation to `text-mode-abbrev-table'.
The misspelled word is taken from OVERLAY.  WORD is the corrected word."
  (let ((abbrev (buffer-substring-no-properties
                 (overlay-start overlay)
                 (overlay-end overlay))))
    (message "Abbrev: %s -> %s" abbrev word)
    (define-abbrev text-mode-abbrev-table abbrev word)))

(advice-add 'jinx--correct-replace :before #'jinx-extras--add-to-abbrev)

(defun jinx-extras-toggle-languages ()
  "Toggle between English and Spanish."
  (interactive)
  (let ((lang (if (string= jinx-languages "en") "es" "en")))
    (jinx-languages lang)))

(provide 'jinx-extras)
;;; jinx-extras.el ends here
