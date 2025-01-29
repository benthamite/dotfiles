;;; kmacro-extras.el --- Extensions for kmacro -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/kmacro-extras.el
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

;; Extensions for `kmacro'.

;;; Code:

(require 'kmacro)

;;;; Functions

(defun kmacro-extras-counter-toggle-alpha-number ()
  "Toggle between a numeric and an alphabetical keyboard macro counter."
  (interactive)
  (if (string= kmacro-counter-format "%d")
      (progn
        (kmacro-set-format "%c")
        (kmacro-set-counter 97)
        (message "Set to alphabetical"))
    (progn
      (kmacro-set-format "%d")
      (kmacro-set-counter 1)
      (message "Set to numeric"))))

(provide 'kmacro-extras)
;;; kmacro-extras.el ends here

