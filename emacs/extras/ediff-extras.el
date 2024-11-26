;;; ediff-extras.el --- Extensions for ediff -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/ediff-extras.el
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

;; Extensions for `ediff'.

;;; Code:

(require 'ediff)

;;;; Functions

;; copied from emacs.stackexchange.com/a/51548/32089
(defun ediff-extras-toggle-word-mode ()
  "Toggle between linewise and wordwise comparisons."
  (interactive)
  (setq ediff-word-mode (not ediff-word-mode))
  (ediff-update-diffs))

(provide 'ediff-extras)
;;; ediff-extras.el ends here

