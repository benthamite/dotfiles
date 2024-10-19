;;; consult-extras.el --- Extensions for consult -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/consult-extras.el
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

;; Extensions for `consult'.

;;; Code:

(require 'consult)
(require 'paths)

;;;; Functions

(defun consult-extras-toggle-multiline ()
  "Toggle between single line and multiline search."
  (interactive)
  (let* ((multiline-flags " --multiline --multiline-dotall")
         (new-value (if (string-match (concat "\\(.*?\\)" multiline-flags) consult-ripgrep-args)
                        (match-string 1)
                      (concat consult-ripgrep-args multiline-flags))))
    (setq consult-ripgrep-args new-value)))

(defun consult-extras-locate-current ()
  "Search with `consult-locate' in the current directory."
  (interactive)
  (let ((consult-locate-args (list "mdfind-wrapper" (format "-onlyin %s" default-directory))))
    (consult-locate)))

(defun consult-extras-locate-file-current ()
  "Search with `consult-locate' in the current directory, matching file names only."
  (interactive)
  (let ((consult-locate-args (list "mdfind-wrapper" (format "-onlyin %s" default-directory) "-name")))
    (consult-locate)))

(defun consult-extras-ripgrep-current ()
  "Search with `rg' in the current directory."
  (interactive)
  (consult-ripgrep default-directory))

(declare-function org-extras-fold-show-all-headings "org-extras")
(declare-function consult-org-heading "consult-org")
;;;###autoload
(defun consult-extras-org-heading (&optional match scope)
  "Jump to an Org heading.
MATCH and SCOPE are as in org-map-entries and determine which
entries are offered.  By default, all entries of the current
buffer are offered."
  (interactive)
  (widen)
  (require 'org-extras)
  (org-extras-fold-show-all-headings)
  (consult-org-heading match scope)
  (recenter 1))

(provide 'consult-extras)
;;; consult-extras.el ends here

