;;; citar-extras.el --- Extensions for citar -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/citar-extras.el
;; Version: 0.2
;; Package-Requires: ((citar "0.1") (paths "0.1"))

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

;; Extensions for `citar'.

;;; Code:

(require 'citar)
(require 'paths)

;;;; User options

(defgroup citar-extras ()
  "Extensions for `citar'."
  :group 'citar)

(defcustom citar-extras-auto-update-bibliographies t
  "Whether to automatically update the bibliography when `citar' is loaded."
  :type 'boolean
  :group 'citar-extras)

;;;; Variables

(autoload 'nerd-icons-faicon "nerd-icons")
(defvar citar-extras-indicator-files-icons
  (citar-indicator-create
   :symbol (nerd-icons-faicon
            "nf-fa-file_o"
            :face 'nerd-icons-green
            :v-adjust -0.1)
   :function #'citar-has-files
   :padding "  " ; need this because the default padding is too low for these icons
   :tag "has:files"))

(defvar citar-extras-indicator-links-icons
  (citar-indicator-create
   :symbol (nerd-icons-faicon
            "nf-fa-link"
            :face 'nerd-icons-orange
            :v-adjust 0.01)
   :function #'citar-has-links
   :padding "  "
   :tag "has:links"))

(autoload 'nerd-icons-codicon "nerd-icons")
(defvar citar-extras-indicator-notes-icons
  (citar-indicator-create
   :symbol (nerd-icons-codicon
            "nf-cod-note"
            :face 'nerd-icons-blue
            :v-adjust -0.3)
   :function #'citar-has-notes
   :padding "    "
   :tag "has:notes"))

(defvar citar-extras-indicator-cited-icons
  (citar-indicator-create
   :symbol (nerd-icons-faicon
            "nf-fa-circle_o"
            :face 'nerd-icon-green)
   :function #'citar-is-cited
   :padding "  "
   :tag "is:cited"))

;;;; Functions

(declare-function ebib "ebib")
(declare-function ebib-extras-open-key "ebib-extras")
(defun citar-extras-open-in-ebib (citekey)
  "Open bibliographic entry associated with the CITEKEY in Ebib."
  (interactive (list (citar-select-ref)))
  (unless (get-buffer "*ebib*")
    (ebib))
  (ebib-extras-open-key citekey))

(declare-function tlon-move-entry-to-fluid "tlon-tex")
(defun citar-extras-move-entry-to-tlon (citekey)
  "Move bibliographic entry associated with the CITEKEY to the Tlön bibliography."
  (interactive (list (citar-select-ref)))
  (save-window-excursion
    (citar-extras-goto-bibtex-entry citekey)
    (tlon-move-entry-to-fluid)))

(declare-function ebib-extras-get-file-of-key "bibtex-extras")
(defun citar-extras-goto-bibtex-entry (citekey)
  "Goto the bibliographic entry associated with the CITEKEY in the bibtex file."
  (interactive (list (citar-select-ref)))
  (if-let ((file (ebib-extras-get-file-of-key citekey)))
      (progn
        (find-file file)
        (widen)
        (goto-char (point-min))
        (re-search-forward (format "@.*?{%s" citekey)))
    (user-error "No entry found for %s" citekey)))

(autoload 'files-extras-get-stem-of-current-buffer "files-extras")
(defun citar-extras-open-file-at-point ()
  "Launch `citar' with citekey associated with file at point."
  (interactive)
  (citar-open `(,(files-extras-get-stem-of-current-buffer))))

;;;###autoload
(defun citar-extras-set-bibliography ()
  "Set the bibliography to `paths-files-bibliography-all'.
This function should be run via a post-init hook, to ensure that
`paths-files-bibliography-all' is initialized."
  (setq citar-bibliography paths-files-bibliography-all))

;;;###autoload
(defun citar-extras-update-bibliographies ()
  "Update the bibliographies."
  (when citar-extras-auto-update-bibliographies
    (dolist (bibliography citar-bibliography)
      (citar-cache--update-bibliography
       (citar-cache--get-bibliography bibliography)))))

(provide 'citar-extras)
;;; citar-extras.el ends here

