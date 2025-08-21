;;; citar-extras.el --- Extensions for citar -*- lexical-binding: t -*-

;; Copyright (C) 2025

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
;;;###autoload
(defun citar-extras-open-in-ebib (citekey)
  "Open bibliographic entry associated with the CITEKEY in Ebib."
  (interactive (list (citar-select-ref)))
  (unless (get-buffer "*ebib*")
    (ebib))
  (ebib-extras-open-key citekey))

(declare-function tlon-move-entry-to-fluid "tlon-bib")
(defun citar-extras-move-entry-to-tlon (citekey)
  "Move bibliographic entry associated with the CITEKEY to the Tlön bibliography."
  (interactive (list (citar-select-ref)))
  (save-window-excursion
    (citar-extras-goto-bibtex-entry citekey)
    (tlon-move-entry-to-fluid)))

(declare-function ebib-extras-get-file-of-key "bibtex-extras")
(defun citar-extras-goto-bibtex-entry (citekey)
  "Go to the bibliographic entry associated with the CITEKEY in the bibtex file."
  (interactive (list (citar-select-ref)))
  (if-let* ((file (ebib-extras-get-file-of-key citekey)))
      (progn
        (find-file file)
        (widen)
        (goto-char (point-min))
        (re-search-forward (format "@.*?{%s" citekey)))
    (user-error "No entry found for %s" citekey)))

;;;;; Cache management

;;;###autoload
(defun citar-extras-refresh-bibliography (file &optional force)
  "Refresh the Citar cache for bibliography FILE.
FILE is the path to a bibliography file.  When FORCE is non-nil,
the cache is rebuilt even if Citar thinks it is current.

Interactively, prompt for FILE."
  (interactive "fBibliography file: ")
  (citar-cache--get-bibliography (file-truename file) force))

;;;###autoload
(defun citar-extras-refresh-all-bibliographies (&optional force)
  "Refresh the Citar cache for all bibliography files.
When FORCE is non-nil, the cache is rebuilt even if Citar thinks it is current."
  (interactive "P")
  (when-let* ((bibs (citar--bibliographies)))
    (dolist (bib bibs)
      (citar-extras-refresh-bibliography bib force))))

(provide 'citar-extras)
;;; citar-extras.el ends here

