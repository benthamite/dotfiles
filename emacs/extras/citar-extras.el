;;; citar-extras.el --- Extensions for citar -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/citar-extras.el
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

;; Extensions for `citar'.

;;; Code:

(require 'citar)
(require 'path)
(require 'el-patch)

;;;; Main variables

(defvar citar-extras-indicator-files-icons
  (citar-indicator-create
   :symbol (all-the-icons-faicon
            "file-o"
            :face 'all-the-icons-green
            :v-adjust -0.1)
   :function #'citar-has-files
   :padding "  " ; need this because the default padding is too low for these icons
   :tag "has:files"))

(defvar citar-extras-indicator-links-icons
  (citar-indicator-create
   :symbol (all-the-icons-octicon
            "link"
            :face 'all-the-icons-orange
            :v-adjust 0.01)
   :function #'citar-has-links
   :padding "  "
   :tag "has:links"))

(defvar citar-extras-indicator-notes-icons
  (citar-indicator-create
   :symbol (all-the-icons-material
            "speaker_notes"
            :face 'all-the-icons-blue
            :v-adjust -0.3)
   :function #'citar-has-notes
   :padding "  "
   :tag "has:notes"))

;;;; Functions

(defun citar-extras-open-in-ebib (citekey)
  "Open bibliographic entry associated with the CITEKEY in Ebib."
  (interactive (list (citar-select-ref)))
  (require 'ebib)
  (require 'ebib-extras)
  (unless (get-buffer "*ebib*")
    (ebib))
  (ebib-extras-open-key citekey))

(defun citar-extras-move-entry-to-tlon (citekey)
  "Move bibliographic entry associated with the CITEKEY to the Tlön bibliography."
  (interactive (list (citar-select-ref)))
  (require 'bibtex-extras)
  (save-window-excursion
    (citar-extras-goto-bibtex-entry citekey)
    (bibtex-extras-move-entry-to-tlon)))

(defun citar-extras-goto-bibtex-entry (citekey)
  "Goto the bibliographic entry associated with the CITEKEY in the bibtex file."
  (interactive (list (citar-select-ref)))
  (require 'ebib-extras)
  (if-let ((file (ebib-extras-get-file-of-key citekey)))
      (progn
        (find-file file)
        (widen)
        (goto-char (point-min))
        (re-search-forward (format "@.*?{%s" citekey)))
    (user-error "No entry found for %s" citekey)))

(defun citar-extras-open-file-at-point ()
  "Launch citar with citekey associated with file at point."
  (interactive)
  (require 'files-extras)
  (citar-open `(,(files-extras-get-stem-of-current-buffer))))

(defun citar-extras-update-old-bibliography ()
  "Update `old.bib' bibliography."
  (interactive)
  (citar-cache--update-bibliography
   (citar-cache--get-bibliography (cadr citar-bibliography))))

(run-with-idle-timer (* 15 60) nil #'citar-extras-update-old-bibliography)

;;;;; Patched functions

;; Remove conditional to allow invocation
;; in any mode. Even if inserting a citation is not allowed, one may
;; want to invoke the command to trigger contextual actions via
;; `embark'.
(el-patch-defun citar-insert-citation (citekeys &optional arg)
  "Insert citation for the CITEKEYS.

Prefix ARG is passed to the mode-specific insertion function. It
should invert the default behaviour for that mode with respect to
citation styles. See specific functions for more detail."
  (interactive
   (el-patch-swap
     (if (citar--get-major-mode-function 'insert-citation)
	 (list (citar-select-refs) current-prefix-arg)
       (error "Citation insertion is not supported for %s" major-mode))
     (list (citar-select-refs) current-prefix-arg)))
  (citar--major-mode-function
   'insert-citation
   #'ignore
   citekeys
   arg))

(provide 'citar-extras)
;;; citar-extras.el ends here

