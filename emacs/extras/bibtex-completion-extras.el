;;; bibtex-completion-extras.el --- Extensions for bibtex-completion -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/bibtex-completion-extras.el
;; Version: 0.2
;; Package-Requires: ((bibtex-completion "1.0.0") (el-patch "2.1"))

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

;; Extensions for `bibtex-completion'.

;;; Code:

(require 'bibtex-completion)
(require 'el-patch)

;;;; Functions

;; disable file watchers
(el-patch-defun bibtex-completion-init ()
  "Check that the files and directories specified by the user actually exist.
Also sets `bibtex-completion-display-formats-internal'."

  ;; Remove current watch-descriptors for bibliography files:
  (mapc (lambda (watch-descriptor)
	  (file-notify-rm-watch watch-descriptor))
	bibtex-completion-file-watch-descriptors)
  (setq bibtex-completion-file-watch-descriptors nil)

  ;; Check that all specified bibliography files exist and add file
  ;; watches for automatic reloading of the bibliography when a file
  ;; is changed:
  (el-patch-remove (mapc (lambda (file)
			   (if  (f-file? file)
			       (if bibtex-completion-watch-bibliography
				   (let ((watch-descriptor
					  (file-notify-add-watch file
								 '(change)
								 (lambda (event) (bibtex-completion-candidates)))))
				     (setq bibtex-completion-file-watch-descriptors
					   (cons watch-descriptor bibtex-completion-file-watch-descriptors))))
			     (user-error "Bibliography file %s could not be found" file)))
			 (bibtex-completion-normalize-bibliography)))

  ;; Pre-calculate minimal widths needed by the format strings for
  ;; various entry types:
  (setq bibtex-completion-display-formats-internal
	(mapcar (lambda (format)
		  (let* ((format-string (cdr format))
			 (fields-width 0)
			 (string-width
			  (string-width
			   (s-format format-string
				     (lambda (field)
				       (setq fields-width
					     (+ fields-width
						(string-to-number
						 (or (cadr (split-string field ":"))
						     ""))))
				       "")))))
		    (-cons* (car format) format-string (+ fields-width string-width))))
		bibtex-completion-display-formats)))

(provide 'bibtex-completion-extras)
;;; bibtex-completion-extras.el ends here

