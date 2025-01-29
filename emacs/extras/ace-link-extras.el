;;; ace-link-extras.el --- Extensions for ace-link -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/ace-link-extras.el
;; Version: 0.2
;; Package-Requires: ((ace-link "0.4.0"))

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

;; Extensions for `ace-link'.

;;; Code:

(require 'ace-link)

;;;; Functions

(defvar mm-text-html-renderer)
;;;###autoload
(defun ace-link-extras-mu4e ()
  "Open a visible link in an `mu4e-view-mode' buffer.
Unlike `ace-link-mu4e', this function is sensitive to the value of
`mm-text-html-renderer'. Specifically, it will use `ace-link--w3m-collect' to
collect the URLs if its value is `w3m'. Otherwise it will use the normal
`ace-link-mu4e'.

We do not use `ace-link-w3m' because we do not want to browse the URL with
`w3m', which we only use to render HTML email messages. Instead, we rely on
`ace-link-extras-w3m-get-url' to get the URL and then open it with
`browse-url' (which will browse it with `eww', `chrome' or `firefox', as
specified by `browse-url-handlers')."
  (interactive)
  (if (eq mm-text-html-renderer 'w3m)
      (when-let* ((url (ace-link-extras-w3m-get-url)))
	(browse-url url))
    (ace-link-mu4e)))

(defun ace-link-extras-w3m-get-url ()
  "Return the selected URL in a `w3m-mode' buffer."
  (let ((pt (avy-with ace-link-w3m
              (avy-process
               (mapcar #'cdr (ace-link--w3m-collect))
               #'avy--overlay-pre))))
    (when pt
      (get-text-property (point) 'w3m-href-anchor))))

;;;###autoload
(defun ace-link-extras-eww-externally ()
  "Browse URL using `browse-url-secondary-browser-function'."
  (interactive)
  (ace-link-eww '(4)))

;;;###autoload
(defun ace-link-extras-eww-new-buffer ()
  "Browse URL in new buffer."
  (interactive)
  (ace-link-eww '(16)))

;;;###autoload
(defun ace-link-extras-org-roam ()
  "Open a visible link in an `org-roam-mode' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-org
		      (avy-process
		       (mapcar #'cdr (ace-link--org-collect))
		       (avy--style-fn avy-style)))))
    (ace-link-extras--org-roam-action pt)))

(autoload 'org-roam-preview-visit "org-roam-mode")
(defun ace-link-extras--org-roam-action (pt)
  "Visit the link at PT in an `org-roam-mode' buffer."
  (when (numberp pt)
    (goto-char pt)
    (call-interactively #'org-roam-preview-visit)))

;;;;; Patched functions

;; (declare-function shr-browse-url "shr")
;; (declare-function mu4e--view-browse-url-from-binding "mu4e-view")
;; (declare-function mu4e--view-open-attach-from-binding "mu4e-view")
;; 2024-11-21 this patch tries to replace two obsolete functions with their
;; replacements. however, the replacement for
;; `mu4e~view-open-attach-from-binding' does not exist, and I wasn’t able to
;; find the name of the actual replacement. Moreover, it seems that these
;; conditions are never triggered, so there is no need to patch the function.
;; commenting out for the time being; delete when I am reasonably confident that
;; the patch is not needed.

;; (el-patch-defun ace-link--mu4e-action (pt)
;; "Open link at PT in a `mu4e-view' buffer."
;; (when (number-or-marker-p pt)
;; (goto-char (1+ pt))
;; (cond ((get-text-property (point) 'shr-url)
;; (shr-browse-url))
;; ((get-text-property (point) 'mu4e-url)
;; (el-patch-swap (mu4e--view-browse-url-from-binding) (mu4e--view-browse-url-from-binding)))
;; ((get-text-property (point) 'mu4e-attnum)
;; (el-patch-swap (mu4e--view-open-attach-from-binding) (mu4e--view-open-attach-from-binding))))))

(provide 'ace-link-extras)
;;; ace-link-extras.el ends here
