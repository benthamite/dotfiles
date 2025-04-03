;;; org-web-tools-extras.el --- Extensions for org-web-tools -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/org-web-tools-extras.el
;; Version: 0.2
;; Package-Requires: ((org-web-tools "1.1") (paths "0.1"))

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

;; Extensions for `org-web-tools'.

;;; Code:

(require 'org-web-tools)
(require 'paths)

;;;; Functions

(defun org-web-tools-extras-insert-link-for-clipboard-url ()
  "Extend `org-web-tools-instert-link-for-url' to take URL from \"kill-ring\".
This function slightly tweaks `org-web-tools-insert-link-for-url' so that it can
be used with `org-capture'."
  (org-web-tools--org-link-for-url (org-web-tools--get-first-url)))

(cl-defun org-web-tools-extras-org-title-for-url (&optional (url (org-web-tools--get-first-url)))
  "Return Org link to URL using title of HTML page at URL.
If URL is not given, look for first URL in `kill-ring'. If page at URL has no
title, return URL."
  (let ((title (if-let* ((dom (condition-case nil
				 (plz 'get url :as #'libxml-parse-html-region)
			       (error nil))))
		   (cl-caddr (car (dom-by-tag dom 'title)))
		 "Downloaded webpage")))
    (org-web-tools--cleanup-title title)))

(provide 'org-web-tools-extras)
;;; org-web-tools-extras.el ends here

