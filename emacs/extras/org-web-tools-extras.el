;;; org-web-tools-extras.el --- Extensions for org-web-tools -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/org-web-tools-extras.el
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

;; Extensions for `org-web-tools'.

;;; Code:

(require 'org-web-tools)
(require 'paths)

;;;; Functions

;; This function slightly tweaks `org-web-tools-insert-link-for-url'
;; so that it can be used with `org-capture'
;; blog.lazkani.io/posts/text-editors/bookmark-with-org-capture/
(defun org-web-tools-extras-insert-link-for-clipboard-url ()
  "Extend `org-web-tools-inster-link-for-url' to take URL from \"kill-ring\"."
  (org-web-tools--org-link-for-url (org-web-tools--get-first-url)))

(cl-defun org-web-tools-extras-org-title-for-url (&optional (url (org-web-tools--get-first-url)))
  "Return Org link to URL using title of HTML page at URL.
If URL is not given, look for first URL in `kill-ring'.  If page
at URL has no title, return URL."
  (if-let ((dom (plz 'get url :as #'libxml-parse-html-region))
           (title (cl-caddr (car (dom-by-tag dom 'title)))))
      (org-web-tools--cleanup-title title)))

(defun org-web-tools-extras-youtube-dl (url)
  "Create org link to local copy of YouTube video downloaded from URL.
To be used in conjunction with associated `org-capture' template."
  (require 'prot-eww)
  (let* ((html (org-web-tools--get-url url))
         (title (org-web-tools--html-title html))
         (file-path (file-name-concat paths-dir-downloads
				      (file-name-with-extension (prot-eww--sluggify title) "webm"))))
    (if title
        (org-link-make-string (concat "file:" file-path) title)
      (user-error "HTML page at URL has no title"))))

(provide 'org-web-tools-extras)
;;; org-web-tools-extras.el ends here

