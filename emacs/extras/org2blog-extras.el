;;; org2blog-extras.el --- Extensions for org2blog -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/org2blog-extras.el
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

;; Extensions for `org2blog'.

;;; Code:

(require 'org2blog)

;;;; Functions

(defun org2blog-extras-move-tags-to-drawer ()
  "Convert `org-mode' tags to values of the property `POST_TAGS' in an org drawer."
  (interactive)
  (while (re-search-forward "^\\* .*?:\\(.*\\):\n")
    (let ((tags (string-join
                 (split-string
                  (substring-no-properties
                   (match-string 1))
                  ":")
                 ", ")))
      (org-set-property "POSG_TAGS" tags))))

;; setting `org2blog/wp-confirm-post' to t asks for confirmation
;; before publication, but I want to be asked only when the
;; publication (i.e. post or page) lacks an ID
(defun org2blog-extras-subtree-publish-save (orig-fun &rest args)
  "Ask for confirmation only when publication lacks an ID.
ORIG-FUN is the original function.  ARGS are the arguments."
  (if (cdr (assoc "post-id" (org2blog--export-as-post t)))
      (apply orig-fun args)
    (when (y-or-n-p "Post has no ID. Proceed anyway? ")
      (apply orig-fun args))))

(advice-add 'org2blog-subtree-post-save :around #'org2blog-extras-subtree-publish-save)
(advice-add 'org2blog-subtree-page-save :around #'org2blog-extras-subtree-publish-save)

(provide 'org2blog-extras)
;;; org2blog-extras.el ends here

