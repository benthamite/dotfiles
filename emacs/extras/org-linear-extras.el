;;; org-linear-extras.el --- Extensions for org-linear -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/org-linear-extras.el
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

;; Extensions for `org-linear': a model-backed
;; `org-linear-merge-body-function', which merges a description edited in
;; Linear into an Org body whose markup Markdown cannot hold.

;;; Code:

(require 'org-linear)
(require 'gptel)

;;;; User options

(defgroup org-linear-extras ()
  "Extensions for `org-linear'."
  :group 'org-linear)

(defcustom org-linear-extras-merge-backend "Claude"
  "Name of the gptel backend that merges description edits."
  :type 'string
  :group 'org-linear-extras)

(defcustom org-linear-extras-merge-model nil
  "Model that merges description edits, or nil for `gptel-model'."
  :type '(choice (const nil) symbol)
  :group 'org-linear-extras)

(defconst org-linear-extras-merge-system
  "You merge an edit made to a task's description in Linear, which stores \
Markdown, into the task's body in an Org mode file, which is the primary copy.

You get three texts: ORG, the current Org body; OLD, the Markdown that ORG \
was last converted to; NEW, the Markdown after the user's edit in Linear.

Return the Org body that applies the edit from OLD to NEW to ORG. Where a \
passage did not change between OLD and NEW, keep ORG's text for it exactly, \
with its Org-only markup: link targets, source block languages and header \
arguments, timestamps, footnotes, drawers, macros. Where it changed, write \
NEW's text in Org syntax, keeping whatever Org markup still applies. The \
result, converted from Org to GitHub Markdown by pandoc, must equal NEW.

Reply with the Org text only: no code fences, no comments, no preamble."
  "System message for merging a description edit into an Org body.")

;;;; Functions

;;;###autoload
(defun org-linear-extras-merge-body (org-body old-md new-md callback)
  "Merge NEW-MD, Linear's edit of OLD-MD, into ORG-BODY with a model.
Call CALLBACK with the merged Org body, or nil if the request failed.
This is meant for `org-linear-merge-body-function', which checks the
result against NEW-MD before using it."
  (let ((gptel-backend (gptel-get-backend org-linear-extras-merge-backend))
        (gptel-model (or org-linear-extras-merge-model gptel-model))
        (gptel-use-tools nil)
        (gptel-use-context nil)
        (gptel-stream nil))
    (gptel-request (org-linear-extras--merge-prompt org-body old-md new-md)
      :system org-linear-extras-merge-system
      :callback (lambda (response _info)
                  (funcall callback (org-linear-extras--merge-result response))))))

(defun org-linear-extras--merge-prompt (org-body old-md new-md)
  "Return the request that merges NEW-MD, an edit of OLD-MD, into ORG-BODY."
  (format "ORG:\n<<<\n%s\n>>>\n\nOLD:\n<<<\n%s\n>>>\n\nNEW:\n<<<\n%s\n>>>"
          org-body old-md new-md))

(defun org-linear-extras--merge-result (response)
  "Return the Org body in the model's RESPONSE, or nil if it holds none.
A code fence the model wrapped the text in is removed."
  (when (and (stringp response) (not (string-blank-p response)))
    (string-trim
     (replace-regexp-in-string
      "\\`[ \t\n]*```[a-z]*\n\\(\\(?:.\\|\n\\)*?\\)\n```[ \t\n]*\\'" "\\1"
      (string-trim response)))))

(provide 'org-linear-extras)
;;; org-linear-extras.el ends here
