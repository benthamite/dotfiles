;;; org-msg-extras.el --- Extensions for org-msg -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/org-msg-extras.el
;; Version: 0.2
;; Package-Requires: ((org-msg "4.0") (org-extras "0.1"))

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

;; Extensions for `org-msg'.

;;; Code:

(require 'org-extras)
(require 'org-msg)

;;;; User options

(defgroup org-msg-extras ()
  "Extensions for `org-msg'."
  :group 'org-msg)

(defcustom org-msg-extras-personal-plain-text-signature
  "\n--\nPablo\n"
  "Personal signature for plain text emails."
  :type 'string
  :group 'org-msg-extras)

(defcustom org-msg-extras-personal-html-signature
  "\n#+begin_signature\n--\n*Pablo*\n#+end_signature"
  "Personal signature for HTML emails."
  :type 'string
  :group 'org-msg-extras)

(defcustom org-msg-extras-work-plain-text-signature
  "\n--\nPablo Stafforini\nOperations Associate, Automations\nEpoch AI | https://epoch.ai/\n"
  "Work signature for plain text emails."
  :type 'string
  :group 'org-msg-extras)

(defcustom org-msg-extras-work-html-signature
  (concat
   "\n#+begin_signature\n--\n"
   "#+begin_export html\n"
   "<span style=\"display:block; font-family:Arial,Helvetica,sans-serif;"
   " font-size:13px; line-height:1.4\">"
   "<b>Pablo Stafforini</b><br>"
   "Operations Associate, Automations</span>\n"
   "#+end_export\n"
   "#+attr_html: :width 120\n"
   "[[file:~/.local/share/epoch/epoch-logo.png]]\n"
   "#+begin_export html\n"
   "<span style=\"display:block; font-family:Arial,Helvetica,sans-serif;"
   " font-size:11px; line-height:1.4\">"
   "<a href=\"https://epoch.ai/\">Website</a> <b>|</b> "
   "<a href=\"https://x.com/EpochAIResearch\">Twitter</a> <b>|</b> "
   "<a href=\"https://epochai.substack.com/subscribe\">Newsletter</a></span>\n"
   "#+end_export\n"
   "#+end_signature")
  "Work signature for HTML emails."
  :type 'string
  :group 'org-msg-extras)

;;;; Functions

(defun org-msg-extras-kill-message ()
  "Save the current message to the kill ring."
  (interactive)
  (unless (derived-mode-p 'org-msg-edit-mode)
    (user-error "Not in org-msg-edit-mode"))
  (save-excursion
    (goto-char (org-msg-start))
    ;; hack; consider refining
    (re-search-forward "^:END:\n")
    (let ((beg (point)))
      (goto-char (org-msg-end))
      (search-backward "#+begin_signature" nil t)
      (kill-region beg (point)))))

;; currently, grammarly isn't working reliably in Emacs, so we use the
;; external editor instead
(defun org-msg-extras-open-in-grammarly ()
  "Copy the current message in Grammarly's external editor."
  (interactive)
  (org-msg-extras-kill-message)
  (browse-url "https://app.grammarly.com/ddocs/2264307164"))

(declare-function simple-extras-visible-mode-enhanced "simple-extras")
;;;###autoload
(defun org-msg-extras-begin-compose ()
  "Move point to start composing an email.
This is a slightly tweaked version of `org-msg-goto-body'."
  (interactive)
  (goto-char (point-min))
  (if org-msg-signature
      (when (search-forward org-msg-signature nil t)
	(goto-char (match-beginning 0)))
    (org-msg-extras-end-of-meta-data)
    (newline)
    (simple-extras-visible-mode-enhanced -1)))

;;;###autoload
(defun org-msg-extras-end-of-meta-data ()
  "If point is on the meta data section, move it past it."
  (while (re-search-forward org-property-re nil t)
    (forward-line)))

(defun org-msg-extras-fold-signature-blocks ()
  "Fold export blocks inside the email signature."
  (save-excursion
    (goto-char (point-max))
    (when (search-backward "#+begin_signature" nil t)
      (save-restriction
	(narrow-to-region (point) (point-max))
	(org-fold-hide-block-all)))))

(provide 'org-msg-extras)
;;; org-msg-extras.el ends here

