;;; org-msg-extras.el --- Extensions for org-msg -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/org-msg-extras.el
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

;; Extensions for `org-msg'.

;;; Code:

(require 'org-msg)
(require 'org-extras)

;;;; Functions

(defun org-msg-extras-kill-message ()
  "Save the current message to the kill ring."
  (interactive)
  (unless (eq major-mode 'org-msg-edit-mode)
    (user-error "Not in org-msg-edit-mode"))
  (save-excursion
    (goto-char (org-msg-start))
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


(provide 'org-msg-extras)
;;; org-msg-extras.el ends here
