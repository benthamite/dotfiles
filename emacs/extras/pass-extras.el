;;; pass-extras.el --- Exteions for pass -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/pass-extras.el
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

;; Extensions for `pass'.

;;; Code:

(require 'pass)

;;;; Functions

(defun pass-extras-open-at-point ()
  "Open the URL of the entry at point and its password to the clipboard."
  (interactive)
  (when (derived-mode-p 'pass-mode)
    (pass-copy)
    (pass--with-closest-entry entry
      (let* ((inhibit-message t)
             (parsed-entries (password-store-parse-entry entry))
             (field password-store-url-field))
        (unless (assoc field parsed-entries)
          (user-error "Field `%s' not in  %s" field entry))
        (browse-url (password-store-get-field entry field))))))

(defun pass-extras-edit ()
  "Edit the entry at point, without confirmation."
  (interactive)
  (pass--with-closest-entry entry
    (password-store-edit entry)))

(defun pass-extras-git-sync ()
  "Synchronize with remote repository."
  (interactive)
  ;; (shell-command "pass git config pull.rebase false")
  (shell-command "pass git pull; pass git push"))

(defun pass-extras-insert-generated-no-symbols ()
  "Insert an entry to the password-store without symbols.
Use a generated password instead of reading the password from
user input."
  (interactive)
  (call-interactively #'password-store-generate-no-symbols)
  (pass-update-buffer))

(provide 'pass-extras)

;;; pass-extras.el ends here

