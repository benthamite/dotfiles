;;; pass-extras.el --- Exteions for pass -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/pass-extras.el
;; Version: 0.2
;; Package-Requires: ((pass "1.0.0") (password-generator "1.20"))

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
(require 'password-generator)

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

(defun pass-extras-insert-generated-no-symbols ()
  "Insert an entry to the password-store without symbols.
Use a generated password instead of reading the password from
user input."
  (interactive)
  (call-interactively #'password-store-generate-no-symbols)
  (pass-update-buffer))

;;;;; Keys

;;;###autoload
(defun pass-extras-store-key (file entry)
  "Store a key in FILE as an ENTRY in `pass'."
  (interactive (list (read-file-name "Key file: ")
		     (completing-read "Entry: " (password-store-list))))
  (shell-command (format "cat %s | pass insert --multiline %s" file entry)))

;;;###autoload
(defun pass-extras-export-key (entry file)
  "Export `pass' ENTRY to FILE."
  (interactive (list (completing-read "Entry: " (password-store-list) nil 'match)
		     (read-file-name "Export to: ")))
  (shell-command (format "pass %s > %s" entry file))
  (message "Key exported to `%s'" file))

(autoload 'password-store-list "password-store")
;;;###autoload
(defun pass-extras-git-crypt-unlock (&optional repo entry)
  "Unlock `git-crypt' REPO with key stored in ENTRY."
  (interactive)
  (let* ((default-directory (or repo default-directory))
	 (entry (or entry (completing-read "Key: " (password-store-list) nil 'match)))
	 (output (call-process-shell-command (format "git-crypt unlock <(pass %s)" entry))))
    (if (zerop output)
	(message "Unlocked repository `%s'" default-directory)
      (message "Error unlocking repository `%s'. Perhaps the repo is dirty?" default-directory))))

;;;;; Generate

;;;###autoload
(defun pass-extras-generate-password ()
  "Generate a password and copy it to clipboard.
Prompts for password type and copies result to clipboard."
  (interactive)
  (let* ((types '(("Simple (alphanumeric)" . password-generator-simple)
                  ("Strong (with special chars)" . password-generator-strong)
                  ("Numeric (PIN)" . password-generator-numeric)
                  ("Phonetic (memorable)" . password-generator-phonetic)
                  ("Words (correct horse battery)" . password-generator-words)
                  ("Custom (your alphabet)" . password-generator-custom)))
         (choice (completing-read "Password type: " types nil t))
         (generator-fn (cdr (assoc choice types)))
         (password (funcall generator-fn nil t)))
    ;; Clear previous password from kill ring
    (password-store-clear 'generated)
    ;; Add new password to kill ring and track its position
    (kill-new password)
    (setq password-store-kill-ring-pointer kill-ring-yank-pointer)
    (message "Password copied to clipboard. Will clear in %s seconds."
             password-store-time-before-clipboard-restore)
    ;; Set timer to clear password
    (when password-store-timeout-timer
      (cancel-timer password-store-timeout-timer))
    (setq password-store-timeout-timer
          (run-at-time password-store-time-before-clipboard-restore nil
                       #'password-store-clear 'generated))))

(provide 'pass-extras)
;;; pass-extras.el ends here

