;;; register-extras.el --- Extensions for register -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/register-extras.el
;; Version: 0.2

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

;; Extensions for `register'.

;;; Code:

(require 'register)
(require 'transient)

;;;; Functions

;;;;; kill to register

;;;###autoload
(defun register-extras-kill-to-register (text register)
  "Copy TEXT from the kill ring to REGISTER."
  (interactive (list (read-from-kill-ring "Kill to store: ")
		     (register-read-with-preview "Register: ")))
  (set-register register text))

;;;;; buffer register

(defvar register-extras-keys-alist nil
  "Alist mapping characters to buffers for quick switching.
Each element is (KEY . BUFFER).")

(defun register-extras-buffer-to-register (key)
  "Associate KEY (a character) with the current buffer."
  (interactive "cSet buffer for key: ")
  (let ((buf (current-buffer)))
    ;; Remove existing binding for this key, if any
    (setq register-extras-keys-alist (assq-delete-all key register-extras-keys-alist))
    ;; Add new binding to the front
    (push (cons key buf) register-extras-keys-alist))
  (message "Associated key '%c' with buffer '%s'" key (buffer-name)))

(defun register-extras-jump-to-buffer (key)
  "Switch to the buffer associated with KEY."
  (interactive "cJump to buffer for key: ")
  (let* ((pair (assoc key register-extras-keys-alist))
         (buf (cdr pair)))
    (if (and buf (buffer-live-p buf))
        (switch-to-buffer buf)
      (message "No live buffer associated with key '%c'" key))))

;;;;; menu

;;;###autoload (autoload 'register-extras-dispatch "register-extras" nil t)
(transient-define-prefix register-extras-dispatch ()
  "Dispatch a `register' command."
  [["region"
    ("a" "append" append-to-register)
    ("c" "copy" copy-to-register)
    ("p" "prepend" prepend-to-register)]
   ["number"
    ("n" "store" number-to-register)
    ("t" "increment" increment-register)]
   ["kill ring"
    ("k" "store" register-extras-kill-to-register)]
   ["kmacro"
    ("m" "store" kmacro-to-register)
    ("j" "execute" jump-to-register)]
   ["rectangle"
    ("r" "copy" copy-rectangle-to-register)]
   ["window/frame config"
    ("f" "store frame" frameset-to-register)
    ("w" "store window" window-configuration-to-register)
    ("W" "store all windows" frame-configuration-to-register)]
   ["navigation"
    ("j" "jump" jump-to-register)
    ("o" "store point" point-to-register)]
   ["buffer"
    ("b" "jump" register-extras-jump-to-buffer)
    ("B" "store" register-extras-buffer-to-register)]
   ["display"
    ("h" "consult" consult-register)
    ("l" "list" list-registers)
    ("v" "view" view-register)]
   ["insert"
    ("i" "insert contents" insert-register)]])

(provide 'register-extras)
;;; register-extras.el ends here
