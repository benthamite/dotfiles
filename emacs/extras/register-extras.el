;;; register-extras.el --- Extensions for register -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/register-extras.el
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

;; Extensions for `register'.

;;; Code:

(require 'register)
(require 'transient)

;;;; Functions

;;;###autoload
(defun register-extras-kill-to-register (text register)
  "Copy TEXT from the kill ring to REGISTER."
  (interactive (list (read-from-kill-ring "Kill to store: ")
		     (register-read-with-preview "Register: ")))
  (set-register register text))

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
   ["display"
    ("h" "consult" consult-register)
    ("l" "list" list-registers)
    ("v" "view" view-register)]
   ["insert"
    ("i" "insert contents" insert-register)]])

(provide 'register-extras)
;;; register-extras.el ends here
