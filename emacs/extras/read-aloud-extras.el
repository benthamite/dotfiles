;;; read-aloud-extras.el --- Extensions for read-aloud -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/read-aloud-extras.el
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

;; Extensions for `read-aloud'.

;;; Code:

(require 'el-patch)
(require 'read-aloud)

;;;; User options

(defgroup read-aloud-extras ()
  "Extensions for `read-aloud'."
  :group 'emacs)

(defcustom read-aloud-extras-rate 200
  "The rate at which to read text aloud, in words per minute."
  :type 'integer
  :group 'read-aloud-extras)

(defcustom read-aloud-extras-rate-change 10
  "The magnitude of the rate increment or decrement."
  :type 'integer
  :group 'read-aloud-extras)

;;;; Functions

(defun read-aloud-extras-engines ()
  "Set `read-aloud' engines dynamically."
  `("speech-dispatcher"		; Linux/FreeBSD only
    (cmd "spd-say" args ("-e" "-w") kill "spd-say -S")
    "flite"				; Cygwin?
    (cmd "flite" args nil)
    "jampal"				; Windows
    (cmd "cscript" args ("C:\\Program Files\\Jampal\\ptts.vbs" "-r" "5"))
    "say"				; macOS
    (cmd "say" args (,(format "-r %s" (read-aloud-extras-rate))))))

(defun read-aloud-extras-rate ()
  "Return the rate at which to read text aloud, in words per minute."
  read-aloud-extras-rate)

(defun read-aloud-extras-change-rate (sign)
  "Increase or decrease the rate at which to read text aloud.
SIGN is the sign of the change: 1 for increase, -1 for decrease."
  (pcase sign
    (1 (setq read-aloud-extras-rate
	     (+ read-aloud-extras-rate read-aloud-extras-rate-change)))
    (-1 (setq read-aloud-extras-rate
	      (- read-aloud-extras-rate read-aloud-extras-rate-change))))
  (message "Rate is now %d" read-aloud-extras-rate))

(defun read-aloud-extras-increase-rate ()
  "Increase the rate at which to read text aloud."
  (interactive)
  (read-aloud-extras-change-rate 1))

(defun read-aloud-extras-decrease-rate ()
  "Decrease the rate at which to read text aloud."
  (interactive)
  (read-aloud-extras-change-rate -1))

;;;;; Patched functions

;; allow changing the rate at which text is read aloud
(el-patch-defun read-aloud--args ()
  (plist-get (lax-plist-get (el-patch-swap read-aloud-engines (read-aloud-extras-engines))
			    read-aloud-engine)
	     'args))

(provide 'read-aloud-extras)
;;; read-aloud-extras.el ends here
