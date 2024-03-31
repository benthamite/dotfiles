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

;;;;; Rate

(defcustom read-aloud-extras-rate 200
  "The rate at which to read text aloud, in words per minute."
  :type 'integer
  :group 'read-aloud-extras)

(defcustom read-aloud-extras-rate-change 10
  "The magnitude of the rate increment or decrement."
  :type 'integer
  :group 'read-aloud-extras)

;;;;; Voice

(defcustom read-aloud-extras-voice ""
  "The rate voice that will read the text aloud."
  :type 'string
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
    (cmd "say" args (,(format "-v %s" read-aloud-extras-voice)
		     ,(format "-r %s" read-aloud-extras-rate)))))

;;;;; Rate


(defun read-aloud-extras-change-rate (sign)
  "Increase or decrease the rate at which to read text aloud, depending on SIGN."
  (message "Rate is now %d"
	   (setq read-aloud-extras-rate
		 (funcall sign read-aloud-extras-rate read-aloud-extras-rate-change))))

(defun read-aloud-extras-increase-rate ()
  "Increase the rate at which to read text aloud."
  (interactive)
  (read-aloud-extras-change-rate #'+))

(defun read-aloud-extras-decrease-rate ()
  "Decrease the rate at which to read text aloud."
  (interactive)
  (read-aloud-extras-change-rate #'-))

;;;;; Voice


(defun read-aloud-extras-voice-select ()
  "Select the voice that will read the text aloud."
  (interactive)
  (pcase read-aloud-engine
    ("say" (setq read-aloud-extras-voice (read-aloud-voice-select-say)))
    (_ (user-error "Currently this function is only available for the 'say' engine. Sorry"))))

(defun read-aloud-voice-select-say ()
  "Prompt the user to select a `say' voice."
  (let* ((shell-output (shell-command-to-string "say -v \\?"))
         (lines (split-string shell-output "\n" t))
         (collection (mapcar (lambda (line)
			       (when (string-match "\\(.*?\\)\\(\\s-+\\)\\([a-z_]*\\)\\(\\s-+\\)#" line)
				 (let* ((name (match-string 1 line))
					(lang (match-string 3 line)))
				   (cons (format "%-36s%s" name lang) name))))
			     lines))
         (collection (delq nil collection)) ; remove nil entries
         (prompt "Pick a voice: ")
	 (selection (completing-read prompt collection nil t)))
    (alist-get selection collection nil nil #'string=)))

;;;;; Patched functions

;; allow changing the rate at which text is read aloud
(el-patch-defun read-aloud--args ()
  (plist-get (lax-plist-get (el-patch-swap read-aloud-engines (read-aloud-extras-engines))
			    read-aloud-engine)
	     'args))

;; allow reading text in PDFs
(declare-function pdf-view-active-region-p "pdf-view")
(declare-function pdf-view-active-region-text "pdf-view")
(el-patch-cl-defun read-aloud-this()
  "Pronounce either the selection or a word under the pointer."
  (interactive)

  (when read-aloud--c-locked
    (read-aloud-stop)
    (cl-return-from read-aloud-selection))

  (if (el-patch-swap (use-region-p) (or (use-region-p) (pdf-view-active-region-p)))
      (read-aloud--string
       (el-patch-swap (buffer-substring-no-properties (region-beginning) (region-end))
		      (if (derived-mode-p 'pdf-view-mode)
			  (car (pdf-view-active-region-text))
			(buffer-substring-no-properties (region-beginning) (region-end))))
       "selection")
    (read-aloud--current-word)) )

(provide 'read-aloud-extras)
;;; read-aloud-extras.el ends here
