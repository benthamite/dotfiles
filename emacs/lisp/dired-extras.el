;;; dired-extras.el --- Extensions for dired -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/lisp/dired-extras.el
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

;; Extensions for dired.

;;; Code:

(require 'dired)
(require 'el-patch)

;;;; Functions

(defun dired-extras-copy-filename-as-kill-sans-extension ()
  "Copy name of file at point excluding its extension."
  (interactive)
  (kill-new (file-name-sans-extension (dired-copy-filename-as-kill))))

;; from emacswiki.org/emacs/DiredOmitMode
(defun dired-extras-dotfiles-toggle ()
  "Show/hide dot-files."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
	(progn
	  (set (make-local-variable 'dired-dotfiles-show-p) nil)
	  (message "h")
	  (dired-mark-files-regexp "^\\\.")
	  (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
	     (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun dired-extras-mark-screenshots ()
  "Mark all screenshot files."
  (interactive)
  (dired-mark-files-regexp "Screenshot [[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\} at [[:digit:]]\\{2\\}.[[:digit:]]\\{2\\}.[[:digit:]]\\{2\\}.png"))

(defun dired-extras-up-directory-reuse ()
  "Like `dired-up-directory, but reuse current buffer."
  (interactive)
  (find-alternate-file ".."))

(defun dired-extras-copy-filename-as-kill-absolute ()
  "Copy absolute names of marked (or next ARG) files into the kill ring."
  (interactive)
  (dired-copy-filename-as-kill '(0)))

(defun dired-extras-copy-to-remote-docs-directory ()
  "Copy marked files to `stafforini.com/docs'.
If no files are marked, copy file at point instead."
  (interactive)
  (dolist (file (dired-get-marked-files nil nil nil t))
    (shell-command (format "scp '%s' 'ab80508@108.167.182.246:/home2/ab80508/public_html/stafforini.com/docs/'" file)))
  (kill-new (concat "https://stafforini.com/docs/" (dired-copy-filename-as-kill))))

;; emacs.stackexchange.com/a/30681/32089
(define-advice dired-clean-up-after-deletion
    (:around (old-fun &rest r) kill-dired-buffer-quietly)
  (define-advice y-or-n-p (:around (old-fun prompt) just-yes)
    (if (or (string-prefix-p "Kill Dired buffer" prompt)
	    (string-prefix-p "Kill buffer of" prompt))
	t
      (funcall old-fun prompt)))
  (unwind-protect (apply old-fun r)
    (advice-remove 'y-or-n-p #'y-or-n-p@just-yes)))

(defun dired-extras-do-delete-fast (&optional arg)
  "Delete all marked (or next ARG) files, without using the `trash' utility.
This command let's you delete large numbers of files quickly, at the expense of
losing the `put back' option."
  (interactive)
  (cl-letf (((symbol-function 'system-move-file-to-trash) nil))
    (dired-do-delete arg)))

;; emacs.stackexchange.com/a/60663/32089
;; consider binding this to something
(defun dired-extras-duplicate-this-file ()
  "Duplicate file at point."
  (interactive)
  (let* ((existing-file (dired-get-filename t))
	 (existing-file-stem (file-name-sans-extension existing-file))
	 (existing-file-extension (file-name-extension existing-file))
	 (counter 1)
	 (new-file (format "%s[%d].%s" existing-file-stem counter existing-file-extension)))
    (while (file-exists-p new-file)
      (setq counter (1+ counter)
	    new-file (format "%s[%d].%s" existing-file-stem counter existing-file-extension)))
    (copy-file existing-file new-file))
  (revert-buffer))

;;;; image-dired

(defun dired-extras-image-dired-current-directory ()
  "Run `image-dired' in the current directory."
  (interactive)
  (require 'image-dired)
  (image-dired-show-all-from-dir (dired-current-directory)))

;;;; all-the-icons-dired

(defun dired-extras-all-the-icons-activate ()
  "Define conditions for activation of `all-the-icons-dired-mode'."
  (require 'all-the-icons-dired)
  (if (< (length (directory-files default-directory)) 1000)
      (all-the-icons-dired-mode)
    (all-the-icons-dired-mode -1)))

(provide 'dired-extras)
;;; dired-extras.el ends here
