;;; dired-extras.el --- Extensions for dired -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/dired-extras.el
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

;; Extensions for Dired.

;;; Code:

(require 'dired)
(require 'el-patch)
(require 'gnus-dired)
(require 'paths)
(require 'shut-up)
(require 'transient)

;;;; Variables

(defvar dired-extras-show-dotfiles-p nil
  "Whether to show dot files in Dired.")

;;;; Functions

(defun dired-extras-copy-filename-as-kill-sans-extension ()
  "Copy name of file at point excluding its extension."
  (interactive)
  (kill-new (file-name-sans-extension (dired-copy-filename-as-kill))))

;; from emacswiki.org/emacs/DiredOmitMode
(defun dired-extras-dotfiles-toggle ()
  "Show/hide dot-files."
  (interactive)
  (when (derived-mode-p 'dired-mode)
    (if dired-extras-show-dotfiles-p
	(progn
	  (setq dired-extras-show-dotfiles-p nil)
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

;;;;; image-dired

(defun dired-extras-image-dired-current-directory ()
  "Run `image-dired' in the current directory."
  (interactive)
  (require 'image-dired)
  (image-dired-show-all-from-dir (dired-current-directory)))

;;;;; gnus-dired

;; replaces `gnus-dired-mail-buffers' function so it works on
;; `message-mode' derived modes, such as `mu4e-compose-mode'
;; djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html
(el-patch-defun gnus-dired-mail-buffers ()
  "Return a list of active mail composition buffers."
  (el-patch-swap
    (if (and (memq gnus-dired-mail-mode '(message-user-agent gnus-user-agent))
	     (require 'message)
	     (fboundp 'message-buffers))
	(message-buffers)
      ;; Cf. `message-buffers' in `message.el':
      (let (buffers)
	(save-excursion
	  (dolist (buffer (buffer-list t))
	    (set-buffer buffer)
	    (when (eq major-mode 'mail-mode)
	      (push (buffer-name buffer) buffers))))
	(nreverse buffers)))
    (let (buffers)
      (save-excursion
	(dolist (buffer (buffer-list t))
	  (set-buffer buffer)
	  (when (derived-mode-p 'mail-mode)
	    (push (buffer-name buffer) buffers))))
      (nreverse buffers))))

;;;;; dired-du

(declare-function dired-du-mode "dired-du")
(defun dired-extras-enable-dired-do-conditionally ()
  "Enable `dired-du-mode' iff `dired-hide-details-mode' is disabled."
  (let ((toggle (if dired-hide-details-mode -1 1)))
    (shut-up (dired-du-mode toggle))))

;;;;; Dispatcher

;;;###autoload (autoload 'dired-extras-dispatch "dired-extras" nil t)
(transient-define-prefix dired-extras-dispatch ()
  "Dispatcher for Dired."
  ["Dired folders"
   [("b" "bibliography" (lambda () (interactive) (dired paths-dir-personal-bibliography)))
    ("d" "dotfiles" (lambda () (interactive) (dired paths-dir-dotfiles)))
    ("e" "Emacs" (lambda () (interactive) (dired paths-dir-emacs)))
    ("i" "Anki" (lambda () (interactive) (dired paths-dir-anki)))
    ("n" "Notes" (lambda () (interactive) (dired paths-dir-notes)))
    ("o" "Google Drive" (lambda () (interactive) (dired paths-dir-google-drive)))
    ("p" "People" (lambda () (interactive) (dired paths-dir-people)))
    ("U" "Audiobooks" (lambda () (interactive) (dired paths-dir-audiobooks)))
    ("v" "movies" (lambda () (interactive) (dired paths-dir-movies)))
    ("w" "downloads" (lambda () (interactive) (dired paths-dir-downloads)))
    ("x" "Dropbox" (lambda () (interactive) (dired paths-dir-dropbox)))
    ("y" "Library: PDF" (lambda () (interactive) (dired paths-dir-pdf-library)))
    ("z" "Library: HTML" (lambda () (interactive) (dired paths-dir-html-library)))]
   ["Music"
    ("m c" "classical" (lambda () (interactive) (dired paths-dir-music-classical)))
    ("m p" "popular" (lambda () (interactive) (dired paths-dir-music-popular)))
    ("m t" "tango" (lambda () (interactive) (dired paths-dir-music-tango)))
    ("m s" "to sort" (lambda () (interactive) (dired paths-dir-music-to-sort)))]
   ["Special"
    ("." "File at point" (lambda () (interactive) (dired-at-point)))
    ("/" "Root" (lambda () (interactive) (dired "/")))
    ("SPC" "user" (lambda () (interactive) (dired "~/")))
    (";" "Current buffer" dired-jump)
    ("H-;" "Current buffer in other window" dired-jump-other-window)]
   ["Tlön: Google Drive"
    ("t H-b" "Google Drive: Babel" (lambda () (interactive) (dired paths-dir-google-drive-tlon-babel)))
    ("t H-n" "Google Drive: EAN" (lambda () (interactive) (dired paths-dir-google-drive-tlon-EAN)))
    ("t H-m" "Google Drive: FM" (lambda () (interactive) (dired paths-dir-google-drive-tlon-FM)))
    ("t H-g" "Google Drive: GPE" (lambda () (interactive) (dired paths-dir-google-drive-tlon-GPE)))
    ("t H-h" "Google Drive: HEAR" (lambda () (interactive) (dired paths-dir-google-drive-tlon-HEAR)))
    ("t H-d" "Google Drive: LBDLH" (lambda () (interactive) (dired paths-dir-google-drive-tlon-LBDLH)))
    ("t H-p" "Google Drive: LP" (lambda () (interactive) (dired paths-dir-google-drive-tlon-LP)))
    ("t H-r" "Google Drive: RAE" (lambda () (interactive) (dired paths-dir-google-drive-tlon-RAE)))
    ("t H-t" "Google Drive: tlon" (lambda () (interactive) (dired paths-dir-google-drive-tlon)))
    ("t H-c" "Google Drive: core" (lambda () (interactive) (dired paths-dir-google-drive-tlon-core)))
    ("t H-l" "Google Drive: leo" (lambda () (interactive) (dired paths-dir-google-drive-tlon-leo)))
    ("t H-f" "Google Drive: fede" (lambda () (interactive) (dired paths-dir-google-drive-tlon-fede)))]
   ["Tlön: Dropbox"
    ("t b" "Dropbox: Babel" (lambda () (interactive) (dired paths-dir-dropbox-tlon-babel)))
    ("t n" "Dropbox: EAN" (lambda () (interactive) (dired paths-dir-dropbox-tlon-EAN)))
    ("t m" "Dropbox: FM" (lambda () (interactive) (dired paths-dir-dropbox-tlon-FM)))
    ("t g" "Dropbox: GPE" (lambda () (interactive) (dired paths-dir-dropbox-tlon-GPE)))
    ("t h" "Dropbox: HEAR" (lambda () (interactive) (dired paths-dir-dropbox-tlon-HEAR)))
    ("t d" "Dropbox: LBDLH" (lambda () (interactive) (dired paths-dir-dropbox-tlon-LBDLH)))
    ("t p" "Dropbox: LP" (lambda () (interactive) (dired paths-dir-dropbox-tlon-LP)))
    ("t r" "Dropbox: RAE" (lambda () (interactive) (dired paths-dir-dropbox-tlon-RAE)))
    ("t t" "Dropbox: tlon" (lambda () (interactive) (dired paths-dir-dropbox-tlon)))
    ("t c" "Dropbox: core" (lambda () (interactive) (dired paths-dir-dropbox-tlon-core)))
    ("t f" "Dropbox: fede" (lambda () (interactive) (dired paths-dir-dropbox-tlon-fede)))
    ("t l" "Dropbox: leo" (lambda () (interactive) (dired paths-dir-dropbox-tlon-leo)))]])

(provide 'dired-extras)
;;; dired-extras.el ends here
