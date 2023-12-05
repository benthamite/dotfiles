;;; files-extras.el --- Extensions for files.el -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/files-extras.el
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

;; Extensions for `files.el'.

;;; Code:

(require 'files)

;;;; User options

(defgroup files-extras ()
  "Extensions for `files'."
  :group 'files)

(defcustom files-extras-new-empty-buffer-major-mode 'org-mode
  "Major mode to use for new empty buffers."
  :type 'symbol
  :group 'files-extras)

;;;; Functions

;; christiantietze.de/posts/2021/06/emacs-trash-file-macos/
(defun files-extras-system-move-file-to-trash (filename)
  "Move file or directory named FILENAME to the recycle bin.
This function overrides `system-move-file-to-trash' to use delete files using
the `trash' utility. Deleting files in this way supports the \"Put Back\"
functionality in macOS."
  (unless (executable-find "trash")
    (user-error "`trash' not found; please install it (e.g. `brew install trash')"))
  (shell-command (concat "trash -vF \"" filename "\""
			 "| sed -e 's/^/Trashed: /'")
		 nil ;; Name of output buffer
		 "*Trash Error Buffer*"))

(advice-add 'system-move-file-to-trash :override #'files-extras-system-move-file-to-trash)

(defun files-extras-save-and-revert-buffer ()
  "Save buffer, then revert it."
  (interactive)
  (save-buffer)
  (revert-buffer nil t))

(defun files-extras-bury-scratch-buffer ()
  "When trying to kill `*scratch' buffer, bury it instead."
  (if (not (equal (buffer-name) "*scratch*"))
      t
    (bury-buffer)
    nil))

(add-hook 'kill-buffer-query-functions #'files-extras-bury-scratch-buffer)

;; Adapted from `spacemacs/new-empty-buffer'.
(defun files-extras-new-empty-buffer (&optional)
  "Create a new buffer called `untitled<n>'."
  (interactive)
  (let ((newbuf (generate-new-buffer "untitled")))
    ;; Prompt to save on `save-some-buffers' with positive PRED
    (with-current-buffer newbuf
      (setq-local buffer-offer-save t)
      (when files-extras-new-empty-buffer-major-mode
	(funcall files-extras-new-empty-buffer-major-mode)))
    (switch-to-buffer newbuf nil 'force-same-window)))


(defun files-extras-new-buffer-in-current-mode ()
  "Create a new buffer in the same major mode as the current buffer."
  (interactive)
  (let ((buffer-name (generate-new-buffer "untitled"))
	(buffer-major-mode major-mode))
    (cond ((eq buffer-major-mode 'shell-mode)
	   (shell))
	  ((eq buffer-major-mode 'eshell-mode)
	   (eshell))
	  (t
	   ;; Prompt to save on `save-some-buffers' with positive PRED
	   (with-current-buffer buffer-name
	     (setq-local buffer-offer-save t)
	     (funcall buffer-major-mode))
	   (switch-to-buffer buffer-name nil 'force-same-window)))))

(defun files-extras-save-all-buffers ()
  "Save all file-visiting buffers."
  (interactive)
  (save-some-buffers
   `(4)))

(defun files-extras-eval-region-or-buffer ()
  "Evaluate a region if selected, otherwise the whole buffer."
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end))
    (eval-buffer)))

(defun files-extras-get-alternate-buffer ()
  "Return name of last buffer active in the current window."
  (let ((current-buffer (window-buffer))
	(buffer-predicate
	 (frame-parameter (window-frame) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (or (cl-find-if (lambda (buffer)
		      (and (not (eq buffer current-buffer))
			   (or (null buffer-predicate)
			       (funcall buffer-predicate buffer))))
		    (mapcar #'car (window-prev-buffers)))
	;; `other-buffer' honors `buffer-predicate' so no need to filter
	(other-buffer current-buffer t))))

(defun files-extras-switch-to-alternate-buffer ()
  "Switch to the last buffer active in the current window."
  (interactive)
  (switch-to-buffer (files-extras-get-alternate-buffer)))

;; reddit.com/r/emacs/comments/64xb3q/killthisbuffer_sometimes_just_stops_working/
(defun files-extras-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun file-extras-kill-other-buffer ()
  "Kill the buffer in the other window."
  (interactive)
  (save-window-excursion
    (other-window 1)
    (files-extras-kill-this-buffer)))

(defun files-extras-kill-this-buffer-switch-to-other-window ()
  "Kill the current buffer and switch to the other window."
  (interactive)
  (files-extras-kill-this-buffer)
  (window-extras-switch-to-last-window))

(defun files-extras-kill-all-file-visiting-buffers (&optional excluded-files)
  "Kill all open buffers visiting a file except those visiting any of EXCLUDED-FILES."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer
	    (and (buffer-file-name)
		 (not (member (buffer-file-name) excluded-files))))
      (kill-buffer buffer))))

(defun files-extras-bury-buffer-switch-to-other-window ()
  "Bury the current buffer and switch to the other window."
  (interactive)
  (bury-buffer)
  (window-extras-switch-to-last-window))

(defun files-extras-download-bypass-paywalls-chrome ()
  "Download and install `Bypass Paywalls Chrome Clean'.
After running the command, both the Chrome extensions page and
the `bypass-paywalls-chrome-clean-master' folder will open.
To install the extension, drag the latter onto the former."
  (interactive)
  (let* ((file (file-name-concat paths-dir-downloads "bypass-paywalls.zip"))
	 (dir (file-name-concat paths-dir-downloads "bypass-paywalls-chrome-clean-master"))
	 (reveal-in-osx
	  (concat
	   "set thePath to POSIX file \"" dir "\"\n"
	   "tell application \"Finder\"\n"
	   " set frontmost to true\n"
	   " reveal thePath \n"
	   "end tell\n")))
    (url-copy-file "https://gitlab.com/magnolia1234/bypass-paywalls-chrome-clean/-/archive/master/bypass-paywalls-chrome-clean-master.zip" file)
    (shell-command (format "unzip %s -d %s" file paths-dir-downloads))
    (delete-file file)
    ;; open Chrome extensions page
    (shell-command "osascript -e 'tell application \"Keyboard Maestro Engine\" to do script \"89243CDA-4876-45C8-9AF2-3666664A0EAA\"'")
    (start-process "osascript-getinfo" nil "osascript" "-e" reveal-in-osx)))

;; Copied from emacs.stackexchange.com/a/24461/32089
(defun files-extras-revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in Emacs will not be reverted. They
will be reverted though if they were modified outside Emacs. Buffers visiting
files which do not exist any more or are no longer readable will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
		 (not (buffer-modified-p buf)))
	(if (file-readable-p filename)
	    ;; If the file exists and is readable, revert the buffer.
	    (with-current-buffer buf
	      (revert-buffer :ignore-auto :noconfirm :preserve-modes))
	  ;; Otherwise, kill the buffer.
	  (let (kill-buffer-query-functions) ; No query done when killing buffer
	    (kill-buffer buf)
	    (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(defun files-extras-show-buffer-name ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((buffer-name (buffer-name)))
    (if buffer-name
	(progn
	  (message buffer-name)
	  (kill-new buffer-name))
      (error "Buffer not visiting a file"))))

;; On MacOS, `DS_Store' files can interfere with this command.
(defun files-extras-newest-file (path)
  "Get latest file in PATH, excluding `.DS_Store` and `.localized` files."
  (car
   (seq-find
    #'(lambda (x)
	(let ((file-name (file-name-nondirectory (car x))))
	  (and (not (nth 1 x)) ; non-directory
	       (not (string= file-name ".DS_Store"))
	       (not (string= file-name ".localized")))))
    (sort
     (directory-files-and-attributes path 'full nil t)
     #'(lambda (x y) (time-less-p (nth 5 y) (nth 5 x)))))))

(defun files-extras-switch-to-most-recent-buffer-in-mode (mode)
  "Switch to the most recent buffer in major mode MODE."
  (let (found)
    (catch 'done
      (mapc (lambda (x)
	      (when (with-current-buffer x (eq major-mode mode))
		(switch-to-buffer x)
		(setq found t)
		(throw 'done nil)))
	    (buffer-list))
      (unless found
	(print "not found")))))

;; stackoverflow.com/questions/21486934/file-specific-key-binding-in-emacs/21493693#21493693
(defun files-extras-buffer-local-set-key (key command)
  "Bind KEY to COMMAND in current buffer only."
  (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
  (let ((oldmap (current-local-map))
	(newmap (make-sparse-keymap)))
    (when oldmap
      (set-keymap-parent newmap oldmap))
    (define-key newmap key command)
    (use-local-map newmap)))

(defun files-extras-ocr-pdf (arg &optional filename parameters)
  "OCR the FILENAME.
If FILENAME is nil, use the PDF file at point or the file visited by the current
buffer. Optionally, pass PARAMETERS to `ocrmypdf'. With prefix argument ARG,
force OCR even if it has already been performed on the file."
  (interactive "P")
  (unless (executable-find "ocrmypdf")
    (user-error "`ocrmypdf' not found. Please install it (e.g. `brew install ocrmypdf'"))
  (let* ((filename (or filename
		       (pcase major-mode
			 ('dired-mode (dired-get-filename))
			 ('pdf-view-mode (buffer-file-name))))))
    (unless (string= (file-name-extension filename) "pdf")
      (user-error "File is not a PDF"))
    (let* ((parameters (or parameters
			   (format (concat (when arg "--force-ocr ") "--deskew '%s' '%s'") filename filename)))
	   (process (start-process-shell-command
		     "ocrmypdf" "*ocr-pdf*"
		     (concat "ocrmypdf " parameters))))
      (set-process-filter process 'files-extras-ocr-pdf-process-filter))))

(defun files-extras-ocr-pdf-process-filter (process string)
  "Process filter function to handle output from `ocrmypdf'.
This function gets STRING when PROCESS produces output."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (cond ((string-match-p "PriorOcrFoundError: page already has text" string)
	     (message "OCR already performed on this file."))
	    ;; when invoked with `--force-ocr'
	    ((string-match-p "page already has text" string)
	     (message "OCR already performed on this file; forcing new OCR."))
	    ;; silence irrelevant messages
	    ((or (string-match-p "Scanning contents" string)
		 (string-match-p "Start processing" string)
		 (string-match-p "Recompressing JPEGs" string)
		 (string-match-p "Deflating JPEGs" string)
		 (string-match-p "empty page" string)))
	    ;; print all other messages
	    (t (princ string))))))

(defun files-extras-get-stem-of-current-buffer ()
  "Return the stem of the current buffer."
  (when-let ((file-name buffer-file-name))
    (file-name-base file-name)))

(defun file-extras-bollp ()
  "Return t if point is at the beginning of the last line."
  (let ((beginning-of-last-line
	 (save-excursion
	   (end-of-buffer)
	   (beginning-of-line)
	   (point))))
    (>= (point) beginning-of-last-line)))

(defun files-extras-recover-all-files ()
  "Recover all files with auto-save data in the `auto-save' directory.
One normally uses `recover-session' for this, but when Emacs crashes a session
may fail to be created and then each file has to be recovered separately. This
command automates the recovery process in these cases."
  (interactive)
  (dolist (file (directory-files (file-name-concat paths-dir-chemacs-profiles "var/auto-save")))
    (when-let ((file-to-recover (string-replace "#" "" file)))
      (ignore-errors (recover-file (string-replace "!" "/" file-to-recover)))
      (files-extras-diff-buffer-with-file))))


(defun files-extras-auto-save-alert ()
  "Alert user when auto save data is detected.
`recover-this-file' notifications are easy to miss. This function triggers a
more intrusive alert."
  (when (and (not buffer-read-only)
	     (file-newer-than-file-p (or buffer-auto-save-file-name
					 (make-auto-save-file-name))
				     buffer-file-name)
	     (alert (format "%s has auto save data"
			    (file-name-nondirectory buffer-file-name))
		    :title "Auto save detected"
		    :severity 'high))))

;; for some reason, `alert' fails to create persistent alerts. so we
;; trigger a warning if either `*log4e-alert*' or `*Messages*'
;; buffers have logged a message related to `recover-this-file'.
(defun files-extras-auto-save-persist ()
  "Prevent killing buffer when auto save data is detected."
  ;; FIXME: This doesn't work
  (alert--log-open-log
   ;; we check both `*log4e-alert*' and `*Messages*' buffers for
   ;; extra safety
   (let ((alert-buffers '(" *log4e-alert*" "*Messages*")))
     (dolist (buffer alert-buffers)
       (when (get-buffer buffer)
	 (set-buffer buffer)
	 (goto-char (point-min))
	 (when (search-forward "has auto save data" nil t)
	   (yes-or-no-p "Buffers with auto save data detected. Check `*log4e-alert*' and `*Messages*' for details. Are you sure you want to proceed? "))
	 (kill-buffer))))))

;; https://emacs.stackexchange.com/a/3778/32089
(defun files-extras-diff-buffer-with-file ()
  "Compare the current modified buffer with the saved version."
  (interactive)
  (let ((diff-switches "-u")) ;; unified diff
    (diff-buffer-with-file (current-buffer))))

(advice-add 'recover-this-file :after #'files-extras-diff-buffer-with-file)

(defun files-extras-copy-current-path ()
  "Copy the path of the current buffer to the kill ring."
  (interactive)
  (let ((path (or (buffer-file-name) default-directory)))
    (kill-new path)
    (message "Copied `%s'" path)))

(add-hook 'find-file-hook #'files-extras-auto-save-alert)
;; (add-hook 'kill-buffer-query-functions #'files-extras-auto-save-persist)

;; reddit.com/r/emacs/comments/t07e7e/comment/hy88bum/?utm_source=reddit&utm_medium=web2x&context=3
(defun files-extras-make-hashed-auto-save-file-name-a (fn)
  "Compress the `auto-save' file name so paths don't get too long.
FN is an argument in the adviced function."
  (let ((buffer-file-name
	 (if (or (null buffer-file-name)
		 (find-file-name-handler buffer-file-name 'make-auto-save-file-name))
	     buffer-file-name
	   (sha1 buffer-file-name))))
    (funcall fn)))

(advice-add #'make-auto-save-file-name :around #'files-extras-make-hashed-auto-save-file-name-a)

(defun files-extras-make-hashed-backup-file-name-a (fn file)
  "A few places use the backup file name so paths don't get too long.
FN and FILE are arguments in the adviced function."
  (let ((alist backup-directory-alist)
	backup-directory)
    (while alist
      (let ((elt (car alist)))
	(if (string-match (car elt) file)
	    (setq backup-directory (cdr elt) alist nil)
	  (setq alist (cdr alist)))))
    (let ((file (funcall fn file)))
      (if (or (null backup-directory)
	      (not (file-name-absolute-p backup-directory)))
	  file
	(expand-file-name (sha1 (file-name-nondirectory file))
			  (file-name-directory file))))))

(advice-add #'make-backup-file-name-1 :around #'files-extras-make-hashed-backup-file-name-a)

(defun files-extras-open-buffer-files ()
  "Return the list of files currently open in Emacs."
  (delq nil
	(mapcar (lambda (x)
		  (if (and (buffer-file-name x)
			   (string-match "\\.org$"
					 (buffer-file-name x)))
		      (buffer-file-name x)))
		(buffer-list))))

;; https://emacs.stackexchange.com/a/5531/32089
(defvar files-extras-walk-dir-locals-upward nil
  "If non-nil, chain `.dir-locals.el' files.")

(defun files-extras-walk-dir-locals-file (old-fun &rest args)
  "Chain `.dir-locals.el' files.
Evaluate `dir-locals.el' files starting in the current directory and going up.
Otherwise they will be evaluated from the top down to the current directory.
OLD-FUN and ARGS are arguments passed to the original function."
  (let* ((dir-locals-list (list dir-locals-file))
	 (walk-dir-locals-file (car dir-locals-list)))
    (while (file-readable-p (concat "../" walk-dir-locals-file))
      (progn
	(setq walk-dir-locals-file (concat "../" walk-dir-locals-file))
	(add-to-list 'dir-locals-list walk-dir-locals-file
		     files-extras-walk-dir-locals-upward)))
    (dolist (file dir-locals-list)
      (let ((dir-locals-file (expand-file-name file)))
	(apply old-fun args)))))

;; (advice-add 'hack-dir-local-variables :around #'files-extras-walk-dir-locals-file)

;; consider binding this to something
(defun files-extras-convert-image-to-pdf ()
  "Convert image at point to PDF."
  (interactive)
  (let ((file (buffer-file-name)))
    (shell-command (format "convert '%s' '%s.pdf'" file (file-name-sans-extension file)))
    (message "Converted image to PDF.")))

(defun files-extras-open-elpaca-package (package)
  "Open the package named PACKAGE in the `repos' elpaca directory."
  (require 'elpaca)
  (let ((file (file-name-concat elpaca-repos-directory
				package
				(file-name-with-extension package "el"))))
    (find-file file)))

(defun files-extras-open-extras-package ()
  "Prompt the user to select an `extras' package and open it."
  (let* ((files (directory-files paths-dir-extras t directory-files-no-dot-files-regexp))
	 (file-names (mapcar #'file-name-nondirectory files))
	 (selection (completing-read "Package: " file-names nil t))
	 (file (file-name-concat paths-dir-extras selection)))
    (find-file file)))

;; TODO: Expand for other modes
(defun file-extras-copy-as-kill-dwim ()
  "Copy the relevant string in the current buffer, depending on its mode.
- In a `helpful-mode' buffer, get the name of the symbol whose docstring the
current helpful buffer displays, then kill the buffer."
  (interactive)
  (pcase major-mode
    ('helpful-mode (kill-new (replace-regexp-in-string "\\(\\*helpful .*: \\)\\(.*\\)\\(\\*\\)" "\\2" (buffer-name)))
		   (files-extras-kill-this-buffer-switch-to-other-window))))

(defun file-extras-grammarly-open-in-external-editor ()
  "Open Grammarly's external editor."
  (interactive)
  (browse-url "https://app.grammarly.com/ddocs/1929393566"))

(defun file-extras-remove-extra-blank-lines ()
  "Remove extra blank lines from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(^\\s-*$\\)\n\\(\\(^\\s-*$\\)\n\\)+" nil t)
      (replace-match "\n"))))

(provide 'files-extras)
;;; files-extras.el ends here
