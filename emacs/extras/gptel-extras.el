;;; gptel-extras.el --- Extensions for gptel -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/gptel-extras.el
;; Version: 0.2
;; Package-Requires: ((el-patch "3.1") (gptel "0.7.1") (paths "0.1"))

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

;; Extensions for `gptel'.

;;; Code:

(require 'el-patch)
(require 'gptel)
(require 'gptel-context)
(require 'paths)

;;;; User options

(defgroup gptel-extras ()
  "Extensions for `gptel'."
  :group 'gptel)

;;;;; Misc

(defcustom gptel-extras-gemini-mullvad-disconnect-after 1
  "The number of minutes to disconnect `mullvad' after starting the Gemini session."
  :type 'integer
  :group 'gptel-extras)

(defcustom gptel-extras-dir (file-name-concat paths-dir-notes "gptel/")
  "The directory where to save the `gptel' buffers.
In Dired, this directory is sorted chronologically rather than alphabetically,
since the typical use case is to look for a recently modified `gptel' file. This
directory-local sorting is set via a the `.dir-locals.el' file in the directory."
  :type 'directory
  :group 'gptel-extras)

(defcustom gptel-extras-alert-when-finished '()
  "List of models for which an alert is displayed when the response is inserted.
If t, always display an alert. If nil, never display an alert."
  :type '(choice (const :tag "Always" t)
		 (const :tag "Never" nil)
		 (repeat :tag "Models" symbol))
  :group 'gptel-extras)

;;;; Variables

(defconst gptel-extras-changelog-file
  (file-name-concat paths-dir-dotemacs "extras/gptel-extras-changelog-template.org")
  "The file with the changelog template for `gptel-extras-summarize-commit-diffs'.")

;;;; Functions

;;;;; Summarize commit diffs

(autoload 'magit-commit-at-point "magit-git")
(autoload 'magit-git-insert "magit-git")
(defun gptel-extras-summarize-commit-diffs (beg end &optional include-stats)
  "Summarize the diffs of commits in the selected region using an LLM.
BEG and END mark the region of commits to summarize in `magit-log-mode'. When
INCLUDE-STATS is non-nil (with prefix arg), include diffstats in the prompt."
  (interactive "r\nP")
  (unless (derived-mode-p 'magit-log-mode)
    (user-error "This function is meant to be called from the Magit log (`M-x magit RET ll')"))
  (save-excursion
    (let* ((commits (save-restriction
		      (narrow-to-region beg end)
		      (goto-char (point-min))
		      ;; Get list of selected commit hashes
		      (cl-loop while (not (eobp))
			       collect (magit-commit-at-point)
			       do (forward-line))))
	   ;; Get diffs of all commits
	   (commit-diffs
	    (with-temp-buffer
	      (magit-git-insert "show" "--patch"
				(and (not include-stats) "--unified=3")
				commits)
	      (buffer-string)))
	   ;; Create appropriate prompt
	   (prompt (format
		    "Here are several git commit diffs:\n\n%s\n\nPlease analyze these commits and
provide a concise summary of the main changes. Include any significant patterns \
you notice. Write the summary using org-mode syntax, NOT Markdown. When writing the summary, \
focus on making it useful for someone who is already familiar with the code and \
wants to learn about the changes made in these commits, so that they can quickly \
determine if they need to handle any breaking changes or if they want to start \
using any of the new functionality. Organize the summary into sections, one for \
each package or feature, following this model:\n\n%s"
		    commit-diffs
		    (let ((file gptel-extras-changelog-file))
		      (with-temp-buffer (insert-file-contents file) (buffer-string)))))
	   (gptel-stream t))
      ;; Display summary in new buffer
      (with-current-buffer (get-buffer-create "*Commit Summary*")
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (read-only-mode -1)
	  (insert "Commit Summary:\n"
		  "==============\n\n"
		  (format "Selected commits: %s\n\n"
			  (mapconcat #'identity commits ", ")))
	  (gptel-request
	      prompt
	    :buffer (current-buffer)
	    :position (point)
	    :in-place t
	    :system
	    "You are a software developer's assistant focused on git commit analysis. \
Be concise but thorough when analyzing changes. Group related changes together if \
you notice patterns. If commit messages are included, use them to inform your analysis.")
	  (pop-to-buffer (current-buffer))
	  (view-mode 1))))))

;;;;; Activate Mullvad

(declare-function mullvad-connect-to-website "mullvad")
(defun gptel-extras-set-mullvad (orig-fun &rest args)
  "Enable `mullvad' when connecting to Gemini, then call ORIG-FUN with ARGS.
Use to circumvent Gemini’s location restrictions."
  (when (eq gptel-model 'gemini-pro)
    (mullvad-connect-to-website "Gemini"
				gptel-extras-gemini-mullvad-disconnect-after
				'silently))
  (apply orig-fun args))

(advice-add 'gptel-curl-get-response :around #'gptel-extras-set-mullvad)

;;;;; Save buffer

(declare-function simple-extras-slugify "simple-extras")
(autoload 'org-insert-heading "org")
(autoload 'org-next-visible-heading "org")
(defun gptel-extras-save-buffer (name _ _ interactivep)
  "Save the `gptel' buffer with NAME right after it is created.
The buffer is saved to a file in `gptel-extras-dir'. INTERACTIVEP is t when
gptel is called interactively.

This function is meant to be an `:after' advice to `gptel'."
  (when interactivep
    ;; do not run if the buffer is visiting a file, because that means the user
    ;; selected an existing buffer
    (unless (buffer-file-name (get-buffer name))
      (switch-to-buffer name)
      (let* ((extension (pcase major-mode
			  ('org-mode "org")
			  ('markdown-mode "md")
			  (_ (user-error "Unsupported major mode"))))
	     (filename (file-name-concat gptel-extras-dir
					 (file-name-with-extension (simple-extras-slugify name) extension))))
	(when (derived-mode-p 'org-mode)
	  (goto-char (point-min))
	  (org-insert-heading nil nil 1)
	  (insert name)
	  (org-next-visible-heading 1)
	  (end-of-line))
	;; we temporarily remove the hook because `gptel--save-state' throws an
	;; error if called at this early stage
	(remove-hook 'before-save-hook #'gptel--save-state t)
	(write-file filename 'confirm)
	(add-hook 'before-save-hook #'gptel--save-state nil t)))))

;;;;; post-response

(autoload 'org-latex-preview "org")
(defun gptel-extras-generate-latex-previews (_ _)
  "Generate LaTeX previews in the current `gptel' buffer."
  (when (and (derived-mode-p 'org-mode)
	     (string= default-directory gptel-extras-dir))
    (org-latex-preview)))

(autoload 'org-fold-show-all "org-fold")
(declare-function files-extras-kill-this-buffer "files-extras")
;;;###autoload
(defun gptel-extras-kill-buffer-then-reopen-file (&optional _ _)
  "Kill the current buffer then reopen the file it was visiting.
This hack command is meant to be added to `gptel-post-response-functions', if
necessary, or called manually, whenever `gptel' starts refusing to process the
request (mysteriously, just killing the buffer and reopening the visited file is
often enough to fix this)."
  (interactive)
  (when-let* ((file (buffer-file-name)))
    (save-buffer)
    (save-buffer)
    (files-extras-kill-this-buffer)
    (find-file file)
    (when (derived-mode-p 'org-mode)
      (org-fold-show-all))))

;;;;; Alert

(declare-function alert "alert")
(defun gptel-extras-alert-when-finished (_ _)
  "Alert the user when `gptel' finishes inserting its response.
START and END are the positions of the inserted response.
To enable this feature, customize `gptel-extras-alert-when-finished'."
  (when (or (eq gptel-extras-alert-when-finished t)
            (memq gptel-model gptel-extras-alert-when-finished))
    (alert "Response inserted" :title "gptel")))

(add-hook 'gptel-post-response-functions #'gptel-extras-alert-when-finished)

;;;;; Misc

;;;###autoload
(defun gptel-extras-toggle-major-mode ()
  "Toggle between `markdown-mode' and `org-mode'."
  (interactive)
  (setq gptel-default-mode
	(pcase gptel-default-mode
	  ('markdown-mode 'org-mode)
	  ('org-mode 'markdown-mode)))
  (message "Switched to %s." gptel-default-mode))

;;;###autoload
(defun gptel-extras-goto-end-and-send ()
  "Go to the end of the buffer and send the prompt."
  (interactive)
  (goto-char (point-max))
  (gptel-send))

(declare-function gptel-rewrite "gptel-rewrite")
;;;###autoload
(defun gptel-extras-rewrite-defun ()
  "Rewrite the current function definition, marking it first.
This command is meant to be invoked via `embark'; simply binding `gptel-rewrite'
won't work because it needs the function to be selected."
  (interactive)
  (if (thing-at-point 'defun t)
      (progn (mark-defun)
             (gptel-rewrite))
    (user-error "Not on a function definition")))

(defun gptel-extras-fix-garbled-chars ()
  "Replace `let/' and `=(' with proper `let*' and `\`(' from point to end of buffer."
  (interactive)
  (save-excursion
    (while (search-forward "let/" nil t)
      (replace-match "let*"))
    (goto-char (point))
    (while (search-forward "=(" nil t)
      (replace-match "`("))))

(provide 'gptel-extras)
;;; gptel-extras.el ends here
