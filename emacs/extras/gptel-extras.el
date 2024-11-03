;;; gptel-extras.el --- Extensions for gptel -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/gptel-extras.el
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

;; Extensions for `gptel'.

;;; Code:

(require 'gptel)
(require 'paths)
(require 'simple-extras)

;;;; User options

(defgroup gptel-extras ()
  "Extensions for `gptel'."
  :group 'gptel)

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

;;;; Variables

(defvar-local gptel-context nil
  "The context for the current buffer.")

(defconst gptel-extras-local-variables
  '(gptel-mode gptel-model gptel--backend-name gptel--bounds)
  "A list of relevant `gptel' file-local variables.")

(defconst gptel-extras-org-properties
  '("GPTEL_SYSTEM" "GPTEL_BACKEND" "GPTEL_MODEL"
    "GPTEL_TEMPERATURE" "GPTEL_MAX_TOKENS"
    "GPTEL_NUM_MESSAGES_TO_SEND")
  "A list of relevant `gptel' Org properties.")

(defconst gptel-extras-changelog-file
  (file-name-concat paths-dir-dotemacs "extras/gptel-extras-changelog-template.org")
  "The file with the changelog template for `gptel-extras-summarize-commit-diffs'.")

;;;; Functions

;;;;; Estimate cost

(defun gptel-extras-get-cost ()
  "Get the rough input cost of prompting the current model.
This is used to display the relevant information in the `gptel' headerline.

Note that the cost is an approximation based on the number of words in the
buffer or selection. The function uses a 1.4 token/word conversion factor, but
the actual cost may deviate from this estimate."
  (let* ((cost-per-1m-tokens (get gptel-model :input-cost))
	 (words-main (if (region-active-p)
			 (count-words (region-beginning) (region-end))
		       (count-words (point-min) (point))))
	 (words-context (gptel-extras-count-words-in-context))
	 (total-words (+ words-main words-context))
	 (tokens-per-word 1.4)
	 (cost (/ (* cost-per-1m-tokens tokens-per-word total-words) 1000000.0)))
    cost))

(defun gptel-extras-count-words-in-context ()
  "Iterate over the files in context and sum the number of words in each file."
  (cl-reduce (lambda (acc file)
	       (let ((words (with-current-buffer (find-file-noselect (car file))
			      (count-words (point-min) (point-max)))))
		 (+ acc words)))
	     gptel-context--alist
	     :initial-value 0))

(define-minor-mode gptel-mode
  "Minor mode for interacting with LLMs.
This is just the original `gptel-mode' definition with a modification
to add an additional cost field in the header line."
  :lighter " GPT"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c RET") #'gptel-send)
    map)
  (if gptel-mode
      (progn
	(unless (or (derived-mode-p 'org-mode 'markdown-mode)
		    (eq major-mode 'text-mode))
	  (gptel-mode -1)
	  (user-error (format "`gptel-mode' is not supported in `%s'." major-mode)))
	(add-hook 'before-save-hook #'gptel--save-state nil t)
	(gptel--restore-state)
	(if gptel-use-header-line
	    (setq gptel--old-header-line header-line-format
		  header-line-format
		  (list '(:eval (concat (propertize " " 'display '(space :align-to 0))
					(format "%s" (gptel-backend-name gptel-backend))))
			(propertize " Ready" 'face 'success)
			'(:eval
			  (let* ((model (gptel--model-name gptel-model))
				 (system
				  (propertize
				   (buttonize
				    (format "[Prompt: %s]"
					    (or (car-safe (rassoc gptel--system-message gptel-directives))
						(truncate-string-to-width gptel--system-message 15 nil nil t)))
				    (lambda (&rest _) (gptel-system-prompt)))
				   'mouse-face 'highlight
				   'help-echo "System message for session"))
				 (cost
				  (propertize
				   (buttonize (format "[Cost: $%.2f]" (gptel-extras-get-cost))
					      (lambda (&rest _) (gptel-menu)))
				   'mouse-face 'highlight
				   'help-echo "Cost of the current prompt"))
				 (context
				  (and gptel-context--alist
				       (cl-loop for entry in gptel-context--alist
						if (bufferp (car entry)) count it into bufs
						else count (stringp (car entry)) into files
						finally return
						(propertize
						 (buttonize
						  (concat "[Context: "
							  (and (> bufs 0) (format "%d buf" bufs))
							  (and (> bufs 1) "s")
							  (and (> bufs 0) (> files 0) ", ")
							  (and (> files 0) (format "%d file" files))
							  (and (> files 1) "s")
							  "]")
						  (lambda (&rest _)
						    (require 'gptel-context)
						    (gptel-context--buffer-setup)))
						 'mouse-face 'highlight
						 'help-echo "Active gptel context"))))
				 (toggle-track-media
				  (lambda (&rest _)
				    (setq-local gptel-track-media
						(not gptel-track-media))
				    (if gptel-track-media
					(message
					 (concat
					  "Sending media from included links.  To include media, create "
					  "a \"standalone\" link in a paragraph by itself, separated from surrounding text."))
				      (message "Ignoring image links.  Only link text will be sent."))
				    (run-at-time 0 nil #'force-mode-line-update)))
				 (track-media
				  (and (gptel--model-capable-p 'media)
				       (if gptel-track-media
					   (propertize
					    (buttonize "[Sending media]" toggle-track-media)
					    'mouse-face 'highlight
					    'help-echo
					    "Sending media from standalone links/urls when supported.\nClick to toggle")
					 (propertize
					  (buttonize "[Ignoring media]" toggle-track-media)
					  'mouse-face 'highlight
					  'help-echo
					  "Ignoring images from standalone links/urls.\nClick to toggle")))))
			    (concat
			     (propertize
			      " " 'display
			      `(space :align-to (- right ,(+ 5 (length model) (length system)
							     (length track-media) (length context) (length cost)))))
			     track-media (and context " ") context " " cost
			     " " system " "
			     (propertize
			      (buttonize (concat "[" model "]")
					 (lambda (&rest _) (gptel-menu)))
			      'mouse-face 'highlight
			      'help-echo "GPT model in use"))))))
	  (setq mode-line-process
		'(:eval (concat " "
				(buttonize (gptel--model-name gptel-model)
					   (lambda (&rest _) (gptel-menu))))))))
    (remove-hook 'before-save-hook #'gptel--save-state t)
    (if gptel-use-header-line
	(setq header-line-format gptel--old-header-line
	      gptel--old-header-line nil)
      (setq mode-line-process nil))))

;;;;; Summarize commit diffs

(declare-function magit-commit-at-point "magit-git")
(declare-function magit-git-insert "magit-git")
(defun gptel-extras-summarize-commit-diffs (beg end &optional include-stats)
  "Summarize the diffs of commits in the selected region using an LLM.
BEG and END mark the region of commits to summarize. When INCLUDE-STATS is
non-nil (with prefix arg), include diffstats in the prompt."
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
    (require 'mullvad)
    (mullvad-connect-to-website "Gemini"
				gptel-extras-gemini-mullvad-disconnect-after
				'silently))
  (apply orig-fun args))

(advice-add 'gptel-curl-get-response :around #'gptel-extras-set-mullvad)

;;;;; Save buffer

(declare-function org-insert-heading "org")
(declare-function org-next-visible-heading "org")
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

(declare-function org-latex-preview "org")
(defun gptel-extras-generate-latex-previews (_ _)
  "Generate LaTeX previews in the current `gptel' buffer."
  (when (and (derived-mode-p 'org-mode)
	     (string= default-directory gptel-extras-dir))
    (org-latex-preview)))

(declare-function files-extras-kill-this-buffer "files-extras")
(declare-function org-fold-show-all "org-fold")
;;;###autoload
(defun gptel-extras-kill-buffer-then-reopen-file (&optional _ _)
  "Kill the current buffer then reopen the file it was visiting.
This hack command is meant to be added to `gptel-post-response-functions', if
necessary, or called manually, whenever `gptel' starts refusing to process the
request (mysteriously, just killing the buffer and reopening the visited file is
often enough to fix this)."
  (interactive)
  (when-let ((file (buffer-file-name)))
    (save-buffer)
    (save-buffer)
    (files-extras-kill-this-buffer)
    (find-file file)
    (when (derived-mode-p 'org-mode)
      (org-fold-show-all))))

;;;;; Enable gptel

(declare-function breadcrumb-mode "breadcrumb")
(defun gptel-extras-enable-gptel-in-org ()
  "Enable `gptel-mode' in `org-mode' files with `gptel' data."
  (when (gptel-extras-file-has-gptel-org-property-p)
    (gptel-extras-enable-gptel-common)))

(defun gptel-extras-enable-gptel-in-markdown ()
  "Enable `gptel-mode' in `markdown-mode' files with `gptel' data."
  (when (gptel-extras-file-has-gptel-local-variable-p)
    (gptel-extras-enable-gptel-common)))

(defun gptel-extras-enable-gptel-common ()
  "Enable `gptel-mode' and in any buffer with `gptel' data."
  (let ((buffer-modified-p (buffer-modified-p)))
    (gptel-mode)
    ;; `breadcrumb-mode' interferes with the `gptel' header line
    (when (bound-and-true-p breadcrumb-mode)
      (breadcrumb-mode -1))
    ;; prevent the buffer from becoming modified merely because `gptel-mode'
    ;; is enabled
    (unless buffer-modified-p
      (save-buffer))))

(defun gptel-extras-file-has-gptel-local-variable-p ()
  "Return t iff the current buffer has a `gptel' local variable."
  (cl-some (lambda (var)
	     (local-variable-p var))
	   gptel-extras-local-variables))

(declare-function org-entry-get "org")
(defun gptel-extras-file-has-gptel-org-property-p ()
  "Return t iff the current buffer has a `gptel' Org property."
  (cl-some (lambda (prop)
	     (org-entry-get (point-min) prop))
	   gptel-extras-org-properties))

;;;;; Save, restore & clear file context

;;;;;; Save

(declare-function org-set-property "org")
(defun gptel-extras-save-file-context ()
  "Save the current `gptel' file context in file visited by the current buffer.
In Org files, saves as a file property. In Markdown, as a file-local variable."
  (interactive)
  (let ((context (pcase major-mode
		   ('org-mode
		    (save-excursion
		      (goto-char (point-min))
		      (org-set-property "GPTEL_CONTEXT" (prin1-to-string gptel-context--alist))))
		   ('markdown-mode (gptel-extras-save-file-context-in-markdown))
		   (_ (user-error "Not in and Org or Markdown buffer")))))
    (message "Saved `gptel' context: %s" context)))

(defun gptel-extras-save-file-context-in-markdown ()
  "Save the current `gptel' file context in file visited by the current MD buffer."
  (interactive)
  (gptel-extras-remove-local-variables-section)
  (let ((context (format "%S" gptel-context--alist)))
    (add-file-local-variable 'gptel-context context)
    (message "Saved `gptel' context: %s" context)))

(defun gptel-extras-remove-local-variables-section ()
  "Remove the existing Local Variables section from the current buffer."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "^<!-- Local Variables: -->" nil t)
      (let ((start (point)))
        (when (re-search-forward "^<!-- End: -->" nil t)
          (delete-region start (point))
          (delete-blank-lines))))))

;;;;;; Restore

(defun gptel-extras-restore-file-context ()
  "Restore the saved file context from the file visited by the current buffer."
  (interactive)
  (when-let ((context (pcase major-mode
			('org-mode
			 (when-let ((gptel-context-prop (org-entry-get (point-min) "GPTEL_CONTEXT")))
			   (read gptel-context-prop)))
			('markdown-mode gptel-context)
			(_ (user-error "Not in and Org or Markdown buffer")))))
    (message "Restored `gptel' context: %s" (setq gptel-context--alist context))))

;;;;;; Clear

(defun gptel-extras-clear-file-context ()
  "Clear the current `gptel' file context."
  (interactive)
  (setq gptel-context--alist nil)
  (message "Cleared `gptel' context."))

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

(provide 'gptel-extras)
;;; gptel-extras.el ends here
