;;; aidermacs-extras.el --- Extensions for aidermacs -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/aidermacs-extras.el
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

;; Extensions for `aidermacs'.

;;; Code:

(require 'aidermacs)

;;;; User options

(defgroup aidermacs-extras ()
  "Extensions for `aidermacs'."
  :group 'aidermacs-extras)

;;;; Functions

(defun aidermacs-extras-copy-recent-history-to-kill-ring (&optional line-count)
  "Copy the last LINE-COUNT lines of `.aider.chat.history.md' to the kill ring.
If LINE-COUNT is nil, defaults to 1000 lines. This function avoids opening the
file in Emacs for better performance with large files."
  (interactive "P")
  (let* ((line-count (or line-count 1000))
         (repo-root (aidermacs-project-root))
         (history-file (expand-file-name ".aider.chat.history.md" repo-root)))
    (if (file-exists-p history-file)
        (let ((output (with-temp-buffer
                        ;; Use tail to get the last N lines efficiently
                        (call-process "tail" nil t nil
                                      (format "-%d" line-count)
                                      history-file)
                        (buffer-string))))
          (if (string-empty-p output)
              (message "No content found in history file")
            (kill-new output)
            (message "Copied last %d lines of chat history to kill ring" line-count)))
      (message "Chat history file not found: %s" history-file))))

(defun aidermacs-extras-confirm-kill-buffer ()
  "Query before killing an Aidermacs buffer with a running process.
Return t if the buffer can be killed (i.e., if it's not an
aidermacs comint buffer with a process, or if the user confirms
the kill). Return nil if the user decides not to kill the
buffer."
  (if (and (derived-mode-p 'comint-mode)
           (buffer-name)
           (string-prefix-p "*aidermacs" (buffer-name))
           (get-buffer-process (current-buffer)))
      (yes-or-no-p "Are you sure you want to kill this Aidermacs buffer? ")
    t))

(defun aidermacs-extras-copy-prompt-region ()
  "Copy a region of the Aider history buffer based on user prompt blocks.
Prompts the user to select the first line of a start prompt block
\\=(consecutive lines beginning with '#### ') and an end prompt block. Copies
the text from the beginning of the start block's first line up to the beginning
of the *next* block after the selected end block (or end of buffer if the end
block is the last one). If the user presses RET for the end prompt, copies to
the end of the buffer."
  (interactive)
  (unless (string-match-p "\\.aider\\.chat\\.history\\.md\\'" (or (buffer-file-name) ""))
    (message "Warning: This buffer might not be an Aider history file."))
  (let ((prompts '()))
    ;; Collect the start position and first line of each prompt block
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#### \\(.*\\)" nil t)
        (let ((first-line-text (match-string 1))
              (block-start-pos (match-beginning 0)))
          ;; Store the first line text and the block's starting position
          (push (cons first-line-text block-start-pos) prompts)
          ;; Skip subsequent contiguous '#### ' lines within the same block
          (while (and (not (eobp))
                      (progn (forward-line 1) (looking-at "^#### ")))
            ;; Keep moving forward within the block
            )
          ;; Ensure the next search starts after the current block
          ;; (re-search-forward moves point, but the inner loop might have moved it further)
          (goto-char (line-end-position)))))
    (setq prompts (nreverse prompts)) ; Put them in buffer order

    (unless prompts
      (user-error "No Aider prompt blocks ('#### ...') found in the buffer"))

    (let* ((prompt-strings (mapcar #'car prompts))
           (start-prompt-text (completing-read "Select start prompt: " prompt-strings nil t))
           (start-assoc (assoc start-prompt-text prompts))
           start-pos candidate-end-prompts-assoc-list end-prompt-text end-assoc end-pos next-prompt-assoc)

      (unless start-assoc
        (user-error "Invalid start prompt selected"))
      (setq start-pos (cdr start-assoc))

      ;; Candidate end prompts include the start prompt and all subsequent ones
      (setq candidate-end-prompts-assoc-list (member start-assoc prompts))

      (unless candidate-end-prompts-assoc-list
        ;; This should theoretically not happen if start-assoc was found, but safety first
        (user-error "Could not determine candidate end prompts"))

      (setq end-prompt-text (completing-read "Select end prompt (or press RET for end of buffer): "
                                             (mapcar #'car candidate-end-prompts-assoc-list)
                                             nil nil nil nil "")) ; Allow empty input

      ;; Determine the final end position for the copy
      (if (string-empty-p end-prompt-text)
          ;; User pressed RET, copy to end of buffer
          (setq end-pos (point-max))
        ;; User selected an end prompt
        (setq end-assoc (assoc end-prompt-text candidate-end-prompts-assoc-list))
        (unless end-assoc
          (user-error "Invalid end prompt selected"))
        ;; Find the prompt *after* the selected end prompt
        (setq next-prompt-assoc (cadr (member end-assoc prompts))) ; Check original list
        (if next-prompt-assoc
            ;; If there is a next prompt, end region at its start
            (setq end-pos (cdr next-prompt-assoc))
          ;; If the selected end prompt was the last one, end region at buffer end
          (setq end-pos (point-max))))

      (kill-new (buffer-substring-no-properties start-pos end-pos))
      (message "Copied region from '%s' up to %s into kill ring"
               start-prompt-text
               (cond
                ((string-empty-p end-prompt-text) "end of buffer") ; User pressed RET
                (next-prompt-assoc (format "start of next prompt ('%s')" (car next-prompt-assoc))) ; Ended before next prompt
                (t (format "end of buffer (after '%s')" end-prompt-text))))))) ; Selected end prompt was last

(defun aidermacs-extras-save-repo-map (&optional async)
  "Save the Aider repository map to a file.
If ASYNC is non-nil, run the command asynchronously."
  (let ((command (if async 'async-shell-command 'shell-command))
	(repo (file-name-concat (aidermacs-project-root) "repo-map.md")))
    (message "`default-directory' is %s" default-directory)
    (funcall command (format "aider --show-repo-map > %s" repo))))

(declare-function files-extras-get-help-file "files-extras")
(defun aidermacs-extras-run-in-current-dir ()
  "Run Aider in the current directory `default-directory`.
If invoked from a file-visiting buffer, add the current file and, if available,
its associated help file."
  (interactive)
  (let* ((current-file (buffer-file-name))
	 (help-file (and current-file (files-extras-get-help-file current-file)))
	 (extra-args (append (when current-file (list "--file" current-file))
			     (when help-file (list "--file" help-file))))
	 (aidermacs-extra-args (append aidermacs-extra-args extra-args)))
    (aidermacs-run-in-current-dir)))

(provide 'aidermacs-extras)
;;; aidermacs-extras.el ends here

