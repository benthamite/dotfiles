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

;;;; Main variables

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
  "Confirm before killing a comint buffer with an active Aidermacs process."
  (unless (and (eq major-mode 'comint-mode)
               (term-check-proc (current-buffer))
               (string-match-p "\\*aidermacs" (buffer-name))
               (null (yes-or-no-p "Buffer has a running process. Kill anyway? ")))
    t))

(defun aidermacs-extras-copy-prompt-region ()
  "Copy a region of the Aider history buffer based on user prompts.
Prompts the user to select a start prompt (lines beginning with '#### ')
and an end prompt. Copies the text from the beginning of the start
prompt line up to the beginning of the end prompt line to the kill ring."
  (interactive)
  (unless (string-match-p "\\.aider\\.chat\\.history\\.md\\'" (or (buffer-file-name) ""))
    (message "Warning: This buffer might not be an Aider history file."))
  (let ((prompts '()))
    ;; Collect all prompts and their positions
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#### \\(.*\\)" nil t)
        (push (cons (match-string 1) (match-beginning 0)) prompts)))
    (setq prompts (nreverse prompts)) ; Put them in buffer order

    (unless prompts
      (user-error "No Aider prompts ('#### ...') found in the buffer"))

    (let* ((prompt-strings (mapcar #'car prompts))
           (start-prompt-text (completing-read "Select start prompt: " prompt-strings nil t))
           (start-assoc (assoc start-prompt-text prompts))
           (start-pos (cdr start-assoc))
           ;; Filter prompts to only include those *after* the selected start prompt
           (remaining-prompts (seq-filter (lambda (p) (> (cdr p) start-pos)) prompts))
           end-prompt-text end-assoc end-pos)

      (unless start-assoc
        (user-error "Invalid start prompt selected"))

      (unless remaining-prompts
        ;; If no prompts remain, copy from start prompt to end of buffer
        (kill-new (buffer-substring-no-properties start-pos (point-max)))
        (message "Copied region from '%s' to end of buffer into kill ring" start-prompt-text)
        (cl-return-from aidermacs-extras-copy-prompt-region)) ; Exit function early

      (setq end-prompt-text (completing-read "Select end prompt (or press RET for end of buffer): "
                                             (mapcar #'car remaining-prompts)
                                             nil nil nil nil "")) ; Allow empty input

      (if (string-empty-p end-prompt-text)
          ;; User pressed RET, copy to end of buffer
          (setq end-pos (point-max))
        ;; User selected an end prompt
        (setq end-assoc (assoc end-prompt-text remaining-prompts))
        (unless end-assoc
          (user-error "Invalid end prompt selected"))
        (setq end-pos (cdr end-assoc)))

      (kill-new (buffer-substring-no-properties start-pos end-pos))
      (message "Copied region from '%s' to %s into kill ring"
               start-prompt-text
               (if end-assoc (format "'%s'" end-prompt-text) "end of buffer")))))

(provide 'aidermacs-extras)
;;; aidermacs-extras.el ends here

