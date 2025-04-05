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

(provide 'aidermacs-extras)
;;; aidermacs-extras.el ends here

