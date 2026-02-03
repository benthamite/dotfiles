;;; claude-code-extras.el --- Extensions for claude-code -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/claude-code-extras.el
;; Version: 0.1
;; Package-Requires: ((claude-code "0.1") (paths "0.1"))

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

;; Extensions for `claude-code'.

;;; Code:

(require 'claude-code)
(require 'paths)

;;;; Variables

(defgroup claude-code-extras ()
  "Extensions for `claude-code'."
  :group 'claude-code)

(defcustom claude-code-extras-protect-buffers t
  "When non-nil, prompt for confirmation before killing claude-code buffers."
  :type 'boolean
  :group 'claude-code-extras)

(defcustom claude-code-extras-log-directory
  (expand-file-name "claude-logs" paths-dir-notes)
  "Directory where Claude conversation logs are saved."
  :type 'directory
  :group 'claude-code-extras)

(defcustom claude-code-extras-log-interval 30
  "Interval in seconds between automatic log saves."
  :type 'integer
  :group 'claude-code-extras)

(defvar-local claude-code-extras--log-file nil
  "Log file path for the current Claude buffer.")

(defvar-local claude-code-extras--log-timer nil
  "Timer for periodic logging in the current Claude buffer.")

;;;; Functions

(defun claude-code-extras--get-log-file (buffer)
  "Get or create the log file path for BUFFER."
  (with-current-buffer buffer
    (or claude-code-extras--log-file
        (setq claude-code-extras--log-file
              (expand-file-name
               (format "%s_%s.txt"
                       (replace-regexp-in-string
                        "[^a-zA-Z0-9_-]" "_"
                        (buffer-name))
                       (format-time-string "%Y-%m-%d_%H-%M-%S"))
               claude-code-extras-log-directory)))))

(defun claude-code-extras--save-log (buffer)
  "Save the conversation log for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((log-file (claude-code-extras--get-log-file buffer)))
        (make-directory claude-code-extras-log-directory t)
        (write-region (point-min) (point-max) log-file nil 'quiet)))))

(defun claude-code-extras-start-logging ()
  "Start periodic logging for the current Claude buffer."
  (when (claude-code--buffer-p (current-buffer))
    (claude-code-extras--save-log (current-buffer))
    (setq claude-code-extras--log-timer
          (run-with-timer
           claude-code-extras-log-interval
           claude-code-extras-log-interval
           #'claude-code-extras--save-log
           (current-buffer)))))

(defun claude-code-extras-stop-logging ()
  "Stop logging and save final log for the current Claude buffer."
  (when (and (claude-code--buffer-p (current-buffer))
             claude-code-extras--log-timer)
    (cancel-timer claude-code-extras--log-timer)
    (claude-code-extras--save-log (current-buffer))))

(defun claude-code-extras-protect-buffer ()
  "Prompt for confirmation before killing claude-code buffers.
Returns t if the buffer should be killed, nil otherwise. Intended for use
in `kill-buffer-query-functions'."
  (or (not claude-code-extras-protect-buffers)
      (not (claude-code--buffer-p (current-buffer)))
      (yes-or-no-p "Kill claude-code buffer? ")))

(add-hook 'kill-buffer-query-functions #'claude-code-extras-protect-buffer)

(provide 'claude-code-extras)
;;; claude-code-extras.el ends here
