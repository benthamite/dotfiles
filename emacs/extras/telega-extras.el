;;; telega-extras.el --- Extensions for telega -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/telega-extras.el
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

;; Extensions for `telega'.

;;; Code:

(require 'paths)
(require 'telega)
(require 'telega-dired-dwim)

;;;; Functions

(defun telega-extras-switch-to ()
  "Switch to the most recent telega buffer, if one exists, else start telega.
Repeated invocations of the command will move point from the
middle of the chat buffer to the end of it, then to the root
buffer, then to the beginning of it if not already there. A
similar sequence of events will be triggered if point is in an
archive buffer."
  (interactive)
  (let* ((telega-buffer-root "*Telega Root*")
         (telega-buffer
          (catch 'tag
            (dolist (buffer (buffer-list))
              (when (with-current-buffer buffer
                      (derived-mode-p 'telega-root-mode 'telega-chat-mode))
		(throw 'tag (buffer-name buffer))))))
	 (telega-buffer-is-current (string= (buffer-name (current-buffer)) telega-buffer)))
    (cond
     ((not telega-buffer)
      (telega))
     ((not telega-buffer-is-current)
      (switch-to-buffer telega-buffer))
     ;; if currently in chatbuf, move to end, else back to main
     ((not (string= telega-buffer-root telega-buffer))
      (if (not (eobp))
          (goto-char (point-max))
	(kill-buffer)
	(switch-to-buffer telega-buffer-root)))
     ;; if currently in archive, move to beg, else back to main
     ((eq (car (telega-filter-active)) 'archive)
      (if (eq (count-lines (point-min) (point)) 3)
          (telega-filters-reset)
	(goto-char (point-min))
	(forward-line 3)))
     ((not (eq (count-lines (point-min) (point)) 4))
      (goto-char (point-min))
      (forward-line 4)))))

(defun telega-extras-chat-org-capture ()
  "Capture chat message at point with `org-capture'."
  (interactive)
  (org-store-link nil)
  (org-capture nil "n"))

(defun telega-extras-view-archive ()
  "Set active filters list to `archive'."
  (interactive)
  (telega-filters-reset)
  (telega-filters-push '(archive)))

(defun telega-extras-view-main ()
  "Set active filters list to `main'."
  (interactive)
  (telega-filters-reset)
  (telega-filters-push '(main)))

(defun telega-extras-dired-attach-func (file)
  "Identify msg type for FILE."
  (let ((file-ext (file-name-extension file)))
    (cond ((member file-ext '("mp3" "flac"))
           #'telega-chatbuf-attach-audio)
          ((member file-ext '("mp4" "mkv"))
           #'telega-chatbuf-attach-video)
          ((image-supported-file-p file)
           #'telega-chatbuf-attach-photo)
          (t
           #'telega-chatbuf-attach-file))))

(defun telega-extras-dired-attach-send ()
  "Send the marked files."
  (interactive)
  (let ((dired-files (dired-get-marked-files)))
    (unless dired-files
      (user-error "No marked files"))
    (with-current-buffer (telega-chat--pop-to-buffer
                          (telega-completing-read-chat
                           (format "Send %d files to: " (length dired-files))))
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (dolist (file dired-files)
          (funcall (telega-extras-dired-attach-func file) file))))))

(declare-function files-extras-newest-file "files-extras")
(defun telega-extras-chatbuf-attach-most-recent-file ()
  "Attach most recently saved file in `downloads' folder."
  (interactive)
  (if-let ((file (files-extras-newest-file paths-dir-downloads)))
      (telega-chatbuf-attach-file file)
    (user-error (format "No files found in %s" paths-dir-downloads))))

;;;;; Message actions

(defun telega-extras-act-on-message (action message)
  "Perform ACTION on MESSAGE."
  (let* ((fun (pcase action
		('download #'telega-msg-save)
		('transcribe #'telega--recognizeSpeech))))
    (funcall fun message)))

(defun telega-extras-transcribe-audio (&optional message)
  "Transcribe the audio for MESSAGE.
If MESSAGE is nil, use the message at point."
  (interactive)
  (let* ((message (or message (telega-msg-at))))
    (telega-extras-act-on-message 'transcribe message)))

(defun telega-extras-download-file (&optional message)
  "Download the file for MESSAGE.
If MESSAGE is nil, use the message at point."
  (interactive)
  (let* ((message (or message (telega-msg-at))))
    (telega-extras-act-on-message 'download message)))

(provide 'telega-extras)
;;; telega-extras.el ends here

