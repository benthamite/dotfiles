;;; telega-extras.el --- Extensions for telega -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/telega-extras.el
;; Version: 0.2
;; Package-Requires: ((paths "0.1") (telega "0.8.1"))

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

;;;; Variables

(defgroup telega-extras ()
  "Extensions for `telega'."
  :group 'telega)

(defcustom telega-extras-auto-share-audio-transcript nil
  "Whether to automatically share transcript after transcribing message."
  :type 'boolean
  :group 'telega-extras)

(defcustom telega-extras-chats nil
  "Information about Tlön groups."
  ;; describe customization type correctly, based on the keywords defined in the elisp manual
  :type '(plist :tag "Chats"
		(string :tag "Name")
		(integer :tag "ID")
		(plist :tag "Topics"
		       (entry :tag "Topic"
			      (string :tag "Name")
			      (integer :tag "ID"))))
  :group 'telega-extras)

(defvar telega-extras-audio-transcript-timer nil
  "Timer to check for audio transcript availability.")

(defvar telega-extras-audio-transcript-timeout-timer nil
  "Timer to handle timeout of waiting for the transcript.")

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

(defun telega-extras-smart-enter ()
  "Take the appropriate action for the thing at point.
If the point is on a URL, open it in the browser. If the point is on a
button, push it. Otherwise, send the message at point."
  (interactive)
  (cond ((thing-at-point 'url)
	 (browse-url-at-point))
	((button-at (point))
	 (push-button))
	(t
	 (telega-chatbuf-input-send nil))))

;;;;; Attach files

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

(autoload 'files-extras-newest-file "files-extras")
(defun telega-extras-chatbuf-attach-most-recent-file ()
  "Attach most recently saved file in `downloads' folder."
  (interactive)
  (if-let* ((file (files-extras-newest-file paths-dir-downloads)))
      (telega-chatbuf-attach-file file)
    (user-error (format "No files found in %s" paths-dir-downloads))))

;;;;; Download file

(defun telega-extras-download-file (&optional message)
  "Download the file for MESSAGE.
If MESSAGE is nil, use the message at point."
  (interactive)
  (let* ((message (or message (telega-msg-at))))
    (telega-msg-save message)))

;;;;; Transcribe audio

;;;###autoload
(defun telega-extras-transcribe-audio (&optional message)
  "Transcribe the audio for MESSAGE.
Additionally, share the audio transcript if
`telega-extras-auto-share-audio-transcript' is non-nil.

If MESSAGE is nil, use the message at point."
  (interactive)
  (let* ((message (or message (telega-msg-at))))
    (when (telega-extras-message-has-audio-p message)
      (telega--recognizeSpeech message)
      (telega-extras-maybe-share-audio-transcript message))))

(defun telega-extras-message-has-audio-p (message)
  "Return t iff MESSAGE is an audio file."
  (telega-msg-match-p message '(type VoiceNote)))

(defun telega-extras-maybe-share-audio-transcript (message)
  "Conditionally share audio transcript for MESSAGE."
  (when (and telega-extras-auto-share-audio-transcript
	     (telega-msg-by-me-p message))
    (setq telega-extras-audio-transcript-timer
	  (run-with-timer 0 5 #'telega-extras-maybe-get-audio-transcript message)
	  telega-extras-audio-transcript-timeout-timer
	  (run-with-timer 60 nil #'telega-extras-cancel-audio-timers))))

(defun telega-extras-get-audio-transcript (message)
  "Return the transcript of the audio file in MESSAGE."
  (when (telega-extras-message-has-audio-p message)
    (telega-tl-str
     (telega--tl-get (plist-get message :content)
		     :voice_note :speech_recognition_result)
     :text)))

(defun telega-extras-maybe-get-audio-transcript (message)
  "Check if the transcript for audio in MESSAGE is ready, and process it if so."
  (when-let* ((transcript (telega-extras-get-audio-transcript message)))
    (telega-extras-cancel-audio-timers)
    (telega-extras-post-audio-transcript transcript message)))

(defun telega-extras-post-audio-transcript (transcript message)
  "Post TRANSCRIPT of audio MESSAGE as a reply to it."
  (telega-msg-reply message)
  (insert (format "Audio transcript:\n\n \"%s\"" transcript))
  (telega-chatbuf-input-send t))

;;;;;; Timer management

(defun telega-extras-cancel-timer-when-active (timer-name)
  "Cancel TIMER-NAME when active."
  (let ((timer (symbol-value timer-name)))
    (when timer
      (cancel-timer timer)
      (set timer-name nil))))

(defun telega-extras-cancel-audio-timers ()
  "Cancel the audio transcript and timeout timers."
  (telega-extras-cancel-timer-when-active 'telega-extras-audio-transcript-timer)
  (telega-extras-cancel-timer-when-active 'telega-extras-audio-transcript-timeout-timer))

;;;;; Open chats & topics

(defun telega-extras-open-chat (chat &optional topic)
  "Open CHAT.
If TOPIC is non-nil, restrict the view to that topic.

Both CHAT and TOPIC are strings, describing the name of the chat and topic,
respectively, as defined in `telega-extras-chats'."
  (telega)
  (let ((id (telega-extras-get-id chat topic)))
    (if (consp id)
	(telega-msg-open-thread-or-topic
	 (telega-extras-get-last-topic-message (car id) (cdr id)))
      (telega-chat-with (telega-chat-get id))
      (telega-chatbuf-filter-cancel t))
    (sleep-for 0.2) ; hack to ensure the buffer is loaded
    (telega-chatbuf-input-insert "")))

(declare-function tlon-lookup "tlon-core")
(defun telega-extras-get-id (chat &optional topic)
  "Return the ID of CHAT.
If TOPIC is non-nil, return the associated topic ID"
  (let* ((chat-id (tlon-lookup telega-extras-chats :id :name chat))
	 (topic-id (when topic
		     (let ((chat-topics (tlon-lookup telega-extras-chats :topics :name chat)))
		       (tlon-lookup chat-topics :id :name topic)))))
    (if topic-id (cons chat-id topic-id) chat-id)))

(defun telega-extras-get-last-topic-message (chat-id topic-id)
  "Get the last message in a topic thread for the given CHAT-ID and TOPIC-ID."
  (let* ((chat (telega-chat-get chat-id))
	 (topic (telega-topic-get chat topic-id)))
    (plist-get topic :last_message)))

;;;;; Tab-bar integration

(declare-function tab-bar-extras-quick-reset "tab-bar-extras")
(defun telega-extras-reset-tab-bar ()
  "Reset the tab shortly after Telega is loaded to show its element correctly."
  (run-with-timer 1 nil #'tab-bar-extras-quick-reset))

;;;;; Menu

(transient-define-prefix telega-extras-menu ()
  "`telega-extras' menu."
  [["Tlön"
    ("t" "All" (lambda () (interactive) (telega-extras-open-chat "Tlön")))
    ("g" "General" (lambda () (interactive) (telega-extras-open-chat "Tlön" "General")))
    ("s" "Social" (lambda () (interactive) (telega-extras-open-chat "Tlön" "Social")))
    ("e" "Emacs" (lambda () (interactive) (telega-extras-open-chat "Tlön" "Emacs")))
    ("m" "Meta" (lambda () (interactive) (telega-extras-open-chat "Tlön" "Meta")))]])

(provide 'telega-extras)
;;; telega-extras.el ends here
