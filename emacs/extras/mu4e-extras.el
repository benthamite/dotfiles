;;; mu4e-extras.el --- Extensions for mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/mu4e-extras.el
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

;; Extensions for `mu4e'.

;;; Code:

(require 'el-patch)
(require 'mu4e)
(require 'org-msg-extras)

;;;; User options

(defgroup mu4e-extras ()
  "Extensions for `mu4e'."
  :group 'mu4e)

(defcustom mu4e-extras-inbox-folder ""
  "Name of the `inbox' folder."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-daily-folder ""
  "Name of the `daily' folder."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-wide-reply 'prompt
  "Whether the reply to messages should be \"wide\" (a.k.a. \"reply-to-all\").
If `prompt', ask the user. If t, always reply to all. If nil, always reply to
the sender only."
  :type '(choice (const :tag "Prompt" prompt)
		 (const :tag "Yes" t)
		 (const :tag "No" nil)))

;;;; Variables

(defvar mu4e-extras-read-and-refile-tracker '()
  "List of `:message-id' of messages to be re-marked as read after synchronization.")

;;;; Functions

;;;;; Post-processing

(defun mu4e-extras-gmail-fix-flags (mark msg)
  "Fix Gmail flags for each MARK and MSG pair."
  (cond ((eq mark 'trash)  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
	((eq mark 'refile) (mu4e-action-retag-message msg "-\\Inbox,+\\Refiled"))
	((eq mark 'flag)   (mu4e-action-retag-message msg "+\\Starred"))
	((eq mark 'unflag) (mu4e-action-retag-message msg "-\\Starred"))))

;; FIXME: this is not working
;; I just can't figure out how to get the plist of the sent message
(defun mu4e-extras-mark-sent-as-read ()
  "Mark the sent message identified by DOCID as read.
When mu4e sends an email with Gmail, Gmail automatically saves a copy in the
\"Sent\" folder, so the local copy is deleted (as specified by
`mu4e-sent-messages-behavior'). However, the saved copy is treated as a new,
unread message when synchronized back to the local client. To handle this
annoyance, this function marks the saved copy as read."
  (let* ((msgid (message-fetch-field "Message-ID"))
	 (msg (save-excursion
		(mu4e-headers-goto-message-id msgid)
		(mu4e-message-at-point))))
    (mu4e-action-retag-message msg "+\\Refiled")
    (mu4e--server-move msgid nil "+S-u-N")))

;;;;; Setup

(defun mu4e-extras-set-shortcuts ()
  "Set `mu4e-maildir-shortcuts'."
  (dolist (shortcut `((:maildir ,mu4e-extras-inbox-folder :key ?i)
		      (:maildir ,mu4e-extras-daily-folder :key ?y)))
    (add-to-list 'mu4e-maildir-shortcuts shortcut)))

(defun mu4e-extras-set-face-locally ()
  "Set `shr-text' face locally in `mu4ew-view-mode' buffers."
  (when (derived-mode-p 'mu4e-view-mode)
    (face-remap-add-relative 'shr-text :height 0.9)))

;;;;; Commands

;;;;;; Refile

;;;###autoload
(defun mu4e-extras-headers-refile ()
  "In headers mode, refile message at point.
Do not ask for confirmation."
  (interactive)
  (mu4e-headers-mark-for-refile)
  (mu4e-mark-execute-all t))

;;;###autoload
(defun mu4e-extras-headers-mark-read-and-refile ()
  "In headers mode, mark message at point and read and refile it.
Do not ask for confirmation."
  (interactive)
  (mu4e-headers-mark-for-read)
  (mu4e-mark-execute-all t)
  (sleep-for 0.1)
  (forward-line -1)
  (mu4e-extras-headers-refile))

;;;###autoload
(defun mu4e-extras-view-refile ()
  "In view mode, refile message at point.
Do not ask for confirmation."
  (interactive)
  (mu4e-view-mark-for-refile)
  (mu4e-mark-execute-all t))

;;;;;; Trash

;;;###autoload
(defun mu4e-extras-headers-trash ()
  "In headers mode, trash message at point.
Do not ask for confirmation."
  (interactive)
  (mu4e-headers-mark-for-trash)
  (mu4e-mark-execute-all t))

;;;###autoload
(defun mu4e-extras-view-trash ()
  "In view mode, trash message at point.
Do not ask for confirmation."
  (interactive)
  (mu4e-view-mark-for-trash)
  (mu4e-mark-execute-all t))

;;;;;; Move

;;;###autoload
(defun mu4e-extras-headers-move ()
  "In headers mode, move and execute message at point.
Do not ask for confirmation."
  (interactive)
  (mu4e-headers-mark-for-move)
  (mu4e-mark-execute-all t))

;;;###autoload
(defun mu4e-extras-view-move ()
  "In view mode, move and execute message at.
Do not ask for confirmation."
  (interactive)
  (mu4e-view-mark-for-move)
  (mu4e-mark-execute-all t))

;;;;;; Compose

(defun mu4e-extras-compose-reply (&optional wide)
  "Reply to the message at point.

If WIDE is non-nil, make it a \"wide\" reply (a.k.a. \"reply-to-all\"). Else,
prompt the user for the reply type if `mu4e-extras-wide-reply' is `prompt', make
it a narrow reply if `mu4e-extras-wide-reply' is nil, and make it a wide reply
otherwise.."
  (interactive)
  (if (mu4e-message-contact-field-matches-me (mu4e-message-at-point) :from)
      (mu4e-compose-supersede)
    (let ((recipients 0))
      (dolist (field '(:to :cc) recipients)
	(setq recipients
	      (+ recipients (length (mu4e-message-field-at-point field)))))
      (if (> recipients 1)
	  (let* ((wide (or wide (pcase mu4e-extras-wide-reply
				  ('prompt (y-or-n-p "Reply to all? "))
				  (_ mu4e-extras-wide-reply)))))
	    (mu4e-compose-reply wide))
	(mu4e-compose-reply)))))

(defun mu4e-extras-compose-new-externally ()
  "Start writing a new message in Gmail."
  (interactive)
  (browse-url "https://mail.google.com/mail/u/0/#inbox?compose=new"))

;;;;;; Misc

;;;###autoload
(defun mu4e-extras-view-org-capture (&optional arg)
  "In view mode, `org-capture' message at point and refile it.
If invoked with prefix argument, capture without archiving it.

If the message body contains with '[org-capture : KEY]',
interpret KEY as the `org-capture' template key.

If ARG is non-nil, do not refile the message after capturing it."
  (interactive "P")
  (if (or (derived-mode-p 'mu4e-view-mode)
	  (derived-mode-p 'mu4e-headers-mode))
      (let* ((message-body (or (mu4e-message-field (mu4e-message-at-point) :body-txt)
			       ;; inexplicably, the above returns nil
			       ;; for a few non-empty messages; to avoid
			       ;; an error in these cases, we add an
			       ;; empty string as a second disjunct
			       ""))
	     (key (if
		      (string-match "\\[org-capture key: \\(.*\\)\\]" message-body)
		      (match-string-no-properties 1 message-body)
		    "e")))
	(org-capture nil key)
	(unless arg
	  (mu4e-extras-view-refile)))
    (user-error "Not in mu4e-view-mode")))

(defun mu4e-extras-view-in-gmail ()
  "Open Gmail in a browser and view message at point in it."
  (interactive)
  (let* ((id (url-hexify-string
	      (plist-get (mu4e-message-at-point) :message-id)))
	 (url (concat "https://mail.google.com/mail/u/0/#search/rfc822msgid%3A" id)))
    (browse-url url)))

;; github.com/djcb/mu/issues/2198
;; djcbsoftware.nl/code/mu/mu4e/Retrieving-mail.html
(defun mu4e-extras-reindex-db ()
  "Reindex `mu' database."
  (interactive)
  (when (shell-command "pkill -2 -u $UID mu")
    (shell-command "sleep 1")
    (shell-command "mu index")))

(defun mu4e-extras-copy-number-in-title ()
  "Copy amount in subject line."
  (interactive)
  (let ((subject (mu4e-message-field (mu4e-message-at-point) :subject)))
    (when (derived-mode-p 'mu4e-headers-mode)
      (string-match
       "\\(\\(?:[[:digit:]]\\{1,3\\}[,.]\\)*\\(?:[[:digit:]]\\{1,3\\}\\)\\(?:[,.][[:digit:]]\\{0,2\\}\\)*\\)"
       subject)
      (let ((number (match-string 1 subject)))
	(kill-new number)
	(message "Copied \"%s\"" number)))))

(defun mu4e-extras-mark-execute-all-no-confirm ()
  "Execute the actions for all marked messages in this buffer.
Do not ask for user confirmation."
  (interactive)
  (mu4e-mark-execute-all))

(defun mu4e-extras-check-all-mail ()
  "Check all Gmail channels.
It takes `mbsync'a while to check all channels, so I run this function less
frequently than `mu4e-update-mail-and-index', which excludes my archive and
takes just a couple of seconds."
  (interactive)
  (let ((mu4e-get-mail-command "mbsync gmail-all"))
    (mu4e-update-mail-and-index t)))

;;;;; Read & refile

;; Messages that are both refiled and marked as read are re-marked as unread
;; after synchronization with the Gmail server. To fix this, we keep track of
;; the `message-id' property of every message that is refiled and marked as
;; read, re-mark them as read after synchronization.

(defun mu4e-extras-headers-mark-for-read-then-refile ()
  "Mark the message at point as read then refile, adding it to the re-mark list."
  (interactive)
  (let ((msg (mu4e-message-at-point)))
    (mu4e-extras-mark-read-then-refile msg)))

(defun mu4e-extras-mark-read-then-refile (msg)
  "Mark MSG as read then refile, adding MSG to the re-mark list after sync."
  (let ((message-id (mu4e-message-field msg :message-id))
        (target (mu4e-get-refile-folder msg)))
    (add-to-list 'mu4e-extras-read-and-refile-tracker message-id)
    ;; Move the message to the refile folder, removing the `new' flag first
    (mu4e--server-move (mu4e-message-field msg :docid) target "+S-u-N"))
  (mu4e-mark-execute-all t))

(defun mu4e-extras-reapply-read-status ()
  "Reapply `read' status to all tracked messages.
The list of tracked messages is stored in `mu4e-extras-read-and-refile-tracker'."
  (dolist (message-id mu4e-extras-read-and-refile-tracker)
    ;; Mark the message as read
    (mu4e--server-move message-id nil "+S"))  ;; '+S' means mark as read (seen)
  ;; Reset the tracker list
  (setq mu4e-extras-read-and-refile-tracker '()))

(defun mu4e-extras-reapply-read-status-set-timer ()
  "Set a timer to reapply `read' status to all tracked messages."
  (run-with-timer 30 nil #'mu4e-extras-reapply-read-status))

(add-hook 'mu4e-update-pre-hook #'mu4e-extras-reapply-read-status-set-timer)

;;;;; Contexts

(defun mu4e-extras-set-contexts ()
  "Set `mu4e-contexts'."
  (setq mu4e-contexts
	`(,(make-mu4e-context
            :name "1 Personal HTML"
            :match-func #'mu4e-extras-msg-is-personal-and-html-p
            :vars `((user-mail-address . ,(getenv "PERSONAL_GMAIL"))
		    (org-msg-signature . ,org-msg-extras-personal-html-signature)))
	  ,(make-mu4e-context
            :name "2 Personal plain text"
            :match-func #'mu4e-extras-msg-is-personal-and-plain-text-p
	    :enter-func (lambda () (org-msg-mode -1))
	    :leave-func #'org-msg-mode
            :vars `((user-mail-address . ,(getenv "PERSONAL_GMAIL"))
		    (org-msg-signature . ,org-msg-extras-personal-plain-text-signature)))
	  ,(make-mu4e-context
            :name "3 Work HTML"
            :match-func #'mu4e-extras-msg-is-work-and-html-p
            :vars `((user-mail-address . ,(getenv "WORK_EMAIL"))
		    (org-msg-signature . ,org-msg-extras-work-html-signature)))
	  ,(make-mu4e-context
            :name "4 Work plain text"
            :match-func #'mu4e-extras-msg-is-work-and-plain-text-p
	    :enter-func (lambda () (org-msg-mode -1))
	    :leave-func #'org-msg-mode
            :vars `((user-mail-address . ,(getenv "WORK_EMAIL"))
		    (org-msg-signature . ,org-msg-extras-work-plain-text-signature))))))

(defun mu4e-extras-msg-is-personal-and-html-p (msg)
  "Return t iff MSG is a personal HTML message."
  (when msg
    (and (org-msg-extras-msg-is-html-p)
	 (mu4e-extras-msg-is-personal-p msg))))

(defun mu4e-extras-msg-is-personal-and-plain-text-p (msg)
  "Return t iff MSG is a personal plain text message."
  (when msg
    (and (not (org-msg-extras-msg-is-html-p))
	 (mu4e-extras-msg-is-personal-p msg))))

(defun mu4e-extras-msg-is-work-and-html-p (msg)
  "Return t iff MSG is a work HTML message."
  (when msg
    (and (org-msg-extras-msg-is-html-p)
	 (mu4e-extras-msg-is-work-p msg))))

(defun mu4e-extras-msg-is-work-and-plain-text-p (msg)
  "Return t iff MSG is a work plain text message."
  (when msg
    (and (not (org-msg-extras-msg-is-html-p))
	 (mu4e-extras-msg-is-work-p msg))))

(defun mu4e-extras-msg-is-personal-p (msg)
  "Return t iff MSG is a personal message."
  (or (mu4e-message-contact-field-matches msg :to (getenv "PERSONAL_GMAIL"))
      (mu4e-message-contact-field-matches msg :to (getenv "PERSONAL_EMAIL"))))

(defun mu4e-extras-msg-is-work-p (msg)
  "Return t iff MSG is a work message."
  (or (mu4e-message-contact-field-matches msg :to (getenv "WORK_EMAIL"))
      (mu4e-message-contact-field-matches msg :reply-to "tlon-team@googlegroups.com")))

;;;;; Patches

;; do not prompt for an URL number when there is only one URL
(el-patch-defun mu4e--view-get-urls-num (prompt &optional multi)
  "Ask the user with PROMPT for an URL number for MSG.
The number is [1..n] for URLs \[0..(n-1)] in the message. If
MULTI is nil, return the number for the URL; otherwise (MULTI is
non-nil), accept ranges of URL numbers, as per
`mu4e-split-ranges-to-numbers', and return the corresponding
string."
  (let* ((count (hash-table-count mu4e--view-link-map)) (def))
    (when (zerop count) (mu4e-error "No links for this message"))
    (if (not multi)
	(if (= count 1)
	    (el-patch-swap (read-number (mu4e-format "%s: " prompt) 1) 1)
	  (read-number (mu4e-format "%s (1-%d): " prompt count)))
      (progn
	(setq def (if (= count 1) "1" (format "1-%d" count)))
	(read-string (mu4e-format "%s (default %s): " prompt def)
		     nil nil def)))))

(provide 'mu4e-extras)
;;; mu4e-extras.el ends here
