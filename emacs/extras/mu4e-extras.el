;;; mu4e-extras.el --- Extensions for mu4e -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/mu4e-extras.el
;; Version: 0.2
;; Package-Requires: ((mu4e "1.4.0") (el-patch "1.1"))

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

(defcustom mu4e-extras-newsletters-folder ""
  "Name of the `newsletters' folder."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-epoch-inbox-folder ""
  "Name of the Epoch AI `inbox' folder."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-epoch-sent-folder ""
  "Name of the Epoch AI `sent' folder."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-epoch-drafts-folder ""
  "Name of the Epoch AI `drafts' folder."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-epoch-refiled-folder ""
  "Name of the Epoch AI `refiled' folder."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-epoch-trash-folder ""
  "Name of the Epoch AI `trash' folder."
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

(defvar mu4e-extras-mark-as-read-queue '()
  "List of `:message-id' of messages to be marked as read after synchronization.")

;;;; Functions

(defun mu4e-extras-init ()
  "Initialize the `mu' database, killing `mu4e' if it is running."
  (interactive)
  (mu4e-kill-update-mail)
  (mu4e-quit)
  (async-shell-command "muinit" "*muinit*"))

;;;;; Post-processing

;;;;;; Fix flags

(defun mu4e-extras-gmail-fix-flags (mark msg)
  "Fix Gmail flags for each MARK and MSG pair.
Skip Epoch messages, which are synced via the Gmail API directly."
  (unless (mu4e-extras-msg-belongs-to-epoch-p msg)
    (cond ((eq mark 'trash)  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
          ((eq mark 'refile) (mu4e-action-retag-message msg "-\\Inbox,+\\Refiled"))
          ((eq mark 'flag)   (mu4e-action-retag-message msg "+\\Starred"))
          ((eq mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

;;;;;; Mark as read

(defun mu4e-extras-reapply-read-status ()
  "Reapply `read' status to all messages in queue.
The list of queued messages is stored in `mu4e-extras-mark-as-read-queue'."
  (dolist (message-id mu4e-extras-mark-as-read-queue)
    (mu4e--server-move message-id nil "+S-u"))  ;; `+S-u' = mark as seen
  (setq mu4e-extras-mark-as-read-queue '()))

(defun mu4e-extras-reapply-read-status-set-timer ()
  "Set a timer to reapply `read' status to all tracked messages."
  (when mu4e-extras-mark-as-read-queue
    (run-with-timer 30 nil #'mu4e-extras-reapply-read-status)))

;;;;;;; Refiled

(defun mu4e-extras-add-refiled-to-mark-as-read-queue (msg)
  "Add MSG to the queue of messages to mark as read upon resync.
Messages that are both refiled and marked as read are re-marked as unread after
synchronization with the Gmail server. To fix this, we keep track of the
`message-id' property of every message that is refiled and marked as read,
and re-mark them as read after synchronization."
  (let ((message-id (mu4e-message-field msg :message-id)))
    (add-to-list 'mu4e-extras-mark-as-read-queue message-id)
    (mu4e-headers-mark-for-refile)
    (mu4e-mark-execute-all t)))

;;;;;;; Sent

(defun mu4e-extras-add-sent-to-mark-as-read-queue ()
  "Add the sent message to the queue of messages to mark as read upon resync.
When `mu4e' sends an email with Gmail, Gmail automatically saves a copy in the
\"Sent\" folder, so the local copy is deleted (as specified by
`mu4e-sent-messages-behavior'). However, the saved copy is treated as a new,
unread message when synchronized back to the local client. To fix this, this
function marks the saved copy as read."
  (when-let* ((message-id (message-fetch-field "Message-ID")))
    (add-to-list 'mu4e-extras-mark-as-read-queue message-id)))

;;;;; Setup

(defun mu4e-extras-set-shortcuts ()
  "Set `mu4e-maildir-shortcuts'."
  (dolist (shortcut `((:maildir ,mu4e-extras-inbox-folder :key ?i)
		      (:maildir ,mu4e-extras-daily-folder :key ?y)
		      (:maildir ,mu4e-extras-epoch-inbox-folder :key ?I)
		      (:maildir ,mu4e-extras-epoch-sent-folder :key ?T)
		      (:maildir ,mu4e-extras-epoch-drafts-folder :key ?D)
		      (:maildir ,mu4e-extras-epoch-refiled-folder :key ?R)
		      (:maildir ,mu4e-extras-epoch-trash-folder :key ?X)))
    (add-to-list 'mu4e-maildir-shortcuts shortcut)))

(defun mu4e-extras-set-bookmarks ()
  "Set `mu4e-bookmarks'."
  (add-to-list 'mu4e-bookmarks
	       `(:name "All inboxes"
		       :query ,(format "maildir:%s OR maildir:%s"
				       mu4e-extras-inbox-folder
				       mu4e-extras-epoch-inbox-folder)
		       :key ?a)))

(defun mu4e-extras-set-face-locally ()
  "Set `shr-text' face locally in `mu4ew-view-mode' buffers."
  (when (derived-mode-p 'mu4e-view-mode)
    (face-remap-add-relative 'shr-text :height 0.9)))

;;;;; Commands

;;;###autoload
(defun mu4e-extras-update-all-mail-and-index (&optional run-in-background)
  "Like `mu4e-update-mail-and-index', but include the \"all mail\" folder.
If RUN-IN-BACKGROUND is non-nil (or called with prefix-argument), run in the
background; otherwise, pop up a window."
  (interactive "P")
  (let ((mu4e-get-mail-command "sh -c 'mbsync gmail-all & gmail-maildir-sync pull --include-all & wait'"))
    (mu4e-update-mail-and-index run-in-background)))

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
  "Mark the message at point as read then refile, adding it to the re-mark list."
  (interactive)
  (mu4e-extras-add-refiled-to-mark-as-read-queue (mu4e-message-at-point)))

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

;;;;;; Message files

(defun mu4e-extras-get-message-file ()
  "Return the path to the message at point.
If no message is found, return nil."
  (when-let* ((message (condition-case nil
			   (or mu4e-compose-parent-message (mu4e-message-at-point))
			 (error nil))))
    (mu4e-message-field message :path)))

(defun mu4e-extras-open-message-file ()
  "Open the current message's file in Dired."
  (interactive)
  (when-let* ((path (mu4e-extras-get-message-file)))
    (dired-jump nil path)))

;;;;;; Gmail

(defun mu4e-extras-gmail-base (&optional msg)
  "Return base Gmail URL for the account that MSG belongs to.
Uses the email address in the URL path so Gmail switches to the
correct account automatically."
  (let ((account (if (and msg (mu4e-extras-msg-belongs-to-epoch-p msg))
		     (getenv "EPOCH_EMAIL")
		   (getenv "PERSONAL_GMAIL"))))
    (format "https://mail.google.com/mail/u/%s/" account)))

(defun mu4e-extras-open-gmail ()
  "Open Gmail in a browser."
  (interactive)
  (browse-url (concat (mu4e-extras-gmail-base) "#inbox")))

(defun mu4e-extras-view-in-gmail ()
  "Open Gmail in a browser and view message at point in it."
  (interactive)
  (let* ((msg (mu4e-message-at-point))
	 (id (url-hexify-string (plist-get msg :message-id)))
	 (url (concat (mu4e-extras-gmail-base msg) "#search/rfc822msgid%3A" id)))
    (browse-url url)))

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
  "Check all mail channels including archives.
It takes `mbsync' a while to check all channels, so I run this function less
frequently than `mu4e-update-mail-and-index', which excludes the archive and
takes just a couple of seconds."
  (interactive)
  (let ((mu4e-get-mail-command "sh -c 'mbsync gmail-all & gmail-maildir-sync pull --include-all & wait'"))
    (mu4e-update-mail-and-index t)))

;;;;; Sending via Gmail API

(defun mu4e-extras-send-via-gmail-api ()
  "Send the current message via `gmail-maildir-sync' Gmail API.
This bypasses `message-send-mail-with-sendmail', which adds
sendmail-specific flags (`-oi', `-f', `-t') that
`gmail-maildir-sync' does not accept.  It also strips the
`mail-header-separator' line that Emacs keeps in the compose
buffer, replacing it with the blank line RFC 2822 requires
between headers and body."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "\n"))
    (replace-match "\n"))
  (let ((errbuf (generate-new-buffer " *gmail-api-send-errors*")))
    (unwind-protect
        (let ((exit-code (call-process-region (point-min) (point-max)
                                              "gmail-maildir-sync"
                                              nil errbuf nil
                                              "send")))
          (unless (zerop exit-code)
            (error "gmail-maildir-sync send failed (exit %d): %s"
                   exit-code
                   (with-current-buffer errbuf
                     (string-trim (buffer-string))))))
      (kill-buffer errbuf))))

;;;;; Contexts

(declare-function org-msg-mode "org-msg")
(defvar org-msg-extras-personal-html-signature)
(defvar org-msg-extras-personal-plain-text-signature)
(defvar org-msg-extras-work-html-signature)
(defvar org-msg-extras-work-plain-text-signature)
(defun mu4e-extras-set-contexts ()
  "Set `mu4e-contexts'."
  (setq mu4e-contexts
	`(,(make-mu4e-context
            :name "1 Personal HTML"
            :match-func #'mu4e-extras-msg-is-personal-and-html-p
            :vars `((user-mail-address . ,(getenv "PERSONAL_GMAIL"))
		    (smtpmail-smtp-user . ,(getenv "PERSONAL_GMAIL"))
		    (message-send-mail-function . smtpmail-send-it)
		    (org-msg-signature . ,org-msg-extras-personal-html-signature)))
	  ,(make-mu4e-context
            :name "2 Personal plain text"
            :match-func #'mu4e-extras-msg-is-personal-and-plain-text-p
	    :enter-func (lambda () (org-msg-mode -1))
	    :leave-func #'org-msg-mode
            :vars `((user-mail-address . ,(getenv "PERSONAL_GMAIL"))
		    (smtpmail-smtp-user . ,(getenv "PERSONAL_GMAIL"))
		    (message-send-mail-function . smtpmail-send-it)
		    (org-msg-signature . ,org-msg-extras-personal-plain-text-signature)))
	  ,(make-mu4e-context
            :name "3 Epoch HTML"
            :match-func #'mu4e-extras-msg-is-epoch-and-html-p
            :vars `((user-mail-address . ,(getenv "EPOCH_EMAIL"))
		    (smtpmail-smtp-user . ,(getenv "EPOCH_EMAIL"))
		    (mu4e-sent-folder . "/epoch/Sent")
		    (mu4e-drafts-folder . "/epoch/Drafts")
		    (message-send-mail-function . mu4e-extras-send-via-gmail-api)
		    (org-msg-signature . ,org-msg-extras-work-html-signature)))
	  ,(make-mu4e-context
            :name "4 Epoch plain text"
            :match-func #'mu4e-extras-msg-is-epoch-and-plain-text-p
	    :enter-func (lambda () (org-msg-mode -1))
	    :leave-func #'org-msg-mode
            :vars `((user-mail-address . ,(getenv "EPOCH_EMAIL"))
		    (smtpmail-smtp-user . ,(getenv "EPOCH_EMAIL"))
		    (mu4e-sent-folder . "/epoch/Sent")
		    (mu4e-drafts-folder . "/epoch/Drafts")
		    (message-send-mail-function . mu4e-extras-send-via-gmail-api)
		    (org-msg-signature . ,org-msg-extras-work-plain-text-signature)))
	  ,(make-mu4e-context
            :name "5 Tlon HTML"
            :match-func #'mu4e-extras-msg-is-tlon-and-html-p
            :vars `((user-mail-address . ,(getenv "WORK_EMAIL"))
		    (smtpmail-smtp-user . ,(getenv "WORK_EMAIL"))
		    (message-send-mail-function . smtpmail-send-it)
		    (org-msg-signature . ,org-msg-extras-work-html-signature)))
	  ,(make-mu4e-context
            :name "6 Tlon plain text"
            :match-func #'mu4e-extras-msg-is-tlon-and-plain-text-p
	    :enter-func (lambda () (org-msg-mode -1))
	    :leave-func #'org-msg-mode
            :vars `((user-mail-address . ,(getenv "WORK_EMAIL"))
		    (smtpmail-smtp-user . ,(getenv "WORK_EMAIL"))
		    (message-send-mail-function . smtpmail-send-it)
		    (org-msg-signature . ,org-msg-extras-work-plain-text-signature))))))

;;;;; Account detection

(defun mu4e-extras-msg-belongs-to-epoch-p (msg)
  "Return t iff MSG belongs to the Epoch AI account."
  (when-let* ((maildir (mu4e-message-field msg :maildir)))
    (string-prefix-p "/epoch/" maildir)))

;;;;; Account folders

(defun mu4e-extras-refile-folder (msg)
  "Return the refile folder for MSG."
  (if (mu4e-extras-msg-belongs-to-epoch-p msg)
      "/epoch/Refiled"
    "/Refiled"))

(defun mu4e-extras-trash-folder (msg)
  "Return the trash folder for MSG."
  (if (mu4e-extras-msg-belongs-to-epoch-p msg)
      "/epoch/Trash"
    "/Trash"))

(defun mu4e-extras-set-account-folders ()
  "Set folder variables for multi-account support."
  (setq mu4e-refile-folder #'mu4e-extras-refile-folder
	mu4e-trash-folder #'mu4e-extras-trash-folder))

;;;;; Context matching

(defun mu4e-extras-msg-is-personal-and-html-p (msg)
  "Return t iff MSG is a personal HTML message."
  (when msg
    (and (mu4e-extras-msg-is-html-p msg)
	 (mu4e-extras-msg-is-personal-p msg))))

(defun mu4e-extras-msg-is-personal-and-plain-text-p (msg)
  "Return t iff MSG is a personal plain text message."
  (when msg
    (and (not (mu4e-extras-msg-is-html-p msg))
	 (mu4e-extras-msg-is-personal-p msg))))

(defun mu4e-extras-msg-is-epoch-and-html-p (msg)
  "Return t iff MSG is an Epoch AI HTML message."
  (when msg
    (and (mu4e-extras-msg-is-html-p msg)
	 (mu4e-extras-msg-is-epoch-p msg))))

(defun mu4e-extras-msg-is-epoch-and-plain-text-p (msg)
  "Return t iff MSG is an Epoch AI plain text message."
  (when msg
    (and (not (mu4e-extras-msg-is-html-p msg))
	 (mu4e-extras-msg-is-epoch-p msg))))

(defun mu4e-extras-msg-is-tlon-and-html-p (msg)
  "Return t iff MSG is a Tlon HTML message."
  (when msg
    (and (mu4e-extras-msg-is-html-p msg)
	 (mu4e-extras-msg-is-tlon-p msg))))

(defun mu4e-extras-msg-is-tlon-and-plain-text-p (msg)
  "Return t iff MSG is a Tlon plain text message."
  (when msg
    (and (not (mu4e-extras-msg-is-html-p msg))
	 (mu4e-extras-msg-is-tlon-p msg))))

(defun mu4e-extras-msg-is-personal-p (msg)
  "Return t iff MSG is a personal message."
  (or (mu4e-message-contact-field-matches msg :to (getenv "PERSONAL_GMAIL"))
      (mu4e-message-contact-field-matches msg :to (getenv "PERSONAL_EMAIL"))))

(defun mu4e-extras-msg-is-epoch-p (msg)
  "Return t iff MSG is an Epoch AI message."
  (mu4e-message-contact-field-matches msg :to (getenv "EPOCH_EMAIL")))

(defun mu4e-extras-msg-is-tlon-p (msg)
  "Return t iff MSG is a Tlon message."
  (or (mu4e-message-contact-field-matches msg :to (getenv "WORK_EMAIL"))
      (mu4e-message-contact-field-matches msg :reply-to "tlon-team@googlegroups.com")))

;; based on `org-msg-article-htmlp-mu4e'
(defun mu4e-extras-msg-is-html-p (msg)
  "Return t iff MSG is an HTML message."
  (with-temp-buffer
    (insert-file-contents-literally
     (mu4e-message-readable-path msg) nil nil nil t)
    (when-let* ((parts (mm-dissect-buffer t t)))
      (mm-destroy-parts parts)
      (stringp (cl-find "text/html" (flatten-tree parts) :test 'equal)))))

;;;;; Index

;; https://github.com/djcb/mu/issues/2778#issuecomment-2485462344
(defun mu4e-extras-set-index-params ()
  "Set the index parameters for the current update.
This is dependent on if I'm active (hence wanting a quick update) or
away (in which case it can take its time). Ideally we would do this
right before the index but currently there is no hook for that.
`mu4e-index-cleanup' is always t to avoid stale index errors when
mbsync removes files between index runs."
  (let* ((raw-idle (current-idle-time))
	 (idle (if raw-idle (time-convert raw-idle 'integer) 0))
         (old-lazy mu4e-index-lazy-check))
    (if (and raw-idle
             (> idle mu4e-update-interval))
	(setopt mu4e-index-lazy-check nil)
      (setopt mu4e-index-lazy-check t))
    (when (not (eq old-lazy mu4e-index-lazy-check))
      (message "`mu4e-extras-set-index-params' idle:%s lazy:%s"
               idle mu4e-index-lazy-check))))

;; TODO: this should once daily remove the `refile' label from all messages and
;; sync all mail (so that the archive is up to date)
(defun mu4e-extras-archive-refiled ()
  "Archive messages with the \"refile\" label."
  (let ((mu4e-get-mail-command "sh $HOME/bin/mbsync-parallel include-gmail-all"))
    (setopt mu4e-index-lazy-check nil
	    mu4e-index-cleanup t)
    (mu4e-update-index)))

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
