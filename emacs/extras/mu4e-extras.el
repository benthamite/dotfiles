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

(require 'cl-lib)
(require 'el-patch)
(require 'mu4e)
(require 'seq)

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

(defcustom mu4e-extras-forethought-inbox-folder ""
  "Name of the Forethought `inbox' folder."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-forethought-sent-folder ""
  "Name of the Forethought `sent' folder."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-forethought-drafts-folder ""
  "Name of the Forethought `drafts' folder."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-forethought-refiled-folder ""
  "Name of the Forethought `refiled' folder."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-forethought-trash-folder ""
  "Name of the Forethought `trash' folder."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-gmail-api-accounts
  '((:name "epoch"
     :label "Epoch"
     :address-env "EPOCH_EMAIL"
     :maildir "/epoch/"
     :chrome-profile "Profile 2"
     :state-db "~/.local/share/gmail-maildir-sync/state.db"
     :html-signature org-msg-extras-epoch-html-signature
     :plain-text-signature org-msg-extras-epoch-plain-text-signature)
    (:name "forethought"
     :label "Forethought"
     :address "pablo@forethought.org"
     :maildir "/forethought/"
     :chrome-profile "Profile 10"
     :state-db "~/.local/share/gmail-maildir-sync/forethought/state.db"
     :html-signature org-msg-extras-forethought-html-signature
     :plain-text-signature org-msg-extras-forethought-plain-text-signature))
  "Accounts synced and sent through `gmail-maildir-sync' via the Gmail API.
Each element is a plist with these keys:

`:name'                 account name passed to `gmail-maildir-sync --account'.
`:label'                human-readable name used in context names.
`:address'              the account's email address, or nil to use
                        `:address-env'.
`:address-env'          environment variable holding the email address.
`:maildir'              maildir prefix owned by the account, with leading and
                        trailing slashes, e.g. \"/epoch/\".
`:chrome-profile'       Chrome profile directory used to open the account's
                        Gmail; see `mu4e-extras-chrome-profile-personal'.
`:state-db'             path to the account's `gmail-maildir-sync' SQLite state
                        database.
`:html-signature'       variable holding the HTML `org-msg' signature.
`:plain-text-signature' variable holding the plain-text signature.

A message belongs to an account when its maildir starts with the account's
`:maildir'.  That ownership decides the refile and trash folders, the Chrome
profile, the Gmail thread lookup, the reaction send path and the compose
context."
  :type '(repeat (plist :value-type sexp))
  :group 'mu4e-extras)

(defcustom mu4e-extras-wide-reply 'prompt
  "Whether the reply to messages should be \"wide\" (a.k.a. \"reply-to-all\").
If `prompt', ask the user. If t, always reply to all. If nil, always reply to
the sender only."
  :type '(choice (const :tag "Prompt" prompt)
		 (const :tag "Yes" t)
		 (const :tag "No" nil)))

(defcustom mu4e-extras-reaction-emojis
  '("👍" "👎" "❤️" "😂" "😮" "😢" "🤔" "🔥" "👏" "🙏" "✅" "👀" "🎉" "💯")
  "List of emoji choices for Gmail message reactions."
  :type '(repeat string)
  :group 'mu4e-extras)

(defcustom mu4e-extras-chrome-profile-personal "Default"
  "Chrome profile directory for personal email.
This is the on-disk directory name inside
\"~/Library/Application Support/Google/Chrome/\",
not the display name shown in Chrome's UI.  Use
\"Default\" for the first profile, \"Profile 1\" for
the second, etc."
  :type 'string
  :group 'mu4e-extras)

(defcustom mu4e-extras-chrome-program
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  "Path to the Google Chrome executable."
  :type 'file
  :group 'mu4e-extras)

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
Skip messages of `mu4e-extras-gmail-api-accounts', which are synced via the
Gmail API directly."
  (unless (mu4e-extras-msg-account msg)
    (cond ((eq mark 'trash)  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
          ((eq mark 'refile) (mu4e-action-retag-message msg "-\\Inbox,+\\Refiled"))
          ((eq mark 'flag)   (mu4e-action-retag-message msg "+\\Starred"))
          ((eq mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

;;;;;; Mark as read

(defvar mu4e-extras--reapply-in-progress nil
  "Non-nil while a follow-up sync is running to push re-applied read status.")

(defun mu4e-extras-reapply-read-status ()
  "Reapply `read' status to all messages in queue, then trigger a follow-up sync.
Gmail resets the Seen flag when a message is moved between mailboxes via IMAP,
so after a sync that includes refiles, we re-apply the flag locally and push it
to Gmail with an automatic follow-up sync.

This function runs on `mu4e-index-updated-hook'.  The
`mu4e-extras--reapply-in-progress' guard prevents infinite loops."
  (when (and mu4e-extras-mark-as-read-queue
	     (not mu4e-extras--reapply-in-progress))
    (let ((count (length mu4e-extras-mark-as-read-queue)))
      (dolist (message-id mu4e-extras-mark-as-read-queue)
	(mu4e--server-move message-id nil "+S-u"))
      (setq mu4e-extras-mark-as-read-queue '())
      (message "mu4e-extras: re-applied read status to %d message(s), syncing..." count)
      ;; Trigger a follow-up sync to push the re-applied flags to Gmail.
      (setq mu4e-extras--reapply-in-progress t)
      (run-with-timer 2 nil
		      (lambda ()
			(unwind-protect
			    (mu4e-update-mail-and-index t)
			  (run-with-timer 60 nil
					  (lambda ()
					    (setq mu4e-extras--reapply-in-progress nil)))))))))

;;;;;;; Shared

(defun mu4e-extras--preserve-read-status (msg)
  "If MSG is already read, add it to the mark-as-read queue.
Gmail may reset the Seen flag when a message is refiled via IMAP.
By queuing already-read messages, we re-apply the Seen flag after sync."
  (when (memq 'seen (mu4e-message-field msg :flags))
    (add-to-list 'mu4e-extras-mark-as-read-queue
		 (mu4e-message-field msg :message-id))))

;;;;;;; Refiled

(defun mu4e-extras-add-refiled-to-mark-as-read-queue (msg)
  "Add MSG to the queue of messages to mark as read upon resync.
Messages that are both refiled and marked as read are re-marked as unread after
synchronization with the Gmail server.  To fix this, we keep track of the
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
  (when-let* ((raw-id (message-fetch-field "Message-ID"))
	      ;; `message-fetch-field' returns the RFC 2822 form with angle
	      ;; brackets, but `mu4e--server-move' needs the bare message-id.
	      (message-id (replace-regexp-in-string "\\`<\\|>\\'" "" raw-id)))
    (add-to-list 'mu4e-extras-mark-as-read-queue message-id)))

;;;;; Setup

(defun mu4e-extras-set-shortcuts ()
  "Set the variable `mu4e-maildir-shortcuts'."
  (dolist (shortcut `((:maildir ,mu4e-extras-inbox-folder :key ?i)
		      (:maildir ,mu4e-extras-daily-folder :key ?y)
		      (:maildir ,mu4e-extras-epoch-inbox-folder :key ?I)
		      (:maildir ,mu4e-extras-epoch-sent-folder :key ?T)
		      (:maildir ,mu4e-extras-epoch-drafts-folder :key ?D)
		      (:maildir ,mu4e-extras-epoch-refiled-folder :key ?R)
		      (:maildir ,mu4e-extras-epoch-trash-folder :key ?X)
		      (:maildir ,mu4e-extras-forethought-inbox-folder :key ?f)
		      (:maildir ,mu4e-extras-forethought-sent-folder :key ?s)
		      (:maildir ,mu4e-extras-forethought-drafts-folder :key ?w)
		      (:maildir ,mu4e-extras-forethought-refiled-folder :key ?F)
		      (:maildir ,mu4e-extras-forethought-trash-folder :key ?z)))
    (add-to-list 'mu4e-maildir-shortcuts shortcut)))

(defun mu4e-extras-set-bookmarks ()
  "Set the variable `mu4e-bookmarks'.
Replace any existing \"All inboxes\" bookmark, so that calling this function
again after changing the inbox folders does not add a second one."
  (setq mu4e-bookmarks (cl-remove "All inboxes" mu4e-bookmarks
				  :key (lambda (bookmark) (plist-get bookmark :name))
				  :test #'equal))
  (add-to-list 'mu4e-bookmarks
	       `(:name "All inboxes"
		       :query ,(mu4e-extras-all-inboxes-query)
		       :key ?a)))

(defun mu4e-extras-all-inboxes-query ()
  "Return a `mu' query matching the personal, Epoch and Forethought inboxes.
Inbox folders whose name is empty are omitted."
  (mapconcat (lambda (folder) (concat "maildir:" folder))
	     (seq-remove #'string-empty-p
			 (list mu4e-extras-inbox-folder
			       mu4e-extras-epoch-inbox-folder
			       mu4e-extras-forethought-inbox-folder))
	     " OR "))

(defun mu4e-extras-set-face-locally ()
  "Set `shr-text' face locally in `mu4ew-view-mode' buffers."
  (when (derived-mode-p 'mu4e-view-mode)
    (face-remap-add-relative 'shr-text :height 0.9)))

(defun mu4e-extras--update-mail-and-index-with-live-directory (orig-fn &rest args)
  "Call ORIG-FN with ARGS from a directory that exists."
  (let ((default-directory (mu4e-extras--live-default-directory)))
    (apply orig-fn args)))

(defun mu4e-extras--live-default-directory ()
  "Return `default-directory' if it exists, or the home directory."
  (if (file-directory-p default-directory)
      default-directory
    (expand-file-name "~/")))

(advice-add 'mu4e-update-mail-and-index :around
	    #'mu4e-extras--update-mail-and-index-with-live-directory)

;;;;; Commands

;;;###autoload
(defun mu4e-extras-update-all-mail-and-index (&optional run-in-background)
  "Like `mu4e-update-mail-and-index', but include the \"all mail\" folder.
If RUN-IN-BACKGROUND is non-nil (or called with prefix-argument), run in the
background; otherwise, pop up a window."
  (interactive "P")
  (let ((mu4e-get-mail-command (mu4e-extras-all-mail-command)))
    (mu4e-update-mail-and-index run-in-background)))

(defun mu4e-extras-all-mail-command ()
  "Return the shell command that syncs all mail, archives included.
The command runs `mbsync gmail-all' and a full `gmail-maildir-sync' pull for
each account in `mu4e-extras-gmail-api-accounts' in parallel.  It waits for each
process separately and exits with the status of a failed one, so that a failure
in one sync is not masked by the others."
  (let ((commands (cons "mbsync gmail-all"
			(mapcar (lambda (account)
				  (format "gmail-maildir-sync --account %s pull --include-all"
					  (plist-get account :name)))
				mu4e-extras-gmail-api-accounts))))
    (format "sh -c 'pids=; %sstatus=0; for pid in $pids; do wait $pid || status=$?; done; exit $status'"
	    (mapconcat (lambda (command) (concat command " & pids=\"$pids $!\"; "))
		       commands ""))))

;;;;;; Refile

;;;###autoload
(defun mu4e-extras-headers-refile ()
  "In headers mode, refile message at point.
Do not ask for confirmation."
  (interactive)
  (mu4e-extras--preserve-read-status (mu4e-message-at-point))
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
  (mu4e-extras--preserve-read-status (mu4e-message-at-point))
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
  (mu4e-extras-browse-url-in-chrome-profile
   "https://mail.google.com/mail/u/0/#inbox?compose=new"
   mu4e-extras-chrome-profile-personal))

;;;;;; Message files

(defun mu4e-extras-get-message-file ()
  "Return the path to the message at point.
If no message is found, return nil."
  (when-let* ((message (condition-case nil
			   (or mu4e-compose-parent-message (mu4e-message-at-point))
			 (error nil))))
    (mu4e-message-field message :path)))

;;;###autoload
(defun mu4e-extras-copy-message-path ()
  "Copy the path to the current message's file to the kill ring."
  (interactive)
  (if-let* ((path (mu4e-extras-get-message-file)))
      (progn
	(kill-new path)
	(message "Copied: %s" path))
    (user-error "No message at point")))

(defun mu4e-extras-open-message-file ()
  "Open the current message's file in Dired."
  (interactive)
  (when-let* ((path (mu4e-extras-get-message-file)))
    (dired-jump nil path)))

;;;;;; Gmail

(defun mu4e-extras-browse-url-in-chrome-profile (url profile)
  "Open URL in the Chrome profile named PROFILE.
PROFILE is a directory name like \"Default\" or \"Profile 2\"."
  (start-process "chrome" nil mu4e-extras-chrome-program
		 (concat "--profile-directory=" profile) "--new-tab" url))

(defun mu4e-extras-chrome-profile-for-msg (&optional msg)
  "Return the Chrome profile directory for MSG.
Return the `:chrome-profile' of the Gmail API account owning MSG, and
`mu4e-extras-chrome-profile-personal' otherwise."
  (if-let* ((account (mu4e-extras-msg-account msg)))
      (plist-get account :chrome-profile)
    mu4e-extras-chrome-profile-personal))

(defun mu4e-extras-gmail-base (&optional _msg)
  "Return the base Gmail URL.
Uses account index 0, since Chrome profile selection already
routes to the correct account."
  "https://mail.google.com/mail/u/0/")

(defun mu4e-extras-open-gmail ()
  "Open Gmail in a browser.
When called from a message buffer, opens the correct Chrome
profile for that message's account."
  (interactive)
  (let ((msg (ignore-errors (mu4e-message-at-point))))
    (mu4e-extras-browse-url-in-chrome-profile
     (concat (mu4e-extras-gmail-base msg) "#inbox")
     (mu4e-extras-chrome-profile-for-msg msg))))

(defun mu4e-extras-gmail-message-id (msg)
  "Return the Gmail hex message ID for MSG, or nil.
Reads the `X-Gmail-Message-Id' header from the message file on disk."
  (when-let* ((path (plist-get msg :path))
	      ((file-exists-p path)))
    (with-temp-buffer
      (insert-file-contents path nil 0 4096)
      (goto-char (point-min))
      (when (re-search-forward "^X-Gmail-Message-Id: \\([0-9a-f]+\\)" nil t)
	(match-string 1)))))

(defun mu4e-extras-gmail-thread-id (gmail-id account)
  "Return the Gmail thread ID for GMAIL-ID in ACCOUNT, or nil.
ACCOUNT is an element of `mu4e-extras-gmail-api-accounts', or nil.  Query the
`gmail-maildir-sync' state database of ACCOUNT."
  (when-let* ((gmail-id)
	      (db (and account (expand-file-name (plist-get account :state-db))))
	      ((file-exists-p db)))
    (with-temp-buffer
      (when (zerop (call-process "sqlite3" nil t nil
				 db
				 (format "SELECT gmail_thread_id FROM messages WHERE gmail_id = '%s' LIMIT 1"
					 gmail-id)))
	(let ((result (string-trim (buffer-string))))
	  (unless (string-empty-p result)
	    result))))))

(defun mu4e-extras-view-in-gmail ()
  "Open Gmail in a browser and view message at point in it.
For messages of a Gmail API account, open the thread directly using
the Gmail thread ID from that account's `gmail-maildir-sync' state
database.  Otherwise, search by RFC 822 message ID."
  (interactive)
  (let* ((msg (mu4e-message-at-point))
	 (gmail-id (mu4e-extras-gmail-message-id msg))
	 (thread-id (mu4e-extras-gmail-thread-id
		     gmail-id (mu4e-extras-msg-account msg)))
	 (url (if thread-id
		  (concat (mu4e-extras-gmail-base msg) "#all/" thread-id)
		(let ((id (url-hexify-string (plist-get msg :message-id))))
		  (concat (mu4e-extras-gmail-base msg) "#search/rfc822msgid%3A" id)))))
    (mu4e-extras-browse-url-in-chrome-profile
     url (mu4e-extras-chrome-profile-for-msg msg))))

;;;;;; Reactions

;;;###autoload
(defun mu4e-extras-react-to-message (emoji)
  "React to the message at point with EMOJI.
Constructs and sends a Gmail reaction message per Google's email
reaction format specification."
  (interactive (list (mu4e-extras-reaction-read-emoji)))
  (let* ((msg (mu4e-message-at-point))
	 (from (mu4e-extras-reaction-from-address msg))
	 (to (mu4e-extras-reaction-to-email msg))
	 (message-id (mu4e-message-field msg :message-id))
	 (subject (or (mu4e-message-field msg :subject) ""))
	 (mime (mu4e-extras-reaction-build-mime
		from to message-id subject emoji)))
    (mu4e-extras-reaction-send msg to mime)
    (message "Reacted with %s" emoji)))

(defun mu4e-extras-reaction-read-emoji ()
  "Prompt the user to select a reaction emoji."
  (completing-read "React with: " mu4e-extras-reaction-emojis nil t))

(defun mu4e-extras-reaction-from-address (msg)
  "Return the user's email address for the account MSG belongs to.
For a message of a Gmail API account, return that account's address.
Otherwise, check which of the user's addresses appears in the To or Cc
fields of MSG."
  (if-let* ((account (or (mu4e-extras-msg-account msg)
			 (mu4e-extras-reaction-addressed-account msg))))
      (mu4e-extras-account-address account)
    (if (mu4e-extras-reaction-msg-matches-address-p msg (getenv "WORK_EMAIL"))
	(getenv "WORK_EMAIL")
      (getenv "PERSONAL_GMAIL"))))

(defun mu4e-extras-reaction-addressed-account (msg)
  "Return the Gmail API account whose address is in the To or Cc fields of MSG."
  (cl-find-if (lambda (account)
		(mu4e-extras-reaction-msg-matches-address-p
		 msg (mu4e-extras-account-address account)))
	      mu4e-extras-gmail-api-accounts))

(defun mu4e-extras-reaction-msg-matches-address-p (msg address)
  "Return non-nil if ADDRESS appears in the To or Cc fields of MSG."
  (and address
       (or (mu4e-message-contact-field-matches msg :to address)
	   (mu4e-message-contact-field-matches msg :cc address))))

(defun mu4e-extras-reaction-to-email (msg)
  "Return the bare email address of the sender of MSG."
  (plist-get (car (mu4e-message-field msg :from)) :email))

(defun mu4e-extras-reaction-build-mime (from to message-id subject emoji)
  "Build a MIME reaction message.
FROM and TO are email addresses.  MESSAGE-ID is the target
message's bare ID (without angle brackets).  SUBJECT is the
original subject line.  EMOJI is the reaction emoji string."
  (let ((boundary (mu4e-extras-reaction-make-boundary)))
    (concat
     (mu4e-extras-reaction-format-headers
      from to message-id subject boundary)
     (mu4e-extras-reaction-format-body from emoji boundary))))

(defun mu4e-extras-reaction-make-boundary ()
  "Generate a unique MIME boundary string."
  (format "----=_reaction_%s"
	  (sha1 (format "%s%s" (emacs-pid) (float-time)))))

(defun mu4e-extras-reaction-format-headers (from to message-id subject boundary)
  "Format the RFC 2822 headers for a reaction message.
FROM and TO are email addresses.  MESSAGE-ID is the target
message's bare ID.  SUBJECT is the original subject.  BOUNDARY is
the MIME boundary string."
  (format (concat "From: %s\n"
		  "To: %s\n"
		  "Subject: Re: %s\n"
		  "In-Reply-To: <%s>\n"
		  "MIME-Version: 1.0\n"
		  "Content-Type: multipart/alternative;"
		  " boundary=\"%s\"\n\n")
	  from to subject message-id boundary))

(defun mu4e-extras-reaction-format-body (from emoji boundary)
  "Format the multipart MIME body for a reaction message.
FROM is the sender's address (used in fallback text).  EMOJI is
the reaction emoji.  BOUNDARY is the MIME boundary string."
  (let ((plain (format "%s reacted with %s to your message" from emoji))
	(json (format "{\"emoji\":\"%s\",\"version\":1}" emoji))
	(html (format
	       "<html><body><p>%s reacted with %s to your message</p></body></html>"
	       from emoji)))
    (concat
     (mu4e-extras-reaction-format-part boundary "text/plain" plain)
     (mu4e-extras-reaction-format-part
      boundary "text/vnd.google.email-reaction+json" json)
     (mu4e-extras-reaction-format-part boundary "text/html" html)
     (format "--%s--\n" boundary))))

(defun mu4e-extras-reaction-format-part (boundary content-type content)
  "Format a single MIME part.
BOUNDARY is the MIME boundary string.  CONTENT-TYPE is the MIME
type.  CONTENT is the part body."
  (format (concat "--%s\n"
		  "Content-Type: %s; charset=UTF-8\n"
		  "Content-Transfer-Encoding: 8bit\n"
		  "\n"
		  "%s\n")
	  boundary content-type content))

(defun mu4e-extras-reaction-send (msg to mime)
  "Send MIME as a reaction to MSG.
TO is the recipient's bare email address.  Use the Gmail API for
messages of a Gmail API account and SMTP for all others."
  (with-temp-buffer
    (insert mime)
    (if-let* ((account (mu4e-extras-msg-account msg)))
	(mu4e-extras-gmail-api-send-buffer account "Gmail reaction send")
      (mu4e-extras-reaction-send-via-smtp to))))

(declare-function smtpmail-via-smtp "smtpmail")

(defun mu4e-extras-reaction-send-via-smtp (to)
  "Send the current buffer as a reaction via SMTP to TO."
  (when-let* ((result (smtpmail-via-smtp (list to) (current-buffer))))
    (error "SMTP reaction send failed: %s" result)))

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
  (let ((mu4e-get-mail-command (mu4e-extras-all-mail-command)))
    (mu4e-update-mail-and-index t)))

;;;;; Sending via Gmail API

(defun mu4e-extras-send-via-gmail-api ()
  "Send the current message via `gmail-maildir-sync' Gmail API.
This bypasses `message-send-mail-with-sendmail', which adds
sendmail-specific flags (`-oi', `-f', `-t') that
`gmail-maildir-sync' does not accept.  It also strips the
`mail-header-separator' line that Emacs keeps in the compose
buffer, replacing it with the blank line RFC 2822 requires
between headers and body.

The message is sent from the Gmail API account whose address is
`user-mail-address', which the active `mu4e' context sets."
  (let ((account (or (mu4e-extras-account-for-address user-mail-address)
		     (error "No Gmail API account for sender %s" user-mail-address))))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward
       (concat "^" (regexp-quote mail-header-separator) "\n"))
      (replace-match "\n"))
    (mu4e-extras-gmail-api-send-buffer account "Gmail-maildir-sync send")))

(defun mu4e-extras-gmail-api-send-buffer (account description)
  "Send the current buffer as an RFC 2822 message from Gmail API ACCOUNT.
ACCOUNT is an element of `mu4e-extras-gmail-api-accounts'.  DESCRIPTION names
the operation in the error signaled when `gmail-maildir-sync' fails."
  (let ((errbuf (generate-new-buffer " *gmail-api-send-errors*")))
    (unwind-protect
        (let ((exit-code (call-process-region (point-min) (point-max)
                                              "gmail-maildir-sync"
                                              nil errbuf nil
                                              "--account" (plist-get account :name)
                                              "send")))
          (unless (zerop exit-code)
            (error "%s failed (exit %d): %s"
                   description exit-code
                   (with-current-buffer errbuf
                     (string-trim (buffer-string))))))
      (kill-buffer errbuf))))

;;;;; Contexts

(declare-function org-msg-mode "org-msg")
(defvar org-msg-extras-personal-html-signature)
(defvar org-msg-extras-personal-plain-text-signature)
(defvar org-msg-extras-tlon-html-signature)
(defvar org-msg-extras-tlon-plain-text-signature)
(defun mu4e-extras-set-contexts ()
  "Set `mu4e-contexts'.
The personal contexts come first, then an HTML and a plain-text context for
each account in `mu4e-extras-gmail-api-accounts', then the Tlon contexts.
Context names are numbered in that order."
  (setq mu4e-contexts
	(mu4e-extras--number-contexts
	 (append
	  (list (mu4e-extras--make-context
		 "Personal" t #'mu4e-extras-msg-is-personal-and-html-p
		 `((user-mail-address . ,(getenv "PERSONAL_GMAIL"))
		   (smtpmail-smtp-user . ,(getenv "PERSONAL_GMAIL"))
		   (message-send-mail-function . smtpmail-send-it)
		   (org-msg-signature . ,org-msg-extras-personal-html-signature)))
		(mu4e-extras--make-context
		 "Personal" nil #'mu4e-extras-msg-is-personal-and-plain-text-p
		 `((user-mail-address . ,(getenv "PERSONAL_GMAIL"))
		   (smtpmail-smtp-user . ,(getenv "PERSONAL_GMAIL"))
		   (message-send-mail-function . smtpmail-send-it)
		   (org-msg-signature . ,org-msg-extras-personal-plain-text-signature))))
	  (mapcan #'mu4e-extras--gmail-api-contexts mu4e-extras-gmail-api-accounts)
	  (list (mu4e-extras--make-context
		 "Tlon" t #'mu4e-extras-msg-is-tlon-and-html-p
		 `((user-mail-address . ,(getenv "WORK_EMAIL"))
		   (smtpmail-smtp-user . ,(getenv "PERSONAL_GMAIL"))
		   (message-send-mail-function . smtpmail-send-it)
		   (org-msg-signature . ,org-msg-extras-tlon-html-signature)))
		(mu4e-extras--make-context
		 "Tlon" nil #'mu4e-extras-msg-is-tlon-and-plain-text-p
		 `((user-mail-address . ,(getenv "WORK_EMAIL"))
		   (smtpmail-smtp-user . ,(getenv "PERSONAL_GMAIL"))
		   (message-send-mail-function . smtpmail-send-it)
		   (org-msg-signature . ,org-msg-extras-tlon-plain-text-signature))))))))

(defun mu4e-extras--gmail-api-contexts (account)
  "Return the HTML and plain-text `mu4e' contexts for Gmail API ACCOUNT.
ACCOUNT is an element of `mu4e-extras-gmail-api-accounts'."
  (let ((address (mu4e-extras-account-address account))
	(maildir (plist-get account :maildir)))
    (mapcar (lambda (html)
	      (mu4e-extras--make-context
	       (plist-get account :label) html
	       (lambda (msg) (mu4e-extras-msg-matches-account-p msg account html))
	       `((user-mail-address . ,address)
		 (smtpmail-smtp-user . ,address)
		 (mu4e-sent-folder . ,(concat maildir "Sent"))
		 (mu4e-drafts-folder . ,(concat maildir "Drafts"))
		 (message-send-mail-function . mu4e-extras-send-via-gmail-api)
		 (org-msg-signature
		  . ,(symbol-value (plist-get account (if html
							   :html-signature
							 :plain-text-signature)))))))
	    '(t nil))))

(defun mu4e-extras--make-context (label html match-func vars)
  "Return a `mu4e' context for LABEL in HTML or plain-text format.
If HTML is non-nil, name the context \"LABEL HTML\"; otherwise, name it
\"LABEL plain text\" and disable `org-msg-mode' while it is active.  MATCH-FUNC
and VARS are the context's match function and variables."
  (if html
      (make-mu4e-context :name (concat label " HTML")
			 :match-func match-func
			 :vars vars)
    (make-mu4e-context :name (concat label " plain text")
		       :match-func match-func
		       :enter-func (lambda () (org-msg-mode -1))
		       :leave-func #'org-msg-mode
		       :vars vars)))

(defun mu4e-extras--number-contexts (contexts)
  "Prefix the name of each of CONTEXTS with its position, starting at 1.
Return CONTEXTS."
  (seq-do-indexed (lambda (context index)
		    (setf (mu4e-context-name context)
			  (format "%d %s" (1+ index) (mu4e-context-name context))))
		  contexts)
  contexts)

;;;;; Account detection

(defun mu4e-extras-msg-account (msg)
  "Return the element of `mu4e-extras-gmail-api-accounts' owning MSG, or nil.
An account owns MSG when MSG's maildir starts with the account's `:maildir'."
  (when-let* ((maildir (and msg (mu4e-message-field msg :maildir))))
    (cl-find-if (lambda (account)
		  (string-prefix-p (plist-get account :maildir) maildir))
		mu4e-extras-gmail-api-accounts)))

(defun mu4e-extras-account-address (account)
  "Return the email address of Gmail API ACCOUNT, or nil if unknown."
  (or (plist-get account :address)
      (when-let* ((variable (plist-get account :address-env)))
	(getenv variable))))

(defun mu4e-extras-account-for-address (address)
  "Return the Gmail API account whose email address is ADDRESS, or nil."
  (when address
    (cl-find-if (lambda (account)
		  (when-let* ((account-address (mu4e-extras-account-address account)))
		    (string-equal-ignore-case account-address address)))
		mu4e-extras-gmail-api-accounts)))

;;;;; Account folders

(defun mu4e-extras-refile-folder (msg)
  "Return the refile folder for MSG."
  (mu4e-extras--account-folder msg "Refiled"))

(defun mu4e-extras-trash-folder (msg)
  "Return the trash folder for MSG."
  (mu4e-extras--account-folder msg "Trash"))

(defun mu4e-extras--account-folder (msg folder)
  "Return FOLDER inside the maildir of the account owning MSG.
For a message of a Gmail API account, FOLDER is inside the account's
`:maildir'; otherwise, it is at the top level of the maildir."
  (concat (if-let* ((account (mu4e-extras-msg-account msg)))
	      (plist-get account :maildir)
	    "/")
	  folder))

(defun mu4e-extras-set-account-folders ()
  "Set folder variables for multi-account support."
  (setq mu4e-refile-folder #'mu4e-extras-refile-folder
	mu4e-trash-folder #'mu4e-extras-trash-folder))

;;;;; Context matching

;; A message stored in a Gmail API account's maildir belongs to that account,
;; whoever it is addressed to: this covers messages addressed to several of the
;; user's accounts, Bcc and group deliveries, and the user's own sent messages.
;; Recipient headers decide only for the remaining (`mbsync') mail.

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

(defun mu4e-extras-msg-matches-account-p (msg account html)
  "Return non-nil iff MSG matches the context of Gmail API ACCOUNT.
If HTML is non-nil, MSG must be an HTML message; otherwise, it must be a plain
text message."
  (when msg
    (and (mu4e-extras-msg-is-account-p msg account)
	 (eq (and (mu4e-extras-msg-is-html-p msg) t) (and html t)))))

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
  "Return t iff MSG is a personal message.
Messages of a Gmail API account are never personal."
  (and (not (mu4e-extras-msg-account msg))
       (or (mu4e-message-contact-field-matches msg :to (getenv "PERSONAL_GMAIL"))
	   (mu4e-message-contact-field-matches msg :to (getenv "PERSONAL_EMAIL")))))

(defun mu4e-extras-msg-is-account-p (msg account)
  "Return non-nil iff MSG belongs to Gmail API ACCOUNT.
If a Gmail API account owns MSG's maildir, MSG belongs to that account only.
Otherwise, MSG belongs to ACCOUNT if it is addressed to ACCOUNT's address."
  (if-let* ((owner (mu4e-extras-msg-account msg)))
      (equal (plist-get owner :name) (plist-get account :name))
    (when-let* ((address (mu4e-extras-account-address account)))
      (mu4e-message-contact-field-matches msg :to address))))

(defun mu4e-extras-msg-is-tlon-p (msg)
  "Return t iff MSG is a Tlon message.
Messages of a Gmail API account are never Tlon messages."
  (and (not (mu4e-extras-msg-account msg))
       (or (mu4e-message-contact-field-matches msg :to (getenv "WORK_EMAIL"))
	   (mu4e-message-contact-field-matches msg :reply-to "tlon-team@googlegroups.com"))))

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
away (in which case it can take its time).  Ideally we would do this
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
string.

While prompting show the URL indicators."
  (let ((count (hash-table-count mu4e--view-link-map))
        (hidden (and (listp buffer-invisibility-spec)
                     (memq 'mu4e-url-indicator buffer-invisibility-spec))))
    (when (zerop count) (mu4e-error "No links for this message"))
    (unwind-protect
        (progn
          (mu4e--view-url-indicator-display t)
          (if (not multi)
              (if (= count 1)
                  (el-patch-swap (read-number (mu4e-format "%s: " prompt) 1) 1)
                (read-number (mu4e-format "%s (1-%d): " prompt count)))
            (let ((def (if (= count 1) "1" (format "1-%d" count))))
              (read-string (mu4e-format "%s (default %s): " prompt def)
                           nil nil def))))
      (mu4e--view-url-indicator-display (not hidden)))))

(provide 'mu4e-extras)
;;; mu4e-extras.el ends here
