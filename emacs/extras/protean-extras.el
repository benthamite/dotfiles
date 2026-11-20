;;; protean-extras.el --- Open and answer Protean's messages in their own clients -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/protean-extras.el
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))

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

;; Openers for Protean's inbox preview, one per channel, for
;; `protean-message-open-functions': each shows a message the way its
;; client shows it, in a buffer handed to Protean, without displaying
;; anything itself.  Email is rendered as mu4e renders it, in a buffer of
;; its own, so that mu4e's own view is left alone; Slack, Signal,
;; Telegram and WhatsApp open in emacs-slack, sgn, telega and wsp.
;;
;; Stagers, one per channel, for `protean-message-stage-functions':
;; each puts a task's draft in the client as a reply to its message,
;; for the user to send: a reply in mu4e, a compose buffer of
;; emacs-slack (the thread's, for a message in one), and the input of
;; sgn, telega and wsp, replying to the message.  None sends anything,
;; and none overwrites text already there.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)

(declare-function mu4e-view-mode "mu4e-view")
(declare-function mu4e-message-readable-path "mu4e-message")
(declare-function mu4e--view-render-buffer "mu4e-view")
(declare-function slack-room-find "slack-room")
(declare-function slack-team-connectedp "slack-team")
(declare-function slack-buffer-goto "slack-buffer")
(declare-function slack-open-message--open-thread "slack-message-buffer")
(declare-function slack-open-message--open-channel "slack-message-buffer")
(declare-function sgn-parse-message-id "sgn-chat")
(declare-function sgn-message-buffer "sgn-chat")
(declare-function telega-server-live-p "telega-server")
(declare-function telega-chat-get "telega-chat")
(declare-function telega-chatbuf--get-create "telega-chat")
(declare-function telega-chatbuf--goto-msg "telega-chat")
(declare-function telega-msg-at "telega-msg")
(declare-function telega-msg-reply "telega-chat")
(declare-function wsp-message-buffer "wsp")
(declare-function wsp-chat-reply "wsp-actions")
(declare-function wsp-chat--input-text "wsp-chat")
(declare-function wsp-chat--set-input "wsp-chat")
(declare-function sgn-reply "sgn-actions")
(declare-function sgn-chat--input-text "sgn-chat")
(declare-function sgn-chat--set-input "sgn-chat")
(declare-function mu4e-compose-reply "mu4e-compose")
(declare-function message-goto-body "message")
(declare-function slack-create-thread-message-compose-buffer "slack-thread-message-compose-buffer")
(declare-function slack-create-room-message-compose-buffer "slack-room-message-compose-buffer")
(declare-function slack-buffer-buffer "slack-buffer")
(declare-function protean-message-property-bounds "protean-preview")
(defvar slack-teams-by-token)
(defvar mu4e--view-message)
(defvar telega-chatbuf--input-marker)
(defvar wsp-send-typing)

(defgroup protean-extras ()
  "Open Protean's messages in their own clients."
  :group 'protean)

(defcustom protean-extras-slack-teams nil
  "The emacs-slack team of each of Protean's Slack accounts.
An alist of (ACCOUNT . NAME): ACCOUNT as Protean records it, such as
\"epoch\", and NAME the team's name in `slack-teams'."
  :type '(alist :key-type string :value-type string)
  :group 'protean-extras)

(defconst protean-extras-mail-buffer-name "*Protean mail*"
  "The buffer that shows an email for the preview.")

(defconst protean-extras-openers
  '(protean-extras-open-email protean-extras-open-slack
    protean-extras-open-signal protean-extras-open-telegram
    protean-extras-open-whatsapp)
  "The openers of this file, for `protean-message-open-functions'.")

(defconst protean-extras-stagers
  '(protean-extras-stage-email protean-extras-stage-slack
    protean-extras-stage-signal protean-extras-stage-telegram
    protean-extras-stage-whatsapp)
  "The stagers of this file, for `protean-message-stage-functions'.")

(defun protean-extras--channel-p (message channel)
  "Return non-nil when MESSAGE, a plist from Protean, is of CHANNEL."
  (equal (plist-get message :channel) channel))

;;;; Email

(defun protean-extras-open-email (message done)
  "Show the email MESSAGE as mu4e renders it, and give its buffer to DONE.
For `protean-message-open-functions'.  The message is found by its
Message-ID with mu, and rendered in `protean-extras-mail-buffer-name' by
mu4e's own renderer, so mu4e's view and headers are left as they are."
  (when (protean-extras--channel-p message "email")
    (let* ((id (string-trim (or (plist-get message :ref) "") "<" ">"))
           (msg (or (protean-extras--mu-message id)
                    (error "mu has no message with this Message-ID"))))
      (require 'mu4e)
      (let ((buffer (get-buffer-create protean-extras-mail-buffer-name)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (mu4e-view-mode)
            (insert-file-contents-literally (mu4e-message-readable-path msg) nil nil nil t)
            (setq-local mu4e--view-message msg)
            (mu4e--view-render-buffer msg))
          (goto-char (point-min)))
        (funcall done buffer)))
    t))

(defun protean-extras--mu-message (id)
  "Return mu's record of the message whose Message-ID is ID, or nil."
  (with-temp-buffer
    (when (and (not (string-empty-p id))
               (eql 0 (call-process "mu" nil t nil "find" (concat "msgid:" id)
                                    "--format=sexp")))
      (goto-char (point-min))
      (ignore-errors (read (current-buffer))))))

;;;; Slack

(defun protean-extras-open-slack (message done)
  "Open the Slack MESSAGE in emacs-slack, and give its room's buffer to DONE.
For `protean-message-open-functions'.  The team is the one
`protean-extras-slack-teams' names for the message's account; it must
be connected.  A reply in a thread opens the thread."
  (when (protean-extras--channel-p message "slack")
    (pcase-let* ((`(,team ,room ,ts ,thread) (protean-extras--slack-place message))
                 (found (lambda ()
                          (slack-buffer-goto ts)
                          (funcall done (current-buffer) nil
                                   (lambda () (protean-message-property-bounds 'ts ts)))))
                 (failed (lambda (&rest _) (funcall done nil "Slack could not load the message"))))
      (if (and thread (not (equal thread ts)))
          (slack-open-message--open-thread room thread team found failed)
        (slack-open-message--open-channel ts room team found failed)))
    t))

(defun protean-extras--slack-place (message)
  "Return where the Slack MESSAGE is, as (TEAM ROOM TS THREAD).
TEAM is the connected emacs-slack team of its account, ROOM its room,
TS its timestamp and THREAD its thread's, or nil.  Signal an error when
any of them cannot be found."
  (require 'slack)
  (let* ((account (plist-get message :account))
         (name (or (alist-get account protean-extras-slack-teams nil nil #'equal)
                   (error "No Slack team is set for the account %s" account)))
         (team (or (protean-extras--slack-team name)
                   (error "Slack's team %s is not registered" name)))
         (parts (split-string (or (plist-get message :ref) "") "/"))
         (ts (or (nth 1 parts) (error "Not a Slack message: %s" (plist-get message :ref))))
         (room (or (slack-room-find (car parts) team)
                   (error "Slack has no room %s" (car parts)))))
    (unless (slack-team-connectedp team)
      (error "Slack's team %s is not connected" name))
    (list team room ts (plist-get message :thread))))

(defun protean-extras--slack-team (name)
  "Return the registered emacs-slack team named NAME, or nil.
emacs-slack keeps its registered teams in `slack-teams-by-token'."
  (seq-find (lambda (team) (equal (eieio-oref team 'name) name))
            (and (boundp 'slack-teams-by-token)
                 (hash-table-values slack-teams-by-token))))

;;;; Signal

(defun protean-extras-open-signal (message done)
  "Give DONE sgn's chat buffer at the Signal MESSAGE.
For `protean-message-open-functions'."
  (when (protean-extras--channel-p message "signal")
    (require 'sgn)
    (pcase-let ((`(,chat ,sender ,timestamp)
                 (or (sgn-parse-message-id (or (plist-get message :ref) ""))
                     (error "Not a Signal message: %s" (plist-get message :ref)))))
      (let* ((buffer (sgn-message-buffer chat sender timestamp))
             (rowid (with-current-buffer buffer
                      (get-text-property (point) 'sgn-message-rowid))))
        (funcall done buffer nil
                 (and rowid (lambda ()
                              (protean-message-property-bounds 'sgn-message-rowid rowid))))))
    t))

;;;; Telegram

(defun protean-extras-open-telegram (message done)
  "Give DONE telega's chat buffer at the Telegram MESSAGE, once it is there.
For `protean-message-open-functions'.  Telegram must be running."
  (when (protean-extras--channel-p message "telegram")
    (require 'telega)
    (unless (telega-server-live-p)
      (error "Telegram is not running"))
    (pcase-let* ((`(,chat-id ,message-id)
                  (mapcar #'string-to-number (split-string (or (plist-get message :ref) "") "/")))
                 (chat (or (and chat-id (telega-chat-get chat-id 'offline))
                           (error "Telegram has no chat %s" chat-id)))
                 (buffer (telega-chatbuf--get-create chat :no-history)))
      (with-current-buffer buffer
        (telega-chatbuf--goto-msg message-id nil
                                  (lambda ()
                                    (funcall done buffer nil
                                             #'protean-extras--telegram-bounds)))))
    t))

(defun protean-extras--telegram-bounds ()
  "Return where telega's message at point is, as (BEG . END), or nil."
  (when-let* ((button (button-at (point))))
    (cons (button-start button) (button-end button))))

;;;; WhatsApp

(defun protean-extras-open-whatsapp (message done)
  "Give DONE wsp's chat buffer at the WhatsApp MESSAGE.
For `protean-message-open-functions'."
  (when (protean-extras--channel-p message "whatsapp")
    (require 'wsp)
    (let* ((buffer (condition-case err
                       (wsp-message-buffer (plist-get message :ref))
                     (wsp-message-not-found (error "%s" (cadr err)))))
           (id (with-current-buffer buffer (get-text-property (point) 'wsp-id))))
      (funcall done buffer nil
               (and id (lambda () (protean-message-property-bounds 'wsp-id id)))))
    t))

;;;; Stagers

(defun protean-extras--stage-via (opener message done reply)
  "Open MESSAGE with OPENER, then give DONE what REPLY returns.
For a stager of `protean-message-stage-functions'.  REPLY is called in
the client's buffer, point at the message, and returns the buffer in
which the reply waits; an error it signals is DONE's reason.  Return
what OPENER returns."
  (funcall opener message
           (lambda (buffer &optional reason _bounds)
             (if (not (buffer-live-p buffer))
                 (funcall done nil reason)
               (let ((staged (condition-case err
                                 (with-current-buffer buffer (funcall reply))
                               (error (funcall done nil (error-message-string err))
                                      nil))))
                 (when staged
                   (funcall done staged)))))))

(defun protean-extras--refuse-unless-empty (input)
  "Signal an error unless INPUT, the text of a reply box, is blank."
  (unless (string-blank-p input)
    (error "The reply box already has text; it was left as it is")))

(defun protean-extras-stage-email (message text done)
  "Give DONE a reply in mu4e to the email MESSAGE, TEXT above the quote.
For `protean-message-stage-functions'."
  (protean-extras--stage-via
   #'protean-extras-open-email message done
   (lambda ()
     (let ((draft (mu4e-compose-reply)))
       (unless (buffer-live-p draft)
         (error "mu4e did not start a reply"))
       (with-current-buffer draft
         (message-goto-body)
         (save-excursion (insert text "\n\n")))
       draft))))

(defun protean-extras-stage-slack (message text done)
  "Give DONE an emacs-slack compose buffer holding TEXT, a reply to MESSAGE.
For `protean-message-stage-functions'.  A message in a thread is
answered in the thread, any other in its room."
  (when (protean-extras--channel-p message "slack")
    (pcase-let* ((`(,team ,room ,_ts ,thread) (protean-extras--slack-place message))
                 (buffer (slack-buffer-buffer
                          (if thread
                              (slack-create-thread-message-compose-buffer room thread team)
                            (slack-create-room-message-compose-buffer room team)))))
      (with-current-buffer buffer
        (protean-extras--refuse-unless-empty (buffer-string))
        (insert text)
        (goto-char (point-min)))
      (funcall done buffer))
    t))

(defun protean-extras-stage-signal (message text done)
  "Give DONE sgn's chat buffer with TEXT as a reply to the Signal MESSAGE.
For `protean-message-stage-functions'.  No typing indicator is sent."
  (protean-extras--stage-via
   #'protean-extras-open-signal message done
   (lambda ()
     (protean-extras--refuse-unless-empty (sgn-chat--input-text))
     (sgn-reply)
     (sgn-chat--set-input text)
     (goto-char (point-max))
     (current-buffer))))

(defun protean-extras-stage-telegram (message text done)
  "Give DONE telega's chat buffer with TEXT as a reply to the Telegram MESSAGE.
For `protean-message-stage-functions'."
  (protean-extras--stage-via
   #'protean-extras-open-telegram message done
   (lambda ()
     (protean-extras--refuse-unless-empty
      (buffer-substring-no-properties telega-chatbuf--input-marker (point-max)))
     (telega-msg-reply (or (telega-msg-at (point))
                           (error "telega shows no message there")))
     (goto-char (point-max))
     (insert text)
     (current-buffer))))

(defun protean-extras-stage-whatsapp (message text done)
  "Give DONE wsp's chat buffer with TEXT as a reply to the WhatsApp MESSAGE.
For `protean-message-stage-functions'.  No typing indicator is sent."
  (protean-extras--stage-via
   #'protean-extras-open-whatsapp message done
   (lambda ()
     (protean-extras--refuse-unless-empty (wsp-chat--input-text))
     (wsp-chat-reply)
     (let ((wsp-send-typing nil))
       (wsp-chat--set-input text))
     (current-buffer))))

(provide 'protean-extras)
;;; protean-extras.el ends here
