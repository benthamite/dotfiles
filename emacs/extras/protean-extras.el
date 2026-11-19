;;; protean-extras.el --- Open Protean's messages in their own clients -*- lexical-binding: t -*-

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
;; its own, so that mu4e's own view is left alone; Slack, Signal and
;; Telegram open in emacs-slack, sgn and telega.  WhatsApp has no client
;; here yet, so Protean shows its own view of those messages.

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
(defvar slack-teams-by-token)
(defvar mu4e--view-message)

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
    protean-extras-open-signal protean-extras-open-telegram)
  "The openers of this file, for `protean-message-open-functions'.")

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
    (require 'slack)
    (let* ((account (plist-get message :account))
           (name (or (alist-get account protean-extras-slack-teams nil nil #'equal)
                     (error "No Slack team is set for the account %s" account)))
           (team (or (protean-extras--slack-team name)
                     (error "Slack's team %s is not registered" name)))
           (parts (split-string (or (plist-get message :ref) "") "/"))
           (ts (or (nth 1 parts) (error "Not a Slack message: %s" (plist-get message :ref))))
           (room (or (slack-room-find (car parts) team)
                     (error "Slack has no room %s" (car parts))))
           (thread (plist-get message :thread))
           (found (lambda () (slack-buffer-goto ts) (funcall done (current-buffer))))
           (failed (lambda (&rest _) (funcall done nil "Slack could not load the message"))))
      (unless (slack-team-connectedp team)
        (error "Slack's team %s is not connected" name))
      (if (and thread (not (equal thread ts)))
          (slack-open-message--open-thread room thread team found failed)
        (slack-open-message--open-channel ts room team found failed)))
    t))

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
      (funcall done (sgn-message-buffer chat sender timestamp)))
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
        (telega-chatbuf--goto-msg message-id nil (lambda () (funcall done buffer)))))
    t))

(provide 'protean-extras)
;;; protean-extras.el ends here
