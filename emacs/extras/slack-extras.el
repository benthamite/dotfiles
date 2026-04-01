;;; slack-extras.el --- Extensions for slack -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/slack-extras.el
;; Version: 0.1
;; Package-Requires: ((slack "0.0.2"))

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

;; Extensions for `slack'.

;;; Code:

(require 'ol)
(require 'slack)
(require 'transient)

;;;; Functions

;;;;; Link storage

(declare-function ol/slack-store-link "ol-emacs-slack")
(declare-function ol/slack-format-link "ol-emacs-slack")

(defun slack-extras--store-link ()
  "Store an org link to the Slack message at point.
Extends `ol/slack-store-link' with support for
`slack-activity-feed-buffer-mode'."
  (cond
   ((derived-mode-p 'slack-message-buffer-mode 'slack-thread-message-buffer-mode)
    (ol/slack-store-link))
   ((derived-mode-p 'slack-activity-feed-buffer-mode)
    (let* ((ts (get-text-property (point) 'ts))
           (room-id (get-text-property (point) 'room-id))
           (team (slack-buffer-team slack-current-buffer))
           (room (and room-id (slack-room-find room-id team))))
      (when (and ts room)
        (let* ((room-name (slack-room-name room team))
               (formatted-ts (get-text-property (point) 'lui-formatted-time-stamp))
               (link (ol/slack-format-link team room ts))
               (description (concat
                             "Slack message in #" room-name
                             (if formatted-ts (format " at %s" formatted-ts) ""))))
          (org-link-store-props
           :type "emacs-slack"
           :link (concat "emacs-slack:" link)
           :description description)))))))

(org-link-set-parameters "emacs-slack" :store #'slack-extras--store-link)

;;;;; Connection

;;;###autoload
(defun slack-extras-start-and-select ()
  "Start Slack if not connected, then select a channel.
If all registered teams are already connected, skip straight to channel
selection."
  (interactive)
  (let ((disconnected
         (let (teams)
           (maphash (lambda (_token team)
                      (unless (oref team ws)
                        (push team teams)))
                    slack-teams-by-token)
           teams)))
    (if disconnected
        (progn
          (slack-start)
          (message "Connecting to Slack... use `slack-channel-select' once ready."))
      (call-interactively #'slack-channel-select))))

;;;###autoload
(defun slack-extras-disconnect-all ()
  "Disconnect all connected Slack teams."
  (interactive)
  (maphash (lambda (_token team)
             (when (oref team ws)
               (slack-team-disconnect team)))
           slack-teams-by-token)
  (message "Disconnected all Slack teams."))

;;;;; Capture

;;;###autoload
(defun slack-extras-personal-capture ()
  "Capture Slack message at point to the personal inbox."
  (interactive)
  (org-capture nil "s"))

;;;###autoload
(defun slack-extras-work-capture ()
  "Capture Slack message at point to the work inbox."
  (interactive)
  (org-capture nil "S"))

;;;;; Messages

(declare-function slack-thread-message-buffer-p "slack-thread-message-buffer")
(declare-function slack-buffer-display-message-compose-buffer "slack-buffer")
(declare-function slack-room-find-message "slack-room")
(declare-function slack-thread-ts "slack-message")
(declare-function slack-thread-show-messages "slack-message-buffer")
(declare-function slack-buffer-start-thread "slack-message-buffer")
(declare-function slack-reply-broadcast-message-p "slack-message")

;;;###autoload
(defun slack-extras-thread-reply ()
  "Open a thread for the message at point and start composing a reply.
If the message already has a thread, open it; otherwise start a
new one.  In both cases, immediately open a compose buffer in the
thread."
  (interactive)
  (slack-if-let* ((buf slack-current-buffer))
      (if (slack-thread-message-buffer-p buf)
          (slack-buffer-display-message-compose-buffer buf)
        (slack-extras--open-thread-and-compose buf (slack-get-ts)))))

(defun slack-extras--open-thread-and-compose (buf ts)
  "Open a thread for TS in BUF, then open a compose buffer.
BUF is a `slack-message-buffer'."
  (slack-if-let* ((team (slack-buffer-team buf))
                  (room (slack-buffer-room buf))
                  (message (slack-room-find-message room ts)))
      (slack-if-let* ((thread-ts (slack-thread-ts message)))
          (slack-thread-show-messages
           message room team #'slack-extras--compose-in-current-buffer)
        (slack-extras--start-thread-and-compose buf message ts))))

(defun slack-extras--start-thread-and-compose (buf message ts)
  "Start a new thread for MESSAGE at TS in BUF, then compose.
BUF is a `slack-message-buffer'."
  (when (slack-reply-broadcast-message-p message)
    (error "Can't start thread from broadcasted message"))
  (let ((thread-buf (slack-create-thread-message-buffer
                     (slack-buffer-room buf)
                     (slack-buffer-team buf)
                     ts)))
    (slack-buffer-display thread-buf)
    (slack-buffer-display-message-compose-buffer thread-buf)))

(defun slack-extras--compose-in-current-buffer ()
  "Open a compose buffer for `slack-current-buffer'."
  (slack-buffer-display-message-compose-buffer slack-current-buffer))

;;;###autoload
(defun slack-extras-yank-code-block ()
  "Yank the kill ring as a Slack code block in the current compose buffer."
  (interactive)
  (insert "```\n")
  (yank)
  (unless (bolp) (insert "\n"))
  (insert "```"))

;;;;; Tab-bar notifications

(defvar tab-bar-extras-slack-notifications-enabled t
  "Whether the Slack element displays notifications in the Tab Bar.")

(defvar slack-has-unreads)
(defvar slack-unread-count)
(defvar slack-teams-by-token)
(declare-function slack-counts-summary "slack-counts")
(declare-function doom-modeline-icon "doom-modeline-core")
(declare-function doom-modeline-vspc "doom-modeline-core")

(defun slack-extras-actionable-unreads-p ()
  "Return non-nil when there are Slack notifications worth attention.
DMs, group DMs, and thread replies are actionable when unread.
Channels are only actionable when they have @mentions."
  (catch 'found
    (maphash
     (lambda (_token team)
       (when-let ((counts (and (slot-boundp team 'counts) (oref team counts))))
         (dolist (e (slack-counts-summary counts))
           (let ((type (car e))
                 (has-unreads (cadr e))
                 (mentions (cddr e)))
             (when (pcase type
                     ((or 'im 'mpim 'thread) has-unreads)
                     ('channel (> mentions 0)))
               (throw 'found t))))))
     slack-teams-by-token)
    nil))

(defconst tab-bar-extras-slack-element
  '(:eval (when (and tab-bar-extras-slack-notifications-enabled
                     (bound-and-true-p slack-teams-by-token)
                     (slack-extras-actionable-unreads-p))
            (let ((count (or (bound-and-true-p slack-unread-count) 0)))
              (concat
               " | "
               (doom-modeline-icon 'mdicon "nf-md-slack" "💬" "S"
                                   :face 'doom-modeline-notification)
               (doom-modeline-vspc)
               (propertize
                (if (> count 0)
                    (if (> count 99) "99+" (number-to-string count))
                  "•")
                'face '(:inherit
                        (doom-modeline-unread-number
                         doom-modeline-notification)))))))
  "Tab-bar element showing Slack unread/mention count.")

(declare-function tab-bar-extras-toggle-individual-notifications
                  "tab-bar-extras")
;;;###autoload
(defun tab-bar-extras-toggle-slack-notifications (&optional action)
  "Toggle Slack notifications in the Tab Bar.
If ACTION is `enable', enable notifications.  If ACTION is
`disable', disable them."
  (tab-bar-extras-toggle-individual-notifications
   'tab-bar-extras-slack-notifications-enabled action))

(defun slack-extras--also-toggle-slack (fn &optional action)
  "Around advice for `tab-bar-extras-toggle-notifications'.
Call FN with ACTION, then also toggle Slack notifications."
  (funcall fn action)
  (tab-bar-extras-toggle-slack-notifications action))

(with-eval-after-load 'tab-bar-extras
  (advice-add 'tab-bar-extras-toggle-notifications
              :around #'slack-extras--also-toggle-slack))

;;;;; Menu

;;;###autoload (autoload 'slack-extras-menu "slack-extras" nil t)
(transient-define-prefix slack-extras-menu ()
  "`slack-extras' menu."
  [["Navigate"
    ("c" "channel" slack-channel-select)
    ("g" "group" slack-group-select)
    ("m" "direct message" slack-im-select)
    ("u" "rooms" slack-select-rooms)
    ("U" "unread rooms" slack-select-unread-rooms)
    ("a" "all threads" slack-all-threads)]
   ["Message"
    ("d" "thread" slack-thread-show-or-create)
    ("Z" "thread reply" slack-extras-thread-reply)
    ("e" "edit" slack-message-edit)
    ("D" "delete" slack-message-delete)
    ("z" "compose in buffer" slack-message-write-another-buffer)
    ("s" "share" slack-message-share)
    ("l" "copy link" slack-message-copy-link)
    ("I" "copy id" slack-message-copy-id)
    ("Q" "quote & reply" slack-quote-and-reply)]
   ["React & pin"
    ("r" "add reaction" slack-message-add-reaction)
    ("R" "remove reaction" slack-message-remove-reaction)
    ("p" "pin" slack-message-pins-add)
    ("P" "unpin" slack-message-pins-remove)
    ("n" "pins list" slack-room-pins-list)]
   ["File"
    ("f" "upload" slack-file-upload)
    ("F" "quick upload" slack-file-upload-quick)
    ("S" "upload snippet" slack-file-upload-snippet)
    ("x" "download at point" slack-download-file-at-point)]]
  [["Search"
    ("/" "messages" slack-search-from-messages)
    ("?" "files" slack-search-from-files)]
   ["Channel"
    ("C" "create" slack-create-channel)
    ("j" "join" slack-channel-join)
    ("v" "leave" slack-channel-leave)
    ("t" "set topic" slack-channel-set-topic)
    ("i" "info" slack-show-channel-info)
    ("b" "bookmarks" slack-show-channel-bookmarks)]
   ["Team & connection"
    ("T" "change team" slack-change-current-team)
    ("o" "start" slack-extras-start-and-select)
    ("O" "stop" slack-extras-disconnect-all)
    ("+" "set status" slack-user-set-status)
    ("-" "reset status" slack-user-reset-status)]
   ["Misc"
    ("*" "stars" slack-stars-list)
    ("A" "activity feed" slack-activity-feed-show)
    ("M" "scheduled messages" slack-scheduled-messages-show)
    ("K" "kill all buffers" slack-kill-all-buffers)
    ("w" "open in browser" slack-jump-to-browser)
    ("y" "yank code block" slack-extras-yank-code-block)]])

(provide 'slack-extras)
;;; slack-extras.el ends here
