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
(defun slack-extras-epoch-capture ()
  "Capture Slack message at point to the Epoch inbox."
  (interactive)
  (org-capture nil "S"))

;;;;; Messages

;;;###autoload
(defun slack-extras-yank-code-block ()
  "Yank the kill ring as a Slack code block in the current compose buffer."
  (interactive)
  (insert "```\n")
  (yank)
  (unless (bolp) (insert "\n"))
  (insert "```"))

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
    ("e" "edit" slack-message-edit)
    ("D" "delete" slack-message-delete)
    ("z" "compose in buffer" slack-message-write-another-buffer)
    ("s" "share" slack-message-share)
    ("l" "copy link" slack-message-copy-link)
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
