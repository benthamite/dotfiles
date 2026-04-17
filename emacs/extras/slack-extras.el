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

;;;;; Tab-bar notifications

(defvar tab-bar-extras-slack-notifications-enabled t
  "Whether the Slack element displays notifications in the Tab Bar.")

(defvar slack-has-unreads)
(defvar slack-unread-count)
(defvar slack-teams-by-token)
(defvar tab-bar-extras-global-mode-string)
(defvar slack-activity-last-refresh-time)
(defvar slack-activity-refresh-debounce)
(declare-function slack-activity-feed-refresh-unread-summary
                  "slack-activity-feed-buffer")
(declare-function doom-modeline-icon "doom-modeline-core")
(declare-function doom-modeline-vspc "doom-modeline-core")

(defun slack-extras-actionable-unreads-p ()
  "Return non-nil when Slack Activity has unseen notifications.
Also triggers a debounced Activity feed refresh so the value
stays current with the Slack UI."
  (slack-extras--maybe-refresh-activity)
  slack-has-unreads)

(defun slack-extras--maybe-refresh-activity ()
  "Refresh Activity feed state if stale.
Calls `slack-activity-feed-refresh-unread-summary' when more than
`slack-activity-refresh-debounce' seconds have elapsed since the
last refresh.  The Activity feed API works over HTTP and does not
require an active WebSocket."
  (when (and (bound-and-true-p slack-activity-refresh-debounce)
             (not (hash-table-empty-p slack-teams-by-token))
             (> (- (float-time) (or slack-activity-last-refresh-time 0))
                slack-activity-refresh-debounce))
    (setq slack-activity-last-refresh-time (float-time))
    (slack-activity-feed-refresh-unread-summary)))

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

(declare-function tab-bar-extras-set-global-mode-string "tab-bar-extras")
(defun slack-extras--register-tab-bar-element ()
  "Append the Slack element to `tab-bar-extras-global-mode-string'.
The symbol is appended rather than its value so that `memq' can
find it across reloads (interned symbols are always `eq').  Also
syncs `global-mode-string', which is what the tab bar actually
displays, so the element is visible even when registration
happens after the initial mode-string sync."
  (unless (memq 'tab-bar-extras-slack-element
                tab-bar-extras-global-mode-string)
    (setq tab-bar-extras-global-mode-string
          (append tab-bar-extras-global-mode-string
                  '(tab-bar-extras-slack-element))))
  (when (fboundp 'tab-bar-extras-set-global-mode-string)
    (tab-bar-extras-set-global-mode-string)))

(with-eval-after-load 'tab-bar-extras
  (advice-add 'tab-bar-extras-toggle-notifications
              :around #'slack-extras--also-toggle-slack)
  (slack-extras--register-tab-bar-element))

(provide 'slack-extras)
;;; slack-extras.el ends here
