;;; tab-bar-extras.el --- Extra functionality for the tab bar -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/tab-bar-extras.el
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

;; Extra functionality for the tab bar.

;;; Code:

;;;; User options

(defgroup tab-bar-extras ()
  "Extensions for the Tab Bar."
  :group 'tab-bar)

(defcustom tab-bar-extras-global-mode-string nil
  "Default value of `global-mode-string' when `tab-bar-extras-mode' is active."
  :type 'alist
  :group 'tab-bar-extras)

(defcustom tab-bar-extras-reset-wttr t
  "Whether to reset weather information when `tab-bar-extras-reset' is invoked."
  :type 'boolean
  :group 'tab-bar-extras)

;;;; Variables

(defvar tab-bar-extras-notifications-enabled t
  "Whether notifications are enabled in the Tab Bar.")

(defvar tab-bar-extras-telega-notifications-enabled t
  "Whether the Telega element actually displays notifications in the Tab Bar.")

(defvar tab-bar-extras-github-notifications-enabled t
  "Whether the GitHub element actually displays notifications in the Tab Bar.")

;;;;; elements

(defconst tab-bar-extras-prefix-element
  " "
  "Element to display at the beginning of the Tab Bar.")

(defconst tab-bar-extras-time-element
  `(:eval (propertize display-time-string 'face 'faces-extras-display-time))
  "Element to display the time.
To change how the time is displayed, customize `display-time-format'.")

(defconst tab-bar-extras-chemacs-element
  `(" " tlon-init-chemacs-profile-name)
  "Element to display the Chemacs profile.")

(defvar doom-modeline--battery-status)
(defconst tab-bar-extras-battery-element
  `(:eval ,(format "%s %s"
		   ;; icon
		   (car doom-modeline--battery-status)
		   ;;  percentage
		   (cdr doom-modeline--battery-status)))
  "Element to display the battery.")

(defconst tab-bar-extras-telega-element
  `(:eval (when (and
		 (bound-and-true-p tab-bar-extras-telega-notifications-enabled)
		 (and (fboundp 'telega-server-live-p) (telega-server-live-p))
		 (> (plist-get telega--unread-message-count :unread_count) 0))
	    (concat " | " telega-mode-line-string)))
  "Element to display Telega notifications.")

(defconst tab-bar-extras-github-element
  `(:eval (when (and tab-bar-extras-github-notifications-enabled
		     (> doom-modeline--github-notification-number 0))
	    (concat
	     " | "
	     (doom-modeline-icon 'octicon "nf-oct-mark_github" "🔔" "&"
				 :face 'doom-modeline-notification)
	     (doom-modeline-vspc)
	     (propertize
	      (cond
	       ((> doom-modeline--github-notification-number 99) "99+")
	       (t (number-to-string doom-modeline--github-notification-number)))
	      'face '(:inherit
		      (doom-modeline-unread-number doom-modeline-notification))))))
  "Element to display Forge notification count.
Note: for this element to work, `doom-modeline-github' must be non-nil.")

(defconst tab-bar-extras-pomodoro-element
  `(:eval (unless (memq 'org-pomodoro-mode-line global-mode-string)
	    (setq global-mode-string (append global-mode-string
					     '(org-pomodoro-mode-line)))))
  "Element to display Pomodoro information.")

(defconst tab-bar-extras-notification-status-element
  `(:eval (unless tab-bar-extras-notifications-enabled
	    (concat (propertize "🔕" 'face '(:height 0.8)) tab-bar-extras-separator-element)))
  "Element to display when the notifications are disabled.")

(defconst tab-bar-extras-separator-element
  " | "
  "Element to separate the Tab Bar elements.
Note that elements that the separator is already part of elements that do not
always show (like Github or Telega notifications), because otherwise the
separator would remain even when the elements are absent.")

;;;; Functions

(defvar calendar-extras-use-geolocation)
(defvar display-wttr-locations)
(defvar calendar-extras-location-name)
(declare-function calendar-extras-set-geolocation "calendar-extras")
(declare-function display-wttr "display-wttr")
(defun tab-bar-extras-reset (&optional quick)
  "Reset the tab bar.
This resets the clock, refreshes the Tab Bar and its color, and updates the
geolocation and weather information. If QUICK is non-nil, run only the essential
reset functions."
  (interactive)
  (tab-bar-extras-set-global-mode-string)
  (unless quick
    (when (featurep 'calendar-extras)
      (when calendar-extras-use-geolocation
	(calendar-extras-set-geolocation))
      (setq display-wttr-locations `(,calendar-extras-location-name))))
  (when (and tab-bar-extras-reset-wttr (bound-and-true-p display-wttr-mode))
    (display-wttr)))

(defun tab-bar-extras-quick-reset ()
  "Reset the tab bar quickly."
  (interactive)
  (tab-bar-extras-reset t))

(defun tab-bar-extras-set-global-mode-string ()
  "Set `global-mode-string' to `tab-bar-extras-global-mode-string'."
  (let ((inhibit-message t))
    (setq global-mode-string tab-bar-extras-global-mode-string)))

(defvar org-clock-current-task)
(defun tab-bar-extras-reset-unless-clock ()
  "Reset the Tab Bar when `org-clock' isn't running.
The condition is included to prevent the currently clocked task from
disappearing when the Tab Bar is reset."
  (require 'org-clock)
  (unless org-clock-current-task
    (tab-bar-extras-reset)))

;;;;; notifications

(defun tab-bar-extras-toggle-notifications (&optional action)
  "Toggle all notifications in the Tab Bar.
If ACTION is `enable', enable notifications. If ACTION is `disable', disable
them."
  (interactive)
  (dolist (fun '(tab-bar-extras-toggle-github-notifications
		 tab-bar-extras-toggle-telega-notifications))
    (funcall fun action))
  (setq tab-bar-extras-notifications-enabled
	(tab-bar-extras-get-state action 'tab-bar-extras-notifications-enabled)))

(defun tab-bar-extras-enable-all-notifications ()
  "Enable all notifications in the Tab Bar."
  (tab-bar-extras-toggle-notifications 'enable))

(defun tab-bar-extras-disable-all-notifications ()
  "Disable all notifications in the Tab Bar."
  (tab-bar-extras-toggle-notifications 'disable))

(defun tab-bar-extras-toggle-telega-notifications (&optional action)
  "Toggle Telega notifications in the Tab Bar.
If ACTION is `enable', enable notifications. If ACTION is `disable', disable
them."
  (tab-bar-extras-toggle-individual-notifications 'tab-bar-extras-telega-notifications-enabled action))

(defun tab-bar-extras-toggle-github-notifications (&optional action)
  "Toggle GitHub notifications in the Tab Bar.
If ACTION is `enable', enable notifications. If ACTION is `disable', disable
them."
  (tab-bar-extras-toggle-individual-notifications 'tab-bar-extras-github-notifications-enabled action))

(defun tab-bar-extras-toggle-individual-notifications (var &optional action)
  "Toggle notifications for VAR in the Tab Bar.
If ACTION is `enable', enable notifications. If ACTION is `disable', disable
them."
  (let ((state (tab-bar-extras-get-state action var)))
    (message "%s notifications."
	     (if (set var state) "Enabled" "Disabled"))))

(defun tab-bar-extras-get-state (action &optional var)
  "Get the state of the Tab Bar based on ACTION and VAR.
Return t or nil depending on whether ACTION is `enable' or `disable',
respectively. Otherwise, return t if the value of VAR is nil, and vice versa."
  (pcase action
    ('enable t)
    ('disable nil)
    (_ (not (symbol-value var)))))

(provide 'tab-bar-extras)
;;; tab-bar-extras.el ends here
