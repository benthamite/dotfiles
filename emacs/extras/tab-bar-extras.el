;;; tab-bar-extras.el --- Extra functionality for the tab bar -*- lexical-binding: t -*-

;; Copyright (C) 2023

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

(require 'calendar-extras)
(require 'display-wttr)

;;;; Variables

;;;; User options

(defgroup tab-bar-extras ()
  "Extensions for `tab-bar'."
  :group 'tab-bar)

(defcustom tab-bar-extras-global-mode-string nil
  "Default value of `global-mode-string' when `tab-bar-extras-mode' is active."
  :type 'alist
  :group 'tab-bar-extras)

(defcustom tab-bar-extras-reset-wttr t
  "Whether to reset weather information when `tab-bar-extras-reset' is invoked."
  :type 'boolean
  :group 'tab-bar-extras)

;;;;; elements

(defcustom tab-bar-extras-prefix-element
  " "
  "Element to display at the beginning of the tab-bar."
  :type 'sexp
  :group 'tab-bar-extras)

(defcustom tab-bar-extras-date-element
  `(:eval (propertize display-time-string 'face 'faces-extras-display-time))
  "Element to display the date."
  :type 'sexp
  :group 'tab-bar-extras)

(defcustom tab-bar-extras-chemacs-element
  `(" " chemacs-profile-name)
  "Element to display the Chemacs profile."
  :type 'sexp
  :group 'tab-bar-extras)

(defcustom tab-bar-extras-battery-element
  `("" fancy-battery-mode-line)
  "Element to display the battery."
  :type 'sexp
  :group 'tab-bar-extras)

(defcustom tab-bar-extras-telega-element
  `(:eval (when (and
		 (telega-server-live-p)
		 (> (plist-get telega--unread-message-count :unread_count) 0))
	    (concat " | " telega-mode-line-string)))
  "Element to display Telega notificaations."
  :type 'sexp
  :group 'tab-bar-extras)

(defcustom tab-bar-extras-github-element
  `(:eval (when (> doom-modeline--github-notification-number 0)
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
  "Element to display Github notifications."
  :type 'sexp
  :group 'tab-bar-extras)

(defcustom tab-bar-extras-forge-element
  `(:eval (when (> (forge-extras-get-unread-notifications) 0)
	    (concat
	     " | "
	     (doom-modeline-icon 'octicon "nf-oct-mark_github" "🔔" "&"
				 :face 'doom-modeline-notification)
	     (doom-modeline-vspc)
	     (propertize
	      (cond
	       ((> (forge-extras-get-unread-notifications) 99) "99+")
	       (t (number-to-string (forge-extras-get-unread-notifications))))
	      'face '(:inherit
		      (doom-modeline-unread-number doom-modeline-notification))))))
  "Element to display Forge notification count."
  :type 'sexp
  :group 'tab-bar-extras)

(defcustom tab-bar-extras-pomodoro-element
  `(:eval (unless (memq 'org-pomodoro-mode-line global-mode-string)
	    (setq global-mode-string (append global-mode-string
					     '(org-pomodoro-mode-line)))))
  "Element to display Pomodoro information."
  :type 'sexp
  :group 'tab-bar-extras)

(defcustom tab-bar-extras-separator-element
  " | "
  "Element to separate the tab-bar elements.
Note that elements that the separator is already part of elements that do not
always show (like Github or Telega notifications), because otherwise the
separator would remain even when the elements are absent."
  :type 'sexp
  :group 'tab-bar-extras)

;;;; Functions

(defun tab-bar-extras-reset (&optional quick)
  "Reset the tab bar.
This resets the clock, refreshes the tab-bar and its color, and updates the
geolocation and weather information. If QUICK is non-nil, run only the essential
reset functions."
  (interactive)
  (display-time)
  (setq global-mode-string tab-bar-extras-global-mode-string)
  (unless quick
    (when calendar-extras-use-geolocation
      (calendar-extras-set-location-variables-from-ip))
    (setq display-wttr-locations `(,calendar-extras-location-name)))
  (when tab-bar-extras-reset-wttr
    (display-wttr)))

(defun tab-bar-extras-quick-reset ()
  "Reset the tab bar quickly."
  (interactive)
  (tab-bar-extras-reset t))

(defvar org-clock-current-task)
(defun tab-bar-extras-reset-unless-clock ()
  "Reset the tab-bar when `org-clock' isn't running.
The condition is included to prevent the currently clocked task from
disappearing when the tab-bar is reset."
  (require 'org-clock)
  (unless org-clock-current-task
    (tab-bar-extras-reset)))

(provide 'tab-bar-extras)
;;; tab-bar-extras.el ends here
