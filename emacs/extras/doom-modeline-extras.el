;;; doom-modeline-extras.el --- Extensions for doom-modeline -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/doom-modeline-extras.el
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

;; Extensions for `doom-modeline'.

;;; Code:

(require 'doom-modeline)
(require 'el-patch)
(require 'forge)
(require 'gh-notify-extras)

;;;; Functions

;;;;; Patched functions

;; Patch the two functions below to remove the `doom-modeline-github`
;; requirement. I show the notifications in the tab-bar, so I don't want them to
;; appear in the modeline as well

(el-patch-defun doom-modeline--github-fetch-notifications ()
  "Fetch GitHub notifications."
  (when (and (el-patch-swap doom-modeline-github t)
	     (require 'async nil t))
    (async-start
     `(lambda ()
	,(async-inject-variables
	  "\\`\\(load-path\\|auth-sources\\|doom-modeline-before-github-fetch-notification-hook\\)\\'")
	(run-hooks 'doom-modeline-before-github-fetch-notification-hook)
	(when (require 'ghub nil t)
	  (with-timeout (10)
	    (ignore-errors
	      (when-let* ((username (ghub--username ghub-default-host))
			  (token (or (ghub--token ghub-default-host username 'forge t)
				     (ghub--token ghub-default-host username 'ghub t))))
		(ghub-get "/notifications"
			  '((notifications . t))
			  :host ghub-default-host
			  :username username
			  :auth token
			  :unpaginate t
			  :noerror t))))))
     (lambda (result)
       (message "")                     ; suppress message
       (setq doom-modeline--github-notification-number (length result))
       (run-hooks 'doom-modeline-after-github-fetch-notification-hook)))))

(el-patch-defun doom-modeline-github-timer ()
  "Start/Stop the timer for GitHub fetching."
  (if (timerp doom-modeline--github-timer)
      (cancel-timer doom-modeline--github-timer))
  (setq doom-modeline--github-timer
	(and (el-patch-swap doom-modeline-github t)
	     (run-with-idle-timer 30
				  doom-modeline-github-interval
				  #'doom-modeline--github-fetch-notifications))))

;;;;; notification counter Forge sync

(defvar doom-modeline-extras-github-notification-last-count nil
  "Notification count when the modeline notification counter was last updated.")

(defun doom-modeline-extras-trigger-forge-update ()
  "Pull notifications in Forge when the modeline notification counter is updated.
Also refresh the the `gh-notify' buffer."
  (unless (eq doom-modeline--github-notification-number
	      (length doom-modeline-extras-github-notification-last-count))
    (setq prev-result doom-modeline--github-notification-number)
    (forge-pull-notifications)
    (gh-notify-extras-refresh-in-background)))

(add-hook 'doom-modeline-after-github-fetch-notification-hook
	  #'doom-modeline-extras-trigger-forge-update)

;;;;; vars

;; TODO: move this to `config.org' if you determine it works correctly

(setq doom-modeline-github t) ; we display the counter in the tab-bar; see `doom-modeline-extras.el'
(setq doom-modeline-github-interval (* 1 60))
(setq doom-modeline-github nil)

(provide 'doom-modeline-extras)
;;; doom-modeline-extras.el ends here
