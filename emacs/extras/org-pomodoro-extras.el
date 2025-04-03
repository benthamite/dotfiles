;;; org-pomodoro-extras.el --- Extensions for org-pomodoro -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/org-pomodoro-extras.el
;; Version: 0.2
;; Package-Requires: ((emacs "24.4") (org-pomodoro "2.1") (el-patch "1.1"))

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

;; Extensions for `org-pomodoro'.

;;; Code:

(require 'el-patch)
(require 'org-pomodoro)

;;;; Functions

(defun org-extras-pomodoro-format-timer ()
  "Format the `org-pomodoro' timer.
We set this value by advising `org-pomodoro' so that the pomodoro count is
updated."
  (setq org-pomodoro-format
	(concat "🍅 %s"
		(format "|%s" (number-to-string org-pomodoro-count)))))

;;;; Patched functions

;; silence silly lecture about killing pomodoros
(el-patch-defun org-pomodoro-killed ()
  "Is invoked when a pomodoro was killed.
This may send a notification, play a sound and adds log."
  (org-pomodoro-reset)
  (org-pomodoro-notify "Pomodoro killed." (el-patch-swap "One does not simply kill a pomodoro!" ""))
  (org-pomodoro-maybe-play-sound :killed)
  (when (org-clocking-p)
    (if org-pomodoro-keep-killed-pomodoro-time
	(org-clock-out nil t)
      (org-clock-cancel)))
  (run-hooks 'org-pomodoro-killed-hook))

(provide 'org-pomodoro-extras)
;;; org-pomodoro-extras.el ends here

