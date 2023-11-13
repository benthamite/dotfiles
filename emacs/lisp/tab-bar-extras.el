;;; tab-bar-extras.el --- Extra functionality for the tab bar -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/lisp/tab-bar-extras.el
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
  "Extensions for `tab-bar'."
  :group 'tab-bar)

(defcustom tab-bar-extras-global-mode-string nil
  "Default value of `global-mode-string' when `tab-bar-extras-mode' is active."
  :type 'list
  :group 'tab-bar-extras)

;;;; Functions

(defun tab-bar-extras-reset ()
  "Temporary command to handle mysterious proliferation of clocks."
  (interactive)
  (require 'display-wttr)
  (setq global-mode-string tab-bar-extras-global-mode-string)
  (display-wttr-mode))

(defun tab-bar-extras-reset-unless-clock ()
  "Reset the tab-bar when org-clock isn't running.
The condition is included to prevent the currently clocked task
from disappearing when the tab-bar is reset."
  (require 'org-clock)
  (unless org-clock-current-task
    (tab-bar-extras-reset)))

(provide 'tab-bar-extras)
;;; tab-bar-extras.el ends here
