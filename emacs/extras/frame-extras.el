;;; frame-extras.el --- Extensions for frame -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/frame-extras.el
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

;; Extensions for `frame'.

;;; Code:

(require 'frame)

;;;; User options

;;;; Main variables

;;;; Functions


(defun frame-extras-maximize-frame ()
  "Maximize the frame every time focus is regained.
When returning focus to Emacs after browsing other macOS windows, the frame is
sometimes not fully maximized. This advice makes Emacs maximize the frame every
time focus is regained."
  (set-frame-parameter nil 'fullscreen 'maximized))

(add-function :after after-focus-change-function #'frame-extras-maximize-frame)

(provide 'frame-extras)
;;; frame-extras.el ends here

