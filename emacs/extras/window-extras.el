;;; window-extras.el --- Extensions for window.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/window-extras.el
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

;; Extensions for `window.el'.

;;; Code:

;;;; User options

(defgroup window-extras ()
  "Extensions for `window'."
  :group 'window)

(defcustom window-extras-frame-split-width-threshold  350
  "Width threshold for splitting the frame, in columns."
  :type 'integer
  :group 'window-extras)

;;;; Functions

(defun window-extras-get-last-window ()
  "Return the most recently selected window in the current frame.
The most recently selected window may include the minibuffer window."
  (interactive)
  (if (and (active-minibuffer-window) (not (minibufferp)))
      (select-window (active-minibuffer-window))
    (get-mru-window (selected-frame) t t)))

(defun window-extras-switch-to-last-window ()
  "Switch to the most recently selected window in the current frame.
The most recently selected window may include the minibuffer window."
  (interactive)
  (select-window (window-extras-get-last-window)))

;;;###autoload
(defun window-extras-split-if-unsplit ()
  "Split windows when frame is unsplit.
Split in three windows if `frame-width' is greater than
`window-extras-frame-split-width-threshold', otherwise in two windows."
  (interactive)
  (when (= (length (window-list)) 1)
    (split-window-right))
  (when (> (frame-width) window-extras-frame-split-width-threshold)
    (when (= (length (window-list)) 2)
      (split-window-right))
    (balance-windows)))

(defun window-extras--move-or-swap (this-buffer other-buffer &optional target-window)
  "Move THIS-BUFFER to the other window, and OTHER-BUFFER to the current window.
If TARGET-WINDOW is non-nil, move THIS-BUFFER to TARGET-WINDOW and OTHER-BUFFER
to the current window. If there is no other window, create it and move
THIS-BUFFER to it."
  (window-extras-split-if-unsplit)
  (let* ((target-window (if (or (not target-window)
                                (eq (window-extras-get-last-window) target-window)
                                (eq (selected-window) target-window))
                            (window-extras-get-last-window)
                          target-window))
         (source-window (if (eq (selected-window) target-window)
                            (window-extras-get-last-window)
                          (selected-window))))
    (set-window-buffer target-window this-buffer)
    (set-window-buffer source-window other-buffer)
    (select-window target-window)))

(defun window-extras-buffer-swap ()
  "Swap the current buffer and the buffer in the other window.
If there is only one window, create a second one."
  (interactive)
  (window-extras--move-or-swap
   (window-buffer)
   (window-buffer (window-extras-get-last-window))))

(declare-function files-extras-get-alternate-buffer "files-extras")
(defun window-extras-buffer-move (target-window)
  "Move the current buffer to the TARGET-WINDOW.
If there is only one window, create a second one."
  (interactive)
  (window-extras--move-or-swap
   (window-buffer) (files-extras-get-alternate-buffer) target-window))

(declare-function winum-get-window-by-number "winum")
(declare-function winum-get-number "winum")
(defun window-extras-buffer-move-right ()
  "Move the current buffer to the right window."
  (interactive)
  (window-extras-buffer-move
   (winum-get-window-by-number (1+ (mod (winum-get-number) (count-windows))))))

(defun window-extras-buffer-move-left ()
  "Move the current buffer to the left window."
  (interactive)
  (window-extras-buffer-move
   (winum-get-window-by-number (1+ (mod (count-windows) (winum-get-number))))))

(defun window-extras-frame-is-maximized-p ()
  "Return t iff the current frame is maximized."
  (and (eq (frame-pixel-width) (display-pixel-width))
       (eq (frame-pixel-height) (display-pixel-height))))

;; superuser.com/a/132454/387888
(defun window-extras-switch-to-minibuffer-window ()
  "Switch to minibuffer window, if active."
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(defun window-extras-select-side-window (side)
  "Select the window in SIDE of of the current frame.
SIDE is either \"left\" or \"right\"."
  (interactive "sSide (left/right): ")
  (unless (member side '("left" "right"))
    (error "Direction must be either 'left' or 'right'"))
  (let* ((compare-func (if (string= side "left") '< '>))
         (extreme-window (car (sort (window-list nil 'no-minibuffer)
                                    (lambda (w1 w2)
                                      (funcall compare-func
                                               (window-left-column w1)
                                               (window-left-column w2)))))))
    (select-window extreme-window)))

(provide 'window-extras)
;;; window-extras.el ends here

