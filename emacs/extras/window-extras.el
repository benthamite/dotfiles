;;; window-extras.el --- Extensions for window.el -*- lexical-binding: t -*-

;; Copyright (C) 2023

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
  "Get to previously selected ordinary or minibuffer window."
  (interactive)
  (if (and (active-minibuffer-window) (not (minibufferp)))
      (select-window (active-minibuffer-window))
    (get-mru-window 'visible t t)))

(defun window-extras-switch-to-last-window ()
  "Switch to previously selected ordinary or minibuffer window."
  (interactive)
  (let ((last-window (window-extras-get-last-window)))
    (select-frame-set-input-focus (window-frame last-window))
    (select-window last-window)))

;; Modified from endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun window-extras-narrow-or-widen-dwim ()
  "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, ledger
  transaction, or defun, whichever applies first. Narrowing to
  org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
  (interactive)
  (declare (interactive-only))
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-narrow-to-block) t))
               (t (org-extras-narrow-to-entry-and-children))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        ((derived-mode-p 'ledger-mode)
         (ledger-mode-extras-narrow-to-xact))
        (t (narrow-to-defun))))

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
  "docstring"
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
  "Swap the current buffer and the buffer in the other
window. If there is only one window, create a second one. If frame
is wide enough, create a third."
  (interactive)
  (window-extras--move-or-swap
   (window-buffer)
   (window-buffer (window-extras-get-last-window))))

(defun window-extras-buffer-move (&optional target-window)
  "Move the current buffer to the other window. If there is only one
window, create a second one. If frame is wide enough, create a third."
  (interactive)
  (require 'files-extras)
  (window-extras--move-or-swap
   (window-buffer)
   (files-extras-get-alternate-buffer)
   target-window))

(defun window-extras-buffer-move-right ()
  "docstring."
  (interactive)
  (window-extras-buffer-move
   (winum-get-window-by-number
    (1+
     (mod
      (winum-get-number)
      (count-windows))))))

(defun window-extras-buffer-move-left ()
  "docstring."
  (interactive)
  (window-extras-buffer-move
   (winum-get-window-by-number
    (1+
     (mod
      (count-windows)
      (winum-get-number))))))

(defun window-extras-buffer-move-dwim ()
  "Based on frame size, create one or two additional windows if
necessary, and move buffer to the other window or to the middle
window depending on the number of present windows."
  (interactive)
  (window-extras-buffer-move (when (> (count-windows) 2) (winum-get-window-by-number 2))))

(defun window-extras-frame-is-maximized-p ()
  "Return t iff the current frame is maximized."
  (and
   (eq (frame-pixel-width) (display-pixel-width))
   (eq (frame-pixel-height) (display-pixel-height))))

;; superuser.com/a/132454/387888
(defun window-extras-switch-to-minibuffer-window ()
  "Switch to minibuffer window, if active."
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(provide 'window-extras)
;;; window-extras.el ends here

