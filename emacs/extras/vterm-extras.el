;;; vterm-extras.el --- Extensions for vterm -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/vterm-extras.el
;; Version: 0.1
;; Package-Requires: ((vterm "0.0.2"))

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

;; Extensions for `vterm'.

;;; Code:

;; Load vterm only at runtime; the top-level `require' hangs in batch mode
;; because vterm prompts to compile its native module via `y-or-n-p'.
(declare-function vterm-send-key "vterm")
(declare-function vterm-send-backspace "vterm")
(declare-function vterm-send-meta-backspace "vterm")
(declare-function vterm-yank "vterm")
(declare-function vterm-yank-pop "vterm")
(declare-function vterm-undo "vterm")
(defvar vterm-keymap-exceptions)
(defvar vterm-mode-map)

;;;; Variables

(defgroup vterm-extras ()
  "Extensions for `vterm'."
  :group 'vterm)

(defvar vterm-extras-keymap-exceptions
  '("A-C-s-p" "A-C-s-u" "C-H-M-q" "C-H-M-r" "C-H-M-s" "C-H-M-v"
    "C-c" "C-g" "C-h" "C-l" "C-u" "C-x" "C-y" "H-z" "M-o" "M-x" "M-y")
  "Keys to exclude from terminal capture.")

;;;; Functions

(defun vterm-extras-send-forward-word ()
  "Send `forward-word' to the terminal."
  (interactive)
  (vterm-send-key "f" nil t nil))

(defun vterm-extras-send-backward-word ()
  "Send `backward-word' to the terminal."
  (interactive)
  (vterm-send-key "b" nil t nil))

(defun vterm-extras-send-kill-word ()
  "Send `kill-word' to the terminal."
  (interactive)
  (vterm-send-key "d" nil t nil))

(defun vterm-extras-send-kill-line ()
  "Send `kill-line' to the terminal."
  (interactive)
  (vterm-send-key "k" nil nil t))

(defun vterm-extras-setup-keymap ()
  "Configure vterm mode map with custom navigation key bindings."
  (setq vterm-keymap-exceptions vterm-extras-keymap-exceptions)
  (bind-keys :map vterm-mode-map
             ("<backspace>" . vterm-send-backspace)
             ("A-C-s-p" . vterm-extras-send-forward-word)
             ("A-C-s-u" . vterm-extras-send-backward-word)
             ("C-H-M-q" . vterm-send-meta-backspace)
             ("C-H-M-r" . vterm-extras-send-kill-word)
             ("C-H-M-s" . vterm-send-backspace)
             ("C-H-M-v" . vterm-extras-send-kill-line)
             ("C-y" . vterm-yank)
             ("DEL" . vterm-send-backspace)
             ("H-z" . vterm-undo)
             ("M-y" . vterm-yank-pop)))

(provide 'vterm-extras)
;;; vterm-extras.el ends here
