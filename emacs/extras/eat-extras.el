;;; eat-extras.el --- Extensions for eat -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/eat-extras.el
;; Version: 0.1
;; Package-Requires: ((eat "0.9"))

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

;; Extensions for `eat'.

;;; Code:

(require 'eat)

;;;; Variables

(defgroup eat-extras ()
  "Extensions for `eat'."
  :group 'eat)

(defvar eat-extras-non-bound-keys
  '([?\C-g] [?\C-h] [?\C-l] [?\C-u] [?\C-x]
    [?\M-o] [?\M-x]
    [A-C-s-p] [A-C-s-u]
    [C-H-M-q] [C-H-M-r] [C-H-M-s] [C-H-M-v]
    [H-v] [H-z])
  "Keys to exclude from terminal capture in semi-char mode.")

(defvar eat-extras-emacs-passthrough-keys
  '("C-g" "C-h" "C-l" "C-u" "C-x" "M-o" "M-x")
  "Standard Emacs prefix keys to release from terminal capture.")

;;;; Functions

(defun eat-extras-send-forward-word ()
  "Send `forward-word' to the terminal."
  (interactive)
  (eat-term-send-string eat-terminal "\ef"))

(defun eat-extras-send-backward-word ()
  "Send `backward-word' to the terminal."
  (interactive)
  (eat-term-send-string eat-terminal "\eb"))

(defun eat-extras-send-kill-word ()
  "Send `kill-word' to the terminal."
  (interactive)
  (eat-term-send-string eat-terminal "\ed"))

(defun eat-extras-send-kill-line ()
  "Send `kill-line' to the terminal."
  (interactive)
  (eat-term-send-string eat-terminal "\C-k"))

(defun eat-extras-send-backspace ()
  "Send backspace to the terminal."
  (interactive)
  (eat-term-send-string eat-terminal "\x7f"))

(defun eat-extras-send-meta-backspace ()
  "Send `backward-kill-word' to the terminal."
  (interactive)
  (eat-term-send-string eat-terminal "\e\x7f"))

(defun eat-extras-send-undo ()
  "Send undo to the terminal."
  (interactive)
  (eat-term-send-string eat-terminal "\C-_"))

(defun eat-extras-setup-semi-char-mode-map ()
  "Configure semi-char mode map with custom navigation key bindings."
  (dolist (key eat-extras-non-bound-keys)
    (add-to-list 'eat-semi-char-non-bound-keys key))
  (eat-update-semi-char-mode-map)
  (dolist (key eat-extras-emacs-passthrough-keys)
    (define-key eat-semi-char-mode-map (kbd key) nil))
  (bind-keys :map eat-semi-char-mode-map
             ("A-C-s-p" . eat-extras-send-forward-word)
             ("A-C-s-u" . eat-extras-send-backward-word)
             ("C-H-M-q" . eat-extras-send-meta-backspace)
             ("C-H-M-r" . eat-extras-send-kill-word)
             ("C-H-M-s" . eat-extras-send-backspace)
             ("C-H-M-v" . eat-extras-send-kill-line)
             ("H-v" . eat-yank)
             ("H-z" . eat-extras-send-undo))
  (eat-extras-sync-semi-char-mode-map))

(defun eat-extras-sync-semi-char-mode-map ()
  "Sync `minor-mode-map-alist' with the current `eat-semi-char-mode-map'.
`eat-update-semi-char-mode-map' replaces the map object via `setq',
but `minor-mode-map-alist' still references the old one."
  (let ((entry (assq 'eat--semi-char-mode minor-mode-map-alist)))
    (when entry
      (setcdr entry eat-semi-char-mode-map))))

(provide 'eat-extras)
;;; eat-extras.el ends here
