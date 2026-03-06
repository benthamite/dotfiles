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

(defcustom eat-extras-marker-cleanup-interval 60
  "Seconds between marker-cleanup checks in eat buffers."
  :type 'natnum
  :group 'eat-extras)

(defcustom eat-extras-marker-cleanup-threshold 0.01
  "Insertion time in seconds that triggers marker cleanup.
When a single character insertion in an eat buffer takes longer than
this threshold, the buffer is cleaned up to free orphaned markers."
  :type 'float
  :group 'eat-extras)

(defvar eat-extras--marker-cleanup-timer nil
  "Timer for periodic eat buffer marker cleanup.")

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

;;;;; Marker cleanup

(defun eat-extras--insertion-time (buffer)
  "Measure the time of a single character insertion in BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (buffer-undo-list t)
          (start (float-time)))
      (save-excursion
        (goto-char (point-max))
        (insert "X")
        (delete-char -1))
      (- (float-time) start))))

(defun eat-extras--cleanup-buffer (buffer)
  "Free orphaned markers in BUFFER by swapping buffer text.
Creates a temporary buffer, swaps text contents (transferring all
markers to the temp buffer), copies back only the text, then kills
the temp buffer along with the orphaned markers."
  (with-current-buffer buffer
    (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
           (pt (point))
           (temp (generate-new-buffer " *eat-cleanup*")))
      (buffer-swap-text temp)
      ;; Now BUFFER has temp's clean (empty) text with no markers.
      ;; Reinsert the content.
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)
            (buffer-undo-list t))
        (insert text)
        (goto-char (min pt (point-max))))
      ;; Kill temp buffer, which holds all the orphaned markers.
      (kill-buffer temp)
      ;; Re-initialize the eat terminal display to update its markers.
      (when (and (boundp 'eat-terminal) eat-terminal)
        (eat-term-resize eat-terminal
                         (eat-term-parameter eat-terminal 'width)
                         (eat-term-parameter eat-terminal 'height))
        (eat-term-redisplay eat-terminal)))))

(defun eat-extras-cleanup-markers ()
  "Check all eat buffers and clean up those with degraded insertion performance."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (eq (buffer-local-value 'major-mode buf) 'eat-mode)
               (> (buffer-size buf) 0))
      (condition-case nil
          (let ((time (eat-extras--insertion-time buf)))
            (when (> time eat-extras-marker-cleanup-threshold)
              (message "eat-extras: cleaning up markers in %s (insertion: %.3fs)"
                       (buffer-name buf) time)
              (eat-extras--cleanup-buffer buf)))
        (error nil)))))

(defun eat-extras-start-marker-cleanup-timer ()
  "Start the periodic marker cleanup timer for eat buffers."
  (eat-extras-stop-marker-cleanup-timer)
  (setq eat-extras--marker-cleanup-timer
        (run-with-timer eat-extras-marker-cleanup-interval
                        eat-extras-marker-cleanup-interval
                        #'eat-extras-cleanup-markers)))

(defun eat-extras-stop-marker-cleanup-timer ()
  "Stop the periodic marker cleanup timer."
  (when eat-extras--marker-cleanup-timer
    (cancel-timer eat-extras--marker-cleanup-timer)
    (setq eat-extras--marker-cleanup-timer nil)))

(provide 'eat-extras)
;;; eat-extras.el ends here
