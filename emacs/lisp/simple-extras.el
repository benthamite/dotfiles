;;; simple-extras.el --- Extra functionality for the simple feature -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/lisp/simle-extras.el
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

;; Extra functionality for the `simple' feature.

;;; Code:

;;;; User options

;;;; Variables

;;;; Functions

(defmacro simple-extras-delete-instead-of-kill (&rest body)
  "Replace `kill-region' with `delete-region' in BODY."
  `(cl-letf (((symbol-function 'kill-region)
              (lambda (beg end)
                (delete-region beg end))))
     ,@body))

(defmacro simple-extras-copy-instead-of-kill (&rest body)
  "Replace `kill-region' with `kill-ring-save' in BODY."
  `(cl-letf (((symbol-function 'kill-region)
              (lambda (beg end)
                (kill-ring-save beg end)
                (setq this-command 'kill-region))))
     ,@body))

(defun simple-extras-kill-whole-thing (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun simple-extras-delete-word (&optional arg)
  "Like `kill-word', but deletes instead of killing.
With argument ARG, do this that many times."
  (interactive "p")
  (ps/delete-instead-of-kill (kill-word arg)))

(defun simple-extras-backward-delete-word (&optional arg)
  "Like `backward-kill-word', but deletes instead of killing.
With argument ARG, do this that many times."
  (interactive "p")
  (ps/delete-instead-of-kill (backward-kill-word arg)))

(defun simple-extras-copy-word (&optional arg)
  "Like `kill-word', but copies instead of killing.
With argument ARG, do this that many times."
  (interactive "P")
  (ps/copy-instead-of-kill (kill-word arg)))

;; The macro wasn't working for `backward-kill-word', so using a custom function.
(defun simple-extras-backward-copy-word ()
  "Like `backward-kill-word', but copies instead of killing."
  (interactive)
  (copy-region-as-kill (point) (progn (backward-word) (point))))

(defun simple-extras-kill-whole-word ()
  "Kill the word at point."
  (interactive)
  (ps/kill-whole-thing 'word))

(defun simple-extras-delete-whole-word ()
  "Like `kill-whole-word', but deletes instead of killing."
  (interactive)
  (ps/delete-instead-of-kill (simple-extras-kill-whole-word)))

(defun simple-extras-copy-whole-word ()
  "Like `kill-whole-word', but copies instead of killing.
With argument ARG, do this that many times."
  (interactive)
  (ps/copy-instead-of-kill (simple-extras-kill-whole-word)))

(defun simple-extras-transpose-words-backward ()
  "Interchange words around point, leaving point at beginning."
  (interactive)
  (transpose-words -1))

(provide 'simple-extras)
;;; simple-extras.el ends here
