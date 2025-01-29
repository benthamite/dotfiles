;;; corfu-extras.el --- Extensions for corfu -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/corfu-extras.el
;; Version: 0.2
;; Package-Requires: ((corfu "0.1"))

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

;; Extensions for `corfu'.

;;; Code:

(require 'corfu)

;;;; Functions

;; Adapted from Prot
(defun corfu-extras-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
  (unless (bound-and-true-p vertico--input)
    (corfu-mode)))

;; github.com/minad/corfu#transfer-completion-to-the-minibuffer
(declare-function consult-completion-in-region "consult")
(defun corfu-extras-move-to-minibuffer ()
  "Transfer selected candidate to the minibuffer."
  (interactive)
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           completion-cycle-threshold completion-cycling)
       (consult-completion-in-region beg end table pred)))))
(add-to-list 'corfu-continue-commands #'corfu-extras-move-to-minibuffer)

(provide 'corfu-extras)
;;; corfu-extras.el ends here

