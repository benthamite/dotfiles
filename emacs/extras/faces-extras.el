;;; faces-extras.el --- Extensions for faces -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/faces-extras.el
;; Version: 0.2

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

;; Extensions for `faces'.

;;; Code:

(require 'faces)

;;;; User options

(defgroup faces-extras ()
  "Extensions for `faces'."
  :group 'faces)

(defvar faces-extras-custom-face-attributes nil
  "Custom face attributes.
These attributes can be set with `faces-extras-set-custom-face-attributes'.")

(defcustom faces-extras-variable-pitch-font
  "Source Serif Pro"
  "Font for fixed-pitch faces."
  :type 'string
  :group 'faces-extras)

(defcustom faces-extras-fixed-pitch-font
  "SauceCodePro Nerd Font"
  "Font for fixed-pitch faces."
  :type 'string
  :group 'faces-extras)

(defcustom faces-extras-fixed-pitch-size 120
  "Size of the font for fixed-pitch faces."
  :type 'integer
  :group 'faces-extras)

;;;; Functions

;;;;; Face attributes

(defun faces-extras-set-face-attribute (attribute)
  "Set a single face ATTRIBUTE."
  (let* ((face (car attribute))
         (args (cdr attribute))
         (evaluated-args (mapcar (lambda (arg)
				   (cond
				    ((functionp arg)
				     (funcall arg))
				    ((symbolp arg)
				     (symbol-value arg))
				    (t arg)))
                                 args)))
    (apply 'set-face-attribute face nil evaluated-args)))

(defun faces-extras-set-face-attributes (attributes)
  "Set a list of face ATTRIBUTES."
  (dolist (attribute attributes)
    (faces-extras-set-face-attribute attribute)))

(defun faces-extras-set-and-store-face-attributes (attributes)
  "Set list of face ATTRIBUTES and store them.
The attributes are stored in `faces-extras-custom-face-attributes'."
  (dolist (attribute attributes)
    (let* ((face (car attribute))
           (args (mapcar (lambda (arg)
			   "Wrap ARG in a lambda if it a list of attributes."
                           (if (and (listp arg) (not (eq (car arg) 'lambda)))
                               `(lambda () ,arg)
                             arg))
                         (cdr attribute)))
	   (cons (cons face args)))
      (faces-extras-set-face-attribute cons)
      (add-to-list 'faces-extras-custom-face-attributes cons t))))

(defun faces-extras-set-custom-face-attributes ()
  "Set custom face attributes stored in `faces-extras-custom-face-attributes'."
  (interactive)
  (faces-extras-set-face-attributes faces-extras-custom-face-attributes))

;;;;; Misc

;; github.com/arunkmv/.config/tree/main/emacs#tab-bar
(defface faces-extras-display-time
  '((t (:inherit bold)))
  "Face for `display-time-string' in `global-mode-string'.")

(defvar hl-line-mode)
;;;###autoload
(defun faces-extras-describe-face ()
  "Like `describe-face', but with `hl-line-mode' disabled.
Always use face at point."
  (interactive)
  (let ((hl-line-mode-enabled hl-line-mode)
        (global-hl-line-mode-enabled global-hl-line-mode))
    (hl-line-mode -1)
    (global-hl-line-mode -1)
    (describe-face (face-at-point t))
    (when hl-line-mode-enabled
      (hl-line-mode))
    (when global-hl-line-mode-enabled
      (global-hl-line-mode))))

(provide 'faces-extras)
;;; faces-extras.el ends here
