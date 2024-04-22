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
(require 'hl-line)

;;;; User options

(defgroup faces-extras ()
  "Extensions for `faces'."
  :group 'faces)

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

(defvar faces-extras-custom-face-attributes '()
  "Custom face attributes.
These attributes can be set with `faces-extras-set-custom-face-attributes'.")

;;;; Functions

(defun faces-extras-set-face-attribute (attribute)
  "Set a single face ATTRIBUTE."
  (let ((face (car attribute))
        (args (cdr attribute)))
    (apply 'set-face-attribute face nil args)))

(defun faces-extras-set-face-attributes (attributes)
  "Set a list of face ATTRIBUTES."
  (dolist (attribute attributes)
    (faces-extras-set-face-attribute attribute)))

(defun faces-extras-set-and-store-face-attributes (attributes)
  "Set list of face ATTRIBUTES and store them.
The attributes are stored in `faces-extras-custom-face-attributes'."
  (dolist (attribute attributes)
    (faces-extras-set-face-attribute attribute)
    (add-to-list 'faces-extras-custom-face-attributes attribute)))

(defun faces-extras-set-custom-face-attributes ()
  "Set custom face attributes."
  (interactive)
  (faces-extras-set-face-attributes faces-extras-custom-face-attributes))

(defun faces-extras-load-custom-faces ()
  "My custom faces, to be used in conjunction with theme."
  (interactive)
  (set-face-attribute 'default nil :family faces-extras-fixed-pitch-font :height faces-extras-fixed-pitch-size)
  (set-face-attribute 'fixed-pitch nil :family faces-extras-fixed-pitch-font :height 1.1)
  (set-face-attribute 'variable-pitch nil :family faces-extras-variable-pitch-font :height 1.4)
  (set-face-attribute 'org-drawer nil :foreground "LightSkyBlue" :family faces-extras-fixed-pitch-font :height 0.8)
  (set-face-attribute 'org-property-value nil :family faces-extras-fixed-pitch-font :height 0.8)
  (set-face-attribute 'org-todo nil :family faces-extras-fixed-pitch-font :height 1.0)
  (set-face-attribute 'org-archived nil :family faces-extras-fixed-pitch-font :height 0.9)
  (set-face-attribute 'org-document-title nil :family faces-extras-fixed-pitch-font :height 1.0)
  (set-face-attribute 'org-special-keyword nil :family faces-extras-fixed-pitch-font :height 0.8)
  (set-face-attribute 'org-tag nil :family faces-extras-fixed-pitch-font :height 0.9)
  (set-face-attribute 'org-code nil :family faces-extras-fixed-pitch-font :height 1.3)
  (set-face-attribute 'org-level-1 nil :family faces-extras-fixed-pitch-font :height 0.9)
  (set-face-attribute 'org-level-2 nil :family faces-extras-fixed-pitch-font :height 0.9)
  (set-face-attribute 'org-level-3 nil :family faces-extras-fixed-pitch-font :height 0.9)
  (set-face-attribute 'org-level-4 nil :family faces-extras-fixed-pitch-font :height 0.9)
  (set-face-attribute 'org-level-5 nil :family faces-extras-fixed-pitch-font :height 0.9)
  (set-face-attribute 'org-level-6 nil :family faces-extras-fixed-pitch-font :height 0.9)
  (set-face-attribute 'org-level-7 nil :family faces-extras-fixed-pitch-font :height 0.9)
  (set-face-attribute 'org-level-8 nil :family faces-extras-fixed-pitch-font :height 0.9)
  (set-face-attribute 'org-date nil :family faces-extras-fixed-pitch-font :height 0.8)
  (set-face-attribute 'org-quote nil :family faces-extras-variable-pitch-font :height 1.3)
  (when (bound-and-true-p org-modern-mode)
    (set-face-attribute 'org-modern-date-active nil :family faces-extras-fixed-pitch-font :height 0.8)
    (set-face-attribute 'org-modern-date-inactive nil :family faces-extras-fixed-pitch-font :height 0.8)
    (set-face-attribute 'org-modern-tag nil :family faces-extras-fixed-pitch-font :height 0.9)
    (set-face-attribute 'org-modern-label nil :family faces-extras-fixed-pitch-font :height 0.8))
  (set-face-attribute 'shr-text nil :height 0.65)
  (when (bound-and-true-p corfu-mode)
    (set-face-attribute 'corfu-default nil :family faces-extras-fixed-pitch-font :height 1))
  (when (bound-and-true-p flycheck-mode)
    (set-face-attribute 'flycheck-error nil :underline '(:color "#ff0000" :style wave))
    (set-face-attribute 'flycheck-warning nil :underline '(:color "#0000ff" :style wave)))
  (when (bound-and-true-p jinx-mode)
    (set-face-attribute 'jinx-misspelled nil :underline '(:color "#008000" :style wave)))
  (set-face-attribute 'window-divider nil :foreground (face-attribute 'mode-line-inactive :background))
  (set-face-attribute 'tab-bar nil
		      :background (face-background 'mode-line)
		      ;; slightly increase the width of the tab-bar
		      :box `(:line-width 6 :color ,(face-attribute 'mode-line :background) :style nil)))

;; github.com/arunkmv/.config/tree/main/emacs#tab-bar
(defface faces-extras-display-time
  '((t (:inherit bold)))
  "Face for `display-time-string' in `global-mode-string'.")

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
