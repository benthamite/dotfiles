;;; modus-themes-extras.el --- Extra functionality for modus-themes -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/modus-themes-extras.el
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

;; Extensions for `modus-themes'.

;;; Code:


;;;; Variables

(defgroup modus-themes-extras ()
  "Extensions for `modus-themes'."
  :group 'modus-themes)

(defcustom modus-themes-extras-dark-theme 'modus-vivendi
  "The `modus' theme to use in dark mode."
  :type 'symbol
  :group 'modus-themes-extras)

(defcustom modus-themes-extras-light-theme 'modus-operandi
  "The `modus' theme to use in light mode."
  :type 'symbol
  :group 'modus-themes-extras)

;;;; Functions

;;;;; Toggle

(declare-function modus-themes-load-theme "modus-themes")
(defun modus-themes-extras-toggle ()
  "Toggle between the configured light and dark themes.
The light and dark themes are read from
`modus-themes-extras-light-theme' and
`modus-themes-extras-dark-theme' at call time, so changes to
those variables take effect immediately."
  (interactive)
  (if (eq (car custom-enabled-themes) modus-themes-extras-light-theme)
      (modus-themes-load-theme modus-themes-extras-dark-theme)
    (modus-themes-load-theme modus-themes-extras-light-theme)))

;;;;; Conditional theme loading

(autoload 'simple-extras-get-emacs-distro "simple-extras")
(defun modus-themes-extras-load-theme-conditionally ()
  "Load themes conditional on the Emacs distribution installed."
  (pcase (simple-extras-get-emacs-distro)
    ('emacs-mac (modus-themes-extras-load-theme-emacs-mac))
    ('emacs-plus (add-hook 'ns-system-appearance-change-functions
			   #'modus-themes-extras-load-theme-emacs-plus)
		 (modus-themes-extras-load-theme-emacs-plus
		  ns-system-appearance))))

(declare-function mac-application-state nil)
(defun modus-themes-extras-load-theme-emacs-mac ()
  "Load `modus' theme that matches system appearance."
  (interactive)
  (pcase (plist-get (mac-application-state) :appearance)
    ("NSAppearanceNameAqua" (modus-themes-load-theme modus-themes-extras-light-theme))
    ("NSAppearanceNameDarkAqua" (modus-themes-load-theme modus-themes-extras-dark-theme))))

(defun modus-themes-extras-load-theme-emacs-plus (appearance)
  "Load `modus' theme that matches system APPEARANCE."
  (pcase appearance
    ('light (modus-themes-load-theme modus-themes-extras-light-theme))
    ('dark (modus-themes-load-theme modus-themes-extras-dark-theme))))

;;;;; Theme configuration

(defvar highlight-parentheses-colors)
(defvar highlight-parentheses-background-colors)
(declare-function modus-themes-get-color-value "modus-themes")
(defun modus-themes-extras-highlight-parentheses (&rest _)
  "Highlight parentheses in the current buffer."
  (let* ((bg-keys '(bg-cyan-intense bg-magenta-intense bg-green-intense bg-yellow-intense))
         (fg-keys '(cyan magenta green yellow))
         (bg (mapcar (lambda (k) (modus-themes-get-color-value k :overrides)) bg-keys))
         (fg (mapcar (lambda (k) (modus-themes-get-color-value k :overrides)) fg-keys)))
    (setq highlight-parentheses-background-colors bg
          highlight-parentheses-colors fg)
    (when (fboundp 'global-highlight-parentheses-mode)
      (global-highlight-parentheses-mode 1))))

(defun modus-themes-extras-set-faces ()
  "Set extra faces for the `modus' themes.
You can set any additional faces like this:

(if (eq modus-themes-extras-light-theme (car custom-enabled-themes))
      (set-face-attribute 'hl-sentence nil :background \"#bfefff\")
    (set-face-attribute 'hl-sentence nil :background \"#004065\"))")

(provide 'modus-themes-extras)
;;; modus-themes-extras.el ends here
