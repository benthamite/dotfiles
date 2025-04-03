;;; modus-themes-extras.el --- Extra functionality for modus-themes -*- lexical-binding: t -*-

;; Copyright (C) 2025

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

(require 'modus-themes)

;;;; Functions

;;;;; Conditional theme loading

(autoload 'simple-extras-get-emacs-distro "simple-extras")
(defun modus-themes-extras-load-theme-conditionally ()
  "Load themes conditional on the Emacs distribution installed."
  (pcase (simple-extras-get-emacs-distro)
    ('emacs-mac (modus-themes-extras-load-theme-emacs-mac))
    ('emacs-plus (add-hook 'ns-system-appearance-change-functions
			   #'modus-themes-extras-load-theme-emacs-plus))))

(declare-function mac-application-state nil)
(defun modus-themes-extras-load-theme-emacs-mac ()
  "Load `modus' theme that matches system appearance."
  (interactive)
  (pcase (plist-get (mac-application-state) :appearance)
    ("NSAppearanceNameAqua" (modus-themes-load-theme 'modus-operand))
    ("NSAppearanceNameDarkAqua" (modus-themes-load-theme 'modus-vivendi))))

(defun modus-themes-extras-load-theme-emacs-plus (appearance)
  "Load `modus' theme that matches system APPEARANCE."
  (pcase appearance
    ('light (modus-themes-load-theme 'modus-operandi))
    ('dark (modus-themes-load-theme 'modus-vivendi))))

;;;;; Theme configuration

(defvar highlight-parentheses-colors)
(defvar highlight-parentheses-background-colors)
(defun modus-themes-extras-highlight-parentheses ()
  "Highlight parentheses in the current buffer."
  (modus-themes-with-colors
    (setq highlight-parentheses-background-colors (list bg-cyan-intense
							bg-magenta-intense
							bg-green-intense
							bg-yellow-intense)
	  highlight-parentheses-colors (list cyan
					     magenta
					     green
					     yellow))))

(defun modus-themes-extras-set-faces ()
  "Set extra faces for the `modus' themes.
You can set any additional faces like this:

(if (eq 'modus-operandi (car custom-enabled-themes))
      (set-face-attribute 'hl-sentence nil :background \"#bfefff\")
    (set-face-attribute 'hl-sentence nil :background \"#004065\"))")

(provide 'modus-themes-extras)
;;; modus-themes-extras.el ends here
