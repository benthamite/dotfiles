;;; modus-themes-extras.el --- Extra functionality for modus-themes -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/lisp/modus-themes-extras.el
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

;; Extra functionality for modus-themes.

;;; Code:

(require 'modus-themes)
(require 'cl-lib)
(require 'highlight-parentheses)

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

(defun modus-themes-extras-load-theme-emacs-mac ()
"Load modus theme that matches system."
(interactive)
(if (string= (plist-get (mac-application-state) :appearance) "NSAppearanceNameDarkAqua")
    (modus-themes-load-theme 'modus-operandi)
  (modus-themes-load-theme 'modus-vivendi)))

(defun modus-themes-extras-load-theme-emacs-plus (appearance)
"Load theme, taking current system APPEARANCE into consideration."
(mapc #'disable-theme custom-enabled-themes)
(pcase appearance
  ('light (modus-themes-load-theme 'modus-operandi))
  ('dark (modus-themes-load-theme 'modus-vivendi))))

(defun modus-themes-extras-load-theme-conditionally ()
  "Load themes conditional on which distribution of Emacs is installed."
  (cond ((boundp 'mac-effective-appearance-change-hook)
         ;; `emacs-mac'
         (modus-themes-extras-load-theme-emacs-mac))
        ;; `emacs-plus'
        ((boundp 'ns-system-appearance-change-functions)
         (add-hook 'ns-system-appearance-change-functions
                   #'modus-themes-extras-load-theme-emacs-plus))))

(defun modus-set-extras-extra-faces ()
  "Set extra faces for the `modus' themes."
  (if (eq 'modus-operandi (car custom-enabled-themes))
      (set-face-attribute 'hl-sentence nil :background "#bfefff")
    (set-face-attribute 'hl-sentence nil :background "#004065")))

(provide 'modus-themes-extras)
;;; modus-themes-extras.el ends here

