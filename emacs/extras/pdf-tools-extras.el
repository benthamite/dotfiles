;;; pdf-tools-extras.el --- Extensions for pdf-tools -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/pdf-tools-extras.el
;; Version: 0.2
;; Package-Requires: ((pdf-tools "1.1"))

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

;; Extensions for `pdf-tools'.

;;; Code:

(require 'pdf-tools)

;;;; Functions

;;;;; Word selection with double-click

;; adapted from emacs.stackexchange.com/a/52463/32089
;;
;; not currently using this since it double-clicking a word usually selects
;; several words and it's unclear how to fix it. also, some people seem to be
;; working on incorporating this or similar functionality so I’d rather wait
;; until that happens. I'm leaving this section here because I may decide to
;; resume work on it if the package is not extended in the end.
(defvar pdf-tools-extras-sel-mode-map nil
  "Keymap for `pdf-tools-extras-sel-mode'.")

(setq pdf-tools-extras-sel-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map [double-mouse-1] 'pdf-tools-extras-sel-mouse)
	(define-key map (kbd "<H-double-mouse-1>") 'goldendict-ng-search)
	map))

(define-minor-mode pdf-tools-extras-sel-mode
  "Minor mode for selecting words in PDFs.
\\<pdf-sel-mode-map>Just binding \\[pdf-tools-extras-sel-mouse] to
`pdf-tools-extras-sel-mouse'. `pdf-tools-extras-sel-mouse' selects the text at
point and copies it to `kill-ring'."
  :keymap pdf-tools-extras-sel-mode-map)

(defun pdf-tools-extras-sel-mouse (ev)
  "Select word at mouse event EV and copy it to `kill-ring'."
  (interactive "@e")
  (let* ((posn (event-start ev))
	 (xy (posn-object-x-y posn))
	 (size (pdf-view-image-size))
	 (page (pdf-view-current-page))
	 (x (/ (car xy) (float (car size))))
         (y (/ (cdr xy) (float (cdr size)))))
    (setq pdf-view-active-region (pdf-info-getselection page (list x y x y) 'word))
    (pdf-view-display-region pdf-view-active-region)
    (kill-new (pdf-info-gettext page (list x y x y) 'word))))

(pdf-tools-extras-sel-mode 1)

;;;;; Misc

(declare-function modus-themes-get-current-theme "modus-themes")
(defun pdf-tools-extras-apply-theme ()
  "Activate `pdf-tools' midnight mode if the active Modus theme is dark."
  (let* ((theme (when (fboundp 'modus-themes-get-current-theme)
                  (modus-themes-get-current-theme)))
         (background (when theme
                       (plist-get (get theme 'theme-properties) :background-mode))))
    (if (eq background 'dark)
        (pdf-view-midnight-minor-mode 1)
      (pdf-view-midnight-minor-mode -1))))

;; gist.github.com/politza/3f46785742e6e12ba0d1a849f853d0b9#file-scroll-other-window-el
(defvar writeroom-width)
(declare-function writeroom-mode "writeroom-mode")
(defun pdf-tools-extras-toggle-writeroom ()
  "Toggle `writeroom-mode' on and off."
  (interactive)
  (let ((writeroom-width 120))
    (writeroom-mode 'toggle)
    (pdf-view-fit-height-to-window)))

(defun pdf-tools-extras-open-externally ()
  "Open current PDF in external application.
If `opentopage' script is available, open to current page."
  (interactive)
  (let ((file (pdf-view-buffer-file-name))
        (script "opentopage")) ; apple.stackexchange.com/a/233987
    (if (file-exists-p (file-name-concat "~/bin" script))
        (shell-command (format "sh %s '%s' %d" script file (pdf-view-current-page)))
      (shell-command (format "open '%s'" file)))))

(defun pdf-tools-extras-count-words ()
  "Count words in current PDF."
  (interactive)
  (kill-new
   (string-trim
    (shell-command-to-string
     (format "pdftotext '%s' - | wc -w" (buffer-file-name)))))
  (message (format "This PDF has %s words." (current-kill 0))))

(declare-function tlon-convert-pdf "tlon-import")
(defun pdf-tools-extras-copy-dwim ()
  "Copy PDF contents to kill ring, or region if selected."
  (interactive)
  (if (region-active-p)
      (pdf-view-kill-ring-save)
    (let ((string (string-trim (tlon-convert-pdf (buffer-file-name)))))
      (kill-new (replace-regexp-in-string "\\([^\n]\\)\n\\([^\n]\\)" "\\1 \\2" string))
      (message "Copied all text in PDF to kill ring."))))

(defun pdf-tools-extras-delete ()
  "Delete current PDF."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (yes-or-no-p (format "Delete %s?" file))
      (delete-file file)
      (kill-buffer))))

(autoload 'pdf-annot-add-markup-annotation "pdf-annot")
(defun pdf-annot-extras-add-highlight-markup-annotation (list-of-edges
							 &optional color property-alist)
  "Like `pdf-annot-add-highlight-markup-annotation', but copy the region.
LIST-OF-EDGES is a list of edges of the region to highlight. COLOR and
PROPERTY-ALIST are as in `pdf-annot-add-highlight-markup-annotation'."
  (interactive (list (pdf-view-active-region)))
  (let* ((txt (pdf-view-active-region-text)))
    (pdf-annot-add-markup-annotation list-of-edges 'highlight color property-alist)
    (pdf-view-deactivate-region)
    (kill-new (mapconcat 'identity txt "\n"))))

(declare-function ebib-extras-open-key "ebib-extras")
(defun pdf-tools-extras-open-in-ebib ()
  "Open the entry corresponding to the current PDF in `ebib'.
The function assumes that the PDF is named after the corresponding BibTeX key."
  (interactive)
  (unless (derived-mode-p 'pdf-view-mode)
    (user-error "Not in `pdf-view-mode'"))
  (let ((key (file-name-base (buffer-file-name))))
    (ebib-extras-open-key key)))

(provide 'pdf-tools-extras)
;;; pdf-tools-extras.el ends here

