;;; pdf-tools-extras.el --- Extensions for pdf-tools -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/pdf-tools-extras.el
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

;; Extensions for `pdf-tools'.

;;; Code:

(require 'pdf-annot)
(require 'pdf-tools)

;;;; Variables

(defvar pdf-tools-extras-selected-pages '()
  "List of pages selected for extraction.")

;;;; Functions

(declare-function modus-themes--current-theme "modus-themes")
(defun pdf-tools-extras-apply-theme ()
  "Activate `pdf-tools' midnight mode if dark theme is active."
  (if (eq (modus-themes--current-theme) 'modus-vivendi)
      (pdf-view-midnight-minor-mode)
    (pdf-view-midnight-minor-mode -1)))

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

(defun pdf-tools-extras-add-or-remove-page ()
  "Add current page number to list of selected pages.
If page number is already listed, remove it from list."
  (interactive)
  (if (member (pdf-view-current-page) pdf-tools-extras-selected-pages)
      (progn
        (setq pdf-tools-extras-selected-pages (delete (pdf-view-current-page) pdf-tools-extras-selected-pages)
              pdf-tools-extras-selected-pages (sort pdf-tools-extras-selected-pages #'<))
        (message "Page removed. Current selection: %s." pdf-tools-extras-selected-pages))
    (add-to-list 'pdf-tools-extras-selected-pages (pdf-view-current-page) t)
    (setq pdf-tools-extras-selected-pages (sort pdf-tools-extras-selected-pages #'<))
    (message "Page added. Current selection: %s." pdf-tools-extras-selected-pages))
  (when (< (pdf-view-current-page) (pdf-cache-number-of-pages))
    (pdf-view-next-page))
  (setq pdf-tools-extras-selected-pages (sort pdf-tools-extras-selected-pages #'<)))

(defun pdf-tools-extras-clear-page-selection ()
  "Clear the list of pages selected in `pdf-tools-extras-selected-pages'."
  (interactive)
  (setq pdf-tools-extras-selected-pages '())
  (message "Page selection cleared."))

(defun pdf-tools-extras-extract-pages (file)
  "Save pages selected in `pdf-tools-extras-selected-pages' to FILE."
  (interactive "FSave as: ")
  (let ((output (if (string= (expand-file-name file) (buffer-file-name))
                    "--replace-input"
                  (expand-file-name file))))
    (shell-command (format "qpdf '%s' --pages . %s -- '%s'"
                           (buffer-file-name)
                           (mapconcat #'number-to-string
                                      pdf-tools-extras-selected-pages
                                      ",")
                           output)))
  (pdf-tools-extras-clear-page-selection))

(defun pdf-extras-count-words ()
  "Count words in current PDF."
  (interactive)
  (kill-new
   (string-trim
    (shell-command-to-string
     (format "pdftotext '%s' - | wc -w" (buffer-file-name)))))
  (message (format "This PDF has %s words." (current-kill 0))))

(defun pdf-tools-extras-copy-dwim ()
  "Copy PDF to kill ring, or region if selected."
  (interactive)
  (if (region-active-p)
      (pdf-view-kill-ring-save)
    (let ((string (string-trim (shell-command-to-string
                                (format "pdftotext '%s' -" (buffer-file-name))))))
      (kill-new (replace-regexp-in-string "\\([^\n]\\)\n\\([^\n]\\)" "\\1 \\2" string))
      (message "Copied all text in PDF to kill ring."))))

(defun pdf-tools-extras-delete ()
  "Delete current PDF."
  (interactive)
  (let ((file (buffer-file-name)))
    (when (yes-or-no-p (format "Delete %s?" file))
      (delete-file file)
      (kill-buffer))))

(defun pdf-annot-extras-add-highlight-markup-annotation (list-of-edges
							 &optional color property-alist)
  "Like `pdf-annot-add-highlight-markup-annotation', but copy the region.
LIST-OF-EDGES is a list of edges of the region to highlight. COLOR and
PROPERTY-ALIST are as in `pdf-annot-add-highlight-markup-annotation'."
  (interactive (list (pdf-view-active-region)))
  (require 'pdf-annot)
  (let* ((txt (pdf-view-active-region-text)))
    (pdf-annot-add-markup-annotation list-of-edges 'highlight color property-alist)
    (pdf-view-deactivate-region)
    (kill-new (mapconcat 'identity txt "\n"))))

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

(provide 'pdf-tools-extras)
;;; pdf-tools-extras.el ends here

