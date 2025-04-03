;;; markdown-mode-extras.el --- Extensions for markdown-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/markdown-mode-extras.el
;; Version: 0.2
;; Package-Requires: ((markdown-mode "2.1"))

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

;; Extensions for `markdown-mode'.

;;; Code:

(require 'markdown-mode)

;;;; Functions

(defun markdown-mode-extras-org-paste-dwim ()
  "Convert between `org-mode' and `markdown' based on current mode."
  (interactive)
  (let* ((clipboard (if (eq system-type 'darwin)
			"pbv public.utf8-plain-text"
		      "xclip -out -selection 'clipboard' -t text/html"))
	 (source (pcase major-mode
		   ('markdown-mode "org")
		   ('org-mode "markdown")
		   (_ (user-error "Not in `org-mode' or `markdown-mode'"))))
	 (target (if (string= source "org") "markdown" "org"))
	 (pandoc (concat "pandoc --wrap=none -f " source " -t " target))
	 (cmd (concat clipboard " | " pandoc))
	 (output (shell-command-to-string cmd))
	 ;; Not sure why Pandoc adds these double slashes; we remove them
	 (output (replace-regexp-in-string "^\\\\\\\\$" "" output))
	 (output (replace-regexp-in-string "= " "= " output)))
    (kill-new output)
    (yank)))

;;;###autoload
(defun markdown-mode-extras-get-section (&optional section)
  "Return SECTION in the current buffer.
If SECTION is nil, return the current section."
  (save-excursion
    (let (beg end)
      (when section
	(goto-char (point-min))
	(re-search-forward (format "^#\\{1,\\} %s" section)))
      (unless (thing-at-point-looking-at markdown-regex-header)
	(markdown-outline-previous))
      (forward-line)
      (setq beg (point))
      (markdown-outline-next)
      (setq end (point))
      (buffer-substring-no-properties beg end))))

(defun markdown-mode-extras-copy-section (&optional section)
  "Copy SECTION in the current buffer to the clipboard.
If SECTION is nil, copy the current section."
  (interactive)
  (let ((section (markdown-mode-extras-get-section section)))
    (kill-new section)
    (message section)))

(defun markdown-mode-extras-delete-link ()
  "Delete link at point and return its name."
  (when-let* ((link (markdown-link-at-pos (point)))
	      (begins (car link))
	      (ends (cadr link))
	      (name (nth 1 (cdr link))))
    (delete-region begins ends)
    name))

(defun markdown-mode-extras-remove-url-in-link (arg)
  "Remove the markdown link at point and keep the description.
With ARG prefix argument, prompt for the URL to use."
  (interactive "P")
  (when (markdown-inside-link-p)
    (save-excursion
      (let* ((name (markdown-mode-extras-delete-link)))
	(if arg
	    (insert (format "[%s](%s)" name (read-string "URL: ")))
	  (insert name))))))

(autoload 'simple-extras-pandoc-convert "simple-extras")
(defun markdown-mode-extras-paste-with-conversion ()
  "Convert the contents of the system clipboard to target Markdown using Pandoc.
This command will convert from HTML if the clipboard contains HTML, and from Org
Mode otherwise.

See also `org-extras-paste-with-conversion'. For the reverse process, use
`ox-clip-formatted-copy'."
  (interactive)
  (insert (simple-extras-pandoc-convert "gfm-raw_html" "org")))

(provide 'markdown-mode-extras)
;;; markdown-mode-extras.el ends here
