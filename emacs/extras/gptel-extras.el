;;; gptel-extras.el --- Extensions for gptel -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/gptel-extras.el
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

;; Extensions for `gptel'.

;;; Code:

(require 'gptel)
(require 'paths)
(require 'simple-extras)

;;;; User options

(defgroup gptel-extras ()
  "Extensions for `gptel'."
  :group 'gptel)

(defcustom gptel-extras-gemini-mullvad-disconnect-after 1
  "The number of minutes to disconnect `mullvad' after starting the Gemini session."
  :type 'integer
  :group 'gptel-extras)

(defcustom gptel-extras-dir (file-name-concat paths-dir-notes "gptel/")
  "The directory where to save the `gptel' buffers."
  :type 'directory
  :group 'gptel-extras)

;;;; Functions

(defun gptel-extras-get-cost ()
  "Get the rough cost of prompting the current model.
This is used to display the relevant information in the modeline (see
`doom-modeline-extras').

Note that the cost is an approximation based on the number of words in the
buffer or selection. The function uses a 1.4 token/word conversion factor, but
the actual cost may deviate from this estimate. Also note that files or buffers
added to the context window are not included in the calculation."
  (let* ((cost-per-1m-tokens (get gptel-model :input-cost))
	 (words (if (region-active-p)
		    (count-words (region-beginning) (region-end))
		  (count-words (point-min) (point))))
	 (tokens-per-word 1.4)
	 (cost (/ (* cost-per-1m-tokens tokens-per-word words) 1000000.0)))
    cost))

(declare-function mullvad-connect-to-website "mullvad")
(defun gptel-extras-set-mullvad (orig-fun &rest args)
  "Enable `mullvad' when connecting to Gemini, then call ORIG-FUN with ARGS.
Use to circumvent Gemini’s location restrictions."
  (when (eq gptel-model 'gemini-pro)
    (require 'mullvad)
    (mullvad-connect-to-website "Gemini"
				gptel-extras-gemini-mullvad-disconnect-after
				'silently))
  (apply orig-fun args))

(advice-add 'gptel-curl-get-response :around #'gptel-extras-set-mullvad)

(declare-function org-insert-heading "org")
(declare-function org-next-visible-heading "org")
(defun gptel-extras-save-buffer (name _ _ _)
  "Save the `gptel' buffer with NAME right after it is created.
The buffer is saved to a file in `gptel-extras-dir'.

This function is meant to be an `:after' advice to `gptel'."
  (switch-to-buffer name)
  (let* ((extension (pcase major-mode
		      ('org-mode "org")
		      ('markdown-mode "md")
		      (_ (user-error "Unsupported major mode"))))
	 (filename (file-name-concat gptel-extras-dir
				     (file-name-with-extension (simple-extras-slugify name) extension))))
    (when (derived-mode-p 'org-mode)
      (goto-char (point-min))
      (org-insert-heading nil nil 1)
      (insert name)
      (org-next-visible-heading 1)
      (end-of-line))
    ;; we temporarily remove the hook because `gptel--save-state' throws an
    ;; error if called at this early stage
    (remove-hook 'before-save-hook #'gptel--save-state t)
    (write-file filename 'confirm)
    (add-hook 'before-save-hook #'gptel--save-state nil t)))

(declare-function org-latex-preview "org")
(defun gptel-extras-generate-latex-previews (_ _)
  "Generate LaTeX previews in the current `gptel' buffer."
  (when (string= default-directory gptel-extras-dir)
    (org-latex-preview)))

(provide 'gptel-extras)
;;; gptel-extras.el ends here
