;;; gptel-extras.el --- Extensions for gptel -*- lexical-binding: t -*-

;; Copyright (C) 2023

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

;;;; User options

(defgroup gptel-extras ()
  "Extensions for `gptel'."
  :group 'gptel-extras)

;;;; Variables

(defvar gptel-extras-gemini-pro-backend-plist
  `(:key ,(auth-source-pass-get 'secret (concat "tlon/core/makersuite.google.com/" tlon-core-email-shared))
	 :stream t)
  "Parameters for creating a Gemini Pro backend.")

(defvar gptel-extras-gemini-pro-backend
  (apply #'gptel-make-gemini "Gemini" gptel-extras-gemini-pro-backend-plist)
  "Backend for `gptel' when using the Gemini Pro model.")

(defvar gptel-extras-backends
  `(("gpt-4" . ,gptel--openai)
    ("gemini-pro" . ,gptel-extras-gemini-pro-backend))
  "List of backends for `gptel'.")

;;;; Functions

(defun gptel-extras-model-config (model)
  "Configure `gptel' for MODEL.
For Gemini, a VPN will be used to circumvent location restrictions."
  (interactive (list (completing-read "Model: " gptel-extras-backends nil t)))
  (require 'mullvad)
  (setq gptel-model model
	gptel-backend (alist-get model gptel-extras-backends nil nil #'string=))
  (when (string= model "gemini-pro")
    (mullvad-connect-to-website "Gemini" "1")))

(provide 'gptel-extras)
;;; gptel-extras.el ends here