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
(require 'gptel-gemini)
(require 'mullvad)
(require 'tlon-core)

;;;; User options

(defgroup gptel-extras ()
  "Extensions for `gptel'."
  :group 'gptel)

(defcustom gptel-extras-gemini-mullvad-disconnect-after 1
  "The number of minutes to disconnect `mullvad' after starting the Gemini session."
  :type 'integer
  :group 'gptel-extras)

;;;; Variables

(defvar tlon-core-email-shared)
(defvar gptel-extras-gemini-pro-backend-plist
  `(:key ,(auth-source-pass-get 'secret
				(concat "tlon/core/makersuite.google.com/" tlon-core-email-shared))
	 :stream t)
  "Parameters for creating a Gemini Pro backend.")

(defvar gptel-extras-gemini-pro-backend
  (apply #'gptel-make-gemini "Gemini" gptel-extras-gemini-pro-backend-plist)
  "`gptel' backend when using the Gemini Pro model.")

(defvar gptel-extras-backends
  `(("gpt-4" . ,gptel--openai)
    ("gemini-pro" . ,gptel-extras-gemini-pro-backend))
  "List of for `gptel' backends.")

;;;; Functions

(defun gptel-extras-model-config (model &optional globally)
  "Configure `gptel' for MODEL.
By default, configure MODEL for the current buffer. If GLOBALLY is non-nil,
configure it globally."
  (interactive (list (completing-read "Model: " gptel-extras-backends nil t)))
  (let ((setter (if globally #'set-default #'set))
	(backend (alist-get model gptel-extras-backends nil nil #'string=)))
    (funcall setter 'gptel-model model)
    (funcall setter 'gptel-backend backend)))

(defun gptel-extras-set-mullvad (orig-fun &rest args)
  "Enable `mullvad' when connecting to Gemini, then call ORIG-FUN with ARGS."
  (when (string= gptel-model "gemini-pro")
    (mullvad-connect-to-website "Gemini"
				gptel-extras-gemini-mullvad-disconnect-after
				'silently))
  (apply orig-fun args))

(advice-add 'gptel-curl-get-response :around #'gptel-extras-set-mullvad)

(provide 'gptel-extras)
;;; gptel-extras.el ends here
