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

(defvar gptel-extras-gemini-pro-backend
  (gptel-make-gemini
   "Gemini"
   :key (auth-source-pass-get 'secret (concat "tlon/core/makersuite.google.com/" tlon-core-email-shared))
   :stream t)
  "Backend for `gptel' when using the Gemini Pro model.")

(defvar gptel-extras-gemini-pro-no-stream-backend
  (gptel-make-gemini
   "Gemini"
   :key (auth-source-pass-get 'secret (concat "tlon/core/makersuite.google.com/" tlon-core-email-shared))
   :stream nil)
  "Backend for `gptel' when using the Gemini Pro model with `:stream' set to nil.")

;;;; Functions

(defun gptel-model-config (model)
  "Configure `gptel' for MODEL."
  (interactive (list (completing-read "Model: " '("gpt-4"
						  "gemini-pro"
						  "gemini-pro-no-stream")
				      nil t)))
  (pcase model
    ("gpt-4" (setq gptel-model "gpt-4"
		   gptel-backend gptel--openai))
    ("gemini-pro" (setq gptel-model "gemini-pro"
			gptel-backend gptel-extras-gemini-pro-backend))
    ("gemini-pro-no-stream" (setq gptel-model "gemini-pro-no-stream"
				  gptel-backend gptel-extras-gemini-pro-no-stream-backend))))

(provide 'gptel-extras)
;;; gptel-extras.el ends here
