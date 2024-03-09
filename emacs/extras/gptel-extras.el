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

;;;; Functions

;; adapted from the `:reader' lambda of `transient-infix-set' in `gptel-transient.el'
(defun gptel-extras-model-config (globally &optional backend-name model-name)
  "Configure `gptel' for BACKEND-NAME and MODEL-NAME.
By default, configure it for the current buffer. If GLOBALLY is non-nil, or
called with a prefix argument, configure it globally."
  (interactive "P")
  (let* ((backend-name (or backend-name
			   (if (<= (length gptel--known-backends) 1)
			       (caar gptel--known-backends)
			     (completing-read "Backend name: " (mapcar #'car gptel--known-backends) nil t))))
	 (backend (alist-get backend-name gptel--known-backends nil nil #'equal))
	 (backend-models (gptel-backend-models backend))
	 (model-name (or model-name
			 (if (= (length backend-models) 1)
			     (car backend-models)
			   (completing-read "Model name: " backend-models))))
	 (setter (if globally #'set-default #'set)))
    (funcall setter 'gptel-model model-name)
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
