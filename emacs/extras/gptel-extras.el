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

;;;; Variables

(defconst gptel-extras-ai-models
  '((:backend "Gemini"
	      :model "gemini-pro"
	      :cost 0
	      :tokens 32000
	      :description "This model is currently free (included in Pablo’s Google Drive plan).")
    (:backend "Gemini"
	      :model "gemini-1.5-pro-latest"
	      :cost 0
	      :tokens 128000
	      :description "[recommended] This model is currently free (included in Pablo’s Google Drive plan).")
    (:backend "ChatGPT"
	      :model "gpt-3.5-turbo"
	      :cost 0.50
	      :description "Less powerful GPT model.")
    (:backend "ChatGPT"
	      :model "gpt-3.5-turbo-16k"
	      :cost 0.50
	      :description "Less powerful GPT model.")
    (:backend "ChatGPT"
	      :model "gpt-4"
	      :cost 10
	      :description "[recommended] With 128k context, fresher knowledge and the broadest set of capabilities, GPT-4 Turbo is more powerful than GPT-4 and offered at a lower price. Currently points to `gpt-4-turbo-2024-04-09'.")
    (:backend "ChatGPT"
	      :model "gpt-4-turbo-preview"
	      :cost 10
	      :description "Currently points to `gpt-4-0125-preview'.") ; cost assumed
    (:backend "ChatGPT"
	      :model "gpt-4-1106-preview"
	      :cost 10
	      :tokens 128000
	      :description "GPT-4 Turbo model featuring improved instruction following, JSON mode, reproducible outputs, parallel function calling, and more. Returns a maximum of 4,096 output tokens. Training data up to April 2023.")
    (:backend "ChatGPT"
	      :model "gpt-4-0125-preview"
	      :cost 10
	      :description "Returns a maximum of 4,096 output tokens. Training data up to December 2023.")
    (:backend "ChatGPT"
	      :model "gpt-4"
	      :cost 30
	      :tokens 8000
	      :description "Standard GPT-4 model.")
    (:backend "ChatGPT"
	      :model "gpt-4-32k"
	      :cost 60
	      :tokens 32000
	      :description "Standard GPT-4 with 32k token window.")
    (:backend "ChatGPT"
	      :model "gpt-4-vision-preview"
	      :cost 0 ; placeholder
	      :description "GPT-4 model for sending images.")
    (:backend "Claude"
	      :model "claude-3-haiku-20240307"
	      :cost 0.25
	      :description "The least capable Anthropic model")
    (:backend "Claude"
	      :model "claude-3-sonnet-20240229"
	      :cost 3
	      :description "The intermediate Anthropic model.")
    (:backend "Claude"
	      :model "claude-3-opus-20240229"
	      :cost 15
	      :description "The most capable Anthropic model."))
  "Alist of AI models and input cost in US dollars per one million tokens.
The pricing information has been obtained from the following websites:
- GPT: <https://openai.com/pricing>
- Claude: <https://www.anthropic.com/api#pricing>")
;; https://github.com/psimm/website/blob/master/blog/llm-price-performance/data.csv

;;;; Functions

(defvar gptel-extras-ai-models)
(declare-function tlon-babel-lookup "tlon-babel-core")
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
	 (backend (alist-get backend-name gptel--known-backends nil nil #'string=))
	 (backend-models (gptel-backend-models backend))
	 (models-with-cost (mapcar (lambda (backend)
				     (cons (format "%-25s $ %5.2f   %-80s"
						   backend
						   (tlon-babel-lookup gptel-extras-ai-models :cost :model backend)
						   (tlon-babel-lookup gptel-extras-ai-models :description :model backend))
					   backend))
				   backend-models))
	 (model-name (or model-name
			 (alist-get (completing-read "Model name: " models-with-cost)
				    models-with-cost nil nil #'string=))))
    (if globally
	(setq gptel-model model-name
	      gptel-backend backend)
      (setq-local gptel-model model-name
		  gptel-backend backend))))

(defun gptel-extras-get-cost ()
  "Get the cost of prompting the current model."
  (let* ((cost-per-1m (tlon-babel-lookup gptel-extras-ai-models :cost :model gptel-model))
	 (words (if (region-active-p)
		    (count-words (region-beginning) (region-end))
		  (count-words (point-min) (point))))
	 (cost (/ (* cost-per-1m words) 1000000.0)))
    cost))

(defvar gptel-extras-gemini-mullvad-disconnect-after)
(declare-function mullvad-connect-to-website "mullvad")
(defun gptel-extras-set-mullvad (orig-fun &rest args)
  "Enable `mullvad' when connecting to Gemini, then call ORIG-FUN with ARGS."
  (when (string= gptel-model "gemini-pro")
    (mullvad-connect-to-website "Gemini"
				gptel-extras-gemini-mullvad-disconnect-after
				'silently))
  (apply orig-fun args))

(advice-add 'gptel-curl-get-response :around #'gptel-extras-set-mullvad)

(defun gptel-extras-save-buffer (name)
  "Save the `gptel' buffer NAME to a file in the appropriate directory.
The `gptel' directory is set by `gptel-extras-dir'."
  (interactive (list (read-string "Name: ")))
  (let* ((extension (pcase major-mode
		      ('org-mode "org")
		      ('markdown-mode "md")
		      (_ (error "Unsupported major mode"))))
	 (filename (file-name-concat gptel-extras-dir
				     (file-name-with-extension (simple-extras-slugify name) extension))))
    (write-file filename)))

(provide 'gptel-extras)
;;; gptel-extras.el ends here
