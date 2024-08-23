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
	      :model "gemini-1.0-pro"
	      :cost 0.50
	      :tokens 32000
	      :description "Gemini’s first-generation model offering only text and image reasoning. Generally available for production use.")
    (:backend "Gemini"
	      :model "gemini-1.5-pro"
	      :cost 3.50
	      :tokens 128000
	      :description "Gemini’s next-generation model with a breakthrough 1 million context window. Now generally available for production use.")
    (:backend "Gemini"
	      :model "gemini-1.5-flash"
	      :cost 0.35
	      :tokens 1000000
	      :description "Gemini’s fastest multimodal model with great performance for diverse, repetitive tasks and a 1 million context window. Now generally available for production use. For requests about 128k tokens, the price is $0.70 per 1 million tokens.")
    (:backend "ChatGPT"
	      :model "gpt-3.5-turbo"
	      :cost 0.50
	      :tokens 16385
	      :last-update "2021-09-01"
	      :description "Less powerful GPT model. Currently points to `gpt-3.5-turbo-0125'.")
    (:backend "ChatGPT"
	      :model "gpt-3.5-turbo-16k"
	      :cost 0.50
	      :tokens 16385
	      :last-update "2021-09-01"
	      :description "Less powerful GPT model. Currently points to `gpt-3.5-turbo-16k-0613'.")
    (:backend "ChatGPT"
	      :model "gpt-4"
	      :cost 30
	      :tokens 8192
	      :last-update "2021-09-01"
	      :description "Currently points to `gpt-4-0613'. ")
    (:backend "ChatGPT"
	      :model "gpt-4o"
	      :cost 5
	      :tokens 128000
	      :last-update "2023-10-01"
	      :description "[Recommended] The most advanced multimodal model that’s faster and cheaper than GPT-4 Turbo with stronger vision capabilities.")
    (:backend "ChatGPT"
	      :model "gpt-4o-mini"
	      :cost 0.15
	      :tokens 128000
	      :last-update "2023-10-01"
	      :description "Affordable and intelligent small model for fast, lightweight tasks.")
    (:backend "ChatGPT"
	      :model "gpt-4-turbo"
	      :cost 10
	      :tokens 128000
	      :last-update "2023-12-01"
	      :description "[Recommended] The latest GPT-4 Turbo model with vision capabilities. Vision requests can now use JSON mode and function calling. Currently points to gpt-4-turbo-2024-04-09.")
    (:backend "ChatGPT"
	      :model "gpt-4-turbo-preview"
	      :cost 10
	      :tokens 128000
	      :last-update "2023-12-01"
	      :description "GPT-4 Turbo preview model. Currently points to `gpt-4-0125-preview'.")
    (:backend "ChatGPT"
	      :model "gpt-4-32k"
	      :cost 60
	      :tokens 32768
	      :last-update "2021-09-01"
	      :description "Currently points to gpt-4-32k-0613. See continuous model upgrades. This model was never rolled out widely in favor of GPT-4 Turbo.")
    (:backend "ChatGPT"
	      :model "gpt-4-1106-preview"
	      :cost 10
	      :tokens 128000
	      :last-update "2023-04-01"
	      :description "GPT-4 Turbo preview model featuring improved instruction following, JSON mode, reproducible outputs, parallel function calling, and more. Returns a maximum of 4,096 output tokens. This is a preview model.")
    (:backend "ChatGPT"
	      :model "gpt-4-0125-preview"
	      :cost 10
	      :tokens 128000
	      :last-update "2023-12-01"
	      :description "GPT-4 Turbo preview model intended to reduce cases of “laziness” where the model doesn’t complete a task. Returns a maximum of 4,096 output tokens.")
    (:backend "Claude"
	      :model "claude-3-haiku-20240307"
	      :tokens 200000
	      :cost 0.25
	      :last-update "2023-08-01"
	      :description "Fastest, most cost-effective model.")
    (:backend "Claude"
	      :model "claude-3-sonnet-20240229"
	      :tokens 200000
	      :cost 3
	      :last-update "2023-08-01"
	      :description "Legacy model.")
    (:backend "Claude"
	      :model "claude-3-opus-20240229"
	      :tokens 200000
	      :cost 15
	      :last-update "2023-08-01"
	      :description "Powerful model for complex tasks.")
    (:backend "Claude"
	      :model "claude-3-5-sonnet-20240620"
	      :tokens 200000
	      :cost 3
	      :last-update "2023-08-01"
	      :description "[recommended] The most capable Anthropic model.")
    (:backend "Perplexity"
	      :model "llama-3.1-sonar-small-128k-online"
	      :tokens 127072
	      :cost 0.2
	      :last-update ""
	      :description "")
    (:backend "Perplexity"
	      :model "llama-3.1-sonar-large-128k-online"
	      :tokens 127072
	      :cost 1
	      :last-update ""
	      :description "")
    (:backend "Perplexity"
	      :model "llama-3.1-sonar-huge-128k-online"
	      :tokens 127072
	      :cost 5
	      :last-update ""
	      :description "[recommended]")
    (:backend "Perplexity"
	      :model "llama-3.1-sonar-small-128k-chat"
	      :tokens 131072
	      :cost 0.2
	      :last-update ""
	      :description "")
    (:backend "Perplexity"
	      :model "llama-3.1-sonar-large-128k-chat"
	      :tokens 131072
	      :cost 1
	      :last-update ""
	      :description "")
    (:backend "Perplexity"
	      :model "llama-3.1-8b-instruct"
	      :tokens 131072
	      :cost 0.2
	      :last-update ""
	      :description "")
    (:backend "Perplexity"
	      :model "llama-3.1-70b-instruct"
	      :tokens 131072
	      :cost 1
	      :last-update ""
	      :description ""))
  "Alist of AI models and associated properties.
- `:tokens': the context window size in tokens.

- `:cost': the cost per 1 million tokens in US dollars.

The relevant information has been obtained from the following websites:

- GPT: <https://openai.com/pricing> and
<https://platform.openai.com/docs/models/gpt-4-turbo-and-gpt-4>.

- Claude: <https://www.anthropic.com/api#pricing>.

- Gemini: <https://ai.google.dev/pricing>")

;;;; Functions

(defvar gptel-extras-ai-models)
(declare-function tlon-lookup "tlon-core")
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
				     (cons (format "%-40s  $ %5.2f  %8s  %6s  %-80s"
						   backend
						   (tlon-lookup gptel-extras-ai-models :cost :model backend)
						   (tlon-lookup gptel-extras-ai-models :last-update :model backend)
						   (tlon-lookup gptel-extras-ai-models :tokens :model backend)
						   (tlon-lookup gptel-extras-ai-models :description :model backend))
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
  "Get the cost of prompting the current model.
This is used to display the relevant information in the modeline (see
`doom-modeline-extras')."
  (let* ((cost-per-1m (tlon-lookup gptel-extras-ai-models :cost :model gptel-model))
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

(declare-function org-insert-heading "org")
(declare-function org-next-visible-heading "org")
(defun gptel-extras-save-buffer (name _ _ _)
  "Save the `gptel' buffer with NAME right after it is created.
The buffer is save to a file in `gptel-extras-dir'.

This function is meant to be an `:after' advice to `gptel-extras-save-buffer'."
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

(advice-add 'gptel :after #'gptel-extras-save-buffer)

(provide 'gptel-extras)
;;; gptel-extras.el ends here
