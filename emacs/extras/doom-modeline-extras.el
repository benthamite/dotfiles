;;; doom-modeline-extras.el --- Extensions for doom-modeline -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/doom-modeline-extras.el
;; Version: 0.2
;; Package-Requires: ((doom-modeline "2.0"))

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

;; Extensions for `doom-modeline'.

;;; Code:

(require 'doom-modeline)

;;;; Variables

(defvar doom-modeline-extras-github-last-count 0
  "Last count of GitHub notifications.")

;;;; User options

(defgroup doom-modeline-extras ()
  "Extensions for `doom-modeline'."
  :group 'doom-modeline)

(defcustom doom-modeline-extras-org-roam t
  "Whether to display the `org-roam' backlink count in the modeline."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-extras-tlon-split t
  "Whether to display if `tlon-split-mode' is active in the modeline."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-extras-ai-context t
  "Whether to display the number of files in the AI context in the modeline."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-extras-claude-code t
  "Whether to display Claude Code status in the modeline."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-extras-claude-code-api-plan nil
  "Whether using an API plan for Claude Code.
When nil (the default), assumes a subscription plan (Free, Pro,
or Max) and displays the cost with strikethrough to indicate no
charge.  When non-nil, displays the cost normally."
  :type 'boolean
  :group 'doom-modeline)

;;;; Faces

(defface doom-modeline-extras-cost-free
  '((t :inherit doom-modeline :strike-through t))
  "Face for cost display on non-API (subscription) plans.
Uses strikethrough to indicate the cost is not actually charged."
  :group 'doom-modeline-extras)

;;;; Functions

;;;;; Modeline segments

;;;;;; tlon

(defvar tlon-split-mode)
(doom-modeline-def-segment tlon-split ()
  "Display \"split\" in the modeline when `tlon-split-mode' is enabled."
  (when (and doom-modeline-extras-tlon-split
	     (bound-and-true-p tlon-split-mode))
    (concat "split" (doom-modeline-spc))))

(doom-modeline-def-segment tlon-paragraph
  (when (and (featurep 'tlon-paragraphs)
             (bound-and-true-p tlon-paragraphs-mode-line-mode)
             (fboundp 'tlon-paragraphs-mode-line-string))
    (tlon-paragraphs-mode-line-string)))

;;;;;; org-roam

;; FIXME (2025-07-16): this stopped working
(defvar org-roam-extras-current-backlink-count)
(doom-modeline-def-segment org-roam-backlinks
  (when (and (derived-mode-p 'org-mode)
	     doom-modeline-extras-org-roam
	     (bound-and-true-p org-roam-extras-current-backlink-count))
    (concat (doom-modeline-spc) (doom-modeline-spc)
	    (format "⟲(%s)" org-roam-extras-current-backlink-count))))

;;;;;; AI context

(defvar gptel-context--alist)
(doom-modeline-def-segment ai-context
  "Display the number of files in the AI context."
  (let ((count (length gptel-context--alist)))
    (when (and doom-modeline-extras-ai-context
               (> count 0))
      (concat (doom-modeline-spc)
              (format "✨(%s)" count)))))
  
;;;;;; Claude Code status

(declare-function claude-code--buffer-p "claude-code")
(declare-function claude-code-extras-status-model "claude-code-extras")
(declare-function claude-code-extras-status-cost "claude-code-extras")
(declare-function claude-code-extras-status-context-percent "claude-code-extras")
(declare-function claude-code-extras-status-token-count "claude-code-extras")
(declare-function claude-code-extras-status-lines-added "claude-code-extras")
(declare-function claude-code-extras-status-lines-removed "claude-code-extras")
(declare-function claude-code-extras-status-duration-ms "claude-code-extras")
(declare-function claude-code-extras-status-cache-read-tokens "claude-code-extras")
(declare-function claude-code-extras-status-cache-total-tokens "claude-code-extras")
(declare-function claude-code-extras-alert-indicator "claude-code-extras")
(declare-function claude-code-extras--session-name "claude-code-extras")
(defvar claude-code-extras--status-data)

(doom-modeline-def-segment claude-code-status
  "Display Claude Code session status: model, tokens, cost, context%."
  (when (and doom-modeline-extras-claude-code
             (fboundp 'claude-code--buffer-p)
             (claude-code--buffer-p (current-buffer)))
    (if (bound-and-true-p claude-code-extras--status-data)
        (doom-modeline-extras--format-claude-status)
      (concat (doom-modeline-spc)
              (claude-code-extras--session-name (buffer-name))
              (doom-modeline-spc)))))

(defun doom-modeline-extras--format-claude-status ()
  "Assemble the Claude Code modeline string from status data."
  (let ((model (claude-code-extras-status-model))
        (tokens (claude-code-extras-status-token-count))
        (cost (claude-code-extras-status-cost))
        (pct (claude-code-extras-status-context-percent))
        (added (claude-code-extras-status-lines-added))
        (removed (claude-code-extras-status-lines-removed))
        (duration (claude-code-extras-status-duration-ms))
        (cache-read (claude-code-extras-status-cache-read-tokens))
        (cache-total (claude-code-extras-status-cache-total-tokens)))
    (concat
     (doom-modeline-spc)
     (propertize (claude-code-extras--session-name (buffer-name))
                 'face '(bold doom-modeline-buffer-major-mode))
     (doom-modeline-extras--format-model model)
     (doom-modeline-extras--format-tokens tokens)
     (doom-modeline-extras--format-cost cost)
     (doom-modeline-extras--format-context-percent pct)
     (doom-modeline-extras--format-lines-changed added removed)
     (doom-modeline-extras--format-duration duration)
     (doom-modeline-extras--format-cache-efficiency cache-read cache-total)
     " | " (claude-code-extras-alert-indicator)
     (doom-modeline-spc))))

(defun doom-modeline-extras--format-model (model)
  "Format MODEL name with separator."
  (when model
    (concat " | " model)))

(defun doom-modeline-extras--format-tokens (tokens)
  "Format TOKENS as a human-readable string with separator."
  (when (and (numberp tokens) (> tokens 0))
    (concat " | " (doom-modeline-extras--humanize-tokens tokens))))

(defun doom-modeline-extras--humanize-tokens (n)
  "Format token count N in a compact human-readable form."
  (cond
   ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
   ((>= n 1000) (format "%.1fk" (/ n 1000.0)))
   (t (format "%d" n))))

(defun doom-modeline-extras--format-cost (cost)
  "Format COST as a dollar amount with separator.
On subscription plans, applies strikethrough to indicate no charge."
  (when (and (numberp cost) (> cost 0))
    (let ((text (format "$%.2f" cost)))
      (concat " | "
              (if doom-modeline-extras-claude-code-api-plan
                  text
                (propertize text
                            'face 'doom-modeline-extras-cost-free))))))

(defun doom-modeline-extras--format-context-percent (pct)
  "Format context usage PCT with color coding and separator.
Uses %%%% to produce a literal %% in the mode-line, since %%
is a mode-line escape character."
  (when (and (numberp pct) (> pct 0))
    (concat " | "
            (propertize (format "%d%%%%" pct)
                        'face (doom-modeline-extras--context-face pct)))))

(defun doom-modeline-extras--context-face (pct)
  "Return the face for context usage percentage PCT."
  (cond
   ((>= pct 80) 'doom-modeline-urgent)
   ((>= pct 60) 'doom-modeline-warning)
   (t 'doom-modeline-info)))

(defun doom-modeline-extras--format-lines-changed (added removed)
  "Format ADDED and REMOVED line counts as a +N/-M string with separator."
  (when (and (numberp added) (numberp removed)
             (> (+ added removed) 0))
    (concat " | "
            (propertize (format "+%d" added) 'face 'doom-modeline-info)
            "/"
            (propertize (format "-%d" removed) 'face 'doom-modeline-urgent))))

(defun doom-modeline-extras--format-duration (ms)
  "Format duration MS (in milliseconds) as a human-readable string."
  (when (and (numberp ms) (> ms 0))
    (concat " | " (doom-modeline-extras--humanize-duration ms))))

(defun doom-modeline-extras--humanize-duration (ms)
  "Format MS milliseconds in a compact human-readable form."
  (let ((secs (/ ms 1000)))
    (cond
     ((>= secs 3600) (format "%dh%dm" (/ secs 3600) (/ (mod secs 3600) 60)))
     ((>= secs 60) (format "%dm%ds" (/ secs 60) (mod secs 60)))
     (t (format "%ds" secs)))))

(defun doom-modeline-extras--format-cache-efficiency (cache-read cache-total)
  "Format cache efficiency as a percentage from CACHE-READ and CACHE-TOTAL."
  (when (and (numberp cache-read) (numberp cache-total) (> cache-total 0))
    (let ((pct (/ (* 100 cache-read) cache-total)))
      (when (> pct 0)
        (concat " | "
                (propertize (format "cache %d%%%%" pct)
                            'face (doom-modeline-extras--cache-face pct)))))))

(defun doom-modeline-extras--cache-face (pct)
  "Return the face for cache efficiency percentage PCT."
  (cond
   ((>= pct 80) 'doom-modeline-info)
   ((>= pct 50) 'doom-modeline-warning)
   (t 'doom-modeline-urgent)))

;;;;; Modeline definitions

(doom-modeline-def-modeline 'claude-code
  '(bar claude-code-status)
  '(misc-info major-mode process time))

;;;;; GitHub notifications

(declare-function forge-pull-notifications "forge-commands")
(defun doom-modeline-extras-handle-github-notifications (&rest _)
  "Handle GitHub notifications after they are fetched."
  (unless (= doom-modeline--github-notification-number doom-modeline-extras-github-last-count)
    (when (> doom-modeline--github-notification-number 0)
      (forge-pull-notifications)
      (message "Pulled forge notifications.")))
  (setq doom-modeline-extras-github-last-count doom-modeline--github-notification-number))

(add-hook 'doom-modeline-after-github-fetch-notification-hook #'doom-modeline-extras-handle-github-notifications)

(provide 'doom-modeline-extras)
;;; doom-modeline-extras.el ends here
