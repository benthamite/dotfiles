;;; doom-modeline-extras.el --- Extensions for doom-modeline -*- lexical-binding: t -*-

;; Copyright (C) 2026

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

(defvar async-prompt-for-password)
(defvar doom-modeline--github-notification-number)
(defvar doom-modeline-after-github-fetch-notification-hook)
(defvar doom-modeline-before-github-fetch-notification-hook)
(defvar doom-modeline-github)
(defvar ghub-default-host-alist)
(declare-function async-inject-variables "async")
(declare-function async-start "async")
(declare-function ghub--token "ghub")
(declare-function ghub--username "ghub")
(declare-function ghub-get "ghub")

;;;; Variables

(defvar doom-modeline-extras-github-last-count 0
  "Last count of GitHub notifications.")

(defvar doom-modeline-extras--github-fetch-process nil
  "Child process of the in-flight GitHub notification count fetch, if any.")

(defvar doom-modeline-extras--github-fetch-started nil
  "Time at which the in-flight GitHub notification count fetch started.")

(defconst doom-modeline-extras--github-fetch-timeout 60
  "Seconds after which an in-flight GitHub count fetch is considered hung.")

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
  
;;;;;; AI session status

(declare-function agent--detect-backend "agents")
(declare-function agent-backend "agent")
(declare-function agent-backend-label "agent")
(declare-function agent-backend-icon-string "agents")
(declare-function agent-display-name "agents")
(declare-function agent-session "agent")
(declare-function agent-session-account "agent")
(declare-function agent-session-state-summary "agent")
(declare-function agent-claude-status-model "agent-claude")
(declare-function agent-claude-status-effort "agent-claude")
(declare-function agent-codex-status-model "agent-codex")
(declare-function agent-codex-status-effort "agent-codex")
(defvar agent-claude--status-data)

(doom-modeline-def-segment ai-session-status
  "Display AI session status for any registered backend.
Shows icon, bold session name, model, effort and account, followed by
the status the session's agent last published (the same text as the
last column of the session switcher)."
  (when (and doom-modeline-extras-claude-code
             (fboundp 'agent--detect-backend))
    (let ((backend (agent--detect-backend (current-buffer))))
      (when backend
        (let ((icon (agent-backend-icon-string backend 'mode-line-active))
              (name (agent-display-name (current-buffer))))
          (concat
           (doom-modeline-spc)
           (when (and icon (not (string-empty-p icon)))
             (concat icon " "))
           (propertize name
                       'face '(bold doom-modeline-buffer-major-mode)
                       'help-echo (format "%s session"
                                          (or (when-let* ((struct (agent-backend backend)))
                                                (agent-backend-label struct))
                                              "AI")))
           (pcase backend
             ('claude-code
              (when (bound-and-true-p agent-claude--status-data)
                (doom-modeline-extras--format-claude-status-fields)))
             ('codex
              (doom-modeline-extras--format-codex-status-fields)))
           (doom-modeline-extras--format-state-summary)
           (doom-modeline-spc)))))))

(defun doom-modeline-extras--format-claude-status-fields ()
  "Return Claude Code model, effort and account fields for the modeline."
  (doom-modeline-extras--format-session-fields
   (agent-claude-status-model) (agent-claude-status-effort)))

(defun doom-modeline-extras--format-codex-status-fields ()
  "Return Codex model, effort and account fields for the modeline."
  (doom-modeline-extras--format-session-fields
   (agent-codex-status-model) (agent-codex-status-effort)))

(defun doom-modeline-extras--format-session-fields (model effort)
  "Format MODEL, EFFORT and the current session's account for the modeline."
  (let ((account (when-let* ((session (agent-session)))
                   (agent-session-account session))))
    (concat
     (doom-modeline-extras--format-model model)
     (doom-modeline-extras--format-effort effort)
     (doom-modeline-extras--format-account account))))

(defun doom-modeline-extras--format-state-summary ()
  "Format the status the current session's agent last published.
Double each percent sign, which the mode line would otherwise treat as
a construct."
  (when-let* ((summary (agent-session-state-summary (current-buffer))))
    (concat " | " (propertize (string-replace "%" "%%" summary)
                              'help-echo "Latest published session status"))))

(defun doom-modeline-extras--format-model (model)
  "Format MODEL name with separator."
  (when model
    (concat " | " (propertize model 'help-echo "Model"))))

(defun doom-modeline-extras--format-effort (effort)
  "Format reasoning EFFORT with separator."
  (when effort
    (concat " | " (propertize effort 'help-echo "Reasoning effort"))))

(defun doom-modeline-extras--format-account (account)
  "Format ACCOUNT name with separator."
  (when account
    (concat " | " (propertize account
                               'face 'doom-modeline-buffer-path
                               'help-echo "Claude account"))))

;;;;; Modeline definitions

(doom-modeline-def-modeline 'ai-session
  '(bar ai-session-status)
  '(misc-info major-mode process time))

;;;;; GitHub notifications

(autoload 'forge-extras-pull-notifications "forge-extras")
(defun doom-modeline-extras-handle-github-notifications (&rest _)
  "Handle GitHub notifications after they are fetched.
When the GitHub notification count changes and is positive, trigger
a forge notification pull to keep forge in sync."
  (unless (= doom-modeline--github-notification-number doom-modeline-extras-github-last-count)
    (when (> doom-modeline--github-notification-number 0)
      (forge-extras-pull-notifications)))
  (setq doom-modeline-extras-github-last-count doom-modeline--github-notification-number))

(add-hook 'doom-modeline-after-github-fetch-notification-hook #'doom-modeline-extras-handle-github-notifications)

(autoload 'doom-modeline--github-fetch-notifications "doom-modeline-segments")

(defun doom-modeline-extras--github-fetch-notifications ()
  "Fetch the GitHub notification count without returning the payload."
  (when (and doom-modeline-github
             (require 'async nil t))
    (doom-modeline-extras--github-reap-fetch)
    (unless doom-modeline-extras--github-fetch-process
      (condition-case err
          (let ((async-prompt-for-password nil))
            (setq doom-modeline-extras--github-fetch-started (current-time)
                  doom-modeline-extras--github-fetch-process
                  (async-start (doom-modeline-extras--github-fetch-count-form)
                               #'doom-modeline-extras--github-fetch-finished)))
        (error
         (setq doom-modeline-extras--github-fetch-process nil
               doom-modeline-extras--github-fetch-started nil)
         (message "doom-modeline-extras: GitHub fetch failed: %S" err))))))

(defun doom-modeline-extras--github-reap-fetch ()
  "Discard the in-flight GitHub count fetch if its child died or hung.
`async-when-done' never invokes the completion callback when the child
exits abnormally, and a child blocked on synchronous DNS resolution can
outlive its own timeout, so a fetch that never reports back would
otherwise disable all future fetches."
  (when-let* ((proc doom-modeline-extras--github-fetch-process))
    (cond ((not (process-live-p proc))
           (doom-modeline-extras--github-discard-fetch proc))
          ((and doom-modeline-extras--github-fetch-started
                (> (float-time (time-since doom-modeline-extras--github-fetch-started))
                   doom-modeline-extras--github-fetch-timeout))
           (delete-process proc)
           (doom-modeline-extras--github-discard-fetch proc)))))

(defun doom-modeline-extras--github-discard-fetch (proc)
  "Forget the GitHub count fetch PROC and kill its leftover buffer."
  (when (buffer-live-p (process-buffer proc))
    (kill-buffer (process-buffer proc)))
  (setq doom-modeline-extras--github-fetch-process nil
        doom-modeline-extras--github-fetch-started nil))

(defun doom-modeline-extras--github-fetch-count-form ()
  "Return an async child form that fetches the GitHub notification count."
  `(lambda ()
     ,(async-inject-variables
       "\\`\\(load-path\\|auth-sources\\|doom-modeline-before-github-fetch-notification-hook\\)\\'")
     (run-hooks 'doom-modeline-before-github-fetch-notification-hook)
     (if (require 'ghub nil t)
         (condition-case nil
             (with-timeout (10 0)
               (if-let* ((host (alist-get 'github ghub-default-host-alist))
                         (username (ghub--username host))
                         (token (or (ghub--token host username 'forge t)
                                    (ghub--token host username 'ghub t))))
                   (length
                    (ghub-get "/notifications"
                              '((notifications . t))
                              :host host
                              :username username
                              :auth token
                              :unpaginate t
                              :noerror t))
                 0))
           (error 0))
       0)))

(defun doom-modeline-extras--github-fetch-finished (result)
  "Store GitHub notification count RESULT and run follow-up hooks."
  (setq doom-modeline-extras--github-fetch-process nil
        doom-modeline-extras--github-fetch-started nil)
  (message "")
  (setq doom-modeline--github-notification-number
        (if (natnump result) result 0))
  (run-hooks 'doom-modeline-after-github-fetch-notification-hook))

(defun doom-modeline-extras--safe-default-directory (orig-fn &rest args)
  "Call ORIG-FN with ARGS, ensuring `default-directory' exists.
`async-start' inherits `default-directory' as the working directory
for the subprocess.  When a timer fires while the current buffer's
directory has been deleted, `make-process' signals `file-missing'.
Fall back to the user's home directory in that case."
  (let ((default-directory (if (file-directory-p default-directory)
                               default-directory
                             "~/")))
    (apply orig-fn args)))

(advice-add 'doom-modeline--github-fetch-notifications :around
            #'doom-modeline-extras--safe-default-directory)

(advice-add 'doom-modeline--github-fetch-notifications :override
            #'doom-modeline-extras--github-fetch-notifications)

(defun doom-modeline-extras-refresh-github-after-forge (&rest _)
  "Refresh the doom-modeline GitHub count after forge pulls notifications.
This ensures the tab-bar count updates immediately when notifications
are pulled via forge, rather than waiting for the next timer cycle."
  (when (bound-and-true-p doom-modeline-github)
    (doom-modeline--github-fetch-notifications)))

(advice-add 'forge--ghub-update-notifications :after
            #'doom-modeline-extras-refresh-github-after-forge)

(provide 'doom-modeline-extras)
;;; doom-modeline-extras.el ends here
