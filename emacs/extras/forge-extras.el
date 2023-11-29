;;; forge-extras.el --- Extensions for forge -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/forge-extras.el
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

;; Extensions for `forge'.

;;; Code:

(require 'forge)
(require 'orgit-forge)
(require 'gh-notify)

;;;; Functions

(transient-define-prefix forge-extras-dispatch ()
  "Dispatch a forge command."
  [["Fetch"
    ("f f" "all topics"             forge-pull)
    ("f t" "one topic"              forge-pull-topic)
    ("f n" "notifications"          forge-pull-notifications)
    """Create"
    ("c i" "issue"                  forge-create-issue)
    ("c p" "pull-request"           forge-create-pullreq)
    ("c u" "pull-request from issue" forge-create-pullreq-from-issue
     :if (lambda () (forge-github-repository-p (forge-get-repository nil))))
    ("c f" "fork or remote"        forge-fork)
    ]
   ["List"
    ("l a" "awaiting review"        forge-list-requested-reviews)
    ("l i" "issues"                 forge-list-issues)
    ("l n" "notifications"          forge-list-notifications)
    ("l g" "notifications (GHub)"   gh-notify)
    ("l p" "pull-requests"          forge-list-pullreqs)
    ("l t" "topics"                 forge-list-topics)
    ("l r" "repositories"           forge-list-repositories)
    """Edit"
    ("e t" "edit title"             forge-edit-topic-title)
    ("e s" "edit state"             forge-edit-topic-state)
    ("e m" "edit milestone"         forge-edit-topic-milestone)
    ("e l" "edit labels"            forge-edit-topic-labels)
    ]
   ["Browse"
    ("b i" "issue"                  forge-browse-issue)
    ("b p" "pull-request"           forge-browse-pullreq)
    ("b r" "remote"                 forge-browse-remote)
    ("b t" "topic"                  forge-browse-topic)
    ("b I" "issues"                 forge-browse-issues)
    ("b P" "pull-requests"          forge-browse-pullreqs)
    """Visit"
    ("v i" "issue"                  forge-visit-issue)
    ("v p" "pull-request"           forge-visit-pullreq)
    ("v t" "topic"                  forge-visit-topic)
    ]
   ["Authored"
    ("u i" "authored issues"        forge-list-authored-issues)
    ("u p" "authored pull-requests" forge-list-authored-pullreqs)
    """Owned"
    ("o i" "owned issues"           forge-list-owned-issues)
    ("o p" "owned pull-requests"    forge-list-owned-pullreqs)]
   ["Assigned"
    ("i i" "assigned issues"        forge-list-assigned-issues)
    ("i p" "assigned pull-requests" forge-list-assigned-pullreqs)
    """Labeled"
    ("d i" "labeled issues"         forge-list-labeled-issues)
    ("d p" "labeled pull-requests"  forge-list-labeled-pullreqs)
    ("e a" "edit assigness"         forge-edit-topic-assignees)]
   ["Misc"
    ("s" "search topics"            forge-search)
    (";" "Show/hide closed topics" forge-toggle-closed-visibility)
    ]
   ]
  )

(advice-add 'forge-dispatch :override #'forge-extras-dispatch)

(defun forge-extras-orgit-store-link (_arg)
  "Like `org-store-link' but store links to all selected commits, if any."
  (interactive "P")
  (if-let ((sections (magit-region-sections 'commit)))
      (save-excursion
        (dolist (section sections)
          (goto-char (oref section start))
          (set-mark (point))
          (activate-mark)
          (call-interactively #'org-store-link))
        (deactivate-mark))
    (save-window-excursion
      (let ((topic (forge-topic-at-point)))
        (cond ((forge-pullreq-p topic)
               (forge-visit-pullreq topic))
              ((forge-issue-p topic)
               (forge-visit-issue topic)))
        (call-interactively #'org-store-link)))))

;; TODO: check that this is programmed correctly
(defun forge-extras-gh-notify-visit-next-notification ()
  "Visit the next notification and mark it as read."
  (interactive)
  (let ((old-buffer (current-buffer))
        (old-window (selected-window)))
    (forward-line)
    (call-interactively 'gh-notify-visit-notification)
    (select-window old-window)
    (switch-to-buffer old-buffer)))

;;;;; gh-notify

;; the code below is a workaround to make `gh-notify' mark issues as read when
;; they are visited. It assumes that you have authenticated with GitHub
;; using the `w3m' browser. If not, please evaluate
;; `(w3m "https://github.com/login")' and enter your credentials.

(defun forge-extras-get-issue-url ()
  "Get the URL of the issue at point."
  (unless (derived-mode-p 'forge-issue-mode)
    (user-error "Not in `forge-issue-mode'"))
  (forge-get-url forge-buffer-topic))

(defun forge-extras-mark-issue-as-read ()
  "Mark issue at point as read on GitHub."
  (require 'w3m)
  (if (derived-mode-p 'forge-issue-mode)
      (save-window-excursion
	(let ((url (forge-extras-get-issue-url))
	      (w3m-new-session-in-background t)
	      (inhibit-message t))
	  (w3m-goto-url-new-session url nil nil nil nil t)))
    (run-with-timer 1 nil 'forge-extras-mark-issue-as-read)))

(defun forge-extras-delete-residual-w3m-buffers (&rest _)
  "Delete all `w3m' buffers minus one."
  (require 'w3m)
  (let ((w3m-buffers (w3m-list-buffers)))
    (while (> (length w3m-buffers) 1)
      (setq w3m-buffers (cdr w3m-buffers))
      (when (buffer-live-p (car w3m-buffers))
        (with-current-buffer (car w3m-buffers)
          (w3m-delete-buffer))))))

(defun forge-extras-w3m-after-load (original-func format-string &rest args)
  "Functions to trigger when `w3m' is done loading.
ORIGINAL-FUNC, FORMAT-STRING and ARGS are passed to the advised function."
  (require 'w3m)
  (let ((message-text (apply 'format format-string args)))
    (when (or (string-match "The content (\\(.*\\)) has been retrieved in \\(.*\\)" message-text)
	      (string-match "fontifying...done" message-text))
      (message "")
      (forge-pull-notifications)
      (when (featurep 'doom-modeline)
	(doom-modeline--github-fetch-notifications)))
    (apply original-func format-string args)))

;; TODO: restrict the scope of this so that it doesn’t conflict with normal uses
;; of `w3m'
(defun forge-extras-rename-w3m-buffers ()
  "Rename w3m buffers to be hidden."
  (when (string-match "^\\*w3m*" (buffer-name))
    (rename-buffer (concat " " (buffer-name)) t)))

(add-hook 'buffer-list-update-hook 'rename-w3m-buffers)
(advice-add 'w3m-message :around #'forge-extras-w3m-after-load)
(advice-add 'w3m--goto-url--handler-function :after #'forge-extras-delete-residual-w3m-buffers)

(defun forge-extras-gh-notify-visit-notification (P)
  "Visit the notification at point and mark it as read if unread.
Browse issue or PR on prefix P."
  (interactive "P")
  (let ((unread (eq (get-text-property (point) 'face)
		    'gh-notify-notification-unread-face)))
    (gh-notify-visit-notification P)
    (when unread
      (forge-extras-mark-issue-as-read))))

(provide 'forge-extras)
;;; forge-extras.el ends here
