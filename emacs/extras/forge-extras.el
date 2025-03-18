;;; forge-extras.el --- Extensions for forge -*- lexical-binding: t; fill-column: 80 -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/forge-extras.el
;; Version: 0.2
;; Package-Requires: ((forge "0.3.1") (shut-up "0.3.1"))

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
(require 'shut-up)

;;;; Variables

(defconst forge-extras-safari-script-format-string
  "osascript -e 'tell application \"Safari\"
                 tell window 1
                   set beforeCount to count of tabs
                   make new tab with properties {URL:\"%s\"}
                   delay 5
                   try
                     set loadState to do JavaScript \"document.readyState\" in tab (beforeCount + 1)
                     if loadState is \"complete\" then
                       close tab (beforeCount + 1)
                       return \"Tab loaded and closed\"
                     end if
                   on error errMsg
                     return \"Error: \" & errMsg
                   end try
                 end tell
               end tell'"
  "The AppleScript to open a new tab in Safari and run JavaScript.")

;;;; Functions

(declare-function org-store-link "ol")
(defun forge-extras-orgit-store-link (_arg)
  "Like `org-store-link' but store links to all selected commits, if any."
  (interactive "P")
  (if-let* ((sections (magit-region-sections 'commit)))
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

(defun forge-extras-browse-github-inbox ()
  "Browse the GitHub notification inbox."
  (interactive)
  (browse-url "https://github.com/notifications"))

(defun forge-extras-state-set-dwim (&optional issue)
  "Close ISSUE at point if open, or reopen it if closed.
If ISSUE is nil, use the issue at point or in the current buffer."
  (interactive)
  (let* ((issue (or issue (forge-current-topic)))
	 (repo (forge-get-repository issue))
	 (state (oref issue state)))
    (pcase state
      ('open (forge--set-topic-state repo issue 'completed))
      ('completed (forge--set-topic-state repo issue 'open)))))

(defun forge-extras-pull-notifications ()
  "Fetch notifications for all repositories from the current forge.
Do not update if `elfeed' is in the process of being updated, since this causes
problems."
  (unless (bound-and-true-p elfeed-extras-auto-update-in-process)
    (shut-up
      (with-no-warnings
	(forge-pull-notifications)))))

;;;;; sync read status

(autoload 'doom-modeline--github-fetch-notifications "doom-modeline-segments")
(defun forge-extras-sync-read-status (&optional _)
  "Ensure that the read status of the issue at point in Forge matches GitHub's."
  (let* ((issue (forge-current-topic))
         (url (forge-get-url issue)))
    (when (eq (oref issue status) 'unread)
      (forge-extras-async-shell-command-to-string
       (format forge-extras-safari-script-format-string url)
       #'forge-extras-update-github-counter))))

(defun forge-extras-update-github-counter (output)
  "Update the GitHub notification counter after the Safari page is loaded.
OUTPUT is the output of the AppleScript script; it is used to check whether
JavaScript is enabled in Safari, which is needed for the command to run
successfully."
  (forge-extras-safari-ensure-javascript-enabled output)
  (when (bound-and-true-p doom-modeline-github)
    (doom-modeline--github-fetch-notifications)))

(defun forge-extras-async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the background.
Call CALLBACK with the resulting output when done."
  (let ((output-buffer (generate-new-buffer "*Async Shell Command*")))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (let ((output (with-current-buffer output-buffer
                         (buffer-string))))
           (funcall callback output)
           (kill-buffer output-buffer)))))))

(defun forge-extras-safari-github-logged-in-p ()
  "Check if user is logged in to GitHub in Safari."
  (let ((output (shell-command-to-string
		 "osascript -e 'tell application \"Safari\" to get name of document 1'")))
    (forge-extras-safari-ensure-javascript-enabled output)
    ;; we search for a word that only shows up if the user is logged in
    (numberp (string-match-p "issue" output))))

(defun forge-extras-safari-ensure-javascript-enabled (output)
  "Ensure that JavaScript is enabled in Safari.
OUTPUT is the output of the shell command that calls the AppleScript."
  (when (string-match-p "allow javascript from apple events" output)
    (error "For this function to work, JavaScript from Apple Events must be enabled in
Safari. This can be done by going to Safari > Preferences > Advanced, ticking
the box labelled \"Show features for web developers\", and then going to Safari
> Preferences > Developer and ticking the box labeled \"Allow JavaScript from
Apple Events\"")))

;;;;; Track repos

(declare-function magit-status "magit-status")
(autoload 'vc-extras-is-git-dir-p "vc-extras")
;;;###autoload
(defun forge-extras-track-repository (&optional dir)
  "Add DIR to the Forge database.
If DIR is nil, use the current directory."
  (interactive)
  (let ((default-directory (or dir default-directory)))
    (if (vc-extras-is-git-dir-p default-directory)
	(let ((url (and-let*
		       ((repo (forge-get-repository :stub))
			(remote (oref repo remote)))
		     (magit-git-string "remote" "get-url" remote))))
	  (forge-extras-track-repo-all-topics url)
	  (magit-status-setup-buffer dir))
      (user-error "`%s' is not a Git repository" default-directory))))

(defun forge-extras-track-repo-all-topics (&optional url-or-path)
  "Add a repository to the forge database, pulling all topics.
If URL-OR-PATH is provided, add that repository. Otherwise, add the current
repo."
  (let* ((default-directory (if (and url-or-path (file-directory-p url-or-path))
				(expand-file-name url-or-path)
			      (or (locate-dominating-file default-directory ".git")
				  default-directory)))
	 (remote (forge--get-remote))
	 (repo-url (cond
		    ((and url-or-path (string-match-p "^\\(https?\\|git@\\)" url-or-path))
		     url-or-path)
		    (remote
		     (magit-git-string "remote" "get-url" remote))
		    (t
		     (user-error "No suitable repository found"))))
	 (repo (forge-get-repository repo-url nil :insert!)))
    (forge--pull repo nil nil)))

;;;;; Copy message at point

(defun forge-extras-copy-message-at-point-as-kill ()
  "Copy the body of the message at point to the kill ring.
The formatting of the message is preserved."
  (interactive)
  (when-let ((message (forge-post-at-point t)))
    (kill-new (oref message body))
    (message "Message copied to kill ring.")))

(provide 'forge-extras)
;;; forge-extras.el ends here
