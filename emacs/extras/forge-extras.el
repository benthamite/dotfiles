;;; forge-extras.el --- Extensions for forge -*- lexical-binding: t -*-

;; Copyright (C) 2024

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
(require 'shut-up)

;;;; Functions

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

(defun forge-extras-sync-read-status (&optional _)
  "Ensure that the read status of the issue at point in Forge matches GitHub’s.
Additionally, if `doom-modeline-github' is non-nil, update the GitHub
notification counter.

The function marks the issue as read by silently browsing it in a Firefox tab."
  (let* ((issue (forge-current-topic))
	 (url (forge-get-url issue)))
    (when (eq (oref issue status) 'unread)
      (shut-up
	(shell-command (format "open -a Firefox --background %s" url)))
      (when (bound-and-true-p doom-modeline-github)
	;; we give it a few seconds to load the page and mark it as read
	(run-with-timer 5 nil 'doom-modeline--github-fetch-notifications)))))

(defun forge-extras-pull-notifications ()
  "Fetch notifications for all repositories from the current forge.
Do not update if `elfeed' is in the process of being updated, since this causes
problems."
  (unless (bound-and-true-p elfeed-extras-auto-update-in-process)
    (shut-up
      (forge-pull-notifications))))

;;;;; Track repos

(declare-function magit-status "magit-status")
(declare-function vc-extras-is-git-dir-p "vc-extras")
;;;###autoload
(defun forge-extras-track-repository (&optional dir)
  "Add DIR to the Forge database.
If DIR is nil, use the current directory."
  (interactive)
  (let ((default-directory (or dir default-directory)))
    (require 'vc-extras)
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

(provide 'forge-extras)
;;; forge-extras.el ends here
