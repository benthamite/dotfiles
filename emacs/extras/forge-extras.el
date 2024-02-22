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

;;;; Functions

(defun forge-extras-get-unread-notifications ()
  "Return the number of unread notifications."
  (when-let ((unread-notifications (forge--ls-notifications '(unread))))
    (length unread-notifications)))

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

;;;;; Menus

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
    ("c f" "fork or remote"        forge-fork)]
   ["List"
    ("l a" "awaiting review"        forge-list-requested-reviews)
    ("l i" "issues"                 forge-list-issues)
    ("l n" "notifications"          forge-list-notifications)
    ("l p" "pull-requests"          forge-list-pullreqs)
    ("l t" "topics"                 forge-list-topics)
    ("l r" "repositories"           forge-list-repositories)
    """Edit"
    ("e t" "edit title"             forge-topic-set-title)
    ("e s" "edit state"             forge-topic-state-menu)
    ("e l" "edit labels"            forge-topic-set-labels)]
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
    ("v t" "topic"                  forge-visit-topic)]
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
    ("d p" "labeled pull-requests"  forge-list-labeled-pullreqs)]
   ["Misc"
    ("s" "search topics"            forge-search)
    (";" "Show/hide closed topics"  forge-toggle-closed-visibility)]])

(provide 'forge-extras)
;;; forge-extras.el ends here
