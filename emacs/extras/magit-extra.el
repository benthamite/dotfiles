;;; magit-extra.el --- Extensions for magit -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/magit-extras.el
;; Version: 0.2
;; Package-Requires: ((magit "3.1") (paths "0.1"))

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

;; Extensions for `magit'.

;;; Code:

(require 'magit)
(require 'paths)

;;;; Functions

;; adapted from Sacha Chua
;;;###autoload
(defun magit-extras-stage-commit-and-push (message &optional file no-push)
  "Stage modifications, commit with MESSAGE and push to remote.
If FILE is provided, only stage and commit that file. If NO-PUSH is non-nil, do
not push to remote."
  (interactive
   (list (progn (magit-diff-unstaged) (read-string "Commit Message: "))))
  (when (or (magit-anything-staged-p)
            (magit-anything-unstaged-p))
    (if file
        (magit-stage-file file)
      (magit-stage-modified t))
    (magit-commit-create (list "-m" message)))
  (unless no-push
    (call-interactively #'magit-push-current-to-pushremote)))

;;;###autoload
(defun magit-extras-stage-commit-and-push-all-repos ()
  "Update all active repositories."
  (dolist (directory paths-dir-all-repos)
    (magit-extras-midnight-update directory)))

(defun magit-extras-midnight-update (directory)
  "Update repo in DIRECTORY with `midnight'."
  (let ((default-directory directory))
    (magit-extras-stage-commit-and-push "Midnight update")))

;; gist.github.com/dotemacs/9a0433341e75e01461c9
(defun magit-extras-parse-url (url)
  "Convert a git remote location as a HTTP URL."
  (if (string-match "^http" url)
      url
    (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
			      "https://\\2/\\3"
			      url)))

(defun magit-extras-move-point-to-start ()
  "Move point to the start of the buffer."
  (run-at-time 0.3 nil #'(lambda () (goto-char (point-min)))))

;; TODO: check if there is a better way to do this
(declare-function files-extras-buffer-file-name "files-extras")
(defun magit-extras-get-commit-file (&optional path)
  "Get file to commit.
If more than one file is being committed, get the first one. By default, the
path of file returned is relative to the current repository. If PATH is `full',
return instead the full path; if PATH is `sans-dir', return the filename only."
  (save-excursion
    (re-search-forward "Changes to be committed:\n#.*?:.  \\(.*/?.*\\)$" nil t)
    (let ((file (match-string-no-properties 1)))
      (pcase path
	('full (let ((repo (file-name-directory (directory-file-name (files-extras-buffer-file-name)))))
		 (file-name-concat repo file)))
	('sans-dir (file-name-nondirectory file))
	(_ file)))))

(autoload 'org-entry-get "org")
(declare-function window-extras-switch-to-last-window "window-extras")
(defun magit-extras-get-commit-heading ()
  "Get the `org-mode' heading above the code to be committed."
  (let ((file (magit-extras-get-commit-file 'full)))
    (save-window-excursion
      (window-extras-switch-to-last-window)
      (magit-section-forward-sibling)
      (magit-diff-visit-file file)
      (org-entry-get nil "ITEM"))))

;;;###autoload
(defun magit-extras-checkout-tag-with-submodules ()
  "Prompt for a tag and check it out in main repo and all submodules."
  (interactive)
  (let* ((tags (magit-list-tags))
         (choices (append '("main") tags))
         (selection (completing-read "Checkout tag or main: " choices nil t)))
    (magit-run-git "checkout" "-f" selection)  ; force checkout of main repo
    (if (string= selection "main")
        (progn
          (magit-run-git "submodule" "foreach" "--recursive" "git" "checkout" "main")
          (magit-run-git "submodule" "foreach" "--recursive" "git" "pull"))
      (progn
        (magit-run-git "submodule" "foreach" "--recursive" "git" "reset" "--hard")
        (magit-run-git "submodule" "update" "--recursive" "--init" "--force")))))

;;;;; dirty repo

(declare-function alert "alert")
;;;###autoload
(defun magit-extras-warn-if-repo-is-dirty (repo-path)
  "Check if repository at REPO-PATH is dirty and emit an alert if so."
  (let ((default-directory repo-path))
    (when (and (file-directory-p repo-path)
               (magit-git-repo-p repo-path)
               (magit-anything-modified-p))
      (alert (format "Repository at %s is dirty!" repo-path)))))

;;;;; transient

(defun magit-extras-get-unstaged-files ()
  "Get a list of unstaged files in the current Git repository."
  (magit-git-lines "diff" "--name-only" "--diff-filter=d"))

(defun magit-extras-track-file (file)
  "Track FILE in the current Git repository."
  (magit-call-git "add" (expand-file-name file)))

(transient-define-suffix magit-extras-update-all-submodules (args)
  "Update all submodules in the current Git repository."
  :class 'magit--git-submodule-suffix
  :description "Update all modules    git submodule update --init [--recursive]"
  (interactive (list (magit-submodule-arguments "--recursive")))
  (magit-with-toplevel
    (magit-run-git-async "submodule" "update" "--init" args)))

(transient-define-prefix magit-extras-dispatch ()
  "Invoke a Magit command from a list of available commands."
  :info-manual "(magit)Top"
  ["Transient and dwim commands"
   ;; → bound in magit-mode-map or magit-section-mode-map
   ;; ↓ bound below
   [("A" "Apply"          magit-cherry-pick)
    ;; a                  ↓
    ("b" "Branch"         magit-branch)
    ("B" "Bisect"         magit-bisect)
    ("c" "Commit"         magit-commit)
    ("C" "Clone"          magit-clone)
    ("d" "Diff"           magit-diff)
    ("D" "Diff (change)"  magit-diff-refresh)
    ("e" "Ediff (dwim)"   magit-ediff-dwim)
    ("E" "Ediff"          magit-ediff)
    ("f" "Fetch"          magit-fetch)
    ("F" "Pull"           magit-pull)
    ;; g                  ↓
    ;; G                → magit-refresh-all
    ("h" "Help"           magit-info)
    ("H" "Section info"   magit-describe-section :if-derived magit-mode)]
   [("i" "Ignore"         magit-gitignore)
    ("I" "Init"           magit-init)
    ("j" "Jump to section"magit-status-jump  :if-mode     magit-status-mode)
    ("j" "Display status" magit-status-quick :if-not-mode magit-status-mode)
    ("J" "Display buffer" magit-display-repository-buffer)
    ;; k                  ↓
    ;; K                → magit-file-untrack
    ("l" "Log"            magit-log)
    ("L" "Log (change)"   magit-log-refresh)
    ("m" "Merge"          magit-merge)
    ("M" "Remote"         magit-remote)
    ("n" "Forge"          forge-dispatch)
    ;; N       reserved → forge-dispatch
    ("o" "Submodule"      magit-submodule)
    ("O" "Subtree"        magit-subtree)
    ("p" "Pull"           magit-pull)
    ;; P                → magit-section-backward
    ;; q                → magit-mode-bury-buffer
    ("Q" "Command"        magit-git-command)]
   [("r" "Rebase"         magit-rebase)
    ;; R                → magit-file-rename
    ;; s                  ↓
    ;; S                  ↓
    ("t" "Tag"            magit-tag)
    ("T" "Note"           magit-notes)
    ;; u                  ↓
    ;; U                  ↓
    ;; v                  ↓
    ("V" "Revert"         magit-revert)
    ("w" "Apply patches"  magit-am)
    ("W" "Format patches" magit-patch)
    ;; x                → magit-reset-quickly
    ("X" "Reset"          magit-reset)
    ("y" "Show Refs"      magit-show-refs)
    ("Y" "Cherries"       magit-cherry)
    ("z" "Stash"          magit-stash)
    ("Z" "Worktree"       magit-worktree)
    ("." "Push"           magit-push)
    ("!" "Run"            magit-run)]]
  ["Applying changes"
   :if-derived magit-mode
   [("a" "Apply"          magit-apply)
    ("v" "Reverse"        magit-reverse)
    ("k" "Discard"        magit-discard)]
   [("s" "Stage"          magit-stage)
    ("u" "Unstage"        magit-unstage)]
   [("S" "Stage all"      magit-stage-modified)
    ("U" "Unstage all"    magit-unstage-all)]]
  ["Essential commands"
   :if-derived magit-mode
   [("g" "       refresh current buffer"   magit-refresh)
    ("q" "       bury current buffer"      magit-mode-bury-buffer)
    ("<tab>" "   toggle section at point"  magit-section-toggle)
    ("<return>" "visit thing at point"     magit-visit-thing)]
   [("C-x m"    "show all key bindings"    describe-mode)
    ("C-x i"    "show Info manual"         magit-info)]])

(advice-add 'magit-dispatch :override #'magit-extras-dispatch)

(defun magit-extras-with-editor-finish-and-push ()
  "Finish editing and push commit."
  (interactive)
  (with-editor-finish nil)
  (call-interactively #'magit-push-current-to-pushremote))

(provide 'magit-extra)
;;; magit-extra.el ends here
