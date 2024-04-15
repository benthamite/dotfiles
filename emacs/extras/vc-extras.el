;;; vc-extras.el --- Extensions for vc -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/vc-extras.el
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

;; Extensions for `vc'.

;;; Code:

(require 'paths)
(require 'vc)
(require 'vc-git)

;;;; User options

(defgroup vc-extras ()
  "Extensions for `vc'."
  :group 'vc)

(defcustom vc-extras-gh-executable (executable-find "gh")
  "The `gh' executable (https://cli.github.com/)."
  :type 'file
  :group 'vc-extras)

(defcustom vc-extras-github-account-personal "benthamite"
  "The GitHub account to use for personal repositories."
  :type 'string
  :group 'vc-extras)

(defcustom vc-extras-github-account-work "tlon-team"
  "The GitHub account to use for work repositories."
  :type 'string
  :group 'vc-extras)

(defcustom vc-extras-personal-repo-dir paths-dir-personal-repos
  "The directory where personal repositories are stored."
  :type 'directory
  :group 'vc-extras)

(defcustom vc-extras-work-repo-dir paths-dir-tlon-repos
  "The directory where work repositories are stored."
  :type 'directory
  :group 'vc-extras)

;;;; Functions

;;;###autoload
(defun vc-extras-create-repo ()
  "Create a new GitHub repository."
  (interactive)
  (let* ((name (read-string "Name: "))
         (description (read-string "Description: "))
         (private (y-or-n-p "Private? "))
         (work (y-or-n-p "Work? "))
	 (account (if work vc-extras-github-account-work vc-extras-github-account-personal))
	 (default-directory (read-directory-name
			     "Directory: " (if (vc-extras-is-git-dir-p default-directory)
					       default-directory
					     (if work vc-extras-work-repo-dir vc-extras-personal-repo-dir))
			     nil nil name)))
    (vc-extras-check-dir-exists)
    (vc-extras-check-dir-git)
    (vc-extras-gh-create-repo name description account private)
    (when (y-or-n-p "Clone repository? ")
      (vc-git-clone (vc-extras-get-github-remote account name)
		    default-directory nil))))

(defun vc-extras-get-github-remote (account name)
  "Return the GitHub remote of ACCOUNT and repo NAME."
  (format "https://github.com/%s/%s.git" account name))

(defun vc-extras-is-git-dir-p (dir)
  "Return non-nil if DIR is a Git repository."
  (eq (vc-responsible-backend dir t) 'Git))

(defun vc-extras-check-dir-exists ()
  "Check if default directory exists; if not, ask user to create it."
  (when (and (not (file-exists-p default-directory))
             (y-or-n-p (format "Directory `%s' does not exist. Create it?" default-directory)))
    (make-directory default-directory)))

(defun vc-extras-check-dir-git ()
  "Check if default directory is a Git repo; if not, ask user to initialize it."
  (when (and (not (vc-extras-is-git-dir-p default-directory))
	     (y-or-n-p (format "Directory `%s' is not a Git repository. Initialize it?" default-directory)))
    (vc-create-repo 'Git)))

;;;;; gh

(defun vc-extras-gh-create-repo (name description account &optional private)
  "Create a new GitHub repository in ACCOUNT with NAME and DESCRIPTION.
If PRIVATE is non-nil, make it a private repository."
  (vc-extras-check-gh-exists)
  (shell-command-to-string
   (format
    "%s repo create %s/%s %s --description \"%s\""
    vc-extras-gh-executable account name (if private "--private" "--public") description)))

(defun vc-extras-gh-list-repos (account)
  "List all repos in GitHub ACCOUNT."
  (vc-extras-check-gh-exists)
  (let* ((repos
	  (shell-command-to-string (format "%s repo list %s | awk '{print $1}'"
					   vc-extras-gh-executable account))))
    (split-string (replace-regexp-in-string (format "%s/" account) "" repos))))

(defun vc-extras-check-gh-exists ()
  "Check that `gh' exists, else signal an error."
  (unless vc-extras-gh-executable
    (user-error "`gh' not found; please install it (`brew install gh')")))

(provide 'vc-extras)
;;; vc-extras.el ends here
