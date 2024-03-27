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

;;;; User options

(defgroup vc-extras ()
  "Extensions for `vc'."
  :group 'vc)

(defcustom vc-extras-hub-executable "/opt/homebrew/bin/hub"
  "The `hub' executable."
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
    (vc-extras-hub-create-repo name description account private)))

(defun vc-extras-is-git-dir-p (dir)
  "Return non-nil if DIR is a Git repository."
  (eq (vc-responsible-backend dir t) 'Git))

(defun vc-extras-check-dir-exists ()
  "Check if DIR exists, and prompt for its creation if it doesn't."
  (when (and (not (file-exists-p default-directory))
             (y-or-n-p (format "Directory `%s' does not exist. Create it?" default-directory)))
    (make-directory default-directory)))

(defun vc-extras-check-dir-git ()
  "Check if DIR is a Git repository, and prompt for its initialization if it isn't."
  (when (and (not (vc-extras-is-git-dir-p default-directory))
	     (y-or-n-p (format "Directory `%s' is not a Git repository. Initialize it?" default-directory)))
    (vc-create-repo 'Git)))

(defun vc-extras-hub-create-repo (name description account &optional private)
  "Create a new GitHub repository in ACCOUNT with NAME and DESCRIPTION.
If PRIVATE is non-nil, make it a private repository."
  (shell-command
   (format
    "%s create -d \"%s\" %s %s/%s"
    vc-extras-hub-executable description (if private "--private" "") account name))
  (dired default-directory))

(provide 'vc-extras)
;;; vc-extras.el ends here
