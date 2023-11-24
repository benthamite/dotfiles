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

(require 'vc)
(require 'paths)

;;;; User options

(defgroup vc-extras ()
  "`Extensions for `vc'."
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

(defcustom vc-extras-personal-repo-dir paths-dir-repos
  "The directory where personal repositories are stored."
  :type 'directory
  :group 'vc-extras)

(defcustom vc-extras-work-repo-dir vc-extras-personal-repo-dir
  "The directory where personal repositories are stored."
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
	 (is-git-dir-p (eq (vc-responsible-backend default-directory t) 'Git))
	 (default-directory (read-directory-name
			     "Directory: " (if is-git-dir-p
					       default-directory
					     (if work vc-extras-work-repo-dir vc-extras-personal-repo-dir))
			     nil nil)))
    (when (and (not (file-exists-p default-directory))
               (y-or-n-p (format "Directory %s does not exist. Create it?" default-directory)))
      (make-directory default-directory))
    (unless (or is-git-dir-p
		(not (y-or-n-p (format "Directory %s is not a Git repository. Initialize it?" default-directory))))
      (vc-create-repo 'Git))
    (shell-command
     (format "%s create -d \"%s\" %s %s/%s"
             vc-extras-hub-executable description (if private "--private" "") account name))))

(provide 'vc-extras)
;;; vc-extras.el ends here
