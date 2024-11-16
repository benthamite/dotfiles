;;; vc-extras.el --- Extensions for vc -*- lexical-binding: t -*-

;; Copyright (C) 2024

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

(defcustom vc-extras-profiles
  `((:name personal
	   :account ,vc-extras-github-account-personal
	   :dir ,paths-dir-personal-repos)
    (:name work
	   :account ,vc-extras-github-account-work
	   :dir ,paths-dir-tlon-repos))
  "A list of GitHub profiles and their corresponding directories."
  ;; TODO: define type correctly
  :type '(alist :key-type symbol :value-type (alist :key-type symbol :value-type string))
  :group 'vc-extras)

(defcustom vc-extras-split-repo 'prompt
  "Whether to split the `.git' directory in a separate directory.
If nil, never split the `.git' directory. If `prompt', ask the user whether to
split the `.git' directory. If t or any other non-nil value, always split the
`.git' directory."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Prompt" prompt)
		 (boolean :tag "Always" t))
  :group 'tlon-repos)

;;;; Functions

;;;;; General

(defun vc-extras-is-git-dir-p (dir)
  "Return non-nil if DIR is a Git repository."
  (eq (vc-responsible-backend dir t) 'Git))

(defun vc-extras-get-account-prop (account prop)
  "Return the value of PROP for ACCOUNT in `vc-extras-profiles'."
  (plist-get (seq-find (lambda (plist)
			 (equal (plist-get plist :account) account))
		       vc-extras-profiles)
	     prop))

;;;;; Create

;;;###autoload
(defun vc-extras-create-repo (&optional name account)
  "Create a new GitHub repository named NAME in ACCOUNT.
If NAME is nil, prompt for one. If ACCOUNT is nil, select one."
  (interactive)
  (let* ((name (or name (read-string "Name: ")))
         (account (or account (completing-read "Account: "
					       (mapcar (lambda (profile)
							 (plist-get profile :account))
						       vc-extras-profiles))))
         (description (read-string "Description: "))
         (private (y-or-n-p "Private? "))
	 (default-directory (vc-extras-get-account-prop account :dir)))
    (vc-extras-gh-create-repo name account description private)
    (when (y-or-n-p "Clone repository? ")
      (vc-extras-clone-repo name account))))

;;;;; Clone

(declare-function forge-get-repository "forge-core")
(declare-function forge-extras-track-repository "forge-extras")
;;;###autoload
(defun vc-extras-clone-repo (&optional name account no-forge)
  "Clone an existing repo.
ACCOUNT is the name of the GitHub account. If NAME is nil, prompt the user for a
repo name. If NO-FORGE is non-nil, do not prompt to add the repo to Forge.

This function does not use `vc-git-clone' because it does not support cloning
submodules."
  (interactive)
  (let* ((repos nil)
	 (name (or name
		   (completing-read "Repo: " (setq repos (vc-extras-gh-list-repos account)))))
	 (account (or account (if repos
				  (alist-get name repos nil nil #'string=)
				(user-error "If you provide a repo name, you must also provide its account"))))
         (remote (vc-extras-get-github-remote name account))
         (parent-dir (vc-extras-get-account-prop account :dir))
         (dir (file-name-concat parent-dir name))
         (process-buffer "/git-clone-output/"))
    (when (file-exists-p dir)
      (user-error "Directory `%s' already exists" dir))
    (message "Cloning repo %s..." name)
    (let ((default-directory parent-dir))
      (set-process-sentinel
       (start-process "git-clone" process-buffer
                      "git" "clone" "--recurse-submodules" remote (file-name-nondirectory dir))
       (lambda (proc event)
	 (when (and (string= event "finished\n")
                    (= (process-exit-status proc) 0))
           (let ((default-directory dir))
             (when (vc-extras-has-submodules-p dir)
	       (call-process "git" nil nil nil "submodule" "init")
               (call-process "git" nil nil nil "submodule" "update" "--recursive"))
             (if (= (process-exit-status proc) 0)
		 (progn
                   (pcase vc-extras-split-repo
                     ('nil)
                     ('prompt (if (y-or-n-p "Move `.git' directory to separate directory?")
                                  (vc-extras-split-local-repo dir)
				(message "You can customize `vc-extras-split-repo' to avoid this prompt.")))
                     (_ (vc-extras-split-local-repo dir)))
                   (require 'forge-core)
                   (require 'forge-extras)
                   (if (and (not no-forge)
                            (not (forge-get-repository :tracked?))
                            (y-or-n-p "Add to Forge? "))
                       (forge-extras-track-repository dir)
                     (dired dir))
                   (kill-buffer process-buffer)
                   (message (concat "Cloned repo "
				    name (when (vc-extras-has-submodules-p dir) " and all its submodules") ".")))
               (message "Clone failed with exit status %d" (process-exit-status proc))))))))))

(defun vc-extras-has-submodules-p (dir)
  "Return non-nil if the repository in DIR has submodules."
  (file-exists-p (expand-file-name ".gitmodules" dir)))

(defun vc-extras-get-github-remote (name &optional account)
  "Return the GitHub remote named NAME in ACCOUNT.
If ACCOUNT is nil, search in all accounts listed in `vc-extras-profiles'."
  (when-let ((account (or account (vc-extras-get-account-of-name name))))
    (format "https://github.com/%s/%s.git" account name)))

(defun vc-extras-get-account-of-name (name)
  "Return the GitHub account that owns the repo named NAME."
  (catch 'account
    (dolist (profile vc-extras-profiles)
      (let ((account (plist-get profile :account)))
	(when (member name (vc-extras-gh-list-repos account))
	  (throw 'account account))))))

(defun vc-extras-get-repo-dir (name account &optional git)
  "Return the directory of the repo named NAME in ACCOUNT.
If GIT is `git', return
the repo’s `.git' directory. If GIT is `split-git', return the repo’s split
`.git' directory. Otherwise, return the repo directory."
  (let* ((account-dir (vc-extras-get-account-prop account :dir))
	 (dir (pcase git
		('split-git paths-dir-split-git)
		(_ account-dir)))
	 (git-dir (pcase git
		    ('git ".git"))))
    (file-name-as-directory (file-name-concat dir name git-dir))))

;;;;; Split

(defun vc-extras-split-local-repo (dir)
  "Move the `.git' dir in DIR to a split dir and set the `.git' file accordingly.
Normally, this command is run for repos managed by Dropbox, to protect the Git
files from possible corruption."
  (interactive "D")
  (let* ((name (file-name-nondirectory (directory-file-name dir)))
	 (source (file-name-concat dir ".git/"))
	 (target (file-name-concat paths-dir-split-git name))
	 (git-file (directory-file-name source)))
    (when (file-exists-p target)
      (user-error "Directory `%s' already exists" target))
    (rename-file source target t)
    (with-temp-file git-file
      (insert (format "gitdir: %s" target))
      (write-file git-file))))

;;;;; Delete

;;;###autoload
(defun vc-extras-delete-local-repo (&optional name account)
  "Delete the repo named NAME in ACCOUNT.
If NAME is nil, prompt the user for a repo name. If ACCOUNT is nil, infer it
from NAME."
  (interactive)
  (let* ((names-and-dirs)
	 (name (or name
		   (let* ((repo-dirs (if account
					 (list (vc-extras-get-account-prop account :dir))
				       (delete-dups
					(mapcar (lambda (profile)
						  (vc-extras-get-account-prop (plist-get profile :account) :dir))
						vc-extras-profiles))))
			  (dirs (mapcan (lambda (dir)
					  (directory-files dir t "^[^.][^/]*$"))
					repo-dirs)))
		     (setq names-and-dirs (mapcar (lambda (dir)
						    (let ((name (file-name-nondirectory dir)))
						      (cons name dir)))
						  dirs))
		     (completing-read "Dir: " names-and-dirs))))
	 (dir (if account
		  (file-name-concat (vc-extras-get-account-prop account :dir) name)
		(alist-get name names-and-dirs nil nil #'string=)))
	 (split-git (file-name-concat paths-dir-split-git name))
	 deleted)
    (when (and (file-exists-p dir)
	       ;; check that it is a repo, to prevent accidental deletion
	       (vc-extras-is-git-dir-p dir))
      (delete-directory dir t)
      (push dir deleted))
    (when (file-exists-p split-git)
      (delete-directory split-git t)
      (push split-git deleted))
    (if deleted
	(message "Deleted repos: %s" (string-join deleted ", "))
      (message "Repo `%s' not found locally" name))))

;;;;; gh

(defun vc-extras-gh-create-repo (name account description &optional private)
  "Create a new GitHub repository in ACCOUNT with NAME and DESCRIPTION.
If PRIVATE is non-nil, make it a private repository."
  (shell-command-to-string
   (format
    "%s repo create %s/%s %s --description \"%s\""
    vc-extras-gh-executable account name (if private "--private" "--public") description)))

(defun vc-extras-ensure-gh-exists ()
  "Check that `gh' exists, else signal an error."
  (unless vc-extras-gh-executable
    (user-error "`gh' not found; please install it (`brew install gh')")))

(defun vc-extras-check-gh-authenticated ()
  "Ensure that `gh' is authenticated."
  (interactive)
  (vc-extras-ensure-gh-exists)
  (unless (string-match "Logged in to github\\.com account"
			(shell-command-to-string "gh auth status"))
    (user-error "`gh' not authenticated; please authenticate (`gh auth login')")))

;;;;;; List repos

(defun vc-extras-gh-list-repos (&optional account)
  "List all repos in GitHub ACCOUNT.
If ACCOUNT is nil, list all repos in all accounts."
  (if account
      (vc-extras-gh-list-repos-in-account account)
    (vc-extras-gh-list-repos-in-all-accounts)))

;; TODO: make it more efficient; currently it takes a couple of seconds
(defun vc-extras-gh-list-repos-in-account (account)
  "List all repos in GitHub ACCOUNT.
Return the result as a list of cons cells, where the car is the repo name and
the cdr is ACCOUNT."
  (let* ((repos
	  (shell-command-to-string (format "%s repo list %s --limit 9999 | awk '{print $1}'"
					   vc-extras-gh-executable account))))
    (mapcar (lambda (line)
	      (let ((name (cadr (split-string line "/"))))
		(cons name account)))
	    (split-string repos "\n" t))))

(defun vc-extras-gh-list-repos-in-all-accounts ()
  "List all repos in all GitHub accounts."
  (mapcan (lambda (profile)
	    (vc-extras-gh-list-repos-in-account (plist-get profile :account)))
	  vc-extras-profiles))

(provide 'vc-extras)
;;; vc-extras.el ends here
