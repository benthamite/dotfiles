;;; vc-extras.el --- Extensions for vc -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/vc-extras.el
;; Version: 0.2
;; Package-Requires: ((paths "0.1"))

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
(require 'transient)

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

(autoload 'forge-get-repository "forge-core")
(declare-function forge-extras-track-repository "forge-extras")
;;;###autoload
(defun vc-extras-clone-repo (&optional name account no-forge)
  "Clone an existing repo.
If NAME is nil, prompt for one using the repository list from GitHub.
ACCOUNT may be specified explicitly; if not, it is derived from the repo lookup.
NO-FORGE, if non-nil, skips prompting to add the repo to Forge.
With a prefix argument, the target parent directory is prompted."
  (interactive "P")
  (let* ((repo-data (vc-extras--select-repo name account))
         (name (car repo-data))
         (account (cdr repo-data))
         (remote (vc-extras-get-github-remote name account))
         (parent-dir (vc-extras-get-account-prop account :dir))
         (clone-dir (if current-prefix-arg
                        (vc-extras--prompt-target-directory parent-dir name)
                      (file-name-concat parent-dir name)))
         (process-buffer "/git-clone-output/"))
    (when (file-exists-p clone-dir)
      (user-error "Directory `%s' already exists" clone-dir))
    (message "Asynchronously cloning repo %s..." name)
    (let ((default-directory parent-dir))
      (set-process-sentinel
       (start-process "git-clone" process-buffer
                      "git" "clone" "--recurse-submodules" remote
                      (file-name-nondirectory clone-dir))
       (lambda (proc event)
         (vc-extras--clone-sentinel proc event clone-dir name no-forge))))))

(defun vc-extras--select-repo (name account)
  "Return a cons cell (NAME . ACCOUNT) for cloning.
If NAME is nil, prompt for one (and if ACCOUNT is nil, try to use the repo
list)."
  (let* ((repos (and (not name)
                     (vc-extras-gh-list-repos account)))
         (name (or name
                   (completing-read "Repo: " (mapcar #'car repos))))
         (account (or account
                      (alist-get name repos nil nil #'string=)
                      (user-error "If a repo is provided, account must be provided"))))
    (cons name account)))

(defun vc-extras--prompt-target-directory (parent-dir name)
  "Prompt for a PARENT-DIR and return a directory with NAME appended."
  (let* ((custom-parent (read-directory-name "Parent directory: " parent-dir))
         (target (file-name-concat custom-parent name)))
    target))

(defun vc-extras--initialize-submodules (dir)
  "Initialize submodules of the repo at DIR.
Runs ‘git submodule init’ and ‘git submodule update --recursive’ with DIR as the
working directory."
  (let ((default-directory dir))
    (call-process "git" nil nil nil "submodule" "init")
    (call-process "git" nil nil nil "submodule" "update" "--recursive")))

(defun vc-extras--get-submodule-paths (dir)
  "Return a list of submodule paths relative to DIR."
  (let ((default-directory dir)
        ;; Use --quiet to avoid the initial summary line if present
        (output (shell-command-to-string "git submodule status --recursive --quiet")))
    (delq nil
          (mapcar (lambda (line)
                    ;; Match lines like " 123abc... path/to/submodule (branch)"
                    ;; Or " +123abc... path/to/submodule (branch)" (if status differs)
                    ;; Or "-123abc... path/to/submodule (branch)" (if not initialized)
                    ;; Capture the path (group 1), which is the sequence of non-space characters after the hash.
                    (when (string-match "^[ +-]?[0-9a-f]+[[:space:]]+\\([^[:space:]]+\\)" line)
                      (match-string 1 line)))
                  (split-string output "\n" t)))))

(defun vc-extras--get-submodule-paths-from-gitmodules (dir)
  "Return a list of submodule paths relative to DIR by parsing .gitmodules."
  (let ((gitmodules-file (expand-file-name ".gitmodules" dir))
        (paths '()))
    (when (file-exists-p gitmodules-file)
      (with-temp-buffer
        (insert-file-contents gitmodules-file)
        (goto-char (point-min))
        ;; Look for lines like 'path = path/to/submodule'
        (while (re-search-forward "^[[:space:]]*path[[:space:]]*=[[:space:]]*\\(.*?\\)[[:space:]]*$" nil t)
          (push (match-string 1) paths))))
    (nreverse paths)))

(defun vc-extras--clone-sentinel (proc event clone-dir name no-forge)
  "Process sentinel for git clone.
PROC is the process, EVENT is the clone event, CLONE-DIR is the cloned repo
directory, NAME is the repo name, and NO-FORGE controls adding the repo to
Forge."
  (when (and (string= event "finished\n")
             (= (process-exit-status proc) 0))
    (let ((default-directory clone-dir))
      (when (vc-extras-has-submodules-p clone-dir)
        (vc-extras--initialize-submodules clone-dir)
        ;; Configure git to automatically update submodules on pull
        (call-process "git" nil nil nil "config" "--local" "submodule.recurse" "true"))
      (pcase vc-extras-split-repo
        ('nil nil)
        ('prompt (when (y-or-n-p "Move .git directory to separate directory? ")
                   (vc-extras-split-local-repo clone-dir)))
        (_ (vc-extras-split-local-repo clone-dir)))
      (unless no-forge
        (when (and (not (forge-get-repository :tracked?))
                   (y-or-n-p "Add to Forge? "))
          (forge-extras-track-repository clone-dir)))
      (message (concat "Cloned repo " name
                       (if (vc-extras-has-submodules-p clone-dir)
                           " with all its submodules." ".") )))))

(defun vc-extras-has-submodules-p (dir)
  "Return non-nil if the repository in DIR has submodules."
  (file-exists-p (expand-file-name ".gitmodules" dir)))

(defun vc-extras-get-github-remote (name &optional account)
  "Return the GitHub remote named NAME in ACCOUNT.
If ACCOUNT is nil, search in all accounts listed in `vc-extras-profiles'."
  (when-let* ((account (or account (vc-extras-get-account-of-name name))))
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
If the repository has submodules, move their `.git' directories, too."
  (interactive "D")
  (let* ((name (file-name-nondirectory (directory-file-name dir)))
	 (source (file-name-concat dir ".git/"))
	 (target (file-name-concat paths-dir-split-git name))
	 (git-file (file-name-concat dir ".git"))
	 (has-submodules (vc-extras-has-submodules-p dir))
	 (submodule-paths (when has-submodules
			    (vc-extras--get-submodule-paths-from-gitmodules dir))))
    (when (file-exists-p target)
      (user-error "Directory `%s' already exists" target))
    (vc-extras--move-git-dir source target)
    (vc-extras--create-git-pointer target git-file)
    (when has-submodules
      (vc-extras--handle-submodules dir target submodule-paths))))

(defun vc-extras--move-git-dir (source target)
  "Move the .git directory from SOURCE to TARGET."
  (make-directory (file-name-directory target) t)
  (rename-file source target t))

(defun vc-extras--create-git-pointer (target git-file)
  "Create a .git file at GIT-FILE pointing to TARGET."
  (let ((temp-git-file (make-temp-file "vc-extras-gitdir-")))
    (with-temp-file temp-git-file
      (insert (format "gitdir: %s" (expand-file-name target))))
    (rename-file temp-git-file git-file t)))

(defun vc-extras--handle-submodules (dir target submodule-paths)
  "Handle SUBMODULE-PATHS in the repository at DIR, using TARGET for split git dir."
  (let ((default-directory dir))
    ;; Submodule git data is already moved within target/modules/
    ;; We just need to create the pointer files in their working directories.
    ;; This should happen *after* the main gitdir points correctly.
    (call-process "git" nil nil nil "submodule" "sync" "--recursive")
    ;; For each submodule path obtained earlier
    (dolist (submodule-path submodule-paths)
      (let* ((submodule-dir (file-name-concat dir submodule-path))
	     ;; Assume submodule name for module path is the last component of its working dir path
	     (submodule-name (file-name-nondirectory submodule-path))
	     ;; Path to the .git file/dir within the submodule's working directory
	     (git-file (file-name-concat submodule-dir ".git"))
	     ;; Target path for the submodule's git data within the *main repo's* split .git directory
	     (module-target (file-name-concat target "modules" submodule-name)))
        ;; Update the .git file in the submodule's working directory
        ;; Ensure the submodule directory actually exists
        (when (file-directory-p submodule-dir)
          ;; --- Create gitdir content in temp file ---
          (let ((temp-submodule-git-file (make-temp-file "vc-extras-submodule-gitdir-")))
	    (with-temp-file temp-submodule-git-file
              (insert (format "gitdir: %s" (expand-file-name module-target))))
	    ;; --- Rename temp file to final submodule .git path ---
	    ;; The final 't' allows overwriting if the path somehow still exists.
	    (rename-file temp-submodule-git-file git-file t)))))
    ;; Sync and Update submodules *after* all pointer files are created
    (call-process "git" nil nil nil "submodule" "sync" "--recursive")
    (call-process "git" nil nil nil "submodule" "update" "--init" "--recursive")))

;;;;; Delete

;;;###autoload
(defun vc-extras-delete-local-repo (&optional name account)
  "Delete the repo named NAME in ACCOUNT.
If NAME is nil, prompt the user to select one from local repositories. If
ACCOUNT is nil, candidates will be gathered from all available profiles.
Otherwise, only local repos in the directory of ACCOUNT will be displayed. Note
that if multiple accounts share the same directory, repos from all these
accounts will be displayed.

Deletes both the main repository directory (if it exists and is a Git repo) and
the corresponding split Git directory (if it exists). A message is displayed
listing the directories that were deleted."
  (interactive)
  (let* ((candidates (unless name (vc-extras-list-local-candidates account)))
         (name (or name
                   (completing-read "Repo to delete: " (mapcar #'car candidates) nil t)))
         (main-dir (vc-extras-resolve-repo-dir name account candidates))
         (split-dir (file-name-concat paths-dir-split-git name))
         deleted)
    (when (file-exists-p main-dir)
      (if (vc-extras-is-git-dir-p main-dir)
          (progn
            (delete-directory main-dir t)
            (push main-dir deleted))
        (message "Directory `%s' is not a Git repository. Skipping deletion." main-dir)))
    (when (file-exists-p split-dir)
      (delete-directory split-dir t)
      (push split-dir deleted))
    (if deleted
        (message "Deleted repos: %s" (string-join (nreverse deleted) ", "))
      (message "Repo `%s' not found locally." name))))

(defun vc-extras-resolve-repo-dir (name account candidates)
  "Return the directory for the repository NAME.
If ACCOUNT is non-nil, construct it using the account’s repository directory.
Otherwise, look it up in CANDIDATES (an alist of (NAME . DIR))."
  (if account
      (file-name-concat (vc-extras-get-account-prop account :dir) name)
    (or (cdr (assoc name candidates))
        (user-error "Repository `%s' not found locally" name))))

(defun vc-extras--delete-repo-directory (dir)
  "Delete directory DIR if it exists and is a Git repository.
Return DIR if deletion was performed, or nil otherwise."
  (when (and (file-exists-p dir) (vc-extras-is-git-dir-p dir))
    (delete-directory dir t)
    dir))

(defun vc-extras-list-local-candidates (&optional account)
  "Return an alist of local Git repositories as (NAME . DIR).
If ACCOUNT is non-nil, only consider repositories in that account’s directory;
otherwise, search all accounts listed in `vc-extras-profiles'. Only directories
that appear to be Git repositories (according to `vc-extras-is-git-dir-p') are
included."
  (let* ((repo-dirs (if account
                        (list (vc-extras-get-account-prop account :dir))
                      (delete-dups
                       (mapcar (lambda (profile)
                                 (vc-extras-get-account-prop (plist-get profile :account) :dir))
                               vc-extras-profiles))))
         (dirs (mapcan (lambda (dir)
                         (directory-files dir t "^[^.][^/]*$"))
                       repo-dirs)))
    (delq nil
          (mapcar (lambda (d)
                    (let ((name (file-name-nondirectory (directory-file-name d))))
                      (when (vc-extras-is-git-dir-p d)
                        (cons name d))))
                  dirs))))

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
  (if (string-match "Logged in to github\\.com account"
		    (shell-command-to-string "gh auth status"))
      (message "`gh' is authenticated.")
    (user-error "`gh' not authenticated; please authenticate (`gh auth login')")))

;;;;;; List repos

(defun vc-extras-gh-list-repos (&optional account)
  "List all repos in GitHub ACCOUNT.
If ACCOUNT is nil, list all repos in all accounts."
  (if account
      (vc-extras-gh-list-repos-in-account account)
    (vc-extras-gh-list-repos-in-all-accounts)))

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

;;;;; misc

(defun vc-extras-strip-diff-markers (start end)
  "Remove leading `+' or `-' characters from each line in region from START to END."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (let ((end-marker (point-marker)))
      (goto-char start)
      (while (< (point) end-marker)
        (beginning-of-line)
        (when (looking-at "^\\([+-]\\)")
          (replace-match "" nil nil nil 1))
        (forward-line 1)))))

;;;;; Menu

;;;###autoload (autoload 'vc-extras-menu "vc-extras" nil t)
(transient-define-prefix vc-extras-menu ()
  "`vc-extras' menu."
  [[""
    ("c" "Create remote repo"                       vc-extras-create-repo)
    ("l" "Clone remote repo"                        vc-extras-clone-repo)
    ("H-l" "Clone remote repo with confirmation"   (lambda () (interactive) (let ((current-prefix-arg '(4)))
									 (vc-extras-clone-repo))))
    ""
    ("s" "Split local repo"                         vc-extras-split-local-repo)
    ("d" "Delete local repo"                        vc-extras-delete-local-repo)
    ""
    ("a" "Check authentication status"              vc-extras-check-gh-authenticated)]])

(provide 'vc-extras)
;;; vc-extras.el ends here
