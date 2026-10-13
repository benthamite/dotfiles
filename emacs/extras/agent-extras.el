;;; agent-extras.el --- Extensions for agent -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/agent-extras.el
;; Version: 0.1
;; Package-Requires: ((emacs "30.0") (agent "0.1"))

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

;; Extensions for `agent' that depend on my own skills: a project audit,
;; the post-push CI closeout, and a read-only view of orchestrate-review
;; runs.  They are added to the Tools column of `agent-menu'.

;;; Code:

(require 'agent)
(require 'button)
(require 'cl-lib)
(require 'json)
(require 'transient)

;;;; User options

(defgroup agent-extras ()
  "Extensions for `agent'."
  :group 'agent)

(defcustom agent-extras-audit-skills
  '("code-audit" "design-audit" "interpretability-audit")
  "Skills to run when performing an integral project audit.
Each entry is a skill name without prefix; each is invoked with
`--accept'."
  :type '(repeat string)
  :group 'agent-extras)

(defcustom agent-extras-audit-project-directories nil
  "Directories available for selection in `agent-extras-audit-project'.
New directories entered by the user are automatically added."
  :type '(repeat directory)
  :group 'agent-extras)

;;;; Post-push CI

(defun agent-extras-post-push-ci (&optional commit)
  "Run the post-push CI closeout skill for COMMIT.
When COMMIT is nil, use the current Git HEAD."
  (interactive)
  (let* ((backend (agent--resolve-backend))
         (skill (or (cl-find "post-push-ci" (agent-discover-skills backend)
                             :key (lambda (s) (plist-get s :name))
                             :test #'equal)
                    (user-error "Skill `post-push-ci' not found for `%s'"
                                backend)))
         (sha (or commit (agent-extras--git-head))))
    (agent--run-skill backend skill
                      (format "--no-push --commit %s" sha))))

(defun agent-extras--git-head ()
  "Return the current Git HEAD SHA."
  (with-temp-buffer
    (unless (zerop (process-file "git" nil t nil "rev-parse" "HEAD"))
      (user-error "Could not resolve Git HEAD"))
    (string-trim (buffer-string))))

;;;; Project audit

(defun agent-extras-audit-project ()
  "Run a comprehensive project audit via the selected backend.
Sequentially runs each skill in `agent-extras-audit-skills' with
`--accept' through the backend's run-prompt slot, auto-committing
after each successful skill, and displays a summary when done."
  (interactive)
  (let* ((backend (agent--resolve-backend))
         (dir (agent-extras--read-audit-directory)))
    (when (yes-or-no-p
           (format "Run %d audit(s) on %s?" (length agent-extras-audit-skills) dir))
      (agent-extras--audit-ensure-clean-worktree dir)
      (agent-extras--audit-run-next
       (list :backend backend :queue agent-extras-audit-skills :results nil
             :dir dir :start-time (current-time))))))

(defun agent-extras--audit-run-next (state)
  "Run the next audit skill in STATE, or finish."
  (if (null (plist-get state :queue))
      (agent-extras--audit-finish state)
    (let* ((backend (plist-get state :backend))
           (queue (plist-get state :queue))
           (name (string-remove-prefix "/" (car queue)))
           (skill (or (cl-find name (agent-discover-skills backend)
                               :key (lambda (s) (plist-get s :name))
                               :test #'equal)
                      (list :name name :style 'slash)))
           (run (or (when-let* ((struct (agent-backend backend)))
                      (agent-backend-run-prompt struct))
                    (user-error "Backend `%s' does not support run-prompt"
                                backend))))
      (message "Running audit %s..." name)
      (funcall run (agent--skill-prompt skill "--accept")
               :directory (plist-get state :dir)
               :callback
               (cl-function
                (lambda (text &key error)
                  (plist-put state :results
                             (cons (list :skill name :text text :error error)
                                   (plist-get state :results)))
                  (plist-put state :queue (cdr queue))
                  (unless error
                    (ignore-errors
                      (agent-extras--audit-commit-changes (plist-get state :dir) name)))
                  (agent-extras--audit-run-next state)))))))

(defun agent-extras--audit-finish (state)
  "Display the audit results from STATE."
  (let* ((results (reverse (plist-get state :results)))
         (total (length results))
         (successes (cl-count-if (lambda (result)
                                   (null (plist-get result :error)))
                                 results))
         (failures (- total successes))
         (duration (float-time
                    (time-subtract (current-time)
                                   (plist-get state :start-time))))
         (buf (get-buffer-create "*Agent audit results*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+title: Agent audit results — %s\n\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S")))
        (insert (format "- Directory: [[file:%s]]\n" (plist-get state :dir)))
        (insert (format "- Total: %d | Success: %d | Failed: %d\n" total successes failures))
        (insert (format "- Time: %.1f seconds\n\n" duration))
        (dolist (result results)
          (insert (format "* %s %s\n"
                          (if (plist-get result :error) "FAIL" "DONE")
                          (plist-get result :skill)))
          (insert (format ":PROPERTIES:\n:ERROR: %s\n:END:\n\n"
                          (or (plist-get result :error) "none")))
          (insert "#+begin_example\n")
          (insert (or (plist-get result :text) "(no output)"))
          (unless (string-suffix-p "\n" (or (plist-get result :text) ""))
            (insert "\n"))
          (insert "#+end_example\n\n")))
      (org-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (message "Agent audit complete: %d/%d succeeded (%.1fs)"
             successes total duration)))

(defun agent-extras--audit-commit-changes (dir title)
  "Commit uncommitted work in DIR after an audit skill completes.
TITLE is the skill name, used to derive the commit message scope."
  (let ((default-directory dir))
    (with-temp-buffer
      (call-process "git" nil t nil "status" "--porcelain")
      (when (> (buffer-size) 0)
        (call-process "git" nil nil nil "add" "-A")
        (let ((scope (replace-regexp-in-string
                      "^/" ""
                      (car (split-string title " ")))))
          (call-process "git" nil nil nil "commit" "-m"
                        (format "%s: apply audit recommendations" scope)))))))

(defun agent-extras--read-audit-directory ()
  "Prompt for a project directory with completion."
  (let* ((candidates (mapcar #'abbreviate-file-name
                             agent-extras-audit-project-directories))
         (input (completing-read "Project directory: " candidates nil nil))
         (dir (file-truename (expand-file-name input))))
    (unless (file-directory-p dir)
      (user-error "Not a directory: %s" dir))
    (unless (member dir (mapcar #'file-truename
                                agent-extras-audit-project-directories))
      (customize-save-variable 'agent-extras-audit-project-directories
                               (append agent-extras-audit-project-directories
                                       (list dir))))
    dir))

(defun agent-extras--audit-ensure-clean-worktree (dir)
  "Signal a user error unless DIR is a clean git worktree."
  (let ((default-directory dir))
    (with-temp-buffer
      (let ((exit (call-process "git" nil t nil
                                "status" "--porcelain")))
        (cond
         ((not (zerop exit))
          (user-error "Cannot inspect git worktree in %s: %s"
                      dir (string-trim (buffer-string))))
         ((> (buffer-size) 0)
          (user-error
           "Refusing audit auto-commit because %s has uncommitted changes"
           dir)))))))

;;;; Orchestration run view

(defvar-local agent-extras-run--file nil
  "Run file displayed in this buffer.")

(defvar-local agent-extras-run--timer nil
  "Refresh timer owned by this view.")

(defvar agent-extras-run--history nil
  "History of explicitly opened run files.")

(define-derived-mode agent-extras-run-mode special-mode "Agent Run"
  "Inspect a recorded orchestration run without controlling its actors.
Use \`g' to refresh and \`q' to quit.  Visible views refresh every five seconds."
  (setq-local revert-buffer-function #'agent-extras-run--revert)
  (add-hook 'kill-buffer-hook #'agent-extras-run--stop-timer nil t)
  (add-hook 'change-major-mode-hook #'agent-extras-run--stop-timer nil t))

(defun agent-extras-run-open (file)
  "Display the orchestrate-review run in FILE."
  (interactive (list (read-file-name "Orchestration run: " nil
                                     (car agent-extras-run--history) t)))
  (setq file (expand-file-name file))
  (agent-extras-run--read file)
  (add-to-history 'agent-extras-run--history file)
  (let ((buffer (get-buffer-create (format "*Agent Run: %s*" file))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-extras-run-mode) (agent-extras-run-mode))
      (setq agent-extras-run--file file)
      (agent-extras-run--revert)
      (unless (timerp agent-extras-run--timer)
        (setq agent-extras-run--timer
              (run-with-timer 5 5 #'agent-extras-run--refresh-visible buffer))))
    (pop-to-buffer buffer)))

(defun agent-extras-run--read (file)
  "Read supported display fields from FILE without evaluating its contents."
  (when (file-remote-p file) (user-error "Run views require a local file"))
  (with-temp-buffer
    (when (> (file-attribute-size (file-attributes file)) (* 4 1024 1024))
      (user-error "Run file exceeds the 4 MiB display limit"))
    (insert-file-contents file)
    (let ((run (json-parse-buffer :object-type 'alist :array-type 'list
                                  :null-object nil :false-object nil)))
      (unless (and (memq (alist-get 'version run) '(2 3))
                   (stringp (alist-get 'stage run))
                   (stringp (alist-get 'status run))
                   (stringp (alist-get 'repo run))
                   (listp (alist-get 'agent1 run))
                   (listp (alist-get 'agent2 run)))
        (user-error "Unsupported orchestration run format"))
      run)))

(defun agent-extras-run--revert (&rest _ignored)
  "Refresh this view from its run file, exposing read errors in the view."
  (let ((inhibit-read-only t)
        (position (point)))
    (erase-buffer)
    (condition-case error
        (agent-extras-run--render (agent-extras-run--read agent-extras-run--file))
      (error (insert "Run unavailable: " (error-message-string error) "\n"
                     "No cached state is displayed.  Press g to retry.\n")))
    (goto-char (min position (point-max)))))

(defun agent-extras-run--render (run)
  "Insert the recorded state of RUN into the current view."
  (let* ((status (alist-get 'status run))
         (phase (cond ((string-prefix-p "implementation-" status) "implementation")
                      ((equal status "complete") "complete")
                      (t (or (alist-get 'active_phase run)
                             (alist-get 'expected_phase run) "not recorded")))))
    (insert (propertize (format "Stage %s — %s\n\n" (alist-get 'stage run) phase)
                        'face 'bold)
            "Recorded status: " status "\n"
            "Attention: " (cond ((alist-get 'pending_submission run)
                                 "delivery needs reconciliation")
                                ((equal status "implementation-stopped")
                                 "implementation stopped; consult the orchestrator")
                                (t "no separate blocker recorded")) "\n\n")
    (agent-extras-run--actor "Author" (alist-get 'agent1 run))
    (agent-extras-run--actor "Reviewer" (alist-get 'agent2 run))
    (insert "\n")
    (agent-extras-run--progress)
    (insert "\nFiles\n")
    (agent-extras-run--file-link "Run record" agent-extras-run--file)
    (agent-extras-run--file-link "Run directory" (file-name-directory agent-extras-run--file))
    (agent-extras-run--file-link "Repository" (alist-get 'repo run))
    (insert "\nSpec/plan paths and blocker reasons are not stored by this run format.\n"
            "Read-only view; completion and verification remain the helper's responsibility.\n")))

(defun agent-extras-run--actor (label actor)
  "Insert LABEL and an identity-checked link for ACTOR."
  (let ((buffer (agent-extras-run--actor-buffer actor)))
    (insert label ": ")
    (if buffer
        (insert-text-button (buffer-name buffer) 'follow-link t
                            'action (lambda (_) (pop-to-buffer
                                                 (or (agent-extras-run--actor-buffer actor)
                                                     (user-error "Recorded session is no longer live")))))
      (insert (format "%s (recorded session unavailable)"
                      (or (alist-get 'buffer actor) "not recorded"))))
    (insert "\n")))

(defun agent-extras-run--actor-buffer (actor)
  "Return ACTOR's buffer only when its recorded identity still matches."
  (let* ((name (alist-get 'buffer actor))
         (buffer (and (stringp name) (get-buffer name)))
         (identity (alist-get 'identity actor))
         (session (and buffer (agent-session buffer))))
    (when (and session identity (memq buffer (agent-session-buffers))
               (equal (symbol-name (agent-session-backend session))
                      (alist-get 'backend identity))
               (equal (agent-session-id session) (alist-get 'session_id identity))
               (stringp (alist-get 'session_id identity))
               (agent-extras-run--same-directory-p (agent-session-directory session)
                                            (alist-get 'directory identity)))
      buffer)))

(defun agent-extras-run--same-directory-p (left right)
  "Return non-nil when local directory names LEFT and RIGHT resolve alike."
  (and (stringp left) (stringp right)
       (not (string-empty-p left)) (not (string-empty-p right))
       (not (file-remote-p left)) (not (file-remote-p right))
       (equal (directory-file-name (file-truename left))
              (directory-file-name (file-truename right)))))

(defun agent-extras-run--progress ()
  "Insert the latest nonblank progress line and its file age."
  (let* ((file (concat agent-extras-run--file ".progress"))
         (attributes (file-attributes file)))
    (if (not attributes)
        (insert "Progress: not published\n")
      (let* ((size (file-attribute-size attributes))
             (start (max 0 (- size 16384)))
             (latest (with-temp-buffer
                       (insert-file-contents file nil start size)
                       (when (> start 0) (delete-region (point-min)
                                                       (progn (goto-char (point-min))
                                                              (forward-line 1) (point))))
                       (car (last (split-string (buffer-string) "[\r\n]+" t "[ \t]+"))))))
        (insert "Progress: " (or latest "no nonblank line in the last 16 KiB") "\n"
                (format "Progress file updated %s ago\n"
                        (format-seconds "%hh %mm %ss"
                                        (max 0 (float-time
                                                (time-subtract nil
                                                 (file-attribute-modification-time attributes)))))))
        (agent-extras-run--file-link "Progress file" file)))))

(defun agent-extras-run--file-link (label file)
  "Insert a read-only navigation link labeled LABEL to local FILE."
  (insert-text-button label 'follow-link t
                      'action (lambda (_)
                                (when (file-remote-p file)
                                  (user-error "Run links require local paths"))
                                (unless (file-exists-p file)
                                  (user-error "Recorded path no longer exists: %s" file))
                                (if (file-directory-p file)
                                    (dired file)
                                  (let ((enable-local-variables nil)
                                        (enable-local-eval nil))
                                    (view-file file)))))
  (insert "\n"))

(defun agent-extras-run--refresh-visible (buffer)
  "Refresh BUFFER only while it is displayed."
  (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'agent-extras-run-mode) agent-extras-run--file)
        (agent-extras-run--revert)))))

(defun agent-extras-run--stop-timer ()
  "Cancel the refresh timer owned by this view."
  (when (timerp agent-extras-run--timer) (cancel-timer agent-extras-run--timer))
  (setq agent-extras-run--timer nil))

;;;; Menu

(defun agent-extras-add-menu-entries ()
  "Add the commands in this file to the Tools column of `agent-menu'."
  (dolist (entry '(("t r" . ("t a" "audit project" agent-extras-audit-project))
                   ("t a" . ("t c" "post-push CI" agent-extras-post-push-ci))
                   ("t c" . ("t o" "orchestration run" agent-extras-run-open))))
    (unless (ignore-errors (transient-get-suffix 'agent-menu (cadr entry)))
      (transient-append-suffix 'agent-menu (car entry) (cdr entry)))))

(agent-extras-add-menu-entries)

(provide 'agent-extras)
;;; agent-extras.el ends here
