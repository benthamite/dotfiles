;;; codex-extras.el --- Extensions for codex -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/codex-extras.el
;; Version: 0.1
;; Package-Requires: ((codex "0.1") (ai-extras "0.1"))

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

;; Extensions for `codex'.

;;; Code:

(require 'codex)
(eval-and-compile (require 'ai-extras))
(require 'subr-x)
(require 'transient)

;;;; Variables

(defgroup codex-extras ()
  "Extensions for `codex'."
  :group 'codex)

(defcustom codex-extras-sync-theme nil
  "Whether to sync the Codex theme with the current Emacs theme.
When non-nil, updates `~/.codex/config.toml' and sends `/theme' to
all running Codex sessions whenever `enable-theme-functions' fires."
  :type 'boolean
  :group 'codex-extras)

(defcustom codex-extras-handoff-file "/tmp/codex-handoff.md"
  "Path to the handoff file written by the handoff skill."
  :type 'file
  :group 'codex-extras)

(defcustom codex-extras-skill-directories nil
  "Additional directories to scan for Codex skills.
Searched in addition to the standard locations."
  :type '(repeat directory)
  :group 'codex-extras)

(defcustom codex-extras-run-skill-model nil
  "Model to use for `codex-extras-run-skill'.
When nil, use the CLI default."
  :type '(choice (const :tag "CLI default" nil) string)
  :group 'codex-extras)

(defcustom codex-extras-audit-skills
  '("/code-audit" "/design-audit" "/interpretability-audit")
  "Skills to run when performing a project audit."
  :type '(repeat string)
  :group 'codex-extras)

(defcustom codex-extras-audit-project-directories nil
  "Directories available for selection in `codex-extras-audit-project'."
  :type '(repeat directory)
  :group 'codex-extras)

(defcustom codex-extras-debug-backtrace-model 'gemini-flash-lite-latest
  "GPtel model for identifying candidate packages from a backtrace."
  :type 'symbol
  :group 'codex-extras)

(defcustom codex-extras-debug-backtrace-backend "Gemini"
  "GPtel backend name for backtrace analysis."
  :type 'string
  :group 'codex-extras)

(defvar codex-extras--last-synced-theme nil
  "The last theme value synced to Codex config.")

(defvar codex-extras--sync-theme-timer nil
  "Pending timer for deferred theme sync, or nil.")

(defvar-local codex-extras--pending-theme nil
  "Theme to apply when this Codex session becomes idle.")

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-tools)
(defvar gptel--known-backends)
(declare-function debug-save-backtrace "init" ())
(declare-function gptel-request "gptel")
(declare-function elpaca-get "elpaca")
(declare-function elpaca-source-dir "elpaca")
(declare-function find-library-name "find-func")
(declare-function paths-dir-downloads "paths")
(defvar paths-dir-downloads)

;;;; Backend registration

(ai-extras-register-backend 'codex
  (list :buffer-p #'codex--buffer-p
        :find-all-buffers #'codex--find-all-codex-buffers
        :find-buffers-for-dir #'codex--find-codex-buffers-for-directory
        :directory (lambda (buf) (with-current-buffer buf (codex--directory)))
        :extract-directory #'codex--extract-directory-from-buffer-name
        :extract-instance-name #'codex--extract-instance-name-from-buffer-name
        :send-command (lambda (cmd &optional _buf) (codex--do-send-command cmd))
        :start #'codex--start
        :start-new #'codex
        :program "codex"
        :send-return (lambda (&optional _buf)
                       (codex--term-send-return codex-terminal-backend))
        :icon (lambda () (if (require 'nerd-icons nil t)
                             (nerd-icons-mdicon "nf-md-hexagon_outline")
                           "CX"))
        :label "Codex"
        :handoff #'codex-extras-handoff
        :run-skill #'codex-extras-run-skill
        :audit-project #'codex-extras-audit-project
        :debug-backtrace #'codex-extras-debug-backtrace
        :setup-kill-on-exit #'codex-extras-setup-kill-on-exit))

;;;; Functions

;;;;; Theme sync

(defun codex-extras--emacs-theme ()
  "Return \"light\" or \"dark\" based on the current frame background mode."
  (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light"))

(defun codex-extras--sync-theme-to-config ()
  "Update `tui.theme' in `~/.codex/config.toml' to match Emacs.
Only writes the file when the theme value actually changes."
  (let* ((theme (codex-extras--emacs-theme))
         (config-file (expand-file-name "~/.codex/config.toml"))
         (new-line (format "theme = \"%s\"" theme)))
    (when (file-exists-p config-file)
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (let ((found nil))
          ;; Look for existing theme line under [tui] section
          (when (re-search-forward "^\\[tui\\]" nil t)
            (let ((section-end (save-excursion
                                 (if (re-search-forward "^\\[" nil t)
                                     (line-beginning-position)
                                   (point-max)))))
              (when (re-search-forward "^theme *= *\"[^\"]*\"" section-end t)
                (replace-match new-line)
                (setq found t))))
          ;; If not found, append under [tui] or create section
          (unless found
            (goto-char (point-min))
            (if (re-search-forward "^\\[tui\\]" nil t)
                (progn
                  (end-of-line)
                  (insert "\n" new-line))
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert "\n[tui]\n" new-line "\n"))))
        (write-region (point-min) (point-max) config-file nil 'silent)))))

(defun codex-extras--send-theme-to-buffer (buffer theme)
  "Send `/theme' to BUFFER and select THEME.
THEME is \"light\" or \"dark\"."
  (with-current-buffer buffer
    (setq ai-extras--waiting-for-input nil)
    (setq codex-extras--pending-theme nil)
    (let ((buf buffer))
      (codex--do-send-command "/theme")
      (run-at-time
       0.5 nil
       (lambda ()
         (when (and (buffer-live-p buf)
                    (process-live-p (get-buffer-process buf)))
           (with-current-buffer buf
             (let ((navigate (if (string= theme "light") "\e[B" "\e[A")))
               (codex--term-send-string codex-terminal-backend navigate)
               (run-at-time
                0.1 nil
                (lambda ()
                  (when (and (buffer-live-p buf)
                             (process-live-p (get-buffer-process buf)))
                    (with-current-buffer buf
                      (codex--term-send-string
                       codex-terminal-backend (kbd "RET"))))))))))))))

(defun codex-extras--apply-pending-theme (buffer)
  "Apply the pending theme to BUFFER, if any."
  (when-let* ((theme (buffer-local-value 'codex-extras--pending-theme buffer)))
    (when (string= theme (codex-extras--emacs-theme))
      (codex-extras--send-theme-to-buffer buffer theme))))

(defun codex-extras--do-sync-theme ()
  "Perform the actual theme sync."
  (setq codex-extras--sync-theme-timer nil)
  (let ((theme (codex-extras--emacs-theme)))
    (unless (equal theme codex-extras--last-synced-theme)
      (setq codex-extras--last-synced-theme theme)
      (codex-extras--sync-theme-to-config)
      (dolist (buf (codex--find-all-codex-buffers))
        (when (and (buffer-live-p buf)
                   (process-live-p (get-buffer-process buf)))
          (if (buffer-local-value 'ai-extras--waiting-for-input buf)
              (codex-extras--send-theme-to-buffer buf theme)
            (with-current-buffer buf
              (setq codex-extras--pending-theme theme))))))))

(defun codex-extras-sync-theme (&rest _)
  "Sync Codex theme with the current Emacs background mode."
  (when codex-extras-sync-theme
    (unless codex-extras--sync-theme-timer
      (setq codex-extras--sync-theme-timer
            (run-at-time 0 nil #'codex-extras--do-sync-theme)))))

;;;;; Notification handling

(defun codex-extras--handle-notification (message)
  "Handle a notification event from Codex CLI.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args.
The :type field is a string from the hook wrapper (e.g. \"Stop\")."
  (let ((hook-type (plist-get message :type)))
    (when (member hook-type '("Stop" "Notification" "SessionStart"))
      (when-let* ((buf (get-buffer (plist-get message :buffer-name))))
        (with-current-buffer buf
          (let ((name (ai-extras--session-name (buffer-name))))
            (pcase hook-type
              ("Stop"
               (setq ai-extras--waiting-for-input (current-time))
               (ai-extras-notify
                "Codex ready"
                (format "%s: waiting for your response" name))
               (ai-extras--scroll-to-bottom buf)
               (codex-extras--apply-pending-theme buf))
              ("Notification"
               (ai-extras-notify
                "Codex"
                (format "%s: needs your attention" name)))))))))
  nil)

;;;;; Skill runner

(defun codex-extras--discover-skills ()
  "Discover available Codex skills.
Scans the standard Codex skill directories and any custom ones
from `codex-extras-skill-directories'."
  (let ((skills (make-hash-table :test #'equal))
        (dirs (append
               ;; Standard Codex skill locations
               (list (expand-file-name "skills"
                                       (or (getenv "CODEX_HOME") "~/.codex")))
               ;; Project-local
               (when-let* ((proj (project-current)))
                 (list (expand-file-name ".agents/skills" (project-root proj))))
               ;; Custom
               codex-extras-skill-directories)))
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (file (file-expand-wildcards
                       (expand-file-name "*/SKILL.md" dir)))
          (when-let* ((meta (codex-extras--parse-skill-frontmatter file))
                      (name (plist-get meta :name)))
            (puthash name (append meta (list :path file :source dir))
                     skills)))))
    (let (result)
      (maphash (lambda (_name skill) (push skill result)) skills)
      (sort result (lambda (a b)
                     (string< (plist-get a :name) (plist-get b :name)))))))

(defun codex-extras--parse-skill-frontmatter (file)
  "Parse YAML frontmatter from skill FILE and return a plist."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (looking-at-p "---")
      (forward-line 1)
      (let ((start (point))
            (result nil))
        (when (re-search-forward "^---$" nil t)
          (let ((yaml (buffer-substring-no-properties start
                                                      (line-beginning-position))))
            (dolist (line (split-string yaml "\n" t))
              (when (string-match "^\\([a-z_-]+\\): *\\(.*\\)$" line)
                (let ((key (match-string 1 line))
                      (val (string-trim (match-string 2 line))))
                  (when (string-match "^[\"']\\(.*\\)[\"']$" val)
                    (setq val (match-string 1 val)))
                  (pcase key
                    ("name" (setq result (plist-put result :name val)))
                    ("description" (setq result (plist-put result :description val)))))))))
        result))))

;;;###autoload
(defun codex-extras-run-skill (skill-name &optional arguments)
  "Run Codex skill SKILL-NAME with optional ARGUMENTS.
Sends the skill invocation to the active Codex session."
  (interactive
   (let* ((skills (codex-extras--discover-skills))
          (_ (unless skills (user-error "No skills found")))
          (name (completing-read
                 "Skill: "
                 (mapcar (lambda (s) (plist-get s :name)) skills)
                 nil t))
          (args (read-string (format "Arguments for %s: " name))))
     (list name (unless (string-empty-p args) args))))
  (let* ((prompt (if arguments
                     (format "/%s %s" skill-name arguments)
                   (format "/%s" skill-name)))
         (buf (or (car (codex--find-all-codex-buffers))
                  (user-error "No running Codex session"))))
    (with-current-buffer buf
      (codex--do-send-command prompt))
    (display-buffer buf)))

;;;;; Project audit

;;;###autoload
(defun codex-extras-audit-project ()
  "Run a comprehensive audit of a project via Codex.
Sequentially sends each skill in `codex-extras-audit-skills'
to a Codex session."
  (interactive)
  (let* ((dir (codex-extras--read-audit-directory))
         (skills codex-extras-audit-skills))
    (when (yes-or-no-p
           (format "Run %d audit(s) on %s?" (length skills) dir))
      (let ((buf (or (car (codex--find-codex-buffers-for-directory dir))
                     (let ((default-directory dir)) (codex)))))
        (dolist (skill skills)
          (with-current-buffer buf
            (codex--do-send-command (format "%s --accept" skill))))))))

(defun codex-extras--read-audit-directory ()
  "Prompt for a project directory with completion."
  (let* ((candidates (mapcar #'abbreviate-file-name
                             codex-extras-audit-project-directories))
         (input (completing-read "Project directory: " candidates nil nil))
         (dir (file-truename (expand-file-name input))))
    (unless (file-directory-p dir)
      (user-error "Not a directory: %s" dir))
    (unless (member dir (mapcar #'file-truename
                                codex-extras-audit-project-directories))
      (customize-save-variable 'codex-extras-audit-project-directories
                               (append codex-extras-audit-project-directories
                                       (list dir))))
    dir))

;;;;; Debug backtrace

;;;###autoload
(defun codex-extras-debug-backtrace ()
  "Save the backtrace, identify the offending package, and open Codex."
  (interactive)
  (let ((backtrace-file (expand-file-name "backtrace.el" paths-dir-downloads)))
    (run-with-timer 0 nil #'codex-extras--debug-identify-package backtrace-file)
    (debug-save-backtrace)))

(defun codex-extras--debug-identify-package (backtrace-file)
  "Identify candidate packages from BACKTRACE-FILE and let the user choose."
  (unless (file-exists-p backtrace-file)
    (user-error "Backtrace file not found: %s" backtrace-file))
  (message "Identifying packages from backtrace...")
  (let ((contents (with-temp-buffer
                    (insert-file-contents backtrace-file)
                    (buffer-string)))
        (gptel-backend (alist-get codex-extras-debug-backtrace-backend
                                 gptel--known-backends nil nil #'string=))
        (gptel-model codex-extras-debug-backtrace-model)
        (gptel-use-tools nil))
    (gptel-request
     (format "Backtrace file: %s\n\nContents:\n%s" backtrace-file contents)
     :system "You are an Emacs expert. Given the backtrace, identify ALL Emacs packages that appear in the stack trace and could be the root cause of the error. Return ONLY a comma-separated list of package names."
     :callback
     (lambda (response info)
       (if (not response)
           (message "gptel request failed: %s" (plist-get info :status))
         (let* ((candidates (mapcar #'string-trim (split-string response ",")))
                (selected (completing-read "Package to debug: " candidates nil nil nil nil
                                           (car candidates))))
           (codex-extras--debug-start-session
            (intern selected) backtrace-file)))))))

(defun codex-extras--debug-start-session (package backtrace-file)
  "Start a Codex session for PACKAGE with BACKTRACE-FILE."
  (let* ((elpaca-entry (and (fboundp 'elpaca-get) (elpaca-get package)))
         (dir (cond
               (elpaca-entry (elpaca-source-dir elpaca-entry))
               ((condition-case nil
                    (file-name-directory (find-library-name (symbol-name package)))
                  (error nil)))
               (t (user-error "Package `%s' not found" package))))
         (prompt (format "Read the backtrace at %s. Identify the bug, fix it, and commit the fix."
                         backtrace-file)))
    (message "Starting Codex for `%s' in %s..." package dir)
    (cl-letf (((symbol-function 'codex--directory) (lambda () dir)))
      (codex--start nil (list prompt) nil t))))

;;;;; Handoff

;;;###autoload
(defun codex-extras-handoff ()
  "Close this Codex session and start a new one with the handoff prompt."
  (interactive)
  (unless (file-exists-p codex-extras-handoff-file)
    (user-error "No handoff file at %s — run /handoff first"
                codex-extras-handoff-file))
  (let* ((prompt (with-temp-buffer
                   (insert-file-contents codex-extras-handoff-file)
                   (string-trim (buffer-string))))
         (dir (if (codex--buffer-p (current-buffer))
                  default-directory
                (codex--directory))))
    (when (string-empty-p prompt)
      (user-error "Handoff file is empty"))
    (when (codex--buffer-p (current-buffer))
      (let ((kill-buffer-query-functions
             (remq 'ai-extras-protect-buffer kill-buffer-query-functions)))
        (kill-buffer (current-buffer))))
    (cl-letf (((symbol-function 'codex--directory) (lambda () dir)))
      (codex--start nil (list prompt) nil t))))

;;;;; Start or switch (Codex-specific entry point)

;;;###autoload
(defun codex-extras-start-or-switch ()
  "Start a new Codex session or switch to an existing one.
If no Codex sessions exist, start a new one.  Otherwise, show the
unified session switcher."
  (interactive)
  (if (null (codex--find-all-codex-buffers))
      (codex)
    (ai-extras--ensure-all-session-keys)
    (transient-setup 'ai-extras--session-switcher)))

;;;; Hooks

(add-hook 'codex-event-hook #'codex-extras--handle-notification)
(add-hook 'kill-buffer-query-functions #'ai-extras-protect-buffer)
(add-hook 'codex-start-hook #'ai-extras--assign-session-key)
(add-hook 'codex-start-hook #'ai-extras--refresh-display-names)
(add-hook 'codex-start-hook #'ai-extras-disable-scrollback-truncation)
(add-hook 'codex-start-hook #'ai-extras-setup-snippet-keys)
(add-hook 'codex-start-hook #'ai-extras-fix-rendering)
(add-hook 'codex-start-hook #'codex-extras-sync-theme)
(add-hook 'kill-buffer-hook #'ai-extras--release-session-key)
(add-hook 'kill-buffer-hook #'ai-extras--refresh-display-names)
(add-hook 'enable-theme-functions #'codex-extras-sync-theme)
(advice-add 'codex--do-send-command :before
            #'ai-extras--clear-waiting-for-input)

;;;; Extend unified menu

(transient-define-infix codex-extras--infix-sync-theme ()
  "Toggle `codex-extras-sync-theme'."
  :class 'ai-extras--boolean-variable
  :variable 'codex-extras-sync-theme
  :description "sync theme (codex)")

(with-eval-after-load 'ai-extras
  (transient-append-suffix 'ai-extras-menu '(1 1 -1)
    '("-t" codex-extras--infix-sync-theme)))

;;;;; Kill on exit

(defun codex-extras-setup-kill-on-exit ()
  "Arrange for the buffer to be killed when the Codex process exits."
  (interactive)
  (when (codex--buffer-p (current-buffer))
    (when-let* ((proc (get-buffer-process (current-buffer))))
      (let ((orig (process-sentinel proc))
            (buf (current-buffer)))
        (set-process-sentinel
         proc
         (lambda (process event)
           (when orig (funcall orig process event))
           (when (buffer-live-p buf)
             (kill-buffer buf))))))))

(add-hook 'codex-start-hook #'codex-extras-setup-kill-on-exit)

;;;; Provide

(provide 'codex-extras)
;;; codex-extras.el ends here
