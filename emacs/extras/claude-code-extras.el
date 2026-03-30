;;; claude-code-extras.el --- Extensions for claude-code -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/claude-code-extras.el
;; Version: 0.1
;; Package-Requires: ((claude-code "0.1") (consult "1.0") (paths "0.1"))

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

;; Extensions for `claude-code'.

;;; Code:

(require 'claude-code)
(require 'consult)
(require 'paths)
(require 'transient)

;;;; Variables

(defgroup claude-code-extras ()
  "Extensions for `claude-code'."
  :group 'claude-code)

(defcustom claude-code-extras-protect-buffers t
  "When non-nil, prompt for confirmation before killing claude-code buffers."
  :type 'boolean
  :group 'claude-code-extras)

(defcustom claude-code-extras-warn-kill-with-branches t
  "When non-nil, warn before killing a session that has branches.
If the session being killed is the root of a branch tree with
more than one member, a second confirmation prompt is shown after
the standard kill-protection prompt."
  :type 'boolean
  :group 'claude-code-extras)

(defcustom claude-code-extras-log-directory
  (expand-file-name "claude-logs" paths-dir-notes)
  "Directory where Claude conversation logs are saved."
  :type 'directory
  :group 'claude-code-extras)

(defcustom claude-code-extras-sync-theme nil
  "Whether to sync the Claude Code theme with the current Emacs theme.
When non-nil, updates `~/.claude/settings.json' and sends `/theme' to all
running Claude Code sessions whenever `enable-theme-functions' fires."
  :type 'boolean
  :group 'claude-code-extras)

(defcustom claude-code-extras-sigwinch-delay 0.5
  "Delay in seconds before sending SIGWINCH to fix terminal rendering.
After Claude Code starts, the TUI may render incorrectly due to a
race condition in terminal size negotiation.  Sending SIGWINCH
forces the TUI to re-query terminal dimensions and redraw."
  :type 'number
  :group 'claude-code-extras)

(defcustom claude-code-extras-alert-style 'both
  "Style of alert when Claude finishes responding.
Only takes effect when `claude-code-extras-alert-on-ready' is
non-nil in the current buffer."
  :type '(choice (const :tag "Visual notification only" visual)
                 (const :tag "Sound only" sound)
                 (const :tag "Both visual and sound" both))
  :group 'claude-code-extras)

(defcustom claude-code-extras-alert-sound
  "/System/Library/Sounds/Glass.aiff"
  "Path to the sound file played when Claude finishes responding.
Only used when `claude-code-extras-alert-style' is `sound' or
`both'."
  :type 'file
  :group 'claude-code-extras)

(defcustom claude-code-extras-alert-on-ready nil
  "When non-nil, alert the user when Claude finishes responding.
Toggle with `claude-code-extras-toggle-alert'."
  :type 'boolean
  :group 'claude-code-extras)

(defcustom claude-code-extras-status-interval 5
  "Interval in seconds between status file polls."
  :type 'integer
  :group 'claude-code-extras)

(defcustom claude-code-extras-copilot-enabled nil
  "When non-nil, enable `copilot-mode' in Claude Code buffers.
Copilot's ghost-text prose completions are shown while typing
prompts and accepted text is sent to the terminal correctly."
  :type 'boolean
  :group 'claude-code-extras)

(defcustom claude-code-extras-accounts nil
  "Alist of account names to `CLAUDE_CONFIG_DIR' paths.
Each entry is (NAME . CONFIG-DIR).  When non-nil,
`claude-code-extras-start-or-switch' uses the persisted account
selection and sets `CLAUDE_CONFIG_DIR' accordingly so each account
maintains its own OAuth credentials.

Use `claude-code-extras-select-account' to change the active account.
The selection persists in `claude-code-extras-account-file'.

Example:
  \\='((\"personal\" . \"~/.claude-personal\")
    (\"work\"     . \"~/.claude-work\"))"
  :type '(alist :key-type string :value-type directory)
  :group 'claude-code-extras)

(defcustom claude-code-extras-account-file
  (expand-file-name ".claude-current-account" "~")
  "File storing the name of the currently active Claude account.
The file contains a single account name from `claude-code-extras-accounts'.
Written by `claude-code-extras-select-account', read at session start."
  :type 'file
  :group 'claude-code-extras)

(defvar claude-code-extras--current-account nil
  "Currently active Claude account name.
Loaded from `claude-code-extras-account-file' on first use;
changed by `claude-code-extras-select-account'.")

(defvar claude-code-extras--pending-account nil
  "Account name for the current `claude-code' invocation.
Dynamically bound by `claude-code-extras--start-with-account';
read by `claude-code-extras--account-env'.")

(defvar-local claude-code-extras--buffer-account nil
  "Account name that was active when this buffer's session started.
Set by `claude-code-extras--capture-buffer-account' via
`claude-code-start-hook'.")

(defconst claude-code-extras--hook-wrapper
  (expand-file-name
   "bin/claude-code-hook-wrapper"
   (expand-file-name "../../repos/claude-code"
                     (file-name-directory
                      (locate-library "claude-code"))))
  "Absolute path to the claude-code hook wrapper script.")

(defconst claude-code-extras--hooks-directory
  (file-truename
   (expand-file-name "../../claude/hooks/"
                     (file-name-directory
                      (file-truename
                       (or load-file-name buffer-file-name)))))
  "Absolute path to the dotfiles hooks directory.")

(defconst claude-code-extras--status-directory "/tmp/claude-code-status/"
  "Directory where the statusline script writes JSON status files.")

(defconst claude-code-extras--statusline-script
  (expand-file-name "etc/claude-code-statusline.sh"
                    (file-name-directory
                     (file-truename
                      (or load-file-name buffer-file-name))))
  "Absolute path to the statusline shell script.")

(defvar-local claude-code-extras--status-data nil
  "Parsed status plist for the current Claude buffer.")

(defvar-local claude-code-extras--display-name-cache nil
  "Cached display name for the modeline.
Updated by `claude-code-extras--refresh-display-names' whenever
sessions are created or destroyed.")

(defvar-local claude-code-extras--original-session-id nil
  "Session ID when this buffer was first created.
Used to detect when `/branch' creates a new session.")

(defconst claude-code-extras--home-row-keys '("a" "s" "d" "f" "j" "k" "l" ";")
  "Home row keys assigned to Claude sessions, in allocation order.
When a session is killed its key becomes available for the next new session.")

(defvar claude-code-extras--session-keys (make-hash-table :test 'eq)
  "Map from live Claude buffer to its assigned home-row key.")

(defvar-local claude-code-extras--status-timer nil
  "Timer for periodic status polling in the current Claude buffer.")

(defvar-local claude-code-extras--waiting-for-input nil
  "Non-nil when this Claude session is waiting for user input.
Set to t by the Notification hook (idle_prompt) and cleared when
input is sent to the session (e.g. a `/theme' command).")

(defvar-local claude-code-extras--pending-theme nil
  "Theme to apply when this Claude session becomes idle.
Set by `claude-code-extras-sync-theme' when the session is busy;
consumed by the Stop hook handler.")

(defvar-local claude-code-extras--copilot-active nil
  "Non-nil when Copilot integration is active in the current buffer.")

(defvar-local claude-code-extras--copilot-change-timer nil
  "Debounce timer for triggering Copilot after eat buffer changes.")

(defvar eat-terminal)
(declare-function json-pretty-print-buffer "json" ())
(declare-function org-map-entries "org" (func &optional match scope &rest skip))
(declare-function org-get-todo-state "org" ())
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-end-of-meta-data "org" (&optional full))
(declare-function org-entry-is-done-p "org" ())
(declare-function outline-next-heading "outline" ())
(declare-function eat-self-input "eat" (n &optional e))
(declare-function eat-term-display-cursor "eat" (terminal))
(declare-function eat-term-send-string "eat" (terminal string))

(declare-function map-values "map")
(declare-function consult-yasnippet--candidates "consult-yasnippet")
(declare-function consult-yasnippet--annotate "consult-yasnippet")
(declare-function yas--template-content "yasnippet")
(declare-function yas--template-expand-env "yasnippet")
(declare-function yas--all-templates "yasnippet")
(declare-function yas--get-snippet-tables "yasnippet")
(declare-function yas-minor-mode "yasnippet")
(declare-function yas-expand-snippet "yasnippet")
(declare-function yas-active-snippets "yasnippet")
(declare-function yas--commit-snippet "yasnippet")
(declare-function yas--template-key "yasnippet")

(defvar yas-minor-mode)
(defvar yas-prompt-functions)
(defvar yas--tables)

(defvar copilot-mode)
(defvar copilot--overlay)
(defvar copilot--connection)
(defvar copilot--doc-version)
(defvar copilot--track-changes-id)
(defvar copilot-disable-predicates)
(declare-function copilot-mode "copilot")
(declare-function copilot-accept-completion "copilot")
(declare-function copilot-complete "copilot")
(declare-function copilot-clear-overlay "copilot")
(declare-function copilot--overlay-visible "copilot")
(declare-function copilot--get-language-id "copilot")
(declare-function copilot--get-uri "copilot")
(declare-function copilot--get-source "copilot")
(declare-function copilot--connection-alivep "copilot")
(declare-function copilot--post-command "copilot")
(declare-function copilot--show-completion "copilot")
(declare-function copilot--set-overlay-text "copilot")
(declare-function track-changes-unregister "track-changes")
(declare-function jsonrpc-notify "jsonrpc")
(declare-function jsonrpc-async-request "jsonrpc")

;;;; Functions

;;;;; C-g fix

(defun claude-code-extras--send-escape-in-current-buffer (orig-fn)
  "When already in a Claude buffer, send escape directly without prompting.
ORIG-FN is `claude-code-send-escape'.  The upstream implementation uses
`claude-code--with-buffer', which re-resolves the target buffer via
`claude-code--get-or-prompt-for-buffer'.  When multiple sessions share
the same project directory, that triggers a selection prompt—defeating
the purpose of C-g as a quick interrupt.  This advice short-circuits
the lookup: if the current buffer is already a Claude buffer, send the
escape sequence directly to it."
  (if (claude-code--buffer-p (current-buffer))
      (claude-code--term-send-string claude-code-terminal-backend (kbd "ESC"))
    (funcall orig-fn)))

(advice-add 'claude-code-send-escape :around
            #'claude-code-extras--send-escape-in-current-buffer)

;;;;; Snippet insertion

(defun claude-code-extras--expand-snippet-to-text (template)
  "Expand yasnippet TEMPLATE to plain text in a temporary buffer."
  (with-temp-buffer
    (yas-minor-mode 1)
    (let ((yas-prompt-functions '(yas-no-prompt)))
      (yas-expand-snippet (yas--template-content template)
                          nil nil
                          (yas--template-expand-env template)))
    (mapc #'yas--commit-snippet (yas-active-snippets))
    (buffer-string)))

(defun claude-code-extras--consult-yasnippet (orig-fn arg)
  "In eat-mode buffers, send snippet content via the terminal.
`consult-yasnippet' manipulates buffer contents directly for
previews and cleanup, which fails in eat-mode because the
terminal emulator manages the buffer.  This advice bypasses
the problematic state function and sends the expanded snippet
text through `eat-term-send-string' instead.
ORIG-FN is `consult-yasnippet'; ARG is the prefix argument."
  (if (not (derived-mode-p 'eat-mode))
      (funcall orig-fn arg)
    (let* ((candidates
            (consult-yasnippet--candidates
             (if arg
                 (progn (require 'map)
                        (yas--all-templates (map-values yas--tables)))
               (yas--all-templates (yas--get-snippet-tables)))))
           (template
            (consult--read
             candidates
             :prompt "Choose a snippet: "
             :annotate (consult-yasnippet--annotate candidates)
             :lookup 'consult--lookup-cdr
             :require-match t
             :group 'consult--prefix-group
             :category 'yasnippet)))
      (when template
        (let* ((expanded (claude-code-extras--expand-snippet-to-text template))
               (text (replace-regexp-in-string "\n" "\e\r" expanded)))
          (eat-term-send-string eat-terminal text))))))

(advice-add 'consult-yasnippet :around
            #'claude-code-extras--consult-yasnippet)

(defun claude-code-extras--try-expand-snippet-at-prompt ()
  "Try to expand a yasnippet key at the eat terminal prompt.
Search backward from `point-max' for the prompt marker, extract
the user's input, and check whether it ends with a snippet key.
If a match is found, send backspaces to erase the key and then
send the expanded snippet text.  Return non-nil if a snippet was
expanded."
  (when (and (derived-mode-p 'eat-mode)
             (bound-and-true-p eat-terminal)
             (bound-and-true-p yas-minor-mode))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "^❯[[:space:]]" nil t)
        (let* ((prompt-start (match-end 0))
               (prompt-end (progn (end-of-line) (point)))
               (input (string-trim-right
                       (buffer-substring-no-properties prompt-start prompt-end)))
               (templates (yas--all-templates (yas--get-snippet-tables)))
               (best-match nil)
               (best-key nil))
          (dolist (template templates)
            (let ((key (yas--template-key template)))
              (when (and key
                         (> (length key) 0)
                         (<= (length key) (length input))
                         (string= key (substring input (- (length input) (length key))))
                         (or (null best-key)
                             (> (length key) (length best-key))))
                (setq best-match template
                      best-key key))))
          (when best-match
            (eat-term-send-string eat-terminal
                                  (make-string (length best-key) ?\x7f))
            (let* ((expanded (claude-code-extras--expand-snippet-to-text best-match))
                   (text (replace-regexp-in-string "\n" "\e\r" expanded)))
              (eat-term-send-string eat-terminal text))
            t))))))

(defun claude-code-extras-snippet-tab ()
  "Try snippet expansion at prompt, otherwise send TAB to eat."
  (interactive)
  (unless (claude-code-extras--try-expand-snippet-at-prompt)
    (eat-self-input 1 ?\t)))

(defvar claude-code-extras--snippet-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'claude-code-extras-snippet-tab)
    (define-key map [tab] #'claude-code-extras-snippet-tab)
    map)
  "Keymap for `claude-code-extras--snippet-keys-mode'.")

(define-minor-mode claude-code-extras--snippet-keys-mode
  "Minor mode providing yasnippet TAB expansion in Claude Code buffers."
  :keymap claude-code-extras--snippet-keys-mode-map)

(defun claude-code-extras-setup-snippet-keys ()
  "Enable yasnippet TAB expansion in the current Claude Code buffer.
Only activates when Copilot is not enabled, since the Copilot TAB
handler already chains through snippet expansion."
  (when (and (claude-code--buffer-p (current-buffer))
             (bound-and-true-p eat-terminal)
             (not claude-code-extras-copilot-enabled))
    (claude-code-extras--snippet-keys-mode 1)))

;;;;; Buffer protection

(defun claude-code-extras-protect-buffer ()
  "Prompt for confirmation before killing claude-code buffers.
Returns t if the buffer should be killed, nil otherwise.  Skips
the prompt when the session process has already exited (e.g. via
/exit).  Intended for use in `kill-buffer-query-functions'."
  (or (not claude-code-extras-protect-buffers)
      (not (claude-code--buffer-p (current-buffer)))
      (not (process-live-p (get-buffer-process (current-buffer))))
      (yes-or-no-p "Kill claude-code buffer? ")))

(defun claude-code-extras--confirm-kill-branches ()
  "Return t unless the current session has branches and user declines.
Reads the status file to find the session ID and project
directory, then does a fast header-only scan to check for
branches.  Returns t (allow kill) if the session has no branches,
if the status file is unavailable, or if the user confirms."
  (condition-case nil
      (let ((status (claude-code-extras--parse-status-file)))
        (if (not status)
            t
          (let ((sid (plist-get status :session_id))
                (transcript (plist-get status :transcript_path)))
            (if (not (and sid transcript))
                t
              (let* ((project-dir (file-name-directory transcript))
                     (headers (claude-code-extras--scan-session-headers project-dir))
                     (children-map (claude-code-extras--build-children-map headers))
                     (members (claude-code-extras--collect-tree-members sid children-map))
                     (branch-count (1- (hash-table-count members))))
                (if (<= branch-count 0)
                    t
                  (yes-or-no-p
                   (format "Session has %d %s — kill anyway? "
                           branch-count
                           (if (= branch-count 1) "branch" "branches")))))))))
    (error t)))

(defun claude-code-extras-setup-kill-on-exit ()
  "Arrange for the buffer to be killed when the Claude process exits.
Works with any terminal backend by wrapping the process sentinel.
When `claude-code-extras-warn-kill-with-branches' is non-nil and
the session has branches, prompts for confirmation before killing."
  (interactive)
  (when (claude-code--buffer-p (current-buffer))
    (when-let* ((proc (get-buffer-process (current-buffer))))
      (let ((orig (process-sentinel proc))
            (buf (current-buffer)))
        (set-process-sentinel
         proc
         (lambda (process event)
           (when orig
             (funcall orig process event))
           (when (and (buffer-live-p buf)
                      (with-current-buffer buf
                        (claude-code-extras--confirm-kill-branches)))
             (kill-buffer buf))))))))

(defun claude-code-extras-fix-rendering ()
  "Send SIGWINCH to fix terminal rendering after startup.
Works around a race condition where Claude Code's TUI queries
terminal dimensions before the terminal window is fully laid out,
resulting in a garbled banner."
  (interactive)
  (when-let* ((proc (get-buffer-process (current-buffer))))
    (claude-code-extras--send-sigwinch-after-delay (current-buffer))))

(defun claude-code-extras--send-sigwinch-after-delay (buffer)
  "Send SIGWINCH to the process in BUFFER after a short delay."
  (run-at-time claude-code-extras-sigwinch-delay nil
               #'claude-code-extras--send-sigwinch buffer))

(defun claude-code-extras--send-sigwinch (buffer)
  "Send SIGWINCH to the process in BUFFER."
  (when (buffer-live-p buffer)
    (when-let* ((proc (get-buffer-process buffer)))
      (signal-process proc 'SIGWINCH))))

;;;;; Smart start

(defun claude-code-extras--account-env (_buffer-name _dir)
  "Return `CLAUDE_CONFIG_DIR' for the pending account.
Reads `claude-code-extras--pending-account', which is dynamically
bound by `claude-code-extras--start-with-account'."
  (when-let* ((account claude-code-extras--pending-account)
              (config-dir (alist-get account claude-code-extras-accounts
                                     nil nil #'string=)))
    (list (format "CLAUDE_CONFIG_DIR=%s" (expand-file-name config-dir)))))

(defun claude-code-extras--sync-account-config (account)
  "Sync shared state into ACCOUNT's `.claude.json'.
Merges the `projects' key from the canonical `~/.claude.json' and
all account configs so folder trust decisions are available
everywhere.  Without this, each account's config dir looks like a
fresh install and re-prompts for folder trust.

Only writes the file when the merged projects actually differ from
what is already on disk, to avoid triggering file-change detection
in running Claude Code sessions."
  (when-let* ((config-dir (alist-get account claude-code-extras-accounts
                                     nil nil #'string=))
              (target-path (expand-file-name
                            ".claude.json" (expand-file-name config-dir))))
    (condition-case err
        (let* ((target (claude-code-extras--read-claude-json target-path))
               (merged (claude-code-extras--collect-all-projects)))
          (when (and target (> (hash-table-count merged) 0))
            (let ((existing (gethash "projects" target)))
              (unless (equal (json-serialize existing)
                             (json-serialize merged))
                (puthash "projects" merged target)
                (claude-code-extras--write-claude-json target-path target)))))
      (error
       (message "claude-code-extras: failed to sync account config: %S" err)))))

(defun claude-code-extras--read-claude-json (path)
  "Read and parse the JSON file at PATH.
Return a hash table, or nil if PATH does not exist or is invalid."
  (when (file-exists-p path)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents path)
          (json-parse-buffer))
      (error nil))))

(defun claude-code-extras--collect-all-projects ()
  "Collect and merge `projects' from all `.claude.json' sources.
Reads the canonical `~/.claude.json' first, then each account
config.  For duplicate keys, prefers entries where
`hasTrustDialogAccepted' is true."
  (let ((merged (make-hash-table :test #'equal))
        (paths (claude-code-extras--all-claude-json-paths)))
    (dolist (path paths)
      (when-let* ((data (claude-code-extras--read-claude-json path))
                  (projects (gethash "projects" data)))
        (when (hash-table-p projects)
          (maphash (lambda (key val)
                     (claude-code-extras--merge-project merged key val))
                   projects))))
    merged))

(defun claude-code-extras--all-claude-json-paths ()
  "Return paths to the canonical and all account `.claude.json' files."
  (cons (expand-file-name ".claude.json" "~")
        (mapcar (lambda (entry)
                  (expand-file-name ".claude.json"
                                    (expand-file-name (cdr entry))))
                claude-code-extras-accounts)))

(defun claude-code-extras--merge-project (table key val)
  "Merge project VAL under KEY into TABLE.
Prefers entries where `hasTrustDialogAccepted' is true."
  (let ((existing (gethash key table)))
    (cond
     ((not existing)
      (puthash key val table))
     ((and (hash-table-p val)
           (eq (gethash "hasTrustDialogAccepted" val) t)
           (not (eq (gethash "hasTrustDialogAccepted" existing) t)))
      (puthash key val table)))))

(defun claude-code-extras--write-claude-json (path data)
  "Write DATA as pretty-printed JSON to PATH."
  (require 'json)
  (with-temp-file path
    (insert (json-serialize data))
    (json-pretty-print-buffer)))

(defun claude-code-extras--load-account ()
  "Load the current account from `claude-code-extras-account-file'.
Return the account name, or nil if the file is missing or stale."
  (when (file-exists-p claude-code-extras-account-file)
    (let ((name (string-trim
                 (with-temp-buffer
                   (insert-file-contents claude-code-extras-account-file)
                   (buffer-string)))))
      (when (alist-get name claude-code-extras-accounts nil nil #'string=)
        name))))

(defun claude-code-extras--save-account (name)
  "Persist NAME as the active account to `claude-code-extras-account-file'."
  (with-temp-file claude-code-extras-account-file
    (insert name "\n"))
  (setq claude-code-extras--current-account name))

(defun claude-code-extras--prompt-account ()
  "Prompt for an account from `claude-code-extras-accounts'.
Return the account name, or nil."
  (when claude-code-extras-accounts
    (let ((names (mapcar #'car claude-code-extras-accounts)))
      (if (= (length names) 1)
          (car names)
        (completing-read "Account: " names nil t)))))

(defun claude-code-extras--resolve-account ()
  "Return the active account, loading from disk or prompting as needed.
On first use, loads from `claude-code-extras-account-file'.  If no
persisted account exists, prompts once and saves the selection."
  (when claude-code-extras-accounts
    (unless claude-code-extras--current-account
      (setq claude-code-extras--current-account
            (claude-code-extras--load-account)))
    (or claude-code-extras--current-account
        (let ((account (claude-code-extras--prompt-account)))
          (when account
            (claude-code-extras--save-account account))
          account))))

;;;###autoload
(defun claude-code-extras-select-account ()
  "Switch the active Claude account.
Prompts for an account from `claude-code-extras-accounts' and
persists the selection.  New sessions will use this account."
  (interactive)
  (unless claude-code-extras-accounts
    (user-error "No accounts configured in `claude-code-extras-accounts'"))
  (let ((account (claude-code-extras--prompt-account)))
    (when account
      (claude-code-extras--save-account account)
      (claude-code-extras--sync-account-config account)
      (message "Switched to account: %s" account))))

(defun claude-code-extras--start-with-account ()
  "Start a new Claude session using the active account."
  (interactive)
  (let ((claude-code-extras--pending-account
         (claude-code-extras--resolve-account)))
    (when claude-code-extras--pending-account
      (claude-code-extras--sync-account-config
       claude-code-extras--pending-account))
    (claude-code)))

;;;###autoload
(defun claude-code-extras-start-or-switch ()
  "Start a new Claude session or switch to an existing one.
If no sessions are active, start a new one.  If sessions exist,
show a transient menu with home-row keys for quick switching."
  (interactive)
  (if (null (claude-code--find-all-claude-buffers))
      (claude-code-extras--start-with-account)
    (claude-code-extras--ensure-all-session-keys)
    (transient-setup 'claude-code-extras--session-switcher)))

(defun claude-code-extras--purge-dead-session-keys ()
  "Remove entries for buffers that are no longer live."
  (let (dead)
    (maphash (lambda (buf _) (unless (buffer-live-p buf) (push buf dead)))
             claude-code-extras--session-keys)
    (dolist (buf dead)
      (remhash buf claude-code-extras--session-keys))))

(defun claude-code-extras--assign-session-key ()
  "Assign a home-row key to the current Claude buffer."
  (when (claude-code--buffer-p (current-buffer))
    (unless (gethash (current-buffer) claude-code-extras--session-keys)
      (claude-code-extras--purge-dead-session-keys)
      (let ((used (hash-table-values claude-code-extras--session-keys)))
        (when-let* ((key (cl-find-if (lambda (k) (not (member k used)))
                                      claude-code-extras--home-row-keys)))
          (puthash (current-buffer) key claude-code-extras--session-keys))))))

(defun claude-code-extras--release-session-key ()
  "Release the home-row key for the current Claude buffer."
  (remhash (current-buffer) claude-code-extras--session-keys))

(defun claude-code-extras--ensure-all-session-keys ()
  "Ensure every active Claude buffer has a home-row key.
Assigns keys to any sessions that were started before the key
system was loaded."
  (claude-code-extras--purge-dead-session-keys)
  (dolist (buf (claude-code--find-all-claude-buffers))
    (unless (gethash buf claude-code-extras--session-keys)
      (let ((used (hash-table-values claude-code-extras--session-keys)))
        (when-let* ((key (cl-find-if (lambda (k) (not (member k used)))
                                      claude-code-extras--home-row-keys)))
          (puthash buf key claude-code-extras--session-keys))))))

(transient-define-prefix claude-code-extras--session-switcher ()
  "Switch to a Claude session or start a new one."
  ["Sessions"
   :class transient-column
   :setup-children claude-code-extras--session-switcher-children])

(defun claude-code-extras--session-switcher-children (_)
  "Build transient suffixes for the session switcher."
  (let (specs)
    (maphash
     (lambda (buf key)
       (when (buffer-live-p buf)
         (let ((cmd (make-symbol (format "claude-switch-%s" key))))
           (fset cmd (lambda () (interactive) (switch-to-buffer buf)))
           (push (list key (claude-code-extras-display-name buf) cmd)
                 specs))))
     claude-code-extras--session-keys)
    (setq specs
          (sort specs
                (lambda (a b)
                  (< (claude-code-extras--home-row-key-index (car a))
                     (claude-code-extras--home-row-key-index (car b))))))
    (setq specs (append specs
                        (list '("n" "new session"
                                claude-code-extras--start-with-account))))
    (transient-parse-suffixes
     'claude-code-extras--session-switcher
     (apply #'vector specs))))

(defun claude-code-extras--home-row-key-index (key)
  "Return the index of KEY in `claude-code-extras--home-row-keys'."
  (or (cl-position key claude-code-extras--home-row-keys :test #'string=) 99))

(defun claude-code-extras--buffer-session-name (buffer)
  "Return the session name for BUFFER."
  (claude-code-extras--session-name (buffer-name buffer)))

(defun claude-code-extras--qualified-session-name (buffer-name)
  "Return a qualified session name from BUFFER-NAME.
Given \"*claude:~/path/to/project/:instance*\", return
\"project:instance\"."
  (let ((project (claude-code-extras--session-name buffer-name))
        (instance (claude-code--extract-instance-name-from-buffer-name
                   buffer-name)))
    (if instance
        (format "%s:%s" project instance)
      project)))

(defun claude-code-extras-display-name (&optional buffer)
  "Return the display name for BUFFER's modeline.
Use the project name alone when it is unique among active Claude
sessions, or \"project:instance\" when multiple sessions share
the same project.  Returns the cached value when available to
avoid scanning all buffers on every redisplay."
  (let ((buf (or buffer (current-buffer))))
    (or (buffer-local-value 'claude-code-extras--display-name-cache buf)
        (claude-code-extras--compute-display-name buf))))

(defun claude-code-extras--compute-display-name (buffer)
  "Compute the display name for BUFFER by scanning Claude sessions."
  (let* ((base (claude-code-extras--base-display-name buffer))
         (branch-suffix (claude-code-extras--branch-suffix buffer)))
    (if branch-suffix
        (format "%s:%s" base branch-suffix)
      base)))

(defun claude-code-extras--base-display-name (buffer)
  "Compute the base display name for BUFFER (without branch suffix)."
  (let* ((name (claude-code-extras--buffer-session-name buffer))
         (others (cl-remove buffer (claude-code--find-all-claude-buffers)))
         (sibling-names (mapcar #'claude-code-extras--buffer-session-name
                                others)))
    (if (member name sibling-names)
        (claude-code-extras--qualified-session-name (buffer-name buffer))
      name)))

(defun claude-code-extras--branch-suffix (buffer)
  "Return a short branch ID for BUFFER, or nil if not branched."
  (with-current-buffer buffer
    (let ((original claude-code-extras--original-session-id)
          (current (when claude-code-extras--status-data
                     (plist-get claude-code-extras--status-data :session_id))))
      (when (and original current (not (string= original current)))
        (substring current 0 8)))))

(defun claude-code-extras--refresh-display-names ()
  "Recompute and cache display names for all Claude buffers."
  (dolist (buf (claude-code--find-all-claude-buffers))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq claude-code-extras--display-name-cache
              (claude-code-extras--compute-display-name buf))))))

;;;;; Status polling

(defun claude-code-extras-start-status-polling ()
  "Start polling the status file for the current Claude buffer."
  (interactive)
  (when (claude-code--buffer-p (current-buffer))
    (when claude-code-extras--status-timer
      (cancel-timer claude-code-extras--status-timer))
    (let* ((buf (current-buffer))
           (timer-cell (cons nil nil))
           (timer (run-with-timer
                   claude-code-extras-status-interval
                   claude-code-extras-status-interval
                   #'claude-code-extras--read-status
                   timer-cell buf)))
      (setcar timer-cell timer)
      (setq claude-code-extras--status-timer timer))))

(defvar monet--sessions)
(declare-function monet--session-server "monet")
(declare-function monet--session-port "monet")
(declare-function monet--remove-lockfile "monet")
(declare-function websocket-server-close "websocket")

(defun claude-code-extras--monet-stop-session (key)
  "Fully stop the monet session for KEY.
Closes the websocket server, removes the lockfile, and removes
the session from `monet--sessions'."
  (when-let* ((session (gethash key monet--sessions))
              (server (monet--session-server session)))
    (ignore-errors
      (monet--remove-lockfile (monet--session-port session)))
    (when (process-live-p server)
      (ignore-errors (websocket-server-close server))
      (when (process-live-p server)
        (delete-process server)))
    (remhash key monet--sessions)))

(defun claude-code-extras--cleanup-monet-session ()
  "Clean up the monet websocket session for the current Claude buffer."
  (when (and (claude-code--buffer-p (current-buffer))
             (boundp 'monet--sessions))
    (claude-code-extras--monet-stop-session (buffer-name))))

(defun claude-code-extras--monet-cleanup-before-start (orig-fn key directory)
  "Clean up old monet session for KEY before ORIG-FN creates a new one.
DIRECTORY is passed through to ORIG-FN."
  (when (and (boundp 'monet--sessions)
             (gethash key monet--sessions))
    (claude-code-extras--monet-stop-session key))
  (funcall orig-fn key directory))

(defun claude-code-extras--monet-gc-orphaned-servers ()
  "Delete websocket server processes not tracked by any monet session.
Runs periodically as a safety net to catch servers leaked through
any code path."
  (when (boundp 'monet--sessions)
    (let ((active-servers nil))
      (maphash (lambda (_k session)
        (when-let* ((server (monet--session-server session)))
          (push server active-servers)))
        monet--sessions)
      (dolist (p (process-list))
        (when (and (string-match-p "\\`websocket server on port [0-9]"
                                   (process-name p))
                   (eq (process-status p) 'listen)
                   (not (memq p active-servers)))
          (delete-process p))))))

(defun claude-code-extras--diff-file-in-session-p (diff-buffer session)
  "Return non-nil if DIFF-BUFFER's file is inside SESSION's directory."
  (when-let ((session-dir (and session (monet--session-directory session)))
             (file-dir (buffer-local-value 'default-directory diff-buffer)))
    (string-prefix-p (file-name-as-directory (expand-file-name session-dir))
                     (expand-file-name file-dir))))

(defun claude-code-extras--display-diff-buffer (diff-buffer &optional session)
  "Display DIFF-BUFFER in a bottom side window without switching tabs.
Override for `monet--display-diff-buffer' that avoids the tab-switching
side effects of `display-buffer-in-tab', which can corrupt the window
layout when called from an async websocket callback.
When SESSION is provided and the file is outside the session directory,
the diff is suppressed entirely; the terminal approval prompt suffices."
  (if (and session (not (claude-code-extras--diff-file-in-session-p diff-buffer session)))
      nil
    (display-buffer diff-buffer
                    '((display-buffer-in-side-window)
                      (side . bottom)
                      (slot . 0)
                      (window-height . 0.3)
                      (preserve-size . (nil . t))))))

(with-eval-after-load 'monet
  (advice-add 'monet-start-server-in-directory :around
              #'claude-code-extras--monet-cleanup-before-start)
  (advice-add 'monet--display-diff-buffer :override
              #'claude-code-extras--display-diff-buffer)
  (run-with-timer 60 60 #'claude-code-extras--monet-gc-orphaned-servers))

(defun claude-code-extras-stop-status-polling ()
  "Stop status polling and clean up the status file."
  (interactive)
  (when (and (claude-code--buffer-p (current-buffer))
             claude-code-extras--status-timer)
    (cancel-timer claude-code-extras--status-timer)
    (claude-code-extras--cleanup-status-file)))

(defun claude-code-extras--read-status (timer-cell buffer)
  "Read and parse the status file for BUFFER.
TIMER-CELL is a cons whose car is the timer that triggered this
call; it is canceled automatically when BUFFER is no longer live."
  (if (not (buffer-live-p buffer))
      (cancel-timer (car timer-cell))
    (with-current-buffer buffer
      (when-let* ((data (claude-code-extras--parse-status-file)))
        (claude-code-extras--detect-branch data)
        (setq claude-code-extras--status-data data)))))

(defun claude-code-extras--detect-branch (new-data)
  "Detect when the session ID changes, indicating a branch.
NEW-DATA is the freshly parsed status plist.  On the first poll,
records the session ID as the original.  On subsequent polls, if
the ID differs from the previous one, refreshes display names so
the modeline reflects the new branch."
  (let ((new-id (plist-get new-data :session_id)))
    (when new-id
      (if (not claude-code-extras--original-session-id)
          (setq claude-code-extras--original-session-id new-id)
        (let ((old-id (plist-get claude-code-extras--status-data :session_id)))
          (when (and old-id (not (string= new-id old-id)))
            (claude-code-extras--refresh-display-names)))))))

(defun claude-code-extras--parse-status-file ()
  "Parse the status JSON file for the current buffer.
Returns a plist, or nil if the file is missing or malformed."
  (let ((file (claude-code-extras--status-file)))
    (when (file-exists-p file)
      (condition-case nil
          (json-parse-string
           (with-temp-buffer
             (insert-file-contents file)
             (buffer-string))
           :object-type 'plist)
        (json-parse-error nil)))))

(defun claude-code-extras--status-file ()
  "Return the status file path for the current buffer."
  (expand-file-name
   (concat (claude-code-extras--sanitize-buffer-name) ".json")
   claude-code-extras--status-directory))

(defun claude-code-extras--sanitize-buffer-name ()
  "Sanitize the current buffer name for use as a filename.
Replaces every character that is not alphanumeric, underscore,
or hyphen with an underscore, mirroring the shell script's
`tr -c' invocation."
  (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" (buffer-name)))

(defun claude-code-extras--cleanup-status-file ()
  "Delete the status file for the current buffer."
  (let ((file (claude-code-extras--status-file)))
    (when (file-exists-p file)
      (delete-file file))))

;;;;; Status accessors

(defun claude-code-extras-status-model ()
  "Return the model display name from the status data."
  (when-let* ((model (plist-get claude-code-extras--status-data :model)))
    (plist-get model :display_name)))

(defun claude-code-extras-status-cost ()
  "Return the total session cost in USD from the status data."
  (when-let* ((cost (plist-get claude-code-extras--status-data :cost)))
    (plist-get cost :total_cost_usd)))

(defun claude-code-extras-status-context-percent ()
  "Return the context window usage percentage from the status data."
  (when-let* ((ctx (plist-get claude-code-extras--status-data :context_window)))
    (plist-get ctx :used_percentage)))

(defun claude-code-extras-status-token-count ()
  "Return the total input token count from the status data."
  (when-let* ((ctx (plist-get claude-code-extras--status-data :context_window)))
    (plist-get ctx :total_input_tokens)))

(defun claude-code-extras-status-lines-added ()
  "Return the total lines added from the status data."
  (when-let* ((cost (plist-get claude-code-extras--status-data :cost)))
    (plist-get cost :total_lines_added)))

(defun claude-code-extras-status-lines-removed ()
  "Return the total lines removed from the status data."
  (when-let* ((cost (plist-get claude-code-extras--status-data :cost)))
    (plist-get cost :total_lines_removed)))

(defun claude-code-extras-status-duration-ms ()
  "Return the total session duration in milliseconds from the status data."
  (when-let* ((cost (plist-get claude-code-extras--status-data :cost)))
    (plist-get cost :total_duration_ms)))

(defun claude-code-extras-status-cache-read-tokens ()
  "Return the cache read input token count from the status data."
  (when-let* ((ctx (plist-get claude-code-extras--status-data :context_window))
              (usage (plist-get ctx :current_usage)))
    (plist-get usage :cache_read_input_tokens)))

(defun claude-code-extras-status-cache-total-tokens ()
  "Return the total input tokens for the current turn from the status data.
This is the sum of INPUT_TOKENS, CACHE_CREATION_INPUT_TOKENS, and
CACHE_READ_INPUT_TOKENS."
  (when-let* ((ctx (plist-get claude-code-extras--status-data :context_window))
              (usage (plist-get ctx :current_usage)))
    (let ((input (or (plist-get usage :input_tokens) 0))
          (creation (or (plist-get usage :cache_creation_input_tokens) 0))
          (read (or (plist-get usage :cache_read_input_tokens) 0)))
      (+ input creation read))))

;;;;; Alert

(declare-function alert "alert")

(defun claude-code-extras-notify (title message)
  "Notification function combining modeline pulse with optional alert.
TITLE is the notification title.  MESSAGE is the notification
body.  When `claude-code-extras-alert-on-ready' is non-nil,
dispatch to the style configured in
`claude-code-extras-alert-style'."
  (claude-code-default-notification title message)
  (when claude-code-extras-alert-on-ready
    (claude-code-extras--alert-visual title message)
    (claude-code-extras--alert-sound)))

(defun claude-code-extras--alert-visual (title message)
  "Show a visual notification with TITLE and MESSAGE.
Only fires when `claude-code-extras-alert-style' is `visual' or
`both'."
  (when (memq claude-code-extras-alert-style '(visual both))
    (alert message :title title)))

(defun claude-code-extras--alert-sound ()
  "Play the configured alert sound.
Only fires when `claude-code-extras-alert-style' is `sound' or
`both'."
  (when (memq claude-code-extras-alert-style '(sound both))
    (when-let* ((sound claude-code-extras-alert-sound)
                ((file-exists-p sound)))
      (start-process "claude-code-alert-sound" nil "afplay" sound))))

(defun claude-code-extras--notification-type (json-str)
  "Extract the notification type from JSON-STR.
Return a string like \"idle_prompt\" or \"permission_prompt\", or
nil if the type cannot be determined."
  (when json-str
    (condition-case nil
        (let ((parsed (json-parse-string json-str :object-type 'alist)))
          (or (alist-get 'notification_type parsed)
              (alist-get 'type parsed)))
      (error nil))))

(defun claude-code-extras--handle-notification (message)
  "Handle a notification event from the Claude Code CLI.
MESSAGE is a plist with :type, :buffer-name, :json-data, and
:args.  Fires OS alerts for idle_prompt, permission_prompt, and
elicitation_dialog notifications."
  (when (eq (plist-get message :type) 'notification)
    (when-let* ((buf (get-buffer (plist-get message :buffer-name))))
      (with-current-buffer buf
        (let* ((name (claude-code-extras--session-name (buffer-name)))
               (ntype (claude-code-extras--notification-type
                       (plist-get message :json-data))))
          (pcase ntype
            ("idle_prompt"
             (setq claude-code-extras--waiting-for-input t)
             (claude-code-extras-notify
              "Claude ready"
              (format "%s: waiting for your response" name)))
            ("permission_prompt"
             (claude-code-extras-notify
              "Claude needs approval"
              (format "%s: permission request pending" name)))
            ("elicitation_dialog"
             (claude-code-extras-notify
              "Claude needs input"
              (format "%s: waiting for your input" name)))
            (_
             (claude-code-extras-notify
              "Claude Code"
              (format "%s: needs your attention" name))))))))
  nil)

(defun claude-code-extras--handle-stop (message)
  "Handle a stop event from the Claude Code CLI.
MESSAGE is a plist with :type, :buffer-name, :json-data, and
:args.  Scrolls to bottom and applies any pending theme."
  (when (eq (plist-get message :type) 'stop)
    (when-let* ((buf (get-buffer (plist-get message :buffer-name))))
      (with-current-buffer buf
        (claude-code-extras--scroll-to-bottom buf)
        (claude-code-extras--apply-pending-theme buf))))
  nil)

(defun claude-code-extras--scroll-to-bottom (buffer)
  "Scroll BUFFER and its windows to the terminal cursor.
Move point and all windows showing BUFFER to the eat terminal
cursor, keeping the cursor line at the bottom of each window."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (bound-and-true-p eat-terminal)
        (let ((cursor-pos (eat-term-display-cursor eat-terminal)))
          (goto-char cursor-pos)
          (claude-code-extras--scroll-windows-to cursor-pos))))))

(defun claude-code-extras--scroll-windows-to (pos)
  "Set `window-point' to POS and recenter in all windows showing this buffer."
  (dolist (window (get-buffer-window-list nil nil t))
    (set-window-point window pos)
    (with-selected-window window
      (goto-char pos)
      (recenter -1))))

(defun claude-code-extras--session-name (buffer-name)
  "Extract the project name from BUFFER-NAME.
Given \"*claude:~/path/to/project/:default*\", return
\"project\"."
  (if (string-match "/\\([^/]+\\)/:[^*]+\\*\\'" buffer-name)
      (match-string 1 buffer-name)
    buffer-name))

(defun claude-code-extras-toggle-alert ()
  "Toggle OS notifications for the current Claude session."
  (interactive)
  (setq claude-code-extras-alert-on-ready
        (not claude-code-extras-alert-on-ready))
  (message "Claude alert notifications %s"
           (if claude-code-extras-alert-on-ready "enabled" "disabled")))

(defun claude-code-extras-alert-indicator ()
  "Return a bell icon reflecting the current alert state."
  (if claude-code-extras-alert-on-ready "🔔" "🔕"))

;;;;; Modeline

(declare-function doom-modeline-set-modeline "doom-modeline-core")

(defun claude-code-extras-set-modeline ()
  "Set the doom-modeline to the `claude-code' modeline for this buffer.
Also starts status polling if it is not already active."
  (when (claude-code--buffer-p (current-buffer))
    (unless claude-code-extras--status-timer
      (claude-code-extras-start-status-polling))
    (doom-modeline-set-modeline 'claude-code)))

(defun claude-code-extras--capture-buffer-account ()
  "Store the pending account name as a buffer-local variable.
Called from `claude-code-start-hook', which runs inside the new
buffer within the dynamic scope of
`claude-code-extras--start-with-account'."
  (when claude-code-extras--pending-account
    (setq claude-code-extras--buffer-account
          claude-code-extras--pending-account)))

(defun claude-code-extras-buffer-account ()
  "Return the account name for the current buffer, or nil."
  claude-code-extras--buffer-account)

;;;;; Copilot integration

(defun claude-code-extras-setup-copilot ()
  "Set up Copilot integration in the current Claude Code buffer.
Guards on `claude-code-extras-copilot-enabled',
`claude-code--buffer-p', `eat-terminal', and the availability of
the `copilot' package."
  (when (and claude-code-extras-copilot-enabled
             (claude-code--buffer-p (current-buffer))
             (bound-and-true-p eat-terminal)
             (require 'copilot nil t))
    (advice-add 'copilot--get-language-id :around
                #'claude-code-extras--copilot-language-id)
    ;; The copilot server ignores virtual buffer URIs.  Setting
    ;; buffer-file-name makes copilot--get-uri return a file URI
    ;; natively, so didOpen uses the same URI as inlineCompletion.
    (setq buffer-file-name
          (expand-file-name
           (concat "claude-code-"
                   (claude-code-extras--sanitize-buffer-name)
                   ".txt")
           temporary-file-directory))
    (setq buffer-auto-save-file-name nil)
    (setq-local copilot-disable-predicates
                (cons (lambda () buffer-read-only)
                      copilot-disable-predicates))
    (advice-add 'copilot-accept-completion :around
                #'claude-code-extras--copilot-accept-around)
    (advice-add 'copilot--show-completion :around
                #'claude-code-extras--copilot-show-completion)
    (advice-add 'copilot--set-overlay-text :around
                #'claude-code-extras--copilot-set-overlay-text)
    (add-hook 'post-command-hook
              #'claude-code-extras--copilot-post-command nil t)
    ;; Eat's semi-char-mode keymap binds TAB to `eat-self-input'.
    ;; Since it is a minor-mode keymap, it takes priority over
    ;; copilot's keymap overlay (which also requires point to be
    ;; within its range).  Use a minor mode activated after eat to
    ;; get higher priority in `minor-mode-map-alist'.
    (claude-code-extras--copilot-keys-mode 1)
    (setq claude-code-extras--copilot-active t)
    (copilot-mode 1)
    ;; Copilot registers a `track-changes' handler when the mode
    ;; enables, but eat's `inhibit-modification-hooks' prevents
    ;; track-changes from working and causes "Recovering from
    ;; confusing calls to before/after-change-functions" errors.
    ;; Unregister it since we do our own full-document sync.
    (when (bound-and-true-p copilot--track-changes-id)
      (track-changes-unregister copilot--track-changes-id)
      (setq copilot--track-changes-id nil))
    ;; Copilot's own `post-command-hook' handler calls
    ;; `copilot-complete' without syncing the document, which would
    ;; use stale content.  Remove it and rely on our handler that
    ;; syncs before requesting completions.
    (remove-hook 'post-command-hook #'copilot--post-command t)))

(defvar claude-code-extras--copilot-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'claude-code-extras--copilot-tab)
    (define-key map [tab] #'claude-code-extras--copilot-tab)
    map)
  "Keymap for `claude-code-extras--copilot-keys-mode'.")

(define-minor-mode claude-code-extras--copilot-keys-mode
  "Minor mode providing Copilot keybindings in Claude Code buffers."
  :keymap claude-code-extras--copilot-keys-mode-map)

(defun claude-code-extras--copilot-tab ()
  "Accept Copilot completion, try snippet expansion, or send TAB to eat."
  (interactive)
  (cond
   ((copilot--overlay-visible)
    (copilot-accept-completion))
   ((claude-code-extras--try-expand-snippet-at-prompt)
    nil)
   (t
    (eat-self-input 1 ?\t))))

(defun claude-code-extras--copilot-language-id (orig-fn)
  "Return \"plaintext\" in Claude Code eat buffers.
ORIG-FN is `copilot--get-language-id'.  The default language ID
for `eat-mode' is \"eat\", which the Copilot server does not
recognize."
  (if claude-code-extras--copilot-active
      "plaintext"
    (funcall orig-fn)))

(defun claude-code-extras--copilot-show-completion (orig-fn completion-data)
  "Relocate the copilot overlay to the TUI prompt line.
ORIG-FN is `copilot--show-completion'.  COMPLETION-DATA is the
server response.  In eat buffers the line numbers in the server's
range drift because the buffer changes between sync and response,
so the overlay often ends up on a TUI chrome line.  This advice
lets the original function create the overlay, then moves it to
the prompt and re-renders the ghost text at the correct position.
Finally it moves point there so the Emacs cursor sits at the
ghost text boundary."
  (if (not claude-code-extras--copilot-active)
      (funcall orig-fn completion-data)
    (funcall orig-fn completion-data)
    (when (copilot--overlay-visible)
      (let ((prompt-bol
             (save-excursion
               (goto-char (point-max))
               (when (re-search-backward "^❯[[:space:]]" nil t)
                 (point)))))
        (when prompt-bol
          (let* ((prompt-eol (save-excursion (goto-char prompt-bol)
                                             (line-end-position)))
                 (line-text (buffer-substring-no-properties
                             prompt-bol prompt-eol))
                 (insert-text (plist-get completion-data :insertText))
                 (prefix-len
                  (when insert-text
                    (let ((i 0)
                          (max-i (min (length line-text)
                                      (length insert-text))))
                      (while (and (< i max-i)
                                  (= (aref line-text i)
                                     (aref insert-text i)))
                        (cl-incf i))
                      i)))
                 (completion
                  (when (and prefix-len
                             (< prefix-len (length insert-text)))
                    (substring insert-text prefix-len)))
                 (insert-pos (+ prompt-bol (or prefix-len 0))))
            (when (and completion (not (string-empty-p completion)))
              (goto-char insert-pos)
              (overlay-put copilot--overlay 'tail-length 0)
              (copilot--set-overlay-text copilot--overlay completion)
              (overlay-put copilot--overlay 'completion completion)
              (overlay-put copilot--overlay 'completion-start insert-pos))))))))

(defun claude-code-extras--copilot-set-overlay-text (orig-fn ov completion)
  "Clip the copilot overlay to the remaining terminal columns.
ORIG-FN is `copilot--set-overlay-text'.  OV is the overlay.
COMPLETION is the completion text.  In eat buffers, the overlay's
`after-string' must not wrap past the line end, otherwise it
pushes TUI chrome down and disrupts the terminal layout.  This
advice truncates the completion to the first line and clips it to
the remaining space on the current terminal line."
  (if claude-code-extras--copilot-active
      (let* ((first-line (car (split-string completion "\n")))
             (remaining (max 0 (- (window-width) (current-column) 1)))
             (clipped (if (> (length first-line) remaining)
                         (substring first-line 0 remaining)
                       first-line)))
        (unless (string-empty-p clipped)
          (funcall orig-fn ov clipped)))
    (funcall orig-fn ov completion)))

(defun claude-code-extras--copilot-accept-around (orig-fn &optional transform-fn)
  "Around advice for `copilot-accept-completion' in eat buffers.
When in a Claude Code eat buffer, extract the completion, send
telemetry, and inject text via the terminal instead of using
`delete-region'/`insert'.  Otherwise, call ORIG-FN with
TRANSFORM-FN unchanged."
  (if (and (bound-and-true-p eat-terminal)
           claude-code-extras--copilot-active)
      (when (copilot--overlay-visible)
        (let* ((completion (overlay-get copilot--overlay 'completion))
               (command (overlay-get copilot--overlay 'command))
               (full-insert-text (overlay-get copilot--overlay 'full-insert-text))
               (t-completion (funcall (or transform-fn #'identity) completion))
               (is-partial (and (string-prefix-p t-completion completion)
                                (not (string-equal t-completion completion)))))
          (claude-code-extras--copilot-send-telemetry
           is-partial command full-insert-text completion t-completion)
          (copilot-clear-overlay t)
          (let ((text (replace-regexp-in-string "\n" "\e\r" t-completion)))
            (eat-term-send-string eat-terminal text))
          t))
    (funcall orig-fn transform-fn)))

(defun claude-code-extras--copilot-send-telemetry
    (is-partial command full-insert-text completion t-completion)
  "Send Copilot telemetry for acceptance.
IS-PARTIAL is non-nil for partial acceptance.  COMMAND is the LSP
command from the overlay.  FULL-INSERT-TEXT is the original full
suggestion.  COMPLETION is the remaining completion text.
T-COMPLETION is the transformed (accepted) portion."
  (when (and command (bound-and-true-p copilot--connection))
    (condition-case nil
        (if is-partial
            (let* ((prefix-len (- (length full-insert-text) (length completion)))
                   (accepted-length (+ prefix-len (length t-completion))))
              (jsonrpc-notify copilot--connection
                              'textDocument/didPartiallyAcceptCompletion
                              (list :item (list :command command)
                                    :acceptedLength accepted-length)))
          (jsonrpc-async-request copilot--connection
                                 'workspace/executeCommand command
                                 :success-fn #'ignore))
      (error nil))))

(defun claude-code-extras--copilot-sync-document ()
  "Send a full-document `textDocument/didChange' to the Copilot server.
Eat's process filter runs with `inhibit-modification-hooks' bound
to t, so `track-changes' never detects buffer modifications and
the server's document becomes stale.  This function works around
the problem by sending the entire buffer contents as a single
replacement change, keeping the server in sync."
  (when (and (bound-and-true-p copilot--connection)
             (copilot--connection-alivep))
    (cl-incf copilot--doc-version)
    (jsonrpc-notify
     copilot--connection
     'textDocument/didChange
     (list :textDocument (list :uri (copilot--get-uri)
                               :version copilot--doc-version)
           :contentChanges
           (vector (list :text (copilot--get-source)))))))

(defun claude-code-extras--copilot-post-command ()
  "Schedule a debounced `copilot-complete' after user input.
Eat's process filter runs with `inhibit-modification-hooks' bound
to t, so neither `after-change-functions' nor `track-changes'
detect buffer modifications (making copilot's normal
`post-command-hook' trigger ineffective).  This function replaces
copilot's trigger by scheduling a document sync and completion
request after a short delay, giving the terminal echo time to
arrive and update the buffer before the Copilot server is
consulted."
  (when (and claude-code-extras--copilot-active
             (bound-and-true-p copilot-mode)
             (not buffer-read-only))
    (copilot-clear-overlay)
    (when (timerp claude-code-extras--copilot-change-timer)
      (cancel-timer claude-code-extras--copilot-change-timer))
    (let ((buf (current-buffer)))
      (setq claude-code-extras--copilot-change-timer
            (run-with-timer
             0.3 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq claude-code-extras--copilot-change-timer nil)
                   (when copilot-mode
                     (claude-code-extras--copilot-complete-at-prompt))))))))))

(defun claude-code-extras--copilot-complete-at-prompt ()
  "Sync the document and request completion at the TUI prompt.
Claude Code's TUI repositions the terminal cursor for rendering,
so `point' often lands on a status-bar or border line rather than
the input prompt.  This function searches backward for the `❯'
prompt marker and moves point to the end of the user's text on
that line before syncing and requesting completions."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "^❯[[:space:]]" nil t)
      (end-of-line)
      (skip-chars-backward " \t")
      (claude-code-extras--copilot-sync-document)
      (copilot-complete))))

(defun claude-code-extras-teardown-copilot ()
  "Clean up Copilot integration in the current Claude Code buffer."
  (when claude-code-extras--copilot-active
    (when (timerp claude-code-extras--copilot-change-timer)
      (cancel-timer claude-code-extras--copilot-change-timer))
    (setq claude-code-extras--copilot-active nil)))

;;;;; Non-interactive execution

(defcustom claude-code-extras-batch-allowed-tools nil
  "Tools to auto-allow via `--allowedTools' for non-interactive execution.
When nil (the default), no `--allowedTools' flag is passed and tool
access is governed by `claude-code-extras-batch-permission-mode'
and the user's settings.json."
  :type '(choice (const :tag "None (use permission-mode)" nil)
                 (repeat string))
  :group 'claude-code-extras)

(defcustom claude-code-extras-batch-permission-mode "bypassPermissions"
  "Permission mode passed via `--permission-mode' for non-interactive execution.
The default \"bypassPermissions\" grants all tool permissions
automatically, which is necessary because `claude -p' cannot
prompt the user for approval."
  :type '(choice (const :tag "Bypass all" "bypassPermissions")
                 (const :tag "Default" "default")
                 (const :tag "Accept edits" "acceptEdits")
                 (const :tag "Don't ask" "dontAsk")
                 (const :tag "Auto" "auto")
                 (const :tag "None" nil))
  :group 'claude-code-extras)

(defcustom claude-code-extras-batch-max-turns 30
  "Maximum agentic turns per entry in non-interactive execution."
  :type 'integer
  :group 'claude-code-extras)

(defcustom claude-code-extras-batch-system-prompt nil
  "Optional system prompt appended via `--append-system-prompt'.
When non-nil, passed to each `claude -p' invocation."
  :type '(choice (const :tag "None" nil) string)
  :group 'claude-code-extras)

(defcustom claude-code-extras-batch-model nil
  "Optional model override via `--model' for non-interactive execution.
When non-nil, passed to each `claude -p' invocation."
  :type '(choice (const :tag "Default" nil) string)
  :group 'claude-code-extras)

(defcustom claude-code-extras-run-skill-model "opus"
  "Model to use for `claude-code-extras-run-skill'.
Skills are complex agentic tasks that benefit from the most
capable model.  Supports aliases like \"opus\", \"sonnet\",
\"haiku\" as well as full model IDs.  Set to nil to use
`claude-code-extras-batch-model' or Claude's default."
  :type '(choice (const :tag "Opus (latest)" "opus")
                 (const :tag "Sonnet (latest)" "sonnet")
                 (const :tag "Haiku (latest)" "haiku")
                 (const :tag "Use batch default" nil)
                 string)
  :group 'claude-code-extras)

(defcustom claude-code-extras-audit-skills
  '("/code-audit" "/design-audit" "/interpretability-audit")
  "Skills to run when performing an integral project audit.
Each entry is a skill name (with leading slash) that will be
invoked with `--accept'."
  :type '(repeat string)
  :group 'claude-code-extras)

(defcustom claude-code-extras-audit-project-directories nil
  "Directories available for selection in `claude-code-extras-audit-project'.
New directories entered by the user are automatically added to this list."
  :type '(repeat directory)
  :group 'claude-code-extras)

;; Batch state is passed as a plist through closures to support
;; parallel runs.  Keys: :queue :results :log-dir :working-dir :start-time

(defun claude-code-extras--batch-collect-todos (scope)
  "Collect TODO entries from the current org buffer according to SCOPE.
SCOPE is one of `buffer', `subtree', or `region'.
Returns a list of plists with :title and :body keys."
  (let ((entries '()))
    (org-map-entries
     (lambda ()
       (when (and (org-get-todo-state)
                  (not (org-entry-is-done-p)))
         (let* ((title (org-get-heading t t t t))
                (body-start (save-excursion
                              (org-end-of-meta-data t)
                              (point)))
                (body-end (save-excursion
                            (outline-next-heading)
                            (or (point) (point-max))))
                (body (string-trim
                       (buffer-substring-no-properties body-start body-end))))
           (push (list :title title :body body) entries))))
     nil
     (pcase scope
       ('buffer nil)
       ('subtree 'tree)
       ('region 'region)))
    (nreverse entries)))

(defun claude-code-extras--batch-format-prompt (entry)
  "Format ENTRY plist as a prompt string for `claude -p'.
Combines :title and :body, using title alone when body is empty."
  (let ((title (plist-get entry :title))
        (body (plist-get entry :body)))
    (if (or (null body) (string-empty-p body))
        title
      (concat title "\n\n" body))))

(defun claude-code-extras-batch-todos ()
  "Process org TODO entries sequentially via `claude -p'.
Infers scope automatically: region if active, subtree if the
buffer is narrowed, buffer otherwise.  Prompts for a working
directory, then runs each TODO as a non-interactive Claude
session.  Results are logged to timestamped files and displayed
in a summary buffer when all entries have been processed."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Must be called from an org-mode buffer"))
  (let* ((scope (cond
                 ((use-region-p) 'region)
                 ((buffer-narrowed-p) 'subtree)
                 (t 'buffer)))
         (entries (claude-code-extras--batch-collect-todos scope)))
    (when (null entries)
      (user-error "No TODO entries found in %s" scope))
    (let ((dir (project-prompt-project-dir)))
      (when (or (eq scope 'region)
                (yes-or-no-p
                 (format "Process %d TODO(s) in %s?" (length entries) dir)))
        (claude-code-extras--batch-start entries dir)))))

(defun claude-code-extras--batch-start (entries dir &optional commit-after-each)
  "Start batch processing of ENTRIES in working directory DIR.
When COMMIT-AFTER-EACH is non-nil, automatically commit any uncommitted
changes in DIR after each entry completes successfully."
  (let* ((log-dir (expand-file-name
                   (format-time-string "batch_%Y-%m-%d_%H-%M-%S")
                   claude-code-extras-log-directory))
         (state (list :queue entries
                      :results nil
                      :log-dir log-dir
                      :working-dir dir
                      :start-time (current-time)
                      :commit-after-each commit-after-each)))
    (make-directory log-dir t)
    (message "Batch processing %d TODO(s)..." (length entries))
    (claude-code-extras--batch-run-next state)))

(defun claude-code-extras--batch-run-next (state)
  "Process the next entry in the batch queue in STATE.
STATE is a plist with keys :queue :results :log-dir :working-dir
:start-time.  When the queue is empty, display the summary buffer."
  (if (null (plist-get state :queue))
      (claude-code-extras--batch-finish state)
    (let* ((queue (plist-get state :queue))
           (entry (car queue))
           (index (1+ (length (plist-get state :results))))
           (title (plist-get entry :title))
           (prompt (claude-code-extras--batch-format-prompt entry))
           (log-file (expand-file-name
                      (format "%02d_%s.json"
                              index
                              (replace-regexp-in-string
                               "[^a-zA-Z0-9_-]" "-"
                               (truncate-string-to-width title 50)))
                      (plist-get state :log-dir))))
      (plist-put state :queue (cdr queue))
      (message "Batch [%d/%d]: %s"
               index
               (+ index (length (plist-get state :queue)))
               title)
      (claude-code-extras--run-prompt
       prompt
       :dir (plist-get state :working-dir)
       :callback
       (lambda (result)
         (when-let* ((raw (plist-get result :raw)))
           (with-temp-file log-file
             (insert raw)))
         (plist-put state :results
                    (cons (list :title title
                                :index index
                                :exit-code (plist-get result :exit-code)
                                :duration (plist-get result :duration)
                                :cost (plist-get result :cost)
                                :result-text (or (plist-get result :text)
                                                 "(failed to parse output)")
                                :log-file log-file)
                          (plist-get state :results)))
         (when (and (zerop (plist-get result :exit-code))
                    (plist-get state :commit-after-each))
           (ignore-errors
             (claude-code-extras--batch-commit-changes state title)))
         (claude-code-extras--batch-run-next state))))))

(defun claude-code-extras--batch-commit-changes (state title)
  "Commit any uncommitted changes in the working directory of STATE.
TITLE is the entry title, used to derive the commit message scope."
  (let ((default-directory (plist-get state :working-dir)))
    (with-temp-buffer
      (call-process "git" nil t nil "status" "--porcelain")
      (when (> (buffer-size) 0)
        (call-process "git" nil nil nil "add" "-A")
        (let ((scope (replace-regexp-in-string
                      "^/" ""
                      (car (split-string title " ")))))
          (call-process "git" nil nil nil "commit" "-m"
                        (format "%s: apply audit recommendations" scope)))))))

(defun claude-code-extras--batch-parse-stream-json (raw)
  "Parse stream-json output RAW into a plist.
Returns (:text ASSISTANT-TEXT :cost COST :session-id ID
         :num-turns N :subtype TYPE)."
  (let (texts cost session-id num-turns subtype)
    (dolist (line (split-string raw "\n" t))
      (condition-case nil
          (let ((obj (json-parse-string line :object-type 'plist)))
            (pcase (plist-get obj :type)
              ("assistant"
               (let ((content (plist-get (plist-get obj :message) :content)))
                 (when (vectorp content)
                   (seq-doseq (block content)
                     (when (equal (plist-get block :type) "text")
                       (push (plist-get block :text) texts))))))
              ("result"
               (setq cost (or (plist-get obj :total_cost_usd)
                              (plist-get obj :cost_usd) 0)
                     session-id (plist-get obj :session_id)
                     num-turns (plist-get obj :num_turns)
                     subtype (plist-get obj :subtype)))))
        (error nil)))
    (list :text (if texts
                    (string-join (nreverse texts) "\n\n")
                  (format (concat "No assistant text captured.\n"
                                  "Session: %s | Turns: %s | Reason: %s\n"
                                  "Resume with: claude --resume %s")
                          (or session-id "?") (or num-turns "?")
                          (or subtype "unknown") (or session-id "?")))
          :cost (or cost 0)
          :session-id session-id)))

(defun claude-code-extras--build-cli-args (prompt &rest kwargs)
  "Build the argument list for `claude -p' with PROMPT.
KWARGS are keyword arguments:
  :allowed-tools   list of tool name strings
  :permission-mode permission mode string
  :system-prompt   string appended via --append-system-prompt
  :model           model name string
  :max-turns       integer, maximum agentic turns
Each defaults to the corresponding `claude-code-extras-batch-*'
customization variable when not supplied."
  (let ((allowed-tools (or (plist-get kwargs :allowed-tools)
                           claude-code-extras-batch-allowed-tools))
        (permission-mode (or (plist-get kwargs :permission-mode)
                             claude-code-extras-batch-permission-mode))
        (system-prompt (or (plist-get kwargs :system-prompt)
                           claude-code-extras-batch-system-prompt))
        (model (or (plist-get kwargs :model)
                   claude-code-extras-batch-model))
        (max-turns (or (plist-get kwargs :max-turns)
                       claude-code-extras-batch-max-turns))
        (args (list claude-code-program
                    "-p" prompt
                    "--output-format" "stream-json"
                    "--verbose")))
    (setq args (append args (list "--max-turns"
                                  (number-to-string max-turns))))
    (when permission-mode
      (setq args (append args (list "--permission-mode" permission-mode))))
    (when allowed-tools
      (setq args (append args (list "--allowedTools"
                                    (string-join allowed-tools ",")))))
    (when system-prompt
      (setq args (append args (list "--append-system-prompt"
                                    system-prompt))))
    (when model
      (setq args (append args (list "--model" model))))
    args))

(defun claude-code-extras--run-prompt (prompt &rest kwargs)
  "Run PROMPT non-interactively via `claude -p' and call back with results.
KWARGS are keyword arguments:
  :dir             working directory (default `default-directory')
  :callback        function called with a result plist (required)
  :allowed-tools   passed to `claude-code-extras--build-cli-args'
  :system-prompt   passed to `claude-code-extras--build-cli-args'
  :model           passed to `claude-code-extras--build-cli-args'
  :max-turns       passed to `claude-code-extras--build-cli-args'

The CALLBACK receives a plist with keys:
  :exit-code  process exit code
  :duration   elapsed seconds (float)
  :cost       USD cost (float)
  :text       parsed assistant text
  :session-id session ID string
  :raw        raw stream-json output

Returns the process object."
  (let* ((dir (or (plist-get kwargs :dir) default-directory))
         (callback (or (plist-get kwargs :callback)
                       (error "claude-code-extras--run-prompt: :callback required")))
         (args (apply #'claude-code-extras--build-cli-args prompt
                      (cl-loop for key in '(:allowed-tools :system-prompt
                                            :model :max-turns)
                               for val = (plist-get kwargs key)
                               when val append (list key val))))
         (env (let ((filtered (cl-remove-if
                               (lambda (s) (or (string-prefix-p "CLAUDE_CODE" s)
                                               (string-prefix-p "ANTHROPIC_API_KEY=" s)))
                               process-environment)))
                (if-let* ((account (claude-code-extras--resolve-account))
                          (config-dir (alist-get account claude-code-extras-accounts
                                                 nil nil #'string=)))
                    (cons (format "CLAUDE_CONFIG_DIR=%s"
                                  (expand-file-name config-dir))
                          filtered)
                  filtered)))
         (start-time (current-time))
         (output-buf (generate-new-buffer " *claude-run-output*")))
    (let ((process-environment env)
          (default-directory dir))
      (make-process
       :name "claude-run"
       :buffer output-buf
       :command args
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let (result)
             (condition-case err
                 (let* ((exit-code (process-exit-status proc))
                        (raw (with-current-buffer (process-buffer proc)
                               (buffer-string)))
                        (duration (float-time
                                   (time-subtract (current-time) start-time)))
                        (parsed (claude-code-extras--batch-parse-stream-json raw)))
                   (setq result (list :exit-code exit-code
                                      :duration duration
                                      :cost (or (plist-get parsed :cost) 0)
                                      :text (plist-get parsed :text)
                                      :session-id (plist-get parsed :session-id)
                                      :raw raw)))
               (error
                (setq result (list :exit-code -1
                                   :duration (float-time
                                              (time-subtract (current-time)
                                                             start-time))
                                   :cost 0
                                   :text (format "Sentinel error: %S" err)
                                   :session-id nil
                                   :raw ""))))
             (ignore-errors (kill-buffer (process-buffer proc)))
             (funcall callback result))))))))

;;;;; Skill runner

(defun claude-code-extras--parse-skill-frontmatter (file)
  "Parse YAML frontmatter from skill FILE and return a plist.
Returns a plist with keys :name, :description, :argument-hint,
:argument-source, :argument-choices, :argument-default,
:argument-multiple, :user-invocable, or nil if FILE has no
frontmatter."
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
              (when (string-match "^\\([a-z-]+\\): *\\(.*\\)$" line)
                (let ((key (match-string 1 line))
                      (val (string-trim (match-string 2 line))))
                  ;; Strip surrounding quotes
                  (when (string-match "^[\"']\\(.*\\)[\"']$" val)
                    (setq val (match-string 1 val)))
                  (pcase key
                    ("name" (setq result (plist-put result :name val)))
                    ("description" (setq result (plist-put result :description val)))
                    ("argument-hint"
                     (setq result (plist-put result :argument-hint val)))
                    ("argument-source"
                     (setq result (plist-put result :argument-source val)))
                    ("argument-choices"
                     (setq result (plist-put result :argument-choices
                                             (mapcar #'string-trim
                                                     (split-string val "," t)))))
                    ("argument-default"
                     (setq result (plist-put result :argument-default val)))
                    ("argument-multiple"
                     (setq result (plist-put result :argument-multiple
                                             (not (equal val "false")))))
                    ("user-invocable"
                     (setq result (plist-put result :user-invocable
                                             (not (equal val "false")))))
                    ("model"
                     (setq result (plist-put result :model val)))))))))
        result))))

(defun claude-code-extras--discover-skills ()
  "Discover available Claude Code skills.
Scans `~/.claude/skills/' for global skills and the current
project's `.claude/skills/' for project-local skills.  Returns a
list of plists, each with keys :name, :description,
:argument-hint, :user-invocable, :path, :source.  Project skills
shadow global skills with the same name."
  (let ((skills (make-hash-table :test #'equal))
        (global-dir (expand-file-name "~/.claude/skills"))
        (project-dir (when-let* ((proj (project-current)))
                       (expand-file-name ".claude/skills"
                                         (project-root proj)))))
    ;; Scan global skills first
    (when (file-directory-p global-dir)
      (dolist (file (file-expand-wildcards
                     (expand-file-name "*/SKILL.md" global-dir)))
        (when-let* ((meta (claude-code-extras--parse-skill-frontmatter file))
                    (name (plist-get meta :name)))
          (puthash name (append meta (list :path file :source "global"))
                   skills))))
    ;; Project skills shadow global ones
    (when (and project-dir (file-directory-p project-dir))
      (dolist (file (file-expand-wildcards
                     (expand-file-name "*/SKILL.md" project-dir)))
        (when-let* ((meta (claude-code-extras--parse-skill-frontmatter file))
                    (name (plist-get meta :name)))
          (puthash name (append meta (list :path file :source "project"))
                   skills))))
    ;; Filter to user-invocable and collect
    (let (result)
      (maphash (lambda (_name skill)
                 ;; Include unless explicitly marked non-invocable
                 (unless (and (plist-member skill :user-invocable)
                              (not (plist-get skill :user-invocable)))
                   (push skill result)))
               skills)
      (sort result (lambda (a b)
                     (string< (plist-get a :name) (plist-get b :name)))))))

(defun claude-code-extras--skill-argument-candidates (skill)
  "Return completion candidates for SKILL's arguments.
SKILL is a plist from `claude-code-extras--discover-skills'.  If
the skill has an :argument-source glob, resolve it relative to the
skill's directory and return file stems as candidates.  If it has
:argument-choices, return those directly.  Otherwise return nil."
  (or (when-let* ((source (plist-get skill :argument-source))
                  (skill-dir (file-name-directory (plist-get skill :path))))
        (let ((pattern (expand-file-name source skill-dir)))
          (mapcar (lambda (f)
                    (file-name-sans-extension (file-name-nondirectory f)))
                  (file-expand-wildcards pattern))))
      (plist-get skill :argument-choices)))

(defun claude-code-extras--skill-display-result (skill-name result
                                                              &optional buffers-before)
  "Display RESULT plist in a buffer for SKILL-NAME.
When BUFFERS-BEFORE is non-nil, detect whether the skill created
its own buffer (via emacsclient) and add metadata there instead of
creating a separate buffer."
  (let* ((new-buf (when buffers-before
                    (car (cl-remove-if
                          (lambda (b)
                            (or (memq b buffers-before)
                                (string-prefix-p " " (buffer-name b))))
                          (buffer-list)))))
         (meta-text (concat
                     (format "#+cost: $%.4f\n" (plist-get result :cost))
                     (format "#+duration: %.1fs\n" (plist-get result :duration))
                     (if-let* ((sid (plist-get result :session-id)))
                         (format "#+session: %s\n" sid)
                       ""))))
    (if new-buf
        ;; Skill created its own buffer — add metadata there
        (with-current-buffer new-buf
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            ;; Insert metadata after #+title line if present
            (if (looking-at "^#\\+title:.*\n")
                (goto-char (match-end 0))
              (goto-char (point-min)))
            (insert meta-text "\n"))
          (goto-char (point-min))
          (pop-to-buffer new-buf))
      ;; No new buffer — show full result in a dedicated buffer
      (let ((buf (get-buffer-create (format "*Claude Skill: %s*" skill-name))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "#+title: /%s — %s\n"
                            skill-name
                            (format-time-string "%Y-%m-%d %H:%M:%S")))
            (insert meta-text)
            (insert "\n")
            (insert (or (plist-get result :text) "(no output)"))
            (unless (string-suffix-p "\n" (or (plist-get result :text) ""))
              (insert "\n")))
          (org-mode)
          (goto-char (point-min)))
        (pop-to-buffer buf)))
    (message "/%s complete (%.1fs, $%.4f)"
             skill-name
             (plist-get result :duration)
             (plist-get result :cost))))

;;;###autoload
(defun claude-code-extras-run-skill (skill-name &optional arguments dir)
  "Run Claude Code skill SKILL-NAME non-interactively.
ARGUMENTS is an optional string of arguments appended to the
skill invocation.  DIR is the working directory for the process;
defaults to `default-directory'.

Interactively, prompts for the skill with completion, then for
arguments if the skill declares an argument-hint or
argument-source."
  (interactive
   (let* ((skills (claude-code-extras--discover-skills))
          (_ (unless skills (user-error "No user-invocable skills found")))
          (max-len (apply #'max (mapcar (lambda (s)
                                          (length (plist-get s :name)))
                                        skills)))
          (annotate (lambda (cand)
                      (when-let* ((skill (cl-find cand skills
                                                  :key (lambda (s) (plist-get s :name))
                                                  :test #'equal))
                                  (desc (plist-get skill :description)))
                        (concat (make-string (- (+ max-len 2) (length cand)) ?\s)
                                (propertize desc 'face 'completions-annotations)))))
          (name (completing-read
                 "Skill: "
                 (lambda (str pred action)
                   (if (eq action 'metadata)
                       `(metadata (annotation-function . ,annotate))
                     (complete-with-action
                      action
                      (mapcar (lambda (s) (plist-get s :name)) skills)
                      str pred)))))
          (skill (cl-find name skills
                          :key (lambda (s) (plist-get s :name))
                          :test #'equal))
          (hint (and skill (plist-get skill :argument-hint)))
          (candidates (and skill
                           (claude-code-extras--skill-argument-candidates skill)))
          (default (and skill (plist-get skill :argument-default)))
          (multiple-p (and skill (plist-get skill :argument-multiple)))
          (args (cond
                 ;; Completion candidates available
                 ((and candidates multiple-p)
                  (let ((selected (completing-read-multiple
                                   (format "Arguments %s: " (or hint ""))
                                   candidates)))
                    (when selected (string-join selected " "))))
                 (candidates
                  (let ((selected (completing-read
                                   (format "Arguments%s: "
                                           (cond
                                            ((and hint default)
                                             (format " %s (default %s)" hint default))
                                            (hint (format " %s" hint))
                                            (default (format " (default %s)" default))
                                            (t "")))
                                   candidates nil nil nil nil default)))
                    (unless (string-empty-p selected) selected)))
                 ;; No candidates but has a hint — free-form input
                 (hint
                  (let ((input (read-string (format "Arguments %s: " hint))))
                    (unless (string-empty-p input) input))))))
     (list name args nil)))
  (let* ((skill (cl-find skill-name (claude-code-extras--discover-skills)
                         :key (lambda (s) (plist-get s :name))
                         :test #'equal))
         (model (or (and skill (plist-get skill :model))
                    claude-code-extras-run-skill-model))
         (prompt (if (and arguments (not (string-empty-p arguments)))
                     (format "/%s %s" skill-name arguments)
                   (format "/%s" skill-name)))
         (buffers-before (buffer-list)))
    (message "Running /%s%s..." skill-name
             (if (and skill (plist-get skill :model))
                 (format " [%s]" model) ""))
    (claude-code-extras--run-prompt
     prompt
     :dir (or dir default-directory)
     :model model
     :callback
     (lambda (result)
       (claude-code-extras--skill-display-result
        skill-name result buffers-before)))))

;;;;; Batch TODO processing

(defun claude-code-extras--batch-finish (state)
  "Display the batch processing summary buffer for STATE."
  (let* ((results (sort (plist-get state :results)
                        (lambda (a b)
                          (< (plist-get a :index) (plist-get b :index)))))
         (total (length results))
         (successes (cl-count 0 results :key (lambda (r) (plist-get r :exit-code))))
         (failures (- total successes))
         (total-cost (cl-reduce #'+ results :key (lambda (r) (plist-get r :cost))))
         (start-time (plist-get state :start-time))
         (total-time (float-time
                      (time-subtract (current-time) start-time)))
         (buf (get-buffer-create "*Claude Batch Results*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+title: Batch results — %s\n\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S" start-time)))
        (insert (format "- Total: %d | Success: %d | Failed: %d\n" total successes failures))
        (insert (format "- Cost: $%.4f\n" total-cost))
        (insert (format "- Time: %.1f seconds\n" total-time))
        (insert (format "- Logs: [[file:%s]]\n\n" (plist-get state :log-dir)))
        (dolist (result results)
          (let ((status (if (= 0 (plist-get result :exit-code)) "DONE" "FAIL")))
            (insert (format "* %s %s\n" status (plist-get result :title)))
            (insert (format ":PROPERTIES:\n:COST: $%.4f\n:DURATION: %.1fs\n:END:\n\n"
                            (plist-get result :cost)
                            (plist-get result :duration)))
            (insert (format "Log: [[file:%s]]\n\n" (plist-get result :log-file)))
            (insert "#+begin_example\n")
            (insert (or (plist-get result :result-text) "(no output)"))
            (unless (string-suffix-p "\n" (or (plist-get result :result-text) ""))
              (insert "\n"))
            (insert "#+end_example\n\n"))))
      (org-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (message "Batch complete: %d/%d succeeded (%.1fs, $%.4f)"
             successes total total-time total-cost)))

;;;;; Project audit

(defun claude-code-extras-audit-project ()
  "Run a comprehensive audit of a project.
Prompt the user to select a project directory from
`claude-code-extras-audit-project-directories' or enter a new one.
New directories are persisted to the list for future use.
Sequentially invokes each skill in `claude-code-extras-audit-skills'
with `--accept', each in a separate non-interactive Claude session.
Results are displayed in a summary buffer when all audits complete."
  (interactive)
  (let* ((dir (claude-code-extras--read-audit-project-directory))
         (entries (mapcar (lambda (skill)
                            (list :title (format "%s --accept" skill)
                                  :body ""))
                          claude-code-extras-audit-skills)))
    (when (yes-or-no-p
           (format "Run %d audit(s) on %s?" (length entries) dir))
      (claude-code-extras--batch-start entries dir t))))

(defun claude-code-extras--read-audit-project-directory ()
  "Prompt the user for a project directory, with completion.
Offer `claude-code-extras-audit-project-directories' as candidates but allow
free input.  When the entered directory is not already in the list, add it and
persist via `customize-save-variable'."
  (let* ((candidates (mapcar #'abbreviate-file-name
                             claude-code-extras-audit-project-directories))
         (input (completing-read "Project directory: " candidates nil nil))
         (dir (file-truename (expand-file-name input))))
    (unless (file-directory-p dir)
      (user-error "Not a directory: %s" dir))
    (unless (member dir (mapcar #'file-truename
                                claude-code-extras-audit-project-directories))
      (customize-save-variable 'claude-code-extras-audit-project-directories
                               (append claude-code-extras-audit-project-directories
                                       (list dir))))
    dir))

;;;;; Theme sync

(defvar claude-code-extras--last-synced-theme nil
  "The last theme value synced to Claude Code settings.")

(defvar claude-code-extras--sync-theme-timer nil
  "Pending timer for deferred theme sync, or nil.")

(defun claude-code-extras--emacs-theme ()
  "Return \"light\" or \"dark\" based on the current frame background mode."
  (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light"))

(defun claude-code-extras--sync-theme-to-settings ()
  "Update the `theme' key in `~/.claude/settings.json' to match Emacs.
Only writes the file when the theme value actually changes."
  (let* ((theme (claude-code-extras--emacs-theme))
         (settings-file (expand-file-name "~/.claude/settings.json"))
         (settings (condition-case nil
                       (json-parse-string
                        (with-temp-buffer
                          (insert-file-contents settings-file)
                          (buffer-string))
                        :object-type 'hash-table)
                     (error (make-hash-table :test 'equal))))
         (current (gethash "theme" settings)))
    (unless (equal current theme)
      (puthash "theme" theme settings)
      (make-directory (file-name-directory settings-file) t)
      (with-temp-file settings-file
        (insert (json-serialize settings))
        (json-pretty-print-buffer)))))

(defun claude-code-extras--send-theme-to-buffer (buffer theme)
  "Send `/theme' to BUFFER and select THEME.

THEME is \"light\" or \"dark\".  Sends the slash command, waits
for the picker to render, then navigates to the correct option
and confirms.  The picker cursor starts on the currently active
theme, so we arrow up for dark (from light) or down for light
\(from dark).

All terminal sends are scheduled asynchronously via `run-at-time'
so that this function returns immediately.  This prevents timers
from firing reentantly during `accept-process-output' inside eat,
which could otherwise block the main thread indefinitely."
  (with-current-buffer buffer
    (setq claude-code-extras--waiting-for-input nil)
    (setq claude-code-extras--pending-theme nil)
    (let ((buf buffer)
          (navigate (if (string= theme "light") "\e[B" "\e[A"))
          (backend claude-code-terminal-backend))
      (claude-code--term-send-string backend "/theme")
      (run-at-time
       0.1 nil
       (lambda ()
         (when (and (buffer-live-p buf)
                    (process-live-p (get-buffer-process buf)))
           (with-current-buffer buf
             (claude-code--term-send-string backend (kbd "RET"))
             (run-at-time
              0.5 nil
              (lambda ()
                (when (and (buffer-live-p buf)
                           (process-live-p (get-buffer-process buf)))
                  (with-current-buffer buf
                    (claude-code--term-send-string backend navigate)
                    (run-at-time
                     0.1 nil
                     (lambda ()
                       (when (and (buffer-live-p buf)
                                  (process-live-p (get-buffer-process buf)))
                         (with-current-buffer buf
                           (claude-code--term-send-string
                            backend (kbd "RET")))))))))))))))))

(defun claude-code-extras--apply-pending-theme (buffer)
  "Apply the pending theme to BUFFER, if any.
Called from the Stop hook handler when Claude finishes a turn."
  (when-let* ((theme (buffer-local-value 'claude-code-extras--pending-theme buffer)))
    (when (string= theme (claude-code-extras--emacs-theme))
      (claude-code-extras--send-theme-to-buffer buffer theme))))

(defun claude-code-extras--do-sync-theme ()
  "Perform the actual theme sync.
Called from a zero-delay timer scheduled by `claude-code-extras-sync-theme'
so that `enable-theme-functions' and `ns-system-appearance-change-functions'
hooks return immediately and cannot be blocked by reentrant timer activity."
  (setq claude-code-extras--sync-theme-timer nil)
  (let ((theme (claude-code-extras--emacs-theme)))
    (unless (equal theme claude-code-extras--last-synced-theme)
      (setq claude-code-extras--last-synced-theme theme)
      (claude-code-extras--sync-theme-to-settings)
      (dolist (buf (claude-code--find-all-claude-buffers))
        (when (and (buffer-live-p buf)
                   (process-live-p (get-buffer-process buf)))
          (if (buffer-local-value 'claude-code-extras--waiting-for-input buf)
              (claude-code-extras--send-theme-to-buffer buf theme)
            (with-current-buffer buf
              (setq claude-code-extras--pending-theme theme))))))))

(defun claude-code-extras-sync-theme (&rest _)
  "Sync Claude Code theme with the current Emacs background mode.
Updates `~/.claude/settings.json' for new sessions and sends
`/theme' to idle sessions immediately.  For busy sessions (where
Claude is still generating), the theme is queued and applied
automatically when Claude finishes its turn.

The work is deferred to a zero-delay timer so that the calling
hook returns immediately, preventing reentrant timer execution
from blocking the main thread."
  (when claude-code-extras-sync-theme
    (unless claude-code-extras--sync-theme-timer
      (setq claude-code-extras--sync-theme-timer
            (run-at-time 0 nil #'claude-code-extras--do-sync-theme)))))

;;;;; Auto-setup

(defun claude-code-extras-ensure-statusline-config ()
  "Ensure `~/.claude/settings.json' has a `statusLine' entry.
Adds the entry pointing to the bundled shell script if absent."
  (interactive)
  (let ((settings-file (expand-file-name "~/.claude/settings.json")))
    (when (file-exists-p settings-file)
      (with-temp-buffer
        (insert-file-contents settings-file)
        (unless (claude-code-extras--has-statusline-key-p)
          (claude-code-extras--insert-statusline-entry settings-file))))))

(defun claude-code-extras--has-statusline-key-p ()
  "Return non-nil if the current buffer has a `statusLine' JSON key."
  (goto-char (point-min))
  (search-forward "\"statusLine\"" nil t))

(defun claude-code-extras--insert-statusline-entry (file)
  "Insert a `statusLine' entry into the JSON settings FILE.
Finds the last `}' and inserts the entry before it."
  (goto-char (point-max))
  (search-backward "}")
  (insert ",\n    \"statusLine\": {\n"
          "        \"type\": \"command\",\n"
          "        \"command\": \""
          claude-code-extras--statusline-script
          "\",\n"
          "        \"padding\": 0\n"
          "    }\n")
  (write-region (point-min) (point-max) file nil 'quiet))

(defun claude-code-extras-ensure-stop-hook-config ()
  "Ensure `~/.claude/settings.json' has a `Stop' hook.
Adds the hook entry pointing to the bundled wrapper script if
absent."
  (interactive)
  (let ((settings-file (expand-file-name "~/.claude/settings.json")))
    (when (file-exists-p settings-file)
      (with-temp-buffer
        (insert-file-contents settings-file)
        (unless (claude-code-extras--has-stop-hook-p)
          (claude-code-extras--insert-stop-hook settings-file))))))

(defun claude-code-extras--has-stop-hook-p ()
  "Return non-nil if the current buffer has a `Stop' hook."
  (goto-char (point-min))
  (search-forward "\"Stop\"" nil t))

(defun claude-code-extras--insert-stop-hook (file)
  "Insert a `Stop' hook entry into the JSON settings FILE.
Adds a `hooks' object if none exists, or appends to the existing
one."
  (goto-char (point-min))
  (if (search-forward "\"hooks\"" nil t)
      (claude-code-extras--append-to-existing-hooks)
    (claude-code-extras--insert-new-hooks-section))
  (write-region (point-min) (point-max) file nil 'quiet))

(defun claude-code-extras--append-to-existing-hooks ()
  "Append a `Stop' entry to the existing `hooks' object."
  (search-forward "{")
  (insert "\n        \"Stop\": [\n"
          "            {\n"
          "                \"matcher\": \"\",\n"
          "                \"hooks\": [\n"
          "                    {\n"
          "                        \"type\": \"command\",\n"
          "                        \"command\": \""
          claude-code-extras--hook-wrapper
          " stop\"\n"
          "                    }\n"
          "                ]\n"
          "            }\n"
          "        ],"))

(defun claude-code-extras--insert-new-hooks-section ()
  "Insert a new `hooks' section with a `Stop' entry."
  (goto-char (point-max))
  (search-backward "}")
  (insert ",\n    \"hooks\": {\n"
          "        \"Stop\": [\n"
          "            {\n"
          "                \"matcher\": \"\",\n"
          "                \"hooks\": [\n"
          "                    {\n"
          "                        \"type\": \"command\",\n"
          "                        \"command\": \""
          claude-code-extras--hook-wrapper
          " stop\"\n"
          "                    }\n"
          "                ]\n"
          "            }\n"
          "        ]\n"
          "    }\n"))

(defun claude-code-extras-ensure-notification-hook-config ()
  "Ensure `~/.claude/settings.json' has a `Notification' hook.
Adds the hook entry pointing to the notification forwarding script
if absent or empty."
  (interactive)
  (let ((settings-file (expand-file-name "~/.claude/settings.json")))
    (when (file-exists-p settings-file)
      (with-temp-buffer
        (insert-file-contents settings-file)
        (unless (claude-code-extras--has-notification-hook-p)
          (claude-code-extras--insert-notification-hook settings-file))))))

(defun claude-code-extras--has-notification-hook-p ()
  "Return non-nil if the current buffer has a configured Notification hook."
  (goto-char (point-min))
  (search-forward "notify-emacs-notification" nil t))

(defun claude-code-extras--notification-hook-command ()
  "Return the command string for the Notification hook in settings.json."
  (format "%s %s"
          (shell-quote-argument
           (expand-file-name "fire-and-forget.sh"
                             claude-code-extras--hooks-directory))
          (shell-quote-argument
           (expand-file-name "notify-emacs-notification.sh"
                             claude-code-extras--hooks-directory))))

(defun claude-code-extras--insert-notification-hook (file)
  "Insert or fill the Notification hook entry in the JSON settings FILE."
  (let ((cmd (claude-code-extras--notification-hook-command)))
    (goto-char (point-min))
    (cond
     ;; Empty Notification array — replace it
     ((search-forward "\"Notification\": []" nil t)
      (replace-match
       (format (concat "\"Notification\": [\n"
                       "      {\n"
                       "        \"matcher\": \"\",\n"
                       "        \"hooks\": [\n"
                       "          {\n"
                       "            \"type\": \"command\",\n"
                       "            \"command\": \"%s\",\n"
                       "            \"timeout\": 5\n"
                       "          }\n"
                       "        ]\n"
                       "      }\n"
                       "    ]")
               cmd)
       t))
     ;; No Notification key — add to hooks section
     ((progn (goto-char (point-min))
             (search-forward "\"hooks\"" nil t))
      (search-forward "{")
      (insert (format (concat "\n        \"Notification\": [\n"
                              "            {\n"
                              "                \"matcher\": \"\",\n"
                              "                \"hooks\": [\n"
                              "                    {\n"
                              "                        \"type\": \"command\",\n"
                              "                        \"command\": \"%s\",\n"
                              "                        \"timeout\": 5\n"
                              "                    }\n"
                              "                ]\n"
                              "            }\n"
                              "        ],")
                      cmd))))
    (write-region (point-min) (point-max) file nil 'quiet)))

(claude-code-extras-ensure-statusline-config)
(claude-code-extras-ensure-stop-hook-config)
(claude-code-extras-ensure-notification-hook-config)

;; Work around upstream bug: `claude-code--adjust-window-size-advice' crashes
;; when `claude-code--window-widths' is nil or void during redisplay.
(defvar claude-code--window-widths nil)
(unless (hash-table-p claude-code--window-widths)
  (setq claude-code--window-widths
        (make-hash-table :test 'eq :weakness 'key)))

(defun claude-code-extras-disable-scrollback-truncation ()
  "Disable eat scrollback truncation in Claude Code buffers.
The default `eat-term-scrollback-size' of 131072 characters causes the
buffer to be truncated, losing earlier output."
  (interactive)
  (setq-local eat-term-scrollback-size nil))


;; Fix upstream scroll function.  Two problems:
;;
;; 1. `eat--synchronize-scroll-windows' only includes windows whose
;;    `window-point' equals the terminal cursor.  When eat modifies the
;;    buffer during a terminal redraw, Emacs can reset window-point to 1
;;    for non-selected windows.  Once that happens the equality check
;;    fails and the window is permanently excluded from sync.
;;
;; 2. The upstream conditional recenter (checking `pos-visible-in-window-p')
;;    can miss recenters when display state is stale.
;;
;; We fix both by re-including any desynchronized windows and always
;; recentering with `(recenter -1)'.
(advice-add 'claude-code--eat-synchronize-scroll :override
            #'claude-code-extras--eat-synchronize-scroll)

(defun claude-code-extras--eat-synchronize-scroll (windows)
  "Keep the terminal cursor at the bottom of WINDOWS.
Re-include any windows showing this buffer that were excluded from
WINDOWS because their point drifted from the cursor, then
unconditionally recenter with `(recenter -1)'."
  (when (not buffer-read-only)
    (let ((cursor-pos (eat-term-display-cursor eat-terminal)))
      ;; Re-include windows that fell out of sync (point != cursor).
      (dolist (w (get-buffer-window-list nil nil t))
        (unless (memq w windows)
          (push w windows)))
      (dolist (window windows)
        (if (eq window 'buffer)
            (goto-char cursor-pos)
          (set-window-point window cursor-pos)
          (with-selected-window window
            (goto-char cursor-pos)
            (recenter -1)))))))

;;;;; Debug backtrace

(defcustom claude-code-extras-debug-backtrace-model 'gemini-flash-lite-latest
  "GPtel model for identifying candidate packages from a backtrace."
  :type 'symbol
  :group 'claude-code-extras)

(defcustom claude-code-extras-debug-backtrace-backend "Gemini"
  "GPtel backend name for backtrace analysis."
  :type 'string
  :group 'claude-code-extras)

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-tools)
(defvar gptel--known-backends)
(declare-function debug-save-backtrace "init" ())
(declare-function gptel-request "gptel")
(declare-function elpaca-get "elpaca")
(declare-function elpaca-source-dir "elpaca")
(declare-function find-library-name "find-func")

;;;###autoload
(defun claude-code-extras-debug-backtrace ()
  "Save the backtrace, choose the offending package, and open Claude Code.
Use `debug-save-backtrace' to save the backtrace to the downloads
directory, then ask a light LLM (via `gptel') to list all packages
implicated in the error.  The user selects the right one via
`completing-read', then an interactive Claude Code session starts
in that package's source directory with the backtrace file path."
  (interactive)
  (let ((backtrace-file (expand-file-name "backtrace.el" paths-dir-downloads)))
    ;; Schedule the identification work to run after the current command.
    ;; `debug-save-backtrace' kills the *Backtrace* buffer, which exits the
    ;; debugger's `recursive-edit' and unwinds this call frame.
    (run-with-timer 0 nil #'claude-code-extras--debug-identify-package backtrace-file)
    (debug-save-backtrace)))

(defun claude-code-extras--debug-identify-package (backtrace-file)
  "Identify candidate packages from BACKTRACE-FILE and let the user choose.
Ask a light LLM to list all packages implicated in the backtrace,
then present the list via `completing-read' so the user can select
the right one before starting a Claude Code session."
  (unless (file-exists-p backtrace-file)
    (user-error "Backtrace file not found: %s" backtrace-file))
  (message "Identifying packages from backtrace...")
  (let ((contents (with-temp-buffer
                    (insert-file-contents backtrace-file)
                    (buffer-string)))
        (gptel-backend (alist-get claude-code-extras-debug-backtrace-backend
                                 gptel--known-backends nil nil #'string=))
        (gptel-model claude-code-extras-debug-backtrace-model)
        (gptel-use-tools nil))
    (gptel-request
     (format "Backtrace file: %s\n\nContents:\n%s" backtrace-file contents)
     :system "You are an Emacs expert. Given the backtrace, identify ALL Emacs packages that appear in the stack trace and could be the root cause of the error. Return ONLY a comma-separated list of package names, ordered from most likely root cause to least likely. For example: \"org-roam, org, emacsql\" or \"magit, transient, with-editor\"."
     :callback
     (lambda (response info)
       (if (not response)
           (message "gptel request failed: %s" (plist-get info :status))
         (let* ((candidates (mapcar #'string-trim (split-string response ",")))
                (selected (completing-read "Package to debug: " candidates nil nil nil nil
                                           (car candidates))))
           (claude-code-extras--debug-start-session
            (intern selected) backtrace-file)))))))

(declare-function claude-code--start "claude-code")

(defun claude-code-extras--debug-start-session (package backtrace-file)
  "Start a Claude Code session for PACKAGE with BACKTRACE-FILE.
Find the elpaca source directory for PACKAGE, start Claude Code
there with the backtrace prompt passed as a CLI argument."
  (let* ((elpaca-entry (elpaca-get package))
         (dir (cond
               (elpaca-entry (elpaca-source-dir elpaca-entry))
               ((condition-case nil
                    (file-name-directory (find-library-name (symbol-name package)))
                  (error nil)))
               (t (user-error "Package `%s' not found" package))))
         (prompt (format "Read the backtrace at %s. Identify the bug, fix it, and commit the fix."
                         backtrace-file)))
    (message "Starting Claude Code for `%s' in %s..." package dir)
    (cl-letf (((symbol-function 'claude-code--directory) (lambda () dir)))
      (claude-code--start nil (list prompt) nil t))))

(setq claude-code-notification-function #'claude-code-default-notification)
(add-hook 'claude-code-event-hook #'claude-code-extras--handle-notification)
(add-hook 'claude-code-event-hook #'claude-code-extras--handle-stop)
(add-hook 'kill-buffer-query-functions #'claude-code-extras-protect-buffer)
(add-hook 'claude-code-start-hook #'claude-code-extras-setup-kill-on-exit)
(add-hook 'claude-code-start-hook #'claude-code-extras-start-status-polling)
(add-hook 'claude-code-start-hook #'claude-code-extras--capture-buffer-account)
(add-hook 'claude-code-start-hook #'claude-code-extras-set-modeline)
(add-hook 'claude-code-start-hook #'claude-code-extras--refresh-display-names)
(add-hook 'kill-buffer-hook #'claude-code-extras-stop-status-polling)
(add-hook 'kill-buffer-hook #'claude-code-extras--refresh-display-names)
(add-hook 'kill-buffer-hook #'claude-code-extras--cleanup-monet-session)
(add-hook 'claude-code-start-hook #'claude-code-extras-disable-scrollback-truncation)
(add-hook 'claude-code-start-hook #'claude-code-extras-setup-snippet-keys)
(add-hook 'claude-code-start-hook #'claude-code-extras--assign-session-key)
(add-hook 'claude-code-start-hook #'claude-code-extras-setup-copilot)
(add-hook 'kill-buffer-hook #'claude-code-extras--release-session-key)
(add-hook 'kill-buffer-hook #'claude-code-extras-teardown-copilot)
(add-hook 'enable-theme-functions #'claude-code-extras-sync-theme)
(add-hook 'claude-code-start-hook #'claude-code-extras-sync-theme)

;;;;; Handoff

(defconst claude-code-extras-handoff-file
  "/tmp/claude-code-handoff.md"
  "Path to the handoff file written by the `/handoff' skill.")

;;;###autoload
(defun claude-code-extras-handoff ()
  "Close this Claude session and start a new one with the handoff prompt.
The `/handoff' skill must have been run first to write the handoff
file.  The new session starts in the same project directory with
the handoff contents passed as a CLI argument."
  (interactive)
  (unless (file-exists-p claude-code-extras-handoff-file)
    (user-error "No handoff file at %s — run /handoff first"
                claude-code-extras-handoff-file))
  (let* ((prompt (claude-code-extras--read-handoff-file))
         (dir (claude-code-extras--handoff-directory)))
    (when (string-empty-p prompt)
      (user-error "Handoff file is empty — run /handoff first"))
    (claude-code-extras--kill-current-claude-buffer)
    (cl-letf (((symbol-function 'claude-code--directory) (lambda () dir)))
      (claude-code--start nil (list prompt) nil t))))

(defun claude-code-extras--read-handoff-file ()
  "Read and return the trimmed contents of the handoff file."
  (with-temp-buffer
    (insert-file-contents claude-code-extras-handoff-file)
    (string-trim (buffer-string))))

(defun claude-code-extras--handoff-directory ()
  "Return the project directory for the handoff session.
Uses the current Claude buffer's directory if in one."
  (if (claude-code--buffer-p (current-buffer))
      default-directory
    (claude-code--directory)))

(defun claude-code-extras--kill-current-claude-buffer ()
  "Kill the current buffer if it is a Claude session.
Bypasses the kill-protection query."
  (when (claude-code--buffer-p (current-buffer))
    (let ((kill-buffer-query-functions
           (remq 'claude-code-extras-protect-buffer
                 kill-buffer-query-functions)))
      (kill-buffer (current-buffer)))))

;;;;; Branch navigation

(require 'iso8601)

(defun claude-code-extras--read-session-header (jsonl-file)
  "Read first line of JSONL-FILE and return a lightweight metadata plist.
Returns (:session-id :forked-from :fork-uuid :file-path) or nil.
This is fast (reads only first few KB) and is used for the initial
scan to build the branch tree."
  (condition-case nil
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents jsonl-file nil 0 65536))
        (goto-char (point-min))
        (let* ((line (buffer-substring-no-properties
                      (point) (line-end-position)))
               (json (json-parse-string line :object-type 'plist))
               (forked (plist-get json :forkedFrom)))
          (list :session-id (plist-get json :sessionId)
                :forked-from (when forked (plist-get forked :sessionId))
                :fork-uuid (when forked (plist-get forked :messageUuid))
                :file-path jsonl-file)))
    (error nil)))

(defun claude-code-extras--read-session-prompt (header)
  "Enrich HEADER plist with :first-prompt and :timestamp.
Reads the full JSONL file referenced by HEADER's :file-path."
  (let ((file (plist-get header :file-path))
        (fork-uuid (plist-get header :fork-uuid))
        (session-id (plist-get header :session-id))
        (forked-from (plist-get header :forked-from)))
    (condition-case nil
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8))
            (insert-file-contents file))
          (goto-char (point-min))
          (if fork-uuid
              (claude-code-extras--branch-prompt
               session-id forked-from fork-uuid)
            (claude-code-extras--root-prompt session-id)))
      (error (list :session-id session-id
                   :forked-from forked-from
                   :first-prompt "(error reading session)"
                   :timestamp nil)))))

(defun claude-code-extras--user-message-prompt-p (json)
  "Return non-nil if JSON is a user message with text content."
  (and (equal (plist-get json :type) "user")
       (let* ((msg (plist-get json :message))
              (content (when msg (plist-get msg :content))))
         (and (stringp content)
              (not (string-empty-p (string-trim content)))))))

(defun claude-code-extras--root-prompt (session-id)
  "Find the first user prompt in the current buffer for SESSION-ID."
  (goto-char (point-min))
  (let ((result nil))
    (while (and (not result) (not (eobp)))
      (let ((json (claude-code-extras--parse-jsonl-line)))
        (when (and json (claude-code-extras--user-message-prompt-p json))
          (setq result (claude-code-extras--meta-from-json
                        session-id nil json))))
      (forward-line 1))
    (or result
        (list :session-id session-id :forked-from nil
              :first-prompt "(no prompt)" :timestamp nil))))

(defun claude-code-extras--branch-prompt (session-id forked-from fork-uuid)
  "Find the first new user prompt after FORK-UUID in the current buffer.
SESSION-ID and FORKED-FROM are passed through to the result."
  (goto-char (point-min))
  (let ((found-fork nil)
        (result nil))
    (while (and (not result) (not (eobp)))
      (let ((json (claude-code-extras--parse-jsonl-line)))
        (when json
          (if (not found-fork)
              (when (string= (plist-get json :uuid) fork-uuid)
                (setq found-fork t))
            (when (claude-code-extras--user-message-prompt-p json)
              (setq result (claude-code-extras--meta-from-json
                            session-id forked-from json))))))
      (forward-line 1))
    (or result
        (list :session-id session-id
              :forked-from forked-from
              :first-prompt "(branch)"
              :timestamp nil))))

(defun claude-code-extras--parse-jsonl-line ()
  "Parse the current line as JSON, returning a plist or nil."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (unless (string-empty-p line)
      (condition-case nil
          (json-parse-string line :object-type 'plist)
        (error nil)))))

(defun claude-code-extras--meta-from-json (session-id forked-from json)
  "Build metadata plist from SESSION-ID, FORKED-FROM id, and message JSON."
  (let* ((msg (plist-get json :message))
         (content (when msg (plist-get msg :content))))
    (list :session-id session-id
          :forked-from forked-from
          :first-prompt (claude-code-extras--truncate-prompt content)
          :timestamp (plist-get json :timestamp))))

(defun claude-code-extras--truncate-prompt (content)
  "Truncate CONTENT to a short display string."
  (if (stringp content)
      (truncate-string-to-width
       (replace-regexp-in-string "[\n\r\t]+" " " (string-trim content))
       60 nil nil "…")
    "(no prompt)"))

(defun claude-code-extras--scan-session-headers (project-dir)
  "Scan JSONL files in PROJECT-DIR and return session headers.
Returns a hash table mapping session ID to a lightweight header
plist.  Only reads the first line of each file (fast)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (file (directory-files project-dir t "\\.jsonl\\'"))
      (let ((header (claude-code-extras--read-session-header file)))
        (when (and header (plist-get header :session-id))
          (puthash (plist-get header :session-id) header table))))
    table))

(defun claude-code-extras--enrich-sessions (headers member-ids)
  "Read full prompts for sessions in MEMBER-IDS.
HEADERS is a hash table of session ID to header plist.  Returns a
new hash table with :first-prompt and :timestamp populated."
  (let ((table (make-hash-table :test 'equal)))
    (maphash (lambda (id header)
               (when (gethash id member-ids)
                 (puthash id (claude-code-extras--read-session-prompt header)
                          table)))
             headers)
    table))

(defun claude-code-extras--find-branch-root (session-id sessions)
  "Follow forkedFrom chain from SESSION-ID upward in SESSIONS hash table.
Returns the root session ID."
  (let ((current session-id)
        (seen (make-hash-table :test 'equal)))
    (catch 'done
      (while t
        (puthash current t seen)
        (let* ((meta (gethash current sessions))
               (parent (when meta (plist-get meta :forked-from))))
          (if (and parent (gethash parent sessions) (not (gethash parent seen)))
              (setq current parent)
            (throw 'done current)))))))

(defun claude-code-extras--build-children-map (sessions)
  "Build hash table mapping parent session ID to sorted list of child IDs.
SESSIONS is a hash table of session ID to metadata.  Children are
sorted by timestamp."
  (let ((map (make-hash-table :test 'equal)))
    (maphash (lambda (_id meta)
               (let ((parent (plist-get meta :forked-from)))
                 (when (and parent (gethash parent sessions))
                   (push (plist-get meta :session-id)
                         (gethash parent map)))))
             sessions)
    (maphash (lambda (parent children)
               (puthash parent
                        (sort children
                              (lambda (a b)
                                (string< (or (plist-get (gethash a sessions) :timestamp) "")
                                         (or (plist-get (gethash b sessions) :timestamp) ""))))
                        map))
             map)
    map))

(defun claude-code-extras--collect-tree-members (root-id children-map)
  "Return hash table of all session IDs reachable from ROOT-ID via CHILDREN-MAP."
  (let ((members (make-hash-table :test 'equal))
        (queue (list root-id)))
    (while queue
      (let ((id (pop queue)))
        (unless (gethash id members)
          (puthash id t members)
          (dolist (child (gethash id children-map))
            (push child queue)))))
    members))

(defun claude-code-extras--format-branch-timestamp (iso-ts)
  "Format ISO-TS as \"Mon DD HH:MM\" for branch display."
  (when iso-ts
    (condition-case nil
        (format-time-string "%b %d %H:%M"
                            (encode-time (iso8601-parse iso-ts)))
      (error (substring iso-ts 0 (min 16 (length iso-ts)))))))

(defun claude-code-extras--format-branch-tree (root-id sessions children-map current-id)
  "Format the branch tree rooted at ROOT-ID as an alist.
SESSIONS maps IDs to metadata, CHILDREN-MAP maps parent to child
IDs, CURRENT-ID is the active session.  Returns an alist of
\(display-string . session-id)."
  (claude-code-extras--format-branch-subtree
   root-id sessions children-map current-id "" ""))

(defun claude-code-extras--format-branch-subtree
    (id sessions children-map current-id prefix child-prefix)
  "Format branch node ID and its children recursively.
PREFIX is the tree connector for this node, CHILD-PREFIX is the
continuation for children.  Returns a list of (display . session-id)."
  (let* ((meta (gethash id sessions))
         (prompt (or (plist-get meta :first-prompt) "(no prompt)"))
         (ts (claude-code-extras--format-branch-timestamp
              (plist-get meta :timestamp)))
         (marker (if (string= id current-id) " *" ""))
         (display (format "%s%s  %s%s" prefix prompt (or ts "") marker))
         (children (gethash id children-map))
         (len (length children))
         (result (list (cons display id))))
    (cl-loop for child in children
             for i from 0
             for last-p = (= i (1- len))
             do (setq result
                      (nconc result
                             (claude-code-extras--format-branch-subtree
                              child sessions children-map current-id
                              (concat child-prefix (if last-p "└─ " "├─ "))
                              (concat child-prefix (if last-p "   " "│  "))))))
    result))

(defun claude-code-extras--find-buffer-for-session (session-id)
  "Return a live Claude buffer whose session matches SESSION-ID, or nil."
  (cl-find-if
   (lambda (buf)
     (when (buffer-live-p buf)
       (with-current-buffer buf
         (let ((status (claude-code-extras--parse-status-file)))
           (and status
                (string= (plist-get status :session_id) session-id))))))
   (claude-code--find-all-claude-buffers)))

;;;###autoload
(defun claude-code-extras-switch-branch ()
  "Navigate between branches of the current Claude session.
Shows a tree of all sessions related by branching and lets you
select one to switch to or resume."
  (interactive)
  (unless (claude-code--buffer-p (current-buffer))
    (user-error "Not in a Claude buffer"))
  (let ((status (claude-code-extras--parse-status-file)))
    (unless status
      (user-error "No status file; is status polling enabled?"))
    (let ((session-id (plist-get status :session_id))
          (transcript (plist-get status :transcript_path)))
      (unless (and session-id transcript)
        (user-error "Status file missing session_id or transcript_path"))
      (let* ((project-dir (file-name-directory transcript))
             (headers (claude-code-extras--scan-session-headers project-dir))
             (children-map (claude-code-extras--build-children-map headers))
             (root-id (claude-code-extras--find-branch-root session-id headers))
             (members (claude-code-extras--collect-tree-members root-id children-map)))
        (when (<= (hash-table-count members) 1)
          (user-error "No branches for this session"))
        (let* ((sessions (claude-code-extras--enrich-sessions headers members))
               (tree-children (claude-code-extras--build-children-map sessions))
               (tree (claude-code-extras--format-branch-tree
                      root-id sessions tree-children session-id))
               (selection (consult--read
                           (mapcar #'car tree)
                           :prompt "Branch: "
                           :require-match t
                           :sort nil))
               (selected-id (cdr (assoc selection tree))))
          (cond
           ((string= selected-id session-id)
            (message "Already on this session"))
           ((claude-code-extras--find-buffer-for-session selected-id)
            (switch-to-buffer
             (claude-code-extras--find-buffer-for-session selected-id)))
           (t
            (claude-code-extras--resume-session selected-id))))))))

(defun claude-code-extras--resume-session (session-id)
  "Resume SESSION-ID in a new Claude buffer.
Auto-generates an instance name from the session ID to avoid the
interactive instance-name prompt."
  (cl-letf (((symbol-function 'claude-code--prompt-for-instance-name)
             (lambda (_dir _existing _force)
               (format "branch-%s" (substring session-id 0 8)))))
    (claude-code--start nil (list "--resume" session-id) nil t)))

;;;; Transient

;;;###autoload (autoload 'claude-code-extras-menu "claude-code-extras" nil t)
(transient-define-prefix claude-code-extras-menu ()
  "Dispatch a `claude-code-extras' command."
  [["Sessions"
    ("e" "start or switch" claude-code-extras-start-or-switch)
    ("a" "select account" claude-code-extras-select-account)
    ("B" "switch branch" claude-code-extras-switch-branch)
    ("h" "handoff" claude-code-extras-handoff)]
   ["Tools"
    ("s" "run skill" claude-code-extras-run-skill)
    ("b" "batch todos" claude-code-extras-batch-todos)
    ("A" "audit project" claude-code-extras-audit-project)
    ("d" "debug backtrace" claude-code-extras-debug-backtrace)
    ("l" "logs" claude-log-menu)]
   ["Alerts & status"
    ("t" "toggle alert" claude-code-extras-toggle-alert)
    ("p" "start status polling" claude-code-extras-start-status-polling)
    ("P" "stop status polling" claude-code-extras-stop-status-polling)]]
  [["Buffer"
    ("K" "setup kill on exit" claude-code-extras-setup-kill-on-exit)
    ("f" "fix rendering" claude-code-extras-fix-rendering)
    ("S" "disable scrollback truncation" claude-code-extras-disable-scrollback-truncation)]
   ["Setup"
    ("E s" "ensure statusline config" claude-code-extras-ensure-statusline-config)
    ("E n" "ensure notification hook" claude-code-extras-ensure-notification-hook-config)
    ("E h" "ensure stop hook" claude-code-extras-ensure-stop-hook-config)]
   ["Options"
    ("-a" claude-code-extras--infix-alert-on-ready)
    ("-p" claude-code-extras--infix-protect-buffers)
    ("-t" claude-code-extras--infix-sync-theme)
    ("-c" claude-code-extras--infix-copilot-enabled)
    ("-w" claude-code-extras--infix-warn-kill-with-branches)]])

(defclass claude-code-extras--boolean-variable (transient-lisp-variable)
  ()
  "A `transient-lisp-variable' that toggles a boolean on each press.")

(cl-defmethod transient-infix-read ((obj claude-code-extras--boolean-variable))
  "Toggle the boolean value."
  (not (oref obj value)))

(transient-define-infix claude-code-extras--infix-alert-on-ready ()
  "Toggle `claude-code-extras-alert-on-ready'."
  :class 'claude-code-extras--boolean-variable
  :variable 'claude-code-extras-alert-on-ready
  :description "alert on ready")

(transient-define-infix claude-code-extras--infix-protect-buffers ()
  "Toggle `claude-code-extras-protect-buffers'."
  :class 'claude-code-extras--boolean-variable
  :variable 'claude-code-extras-protect-buffers
  :description "protect buffers")

(transient-define-infix claude-code-extras--infix-sync-theme ()
  "Toggle `claude-code-extras-sync-theme'."
  :class 'claude-code-extras--boolean-variable
  :variable 'claude-code-extras-sync-theme
  :description "sync theme")

(transient-define-infix claude-code-extras--infix-copilot-enabled ()
  "Toggle `claude-code-extras-copilot-enabled'."
  :class 'claude-code-extras--boolean-variable
  :variable 'claude-code-extras-copilot-enabled
  :description "copilot")

(transient-define-infix claude-code-extras--infix-warn-kill-with-branches ()
  "Toggle `claude-code-extras-warn-kill-with-branches'."
  :class 'claude-code-extras--boolean-variable
  :variable 'claude-code-extras-warn-kill-with-branches
  :description "warn kill with branches")

(provide 'claude-code-extras)
;;; claude-code-extras.el ends here
