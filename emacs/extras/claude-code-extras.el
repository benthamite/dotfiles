;;; claude-code-extras.el --- Extensions for claude-code -*- lexical-binding: t -*-

;; Copyright (C) 2025

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

;;;; Variables

(defgroup claude-code-extras ()
  "Extensions for `claude-code'."
  :group 'claude-code)

(defcustom claude-code-extras-protect-buffers t
  "When non-nil, prompt for confirmation before killing claude-code buffers."
  :type 'boolean
  :group 'claude-code-extras)

(defcustom claude-code-extras-log-directory
  (expand-file-name "claude-logs" paths-dir-notes)
  "Directory where Claude conversation logs are saved."
  :type 'directory
  :group 'claude-code-extras)

(defcustom claude-code-extras-log-interval 30
  "Interval in seconds between automatic log saves."
  :type 'integer
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

(defconst claude-code-extras--hook-wrapper
  (expand-file-name
   "bin/claude-code-hook-wrapper"
   (expand-file-name "../../repos/claude-code"
                     (file-name-directory
                      (locate-library "claude-code"))))
  "Absolute path to the claude-code hook wrapper script.")

(defconst claude-code-extras--status-directory "/tmp/claude-code-status/"
  "Directory where the statusline script writes JSON status files.")

(defconst claude-code-extras--statusline-script
  (expand-file-name "etc/claude-code-statusline.sh"
                    (file-name-directory
                     (file-truename
                      (or load-file-name buffer-file-name))))
  "Absolute path to the statusline shell script.")

(defvar-local claude-code-extras--log-file nil
  "Log file path for the current Claude buffer.")

(defvar-local claude-code-extras--log-timer nil
  "Timer for periodic logging in the current Claude buffer.")

(defvar-local claude-code-extras--status-data nil
  "Parsed status plist for the current Claude buffer.")

(defvar-local claude-code-extras--status-timer nil
  "Timer for periodic status polling in the current Claude buffer.")

(defvar-local claude-code-extras--copilot-active nil
  "Non-nil when Copilot integration is active in the current buffer.")

(defvar-local claude-code-extras--copilot-change-timer nil
  "Debounce timer for triggering Copilot after eat buffer changes.")

(defvar eat-terminal)
(declare-function json-pretty-print-buffer "json" ())
(declare-function eat-self-input "eat" (n &optional e))
(declare-function eat-term-display-cursor "eat" (terminal))
(declare-function eat-term-send-string "eat" (terminal string))

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

(defun claude-code-extras--get-log-file (buffer)
  "Get or create the log file path for BUFFER."
  (with-current-buffer buffer
    (or claude-code-extras--log-file
        (setq claude-code-extras--log-file
              (expand-file-name
               (format "%s_%s.txt"
                       (replace-regexp-in-string
                        "[^a-zA-Z0-9_-]" "_"
                        (buffer-name))
                       (format-time-string "%Y-%m-%d_%H-%M-%S"))
               claude-code-extras-log-directory)))))

(defun claude-code-extras--save-log (buffer)
  "Save the conversation log for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (claude-code-extras--conversation-cleared-p buffer)
        (setq claude-code-extras--log-file nil))
      (let ((log-file (claude-code-extras--get-log-file buffer)))
        (make-directory claude-code-extras-log-directory t)
        (write-region (point-min) (point-max) log-file nil 'quiet)))))

(defun claude-code-extras--conversation-cleared-p (buffer)
  "Return non-nil if the conversation in BUFFER was cleared.
A conversation is considered cleared when the log file exists and
is at least twice as large as the current BUFFER content,
distinguishing genuine clears from minor terminal fluctuations."
  (when-let* ((log-file claude-code-extras--log-file)
              ((file-exists-p log-file)))
    (> (file-attribute-size (file-attributes log-file))
       (* 2 (buffer-size buffer)))))

(defun claude-code-extras-start-logging ()
  "Start periodic logging for the current Claude buffer."
  (when (claude-code--buffer-p (current-buffer))
    (claude-code-extras--save-log (current-buffer))
    (setq claude-code-extras--log-timer
          (run-with-timer
           claude-code-extras-log-interval
           claude-code-extras-log-interval
           #'claude-code-extras--save-log
           (current-buffer)))))

(defun claude-code-extras-stop-logging ()
  "Stop logging and save final log for the current Claude buffer."
  (when (and (claude-code--buffer-p (current-buffer))
             claude-code-extras--log-timer)
    (cancel-timer claude-code-extras--log-timer)
    (claude-code-extras--save-log (current-buffer))))

(defun claude-code-extras-protect-buffer ()
  "Prompt for confirmation before killing claude-code buffers.
Returns t if the buffer should be killed, nil otherwise.  Skips
the prompt when the session process has already exited (e.g. via
/exit).  Intended for use in `kill-buffer-query-functions'."
  (or (not claude-code-extras-protect-buffers)
      (not (claude-code--buffer-p (current-buffer)))
      (not (process-live-p (get-buffer-process (current-buffer))))
      (yes-or-no-p "Kill claude-code buffer? ")))

(defun claude-code-extras-setup-kill-on-exit ()
  "Arrange for the buffer to be killed when the Claude process exits.
Works with any terminal backend by wrapping the process sentinel."
  (when (claude-code--buffer-p (current-buffer))
    (when-let* ((proc (get-buffer-process (current-buffer))))
      (let ((orig (process-sentinel proc))
            (buf (current-buffer)))
        (set-process-sentinel
         proc
         (lambda (process event)
           (when orig
             (funcall orig process event))
           (when (buffer-live-p buf)
             (kill-buffer buf))))))))

(defun claude-code-extras-fix-rendering ()
  "Send SIGWINCH to fix terminal rendering after startup.
Works around a race condition where Claude Code's TUI queries
terminal dimensions before the terminal window is fully laid out,
resulting in a garbled banner."
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

;;;###autoload
(defun claude-code-extras-start-or-switch ()
  "Start a new Claude session or switch to an existing one.
If no sessions are active, start a new one.  If sessions exist,
offer to switch to one of them or create a new session."
  (interactive)
  (let ((buffers (claude-code--find-all-claude-buffers)))
    (if (null buffers)
        (claude-code)
      (claude-code-extras--prompt-start-or-switch buffers))))

(defun claude-code-extras--prompt-start-or-switch (buffers)
  "Prompt to switch to one of BUFFERS or start a new session."
  (let* ((choices (claude-code-extras--buffers-to-choices buffers))
         (new-label "[new session]")
         (all-choices (append (mapcar #'car choices) (list new-label)))
         (selection (consult--read
                     all-choices
                     :prompt "Claude session: "
                     :require-match t
                     :sort nil
                     :state (claude-code-extras--buffer-preview-state choices))))
    (if (string= selection new-label)
        (claude-code)
      (switch-to-buffer (cdr (assoc selection choices))))))

(defun claude-code-extras--buffers-to-choices (buffers)
  "Convert BUFFERS to an alist of (display-name . buffer) pairs.
Use the project name as display name, appending the instance name
only when multiple sessions share the same project.  If names
still collide after qualification, append a numeric suffix."
  (let* ((names (mapcar #'claude-code-extras--buffer-session-name buffers))
         (duplicates (claude-code-extras--find-duplicate-names names))
         (qualified (cl-mapcar
                     (lambda (buf name)
                       (if (member name duplicates)
                           (claude-code-extras--qualified-session-name
                            (buffer-name buf))
                         name))
                     buffers names)))
    (cl-mapcar #'cons
               (claude-code-extras--deduplicate-names qualified)
               buffers)))

(defun claude-code-extras--buffer-session-name (buffer)
  "Return the session name for BUFFER."
  (claude-code-extras--session-name (buffer-name buffer)))

(defun claude-code-extras--deduplicate-names (names)
  "Add numeric suffixes to duplicate entries in NAMES.
The first occurrence keeps its original name; subsequent
duplicates get \" (2)\", \" (3)\", etc."
  (let ((counts (make-hash-table :test 'equal)))
    (mapcar (lambda (name)
              (let ((n (1+ (or (gethash name counts) 0))))
                (puthash name n counts)
                (if (= n 1) name (format "%s (%d)" name n))))
            names)))

(defun claude-code-extras--find-duplicate-names (names)
  "Return the list of NAMES that appear more than once."
  (let (seen dups)
    (dolist (name names dups)
      (if (member name seen)
          (cl-pushnew name dups :test #'string=)
        (push name seen)))))

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

(defun claude-code-extras--buffer-preview-state (choices)
  "Return a preview state function for CHOICES.
CHOICES is an alist of (display-name . buffer) pairs."
  (let ((orig-buf (window-buffer (consult--original-window)))
        selected)
    (lambda (action cand)
      (pcase action
        ('return (setq selected t))
        ('exit
         (when (and (not selected) (buffer-live-p orig-buf))
           (set-window-buffer (consult--original-window) orig-buf)))
        ('preview
         (claude-code-extras--preview-buffer cand choices))))))

(defun claude-code-extras--preview-buffer (cand choices)
  "Show the buffer for CAND from CHOICES in the original window."
  (when-let* ((cand)
              (buf (cdr (assoc cand choices)))
              ((buffer-live-p buf)))
    (set-window-buffer (consult--original-window) buf)))

;;;;; Status polling

(defun claude-code-extras-start-status-polling ()
  "Start polling the status file for the current Claude buffer."
  (when (claude-code--buffer-p (current-buffer))
    (setq claude-code-extras--status-timer
          (run-with-timer
           claude-code-extras-status-interval
           claude-code-extras-status-interval
           #'claude-code-extras--read-status
           (current-buffer)))))

(defun claude-code-extras-stop-status-polling ()
  "Stop status polling and clean up the status file."
  (when (and (claude-code--buffer-p (current-buffer))
             claude-code-extras--status-timer)
    (cancel-timer claude-code-extras--status-timer)
    (claude-code-extras--cleanup-status-file)))

(defun claude-code-extras--read-status (buffer)
  "Read and parse the status file for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((data (claude-code-extras--parse-status-file)))
        (setq claude-code-extras--status-data data)))))

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

(defun claude-code-extras--handle-stop (message)
  "Handle a stop event from the Claude Code CLI.
MESSAGE is a plist with :type, :buffer-name, :json-data, and
:args.  Only acts on `stop' events."
  (when (eq (plist-get message :type) 'stop)
    (when-let* ((buf (get-buffer (plist-get message :buffer-name))))
      (with-current-buffer buf
        (let ((name (claude-code-extras--session-name (buffer-name))))
          (claude-code-extras-notify
           "Claude ready"
           (format "%s: waiting for your response" name)))
        (claude-code-extras--scroll-to-bottom buf))))
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
  "Accept a Copilot completion if visible, otherwise send TAB to eat."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion)
    (eat-self-input 1 ?\t)))

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

;;;;; Theme sync

(defvar claude-code-extras--last-synced-theme nil
  "The last theme value synced to Claude Code settings.")

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

THEME is \"light\" or \"dark\".  Sends the slash command followed
by the navigation keystrokes with a short delay, so the picker is
dismissed almost immediately after rendering.  The picker cursor
starts on the currently active theme, so we arrow up for dark
\(from light) or down for light (from dark)."
  (with-current-buffer buffer
    (claude-code--term-send-string
     claude-code-terminal-backend "/theme\r")
    (let ((buf buffer)
          (navigate (if (string= theme "light") "\e[B" "\e[A")))
      (run-at-time
       0.3 nil
       (lambda ()
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (claude-code--term-send-string
              claude-code-terminal-backend
              (concat navigate "\r")))))))))

(defun claude-code-extras-sync-theme (&rest _)
  "Sync Claude Code theme with the current Emacs background mode.
Updates `~/.claude/settings.json' for new sessions and sends
`/theme' to all running sessions when the background mode
changes."
  (let ((theme (claude-code-extras--emacs-theme)))
    (unless (equal theme claude-code-extras--last-synced-theme)
      (setq claude-code-extras--last-synced-theme theme)
      (claude-code-extras--sync-theme-to-settings)
      (dolist (buf (claude-code--find-all-claude-buffers))
        (when (and (buffer-live-p buf)
                   (process-live-p (get-buffer-process buf)))
          (claude-code-extras--send-theme-to-buffer buf theme))))))

;;;;; Auto-setup

(defun claude-code-extras-ensure-statusline-config ()
  "Ensure `~/.claude/settings.json' has a `statusLine' entry.
Adds the entry pointing to the bundled shell script if absent."
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

(claude-code-extras-ensure-statusline-config)
(claude-code-extras-ensure-stop-hook-config)

;; Work around upstream bug: `claude-code--adjust-window-size-advice' crashes
;; when `claude-code--window-widths' is nil or void during redisplay.
(defvar claude-code--window-widths nil)
(unless (hash-table-p claude-code--window-widths)
  (setq claude-code--window-widths
        (make-hash-table :test 'eq :weakness 'key)))

;; Fix upstream scroll function: `(recenter)' centers the cursor mid-window,
;; which makes the view jump upward when there is scrollback above.  Using
;; `(recenter -1)' keeps the cursor at the bottom, matching terminal behavior.
(advice-add 'claude-code--eat-synchronize-scroll :override
            #'claude-code-extras--eat-synchronize-scroll)

(defun claude-code-extras--eat-synchronize-scroll (windows)
  "Keep the terminal cursor at the bottom of WINDOWS.
Like `claude-code--eat-synchronize-scroll' but uses (recenter -1)
instead of (recenter) when the cursor is not visible, preventing
the view from jumping to the middle of the buffer."
  (dolist (window windows)
    (if (eq window 'buffer)
        (goto-char (eat-term-display-cursor eat-terminal))
      (when (not buffer-read-only)
        (let ((cursor-pos (eat-term-display-cursor eat-terminal)))
          (set-window-point window cursor-pos)
          (cond
           ((>= cursor-pos (- (point-max) 2))
            (with-selected-window window
              (goto-char cursor-pos)
              (recenter -1)))
           ((not (pos-visible-in-window-p cursor-pos window))
            (with-selected-window window
              (goto-char cursor-pos)
              (recenter -1)))))))))

(setq claude-code-notification-function #'claude-code-extras-notify)
(add-hook 'claude-code-event-hook #'claude-code-extras--handle-stop)
(add-hook 'kill-buffer-query-functions #'claude-code-extras-protect-buffer)
(add-hook 'claude-code-start-hook #'claude-code-extras-setup-kill-on-exit)
(add-hook 'claude-code-start-hook #'claude-code-extras-start-logging)
(add-hook 'claude-code-start-hook #'claude-code-extras-start-status-polling)
(add-hook 'claude-code-start-hook #'claude-code-extras-set-modeline)
(add-hook 'kill-buffer-hook #'claude-code-extras-stop-logging)
(add-hook 'kill-buffer-hook #'claude-code-extras-stop-status-polling)
(add-hook 'claude-code-start-hook #'claude-code-extras-setup-copilot)
(add-hook 'kill-buffer-hook #'claude-code-extras-teardown-copilot)
(add-hook 'enable-theme-functions #'claude-code-extras-sync-theme)
(add-hook 'claude-code-start-hook #'claude-code-extras--sync-theme-to-settings)

(provide 'claude-code-extras)
;;; claude-code-extras.el ends here
