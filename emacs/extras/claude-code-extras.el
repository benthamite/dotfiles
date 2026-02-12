;;; claude-code-extras.el --- Extensions for claude-code -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/dotfiles/tree/master/emacs/extras/claude-code-extras.el
;; Version: 0.1
;; Package-Requires: ((claude-code "0.1") (paths "0.1"))

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
  "Delay in seconds before sending SIGWINCH to fix vterm rendering.
After Claude Code starts in a vterm buffer, the TUI may render
incorrectly due to a race condition in terminal size negotiation.
Sending SIGWINCH forces the TUI to re-query terminal dimensions
and redraw."
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

(defvar-local claude-code-extras-alert-on-ready nil
  "When non-nil, alert the user when Claude finishes responding.
This variable is buffer-local, so each Claude session can have
its own notification setting.  Toggle with
`claude-code-extras-toggle-alert'.")

(defcustom claude-code-extras-status-interval 5
  "Interval in seconds between status file polls."
  :type 'integer
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
Returns t if the buffer should be killed, nil otherwise. Intended for use
in `kill-buffer-query-functions'."
  (or (not claude-code-extras-protect-buffers)
      (not (claude-code--buffer-p (current-buffer)))
      (yes-or-no-p "Kill claude-code buffer? ")))

(defun claude-code-extras-fix-vterm-rendering ()
  "Send SIGWINCH to fix vterm rendering after startup.
Works around a race condition where Claude Code's TUI queries
terminal dimensions before the vterm window is fully laid out,
resulting in a garbled banner."
  (when-let* ((proc (get-buffer-process (current-buffer)))
              ((eq claude-code-terminal-backend 'vterm)))
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
  (let* ((choices (claude-code--buffers-to-choices buffers))
         (new-label "[new session]")
         (all-choices (append (mapcar #'car choices) (list new-label)))
         (selection (completing-read "Claude session: " all-choices nil t)))
    (if (string= selection new-label)
        (claude-code)
      (pop-to-buffer (cdr (assoc selection choices))))))

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
           (format "%s: waiting for your response" name)))))
    nil))

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
  "Return non-nil if the current buffer contains a `statusLine' JSON key."
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

(setq claude-code-notification-function #'claude-code-extras-notify)
(add-hook 'claude-code-event-hook #'claude-code-extras--handle-stop)
(add-hook 'kill-buffer-query-functions #'claude-code-extras-protect-buffer)

(provide 'claude-code-extras)
;;; claude-code-extras.el ends here
