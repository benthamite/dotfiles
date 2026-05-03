;;; claude-code-extras-test.el --- Tests for claude-code-extras -*- lexical-binding: t -*-

;; Tests for pure and near-pure helper functions in claude-code-extras.el.

;;; Code:

(require 'ert)
(require 'json)
(require 'claude-code-extras)

;;;; Session name extraction

(ert-deftest claude-code-extras-test-session-name-standard ()
  "Extract project name from a standard Claude buffer name."
  (should (equal (claude-code-extras--session-name
                  "*claude:~/path/to/project/:default*")
                 "project")))

(ert-deftest claude-code-extras-test-session-name-named-instance ()
  "Extract project name regardless of instance name."
  (should (equal (claude-code-extras--session-name
                  "*claude:~/repos/my-app/:worktree-1*")
                 "my-app")))

(ert-deftest claude-code-extras-test-session-name-deep-path ()
  "Extract project name from a deeply nested path."
  (should (equal (claude-code-extras--session-name
                  "*claude:~/My Drive/repos/org/subdir/:main*")
                 "subdir")))

(ert-deftest claude-code-extras-test-session-name-non-matching ()
  "Return buffer name unchanged when it does not match the pattern."
  (should (equal (claude-code-extras--session-name "*scratch*")
                 "*scratch*")))

(ert-deftest claude-code-extras-test-session-name-no-trailing-star ()
  "Return buffer name unchanged when trailing asterisk is missing."
  (should (equal (claude-code-extras--session-name
                  "*claude:~/path/to/project/:default")
                 "*claude:~/path/to/project/:default")))

;;;; Sanitize buffer name

(ert-deftest claude-code-extras-test-sanitize-buffer-name-replaces-special ()
  "Non-alphanumeric characters (except _ and -) are replaced with underscores."
  (with-temp-buffer
    (rename-buffer "*claude:~/foo/bar/:default*" t)
    (should (equal (claude-code-extras--sanitize-buffer-name)
                   "_claude___foo_bar__default_"))))

(ert-deftest claude-code-extras-test-sanitize-buffer-name-preserves-safe ()
  "Alphanumeric characters, underscores, and hyphens are preserved."
  (with-temp-buffer
    (rename-buffer "hello_world-123" t)
    (should (equal (claude-code-extras--sanitize-buffer-name)
                   "hello_world-123"))))

(ert-deftest claude-code-extras-test-sanitize-buffer-name-spaces ()
  "Spaces are replaced with underscores."
  (with-temp-buffer
    (rename-buffer "my buffer name" t)
    (should (equal (claude-code-extras--sanitize-buffer-name)
                   "my_buffer_name"))))

;;;; Theme sync

(defun claude-code-extras-test--json-theme (file)
  "Return the `theme' value from JSON FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (gethash "theme" (json-parse-buffer))))

(ert-deftest claude-code-extras-test-sync-theme-writes-config-files ()
  "Persist theme changes to Claude Code JSON config files."
  (let* ((dir (make-temp-file "claude-theme" t))
         (settings (expand-file-name ".claude/settings.json" dir))
         (legacy (expand-file-name ".claude.json" dir))
         (account (expand-file-name "account/.claude.json" dir)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory settings) t)
          (make-directory (file-name-directory account) t)
          (with-temp-file settings
            (insert "{\"theme\":\"light\",\"other\":1}"))
          (with-temp-file legacy
            (insert "{\"theme\":\"light\",\"other\":1}"))
          (with-temp-file account
            (insert "{\"theme\":\"light\"}"))
          (cl-letf (((symbol-function 'claude-code-extras--theme-config-files)
                     (lambda () (list settings legacy account))))
            (should (= (claude-code-extras--sync-theme "dark") 3))
            (should (equal (claude-code-extras-test--json-theme settings)
                           "dark"))
            (should (equal (claude-code-extras-test--json-theme legacy)
                           "dark"))
            (should (equal (claude-code-extras-test--json-theme account)
                           "dark"))))
      (delete-directory dir t))))

(ert-deftest claude-code-extras-test-theme-config-files-prefers-settings ()
  "Sync modern settings files before legacy `.claude.json' files."
  (let* ((dir (make-temp-file "claude-theme" t))
         (settings (expand-file-name "settings.json" dir))
         (missing-settings (expand-file-name "missing/settings.json" dir))
         (legacy (expand-file-name ".claude.json" dir))
         (missing-legacy (expand-file-name "missing/.claude.json" dir)))
    (unwind-protect
        (progn
          (with-temp-file settings (insert "{}"))
          (with-temp-file legacy (insert "{}"))
          (cl-letf (((symbol-function 'claude-code-extras--all-claude-settings-paths)
                     (lambda () (list settings missing-settings)))
                    ((symbol-function 'claude-code-extras--all-claude-json-paths)
                     (lambda () (list legacy missing-legacy))))
            (should (equal (claude-code-extras--theme-config-files)
                           (list settings legacy)))))
      (delete-directory dir t))))

(ert-deftest claude-code-extras-test-sync-theme-skips-unchanged-config ()
  "Avoid rewriting Claude Code JSON files when the theme already matches."
  (let* ((dir (make-temp-file "claude-theme" t))
         (canonical (expand-file-name ".claude.json" dir)))
    (unwind-protect
        (progn
          (with-temp-file canonical
            (insert "{\"theme\":\"dark\"}"))
          (cl-letf (((symbol-function 'claude-code-extras--theme-config-files)
                     (lambda () (list canonical))))
            (should (= (claude-code-extras--sync-theme "dark") 0))))
      (delete-directory dir t))))

(ert-deftest claude-code-extras-test-sync-theme-errors-on-invalid-json ()
  "Do not overwrite an existing invalid Claude Code JSON file."
  (let* ((dir (make-temp-file "claude-theme" t))
         (canonical (expand-file-name ".claude.json" dir)))
    (unwind-protect
        (progn
          (with-temp-file canonical
            (insert "{"))
          (cl-letf (((symbol-function 'claude-code-extras--theme-config-files)
                     (lambda () (list canonical))))
            (should-error (claude-code-extras--sync-theme "dark")))
          (should (equal (with-temp-buffer
                           (insert-file-contents canonical)
                           (buffer-string))
                         "{")))
      (delete-directory dir t))))

(ert-deftest claude-code-extras-test-sync-theme-before-start ()
  "Run shared theme sync before starting a Claude Code process."
  (let ((called nil))
    (cl-letf (((symbol-function 'ai-extras-sync-theme-now)
               (lambda (&rest _) (setq called t))))
      (should-not (claude-code-extras--sync-theme-before-start "buf" "/tmp"))
      (should called))))

;;;; Batch format prompt

(ert-deftest claude-code-extras-test-batch-format-prompt-title-only ()
  "Return title alone when body is empty."
  (should (equal (claude-code-extras--batch-format-prompt
                  '(:title "Fix the bug" :body ""))
                 "Fix the bug")))

(ert-deftest claude-code-extras-test-batch-format-prompt-title-and-body ()
  "Return title and body separated by blank line."
  (should (equal (claude-code-extras--batch-format-prompt
                  '(:title "Fix the bug" :body "See error in log"))
                 "Fix the bug\n\nSee error in log")))

(ert-deftest claude-code-extras-test-batch-format-prompt-nil-body ()
  "Return title alone when body is nil."
  (should (equal (claude-code-extras--batch-format-prompt
                  '(:title "Refactor module" :body nil))
                 "Refactor module")))

;;;; Status accessors

(ert-deftest claude-code-extras-test-status-model-present ()
  "Return display_name when model data is present."
  (let ((claude-code-extras--status-data
         '(:model (:display_name "Claude Opus 4"))))
    (should (equal (claude-code-extras-status-model) "Claude Opus 4"))))

(ert-deftest claude-code-extras-test-status-model-nil ()
  "Return nil when status data has no model."
  (let ((claude-code-extras--status-data nil))
    (should-not (claude-code-extras-status-model))))

(ert-deftest claude-code-extras-test-status-cost-present ()
  "Return total_cost_usd when cost data is present."
  (let ((claude-code-extras--status-data
         '(:cost (:total_cost_usd 0.42))))
    (should (= (claude-code-extras-status-cost) 0.42))))

(ert-deftest claude-code-extras-test-status-cost-nil ()
  "Return nil when status data has no cost."
  (let ((claude-code-extras--status-data nil))
    (should-not (claude-code-extras-status-cost))))

(ert-deftest claude-code-extras-test-status-context-percent ()
  "Return used_percentage from context_window data."
  (let ((claude-code-extras--status-data
         '(:context_window (:used_percentage 73.5))))
    (should (= (claude-code-extras-status-context-percent) 73.5))))

(ert-deftest claude-code-extras-test-status-context-percent-nil ()
  "Return nil when no context_window data."
  (let ((claude-code-extras--status-data nil))
    (should-not (claude-code-extras-status-context-percent))))

(ert-deftest claude-code-extras-test-status-token-count ()
  "Return total_input_tokens from context_window data."
  (let ((claude-code-extras--status-data
         '(:context_window (:total_input_tokens 50000))))
    (should (= (claude-code-extras-status-token-count) 50000))))

(ert-deftest claude-code-extras-test-status-token-count-nil ()
  "Return nil when no context_window data."
  (let ((claude-code-extras--status-data nil))
    (should-not (claude-code-extras-status-token-count))))

(ert-deftest claude-code-extras-test-status-lines-added ()
  "Return total_lines_added from cost data."
  (let ((claude-code-extras--status-data
         '(:cost (:total_lines_added 120))))
    (should (= (claude-code-extras-status-lines-added) 120))))

(ert-deftest claude-code-extras-test-status-lines-removed ()
  "Return total_lines_removed from cost data."
  (let ((claude-code-extras--status-data
         '(:cost (:total_lines_removed 30))))
    (should (= (claude-code-extras-status-lines-removed) 30))))

(ert-deftest claude-code-extras-test-status-duration-ms ()
  "Return total_duration_ms from cost data."
  (let ((claude-code-extras--status-data
         '(:cost (:total_duration_ms 12500))))
    (should (= (claude-code-extras-status-duration-ms) 12500))))

(ert-deftest claude-code-extras-test-status-cache-read-tokens ()
  "Return cache_read_input_tokens from current_usage."
  (let ((claude-code-extras--status-data
         '(:context_window (:current_usage (:cache_read_input_tokens 8000)))))
    (should (= (claude-code-extras-status-cache-read-tokens) 8000))))

(ert-deftest claude-code-extras-test-status-cache-read-tokens-nil ()
  "Return nil when current_usage is missing."
  (let ((claude-code-extras--status-data
         '(:context_window (:used_percentage 50))))
    (should-not (claude-code-extras-status-cache-read-tokens))))

(ert-deftest claude-code-extras-test-status-cache-total-tokens-all-fields ()
  "Sum input_tokens, cache_creation_input_tokens, and cache_read_input_tokens."
  (let ((claude-code-extras--status-data
         '(:context_window
           (:current_usage (:input_tokens 100
                            :cache_creation_input_tokens 200
                            :cache_read_input_tokens 300)))))
    (should (= (claude-code-extras-status-cache-total-tokens) 600))))

(ert-deftest claude-code-extras-test-status-cache-total-tokens-partial ()
  "Missing sub-fields default to zero in the sum."
  (let ((claude-code-extras--status-data
         '(:context_window
           (:current_usage (:cache_read_input_tokens 500)))))
    (should (= (claude-code-extras-status-cache-total-tokens) 500))))

(ert-deftest claude-code-extras-test-status-cache-total-tokens-nil ()
  "Return nil when current_usage is absent."
  (let ((claude-code-extras--status-data
         '(:context_window (:used_percentage 50))))
    (should-not (claude-code-extras-status-cache-total-tokens))))

;;;; Alert indicator

(ert-deftest claude-code-extras-test-alert-indicator-active ()
  "Return bell-on icon when alert is enabled."
  (let ((claude-code-extras-alert-on-ready t))
    (should (equal (claude-code-extras-alert-indicator) "🔔"))))

(ert-deftest claude-code-extras-test-alert-indicator-inactive ()
  "Return bell-off icon when alert is disabled."
  (let ((claude-code-extras-alert-on-ready nil))
    (should (equal (claude-code-extras-alert-indicator) "🔕"))))

(ert-deftest claude-code-extras-test-alert-indicator-uses-shared-state ()
  "Reflect the shared `ai-extras-alert-on-ready' state."
  (let ((ai-extras-alert-on-ready t))
    (should (equal (claude-code-extras-alert-indicator) "🔔"))))

;;;; Transient menu

(ert-deftest claude-code-extras-test-agent-log-menu-is-autoloaded ()
  "Expose `agent-log-menu' as a command for transient suffix validation."
  (should (commandp 'agent-log-menu)))

;;;; Display names

(ert-deftest claude-code-extras-test-display-name-adds-branch-suffix ()
  "Append Claude branch suffixes via the shared display-name hook."
  (with-temp-buffer
    (rename-buffer "*claude:~/repo/unique-claude-display-test/:default*" t)
    (let ((claude-code-extras--original-session-id "original-session")
          (claude-code-extras--status-data
           '(:session_id "branched-session-id")))
      (should (equal (claude-code-extras-display-name (current-buffer))
                     "unique-claude-display-test:branched")))))

;;;; Batch parse stream JSON

(ert-deftest claude-code-extras-test-batch-parse-stream-json-assistant-text ()
  "Extract assistant text from stream-json output."
  (let* ((line1 (json-encode '(:type "assistant"
                                :message (:content [(:type "text" :text "Hello world")]))))
         (line2 (json-encode '(:type "result"
                                :total_cost_usd 0.05
                                :session_id "sess-123"
                                :num_turns 1
                                :subtype "success")))
         (raw (concat line1 "\n" line2))
         (result (claude-code-extras--batch-parse-stream-json raw)))
    (should (equal (plist-get result :text) "Hello world"))
    (should (= (plist-get result :cost) 0.05))
    (should (equal (plist-get result :session-id) "sess-123"))))

(ert-deftest claude-code-extras-test-batch-parse-stream-json-multiple-blocks ()
  "Multiple assistant text blocks are joined with double newlines."
  (let* ((line1 (json-encode '(:type "assistant"
                                :message (:content [(:type "text" :text "Part one")]))))
         (line2 (json-encode '(:type "assistant"
                                :message (:content [(:type "text" :text "Part two")]))))
         (line3 (json-encode '(:type "result" :total_cost_usd 0.1
                                :session_id "s1" :num_turns 2 :subtype "success")))
         (raw (concat line1 "\n" line2 "\n" line3))
         (result (claude-code-extras--batch-parse-stream-json raw)))
    (should (equal (plist-get result :text) "Part one\n\nPart two"))))

(ert-deftest claude-code-extras-test-batch-parse-stream-json-no-text ()
  "Produce fallback message when no assistant text is captured."
  (let* ((line (json-encode '(:type "result" :total_cost_usd 0.0
                               :session_id "s99" :num_turns 0 :subtype "timeout")))
         (raw line)
         (result (claude-code-extras--batch-parse-stream-json raw)))
    (should (string-match-p "No assistant text captured" (plist-get result :text)))
    (should (string-match-p "s99" (plist-get result :text)))))

(ert-deftest claude-code-extras-test-batch-parse-stream-json-cost-usd-fallback ()
  "Use cost_usd when total_cost_usd is absent."
  (let* ((line (json-encode '(:type "result" :cost_usd 0.03
                               :session_id "s1" :num_turns 1 :subtype "ok")))
         (result (claude-code-extras--batch-parse-stream-json line)))
    (should (= (plist-get result :cost) 0.03))))

(ert-deftest claude-code-extras-test-batch-parse-stream-json-malformed-lines ()
  "Malformed JSON lines are silently skipped."
  (let* ((good (json-encode '(:type "result" :total_cost_usd 0.01
                               :session_id "s1" :num_turns 1 :subtype "ok")))
         (raw (concat "not valid json\n" good))
         (result (claude-code-extras--batch-parse-stream-json raw)))
    (should (= (plist-get result :cost) 0.01))))

(ert-deftest claude-code-extras-test-batch-parse-stream-json-empty-input ()
  "Empty input returns zero cost and fallback text."
  (let ((result (claude-code-extras--batch-parse-stream-json "")))
    (should (= (plist-get result :cost) 0))
    (should (string-match-p "No assistant text captured" (plist-get result :text)))))

;;;; Batch build args

(ert-deftest claude-code-extras-test-batch-build-args-minimal ()
  "Build args with only required settings (no optional overrides)."
  (let ((claude-code-program "claude")
        (claude-code-extras-batch-max-turns 10)
        (claude-code-extras-batch-permission-mode nil)
        (claude-code-extras-batch-allowed-tools nil)
        (claude-code-extras-batch-system-prompt nil)
        (claude-code-extras-batch-model nil))
    (should (equal (claude-code-extras--build-cli-args "do stuff")
                   '("claude" "-p" "do stuff"
                     "--output-format" "stream-json"
                     "--verbose"
                     "--max-turns" "10")))))

(ert-deftest claude-code-extras-test-batch-build-args-with-tools ()
  "Include --allowedTools when batch-allowed-tools is set."
  (let ((claude-code-program "claude")
        (claude-code-extras-batch-max-turns 5)
        (claude-code-extras-batch-permission-mode nil)
        (claude-code-extras-batch-allowed-tools '("Read" "Write"))
        (claude-code-extras-batch-system-prompt nil)
        (claude-code-extras-batch-model nil))
    (let ((args (claude-code-extras--build-cli-args "test")))
      (should (member "--allowedTools" args))
      (should (member "Read,Write" args)))))

(ert-deftest claude-code-extras-test-batch-build-args-with-system-prompt ()
  "Include --append-system-prompt when batch-system-prompt is set."
  (let ((claude-code-program "claude")
        (claude-code-extras-batch-max-turns 5)
        (claude-code-extras-batch-permission-mode nil)
        (claude-code-extras-batch-allowed-tools nil)
        (claude-code-extras-batch-system-prompt "Be concise")
        (claude-code-extras-batch-model nil))
    (let ((args (claude-code-extras--build-cli-args "test")))
      (should (member "--append-system-prompt" args))
      (should (member "Be concise" args)))))

(ert-deftest claude-code-extras-test-batch-build-args-with-model ()
  "Include --model when batch-model is set."
  (let ((claude-code-program "claude")
        (claude-code-extras-batch-max-turns 5)
        (claude-code-extras-batch-permission-mode nil)
        (claude-code-extras-batch-allowed-tools nil)
        (claude-code-extras-batch-system-prompt nil)
        (claude-code-extras-batch-model "opus"))
    (let ((args (claude-code-extras--build-cli-args "test")))
      (should (member "--model" args))
      (should (member "opus" args)))))

(ert-deftest claude-code-extras-test-batch-build-args-all-options ()
  "All optional flags appear when all batch variables are set."
  (let ((claude-code-program "/usr/bin/claude")
        (claude-code-extras-batch-max-turns 20)
        (claude-code-extras-batch-permission-mode "bypassPermissions")
        (claude-code-extras-batch-allowed-tools '("Bash" "Read"))
        (claude-code-extras-batch-system-prompt "Be thorough")
        (claude-code-extras-batch-model "sonnet"))
    (let ((args (claude-code-extras--build-cli-args "hello")))
      (should (equal (car args) "/usr/bin/claude"))
      (should (member "--permission-mode" args))
      (should (member "bypassPermissions" args))
      (should (member "--allowedTools" args))
      (should (member "Bash,Read" args))
      (should (member "--append-system-prompt" args))
      (should (member "Be thorough" args))
      (should (member "--model" args))
      (should (member "sonnet" args))
      (should (member "--max-turns" args))
      (should (member "20" args)))))

;;;; Has statusline key

(ert-deftest claude-code-extras-test-has-statusline-key-present ()
  "Return non-nil when buffer contains a statusLine JSON key."
  (with-temp-buffer
    (insert "{\n  \"statusLine\": {}\n}")
    (should (claude-code-extras--has-statusline-key-p))))

(ert-deftest claude-code-extras-test-has-statusline-key-absent ()
  "Return nil when buffer lacks a statusLine JSON key."
  (with-temp-buffer
    (insert "{\n  \"someOtherKey\": true\n}")
    (should-not (claude-code-extras--has-statusline-key-p))))

(ert-deftest claude-code-extras-test-has-statusline-key-empty ()
  "Return nil in an empty buffer."
  (with-temp-buffer
    (should-not (claude-code-extras--has-statusline-key-p))))

;;;; Has stop hook

(ert-deftest claude-code-extras-test-has-stop-hook-present ()
  "Return non-nil when buffer contains a Stop JSON key."
  (with-temp-buffer
    (insert "{\n  \"hooks\": {\n    \"Stop\": []\n  }\n}")
    (should (claude-code-extras--has-stop-hook-p))))

(ert-deftest claude-code-extras-test-has-stop-hook-absent ()
  "Return nil when buffer lacks a Stop JSON key."
  (with-temp-buffer
    (insert "{\n  \"hooks\": {}\n}")
    (should-not (claude-code-extras--has-stop-hook-p))))

(ert-deftest claude-code-extras-test-has-stop-hook-empty ()
  "Return nil in an empty buffer."
  (with-temp-buffer
    (should-not (claude-code-extras--has-stop-hook-p))))

;;;; Insert statusline entry

(ert-deftest claude-code-extras-test-insert-statusline-entry ()
  "Insert statusLine JSON before the final closing brace."
  (with-temp-buffer
    (insert "{\n    \"someKey\": true\n}")
    (let ((temp-file (make-temp-file "statusline-test" nil ".json"))
          (claude-code-extras--statusline-script "/path/to/script.sh"))
      (unwind-protect
          (progn
            (claude-code-extras--insert-statusline-entry temp-file)
            (should (string-match-p "\"statusLine\"" (buffer-string)))
            (should (string-match-p "/path/to/script.sh" (buffer-string)))
            ;; The closing brace should still be present
            (should (string-match-p "}$" (string-trim (buffer-string)))))
        (delete-file temp-file)))))

(ert-deftest claude-code-extras-test-insert-statusline-entry-structure ()
  "Inserted statusLine has expected JSON structure."
  (with-temp-buffer
    (insert "{\n    \"existing\": 1\n}")
    (let ((temp-file (make-temp-file "statusline-test" nil ".json"))
          (claude-code-extras--statusline-script "/test/script"))
      (unwind-protect
          (progn
            (claude-code-extras--insert-statusline-entry temp-file)
            (let ((content (buffer-string)))
              (should (string-match-p "\"type\": \"command\"" content))
              (should (string-match-p "\"padding\": 0" content))
              (should (string-match-p "\"command\": \"/test/script\"" content))))
        (delete-file temp-file)))))

;;;; Batch collect todos

(ert-deftest claude-code-extras-test-batch-collect-todos-buffer-scope ()
  "Collect TODO entries from the entire buffer."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO First task\nSome body text\n* TODO Second task\nMore body\n* DONE Finished\nDone body\n")
    (let ((entries (claude-code-extras--batch-collect-todos 'buffer)))
      (should (= (length entries) 2))
      (should (equal (plist-get (nth 0 entries) :title) "First task"))
      (should (string-match-p "Some body text" (plist-get (nth 0 entries) :body)))
      (should (equal (plist-get (nth 1 entries) :title) "Second task")))))

(ert-deftest claude-code-extras-test-batch-collect-todos-skips-done ()
  "DONE entries are excluded from the collected list."
  (with-temp-buffer
    (org-mode)
    (insert "* DONE Completed\nBody\n* TODO Active\nActive body\n")
    (let ((entries (claude-code-extras--batch-collect-todos 'buffer)))
      (should (= (length entries) 1))
      (should (equal (plist-get (nth 0 entries) :title) "Active")))))

(ert-deftest claude-code-extras-test-batch-collect-todos-empty-body ()
  "TODO entries with no body text get an empty string body."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO No body entry\n* TODO Another entry\n")
    (let ((entries (claude-code-extras--batch-collect-todos 'buffer)))
      (should (= (length entries) 2))
      (should (equal (plist-get (nth 0 entries) :title) "No body entry"))
      (should (string-empty-p (plist-get (nth 0 entries) :body))))))

(ert-deftest claude-code-extras-test-batch-collect-todos-no-todos ()
  "Return nil when buffer has no TODO entries."
  (with-temp-buffer
    (org-mode)
    (insert "* Regular heading\nSome text\n* Another heading\n")
    (let ((entries (claude-code-extras--batch-collect-todos 'buffer)))
      (should (null entries)))))

(ert-deftest claude-code-extras-test-batch-collect-todos-subtree-scope ()
  "Collect only TODO entries within the current subtree."
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** TODO Child task\nChild body\n** DONE Done child\n* TODO Outside\nOutside body\n")
    (goto-char (point-min))
    (save-restriction
      (org-narrow-to-subtree)
      (let ((entries (claude-code-extras--batch-collect-todos 'subtree)))
        (should (= (length entries) 1))
        (should (equal (plist-get (nth 0 entries) :title) "Child task"))))))

(provide 'claude-code-extras-test)
;;; claude-code-extras-test.el ends here
