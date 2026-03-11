;;; claude-code-extras-test.el --- Tests for claude-code-extras -*- lexical-binding: t -*-

;; Tests for pure and near-pure helper functions in claude-code-extras.el.

;;; Code:

(require 'ert)
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

;;;; Find duplicate names

(ert-deftest claude-code-extras-test-find-duplicate-names-no-duplicates ()
  "Return nil when all names are unique."
  (should (null (claude-code-extras--find-duplicate-names
                 '("alpha" "beta" "gamma")))))

(ert-deftest claude-code-extras-test-find-duplicate-names-one-duplicate ()
  "Return a list containing the duplicated name."
  (should (equal (claude-code-extras--find-duplicate-names
                  '("alpha" "beta" "alpha"))
                 '("alpha"))))

(ert-deftest claude-code-extras-test-find-duplicate-names-multiple-duplicates ()
  "Return all names that appear more than once."
  (let ((result (claude-code-extras--find-duplicate-names
                 '("alpha" "beta" "alpha" "beta" "gamma"))))
    (should (member "alpha" result))
    (should (member "beta" result))
    (should (= (length result) 2))))

(ert-deftest claude-code-extras-test-find-duplicate-names-empty ()
  "Return nil for an empty list."
  (should (null (claude-code-extras--find-duplicate-names '()))))

(ert-deftest claude-code-extras-test-find-duplicate-names-triple ()
  "A name appearing three times is reported once."
  (let ((result (claude-code-extras--find-duplicate-names
                 '("x" "x" "x"))))
    (should (equal result '("x")))))

;;;; Deduplicate names

(ert-deftest claude-code-extras-test-deduplicate-names-no-duplicates ()
  "Return names unchanged when all are unique."
  (should (equal (claude-code-extras--deduplicate-names
                  '("alpha" "beta" "gamma"))
                 '("alpha" "beta" "gamma"))))

(ert-deftest claude-code-extras-test-deduplicate-names-with-duplicates ()
  "First occurrence keeps its name; subsequent get numeric suffixes."
  (should (equal (claude-code-extras--deduplicate-names
                  '("proj" "proj" "proj"))
                 '("proj" "proj (2)" "proj (3)"))))

(ert-deftest claude-code-extras-test-deduplicate-names-mixed ()
  "Only duplicate names get suffixes; unique names are unchanged."
  (should (equal (claude-code-extras--deduplicate-names
                  '("alpha" "beta" "alpha" "gamma" "beta"))
                 '("alpha" "beta" "alpha (2)" "gamma" "beta (2)"))))

(ert-deftest claude-code-extras-test-deduplicate-names-empty ()
  "Return nil for an empty list."
  (should (null (claude-code-extras--deduplicate-names '()))))

(ert-deftest claude-code-extras-test-deduplicate-names-single ()
  "A single-element list is returned unchanged."
  (should (equal (claude-code-extras--deduplicate-names '("only"))
                 '("only"))))

;;;; Emacs theme detection

(ert-deftest claude-code-extras-test-emacs-theme-dark ()
  "Return \"dark\" when frame background mode is dark."
  (let ((frame-params '((background-mode . dark))))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame param) (alist-get param frame-params))))
      (should (equal (claude-code-extras--emacs-theme) "dark")))))

(ert-deftest claude-code-extras-test-emacs-theme-light ()
  "Return \"light\" when frame background mode is light."
  (let ((frame-params '((background-mode . light))))
    (cl-letf (((symbol-function 'frame-parameter)
               (lambda (_frame param) (alist-get param frame-params))))
      (should (equal (claude-code-extras--emacs-theme) "light")))))

;;;; Theme sync busy/idle handling

(ert-deftest claude-code-extras-test-sync-theme-queues-when-busy ()
  "Queue theme for busy sessions instead of sending immediately."
  (with-temp-buffer
    (setq claude-code-extras--waiting-for-input nil)
    (setq claude-code-extras--pending-theme nil)
    (let ((claude-code-extras-sync-theme t)
          (claude-code-extras--last-synced-theme nil)
          (frame-params '((background-mode . dark)))
          (sent nil))
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (_frame param) (alist-get param frame-params)))
                ((symbol-function 'claude-code--find-all-claude-buffers)
                 (lambda () (list (current-buffer))))
                ((symbol-function 'get-buffer-process)
                 (lambda (_buf) t))
                ((symbol-function 'process-live-p)
                 (lambda (_proc) t))
                ((symbol-function 'claude-code-extras--sync-theme-to-settings)
                 #'ignore)
                ((symbol-function 'claude-code-extras--send-theme-to-buffer)
                 (lambda (_buf _theme) (setq sent t))))
        (claude-code-extras-sync-theme)
        (should-not sent)
        (should (equal claude-code-extras--pending-theme "dark"))))))

(ert-deftest claude-code-extras-test-sync-theme-sends-when-idle ()
  "Send theme immediately for idle sessions."
  (with-temp-buffer
    (setq claude-code-extras--waiting-for-input t)
    (setq claude-code-extras--pending-theme nil)
    (let ((claude-code-extras-sync-theme t)
          (claude-code-extras--last-synced-theme nil)
          (frame-params '((background-mode . light)))
          (sent-theme nil))
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (_frame param) (alist-get param frame-params)))
                ((symbol-function 'claude-code--find-all-claude-buffers)
                 (lambda () (list (current-buffer))))
                ((symbol-function 'get-buffer-process)
                 (lambda (_buf) t))
                ((symbol-function 'process-live-p)
                 (lambda (_proc) t))
                ((symbol-function 'claude-code-extras--sync-theme-to-settings)
                 #'ignore)
                ((symbol-function 'claude-code-extras--send-theme-to-buffer)
                 (lambda (_buf theme) (setq sent-theme theme))))
        (claude-code-extras-sync-theme)
        (should (equal sent-theme "light"))))))

(ert-deftest claude-code-extras-test-apply-pending-theme-on-stop ()
  "Apply pending theme when the Stop hook fires."
  (with-temp-buffer
    (setq claude-code-extras--pending-theme "dark")
    (let ((frame-params '((background-mode . dark)))
          (sent nil))
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (_frame param) (alist-get param frame-params)))
                ((symbol-function 'claude-code-extras--send-theme-to-buffer)
                 (lambda (_buf _theme) (setq sent t))))
        (claude-code-extras--apply-pending-theme (current-buffer))
        (should sent)))))

(ert-deftest claude-code-extras-test-pending-theme-skipped-when-stale ()
  "Skip pending theme if Emacs theme changed again since it was queued."
  (with-temp-buffer
    (setq claude-code-extras--pending-theme "light")
    (let ((frame-params '((background-mode . dark)))
          (sent nil))
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (_frame param) (alist-get param frame-params)))
                ((symbol-function 'claude-code-extras--send-theme-to-buffer)
                 (lambda (_buf _theme) (setq sent t))))
        (claude-code-extras--apply-pending-theme (current-buffer))
        (should-not sent)))))

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
        (claude-code-extras-batch-allowed-tools nil)
        (claude-code-extras-batch-system-prompt nil)
        (claude-code-extras-batch-model nil))
    (should (equal (claude-code-extras--batch-build-args "do stuff")
                   '("claude" "-p" "do stuff"
                     "--output-format" "stream-json"
                     "--verbose"
                     "--max-turns" "10")))))

(ert-deftest claude-code-extras-test-batch-build-args-with-tools ()
  "Include --allowedTools when batch-allowed-tools is set."
  (let ((claude-code-program "claude")
        (claude-code-extras-batch-max-turns 5)
        (claude-code-extras-batch-allowed-tools '("Read" "Write"))
        (claude-code-extras-batch-system-prompt nil)
        (claude-code-extras-batch-model nil))
    (let ((args (claude-code-extras--batch-build-args "test")))
      (should (member "--allowedTools" args))
      (should (member "Read,Write" args)))))

(ert-deftest claude-code-extras-test-batch-build-args-with-system-prompt ()
  "Include --append-system-prompt when batch-system-prompt is set."
  (let ((claude-code-program "claude")
        (claude-code-extras-batch-max-turns 5)
        (claude-code-extras-batch-allowed-tools nil)
        (claude-code-extras-batch-system-prompt "Be concise")
        (claude-code-extras-batch-model nil))
    (let ((args (claude-code-extras--batch-build-args "test")))
      (should (member "--append-system-prompt" args))
      (should (member "Be concise" args)))))

(ert-deftest claude-code-extras-test-batch-build-args-with-model ()
  "Include --model when batch-model is set."
  (let ((claude-code-program "claude")
        (claude-code-extras-batch-max-turns 5)
        (claude-code-extras-batch-allowed-tools nil)
        (claude-code-extras-batch-system-prompt nil)
        (claude-code-extras-batch-model "opus"))
    (let ((args (claude-code-extras--batch-build-args "test")))
      (should (member "--model" args))
      (should (member "opus" args)))))

(ert-deftest claude-code-extras-test-batch-build-args-all-options ()
  "All optional flags appear when all batch variables are set."
  (let ((claude-code-program "/usr/bin/claude")
        (claude-code-extras-batch-max-turns 20)
        (claude-code-extras-batch-allowed-tools '("Bash" "Read"))
        (claude-code-extras-batch-system-prompt "Be thorough")
        (claude-code-extras-batch-model "sonnet"))
    (let ((args (claude-code-extras--batch-build-args "hello")))
      (should (equal (car args) "/usr/bin/claude"))
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
