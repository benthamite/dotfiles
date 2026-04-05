#!/bin/bash
# Forward Notification events to Emacs via claude-code-event-hook.
# Called by the Claude Code "Notification" hook; intended to run through
# fire-and-forget.sh so the CLI doesn't block on emacsclient.
# Reads JSON from stdin and passes it as an additional emacsclient argument
# so --handle-notification can determine the notification type.
json_data=$(cat)
buf=${CLAUDE_BUFFER_NAME:-}
# Escape backslashes and double-quotes so the value is safe inside an Elisp string.
buf=${buf//\\/\\\\}
buf=${buf//\"/\\\"}
emacsclient --eval "(claude-code-handle-hook 'notification \"${buf}\")" "$json_data" 2>/dev/null || true
