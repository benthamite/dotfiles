#!/bin/bash
# Forward Notification events to Emacs via claude-code-event-hook.
# Called by the Claude Code "Notification" hook; intended to run through
# fire-and-forget.sh so the CLI doesn't block on emacsclient.
# Reads JSON from stdin and passes it as an additional emacsclient argument
# so --handle-notification can determine the notification type.
json_data=$(cat)
emacsclient --eval "(claude-code-handle-hook 'notification \"${CLAUDE_BUFFER_NAME:-}\")" "$json_data" 2>/dev/null || true
