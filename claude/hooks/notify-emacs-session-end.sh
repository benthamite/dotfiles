#!/bin/bash
# Forward session-end event to Emacs via claude-code-event-hook.
# Called by the Claude Code "Stop" hook; intended to run through
# fire-and-forget.sh so the CLI doesn't block on emacsclient.
emacsclient -e "(claude-code-handle-hook 'stop \"${CLAUDE_BUFFER_NAME:-}\")" 2>/dev/null || true
