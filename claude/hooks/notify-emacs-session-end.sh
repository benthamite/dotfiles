#!/bin/bash
# Forward session-end event to Emacs via claude-code-event-hook.
# Called by the Claude Code "Stop" hook; intended to run through
# fire-and-forget.sh so the CLI doesn't block on emacsclient.
buf=${CLAUDE_BUFFER_NAME:-}
# Escape backslashes and double-quotes so the value is safe inside an Elisp string.
buf=${buf//\\/\\\\}
buf=${buf//\"/\\\"}
emacsclient -e "(claude-code-handle-hook 'stop \"${buf}\")" 2>/dev/null || true
