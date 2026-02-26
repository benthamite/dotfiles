#!/bin/bash
# PostToolUse hook: create a session marker when an `emacs --batch`
# command completes, so the commit hook knows testing was done.
#
# Reads JSON from stdin (Claude Code PostToolUse format).

set -euo pipefail

INPUT=$(cat)

COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')

# Check if the command used emacs --batch (the standard Elisp testing method)
if echo "$COMMAND" | grep -qE 'emacs\s+--batch'; then
  MARKER="/tmp/claude-elisp-tested-${SESSION_ID}"
  touch "$MARKER"
fi

exit 0
