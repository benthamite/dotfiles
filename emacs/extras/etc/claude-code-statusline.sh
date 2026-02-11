#!/bin/bash
# Claude Code statusline script.
# Reads JSON from stdin and writes to temp file for Emacs polling.
# Requires: jq

input=$(cat)
SAFE_NAME=$(printf '%s' "$CLAUDE_BUFFER_NAME" | tr -c 'a-zA-Z0-9_-' '_')
mkdir -p /tmp/claude-code-status
echo "$input" > "/tmp/claude-code-status/${SAFE_NAME}.json"

