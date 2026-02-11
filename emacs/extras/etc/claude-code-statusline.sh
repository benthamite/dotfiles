#!/bin/bash
# Claude Code statusline script.
# Reads JSON from stdin, writes to temp file for Emacs polling,
# and outputs minimal status for the terminal bar.
# Requires: jq

input=$(cat)
SAFE_NAME=$(echo "$CLAUDE_BUFFER_NAME" | tr -c 'a-zA-Z0-9_-' '_')
mkdir -p /tmp/claude-code-status
echo "$input" > "/tmp/claude-code-status/${SAFE_NAME}.json"

MODEL=$(echo "$input" | jq -r '.model.display_name // "?"')
PCT=$(echo "$input" | jq -r '.context_window.used_percentage // 0' | cut -d. -f1)
COST=$(printf "%.2f" "$(echo "$input" | jq -r '.cost.total_cost_usd // 0')")
echo "[$MODEL] ${PCT}% context | \$${COST}"
