#!/bin/bash
# PostToolUse hook: create a session marker when the elisp-conventions
# skill is loaded, so the edit-blocking hook allows .el edits.

set -euo pipefail

INPUT=$(cat)

SKILL=$(printf '%s' "$INPUT" | jq -r '.tool_input.skill // empty')
SESSION_ID=$(printf '%s' "$INPUT" | jq -r '.session_id // empty')

# Only act on the elisp-conventions skill
[[ "$SKILL" == "elisp-conventions" ]] || exit 0

MARKER="/tmp/claude-elisp-skill-${SESSION_ID}"
touch "$MARKER"

# Clean up stale markers from old sessions (>2 hours)
find /tmp -maxdepth 1 -name 'claude-elisp-skill-*' -mmin +120 -delete 2>/dev/null || true

exit 0
