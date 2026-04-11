#!/bin/bash
# PreToolUse hook: block Bash commands (except emacsclient) when
# Elisp changes have been committed but not yet verified in the
# running Emacs session.
#
# Works in tandem with track-elisp-verify.sh which sets and clears
# the verification marker.

set -euo pipefail

INPUT=$(cat)

COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
SESSION_ID=$(printf '%s' "$INPUT" | jq -r '.session_id // empty')

MARKER="/tmp/claude-elisp-verify-needed-${SESSION_ID}"

# No marker means no pending verification
if [ ! -f "$MARKER" ]; then
  exit 0
fi

# Allow emacsclient commands through — that's how verification happens
if echo "$COMMAND" | grep -qE '\bemacsclient\b'; then
  exit 0
fi

# Block everything else
REASON="BLOCKED: You committed Elisp changes but have not verified them in the running Emacs. Run \`emacsclient -e\` to exercise the changed code path before continuing. The post-commit hook has already reloaded the code — verify the fix works, don't just compile."
jq -n --arg reason "$REASON" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": $reason
  }
}'
