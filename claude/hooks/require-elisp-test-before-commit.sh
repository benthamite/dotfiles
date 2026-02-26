#!/bin/bash
# PreToolUse hook: block git commit when Elisp files are staged
# but no test has been run in this session.
#
# Reads JSON from stdin (Claude Code PreToolUse format).
# Outputs JSON with permissionDecision to allow or deny.

set -euo pipefail

INPUT=$(cat)

COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')

# Only intercept git commit commands
if ! echo "$COMMAND" | grep -qE '^\s*git\s+commit\b'; then
  exit 0
fi

# Check if any staged files are Elisp-related
STAGED=$(git diff --cached --name-only 2>/dev/null || true)
if [ -z "$STAGED" ]; then
  exit 0
fi

HAS_ELISP=false
while IFS= read -r file; do
  case "$file" in
    *.el|emacs/config.org)
      HAS_ELISP=true
      break
      ;;
  esac
done <<< "$STAGED"

if [ "$HAS_ELISP" = false ]; then
  exit 0
fi

# Check for test marker
MARKER="/tmp/claude-elisp-tested-${SESSION_ID}"
if [ -f "$MARKER" ]; then
  # Tests were run; allow the commit and remove the marker
  rm -f "$MARKER"
  exit 0
fi

# Block the commit
jq -n '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": "BLOCKED: Elisp files are staged but you have not tested them in this session. You MUST run `emacs --batch` to verify the changed code before committing. For config.org changes, extract the modified source block and eval it. For .el files, byte-compile them."
  }
}'
