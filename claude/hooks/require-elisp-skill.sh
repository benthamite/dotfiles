#!/bin/bash
# PreToolUse hook: block .el edits until the elisp-conventions skill
# has been loaded in this session.
#
# The skill sets a session marker via a companion PostToolUse hook
# (track-elisp-skill.sh).  If no marker exists, the edit is denied
# with a message telling Claude to load the skill first.

set -euo pipefail

INPUT=$(cat)

FILE_PATH=$(printf '%s' "$INPUT" | jq -r '.tool_input.file_path // empty')
SESSION_ID=$(printf '%s' "$INPUT" | jq -r '.session_id // empty')

# Only act on .el files (not .elc, not tests)
[[ "$FILE_PATH" == *.el ]]      || exit 0
[[ "$FILE_PATH" != *.elc ]]     || exit 0

# Check for skill marker
MARKER="/tmp/claude-elisp-skill-${SESSION_ID}"
if [ -f "$MARKER" ]; then
  exit 0
fi

jq -n '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": "BLOCKED: You must load the elisp-conventions skill before editing .el files. Run: Skill(skill: \"elisp-conventions\")"
  }
}'
