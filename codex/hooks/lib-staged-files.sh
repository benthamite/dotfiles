#!/bin/bash
# Shared helper: list staged files, handling --amend correctly.
#
# When amending, `git diff --cached --name-only` compares against HEAD,
# so files already in the commit being amended are invisible.  This
# helper detects --amend in the command string and compares against
# HEAD~1 instead, giving hooks the full picture of the amended commit.
#
# Usage: source this file after setting $COMMAND, then use $STAGED.
#
#   COMMAND=$(codex_shell_command "$INPUT")
#   source "$(dirname "$0")/lib-staged-files.sh"
#   # $STAGED now contains the list of staged files
#
# $STAGED_BASE holds the revision the file list was computed against:
# "HEAD~1" when amending, empty otherwise.  A hook that inspects the
# staged diff must pass it so that an amend is judged by its full
# effect rather than by the newest index write alone.

_amend_base() {
  if echo "$COMMAND" | grep -qE '\b--amend\b'; then
    if git rev-parse HEAD~1 >/dev/null 2>&1; then
      echo "HEAD~1"
      return
    fi
  fi
  echo ""
}

# Resolve explicit-path commits against their proposed tree, not unrelated
# staged changes. The helper owns only disposable index/object files.
STAGED_SELECTION=0
STAGED_MANUAL_CONTENTS='{}'
if ! _selection=$(printf '%s' "$COMMAND" | COMMIT_FILE_RECORD="${COMMIT_RECORD:-}" COMMIT_FILE_CWD="${REPO_COMMAND_CONTEXT:-${REPO_ROOT:-$PWD}}" python3 "$(dirname "${BASH_SOURCE[0]}")/commit-file-selection.py") ||
   ! printf '%s' "$_selection" | jq -e 'type == "object" and (has("error") or (.mode == "index") or ((.mode == "selection" or .mode == "inspection") and (.staged | type == "string") and (.status | type == "string") and (.diffs | type == "object")))' >/dev/null; then
  jq -n '{hookSpecificOutput: {hookEventName: "PreToolUse", permissionDecision: "deny", permissionDecisionReason: "Cannot determine proposed commit files: selection helper failed or returned invalid data"}}'
  exit 0
fi
if ! printf '%s' "$_selection" | jq -e '(.manual_contents // {}) | type == "object" and all(.[]; . == null or type == "string")' >/dev/null; then
  jq -n '{hookSpecificOutput: {hookEventName: "PreToolUse", permissionDecision: "deny", permissionDecisionReason: "Cannot determine proposed commit files: invalid candidate manual contents"}}'
  exit 0
fi
if [ -n "$(printf '%s' "$_selection" | jq -r '.error // empty')" ]; then
  printf '%s' "$_selection" | jq '{hookSpecificOutput: {hookEventName: "PreToolUse", permissionDecision: "deny", permissionDecisionReason: .error}}'
  exit 0
fi
if [ "$(printf '%s' "$_selection" | jq -r '.mode')" != index ]; then
  STAGED_SELECTION=1
  STAGED=$(printf '%s' "$_selection" | jq -r '.staged')
  STAGED_STATUS=$(printf '%s' "$_selection" | jq -r '.status')
  STAGED_ELISP_DIFFS=$(printf '%s' "$_selection" | jq -c '.diffs')
  STAGED_MANUAL_CONTENTS=$(printf '%s' "$_selection" | jq -c '.manual_contents // {}')
  STAGED_BASE=""
  unset _selection
  return 0
fi
unset _selection
_BASE=$(_amend_base)
if [ -n "$_BASE" ]; then
  STAGED=$(git diff --cached --name-only "$_BASE" 2>/dev/null || true)
  STAGED_STATUS=$(git diff --cached --name-status -M "$_BASE" 2>/dev/null || true)
else
  STAGED=$(git diff --cached --name-only 2>/dev/null || true)
  STAGED_STATUS=$(git diff --cached --name-status -M 2>/dev/null || true)
fi
STAGED_BASE="$_BASE"
unset _BASE
