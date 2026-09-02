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
#   COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
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
