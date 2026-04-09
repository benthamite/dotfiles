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
else
  STAGED=$(git diff --cached --name-only 2>/dev/null || true)
fi
unset _BASE
