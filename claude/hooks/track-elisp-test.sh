#!/bin/bash
# PostToolUse hook: create a session marker when an `emacs --batch`
# command completes, so the commit hook knows testing was done.
#
# Also verifies that the test loaded files from the working tree
# (emacs/extras/), not from stale elpaca builds. If the command
# output contains a load warning about elpaca/builds, the marker
# is NOT created and a warning is printed.
#
# Reads JSON from stdin (Claude Code PostToolUse format).

set -euo pipefail

INPUT=$(cat)

COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
SESSION_ID=$(printf '%s' "$INPUT" | jq -r '.session_id // empty')
STDOUT=$(printf '%s' "$INPUT" | jq -r '.tool_output.stdout // empty')
STDERR=$(printf '%s' "$INPUT" | jq -r '.tool_output.stderr // empty')
COMBINED="$STDOUT$STDERR"

# Check if the command used emacs --batch (the standard Elisp testing method)
if echo "$COMMAND" | grep -qE 'emacs\s+--batch'; then
  # Safety check: did the test load a file from elpaca/builds instead
  # of the working tree? This catches the silent-stale-load bug where
  # the .elc from elpaca shadows the edited .el source.
  if echo "$COMBINED" | grep -qE 'elpaca/builds/.*\.elc|using older file'; then
    echo "WARNING: emacs --batch loaded a stale .elc from elpaca/builds." >&2
    echo "The test did NOT verify your edits. Fix the load-path order:" >&2
    echo '  (push "/path/to/emacs/extras" load-path)  ; MUST come last' >&2
    # Do NOT create the marker — the commit hook should still block.
    exit 0
  fi

  MARKER="/tmp/claude-elisp-tested-${SESSION_ID}"
  touch "$MARKER"
fi

exit 0
