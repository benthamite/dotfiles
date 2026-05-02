#!/bin/bash
# PostToolUse hook: create a session marker when an `emacs --batch`
# command completes, so the commit hook knows testing was done.
#
# Also verifies that the test loaded files from the working tree
# (emacs/extras/), not from stale elpaca builds. If the command
# output contains a load warning about elpaca/builds, the marker
# is NOT created and a warning is printed.
#
# Reads JSON from stdin. Supports both Claude Code's `tool_output` payload and
# Codex's `tool_response` payload.

set -euo pipefail

INPUT=$(cat)

COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
SESSION_ID=$(printf '%s' "$INPUT" | jq -r '.session_id // empty')
STDOUT=$(printf '%s' "$INPUT" | jq -r '
  def response_object:
    .tool_response? as $response |
    if ($response | type) == "object" then $response
    elif ($response | type) == "string" then ($response | fromjson? // {"output": $response})
    else {}
    end;
  .tool_output.stdout // response_object.stdout // response_object.output // response_object.text // empty
')
STDERR=$(printf '%s' "$INPUT" | jq -r '
  def response_object:
    .tool_response? as $response |
    if ($response | type) == "object" then $response
    elif ($response | type) == "string" then ($response | fromjson? // {"output": $response})
    else {}
    end;
  .tool_output.stderr // response_object.stderr // empty
')
COMBINED="$STDOUT$STDERR"

# Check if the command used emacs --batch or the batch-test.sh wrapper
if echo "$COMMAND" | grep -qE 'emacs\s+--batch|batch-test\.sh'; then
  # Safety check: did Emacs print a real stale-load warning? These are
  # the canonical messages emitted when a .elc shadows a newer .el. We
  # match them literally rather than any mention of elpaca/builds, which
  # produces false positives when the test prints function source paths
  # or when a standalone package legitimately loads from elpaca/builds.
  if echo "$COMBINED" | grep -qE 'newer than byte-compiled file|using older file'; then
    echo "WARNING: emacs --batch printed a stale-load warning." >&2
    echo "The test may not have verified your edits. Fix load-path order:" >&2
    echo '  (push "/path/to/canonical/source" load-path)  ; before elpaca builds' >&2
    # Do NOT create the marker — the commit hook should still block.
    exit 0
  fi

  MARKER="/tmp/claude-elisp-tested-${SESSION_ID}"
  touch "$MARKER"

  # Clean up stale markers from old sessions (>1 hour)
  find /tmp -maxdepth 1 -name 'claude-elisp-tested-*' -mmin +60 -delete 2>/dev/null || true
fi

exit 0
