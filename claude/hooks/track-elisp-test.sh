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
EXIT_CODE=$(printf '%s' "$INPUT" | jq -r '
  def response_object:
    .tool_response? as $response |
    if ($response | type) == "object" then $response
    elif ($response | type) == "string" then ($response | fromjson? // {"output": $response})
    else {}
    end;
  .tool_output.exitCode // .tool_output.exit_code //
  response_object.exitCode // response_object.exit_code // "0"
')
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

# Only the source-aware wrapper emits revision-bound evidence.
if [ "$EXIT_CODE" = 0 ] && echo "$COMMAND" | grep -qE 'batch-test\.sh|elisp-check-evidence'; then
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

  EVIDENCE=$(printf '%s\n' "$COMBINED" | grep '^ELISP_TEST_EVIDENCE_V1:' | tail -1 || true)
  if [ -z "$EVIDENCE" ]; then
    echo "WARNING: Elisp check returned no revision-bound evidence." >&2
    exit 0
  fi

  IFS=: read -r VERSION REPO_B64 PACKAGE_B64 REVISION <<< "$EVIDENCE"
  if [ "$VERSION" != ELISP_TEST_EVIDENCE_V1 ] ||
     ! [[ "$REVISION" =~ ^[0-9a-f]{64}$ ]]; then
    echo "WARNING: Elisp check returned malformed test evidence." >&2
    exit 0
  fi

  decode_base64() {
    local encoded="$1"
    if printf '%s' "$encoded" | base64 -D 2>/dev/null; then
      return 0
    fi
    printf '%s' "$encoded" | base64 -d 2>/dev/null
  }

  REPO=$(decode_base64 "$REPO_B64") || exit 0
  PACKAGE=$(decode_base64 "$PACKAGE_B64") || exit 0
  DOTFILES_ROOT=$(cd -- "$(dirname -- "$0")/../.." && pwd)
  REVISION_HELPER="$DOTFILES_ROOT/claude/bin/elisp-source-revision"
  WORKING_REVISION=$("$REVISION_HELPER" "$REPO" 2>/dev/null || true)
  INDEX_REVISION=$("$REVISION_HELPER" --index "$REPO" 2>/dev/null || true)
  if [ ! -d "$REPO" ] || [ -z "$PACKAGE" ] ||
     { [ "$WORKING_REVISION" != "$REVISION" ] && [ "$INDEX_REVISION" != "$REVISION" ]; }; then
    echo "WARNING: Elisp check evidence does not match the current source." >&2
    exit 0
  fi

  MARKER="/tmp/claude-elisp-tested-${SESSION_ID}"
  LOCK_DIR="${MARKER}.lock"
  acquired=false
  for _attempt in $(seq 1 200); do
    if mkdir "$LOCK_DIR" 2>/dev/null; then
      printf '%s\n' "$$" > "$LOCK_DIR/owner"
      acquired=true
      break
    fi
    if [ -s "$LOCK_DIR/owner" ]; then
      lock_owner=$(sed -n '1p' "$LOCK_DIR/owner")
      if ! kill -0 "$lock_owner" 2>/dev/null; then
        rm -f "$LOCK_DIR/owner"
        rmdir "$LOCK_DIR" 2>/dev/null || true
      fi
    fi
    sleep 0.01
  done
  if [ "$acquired" != true ]; then
    echo "WARNING: Could not lock the Elisp test evidence marker." >&2
    exit 0
  fi
  release_lock() {
    rm -f "$LOCK_DIR/owner"
    rmdir "$LOCK_DIR" 2>/dev/null || true
  }
  trap release_lock EXIT

  TEMPORARY=$(mktemp "${TMPDIR:-/tmp}/elisp-tested.XXXXXX")
  if [ -f "$MARKER" ]; then
    awk -F: -v repo="$REPO_B64" -v package="$PACKAGE_B64" \
      '$1 != repo || $2 != package' "$MARKER" > "$TEMPORARY"
  fi
  printf '%s:%s:%s\n' "$REPO_B64" "$PACKAGE_B64" "$REVISION" >> "$TEMPORARY"
  mv -f "$TEMPORARY" "$MARKER"
fi

exit 0
