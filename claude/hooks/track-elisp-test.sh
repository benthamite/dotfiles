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

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# Reuse the quote-aware wrapper classifier used by the paired Codex hook.
# shellcheck source=../../codex/hooks/lib-codex-hook-json.sh
source "$SCRIPT_DIR/../../codex/hooks/lib-codex-hook-json.sh"
# shellcheck source=lib-elisp-evidence.sh
source "$SCRIPT_DIR/lib-elisp-evidence.sh"

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

EXPECTED_LABEL=""
EXPECTED_COUNT=0
EXECUTABLE_COUNT=0
while IFS= read -r -d '' candidate; do
  EXPECTED_COUNT=$((EXPECTED_COUNT + 1))
  EXPECTED_LABEL="$candidate"
done < <(printf '%s' "$COMMAND" | codex_elisp_evidence_labels test)
while IFS= read -r -d '' executable; do
  EXECUTABLE_COUNT=$((EXECUTABLE_COUNT + 1))
done < <(printf '%s' "$COMMAND" | codex_shell_executables)

# A detected wrapper run whose evidence cannot be recorded must say so
# visibly: the commit gate would otherwise block later with a message
# that no longer names the cause. additionalContext reaches the model;
# a bare stderr warning from a PostToolUse hook does not.
report_unrecorded() {
  jq -n --arg message "Elisp test evidence NOT recorded: $1 The commit gate will block until a wrapper run records evidence." \
    '{"hookSpecificOutput":{"hookEventName":"PostToolUse","additionalContext":$message}}'
  exit 0
}

# Only the source-aware wrapper emits revision-bound evidence.
if [ "$EXIT_CODE" = 0 ] && [ "$EXPECTED_COUNT" -gt 0 ]; then
  if [ "$EXPECTED_COUNT" -gt 1 ]; then
    report_unrecorded "the command contains $EXPECTED_COUNT wrapper invocations. Run each check as its own command."
  fi
  if [ "$EXECUTABLE_COUNT" -ne 1 ]; then
    report_unrecorded "the wrapper must be the only executable in the command. Re-run it bare, without pipes, chains, or other commands."
  fi
  # Safety check: did Emacs print a real stale-load warning? These are
  # the canonical messages emitted when a .elc shadows a newer .el. We
  # match them literally rather than any mention of elpaca/builds, which
  # produces false positives when the test prints function source paths
  # or when a standalone package legitimately loads from elpaca/builds.
  if echo "$COMBINED" | grep -qE 'newer than byte-compiled file|using older file'; then
    # Do NOT create the marker — the commit hook should still block.
    report_unrecorded "emacs --batch printed a stale-load warning, so the test may not have verified your edits. Fix load-path order: (push \"/path/to/canonical/source\" load-path) before elpaca builds."
  fi

  EVIDENCE=$(printf '%s\n' "$COMBINED" | grep '^ELISP_TEST_EVIDENCE_V2:' | tail -1 || true)
  if [ -z "$EVIDENCE" ]; then
    report_unrecorded "the check printed no revision-bound evidence line."
  fi
  VERIFIED_EVIDENCE=$(elisp_evidence_consume test "$EVIDENCE" || true)
  if [ -z "$VERIFIED_EVIDENCE" ]; then
    report_unrecorded "the evidence has no valid one-time receipt."
  fi

  IFS=: read -r VERSION REPO_B64 PACKAGE_B64 REVISION <<< "$VERIFIED_EVIDENCE"
  if [ "$VERSION" != ELISP_TEST_EVIDENCE_V2 ] ||
     ! [[ "$REVISION" =~ ^[0-9a-f]{64}$ ]]; then
    report_unrecorded "the evidence line is malformed."
  fi

  decode_base64() {
    local encoded="$1"
    if printf '%s' "$encoded" | base64 -D 2>/dev/null; then
      return 0
    fi
    printf '%s' "$encoded" | base64 -d 2>/dev/null
  }

  REPO=$(decode_base64 "$REPO_B64") ||
    report_unrecorded "the evidence repository field cannot be decoded."
  PACKAGE=$(decode_base64 "$PACKAGE_B64") ||
    report_unrecorded "the evidence package field cannot be decoded."
  if [ "$PACKAGE" != "$EXPECTED_LABEL" ]; then
    report_unrecorded "the evidence label does not match the wrapper command."
  fi
  DOTFILES_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)
  REVISION_HELPER="$DOTFILES_ROOT/claude/bin/elisp-source-revision"
  WORKING_REVISION=$("$REVISION_HELPER" "$REPO" 2>/dev/null || true)
  INDEX_REVISION=$("$REVISION_HELPER" --index "$REPO" 2>/dev/null || true)
  if [ ! -d "$REPO" ] || [ -z "$PACKAGE" ] ||
     { [ "$WORKING_REVISION" != "$REVISION" ] && [ "$INDEX_REVISION" != "$REVISION" ]; }; then
    report_unrecorded "the evidence does not match the current source revision. Re-run the check after the final source edit."
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
    report_unrecorded "the evidence marker could not be locked. Re-run the check."
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
