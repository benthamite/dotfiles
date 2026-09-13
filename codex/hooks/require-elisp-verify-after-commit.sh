#!/bin/bash
# PreToolUse hook: require live checks while permitting read-only diagnosis.
#
# Works in tandem with track-elisp-verify.sh which sets and clears
# the verification marker.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-hook-json.sh
source "$SCRIPT_DIR/lib-codex-hook-json.sh"

INPUT=$(cat)

COMMAND=$(codex_shell_command "$INPUT")
SESSION_ID=$(codex_session_id "$INPUT")
TOOL_NAME=$(codex_tool_name "$INPUT")

MARKER="/tmp/claude-elisp-verify-needed-${SESSION_ID}"

command_only_elisp_helpers() {
  local command="$1" executable found=false
  while IFS= read -r -d '' executable; do
    found=true
    case "$executable" in
      elpaca-rebuild-wait|elisp-live-verify) ;;
      *) return 1 ;;
    esac
  done < <(printf '%s' "$command" | codex_shell_executables)
  [ "$found" = true ]
}

command_inspects_state() {
  printf '%s' "$1" | python3 "$SCRIPT_DIR/lib-elisp-diagnostics.py"
}

# No marker means no pending verification
if [ ! -f "$MARKER" ]; then
  exit 0
fi

# Permit only exact helper executables. Textual mentions do not bypass the gate.
if [ "$TOOL_NAME" != functions.exec ] &&
   { command_only_elisp_helpers "$COMMAND" || command_inspects_state "$COMMAND"; }; then
  exit 0
fi

if [ "$TOOL_NAME" = functions.exec ]; then
  nested_found=false
  nested_valid=true
  while IFS= read -r -d '' context; do
    nested_found=true
    if [ "$(printf '%s' "$context" | jq -r '.ambiguous')" = true ]; then
      nested_valid=false
      continue
    fi
    nested_command=$(printf '%s' "$context" | jq -r '.cmd // empty')
    if ! command_only_elisp_helpers "$nested_command" &&
       ! command_inspects_state "$nested_command"; then nested_valid=false; fi
  done < <(printf '%s' "$COMMAND" | codex_nested_exec_contexts)
  if [ "$nested_found" = true ] && [ "$nested_valid" = true ] &&
     printf '%s' "$COMMAND" | python3 "$SCRIPT_DIR/lib-elisp-diagnostics.py" --nested; then
    exit 0
  fi
fi

# Inspection does not clear the marker or certify the pending changes.
PENDING=$(python3 "$SCRIPT_DIR/lib-elisp-diagnostics.py" --pending "$MARKER")
REASON="BLOCKED: Committed Elisp still needs package- and commit-bound live evidence.
$PENDING
Run \`~/My\\ Drive/dotfiles/claude/bin/elisp-live-verify LABEL -- ELISP-EXPRESSION\` for the listed package. The expression must exercise the changed behavior. Literal read-only diagnosis remains available, including cat, sed -n, and rg --no-config. Git diagnostics require --no-pager; status, ls-files, and diff also require --no-optional-locks -c core.fsmonitor=false. Other commands remain blocked until the live check succeeds."
jq -n --arg reason "$REASON" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": $reason
  }
}'
