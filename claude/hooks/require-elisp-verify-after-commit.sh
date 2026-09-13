#!/bin/bash
# PreToolUse hook: require live checks while permitting read-only diagnosis.
#
# Works in tandem with track-elisp-verify.sh which sets and clears
# the verification marker.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=../../codex/hooks/lib-codex-hook-json.sh
source "$SCRIPT_DIR/../../codex/hooks/lib-codex-hook-json.sh"

INPUT=$(cat)

COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
SESSION_ID=$(printf '%s' "$INPUT" | jq -r '.session_id // empty')

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
if command_only_elisp_helpers "$COMMAND" || command_inspects_state "$COMMAND"; then
  exit 0
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
