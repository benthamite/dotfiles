#!/bin/bash
# PreToolUse hook: block Bash commands (except emacsclient) when
# Elisp changes have been committed but not yet verified in the
# running Emacs session.
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

# No marker means no pending verification
if [ ! -f "$MARKER" ]; then
  exit 0
fi

# Permit only exact helper executables. Textual mentions do not bypass the gate.
if command_only_elisp_helpers "$COMMAND"; then
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
    if ! command_only_elisp_helpers "$nested_command"; then nested_valid=false; fi
  done < <(printf '%s' "$COMMAND" | codex_nested_exec_contexts)
  other_tools=$(printf '%s' "$COMMAND" | grep -oE 'tools\.[A-Za-z_]+' | grep -vxF 'tools.exec_command' || true)
  if [ "$nested_found" = true ] && [ "$nested_valid" = true ] && [ -z "$other_tools" ]; then
    exit 0
  fi
fi

# Block everything else
REASON="BLOCKED: Committed Elisp still needs package- and commit-bound live evidence. Run \`~/My\\ Drive/dotfiles/claude/bin/elisp-live-verify LABEL -- ELISP-EXPRESSION\`. For a package, the expression must name and exercise that package. The helper waits for any required rebuild before it runs the live check."
jq -n --arg reason "$REASON" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": $reason
  }
}'
