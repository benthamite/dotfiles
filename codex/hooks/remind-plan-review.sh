#!/usr/bin/env bash
# PostToolUse hook: after a Superpowers implementation plan is written or
# edited, remind the agent to run a plan-review subagent before presenting it
# as ready.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-paths.sh
source "$SCRIPT_DIR/lib-codex-paths.sh"

INPUT=$(cat)
TOOL=$(codex_tool_name "$INPUT")

case "$TOOL" in
  Write|Edit|apply_patch) ;;
  *) exit 0 ;;
esac

while IFS= read -r FILE_PATH; do
  [ -n "$FILE_PATH" ] || continue

  case "$FILE_PATH" in
    docs/superpowers/plans/*.md|*/docs/superpowers/plans/*.md|.codex/private-plans/*.md|*/.codex/private-plans/*.md|/tmp/codex-plans/*.md) ;;
    *) continue ;;
  esac

  MESSAGE=$(cat <<EOF
REMINDER: You just modified an implementation plan:
  $FILE_PATH

Before presenting this plan as ready, dispatch a plan-review subagent using the Superpowers writing-plans review prompt, or explicitly state why a subagent review is unavailable. Fix any blocking review findings before committing or handing off the plan.
EOF
)
  jq -n --arg message "$MESSAGE" '{"hookSpecificOutput":{"hookEventName":"PostToolUse","additionalContext":$message}}'
  exit 0
done < <(codex_changed_paths "$INPUT")
