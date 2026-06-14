#!/usr/bin/env bash
# PostToolUse hook: after a Superpowers implementation plan is written or
# edited, remind the agent to run a plan-review subagent before presenting it
# as ready.

set -euo pipefail

INPUT=$(cat)
TOOL=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')

case "$TOOL" in
  Write|Edit) ;;
  *) exit 0 ;;
esac

FILE_PATH=$(printf '%s' "$INPUT" | jq -r '.tool_input.file_path // empty')
[ -n "$FILE_PATH" ] || exit 0

case "$FILE_PATH" in
  docs/superpowers/plans/*.md|*/docs/superpowers/plans/*.md|.claude/private-plans/*.md|*/.claude/private-plans/*.md|/tmp/claude-plans/*.md) ;;
  *) exit 0 ;;
esac

cat <<EOF
REMINDER: You just modified an implementation plan:
  $FILE_PATH

Before presenting this plan as ready, dispatch a plan-review subagent using the Superpowers writing-plans review prompt, or explicitly state why a subagent review is unavailable. Fix any blocking review findings before committing or handing off the plan.
EOF
