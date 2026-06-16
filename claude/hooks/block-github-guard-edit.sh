#!/bin/bash
# PreToolUse hook: protect the GitHub write allowlist and guard from agent edits.
#
# The GitHub write guard is meant to be a hard gate, not a policy an agent can
# casually amend before taking a forbidden external action. Changes to these
# files should be made manually by Pablo outside Claude.
#
# Matcher: Edit|Write|NotebookEdit

set -euo pipefail

INPUT=$(cat)
FILE_PATH=$(printf '%s' "$INPUT" | jq -r '.tool_input.file_path // .tool_input.path // .tool_input.notebook_path // empty')
[ -n "$FILE_PATH" ] || exit 0

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
DOTFILES_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)

case "$FILE_PATH" in
  "$DOTFILES_ROOT"/*)
    REL="${FILE_PATH#"$DOTFILES_ROOT"/}"
    ;;
  "$HOME/.codex/hooks.json")
    REL="codex/hooks.json"
    ;;
  "$HOME/.claude/settings.json")
    REL="claude-live-settings"
    ;;
  *)
    REL="$FILE_PATH"
    ;;
esac

case "$REL" in
  agents/github-write-allowlist.txt|\
  codex/hooks/block-github-write-command.sh|\
  codex/hooks/block-github-guard-edit.sh|\
  codex/hooks.json|\
  claude-live-settings|\
  claude/hooks/block-github-write-command.sh|\
  claude/hooks/block-github-guard-edit.sh|\
  claude/hooks/pretooluse-bash.sh)
    jq -n --arg path "$REL" '{
      "hookSpecificOutput": {
        "hookEventName": "PreToolUse",
        "permissionDecision": "deny",
        "permissionDecisionReason": ("BLOCKED: attempted edit to self-protected GitHub write-guard file `" + $path + "`.\n\nThe guard and allowlist must be changed manually outside Claude, so an agent cannot bypass the GitHub write gate by editing its own policy.")
      }
    }'
    exit 0
    ;;
esac

exit 0
