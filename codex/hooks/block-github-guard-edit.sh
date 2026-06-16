#!/bin/bash
# PreToolUse hook: protect the GitHub write allowlist and guard from agent edits.
#
# The GitHub write guard is meant to be a hard gate, not a policy an agent can
# casually amend before taking a forbidden external action. Changes to these
# files should be made manually by Pablo outside Codex.
#
# Matcher: apply_patch|Edit|Write

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-paths.sh
source "$SCRIPT_DIR/lib-codex-paths.sh"

INPUT=$(cat)
DOTFILES_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)

normalize_path() {
  local path="$1"
  case "$path" in
    "$DOTFILES_ROOT"/*)
      printf '%s' "${path#"$DOTFILES_ROOT"/}"
      ;;
    "$HOME/.codex/hooks.json")
      printf '%s' "codex/hooks.json"
      ;;
    "$HOME/.claude/settings.json")
      printf '%s' "claude-live-settings"
      ;;
    *)
      printf '%s' "$path"
      ;;
  esac
}

deny() {
  local path="$1"
  jq -n --arg path "$path" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": ("BLOCKED: attempted edit to self-protected GitHub write-guard file `" + $path + "`.\n\nThe guard and allowlist must be changed manually outside Codex, so an agent cannot bypass the GitHub write gate by editing its own policy.")
    }
  }'
  exit 0
}

while IFS= read -r file_path; do
  rel=$(normalize_path "$file_path")
  case "$rel" in
    agents/github-write-allowlist.txt|\
    codex/hooks/block-github-write-command.sh|\
    codex/hooks/block-github-guard-edit.sh|\
    codex/hooks.json|\
    claude-live-settings|\
    claude/hooks/block-github-write-command.sh|\
    claude/hooks/block-github-guard-edit.sh|\
    claude/hooks/pretooluse-bash.sh)
      deny "$rel"
      ;;
  esac
done < <(codex_changed_paths "$INPUT")

exit 0
