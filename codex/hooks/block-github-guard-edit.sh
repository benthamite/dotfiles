#!/bin/bash
# PreToolUse hook: protect the GitHub write allowlist and guard from agent edits.
#
# The GitHub write guard is meant to be a hard gate, not a policy an agent can
# casually amend before taking a forbidden external action. Changes to these
# files should be made manually by Pablo outside Codex.
#
# Matcher: apply_patch|Edit|Write|functions.exec

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-paths.sh
source "$SCRIPT_DIR/lib-codex-paths.sh"

INPUT=$(cat)
DOTFILES_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)
TOOL_NAME=$(codex_tool_name "$INPUT")

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

# functions.exec does not dispatch a second PreToolUse event for its nested
# apply_patch call. Refuse a standard nested patch that names any protected
# file, so the nested-command parser cannot rewrite itself before a GitHub
# write. Dynamic JavaScript remains outside what a static pre-execution hook can
# prove.
if [ "$TOOL_NAME" = "functions.exec" ]; then
  source_text=$(codex_shell_command "$INPUT")
  if printf '%s' "$source_text" | grep -qE 'tools\.apply_patch[[:space:]]*\(' && \
     printf '%s' "$source_text" | grep -qE '(agents/github-write-allowlist\.txt|codex/(hooks/block-github-write-command\.sh|hooks/block-github-guard-edit\.sh|hooks/lib-codex-hook-json\.sh|hooks/lib-codex-paths\.sh|hooks/lib-repo-root\.sh|hooks\.json)|claude/(hooks/block-github-write-command\.sh|hooks/block-github-guard-edit\.sh|hooks/pretooluse-bash\.sh)|\.codex/hooks\.json|\.claude/settings\.json)'; then
    deny "nested functions.exec apply_patch"
  fi
  exit 0
fi

while IFS= read -r file_path; do
  rel=$(normalize_path "$file_path")
  case "$rel" in
    agents/github-write-allowlist.txt|\
    codex/hooks/block-github-write-command.sh|\
    codex/hooks/block-github-guard-edit.sh|\
    codex/hooks/lib-codex-hook-json.sh|\
    codex/hooks/lib-codex-paths.sh|\
    codex/hooks/lib-repo-root.sh|\
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
