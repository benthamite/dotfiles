#!/bin/bash
# PreToolUse hook: block direct tool access to the walk-list protected data store.
#
# walk-list moves user-supplied input files into ~/.claude/walk-list-data/<uuid>/
# so that only the walk.py CLI can see their contents. This hook enforces that
# no other tool (Read, Grep, Glob, Bash, Edit, Write, NotebookEdit, apply_patch) can bypass
# the CLI to peek at the locked data.
#
# Matchers: Read, Grep, Glob, Edit, Write, NotebookEdit, Bash, apply_patch
#
# Bash commands are allowed only if they invoke walk.py explicitly.

set -euo pipefail

# shellcheck source=lib-codex-paths.sh
source "$(dirname "$0")/lib-codex-paths.sh"

INPUT=$(cat)

TOOL_NAME=$(codex_tool_name "$INPUT")
[ -z "$TOOL_NAME" ] && exit 0

GUARDED_PATTERN='/\.claude/walk-list-data'
EXPANDED_HOME="$HOME/.claude/walk-list-data"

deny() {
  local reason="$1"
  jq -n --arg r "$reason" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": ("BLOCKED: " + $r + "\n\nThe walk-list protected store at ~/.claude/walk-list-data/ is unreadable by any tool other than `python ~/.claude/skills/walk-list/walk.py`. Use `walk.py next <input-file> <decision>` to advance one item; the script will print exactly the one item you are permitted to see. Do NOT attempt to circumvent.")
    }
  }'
  exit 0
}

path_is_guarded() {
  local p="$1"
  [ -z "$p" ] && return 1
  # Expand ~
  p="${p/#\~/$HOME}"
  case "$p" in
    "$EXPANDED_HOME"|"$EXPANDED_HOME"/*) return 0 ;;
  esac
  case "$p" in
    *"$GUARDED_PATTERN"*) return 0 ;;
  esac
  return 1
}

case "$TOOL_NAME" in
  Read|Edit|Write|NotebookEdit)
    FIELD=$(codex_hook_jq "$INPUT" 'codex_tool_input.file_path // codex_tool_input.notebook_path // codex_tool_input.path // empty')
    if path_is_guarded "$FIELD"; then
      deny "$TOOL_NAME of walk-list protected path: $FIELD"
    fi
    ;;
  Grep|Glob)
    FIELD=$(codex_tool_input_field "$INPUT" path)
    if path_is_guarded "$FIELD"; then
      deny "$TOOL_NAME of walk-list protected path: $FIELD"
    fi
    PATTERN=$(codex_tool_input_field "$INPUT" pattern)
    if path_is_guarded "$PATTERN"; then
      deny "$TOOL_NAME pattern references walk-list protected path: $PATTERN"
    fi
    ;;
  Bash)
    CMD=$(codex_tool_input_field "$INPUT" command)
    [ -z "$CMD" ] && exit 0
    # If the command doesn't reference the guarded dir at all, allow.
    if ! echo "$CMD" | grep -qE "(\.claude/walk-list-data|~/\.claude/walk-list-data|$EXPANDED_HOME)"; then
      exit 0
    fi
    # Command references the guarded dir. Allow only if it's a walk.py invocation.
    # walk.py legitimately holds the data; it never dumps more than one item.
    if echo "$CMD" | grep -qE '(^|[;&|[:space:]])python[0-9.]*[[:space:]]+(-[a-zA-Z]+[[:space:]]+)*([^[:space:]]+/)?walk\.py([[:space:]]|$)'; then
      # Allowed: walk.py invocation
      exit 0
    fi
    deny "Bash command references ~/.claude/walk-list-data/ outside of walk.py: $CMD"
    ;;
  apply_patch)
    while IFS= read -r FIELD; do
      if path_is_guarded "$FIELD"; then
        deny "apply_patch of walk-list protected path: $FIELD"
      fi
    done < <(codex_changed_paths "$INPUT")
    PATCH=$(codex_patch_text "$INPUT")
    if echo "$PATCH" | grep -qE "(\.claude/walk-list-data|~/\.claude/walk-list-data|$EXPANDED_HOME)"; then
      deny "apply_patch references ~/.claude/walk-list-data/"
    fi
    ;;
esac

exit 0
