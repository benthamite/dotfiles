#!/usr/bin/env bash
# PreToolUse hook: block git commit when Codex integration files are staged
# but codex/README.org is not.

set -euo pipefail

INPUT=$(cat)
COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')

if ! echo "$COMMAND" | grep -qE '\bgit\s+commit\b'; then
  exit 0
fi

# shellcheck source=lib-staged-files.sh
source "$(dirname "$0")/lib-staged-files.sh"

HAS_CODEX_CHANGES=false
HAS_README=false

if [ -n "$STAGED" ]; then
  while IFS= read -r file; do
    case "$file" in
      codex/README.org)
        HAS_README=true
        ;;
      AGENTS.md|.codex/*|codex/*|ai-config-sync.json|bin/ai-config-sync)
        HAS_CODEX_CHANGES=true
        ;;
    esac
  done <<< "$STAGED"
fi

if [ "$HAS_CODEX_CHANGES" = false ] && echo "$COMMAND" | grep -qE '\bgit\s+add\b'; then
  if echo "$COMMAND" | grep -qE '(^|[[:space:]])(AGENTS\.md|\.codex/|codex/|ai-config-sync\.json|bin/ai-config-sync)'; then
    HAS_CODEX_CHANGES=true
  fi
fi

if [ "$HAS_README" = false ] && echo "$COMMAND" | grep -qF 'codex/README.org'; then
  HAS_README=true
fi

if [ "$HAS_CODEX_CHANGES" = false ] || [ "$HAS_README" = true ]; then
  exit 0
fi

jq -n '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": "BLOCKED: You are committing Codex integration changes without updating codex/README.org. Update the README to reflect the change, then try again."
  }
}'
