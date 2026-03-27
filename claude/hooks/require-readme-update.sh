#!/bin/bash
# PreToolUse hook: block git commit when files in claude/ (skills, hooks,
# settings) are staged but README.org is not.
#
# Reads JSON from stdin (Claude Code PreToolUse format).
# Outputs JSON with permissionDecision to allow or deny.

set -euo pipefail

INPUT=$(cat)

COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')

# Only intercept git commit commands
if ! echo "$COMMAND" | grep -qE '\bgit\s+commit\b'; then
  exit 0
fi

# Check staged files
STAGED=$(git diff --cached --name-only 2>/dev/null || true)

HAS_CLAUDE_CHANGES=false
HAS_README=false
if [ -n "$STAGED" ]; then
  while IFS= read -r file; do
    case "$file" in
      claude/skills/*|claude/hooks/*|claude/settings*.json|claude/CLAUDE.md)
        HAS_CLAUDE_CHANGES=true
        ;;
      claude/README.org)
        HAS_README=true
        ;;
    esac
  done <<< "$STAGED"
fi

# Also catch git add ... && git commit in a single bash command.
# At hook-fire time git diff --cached doesn't see the new files yet.
if [ "$HAS_CLAUDE_CHANGES" = false ]; then
  if echo "$COMMAND" | grep -qE '\bgit\s+add\b'; then
    if echo "$COMMAND" | grep -qE '(^|[[:space:]])claude/(skills|hooks|settings|CLAUDE)' && ! echo "$COMMAND" | grep -qE '\.claude/'; then
      HAS_CLAUDE_CHANGES=true
    fi
  fi
fi
if [ "$HAS_README" = false ]; then
  if echo "$COMMAND" | grep -qF 'README.org'; then
    HAS_README=true
  fi
fi

if [ "$HAS_CLAUDE_CHANGES" = false ] || [ "$HAS_README" = true ]; then
  exit 0
fi

# Block the commit
jq -n '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": "BLOCKED: You are committing changes to claude/ (skills, hooks, or settings) without updating claude/README.org. Update the README to reflect your changes, then try again."
  }
}'
