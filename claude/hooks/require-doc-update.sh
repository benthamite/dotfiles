#!/bin/bash
# PreToolUse hook: block git commit when .el files are staged but no .org
# file under doc/ is also staged — only in repos that have a doc/ directory.
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

# Find the repo root; bail if not in a git repo
REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || true)
if [ -z "$REPO_ROOT" ]; then
  exit 0
fi

# Only apply in repos that have a doc/ directory at the root
if [ ! -d "$REPO_ROOT/doc" ]; then
  exit 0
fi

# Check staged files
STAGED=$(git diff --cached --name-only 2>/dev/null || true)
if [ -z "$STAGED" ]; then
  exit 0
fi

HAS_EL=false
HAS_DOC_ORG=false
while IFS= read -r file; do
  case "$file" in
    *.el)
      HAS_EL=true
      ;;
    doc/*.org)
      HAS_DOC_ORG=true
      ;;
  esac
done <<< "$STAGED"

if [ "$HAS_EL" = false ] || [ "$HAS_DOC_ORG" = true ]; then
  exit 0
fi

# Block the commit
jq -n '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": "BLOCKED: Elisp files are staged but no doc/*.org file is included. Update the org manual in doc/ to reflect your changes, then try again. Use /doc-elisp to generate or update documentation."
  }
}'
