#!/bin/bash
# PreToolUse hook: block git commit when Elisp files are staged
# but no test has been run in this session.
#
# Reads JSON from stdin (Claude Code PreToolUse format).
# Outputs JSON with permissionDecision to allow or deny.

set -euo pipefail

INPUT=$(cat)

COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
SESSION_ID=$(printf '%s' "$INPUT" | jq -r '.session_id // empty')

# Only intercept git commit commands
if ! echo "$COMMAND" | grep -qE '\bgit\s+commit\b'; then
  exit 0
fi

# Check if any staged files are Elisp-related
STAGED=$(git diff --cached --name-only 2>/dev/null || true)

HAS_ELISP=false
if [ -n "$STAGED" ]; then
  while IFS= read -r file; do
    case "$file" in
      *.el|emacs/config.org)
        HAS_ELISP=true
        break
        ;;
    esac
  done <<< "$STAGED"
fi

# Also catch the pattern: git add ... && git commit in a single bash command.
# When staging and committing happen in one call, git diff --cached sees nothing
# yet at hook-fire time, so we must also scan the command string itself.
if [ "$HAS_ELISP" = false ]; then
  if echo "$COMMAND" | grep -qE '\bgit\s+add\b'; then
    if echo "$COMMAND" | grep -qE '\.el[[:space:]|&;]|\.el$' || \
       echo "$COMMAND" | grep -qF 'emacs/config.org'; then
      HAS_ELISP=true
    fi
  fi
fi

if [ "$HAS_ELISP" = false ]; then
  exit 0
fi

# Check for test marker
MARKER="/tmp/claude-elisp-tested-${SESSION_ID}"
if [ -f "$MARKER" ]; then
  # Tests were run; allow the commit and remove the marker
  rm -f "$MARKER"
  exit 0
fi

# Block the commit
jq -n '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": "BLOCKED: Elisp files are staged but you have not tested them in this session. You MUST verify the changed code before committing. For .el files: run emacs --batch to byte-compile, then reload via emacsclient. For config.org changes: (1) tangle the file, (2) use emacsclient to verify the changed use-package forms load without errors in the running Emacs (e.g. check key bindings, require the affected package, etc.). Simply evalling the source block is not sufficient—test the full load path."
  }
}'
