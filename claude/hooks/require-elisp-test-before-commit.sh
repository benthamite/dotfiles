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
# Extract the first staged .el filename to generate a ready-to-paste command
STAGED_EL=$(echo "$STAGED" | grep '\.el$' | head -1 || true)
PKG_NAME=""
if [ -n "$STAGED_EL" ]; then
  PKG_NAME=$(basename "$STAGED_EL" .el)
fi

ELPACA_PROFILE=$(emacsclient -e 'init-current-profile' 2>/dev/null | tr -d '"' || echo "8.1.0")
ELPACA_DIR="\$HOME/.config/emacs-profiles/${ELPACA_PROFILE}/elpaca"

if [ -n "$PKG_NAME" ]; then
  EXAMPLE_CMD="rm -f emacs/extras/${PKG_NAME}.elc && emacs --batch --eval \\\"(dolist (dir (file-expand-wildcards \\\\\\\"${ELPACA_DIR}/builds/*/\\\\\\\")) (add-to-list 'load-path dir))\\\" --eval \\\"(push \\\\\\\"$PWD/emacs/extras\\\\\\\" load-path)\\\" --eval \\\"(require '${PKG_NAME})\\\"\n\nCRITICAL: The push MUST come AFTER the dolist so emacs/extras is at the FRONT of load-path. Otherwise the stale .elc from elpaca/builds will be loaded instead of your edits."
else
  EXAMPLE_CMD="See the batch testing pattern in the dotfiles-context skill."
fi

REASON="BLOCKED: Elisp files are staged but you have not tested them in this session. You MUST run \`emacs --batch\` to verify the changed code before committing.\n\nCommand:\n${EXAMPLE_CMD}\n\nFor config.org changes: first tangle with \`emacsclient -e '(init-build-profile (file-name-directory user-init-file))'\`, then in the batch session also eval the changed use-package form before requiring the package, so after-load hooks are registered and exercised."
jq -n --arg reason "$REASON" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": $reason
  }
}'
