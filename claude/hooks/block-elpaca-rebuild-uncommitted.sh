#!/bin/bash
# PreToolUse hook: block elpaca-rebuild when there are uncommitted .el changes.
#
# The elpaca source clone syncs from the dotfiles repo via git commits.
# Running elpaca-rebuild before committing will compile and load stale
# code from the last commit, silently ignoring the working tree edits.
# This hook catches that mistake at the point of action.
#
# Reads JSON from stdin (Claude Code PreToolUse format).
# Outputs JSON with permissionDecision to allow or deny.

set -euo pipefail

INPUT=$(cat)

COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')

# Only intercept emacsclient commands that trigger elpaca-rebuild or reload
if ! echo "$COMMAND" | grep -qE '\bemacsclient\b'; then
  exit 0
fi
if ! echo "$COMMAND" | grep -qE 'elpaca-rebuild|elpaca-extras-reload'; then
  exit 0
fi

DOTFILES="$HOME/My Drive/dotfiles"

# Check for uncommitted .el changes (staged or unstaged) in extras
DIRTY=$(git -C "$DOTFILES" diff HEAD --name-only -- 'emacs/extras/*.el' 2>/dev/null || true)

if [ -z "$DIRTY" ]; then
  exit 0
fi

REASON="BLOCKED: You have uncommitted .el changes in the dotfiles repo. The elpaca source clone only has committed code, so elpaca-rebuild would load stale definitions.\n\nUncommitted files:\n${DIRTY}\n\nCommit first, then rebuild."

jq -n --arg reason "$REASON" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": $reason
  }
}'
