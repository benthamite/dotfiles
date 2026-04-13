#!/bin/bash
# Shared helper: resolve the git repo root from the command context.
#
# PreToolUse hooks run before the bash command executes, so the hook's
# cwd may differ from the command's effective cwd.  If the command
# starts with `cd <path> &&`, this helper resolves the repo root from
# that path instead of the hook's cwd.
#
# Usage: source this file after setting $COMMAND, then use $REPO_ROOT.
# The script also cds into $REPO_ROOT so subsequent git commands
# (including lib-staged-files.sh) operate on the correct repo.
#
#   COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
#   source "$(dirname "$0")/lib-repo-root.sh"
#   # $REPO_ROOT is set and cwd is $REPO_ROOT

_cd_prefix=$(printf '%s' "$COMMAND" | grep -oE '^\s*cd\s+[^&]+' | head -1)
if [ -n "$_cd_prefix" ]; then
  REPO_ROOT=$(bash -c "$_cd_prefix && git rev-parse --show-toplevel" 2>/dev/null || true)
else
  REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || true)
fi
unset _cd_prefix

if [ -n "$REPO_ROOT" ]; then
  cd "$REPO_ROOT"
fi
