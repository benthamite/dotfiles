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

# Extract the cd target from "cd <path> ..." patterns without re-evaluating
# the command in a subshell.  Reject inputs that contain command substitution
# so an injected `$(...)` or backtick can't reach `git -C`.
_cd_target=""
case "$COMMAND" in
  *'$('* | *'`'* ) ;;  # bail: command substitution present
  *)
    if [[ "$COMMAND" =~ ^[[:space:]]*cd[[:space:]]+([^\&\;\|]+) ]]; then
      _cd_target="${BASH_REMATCH[1]}"
      _cd_target="${_cd_target%"${_cd_target##*[![:space:]]}"}"  # rtrim
      _cd_target="${_cd_target/#~/$HOME}"                          # tilde
      _cd_target="${_cd_target//\\ / }"                            # \  → ' '
    fi
    ;;
esac

if [ -n "$_cd_target" ] && [ -d "$_cd_target" ]; then
  REPO_ROOT=$(git -C "$_cd_target" rev-parse --show-toplevel 2>/dev/null || true)
else
  REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || true)
fi
unset _cd_target

if [ -n "$REPO_ROOT" ]; then
  cd "$REPO_ROOT"
fi
