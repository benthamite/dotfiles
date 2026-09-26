#!/bin/bash
# PreToolUse hook: deny Git commands that override core.hooksPath.
#
# Elisp test and live-verification evidence is enforced by the global Git
# hooks in claude/git-hooks and by the Stop hook, not by reading commands.
# Overriding core.hooksPath would silently disable both, so only that
# command shape is denied.  Mirrors check_hookspath_override in
# claude/hooks/pretooluse-bash.sh.

set -euo pipefail

hook_bootstrap_complete=0
trap 'hook_status=$?; if [ "$hook_status" -ne 0 ] || [ "$hook_bootstrap_complete" -ne 1 ]; then
  printf "%s\n" "Security hook failed; tool execution denied." >&2
  exit 2
fi' EXIT

# shellcheck source=lib-codex-paths.sh
source "$(dirname "$0")/lib-codex-paths.sh"

# A read (`git config --get core.hooksPath`, or the bare key with no value)
# changes nothing; every other `git config` form naming the key may write it.
hookspath_read_only_p() {
  local segments segment
  printf '%s' "$1" | grep -qiE -- '-c[[:space:]]*core\.hookspath' && return 1
  segments=$(printf '%s' "$1" | grep -oiE '(^|[^[:alnum:]_-])git[[:space:]][^|;&]*config[[:space:]][^|;&]*core\.hookspath[^|;&]*') || return 1
  while IFS= read -r segment; do
    printf '%s' "$segment" | grep -qiE -- '--(unset|unset-all|add|replace-all|rename-section|remove-section|edit)([[:space:]=]|$)|(^|[[:space:]])-e([[:space:]]|$)' && return 1
    printf '%s' "$segment" | grep -qiE -- '--get(-all|-regexp|-urlmatch)?([[:space:]=]|$)' && continue
    printf '%s' "$segment" | grep -qiE 'core\.hookspath[[:space:]]*$' && continue
    return 1
  done <<< "$segments"
  return 0
}

hook_bootstrap_complete=1
INPUT=$(cat)
CMD=$(codex_shell_command "$INPUT")
[ -n "$CMD" ] || exit 0
if printf '%s' "$CMD" | grep -qiE -- '-c[[:space:]]*core\.hookspath|(^|[^[:alnum:]_-])git[[:space:]][^|;&]*config[[:space:]][^|;&]*core\.hookspath' \
    && ! hookspath_read_only_p "$CMD"; then
  jq -n '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": "BLOCKED: Git commands may not override core.hooksPath. The global hooks in ~/My Drive/dotfiles/claude/git-hooks enforce Elisp test evidence and run each repository'"'"'s own hooks."
    }
  }'
fi
exit 0
