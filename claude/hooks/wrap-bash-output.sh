#!/bin/bash
# PreToolUse hook: rewrite Bash commands to pipe combined stdout+stderr
# through `redact-secrets.sh` before the model sees them.
#
# Returns `hookSpecificOutput.updatedInput.command` with the wrapped form;
# exit code propagates the original command's status via PIPESTATUS.
#
# Skips commands that already include the redactor (no double-wrapping),
# commands containing heredocs (`<<`) where wrapping in `{ ; }` breaks the
# delimiter, and commands ending in `&` (background launches that the pipe
# would deadlock or strip output from).
#
# Matchers in settings.json: Bash

set -euo pipefail

INPUT=$(cat)
TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')
[ "$TOOL_NAME" = "Bash" ] || exit 0

COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
[ -z "$COMMAND" ] && exit 0

REDACTOR="$HOME/My Drive/dotfiles/claude/hooks/redact-secrets.sh"

# Skip if already wrapped (idempotent).
case "$COMMAND" in
  *"redact-secrets.sh"*) exit 0 ;;
esac

# Skip heredoc commands — wrapping in `{ ; }` after a heredoc body trips
# the terminator. The user's existing block-sensitive-read hook still
# guards the dangerous reads inside these.
if printf '%s' "$COMMAND" | grep -qE '<<-?[[:space:]]*[A-Za-z_'"'"'"]'; then
  exit 0
fi

# Skip background commands — the pipe would either deadlock or drop the
# launcher's output silently.
if printf '%s' "$COMMAND" | grep -qE '&[[:space:]]*$'; then
  exit 0
fi

# Wrap. `{ ...; }` is a no-fork group so `cd`/exports/etc. still affect
# the parent shell as expected. `2>&1` brings stderr through the filter.
# `PIPESTATUS[0]` preserves the original command's exit code so callers
# (and the model) still see real success/failure.
WRAPPED="{ ${COMMAND}; } 2>&1 | \"${REDACTOR}\"; exit \"\${PIPESTATUS[0]}\""

jq -nc --arg cmd "$WRAPPED" '{
  hookSpecificOutput: {
    hookEventName: "PreToolUse",
    updatedInput: { command: $cmd }
  }
}'
