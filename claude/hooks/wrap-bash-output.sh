#!/bin/bash
# PreToolUse hook: rewrite Bash commands that could plausibly print a secret to
# pipe combined stdout+stderr through `redact-secrets.sh` before the model sees
# them.
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

# Wrapping every command makes benign commands look like output-tunneling to
# Claude's permission classifier. Keep this broad enough to over-match possible
# secret sources while leaving ordinary git, gh, and local commands unchanged.
wrap_needs_redaction() {
  printf '%s' "$COMMAND" | grep -qE '(^|[[:space:];|&(])(pass|op|security|git-crypt)[[:space:]]' && return 0
  printf '%s' "$COMMAND" | grep -qE 'gh[[:space:]]+auth[[:space:]]+token|gcloud[[:space:]]+auth[[:space:]]+print|aws[[:space:]]+sts[[:space:]]|aws[[:space:]]+secretsmanager|kubectl[[:space:]]+get[[:space:]]+secret' && return 0
  printf '%s' "$COMMAND" | grep -qE '(^|[[:space:];|&(])(env|printenv)([[:space:];|&)]|$)' && return 0
  printf '%s' "$COMMAND" | grep -qE '(^|[[:space:];|&(])export[[:space:]]+-p([[:space:];|&)]|$)' && return 0
  printf '%s' "$COMMAND" | grep -qE '(^|[[:space:];|&(])(declare|typeset)([[:space:];|&)]|$)' && return 0
  printf '%s' "$COMMAND" | grep -qE '(^|[[:space:];|&(])set[[:space:]]*([;&|)]|$)' && return 0
  printf '%s' "$COMMAND" | grep -qE '\.zshenv-secrets|\.env\b|\.envrc\b|\.netrc\b|\.npmrc\b|\.pypirc\b|credentials|tokens\.json|keychain|\.pem\b|\.ssh/|\.gnupg/|_TOKEN|_SECRET|API_KEY|APIKEY|PASSWORD' && return 0
  return 1
}

wrap_needs_redaction || exit 0

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
    updatedInput: (input.tool_input + { command: $cmd })
  }
}' <<<"$INPUT"
