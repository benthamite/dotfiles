#!/usr/bin/env bash
# PreToolUse hook: block commits that change only one side of the
# Claude Code <-> Codex configuration port.
#
# Self-scoping: guard-commit only cares about `git commit` commands, so for
# every other Bash command (the overwhelming majority) we exit immediately
# instead of paying the binary's startup + git inspection cost.

set -euo pipefail

INPUT=$(cat)
COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')

# Only run the full guard when the command mentions a commit; the binary does
# the authoritative parse. Matching any `commit` token (not just `git commit`)
# guarantees we never skip a commit the binary would otherwise catch, while
# still fast-pathing the common commands that never contain the word.
printf '%s' "$COMMAND" | grep -qE '\bcommit\b' || exit 0

printf '%s' "$INPUT" | "$HOME/My Drive/dotfiles/bin/ai-config-sync" guard-commit
