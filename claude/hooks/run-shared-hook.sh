#!/bin/bash
# Claude Code adapter for shared hook scripts.
#
# Shared hook scripts in claude/hooks/ consume Claude-shaped JSON on stdin.
# This wrapper is intentionally thin so Claude hook config follows the same
# "adapter -> shared script" pattern as Codex hook config.

set -euo pipefail

if [ "$#" -lt 1 ]; then
  echo "Usage: run-shared-hook.sh HOOK [ARG...]" >&2
  exit 2
fi

HOOK="$1"
shift

INPUT=$(cat)
printf '%s' "$INPUT" | "$HOOK" "$@"
