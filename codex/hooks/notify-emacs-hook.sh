#!/usr/bin/env bash
# Forward Codex hook events to the Emacs codex package.

set -euo pipefail

hook_type="${1:-}"
shift || true

[ -n "$hook_type" ] || exit 0

json_input=$(cat)

response=$(emacsclient --eval '(codex-handle-hook-from-emacsclient)' \
  "$hook_type" "${CODEX_BUFFER_NAME:-}" "$json_input" "$@" 2>/dev/null || true)

if [[ "$response" =~ ^\".*\"$ ]]; then
  printf '%s\n' "$response" | sed 's/^"//;s/"$//' | sed 's/\\"/"/g'
elif [[ "$response" == "nil" || -z "$response" ]]; then
  :
else
  printf '%s\n' "$response"
fi
