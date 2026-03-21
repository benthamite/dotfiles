#!/usr/bin/env bash
# PostToolUse hook: load edited Elisp files into the running Emacs session.
#
# Fires on Edit|Write. If the file is .el, evaluates it via emacsclient
# so the user can test changes immediately without manual reload.

set -euo pipefail

input=$(cat)

file_path=$(printf '%s' "$input" | jq -r '.tool_input.file_path // empty')

# Only act on .el files
[[ "$file_path" == *.el ]] || exit 0

# Skip byte-compiled files
[[ "$file_path" != *.elc ]] || exit 0

# Skip test files — they should be run, not loaded
[[ "$file_path" != *-test.el ]] && [[ "$file_path" != *-tests.el ]] || exit 0

# Try to load in running Emacs; fail silently if Emacs isn't available
if emacsclient --eval "(load-file \"$file_path\")" >/dev/null 2>&1; then
  jq -n --arg f "$file_path" '{
    "hookSpecificOutput": {
      "message": ("Loaded " + $f + " in running Emacs.")
    }
  }'
fi
