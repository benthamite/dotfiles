#!/usr/bin/env bash
# PostToolUse hook: load edited Elisp files into the running Emacs session.
#
# Fires on Edit|Write. If the file is .el, deletes any stale .elc in elpaca
# builds/ (so Emacs won't prefer old bytecode), then loads the source via
# emacsclient.

set -euo pipefail

input=$(cat)

file_path=$(printf '%s' "$input" | jq -r '.tool_input.file_path // empty')

# Only act on .el files
[[ "$file_path" == *.el ]] || exit 0

# Skip byte-compiled files
[[ "$file_path" != *.elc ]] || exit 0

# Skip test files — they should be run, not loaded
[[ "$file_path" != *-test.el ]] && [[ "$file_path" != *-tests.el ]] || exit 0

# Try to delete stale .elc and load source in running Emacs.
# locate-library finds the .elc via the load path; if it lives under
# elpaca/builds/ we delete it so the freshly-edited .el takes precedence.
if emacsclient --eval "
(let* ((feat (file-name-sans-extension (file-name-nondirectory \"$file_path\")))
       (elc  (locate-library (concat feat \".elc\"))))
  (when (and elc (string-match-p \"elpaca/builds/\" elc))
    (delete-file elc)
    (message \"Deleted stale %s\" elc))
  (load-file \"$file_path\"))" >/dev/null 2>&1; then
  jq -n --arg f "$file_path" '{
    "hookSpecificOutput": {
      "message": ("Loaded " + $f + " in running Emacs (stale .elc invalidated).")
    }
  }'
fi
