#!/usr/bin/env bash
# PostToolUse hook: rebuild and reload elpaca package after editing .el files.
#
# Fires on Edit|Write. If the file is .el and belongs to an elpaca package,
# runs elpaca-rebuild + elpaca-wait + elpaca-extras-reload synchronously so
# the running Emacs always has the latest byte-compiled code — matching what
# the user gets after a restart.

set -euo pipefail

input=$(cat)

file_path=$(printf '%s' "$input" | jq -r '.tool_input.file_path // empty')

# Only act on .el source files inside elpaca
[[ "$file_path" == *.el ]]              || exit 0
[[ "$file_path" != *.elc ]]             || exit 0
[[ "$file_path" != *-test.el ]]         || exit 0
[[ "$file_path" != *-tests.el ]]        || exit 0
[[ "$file_path" == *elpaca/sources/* ]] || exit 0

# Ask Emacs to find the package, rebuild, wait, and reload.
# First try the file's basename as a package name (handles extras packages
# that share the dotfiles source dir).  Fall back to source dir prefix match.
result=$(timeout 30 emacsclient -e "
(let* ((file \"$file_path\")
       (base (intern (file-name-sans-extension (file-name-nondirectory file))))
       (pkg (or (and (elpaca-get base) base)
                (cl-loop for e in (mapcar #'cdr (elpaca--queued))
                         for src = (elpaca-source-dir e)
                         when (and src (string-prefix-p src file))
                         return (cadr e)))))
  (when pkg
    (elpaca-rebuild pkg t)
    (elpaca-wait)
    (elpaca-extras-reload pkg)
    (format \"%s\" pkg)))" 2>&1) || exit 0

# Strip quotes from emacsclient output
pkg=$(echo "$result" | tr -d '"')
[[ "$pkg" != "nil" ]] || exit 0

jq -n --arg p "$pkg" '{
  "hookSpecificOutput": {
    "message": ("Rebuilt and reloaded: " + $p)
  }
}'
