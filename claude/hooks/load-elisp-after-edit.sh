#!/usr/bin/env bash
# PostToolUse hook: rebuild and reload elpaca package after editing .el files.
#
# Fires on Edit|Write. If the file is .el and belongs to an elpaca package,
# schedules an asynchronous elpaca rebuild that reloads the package once the
# build process finishes (via elpaca-post-queue-hook). The emacsclient call
# returns immediately and never blocks the daemon's command loop.

set -euo pipefail

input=$(cat)

file_path=$(printf '%s' "$input" | jq -r '.tool_input.file_path // empty')

absolute_changed_path() {
  local path="$1"

  case "$path" in
    /*) printf '%s\n' "$path" ;;
    *) printf '%s\n' "$PWD/$path" ;;
  esac
}

file_path=$(absolute_changed_path "$file_path")

# Only act on .el source files inside elpaca or dotfiles extras
[[ "$file_path" == *.el ]]              || exit 0
[[ "$file_path" != *.elc ]]             || exit 0
[[ "$file_path" != *-test.el ]]         || exit 0
[[ "$file_path" != *-tests.el ]]        || exit 0
[[ "$file_path" == *elpaca/sources/* ]] || \
[[ "$file_path" == */dotfiles/emacs/extras/* ]] || exit 0

# Encode the path with base64 so the elisp side can decode a literal string
# without ever exposing $(...) or backticks to the shell during interpolation.
path_b64=$(printf '%s' "$file_path" | base64 | tr -d '\n')

# Ask Emacs to find the package and schedule an async rebuild.
# `elpaca-extras-rebuild-and-reload' enqueues the build and reloads on
# completion via `elpaca-post-queue-hook' (elpaca's process sentinels), so it
# never blocks the command loop.  The enqueue is deferred via `run-at-time' 0
# only to leave the emacsclient process-filter context cleanly; it is a
# one-shot defer, not a wait loop.
# First try the file's basename as a package name (handles extras packages
# that share the dotfiles source dir).  Fall back to source dir prefix match.
result=$(timeout 30 emacsclient -e "
(let* ((file (decode-coding-string (base64-decode-string \"$path_b64\") 'utf-8))
       (base (intern (file-name-sans-extension (file-name-nondirectory file))))
       (pkg (or (and (elpaca-get base) base)
                (cl-loop for e in (mapcar #'cdr (elpaca--queued))
                         for src = (elpaca-source-dir e)
                         when (and src (string-prefix-p src file))
                         return (cadr e)))))
  (when pkg
    (run-at-time 0 nil (lambda () (elpaca-extras-rebuild-and-reload pkg)))
    (format \"%s\" pkg)))" 2>&1) || exit 0

# Strip quotes from emacsclient output
pkg=$(echo "$result" | tr -d '"')
if [[ "$pkg" == "nil" ]] || [[ -z "$pkg" ]]; then
  jq -n '{
    "hookSpecificOutput": {
      "message": "Edited Elisp file, but no elpaca package was resolved for rebuild"
    }
  }'
  exit 0
fi

jq -n --arg p "$pkg" '{
  "hookSpecificOutput": {
    "message": ("Scheduled async rebuild+reload of " + $p + " (reloads when the build finishes)")
  }
}'
