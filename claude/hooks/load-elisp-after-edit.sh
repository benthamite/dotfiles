#!/usr/bin/env bash
# PostToolUse hook: rebuild and reload elpaca package after editing .el files.
#
# Fires on Edit|Write. If the file is .el and belongs to an elpaca package,
# schedules an asynchronous elpaca rebuild that reloads the package once the
# build process finishes (via elpaca-post-queue-hook). The hook waits by
# polling a status token with short emacsclient calls, so the daemon's command
# loop is never held by a long-running wait expression.

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
reload_timeout=${ELPACA_RELOAD_TIMEOUT_SECONDS:-120}
reload_poll_interval=${ELPACA_RELOAD_POLL_INTERVAL_SECONDS:-1}

strip_emacs_string() {
  sed 's/^"//; s/"$//'
}

# Ask Emacs to find the package and schedule an async rebuild.
# `elpaca-extras-rebuild-and-reload' enqueues the build and reloads on
# completion via `elpaca-post-queue-hook' (elpaca's process sentinels), so it
# never blocks the command loop.  The shell hook then polls the returned token
# with short emacsclient calls; there is no long-running wait inside Emacs.
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
    (format \"%s:%s\" pkg (elpaca-extras-rebuild-and-reload pkg))))" 2>&1) || exit 0

# Strip quotes from emacsclient output
result=$(printf '%s' "$result" | strip_emacs_string)
pkg=${result%%:*}
token=${result#*:}
if [[ "$pkg" == "nil" ]] || [[ -z "$pkg" ]]; then
  jq -n '{
    "hookSpecificOutput": {
      "message": "Edited Elisp file, but no elpaca package was resolved for rebuild"
    }
  }'
  exit 0
fi

if [[ -z "$token" ]] || [[ "$token" == "$pkg" ]]; then
  jq -n --arg p "$pkg" '{
    "hookSpecificOutput": {
      "message": ("Scheduled rebuild+reload for " + $p + ", but no completion token was returned")
    }
  }'
  exit 0
fi

deadline=$(( $(date +%s) + reload_timeout ))
status="queued"
message="Build queued"

while (( $(date +%s) <= deadline )); do
  status_result=$(timeout 10 emacsclient -e "(elpaca-extras-format-build-reload-status \"$token\")" 2>&1 | strip_emacs_string) || status_result="poll-error:$status_result"
  status=${status_result%%:*}
  message=${status_result#*:}
  case "$status" in
    finished)
      jq -n --arg p "$pkg" --arg m "$message" '{
        "hookSpecificOutput": {
          "message": ("Completed rebuild+reload of " + $p + ": " + $m)
        }
      }'
      exit 0
      ;;
    failed)
      jq -n --arg p "$pkg" --arg m "$message" '{
        "hookSpecificOutput": {
          "message": ("Failed rebuild+reload of " + $p + ": " + $m)
        }
      }'
      exit 1
      ;;
  esac
  sleep "$reload_poll_interval"
done

jq -n --arg p "$pkg" --arg s "$status" --arg m "$message" --arg timeout "$reload_timeout" '{
  "hookSpecificOutput": {
    "message": ("Timed out after " + $timeout + "s waiting for rebuild+reload of " + $p + " (last status: " + $s + ": " + $m + ")")
  }
}'
exit 1
