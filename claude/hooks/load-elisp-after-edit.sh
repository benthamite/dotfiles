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

normalize_changed_path() {
  local path="$1"
  local directory

  directory=$(cd -- "$(dirname -- "$path")" 2>/dev/null && pwd -P) || return 1
  printf '%s/%s\n' "$directory" "$(basename -- "$path")"
}

git_operation_in_progress() {
  local path="$1"
  local git_dir

  git_dir=$(git -C "$(dirname "$path")" rev-parse --absolute-git-dir 2>/dev/null) || return 1
  if [ -d "$git_dir/rebase-merge" ] ||
     [ -d "$git_dir/rebase-apply" ] ||
     [ -f "$git_dir/MERGE_HEAD" ] ||
     [ -f "$git_dir/CHERRY_PICK_HEAD" ] ||
     [ -f "$git_dir/REVERT_HEAD" ]; then
    return 0
  fi

  if git -C "$(dirname "$path")" diff --quiet --diff-filter=U -- 2>/dev/null; then
    return 1
  else
    [ "$?" -eq 1 ]
  fi
}

test_elisp_file_p() {
  local path="$1"
  local relative

  case "$path" in
    *elpaca/sources/*)
      relative="${path##*elpaca/sources/}"
      relative="${relative#*/}"
      ;;
    */dotfiles/emacs/extras/*)
      relative="${path##*/dotfiles/emacs/extras/}"
      ;;
    *) return 1 ;;
  esac

  case "$relative" in
    test/* | */test/* | tests/* | */tests/*) return 0 ;;
  esac
  case "${relative##*/}" in
    *-test.el | *-tests.el | test-*.el) return 0 ;;
    *) return 1 ;;
  esac
}

file_path=$(absolute_changed_path "$file_path")
file_path=$(normalize_changed_path "$file_path") || exit 0

# Only act on .el source files inside elpaca or dotfiles extras
[[ "$file_path" == *.el ]]              || exit 0
[[ "$file_path" != *.elc ]]             || exit 0
[[ "$file_path" == *elpaca/sources/* ]] || \
[[ "$file_path" == */dotfiles/emacs/extras/* ]] || exit 0
test_elisp_file_p "$file_path" && exit 0

if git_operation_in_progress "$file_path"; then
  jq -n '{
    "hookSpecificOutput": {
      "message": "Skipped rebuild+reload because a Git operation is in progress"
    }
  }'
  exit 0
fi

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
if ! result=$(timeout 30 emacsclient -e "
(let* ((file (decode-coding-string (base64-decode-string \"$path_b64\") 'utf-8))
       (resolution (elpaca-extras-resolve-package file))
       (pkg (plist-get resolution :id)))
  (when pkg
    (format \"%s:%s\" pkg (elpaca-extras-rebuild-and-reload pkg))))" 2>&1); then
  jq -n --arg m "$result" '{
    "hookSpecificOutput": {
      "message": ("Failed to resolve the edited Elisp package: " + $m)
    }
  }'
  exit 1
fi

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
