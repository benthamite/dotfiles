#!/usr/bin/env bash
# PostToolUse hook: rebuild and reload elpaca package after editing .el files.
#
# Fires on Edit|Write. If the file is .el and belongs to an elpaca package,
# schedules an asynchronous elpaca rebuild that reloads the package once the
# build process finishes (via elpaca-post-queue-hook). The hook waits by
# polling a status token with short emacsclient calls, so the daemon's command
# loop is never held by a long-running wait expression.

set -euo pipefail

# shellcheck source=lib-codex-paths.sh
source "$(dirname "$0")/lib-codex-paths.sh"

input=$(cat)

first_pkg=""
count=0
failed=0
unresolved=0
reload_timeout=${ELPACA_RELOAD_TIMEOUT_SECONDS:-120}
reload_poll_interval=${ELPACA_RELOAD_POLL_INTERVAL_SECONDS:-1}
last_status=""

strip_emacs_string() {
  sed 's/^"//; s/"$//'
}

codex_absolute_changed_path() {
  local path="$1"

  case "$path" in
    /*) printf '%s\n' "$path" ;;
    *) printf '%s\n' "$PWD/$path" ;;
  esac
}

while IFS= read -r file_path; do
  file_path=$(codex_absolute_changed_path "$file_path")

  # Only act on .el source files inside elpaca or dotfiles extras.
  [[ "$file_path" == *.el ]]              || continue
  [[ "$file_path" != *.elc ]]             || continue
  [[ "$file_path" != *-test.el ]]         || continue
  [[ "$file_path" != *-tests.el ]]        || continue
  [[ "$file_path" == *elpaca/sources/* ]] || \
  [[ "$file_path" == */dotfiles/emacs/extras/* ]] || continue

  # Encode the path with base64 so the elisp side can decode a literal string
  # without ever exposing $(...) or backticks to the shell during interpolation.
  path_b64=$(printf '%s' "$file_path" | base64 | tr -d '\n')

  # Ask Emacs to find the package and schedule an async rebuild.
  # `elpaca-extras-rebuild-and-reload' enqueues the build and reloads on
  # completion via `elpaca-post-queue-hook' (elpaca's process sentinels), so it
  # never blocks the command loop.  The shell hook then polls the returned
  # token with short emacsclient calls; there is no long-running wait inside
  # Emacs.
  # First try the file's basename as a package name (handles extras packages
  # that share the dotfiles source dir). Fall back to source dir prefix match.
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

  # Strip quotes from emacsclient output.
  result=$(printf '%s' "$result" | strip_emacs_string)
  pkg=${result%%:*}
  token=${result#*:}
  if [[ "$pkg" == "nil" ]] || [[ -z "$pkg" ]]; then
    unresolved=$((unresolved + 1))
    continue
  fi
  if [[ -z "$token" ]] || [[ "$token" == "$pkg" ]]; then
    failed=$((failed + 1))
    last_status="no completion token returned for $pkg"
    continue
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
        break
        ;;
      failed)
        failed=$((failed + 1))
        last_status="$pkg: $message"
        break
        ;;
    esac
    sleep "$reload_poll_interval"
  done

  if [[ "$status" != "finished" ]]; then
    if [[ "$status" != "failed" ]]; then
      failed=$((failed + 1))
      last_status="$pkg timed out after ${reload_timeout}s (last status: $status: $message)"
    fi
    continue
  fi

  count=$((count + 1))
  if [ -z "$first_pkg" ]; then
    first_pkg="$pkg"
  fi
done < <(codex_changed_paths "$input")

if [ "$failed" -gt 0 ]; then
  jq -n --argjson f "$failed" --arg s "$last_status" '{
    "hookSpecificOutput": {
      "hookEventName": "PostToolUse",
      "additionalContext": ("Failed rebuild+reload for " + ($f|tostring) + " Elisp package(s): " + $s)
    }
  }'
  exit 1
fi

if [ "$count" -eq 0 ]; then
  if [ "$unresolved" -gt 0 ]; then
    jq -n --argjson c "$unresolved" '{
      "hookSpecificOutput": {
        "hookEventName": "PostToolUse",
        "additionalContext": ("Edited " + ($c|tostring) + " Elisp file(s), but no elpaca package was resolved for rebuild")
      }
    }'
  fi
  exit 0
fi

jq -n --arg p "$first_pkg" --argjson c "$count" '{
  "hookSpecificOutput": {
    "hookEventName": "PostToolUse",
    "additionalContext": (if $c == 1 then "Completed rebuild+reload: " + $p else "Completed rebuild+reload for " + ($c|tostring) + " Elisp package(s), starting with: " + $p end)
  }
}'
