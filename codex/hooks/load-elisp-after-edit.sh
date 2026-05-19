#!/usr/bin/env bash
# PostToolUse hook: rebuild and reload elpaca package after editing .el files.
#
# Fires on Edit|Write. If the file is .el and belongs to an elpaca package,
# runs elpaca-rebuild + elpaca-wait + elpaca-extras-reload synchronously so
# the running Emacs always has the latest byte-compiled code — matching what
# the user gets after a restart.

set -euo pipefail

# shellcheck source=lib-codex-paths.sh
source "$(dirname "$0")/lib-codex-paths.sh"

input=$(cat)

first_pkg=""
count=0
unresolved=0

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

  # Ask Emacs to find the package and schedule a rebuild.
  # The rebuild is deferred via run-at-time because elpaca-wait uses sit-for,
  # which deadlocks when called from inside emacsclient's server process filter.
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
    (run-at-time 0 nil
      (lambda ()
        (elpaca-rebuild pkg t)
        (elpaca-wait)
        (elpaca-extras-reload pkg)
        (message \"Rebuilt and reloaded: %s\" pkg)))
    (format \"%s\" pkg)))" 2>&1) || exit 0

  # Strip quotes from emacsclient output.
  pkg=$(echo "$result" | tr -d '"')
  if [[ "$pkg" == "nil" ]] || [[ -z "$pkg" ]]; then
    unresolved=$((unresolved + 1))
    continue
  fi

  count=$((count + 1))
  if [ -z "$first_pkg" ]; then
    first_pkg="$pkg"
  fi
done < <(codex_changed_paths "$input")

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
    "additionalContext": (if $c == 1 then "Rebuilt and reloaded: " + $p else "Rebuilt and reloaded " + ($c|tostring) + " Elisp package(s), starting with: " + $p end)
  }
}'
