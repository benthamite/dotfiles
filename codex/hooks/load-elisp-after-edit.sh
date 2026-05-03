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

while IFS= read -r file_path; do
  # Only act on .el source files inside elpaca or dotfiles extras.
  [[ "$file_path" == *.el ]]              || continue
  [[ "$file_path" != *.elc ]]             || continue
  [[ "$file_path" != *-test.el ]]         || continue
  [[ "$file_path" != *-tests.el ]]        || continue
  [[ "$file_path" == *elpaca/sources/* ]] || \
  [[ "$file_path" == */dotfiles/emacs/extras/* ]] || continue

  # Escape backslashes and double-quotes so the path is safe inside an Elisp string.
  escaped_path=${file_path//\\/\\\\}
  escaped_path=${escaped_path//\"/\\\"}

  # Ask Emacs to find the package and schedule a rebuild.
  # The rebuild is deferred via run-at-time because elpaca-wait uses sit-for,
  # which deadlocks when called from inside emacsclient's server process filter.
  # First try the file's basename as a package name (handles extras packages
  # that share the dotfiles source dir). Fall back to source dir prefix match.
  result=$(timeout 30 emacsclient -e "
(let* ((file \"$escaped_path\")
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
  [[ "$pkg" != "nil" ]] || continue

  count=$((count + 1))
  if [ -z "$first_pkg" ]; then
    first_pkg="$pkg"
  fi
done < <(codex_changed_paths "$input")

[ "$count" -gt 0 ] || exit 0

jq -n --arg p "$first_pkg" --argjson c "$count" '{
  "hookSpecificOutput": {
    "message": (if $c == 1 then "Rebuilt and reloaded: " + $p else "Rebuilt and reloaded " + ($c|tostring) + " Elisp package(s), starting with: " + $p end)
  }
}'
