#!/usr/bin/env bash
# PostToolUse hook: regenerate Texinfo manual (.texi and .info) after editing
# an .org source that drives a Texinfo export.
#
# Fires on Edit|Write.  The .dir-locals.el in these repos runs
# `org-texinfo-export-to-texinfo` from `after-save-hook`, which fires only on
# interactive Emacs saves.  Claude's Edit/Write tools bypass that hook, so the
# generated manual silently drifts from its org source.  This hook closes that
# gap for Claude's edit path.
#
# Criteria to fire:
#   - edited file ends in .org
#   - file contains `#+texinfo_filename:` or `#+export_file_name: *.info`
#
# Actions:
#   - regenerate <name>.texi via ox-texinfo
#   - if <name>.info exists next to the .texi, regenerate it via makeinfo

set -euo pipefail

# shellcheck source=lib-codex-paths.sh
source "$(dirname "$0")/lib-codex-paths.sh"

input=$(cat)

total_texi=0
total_info=0
last_dir=""

while IFS= read -r file_path; do
  [ -n "$file_path" ]         || continue
  [[ "$file_path" == *.org ]] || continue
  [ -f "$file_path" ]         || continue

  # Only regenerate when the .org declares a Texinfo export target.
  grep -qiE '^#\+(texinfo_filename|export_file_name):' "$file_path" || continue

  dir=$(dirname "$file_path")
  last_dir="$dir"

  # Run ox-texinfo in a clean batch Emacs.
  if ! texi_output=$(emacs --batch -Q "$file_path" --eval '
(progn
  (setq create-lockfiles nil
        make-backup-files nil)
  (require (quote org))
  (require (quote ox-texinfo))
  (setq org-export-with-broken-links t)
  (princ (concat "\n::codex-texi::"
                 (org-texinfo-export-to-texinfo)
                 "\n")))' 2>&1); then
    jq -n --arg msg "Texinfo export failed: $(echo "$texi_output" | tail -5 | tr '\n' ' ')" \
      '{"hookSpecificOutput":{"hookEventName":"PostToolUse","additionalContext":$msg}}'
    exit 0
  fi

  texi=$(printf '%s\n' "$texi_output" | sed -n 's/^::codex-texi:://p' | tail -1)
  [ -n "$texi" ] || continue
  case "$texi" in
    /*) ;;
    *) texi="$dir/$texi" ;;
  esac
  [ -f "$texi" ] || continue

  # ox-texinfo drops the .texi alongside the .org. Regenerate the matching
  # .info only when that .info is already part of the repository layout.
  info_count=0
  texi_count=1
  info="${texi%.texi}.info"
  if [ -f "$info" ] && makeinfo --no-split "$texi" -o "$info" >/dev/null 2>&1; then
    info_count=1
  fi

  total_texi=$((total_texi + texi_count))
  total_info=$((total_info + info_count))
done < <(codex_changed_paths "$input")

[ "$total_texi" -gt 0 ] || exit 0

jq -n --arg dir "$last_dir" --argjson t "$total_texi" --argjson i "$total_info" \
  '{"hookSpecificOutput":{"hookEventName":"PostToolUse","additionalContext":("Regenerated " + ($t|tostring) + " .texi and " + ($i|tostring) + " .info file(s) in " + $dir)}}'
