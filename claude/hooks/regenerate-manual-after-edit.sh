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

input=$(cat)
file_path=$(printf '%s' "$input" | jq -r '.tool_input.file_path // empty')

[ -n "$file_path" ]            || exit 0
[[ "$file_path" == *.org ]]    || exit 0
[ -f "$file_path" ]            || exit 0

# Only regenerate when the .org declares a Texinfo export target.
grep -qiE '^#\+(texinfo_filename|export_file_name):' "$file_path" || exit 0

dir=$(dirname "$file_path")

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
    '{"hookSpecificOutput":{"message":$msg}}'
  exit 0
fi

texi=$(printf '%s\n' "$texi_output" | sed -n 's/^::codex-texi:://p' | tail -1)
[ -n "$texi" ] || exit 0
case "$texi" in
  /*) ;;
  *) texi="$dir/$texi" ;;
esac
[ -f "$texi" ] || exit 0

# ox-texinfo drops the .texi alongside the .org.  Regenerate the matching
# .info only when that .info is already part of the repository layout.
info_count=0
texi_count=1
info="${texi%.texi}.info"
if [ -f "$info" ] && makeinfo --no-split "$texi" -o "$info" >/dev/null 2>&1; then
  info_count=1
fi

[ "$texi_count" -gt 0 ] || exit 0

jq -n --arg dir "$dir" --argjson t "$texi_count" --argjson i "$info_count" \
  '{"hookSpecificOutput":{"message":("Regenerated " + ($t|tostring) + " .texi and " + ($i|tostring) + " .info file(s) in " + $dir)}}'
