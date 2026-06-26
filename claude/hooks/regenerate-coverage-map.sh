#!/usr/bin/env bash
# PostToolUse hook (Edit|Write): when a CR task's grading/rubric.md or
# grading/coverage-map.json is edited, regenerate its rubric-visualizer coverage map so
# the visualizer ALWAYS reflects the latest change. (Run-side grade updates are refreshed
# by the eval wrapper, which ends with the same regenerate.) Silent no-op for any other
# file; never blocks.
input=$(cat)
fp=$(printf '%s' "$input" | python3 -c "import sys,json;print(json.load(sys.stdin).get('tool_input',{}).get('file_path',''))" 2>/dev/null)
case "$fp" in
  */grading/rubric.md|*/grading/coverage-map.json) ;;
  *) exit 0 ;;
esac
task_dir="${fp%/grading/*}"
viz="$HOME/My Drive/repos/rubric-visualizer/coverage_map.py"
[ -f "$viz" ] && [ -d "$task_dir/grading" ] && python3 "$viz" "$task_dir" >/dev/null 2>&1
exit 0
