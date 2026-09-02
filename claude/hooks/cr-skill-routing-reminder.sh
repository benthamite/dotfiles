#!/bin/bash
# PreToolUse hook (Edit|Write): fires when the agent edits a CR "craft" file and
# reminds it that prompt/rubric/GOLDEN authoring is owned by a public skill, not
# something to hand-author. Non-blocking — it injects context, never denies.
#
# Why this exists: the recurring failure is the agent hand-editing a CR prompt or
# rubric instead of invoking /article-to-rubric (or /qa-reasoning for QA). A rule
# in an always-on doc demonstrably fails to stop it. This hook fires on the ACTION,
# in every session, with no reliance on what the agent remembers.
#
# Registered per reasoning-tasks worktree via sync-reasoning-tasks-worktree.sh
# (worktree-local .claude/settings.local.json).

input=$(cat)
fp=$(printf '%s' "$input" | python3 -c 'import sys,json
try:
    print(json.load(sys.stdin).get("tool_input",{}).get("file_path",""))
except Exception:
    print("")' 2>/dev/null)

case "$fp" in
  */tasks/*/prompt.txt|*/tasks/*/grading/rubric.md|*/tasks/*/grading/GOLDEN.md)
    cat <<'JSON'
{"hookSpecificOutput":{"hookEventName":"PreToolUse","additionalContext":"You are editing a CR prompt, rubric, or GOLDEN file. Authoring these is owned by the /article-to-rubric skill (QA by /qa-reasoning). If you have not invoked the owning skill for this change, invoke it now and continue from there rather than hand-editing."}}
JSON
    ;;
esac
exit 0
