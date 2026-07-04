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
# Registered per reasoning-tasks worktree via sync-reasoning-tasks-worktree.sh (worktree-local
# .claude/settings.local.json), because the user-level ~/.claude/settings.json is a
# self-protected guard file agents cannot edit.

input=$(cat)
fp=$(printf '%s' "$input" | python3 -c 'import sys,json
try:
    print(json.load(sys.stdin).get("tool_input",{}).get("file_path",""))
except Exception:
    print("")' 2>/dev/null)

case "$fp" in
  */tasks/*/prompt.txt|*/tasks/*/grading/rubric.md|*/tasks/*/grading/GOLDEN.md)
    cat <<'JSON'
{"hookSpecificOutput":{"hookEventName":"PreToolUse","additionalContext":"⚠️ CR ROUTING GUARD — you are editing a CR prompt/rubric/GOLDEN. This is authoring craft OWNED by the /article-to-rubric skill (QA is owned by /qa-reasoning). If you are hand-authoring or hand-fixing this instead of having INVOKED the owning skill, STOP now and invoke the skill so you behave exactly as if the user had invoked it directly. Doing skill-owned craft by hand is a routing-principle violation."}}
JSON
    ;;
esac
exit 0
