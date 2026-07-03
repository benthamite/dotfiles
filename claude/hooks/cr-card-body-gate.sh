#!/bin/bash
# PreToolUse hook (Bash): block `gh issue create|edit` whose --body-file is a CR
# card attempt but does NOT pass cr_card_body.py check. Forces CR cards through the
# template tool instead of hand-assembled bodies.
#
# Why: the recurring failure is creating/editing a CR task card with a hand-rolled
# body (missing the biblio line, author, or AI abstract). cr-task-build already
# mandates "cr_card_body.py check must pass before every gh issue create/edit"; this
# hook makes that gate mechanical — a fresh session that hand-rolls a card body is
# denied at the gh command, regardless of what it remembers.
#
# Scoped so it does NOT block non-CR issues: it only runs the check when the body
# already looks like a CR card attempt (has a Files/Scores/Triage marker or a
# Redwood source). Anything else is allowed untouched. Fail-open on an unreadable
# body-file path (don't block a command we can't inspect).

input=$(cat)
cmd=$(printf '%s' "$input" | python3 -c 'import sys,json
try: print(json.load(sys.stdin).get("tool_input",{}).get("command",""))
except Exception: print("")' 2>/dev/null)

case "$cmd" in
  *"gh issue create"*|*"gh issue edit"*) : ;;
  *) exit 0 ;;
esac

bf=$(printf '%s' "$cmd" | grep -oE -- '--body-file[ =]+[^ ]+' | head -1 | sed -E 's/--body-file[ =]+//' | tr -d "\"'")
[ -n "$bf" ] || exit 0
[ -f "$bf" ] || exit 0

# only enforce when the body is clearly a CR-card attempt
grep -qE '^\*\*Files:\*\*|^## Scores|^\*\*Triage:\*\*|blog\.redwoodresearch\.org|^\*\*Published:\*\*' "$bf" || exit 0

CB="$HOME/Trajectory/.claude/skills/cr-task-build/cr_card_body.py"
[ -f "$CB" ] || exit 0
out=$(python3 "$CB" check --body-file "$bf" 2>&1)
if [ $? -ne 0 ]; then
  python3 - "$out" <<'PY'
import json, sys
reason = ("⛔ CR card body failed cr_card_body.py check — do NOT hand-assemble a CR card. "
          "Build it via cr_card_body.py (title/venue/author/words/source/abstract), which prepends "
          "the biblio+abstract to the scorecard. Check errors:\n" + sys.argv[1])
print(json.dumps({"hookSpecificOutput": {"hookEventName": "PreToolUse",
      "permissionDecision": "deny", "permissionDecisionReason": reason}}))
PY
fi
exit 0
