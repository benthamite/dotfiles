#!/usr/bin/env bash
# PreToolUse guard: block posting a GitHub issue comment to the agent-c repo
# when the issue has a comment the agent has not acknowledged reading.
#
# Why: QA/adjudication comments are drafted over long sessions; the thread can
# move meanwhile (2026-07-03: a QA comment asserting "no Fable run exists" was
# posted 31 minutes after a comment reporting exactly that run). Prose rules
# ("re-read the thread before posting") are attention-dependent; this makes the
# staleness check deterministic.
#
# Mechanics: registered per-worktree in .claude/settings.local.json (installed
# by sync-agent-c-worktree.sh) as a PreToolUse hook on Bash. When the command
# is a comment-post to trajectory-labs-pbc/agent-c, fetch the newest comment on
# the target issue; allow only if ~/.cache/cr-freshness/agent-c-issue-<N>
# already contains that comment's id. Otherwise deny (exit 2) and print the
# newest comment so the agent reads it in the denial itself, then acks:
#   mkdir -p ~/.cache/cr-freshness && echo <id> > ~/.cache/cr-freshness/agent-c-issue-<N>
# Fail-open on network/parse errors: a broken guard must not block all posting.

set -u
REPO="trajectory-labs-pbc/agent-c"
CACHE_DIR="$HOME/.cache/cr-freshness"

payload="$(cat 2>/dev/null || true)"
cmd="$(printf '%s' "$payload" | python3 -c '
import json,sys
try:
    d = json.load(sys.stdin)
    if d.get("tool_name") != "Bash": raise SystemExit
    print(d.get("tool_input", {}).get("command", ""))
except Exception:
    pass
' 2>/dev/null || true)"
[ -n "$cmd" ] || exit 0

# Only comment-POSTS to the agent-c repo. Two shapes:
#   gh issue comment <N> --repo trajectory-labs-pbc/agent-c ...
#   gh api [-X POST] repos/trajectory-labs-pbc/agent-c/issues/<N>/comments -f body=...
issue=""
case "$cmd" in
  *"gh issue comment"*"$REPO"*)
    issue="$(printf '%s' "$cmd" | sed -nE 's/.*gh issue comment[[:space:]]+"?([0-9]+)"?.*/\1/p')"
    ;;
  *"gh api"*"$REPO/issues/"*/comments*)
    # gh api defaults to GET; only field/POST forms create comments.
    case "$cmd" in
      *" -f "*|*" -F "*|*"--field"*|*"-X POST"*|*"--method POST"*)
        issue="$(printf '%s' "$cmd" | sed -nE "s|.*$REPO/issues/([0-9]+)/comments.*|\1|p")"
        ;;
    esac
    ;;
esac
[ -n "$issue" ] || exit 0

# NB: the REST comments list ignores sort/direction params (always oldest-first),
# so use `gh issue view`, whose comments array is ascending -- the last is newest.
newest="$(gh issue view "$issue" --repo "$REPO" --json comments \
  --jq '.comments[-1] | "\(.id)\(.author.login)\(.createdAt)\(.body[0:500])"' 2>/dev/null || true)"
[ -n "$newest" ] || exit 0   # no comments yet, or API failure — fail open

newest_id="${newest%%$'\x1f'*}"
ack_file="$CACHE_DIR/agent-c-issue-$issue"
if [ -f "$ack_file" ] && [ "$(cat "$ack_file" 2>/dev/null)" = "$newest_id" ]; then
  exit 0
fi

rest="${newest#*$'\x1f'}"
author="${rest%%$'\x1f'*}"; rest="${rest#*$'\x1f'}"
created="${rest%%$'\x1f'*}"; body="${rest#*$'\x1f'}"

{
  echo "BLOCKED: issue #$issue has a comment you have not acknowledged."
  echo "Newest comment — id $newest_id by $author at $created:"
  echo "---"
  printf '%s\n' "$body"
  echo "---"
  echo "Read the full thread if needed (gh issue view $issue --repo $REPO --comments)."
  echo "If your draft is still correct AFTER reading this, acknowledge and retry:"
  echo "  mkdir -p $CACHE_DIR && echo $newest_id > $ack_file"
  echo "If the comment changes anything, revise your draft first."
} >&2
exit 2
