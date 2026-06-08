#!/bin/bash
# PreToolUse hook: gate long-running consensus-trader pipeline launches.
#
# Multi-hour rebuilds (scripts/setup_db.py --full/--target,
# scripts/refresh_data.sh) have repeatedly wasted overnight sessions when
# launched without a complete pre-flight: an unhardened phase OOMs partway, or
# checkpointing is off, or the code was still mid-edit.  This hook turns the
# implicit "should have prepared" into an enforced gate.  It blocks such a
# launch unless ALL of:
#
#   1. the working tree is clean for pipeline code (consensus_trader/, scripts/)
#      — never start a multi-hour run mid-edit;
#   2. checkpointing is not disabled (no PIPELINE_CHECKPOINT=0) — so a crash or
#      incomplete prep costs one phase, not the whole run;
#   3. the command carries an explicit pre-flight ack token, PREFLIGHT=ok —
#      forcing a conscious pass over the checklist below before committing hours
#      of compute.
#
# Reads JSON from stdin (Claude Code PreToolUse format); emits a deny decision
# with the failing gate(s) + checklist, or exits 0 to allow.

set -euo pipefail

INPUT=$(cat)
COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')

# Only intercept actual long-running pipeline LAUNCHES, not commands that
# merely mention the scripts (grep/cat/git show/etc. stay allowed).  A launch
# is a python invocation of setup_db.py with --full/--target, or a shell
# invocation of refresh_data.sh.
LAUNCH=0
if echo "$COMMAND" | grep -qE '(python3?|nohup)[^|]*setup_db\.py[^|]*(--full|--target)'; then
  LAUNCH=1
fi
if echo "$COMMAND" | grep -qE '(bash|sh|nohup|\./|source )[^|]*refresh_data\.sh'; then
  LAUNCH=1
fi
if [ "$LAUNCH" -eq 0 ]; then
  exit 0
fi

# Scope to the consensus-trader repo so this never fires on other projects.
source "$(dirname "$0")/lib-repo-root.sh" 2>/dev/null || true
REPO_ROOT="${REPO_ROOT:-$PWD}"
case "$REPO_ROOT" in
  *consensus-trader) : ;;
  *) exit 0 ;;
esac

FAILURES=""

# Gate 1: clean working tree for pipeline code.
DIRTY=$(git -C "$REPO_ROOT" status --porcelain -- consensus_trader scripts 2>/dev/null || true)
if [ -n "$DIRTY" ]; then
  FAILURES="${FAILURES}\n  [1] Uncommitted pipeline changes — commit before a multi-hour run:\n$(printf '%s' "$DIRTY" | sed 's/^/      /')"
fi

# Gate 2: checkpointing must not be disabled.
if echo "$COMMAND" | grep -qE 'PIPELINE_CHECKPOINT=0'; then
  FAILURES="${FAILURES}\n  [2] PIPELINE_CHECKPOINT=0 disables resumability. Long runs must be checkpointed so a crash costs one phase, not the night."
fi

# Gate 3: explicit pre-flight acknowledgment.
if ! echo "$COMMAND" | grep -qE '\bPREFLIGHT=ok\b'; then
  FAILURES="${FAILURES}\n  [3] Missing PREFLIGHT=ok. Confirm the pre-flight checklist, then prefix the command with PREFLIGHT=ok."
fi

if [ -z "$FAILURES" ]; then
  exit 0
fi

REASON="BLOCKED: long pipeline run is not pre-flight-ready.${FAILURES}\n\nPre-flight checklist before committing hours of compute:\n  - Every analogous phase hardened? (e.g. ALL big-table scans chunked, not just the one that crashed last time — grep the code path, don't fix reactively.)\n  - Checkpoint/resume on (default) so any crash resumes in minutes?\n  - Runtime range quoted and (if >30 min) approved?\n  - Kill-switch / early-failure check defined?\n  - Code committed and tests green for the whole path being exercised?\n\nWhen all of the above hold, re-run with PREFLIGHT=ok prefixed."

jq -n --arg reason "$REASON" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": $reason
  }
}'
