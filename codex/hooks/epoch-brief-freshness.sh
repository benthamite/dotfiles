#!/usr/bin/env bash
# PostToolUse hook (shell tools): when the tool call read an Epoch project brief,
# add that brief's possibly stale TODOs to Codex's context, once per session
# and brief. The Epoch helper owns the detection, audit and dedup logic and
# prints the PostToolUse additionalContext JSON; this wrapper only filters and
# dispatches cheaply. Machines without the Epoch notes tree are a silent no-op.

set -uo pipefail

INPUT=$(cat)
case "$INPUT" in
  *.org*) ;;
  *) exit 0 ;;
esac

# The shared directory resolves from the most specific variable set:
# EPOCH_SHARED_DIR, then EPOCH_PROJECTS, then EPOCH_WORKSPACE (EPOCH_ROOT is
# its older alias), then the notes tree's original Drive location.
WORKSPACE="${EPOCH_WORKSPACE:-${EPOCH_ROOT:-$HOME/My Drive/Epoch}}"
PROJECTS="${EPOCH_PROJECTS:-$WORKSPACE/projects}"
SHARED="${EPOCH_SHARED_DIR:-$PROJECTS/shared}"
HELPER="$SHARED/scripts/brief_freshness_notice.py"
[ -f "$HELPER" ] || exit 0

printf '%s' "$INPUT" | python3 -B "$HELPER"
exit 0
