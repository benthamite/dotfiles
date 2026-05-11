#!/bin/bash
# Stop hook: prompt the active agent to capture reusable session lessons.
set -euo pipefail

ROOT="/Users/pablostafforini/My Drive/dotfiles"
payload="$(cat)"

event="$(jq -r '.hook_event_name // ""' <<<"$payload")"
cwd="$(jq -r '.cwd // ""' <<<"$payload")"
active="$(jq -r '.stop_hook_active // false' <<<"$payload")"
session_id="$(jq -r '.session_id // "unknown-session"' <<<"$payload")"
transcript_path="$(jq -r '.transcript_path // "unavailable"' <<<"$payload")"

if [[ "$event" != "Stop" || "$active" == "true" ]]; then
  exit 0
fi

case "$cwd" in
  "$ROOT"|"$ROOT"/*) ;;
  *) exit 0 ;;
esac

reason=$(cat <<EOF
Use the session-learning-capture skill now.

Review this session for reusable lessons and write candidate lessons to .agent-learnings/inbox/ if any are worth preserving.

Hook context:
- tool: claude
- session_id: $session_id
- transcript_path: $transcript_path
- cwd: $cwd

Do not implement or promote lessons. Do not run session-retro. If there are no useful lessons, say so and stop.
EOF
)

jq -n --arg reason "$reason" '{decision: "block", reason: $reason}'
