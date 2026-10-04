#!/bin/bash
# Forward session-end event to Emacs via claude-code-event-hook.
# Called by the Claude Code "Stop" hook; intended to run through
# fire-and-forget.sh so the CLI doesn't block on emacsclient.
#
# A `claude -p' run from a session's shell inherits CLAUDE_BUFFER_NAME,
# so its Stop would mark the session waiting mid-turn.  The agent
# package writes the session id the session's status line reports to
# <sha256(AGENT_SESSION_UUID)>.sid; a payload naming another session is
# dropped.  This mirrors the agent package's hooks/session-owner.sh.
payload=$(cat)
uuid=${AGENT_SESSION_UUID:-}
if [ -n "$uuid" ]; then
  dir=${AGENT_CLAUDE_STATUS_DIR:-${TMPDIR:-/tmp}/claude-code-status}
  key=$(printf '%s' "$uuid" | shasum -a 256 | awk '{print $1}')
  expected=$(cat "$dir/$key.sid" 2>/dev/null)
  actual=$(printf '%s' "$payload" | grep -o '"session_id":"[^"]*"' | head -1 | cut -d'"' -f4)
  if [ -n "$expected" ] && [ -n "$actual" ] && [ "$actual" != "$expected" ]; then
    exit 0
  fi
fi
buf=${CLAUDE_BUFFER_NAME:-}
# Escape backslashes and double-quotes so the value is safe inside an Elisp string.
buf=${buf//\\/\\\\}
buf=${buf//\"/\\\"}
emacsclient -e "(claude-code-handle-hook 'stop \"${buf}\")" 2>/dev/null || true
