#!/bin/bash
# SessionStart hook: if the current Claude Code account lacks a
# `chromeExtension.pairedDeviceId` in its .claude.json, print a one-line
# reminder to pair it to the matching Chrome profile. Non-blocking.
#
# Account ↔ Chrome-profile mapping (maintained by hand — keep in sync
# with claude/README.org "Multi-account setup" section):
#   .claude-personal → Chrome "Default"    (pablo.stafforini@gmail.com)
#   .claude-epoch    → Chrome "Profile 2"  (pablo@epoch.ai)
#   .claude-tlon     → Chrome "Profile 3"  (pablo@tlon.team)

set -euo pipefail

# Drain stdin so Claude Code doesn't block on a broken pipe.
cat >/dev/null || true

CFG_DIR="${CLAUDE_CONFIG_DIR:-}"
case "$CFG_DIR" in
  */.claude-personal) ACCOUNT="personal"; PROFILE="Default";    GOOGLE_ACCT="pablo.stafforini@gmail.com" ;;
  */.claude-epoch)    ACCOUNT="epoch";    PROFILE="Profile 2";  GOOGLE_ACCT="pablo@epoch.ai" ;;
  */.claude-tlon)     ACCOUNT="tlon";     PROFILE="Profile 3";  GOOGLE_ACCT="pablo@tlon.team" ;;
  *) exit 0 ;;  # unknown layout; stay silent
esac

JSON="$CFG_DIR/.claude.json"
[ -f "$JSON" ] || exit 0

PAIRED=$(jq -r '.chromeExtension.pairedDeviceId // empty' "$JSON" 2>/dev/null || true)
[ -n "$PAIRED" ] && exit 0  # already paired

cat <<EOF
[chrome-pair] Claude account "$ACCOUNT" is not paired to a Chrome profile.
             Expected profile: "$PROFILE" ($GOOGLE_ACCT).
             To pair once: run "/chrome" in this session, then click
             Connect in the Claude extension inside the "$PROFILE" window.
EOF

exit 0
