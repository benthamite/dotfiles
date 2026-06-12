#!/bin/bash
# PreToolUse hook: block raw paid Ahrefs API calls.
#
# Ahrefs read-only API calls still consume a shared monthly unit budget.
# Agents must use claude/bin/ahrefs-api-guard (or an equivalent project-level
# guard) so the free quota endpoint is checked before any paid request.
#
# Matcher: Bash

set -euo pipefail

INPUT=$(cat)

TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')
[ "$TOOL_NAME" = "Bash" ] || exit 0

CMD=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
[ -n "$CMD" ] || exit 0

if ! echo "$CMD" | grep -q 'api\.ahrefs\.com'; then
  exit 0
fi

# The dedicated guard is allowed to contact both the free usage endpoint and
# the requested paid endpoint after it has checked the live quota.
if echo "$CMD" | grep -qE '(^|[[:space:]/])ahrefs-api-guard([[:space:]]|$)'; then
  exit 0
fi

# The free quota endpoint is allowed directly; it consumes zero units and is the
# preflight agents must run before deciding whether work can proceed.
if echo "$CMD" | grep -q 'subscription-info/limits-and-usage' && \
   ! echo "$CMD" | grep -qE 'site-explorer|site-audit|keywords-explorer|rank-tracker|web-analytics|brand-radar|gsc|serp-overview|batch-analysis|public-crawler|management'; then
  exit 0
fi

jq -n '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": "BLOCKED: raw Ahrefs API call detected. Ahrefs API units are shared across Epoch automations. Use `ahrefs-api-guard request ...` so the free quota probe runs and blocks the call if remaining units are below the required reserve."
  }
}'
exit 0
