#!/bin/bash
# PreToolUse hook: require confirmation before externally-visible MCP actions.
#
# Guards tools that post messages, send emails, create tweets, or delete
# shared resources. These actions are visible to others and hard to undo.
#
# Matcher: mcp__

set -euo pipefail

INPUT=$(cat)

TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')

# Only check MCP tools
echo "$TOOL_NAME" | grep -q '^mcp__' || exit 0

deny() {
  local label="$1"
  jq -n --arg label "$label" --arg tool "$TOOL_NAME" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": ("BLOCKED: " + $tool + " — " + $label + ".\n\nThis action is visible to others. Ask the user for explicit confirmation before proceeding.\n\nIf the user has already confirmed, they can run this via `!` or approve the tool call.")
    }
  }'
  exit 0
}

case "$TOOL_NAME" in
  # Gmail — sending and replying
  mcp__google-workspace-epoch__gmail_send_email|\
  mcp__google-workspace-epoch__gmail_reply_to_email|\
  mcp__google-workspace-epoch__gmail_send_draft|\
  mcp__claude_ai_Gmail__gmail_create_draft)
    deny "sends or drafts an email"
    ;;

  # Slack — posting messages (unofficial server uses mark, which is OK)
  # The official Slack MCP doesn't have a post tool, but guard it if added
  mcp__slack-unofficial-epochai__conversations_mark)
    # Marking as read is low-risk, allow it
    exit 0
    ;;

  # Twitter — creating tweets
  mcp__twitterapi-io__create_tweet)
    deny "posts a tweet"
    ;;

  # Google Drive — deleting files
  mcp__google-workspace-epoch__drive_delete_file|\
  mcp__google-docs-personal__deleteFile|\
  mcp__google-docs-personal__deleteComment|\
  mcp__google-docs-personal__deleteSheet)
    deny "deletes a shared resource"
    ;;

  # Google Calendar — deleting events
  mcp__google-workspace-epoch__delete_calendar_event|\
  mcp__google-calendar-personal__delete-event|\
  mcp__claude_ai_Google_Calendar__gcal_delete_event)
    deny "deletes a calendar event"
    ;;

  *)
    exit 0
    ;;
esac
