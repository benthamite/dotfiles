#!/bin/bash
# PreToolUse hook: deny-by-default guard for MCP tools.
#
# Posture: every MCP tool is BLOCKED (exit 2 → user confirmation required)
# unless it matches an explicit read-only allowlist. This ensures new MCP
# servers and tools are guarded automatically without needing to enumerate
# every dangerous action.
#
# Read-only allowlist patterns: read_*, list_*, get_*, search_*, find_*,
# *_read*, *_list*, *_get*, *_search*, *_find*, *_context*, *_overview*,
# *_history*, *_stats*, *_info*, *_check*, plus per-server tool names.
#
# Matcher: mcp__

set -euo pipefail

INPUT=$(cat)

TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')

# Only check MCP tools
echo "$TOOL_NAME" | grep -q '^mcp__' || exit 0

# --- Read-only allowlist ---
# Tools matching these patterns are considered safe (read-only) and pass through.
# Everything else requires user confirmation (exit 2).

# 1. Strip the mcp__<server>__ prefix to get the bare tool name for pattern matching.
bare_tool="${TOOL_NAME#mcp__}"       # remove first mcp__
bare_tool="${bare_tool#*__}"          # remove <server>__

# 2. Match read-only patterns against the bare tool name.
#    We use a case statement with glob patterns for clarity and speed.
case "$bare_tool" in
  # Patterns: <verb>_<rest> (prefix match)
  read_*|list_*|get_*|search_*|find_*)
    exit 0
    ;;

  # Patterns: <rest>_<verb><rest> (contains match)
  *_read|*_read_*|*_list|*_list_*|*_get|*_get_*|*_search|*_search_*|*_find|*_find_*)
    exit 0
    ;;

  # Specific substring patterns for information-retrieval tools
  # Note: *doc* is intentionally excluded — too broad (matches docs_insert_text).
  # Tools like readDocument, getDocumentInfo, docs_get_* are covered by specific entries below.
  *_context*|*_overview*|*_history*|*_stats*|*_info*|*_check*)
    exit 0
    ;;

  # Chrome browser read-only tools (specific prefixes)
  tabs_context*|read_console*|read_network*|read_page*|get_page*|shortcuts_list|find)
    exit 0
    ;;

  # Home Assistant read-only tools
  GetDateTime|GetLiveContext|todo_get_items)
    exit 0
    ;;

  # Slack low-risk: marking as read and listing
  conversations_mark)
    exit 0
    ;;

  # Google Calendar read-only tools that don't match generic patterns
  get-current-time|get-freebusy|list-calendars|list-colors|list-events|search-events|get-event)
    exit 0
    ;;

  # Google Calendar read-only tools (underscore variant)
  gcal_list_calendars|gcal_list_events|gcal_get_event|gcal_find_meeting_times|gcal_find_my_free_time)
    exit 0
    ;;

  # Gmail read-only tools (label/unlabel are low-risk organizational actions)
  label_message|label_thread|unlabel_message|unlabel_thread|list_drafts|list_labels)
    exit 0
    ;;

  # Google Sheets/Docs read-only tools that use "read" in a non-prefix position
  readDocument|readSpreadsheet|readCellFormat|sheets_read_range)
    exit 0
    ;;

  # Google Workspace read-only tools
  drive_read_file_content|drive_list_shared_drives|drive_search_files)
    exit 0
    ;;

  # Google Workspace presentation read-only
  get_presentation|get_slides)
    exit 0
    ;;

  # Google Workspace Gmail read-only
  query_gmail_emails|gmail_get_message_details|gmail_get_attachment_content)
    exit 0
    ;;

  # Workspace calendar read-only
  calendar_get_events|calendar_get_event_details)
    exit 0
    ;;

  # Workspace docs read-only
  docs_get_content_as_markdown|docs_get_document_metadata)
    exit 0
    ;;

  # Google Docs personal read-only tools that don't match generic patterns
  listComments|listDocuments|listDriveFiles|listFolderContents|listSpreadsheets|listTables|listTabs)
    exit 0
    ;;
  getComment|getDocumentInfo|getFolderInfo|getSpreadsheetInfo|getTable)
    exit 0
    ;;
  searchDocuments|searchDriveFiles)
    exit 0
    ;;
  downloadFile)
    exit 0
    ;;

  # Slack read-only tools (camelCase and snake_case variants)
  channels_list|conversations_history|conversations_replies|conversations_search_messages|conversations_unreads)
    exit 0
    ;;
  usergroups_list|usergroups_me|users_search)
    exit 0
    ;;

  # Twitter API read-only tools
  get_tweet_by_id|get_tweet_replies|get_user_by_id|get_user_by_username|get_user_followers|get_user_following|get_user_tweets|search_tweets|search_users|login_user)
    exit 0
    ;;

  # Ahrefs — entire server is read-only analytics; match common prefixes
  doc|site-explorer-*|site-audit-*|keywords-explorer-*|rank-tracker-*|web-analytics-*|brand-radar-*|gsc-*|serp-overview|batch-analysis|public-crawler-*|subscription-info-*|management-*)
    exit 0
    ;;

  # Anna's Archive — search and download are read-only research tools
  article_search|article_download|book_search|book_download)
    exit 0
    ;;

  # Home Assistant manage-accounts (Google Calendar) is informational
  manage-accounts)
    exit 0
    ;;

  # --- Default: BLOCK ---
  *)
    exit 2
    ;;
esac
