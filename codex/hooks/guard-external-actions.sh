#!/bin/bash
# PreToolUse hook: approval-by-default guard for MCP tools (Codex copy).
#
# Every MCP tool call is classified as exactly one of:
#   safe            → allowed silently
#   needs_approval  → escalated to the user
#   deny            → blocked outright
# Anything unrecognised is needs_approval, so a newly added MCP server is
# guarded without having to enumerate its tools first.
#
# Enforcement is per host, because the two hosts do not agree, so this hook is
# recorded as paired-dispatched rather than paired in ai-config-sync.json:
#   Claude Code      exit 0 + permissionDecision ask/deny
#   Codex 0.146.0    has no "ask"; needs_approval becomes deny + instruction
#                    (this copy). Emitting "ask" here is worse than useless:
#                    Codex rejects it as an unsupported permissionDecision and
#                    then lets the call through.
#
# Both copies must emit structured decision JSON. Until 2026-07-31 this hook
# used a bare `exit 2` with nothing on stderr, which was wrong in both hosts:
# Claude Code showed the agent an unexplained "hook error: No stderr output",
# and Codex treats exit 2 with an empty stderr as a *failed* hook and lets the
# call proceed — so the guard blocked nothing at all under Codex. Never
# reintroduce a bare exit 2 here; see tests/test_security_hook_hardening.py.
#
# Required matcher: mcp__.* (or ^mcp__). A bare mcp__ matcher is an exact
# tool-name match in current Claude Code and Codex releases, so it never fires.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-hook-json.sh
source "$SCRIPT_DIR/lib-codex-hook-json.sh"

INPUT=$(cat)

TOOL_NAME=$(codex_tool_name "$INPUT")

# Only check MCP tools
echo "$TOOL_NAME" | grep -q '^mcp__' || exit 0

# Host-specific payload access. Codex wraps tool_input as a JSON *string* on
# some events, so its copy normalises through lib-codex-hook-json.sh; keeping
# the accessor separate lets every classifier below stay byte-identical.
tool_input_json() {
  printf '%s' "$INPUT" | jq -c "${CODEX_HOOK_JQ_DEFS}
codex_tool_input"
}

# --- host-specific decision emitters -----------------------------------------

emit_deny() {
  local label="$1" detail="$2"
  jq -n --arg label "$label" --arg detail "$detail" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": ("BLOCKED: " + $label + " — " + $detail)
    }
  }'
  exit 0
}

emit_needs_approval() {
  local label="$1" detail="$2"
  jq -n --arg label "$label" --arg detail "$detail" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": ("BLOCKED: " + $label + " — " + $detail + "\n\nCodex hooks do not have Claude Code ask decision semantics here. Ask the user before making this call.")
    }
  }'
  exit 0
}

# --- classification (identical in both copies) --------------------------------

# Combine two verdicts, keeping the more restrictive one.
worse_verdict() {
  case "$1:$2" in
    deny:*|*:deny) echo deny ;;
    needs_approval:*|*:needs_approval) echo needs_approval ;;
    *) echo safe ;;
  esac
}

# Browser navigation. http(s) and bare hosts are ordinary page loads. The
# denied schemes reach past the page into the browser and the local machine:
# javascript: executes script without going through javascript_tool's approval,
# file: reads local disk, and chrome:/devtools: reach browser settings.
classify_navigate() {
  local url lowered
  url=$(printf '%s' "$1" | jq -r '.url // empty')
  if [ -z "$url" ]; then
    echo needs_approval
    return
  fi
  lowered=$(printf '%s' "$url" | tr '[:upper:]' '[:lower:]')
  case "$lowered" in
    back|forward) echo safe ;;
    http://*|https://*) echo safe ;;
    javascript:*|data:*|file:*|about:*|blob:*|view-source:*) echo deny ;;
    chrome:*|chrome-extension:*|chrome-search:*|devtools:*|filesystem:*) echo deny ;;
    *://*) echo needs_approval ;;
    *) echo safe ;;
  esac
}

# The computer tool covers both observation and input. Looking at a page is
# safe; acting on it can submit forms, follow irreversible controls, or type
# into fields, so it goes to the user.
classify_computer() {
  local action
  action=$(printf '%s' "$1" | jq -r '.action // empty')
  case "$action" in
    screenshot|wait|scroll|zoom|hover|scroll_to) echo safe ;;
    left_click|right_click|double_click|triple_click) echo needs_approval ;;
    type|key|left_click_drag) echo needs_approval ;;
    *) echo needs_approval ;;
  esac
}

# browser_batch runs a list of {name, input} items, each of which is a real
# browser call. Classify every item and keep the most restrictive verdict, or a
# batch could smuggle a click past a guard that only looked at the wrapper.
classify_batch() {
  local input="$1" count index name sub verdict worst
  count=$(printf '%s' "$input" | jq -r '(.actions // []) | length')
  if ! [ "$count" -gt 0 ] 2>/dev/null; then
    echo needs_approval
    return
  fi
  worst=safe
  index=0
  while [ "$index" -lt "$count" ]; do
    name=$(printf '%s' "$input" | jq -r --argjson i "$index" '.actions[$i].name // empty')
    sub=$(printf '%s' "$input" | jq -c --argjson i "$index" '.actions[$i].input // {}')
    if [ -z "$name" ]; then
      echo needs_approval
      return
    fi
    # The tool itself forbids nesting; a nested batch is a malformed call.
    if [ "$name" = "browser_batch" ]; then
      echo deny
      return
    fi
    verdict=$(classify_browser "$name" "$sub")
    worst=$(worse_verdict "$worst" "$verdict")
    index=$((index + 1))
  done
  echo "$worst"
}

classify_browser() {
  local tool="$1" input="$2"
  case "$tool" in
    # Session plumbing and tab management: needed to reach a page at all.
    list_connected_browsers|select_browser|switch_browser) echo safe ;;
    tabs_context_mcp|tabs_create_mcp|tabs_close_mcp) echo safe ;;
    # Observation.
    read_page|get_page_text|find|read_console_messages|read_network_requests) echo safe ;;
    shortcuts_list|resize_window|gif_creator) echo safe ;;
    # Input-dependent.
    navigate) classify_navigate "$input" ;;
    computer) classify_computer "$input" ;;
    browser_batch) classify_batch "$input" ;;
    # Forms, uploads, arbitrary script, and configured automations.
    form_input|file_upload|upload_image|javascript_tool|shortcuts_execute) echo needs_approval ;;
    *) echo needs_approval ;;
  esac
}

# Non-browser MCP servers are classified by tool name alone: the servers are
# too varied to model their inputs, and a read-only name is the only signal
# available that does not need per-server knowledge.
classify_generic() {
  local bare="$1" normalized

  # Reject mixed names that contain a mutating verb before considering
  # read-only prefixes. Without this check, names such as get_or_create_*
  # and check_and_update_* are incorrectly admitted by get_* and *_check*.
  normalized=$(
    printf '%s' "$bare" \
      | sed -E 's/([[:lower:][:digit:]])([[:upper:]])/\1_\2/g; s/[^[:alnum:]]+/_/g' \
      | tr '[:upper:]' '[:lower:]'
  )
  normalized="_${normalized}_"
  case "$normalized" in
    *_create_*|*_update_*|*_delete_*|*_write_*|*_send_*|*_insert_*|*_remove_*|*_set_*|*_add_*|*_upload_*|*_publish_*|*_execute_*|*_trigger_*|*_start_*|*_stop_*|*_enable_*|*_disable_*|*_approve_*|*_reject_*|*_archive_*|*_unarchive_*|*_invite_*|*_move_*|*_copy_*|*_rename_*|*_edit_*|*_modify_*|*_restore_*|*_revoke_*|*_grant_*|*_submit_*|*_reply_*)
      echo needs_approval
      return
      ;;
  esac

  case "$bare" in
    # Patterns: <verb>_<rest> (prefix match)
    read_*|list_*|get_*|search_*|find_*) echo safe ;;

    # Patterns: <rest>_<verb><rest> (contains match)
    *_read|*_read_*|*_list|*_list_*|*_get|*_get_*|*_search|*_search_*|*_find|*_find_*) echo safe ;;

    # Specific substring patterns for information-retrieval tools.
    # Note: *doc* is intentionally excluded — too broad (matches docs_insert_text).
    *_context*|*_overview*|*_history*|*_stats*|*_info*|*_check*) echo safe ;;

    # Slack low-risk: marking as read and listing
    conversations_mark) echo safe ;;

    # Google Calendar read-only tools that don't match generic patterns
    get-current-time|get-freebusy|list-calendars|list-colors|list-events|search-events|get-event) echo safe ;;

    # Google Calendar read-only tools (underscore variant)
    gcal_list_calendars|gcal_list_events|gcal_get_event|gcal_find_meeting_times|gcal_find_my_free_time) echo safe ;;

    # Gmail read-only tools (label/unlabel are low-risk organizational actions)
    label_message|label_thread|unlabel_message|unlabel_thread|list_drafts|list_labels) echo safe ;;

    # Google Sheets/Docs read-only tools that use "read" in a non-prefix position
    readDocument|readSpreadsheet|readCellFormat|sheets_read_range) echo safe ;;

    # Google Workspace read-only tools
    drive_read_file_content|drive_list_shared_drives|drive_search_files) echo safe ;;

    # Google Workspace presentation read-only
    get_presentation|get_slides) echo safe ;;

    # Google Workspace Gmail read-only
    query_gmail_emails|gmail_get_message_details|gmail_get_attachment_content) echo safe ;;

    # Workspace calendar read-only
    calendar_get_events|calendar_get_event_details) echo safe ;;

    # Workspace docs read-only
    docs_get_content_as_markdown|docs_get_document_metadata) echo safe ;;

    # Google Docs personal read-only tools that don't match generic patterns
    listComments|listDocuments|listDriveFiles|listFolderContents|listSpreadsheets|listTables|listTabs) echo safe ;;
    getComment|getDocumentInfo|getFolderInfo|getSpreadsheetInfo|getTable) echo safe ;;
    searchDocuments|searchDriveFiles) echo safe ;;
    downloadFile) echo safe ;;

    # Slack read-only tools (camelCase and snake_case variants)
    channels_list|conversations_history|conversations_replies|conversations_search_messages|conversations_unreads) echo safe ;;
    usergroups_list|usergroups_me|users_search) echo safe ;;

    # Twitter API read-only tools
    get_tweet_by_id|get_tweet_replies|get_user_by_id|get_user_by_username|get_user_followers|get_user_following|get_user_tweets|search_tweets|search_users|login_user) echo safe ;;

    # Ahrefs — only docs and the free subscription-info endpoint are safe.
    # Paid read-only analytics still consume shared API units and must go
    # through ahrefs-api-guard or project code with an equivalent quota gate.
    doc|subscription-info-*) echo safe ;;

    # Anna's Archive — search and download are read-only research tools
    article_search|article_download|book_search|book_download) echo safe ;;

    # Home Assistant manage-accounts (Google Calendar) is informational
    manage-accounts) echo safe ;;

    *) echo needs_approval ;;
  esac
}

# --- dispatch -----------------------------------------------------------------

BARE_TOOL="${TOOL_NAME#mcp__}"      # remove first mcp__
SERVER="${BARE_TOOL%%__*}"          # <server>
BARE_TOOL="${BARE_TOOL#*__}"        # remove <server>__

TOOL_INPUT=$(tool_input_json)

if [ "$SERVER" = "claude-in-chrome" ]; then
  VERDICT=$(classify_browser "$BARE_TOOL" "$TOOL_INPUT")
else
  VERDICT=$(classify_generic "$BARE_TOOL")
fi

case "$VERDICT" in
  safe)
    exit 0
    ;;
  deny)
    emit_deny "$TOOL_NAME is not permitted" \
      "This call reaches outside the page into the browser or the local machine. Use an ordinary http(s) navigation, or ask the user to perform it themselves."
    ;;
  *)
    emit_needs_approval "$TOOL_NAME is not on the read-only allowlist" \
      "It may change state outside this session, so it needs the user's approval. If a read-only tool is being escalated by mistake, add it to the allowlist in guard-external-actions.sh (both copies) rather than working around this."
    ;;
esac
