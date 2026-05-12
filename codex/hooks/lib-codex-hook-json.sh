#!/usr/bin/env bash
# Shared JSON helpers for Codex hook payloads.

CODEX_HOOK_JQ_DEFS='
  def codex_tool_input:
    .tool_input? as $input |
    if ($input | type) == "object" then $input
    elif ($input | type) == "string" then
      ($input as $raw |
       (try ($raw | fromjson) catch {"command": $raw}) |
       if type == "object" then . else {"command": $raw} end)
    else {}
    end;

  def codex_tool_response:
    .tool_response? as $response |
    if ($response | type) == "object" then $response
    elif ($response | type) == "string" then
      ($response as $raw |
       (try ($raw | fromjson) catch {"output": $raw}) |
       if type == "object" then . else {"output": $raw} end)
    else {}
    end;
'

codex_hook_jq() {
  local input="$1"
  local filter="$2"
  printf '%s' "$input" | jq -r "${CODEX_HOOK_JQ_DEFS}
${filter}"
}

codex_tool_name() {
  local input="$1"
  codex_hook_jq "$input" '.tool_name // empty'
}

codex_session_id() {
  local input="$1"
  codex_hook_jq "$input" '.session_id // empty'
}

codex_tool_input_field() {
  local input="$1"
  local field="$2"
  printf '%s' "$input" | jq -r --arg field "$field" "${CODEX_HOOK_JQ_DEFS}
codex_tool_input[\$field] // empty"
}

codex_shell_command() {
  local input="$1"
  codex_hook_jq "$input" '
    codex_tool_input.command //
    codex_tool_input.cmd //
    codex_tool_input.input //
    empty
  '
}

codex_shell_tool_p() {
  local tool_name="$1"
  case "$tool_name" in
    Bash|exec_command|functions.exec_command) return 0 ;;
    *) return 1 ;;
  esac
}
