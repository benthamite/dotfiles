#!/bin/bash
# Codex adapter for shared Claude hook scripts.
#
# Codex and Claude use similar lifecycle concepts but different JSON payloads.
# This adapter normalizes the Codex payload into the Claude-shaped fields that
# the shared scripts in claude/hooks/ already consume, then passes hook output
# and exit status through unchanged.

set -euo pipefail

if [ "$#" -lt 1 ]; then
  echo "Usage: run-claude-hook.sh HOOK [ARG...]" >&2
  exit 2
fi

HOOK="$1"
shift

INPUT=$(cat)
if [ -z "$INPUT" ]; then
  INPUT='{}'
fi

NORMALIZED=$(printf '%s' "$INPUT" | jq '
  def object_or_empty($value):
    if ($value | type) == "object" then $value
    elif $value == null then {}
    else {"value": $value}
    end;

  def response_object:
    .tool_response? as $response |
    if ($response | type) == "object" then $response
    elif ($response | type) == "string" then
      ($response | fromjson? // {"output": $response})
    else {}
    end;

  . as $root
  | . + {
      "hook_event_name": ($root.hook_event_name // $root.hookEventName // ""),
      "tool_name": ($root.tool_name // $root.toolName // ""),
      "tool_input": object_or_empty($root.tool_input)
    }
  | .tool_output = (
      if ($root.tool_output? | type) == "object" then $root.tool_output
      else
        response_object as $response |
        {
          "stdout": ($response.stdout // $response.output // $response.text // ""),
          "stderr": ($response.stderr // ""),
          "exit_code": (
            $response.exit_code
            // $response.exitCode
            // $response.status
            // null
          )
        }
      end
    )
')

printf '%s' "$NORMALIZED" | "$HOOK" "$@"
