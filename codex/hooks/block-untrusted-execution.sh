#!/bin/bash
# Static routing checks, including each nested functions.exec shell context.
set -euo pipefail
hook_bootstrap_complete=0
trap 'status=$?; if [ "$status" -ne 0 ] || [ "$hook_bootstrap_complete" -ne 1 ]; then printf "%s\n" "Security hook failed; tool execution denied." >&2; exit 2; fi' EXIT
SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
source "$SCRIPT_DIR/lib-codex-hook-json.sh" || exit 2
hook_bootstrap_complete=1
INPUT=$(cat)
TOOL=$(codex_tool_name "$INPUT")
[ -n "$TOOL" ] || exit 2
codex_shell_tool_p "$TOOL" || exit 0
[ "$(codex_hook_jq "$INPUT" '(codex_tool_input | (.command // .cmd // .input) | type) == "string"')" = "true" ] || exit 2
[ "$(codex_hook_jq "$INPUT" '((codex_tool_input.workdir // .cwd // "") | type) == "string"')" = "true" ] || exit 2
COMMAND=$(codex_shell_command "$INPUT")
CWD=$(codex_hook_jq "$INPUT" 'codex_tool_input.workdir // .cwd // empty')
CWD=${CWD:-$PWD}
if [ "$TOOL" = "functions.exec" ]; then
  # Capture parser failure through pipefail rather than a process substitution.
  CONTEXTS=$(printf '%s' "$COMMAND" | codex_nested_exec_contexts | python3 -I -c '
import json,sys
print(json.dumps([json.loads(row) for row in sys.stdin.read().split("\0") if row]))')
  printf '%s' "$CONTEXTS" | jq -ce --arg cwd "$CWD" 'map(.workdir = (.workdir // $cwd))' |
    python3 -I "$SCRIPT_DIR/lib-untrusted-execution.py"
else
  printf '%s' "$INPUT" | jq -ce --arg cmd "$COMMAND" --arg cwd "$CWD" '[{cmd:$cmd, workdir:$cwd}]' |
    python3 -I "$SCRIPT_DIR/lib-untrusted-execution.py"
fi
