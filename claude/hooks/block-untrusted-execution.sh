#!/bin/bash
# Static routing checks for recognizable untrusted Bash execution.
set -euo pipefail
hook_bootstrap_complete=0
trap 'status=$?; if [ "$status" -ne 0 ] || [ "$hook_bootstrap_complete" -ne 1 ]; then printf "%s\n" "Security hook failed; tool execution denied." >&2; exit 2; fi' EXIT
SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
hook_bootstrap_complete=1
INPUT=$(cat)
TOOL=$(printf '%s' "$INPUT" | jq -er '.tool_name | strings')
[ "$TOOL" = "Bash" ] || exit 0
printf '%s' "$INPUT" | jq -ce --arg cwd "$PWD" '
  [.tool_input | {cmd: .command, workdir: (.workdir // $cwd)}]
' | python3 -I "$SCRIPT_DIR/lib-untrusted-execution.py"
