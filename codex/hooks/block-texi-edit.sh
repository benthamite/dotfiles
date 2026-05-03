#!/usr/bin/env bash
# PreToolUse hook: block editing .texi files (auto-generated from .org via ox-texinfo)

set -euo pipefail

# shellcheck source=lib-codex-paths.sh
source "$(dirname "$0")/lib-codex-paths.sh"

INPUT=$(cat)

while IFS= read -r FILE_PATH; do
  if [[ "$FILE_PATH" == *.texi ]]; then
    jq -n '{
      "hookSpecificOutput": {
        "hookEventName": "PreToolUse",
        "permissionDecision": "deny",
        "permissionDecisionReason": "BLOCKED: .texi files are auto-generated from .org via ox-texinfo. Edit the corresponding .org file instead, then regenerate."
      }
    }'
    exit 0
  fi
done < <(codex_changed_paths "$INPUT")
