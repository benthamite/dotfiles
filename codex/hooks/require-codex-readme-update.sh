#!/usr/bin/env bash
# PreToolUse hook: block git commit when Codex integration files are staged
# but codex/README.org is not.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-hook-json.sh
source "$SCRIPT_DIR/lib-codex-hook-json.sh"

INPUT=$(cat)
COMMAND=$(codex_shell_command "$INPUT")

if ! echo "$COMMAND" | grep -qE '\bgit\s+commit\b'; then
  exit 0
fi

# Inspect staged files in the repo targeted by the command, not the hook cwd.
# shellcheck source=lib-repo-root.sh
source "$SCRIPT_DIR/lib-repo-root.sh"
if [ -z "$REPO_ROOT" ]; then
  exit 0
fi

DOTFILES_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)
if [ "$(cd -- "$REPO_ROOT" && pwd)" != "$DOTFILES_ROOT" ]; then
  exit 0
fi

# shellcheck source=lib-staged-files.sh
source "$SCRIPT_DIR/lib-staged-files.sh"

HAS_CODEX_CHANGES=false
HAS_README=false

if [ -n "$STAGED" ]; then
  while IFS= read -r file; do
    case "$file" in
      codex/README.org)
        HAS_README=true
        ;;
      AGENTS.md|.codex/*|codex/*|ai-config-sync.json|bin/ai-config-sync)
        HAS_CODEX_CHANGES=true
        ;;
    esac
  done <<< "$STAGED"
fi

if [ "$HAS_CODEX_CHANGES" = false ] && echo "$COMMAND" | grep -qE '\bgit\s+add\b'; then
  # The staged-file pass above is authoritative for .codex/ paths.  Do not
  # infer from command text here: project-local commits commonly stage
  # `.codex/skills/...`, and Codex hook payloads do not always include enough
  # cwd/workdir context for lib-repo-root.sh to distinguish those commands
  # before `git add` has run.
  if echo "$COMMAND" | grep -qE '(^|[[:space:]])(AGENTS\.md|codex/|ai-config-sync\.json|bin/ai-config-sync)'; then
    HAS_CODEX_CHANGES=true
  fi
fi

if [ "$HAS_README" = false ] && echo "$COMMAND" | grep -qF 'codex/README.org'; then
  HAS_README=true
fi

if [ "$HAS_CODEX_CHANGES" = false ] || [ "$HAS_README" = true ]; then
  exit 0
fi

jq -n '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": "BLOCKED: You are committing Codex integration changes without updating codex/README.org. Update the README to reflect the change, then try again."
  }
}'
