#!/bin/bash
# PreToolUse hook: block direct edits to the elpaca clone of dotfiles.
#
# The canonical dotfiles live in ~/My Drive/dotfiles/. Elpaca keeps a
# separate git clone at elpaca/sources/dotfiles/ that syncs via commits.
# Editing the clone directly is always wrong — the changes get overwritten
# on the next pull. This hook forces edits to the canonical location.
#
# Git operations (pull, merge, elpaca-fetch) are unaffected because they
# use the Bash tool, not Edit/Write.

set -euo pipefail

INPUT=$(cat)

FILE_PATH=$(printf '%s' "$INPUT" | jq -r '.tool_input.file_path // empty')

[[ "$FILE_PATH" == *elpaca/sources/dotfiles/* ]] || exit 0

CANONICAL=${FILE_PATH##*elpaca/sources/dotfiles/}

jq -n --arg canonical "$CANONICAL" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": ("BLOCKED: Do not edit the elpaca clone of dotfiles — it syncs via git and your changes will be overwritten.\n\nEdit the canonical file instead:\n  ~/My Drive/dotfiles/" + $canonical + "\n\nThen commit and push in the dotfiles repo. The elpaca clone will pick up the change on next sync.")
  }
}'
