#!/bin/bash
# PreToolUse hook: block git commit when .el files are staged but the
# package manual is not also staged.
#
# Applies to repos that have either:
#   - a doc/ directory (at root or nested) → expects a doc/*.org file staged
#   - a README.org at the root             → expects README.org staged
#
# NOTE: README.md is the GitHub-facing intro, NOT the manual. It only
# needs updating when the high-level picture changes (new major features,
# new dependencies, changed installation, etc.). This hook enforces
# manual (README.org or doc/*.org) updates only.
#
# Reads JSON from stdin (Claude Code PreToolUse format).
# Outputs JSON with permissionDecision to allow or deny.

set -euo pipefail

INPUT=$(cat)

COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')

# Only intercept git commit commands
if ! echo "$COMMAND" | grep -qE '\bgit\s+commit\b'; then
  exit 0
fi

# Find the repo root; bail if not in a git repo
REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || true)
if [ -z "$REPO_ROOT" ]; then
  exit 0
fi

# Skip during rebase, merge, or cherry-pick — the commit content is
# predetermined, and the manual can be updated in a follow-up commit.
GIT_DIR="$REPO_ROOT/.git"
# Handle worktrees where .git is a file pointing to the real gitdir
if [ -f "$GIT_DIR" ]; then
  GIT_DIR=$(sed -n 's/^gitdir: //p' "$GIT_DIR")
fi
if [ -d "$GIT_DIR/rebase-merge" ] || [ -d "$GIT_DIR/rebase-apply" ] || \
   [ -f "$GIT_DIR/MERGE_HEAD" ] || [ -f "$GIT_DIR/CHERRY_PICK_HEAD" ]; then
  exit 0
fi

# Determine which documentation pattern applies
HAS_DOC_DIR=false
HAS_README_ORG=false

# Check for doc/ at root or nested (up to 3 levels deep)
if [ -d "$REPO_ROOT/doc" ] || \
   compgen -G "$REPO_ROOT/*/doc" > /dev/null 2>&1 || \
   compgen -G "$REPO_ROOT/*/*/doc" > /dev/null 2>&1 || \
   compgen -G "$REPO_ROOT/*/*/*/doc" > /dev/null 2>&1; then
  HAS_DOC_DIR=true
fi
if [ -f "$REPO_ROOT/README.org" ]; then
  HAS_README_ORG=true
fi

# Only apply in repos that have some form of manual
if [ "$HAS_DOC_DIR" = false ] && [ "$HAS_README_ORG" = false ]; then
  exit 0
fi

# Check staged files
STAGED=$(git diff --cached --name-only 2>/dev/null || true)

HAS_EL=false
HAS_DOC_ORG=false
HAS_README_ORG_STAGED=false
if [ -n "$STAGED" ]; then
  while IFS= read -r file; do
    case "$file" in
      *.el)
        HAS_EL=true
        ;;
      doc/*.org | */doc/*.org)
        HAS_DOC_ORG=true
        ;;
      README.org)
        HAS_README_ORG_STAGED=true
        ;;
    esac
  done <<< "$STAGED"
fi

# Also catch `git add ... && git commit` in a single bash command.
# When staging and committing happen in one call, git diff --cached
# sees nothing yet at hook-fire time, so scan the command string too.
if echo "$COMMAND" | grep -qE '\bgit\s+add\b'; then
  if [ "$HAS_EL" = false ]; then
    if echo "$COMMAND" | grep -qE '\.el[[:space:]|&;]|\.el$'; then
      HAS_EL=true
    fi
  fi
  if [ "$HAS_DOC_ORG" = false ] && echo "$COMMAND" | grep -qE '(^|/)doc/[^ ]*\.org'; then
    HAS_DOC_ORG=true
  fi
  if [ "$HAS_README_ORG_STAGED" = false ] && echo "$COMMAND" | grep -qF 'README.org'; then
    HAS_README_ORG_STAGED=true
  fi
fi

if [ "$HAS_EL" = false ]; then
  exit 0
fi

# Accept if any documentation file is staged:
# - doc/*.org (for repos with a doc/ directory)
# - README.org (for repos that use README.org as the manual)
if [ "$HAS_DOC_ORG" = true ] || [ "$HAS_README_ORG_STAGED" = true ]; then
  exit 0
fi

# Block the commit
if [ "$HAS_DOC_DIR" = true ]; then
  REASON="BLOCKED: Elisp files are staged but no doc/*.org file is included. Update the org manual in the relevant doc/ directory to reflect your changes, then try again. Use /doc-elisp to generate or update documentation."
else
  REASON="BLOCKED: Elisp files are staged but README.org is not included. Update the manual (README.org) to reflect your changes, then try again. Use /doc-elisp to update the manual. README.md is the GitHub intro, not the manual — only update it when the high-level picture changes."
fi

jq -n --arg reason "$REASON" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": $reason
  }
}'
