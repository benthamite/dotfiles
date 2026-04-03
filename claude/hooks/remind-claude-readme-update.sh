#!/bin/bash
# PostToolUse hook: after Write or Edit, if the target file resolves to
# a path under dotfiles/claude/, remind the agent to update README.org
# and commit in the dotfiles repo.
#
# This catches the case where the agent is working in project A (e.g. Epoch)
# but writes a file to dotfiles/claude/ via the ~/.claude/skills symlink.
# The existing require-readme-update.sh blocks commits in the dotfiles repo
# that omit README.org, but it can't fire if the agent never switches to
# the dotfiles repo. This hook ensures the agent is reminded immediately.

set -euo pipefail

INPUT=$(cat)

TOOL=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')

# Only fire on Write or Edit
case "$TOOL" in
  Write|Edit) ;;
  *) exit 0 ;;
esac

FILE_PATH=$(printf '%s' "$INPUT" | jq -r '.tool_input.file_path // empty')

if [ -z "$FILE_PATH" ]; then
  exit 0
fi

# Resolve symlinks to get the real path
REAL_PATH=$(python3 -c "import os,sys; print(os.path.realpath(sys.argv[1]))" "$FILE_PATH" 2>/dev/null || true)

if [ -z "$REAL_PATH" ]; then
  exit 0
fi

DOTFILES_CLAUDE="$HOME/My Drive/dotfiles/claude"
DOTFILES_CLAUDE_REAL=$(python3 -c "import os,sys; print(os.path.realpath(sys.argv[1]))" "$DOTFILES_CLAUDE" 2>/dev/null || true)

# Check if the file is under dotfiles/claude/
case "$REAL_PATH" in
  "$DOTFILES_CLAUDE_REAL"/*)
    ;;
  *)
    exit 0
    ;;
esac

# Skip if the file IS README.org (the agent is already updating it)
case "$REAL_PATH" in
  */README.org)
    exit 0
    ;;
esac

# Skip if we're already in the dotfiles repo (the commit hook will handle it)
REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || true)
DOTFILES_ROOT="$HOME/My Drive/dotfiles"
DOTFILES_ROOT_REAL=$(python3 -c "import os,sys; print(os.path.realpath(sys.argv[1]))" "$DOTFILES_ROOT" 2>/dev/null || true)

if [ "$REPO_ROOT" = "$DOTFILES_ROOT" ] || [ "$REPO_ROOT" = "$DOTFILES_ROOT_REAL" ]; then
  exit 0
fi

# Compute relative path for the message
REL_PATH="${REAL_PATH#$DOTFILES_CLAUDE_REAL/}"

cat <<EOF
REMINDER: You just modified claude/$REL_PATH (in the dotfiles repo, via symlink).
You MUST:
  1. Update claude/README.org to reflect this change.
  2. Commit in the dotfiles repo: cd "$DOTFILES_ROOT" && git add claude/ && git commit
The require-readme-update.sh hook will block the commit if README.org is not staged.
EOF
