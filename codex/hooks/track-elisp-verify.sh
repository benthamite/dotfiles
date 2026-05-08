#!/bin/bash
# PostToolUse hook: manage the post-commit verification marker.
#
# After a successful `git commit` that includes .el files, creates a
# marker signaling that live Emacs verification is needed.
# After an `emacsclient -e` command, clears the marker.
#
# Works in tandem with require-elisp-verify-after-commit.sh which
# blocks subsequent commands until the marker is cleared.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-hook-json.sh
source "$SCRIPT_DIR/lib-codex-hook-json.sh"

INPUT=$(cat)

COMMAND=$(codex_tool_input_field "$INPUT" command)
SESSION_ID=$(codex_session_id "$INPUT")
EXIT_CODE=$(codex_hook_jq "$INPUT" '
  .tool_output.exitCode //
  .tool_output.exit_code //
  codex_tool_response.exitCode //
  codex_tool_response.exit_code //
  "0"
')

MARKER="/tmp/claude-elisp-verify-needed-${SESSION_ID}"

# On successful git commit with .el files: set marker
if echo "$COMMAND" | grep -qE '\bgit\s+commit\b'; then
  if [ "$EXIT_CODE" = "0" ]; then
    # Inspect the repo that the committed command actually targeted, not the
    # hook process cwd.
    # shellcheck source=lib-repo-root.sh
    source "$SCRIPT_DIR/lib-repo-root.sh"
    if [ -z "$REPO_ROOT" ]; then
      exit 0
    fi

    # After a successful commit, the staged files are now committed.
    # Check the just-committed files via git diff-tree.
    COMMITTED=$(git diff-tree --no-commit-id --name-only -r HEAD 2>/dev/null || true)
    HAS_ELISP=false
    if [ -n "$COMMITTED" ]; then
      while IFS= read -r file; do
        case "$file" in
          *.el)
            # Skip test files — they don't need live verification
            case "$file" in
              test/*|tests/*|*-test.el|*-tests.el) ;;
              *) HAS_ELISP=true; break ;;
            esac
            ;;
        esac
      done <<< "$COMMITTED"
    fi
    if [ "$HAS_ELISP" = true ]; then
      touch "$MARKER"
    fi
  fi
fi

# On emacsclient -e: clear marker
if echo "$COMMAND" | grep -qE '\bemacsclient\s.*-e\b|\bemacsclient\s+-e\b'; then
  if [ "$EXIT_CODE" = "0" ] && [ -f "$MARKER" ]; then
    rm -f "$MARKER"
  fi
fi

# Clean up stale markers from old sessions (>2 hours)
find /tmp -maxdepth 1 -name 'claude-elisp-verify-needed-*' -mmin +120 -delete 2>/dev/null || true

exit 0
