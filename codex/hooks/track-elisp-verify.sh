#!/bin/bash
# PostToolUse hook: manage the post-commit verification marker.
#
# After a successful `git commit` that includes .el files, creates a
# marker signaling that live Emacs verification is needed.
# After a non-status `emacsclient -e` (or equivalent `--eval`) command, clears
# the marker.
#
# Works in tandem with require-elisp-verify-after-commit.sh which
# blocks subsequent commands until the marker is cleared.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-hook-json.sh
source "$SCRIPT_DIR/lib-codex-hook-json.sh"

INPUT=$(cat)

COMMAND=$(codex_shell_command "$INPUT")
TOOL_NAME=$(codex_tool_name "$INPUT")
SESSION_ID=$(codex_session_id "$INPUT")
EXIT_CODE=$(codex_hook_jq "$INPUT" '
  .tool_output.exitCode //
  .tool_output.exit_code //
  codex_tool_response.exitCode //
  codex_tool_response.exit_code //
  "0"
')

MARKER="/tmp/claude-elisp-verify-needed-${SESSION_ID}"

resolve_command_repo() (
  local completed_command="$1"
  local context_dir="$2"

  COMMAND="$completed_command"
  unset REPO_CONTEXT_DIR
  if [ -n "$context_dir" ]; then
    REPO_CONTEXT_DIR="$context_dir"
  fi
  # shellcheck source=lib-repo-root.sh
  source "$SCRIPT_DIR/lib-repo-root.sh"
  printf '%s' "$REPO_ROOT"
)

commit_contains_elisp() {
  local repo_root="$1"
  local committed file

  committed=$(git -C "$repo_root" diff-tree --no-commit-id --name-only -r HEAD 2>/dev/null || true)
  [ -n "$committed" ] || return 1
  while IFS= read -r file; do
    case "$file" in
      *.el)
        case "$file" in
          test/*|tests/*|*-test.el|*-tests.el) ;;
          *) return 0 ;;
        esac
        ;;
    esac
  done <<< "$committed"
  return 1
}

process_completed_command() {
  local completed_command="$1"
  local context_dir="$2"
  local commit_count eval_count repo_root

  commit_count=$(printf '%s' "$completed_command" | codex_git_commit_count)
  if [ "$commit_count" -gt 0 ]; then
    repo_root=$(resolve_command_repo "$completed_command" "$context_dir")
    if [ -n "$repo_root" ] && commit_contains_elisp "$repo_root"; then
      touch "$MARKER"
    fi
  fi

  # Only an executable-position live evaluation clears the marker. Quoted
  # arguments, commit messages, and comments are inert.
  eval_count=$(printf '%s' "$completed_command" | codex_emacsclient_eval_count)
  if [ "$eval_count" -gt 0 ] && [ -f "$MARKER" ]; then
    rm -f "$MARKER"
  fi
}

if [ "$TOOL_NAME" = "functions.exec" ]; then
  # The outer result does not expose a status for each nested shell call. Only
  # use it to conservatively set the marker; exact successful nested
  # emacsclient events clear the marker through their own PostToolUse event.
  commit_repos=()
  while IFS= read -r -d '' context; do
    if [ "$(printf '%s' "$context" | jq -r '.ambiguous')" = "true" ]; then
      # Dynamic or overridable fields cannot be attributed safely from the
      # outer wrapper. Retain verification until an exact direct event clears
      # it instead of guessing that the call was unrelated to an Elisp commit.
      touch "$MARKER"
      continue
    fi

    nested_command=$(printf '%s' "$context" | jq -r '.cmd')
    commit_count=$(printf '%s' "$nested_command" | codex_git_commit_count)
    if [ "$commit_count" -eq 0 ]; then
      continue
    fi
    if [ "$commit_count" -gt 1 ]; then
      # One final HEAD cannot prove what an earlier commit in this same shell
      # command contained.
      touch "$MARKER"
    fi

    nested_workdir=$(printf '%s' "$context" | jq -r '.workdir // empty')
    if [ -z "$nested_workdir" ] || [ ! -d "$nested_workdir" ]; then
      # Without an attributable target, requiring verification is safer than
      # inspecting the hook cwd and guessing which repository was committed.
      touch "$MARKER"
      continue
    fi

    repo_root=$(resolve_command_repo "$nested_command" "$nested_workdir")
    if [ -z "$repo_root" ]; then
      touch "$MARKER"
      continue
    fi

    repo_seen=false
    for seen_repo in "${commit_repos[@]-}"; do
      if [ "$seen_repo" = "$repo_root" ]; then
        repo_seen=true
        break
      fi
    done
    if [ "$repo_seen" = true ]; then
      # HEAD only describes the last commit, so repeated commits to one repo
      # cannot prove that an earlier commit contained no production Elisp.
      touch "$MARKER"
    else
      commit_repos+=("$repo_root")
    fi

    if commit_contains_elisp "$repo_root"; then
      touch "$MARKER"
    fi
  done < <(printf '%s' "$COMMAND" | codex_nested_exec_contexts)
elif [ "$EXIT_CODE" = "0" ]; then
  process_completed_command "$COMMAND" ""
fi

# Clean up stale markers from old sessions (>2 hours)
find /tmp -maxdepth 1 -name 'claude-elisp-verify-needed-*' -mmin +120 -delete 2>/dev/null || true

exit 0
