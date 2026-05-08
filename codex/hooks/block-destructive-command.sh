#!/bin/bash
# PreToolUse hook: block destructive shell commands that are hard to reverse.
#
# Enforces CLAUDE.md's "use trash instead of rm -rf" instruction with code,
# not just prose.
#
# Matcher: Bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-hook-json.sh
source "$SCRIPT_DIR/lib-codex-hook-json.sh"

INPUT=$(cat)

TOOL_NAME=$(codex_tool_name "$INPUT")
[ "$TOOL_NAME" != "Bash" ] && exit 0

CMD=$(codex_tool_input_field "$INPUT" command)
[ -z "$CMD" ] && exit 0

deny() {
  local label="$1"
  local suggestion="$2"
  jq -n --arg label "$label" --arg suggestion "$suggestion" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": ("BLOCKED: " + $label + ".\n\n" + $suggestion + "\n\nIf you are certain this is the right action, ask the user to run it manually with `!`.")
    }
  }'
  exit 0
}

ask() {
  local label="$1"
  local suggestion="$2"
  jq -n --arg label "$label" --arg suggestion "$suggestion" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": ("BLOCKED: " + $label + ".\n\n" + $suggestion + "\n\nCodex hooks do not have Claude Code ask decision semantics here. Ask the user before running this command.")
    }
  }'
  exit 0
}

# --- rm -rf / rm -r (should use trash) ---
if echo "$CMD" | grep -qE '\brm\s+(-[a-zA-Z]*r[a-zA-Z]*f|(-[a-zA-Z]*f[a-zA-Z]*r)|-rf|-fr)\b'; then
  deny "rm -rf detected" "Use 'trash' instead of 'rm -rf' to allow recovery."
fi

# --- git push --force / -f (dangerous to shared branches) ---
# --force-with-lease is the safe form (per CLAUDE.md, it's medium-risk on a
# verified user-owned feature branch) — allow it through.
if echo "$CMD" | grep -qE '\bgit\s+push\b' && \
   echo "$CMD" | grep -qE '(\s-f\b|\s--force\b)' && \
   ! echo "$CMD" | grep -qF -- '--force-with-lease'; then
  deny "git push --force detected" "Force-pushing can overwrite upstream history. Use --force-with-lease for branch-scoped pushes, or confirm with the user."
fi

# --- git clone (prevent cloning without approval) ---
if echo "$CMD" | grep -qE '\b(git\s+clone|gh\s+repo\s+clone)\b'; then
  ask "git clone detected" "Only clone repositories the user has explicitly requested."
fi

# --- git reset --hard ---
if echo "$CMD" | grep -qE '\bgit\s+reset\s+--hard\b'; then
  deny "git reset --hard detected" "This discards uncommitted changes irreversibly. Consider 'git stash' instead."
fi

# --- git clean -f ---
if echo "$CMD" | grep -qE '\bgit\s+clean\s+.*-[a-zA-Z]*f'; then
  deny "git clean -f detected" "This permanently deletes untracked files. Review 'git clean -n' first."
fi

# --- git checkout -- . (discard all changes) ---
if echo "$CMD" | grep -qE '\bgit\s+checkout\s+--\s+\.'; then
  deny "git checkout -- . detected" "This discards all unstaged changes. Consider 'git stash' instead."
fi

# --- git branch -D (force delete) ---
if echo "$CMD" | grep -qE '\bgit\s+branch\s+-D\b'; then
  deny "git branch -D detected" "Force-deleting a branch can lose unmerged work. Use -d for safe delete."
fi

# --- GitHub repo visibility change (destroys stars and watchers) ---
if echo "$CMD" | grep -qE '\bgh\s+api\s+.*repos/.*visibility|gh\s+repo\s+edit\s+.*--visibility'; then
  deny "GitHub repo visibility change detected" "Toggling a repo private permanently destroys all stars and watchers. NEVER do this."
fi

# --- gh repo delete (irreversible) ---
if echo "$CMD" | grep -qE '\bgh\s+repo\s+delete\b'; then
  deny "gh repo delete detected" "Deleting a GitHub repo is irreversible and destroys all stars, forks, and history. Confirm with the user first."
fi

# --- dropdb (PostgreSQL/MySQL database removal) ---
if echo "$CMD" | grep -qE '\bdropdb\b'; then
  deny "dropdb detected" "Dropping a database is irreversible. Confirm with the user first."
fi

# --- bq rm (BigQuery dataset/table removal) ---
if echo "$CMD" | grep -qE '\bbq\s+rm\b'; then
  deny "bq rm detected" "BigQuery rm is irreversible. Confirm with the user first."
fi

# --- aws s3 rm --recursive ---
if echo "$CMD" | grep -qE '\baws\s+s3\s+rm\b' && \
   echo "$CMD" | grep -qE '(--recursive\b|\s-r\b)'; then
  deny "aws s3 rm --recursive detected" "Recursive S3 deletion is irreversible. Confirm with the user first."
fi

# --- op item delete (1Password) ---
if echo "$CMD" | grep -qE '\bop\s+item\s+(delete|remove)\b'; then
  deny "op item delete detected" "Deleting a 1Password item is irreversible. Confirm with the user first."
fi

exit 0
