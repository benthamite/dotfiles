#!/bin/bash
# PreToolUse hook: block destructive shell commands that are hard to reverse.
#
# Enforces CLAUDE.md's "use trash instead of rm -rf" and "never push without
# confirmation" instructions with code, not just prose.
#
# Matcher: Bash

set -euo pipefail

INPUT=$(cat)

TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')
[ "$TOOL_NAME" != "Bash" ] && exit 0

CMD=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
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
      "permissionDecision": "ask",
      "permissionDecisionReason": ($label + ". " + $suggestion)
    }
  }'
  exit 0
}

# --- rm -rf / rm -r (should use trash) ---
if echo "$CMD" | grep -qE '\brm\s+(-[a-zA-Z]*r[a-zA-Z]*f|(-[a-zA-Z]*f[a-zA-Z]*r)|-rf|-fr)\b'; then
  deny "rm -rf detected" "Use 'trash' instead of 'rm -rf' to allow recovery."
fi

# --- git push --force / -f (dangerous to shared branches) ---
if echo "$CMD" | grep -qE '\bgit\s+push\s+.*(-f|--force|--force-with-lease)\b'; then
  deny "git push --force detected" "Force-pushing can overwrite upstream history. Confirm with the user first."
fi

# --- git push (all pushes need user approval) ---
if echo "$CMD" | grep -qE '\bgit\s+push\b'; then
  ask "git push detected" "Pushes are visible to others. Confirm with the user first."
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

exit 0
