#!/bin/bash
# PreToolUse hook: block reads of sensitive files (secrets, keys, tokens).
#
# Fires on Read AND Bash. Escalates to the user (permissionDecision: ask)
# when the operation would expose the contents of a sensitive file in the
# conversation context.
#
# - On Read: matches the target file_path against a sensitive-path set.
# - On Bash: scans the command for any mention of the same sensitive paths.
#   Allowlists safe operations (ls/stat/file/wc/test, grep -l/-c/-q/-L,
#   the auth-aware tools pass/op/git-crypt/security). Anything else that
#   touches a sensitive path content-extracts by default — block.
#
# Matchers in settings.json: Read, Bash

set -euo pipefail

INPUT=$(cat)
TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')

ask() {
  local label="$1" detail="$2"
  jq -n --arg label "$label" --arg detail "$detail" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "ask",
      "permissionDecisionReason": ("SENSITIVE: " + $label + " — " + $detail + ".\n\nThis would expose secret values in the conversation context. Confirm with the user before proceeding, or use a safe alternative: pass/op/git-crypt for auth-managed secrets; ls/stat/file/wc -l/-c for metadata; grep -l/-c/-q/-L for filename/count/quiet matches.")
    }
  }'
  exit 0
}

# --------------------------------------------------------------------------
# Read tool branch — match on tool_input.file_path.
# --------------------------------------------------------------------------

if [ "$TOOL_NAME" = "Read" ]; then
  FILE_PATH=$(printf '%s' "$INPUT" | jq -r '.tool_input.file_path // empty')
  [ -z "$FILE_PATH" ] && exit 0

  # Expand ~ to the actual home directory for matching
  EXPANDED_PATH="${FILE_PATH/#\~/$HOME}"

  # Password store
  case "$EXPANDED_PATH" in
    "$HOME/.password-store/"*|/Users/pablostafforini/.password-store/*)
      ask "password store (GPG-encrypted secrets)" "$FILE_PATH" ;;
  esac
  # Shell secrets
  case "$EXPANDED_PATH" in
    *.zshenv-secrets) ask "shell secrets file" "$FILE_PATH" ;;
  esac
  # SSH private keys
  case "$EXPANDED_PATH" in
    "$HOME/.ssh/id_"*|/Users/pablostafforini/.ssh/id_*)
      ask "SSH private key" "$FILE_PATH" ;;
  esac
  # GPG keys
  case "$EXPANDED_PATH" in
    "$HOME/.gnupg/"*|/Users/pablostafforini/.gnupg/*)
      ask "GPG keyring" "$FILE_PATH" ;;
  esac
  # OAuth tokens
  case "$EXPANDED_PATH" in
    "$HOME/.config/"*/tokens.json|/Users/pablostafforini/.config/*/tokens.json)
      ask "OAuth tokens" "$FILE_PATH" ;;
  esac
  # Gmail MCP credentials
  case "$EXPANDED_PATH" in
    "$HOME/.gmail-mcp-epoch/credentials/"*|/Users/pablostafforini/.gmail-mcp-epoch/credentials/*)
      ask "Gmail MCP credentials" "$FILE_PATH" ;;
  esac
  # OAuth client secrets
  case "$EXPANDED_PATH" in
    "$HOME/.config/"*/secret.json|/Users/pablostafforini/.config/*/secret.json)
      ask "OAuth client secret" "$FILE_PATH" ;;
    "$HOME/.config/"*/client_secret*.json|/Users/pablostafforini/.config/*/client_secret*.json)
      ask "OAuth client secret" "$FILE_PATH" ;;
  esac

  exit 0
fi

# --------------------------------------------------------------------------
# Bash tool branch — scan the command string for sensitive path mentions.
# --------------------------------------------------------------------------

if [ "$TOOL_NAME" = "Bash" ]; then
  COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
  [ -z "$COMMAND" ] && exit 0

  # 1. Identify which sensitive path (if any) is mentioned.
  #    These regexes use the same set as the Read branch above. If you add
  #    a path there, add it here too.
  SENSITIVE_LABEL=""
  if   echo "$COMMAND" | grep -qE '\.zshenv-secrets\b'; then
    SENSITIVE_LABEL="shell secrets file"
  elif echo "$COMMAND" | grep -qE '\.password-store/'; then
    SENSITIVE_LABEL="password store (GPG-encrypted secrets)"
  elif echo "$COMMAND" | grep -qE '(^|[ /=])\.ssh/id_[A-Za-z0-9_]+\b' && \
       ! echo "$COMMAND" | grep -qE '\.ssh/id_[A-Za-z0-9_]+\.pub\b'; then
    # Match private keys but exclude public keys (.pub).
    SENSITIVE_LABEL="SSH private key"
  elif echo "$COMMAND" | grep -qE '\.gnupg/'; then
    SENSITIVE_LABEL="GPG keyring"
  elif echo "$COMMAND" | grep -qE '\.config/[^/[:space:]]+/tokens\.json\b'; then
    SENSITIVE_LABEL="OAuth tokens"
  elif echo "$COMMAND" | grep -qE '\.gmail-mcp-epoch/credentials/'; then
    SENSITIVE_LABEL="Gmail MCP credentials"
  elif echo "$COMMAND" | grep -qE '\.config/[^/[:space:]]+/(secret\.json|client_secret[^"[:space:]]*\.json)\b'; then
    SENSITIVE_LABEL="OAuth client secret"
  fi

  # No sensitive path mentioned → allow.
  [ -z "$SENSITIVE_LABEL" ] && exit 0

  # 2. Allowlist of safe operations on these paths.
  #    Each rule must be tight enough that it only matches commands which
  #    cannot extract file content.

  # Auth-aware tools (these manage secrets safely; allow even when the
  # command names a sensitive path, e.g. `pass insert` or `git-crypt unlock`).
  if echo "$COMMAND" | grep -qE '^[[:space:]]*(pass[[:space:]]|op[[:space:]]|git-crypt[[:space:]]|security[[:space:]])'; then
    exit 0
  fi

  # Pure metadata commands at the start of the command.
  # ls, stat, file, basename, dirname, realpath: do not emit file content.
  if echo "$COMMAND" | grep -qE '^[[:space:]]*(ls|stat|file|basename|dirname|realpath)([[:space:]]|$)'; then
    exit 0
  fi

  # `wc` with safe flags only (-l, -c, -w, -m). `wc -L` (longest line)
  # would leak content length but not content; allowed too. Bare `wc` (no
  # flag) defaults to lines+words+bytes — also content-free, allowed.
  if echo "$COMMAND" | grep -qE '^[[:space:]]*wc([[:space:]]+-[lcwmL]+)*([[:space:]]|$)'; then
    exit 0
  fi

  # `grep`/`rg`/`ripgrep` with at least one non-content flag (-l, -c, -q, -L).
  # These output filenames, counts, or nothing — never matched lines.
  if echo "$COMMAND" | grep -qE '^[[:space:]]*(grep|rg|ripgrep)[[:space:]]+(-[a-zA-Z]*[lcqL][a-zA-Z]*[[:space:]]|--(files-with-matches|files-without-match|count|quiet|silent)[[:space:]])'; then
    exit 0
  fi

  # POSIX-style existence/test bracket forms: `[ -f path ]`, `[[ -f path ]]`,
  # `test -f path`. These don't read content.
  if echo "$COMMAND" | grep -qE '^[[:space:]]*(\[\[?|test)[[:space:]]+-[fedrwxs][[:space:]]'; then
    exit 0
  fi

  # 3. Anything else that touched a sensitive path content-extracts by default.
  ask "$SENSITIVE_LABEL" "Bash command appears to read or process the contents of $SENSITIVE_LABEL"
fi

exit 0
