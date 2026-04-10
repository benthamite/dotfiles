#!/bin/bash
# PreToolUse hook: block reads of sensitive files (secrets, keys, tokens).
#
# Fires on Read and escalates to the user (permissionDecision: ask) when
# the target path matches known sensitive locations. This prevents
# accidental ingestion of secrets into the context window.
#
# Matcher: Read

set -euo pipefail

INPUT=$(cat)

TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')
[ "$TOOL_NAME" != "Read" ] && exit 0

FILE_PATH=$(printf '%s' "$INPUT" | jq -r '.tool_input.file_path // empty')
[ -z "$FILE_PATH" ] && exit 0

# Expand ~ to the actual home directory for matching
EXPANDED_PATH="${FILE_PATH/#\~/$HOME}"

ask() {
  local label="$1"
  jq -n --arg label "$label" --arg path "$FILE_PATH" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "ask",
      "permissionDecisionReason": ("SENSITIVE FILE: " + $path + " — " + $label + ".\n\nThis file may contain secrets. Reading it would expose its contents in the context window. Confirm with the user before proceeding.")
    }
  }'
  exit 0
}

# --- Password store ---
case "$EXPANDED_PATH" in
  "$HOME/.password-store/"*|/Users/pablostafforini/.password-store/*)
    ask "password store (GPG-encrypted secrets)"
    ;;
esac

# --- Shell secrets ---
case "$EXPANDED_PATH" in
  *.zshenv-secrets)
    ask "shell secrets file"
    ;;
esac

# --- SSH private keys ---
case "$EXPANDED_PATH" in
  "$HOME/.ssh/id_"*|/Users/pablostafforini/.ssh/id_*)
    ask "SSH private key"
    ;;
esac

# --- GPG keys ---
case "$EXPANDED_PATH" in
  "$HOME/.gnupg/"*|/Users/pablostafforini/.gnupg/*)
    ask "GPG keyring"
    ;;
esac

# --- OAuth tokens inside ~/.config/ ---
case "$EXPANDED_PATH" in
  "$HOME/.config/"*/tokens.json|/Users/pablostafforini/.config/*/tokens.json)
    ask "OAuth tokens"
    ;;
esac

# --- Gmail MCP credentials ---
case "$EXPANDED_PATH" in
  "$HOME/.gmail-mcp-epoch/credentials/"*|/Users/pablostafforini/.gmail-mcp-epoch/credentials/*)
    ask "Gmail MCP credentials"
    ;;
esac

# --- OAuth client secrets inside ~/.config/ ---
case "$EXPANDED_PATH" in
  "$HOME/.config/"*/secret.json|/Users/pablostafforini/.config/*/secret.json)
    ask "OAuth client secret"
    ;;
  "$HOME/.config/"*/client_secret*.json|/Users/pablostafforini/.config/*/client_secret*.json)
    ask "OAuth client secret"
    ;;
esac

exit 0
