#!/bin/bash
# PreToolUse hook: block reads of sensitive files that would expose secrets.
#
# This complements block-secret-leak.sh. That hook catches literal secret values
# in tool input; this hook catches commands whose arguments name files likely to
# contain secrets, even when the command text itself contains no secret value.
#
# Matchers: Bash, exec_command, functions.exec_command, Read

set -euo pipefail

# shellcheck source=lib-codex-hook-json.sh
source "$(dirname "$0")/lib-codex-hook-json.sh"

INPUT=$(cat)
TOOL_NAME=$(codex_tool_name "$INPUT")

deny() {
  local label="$1" detail="$2"
  jq -n --arg label "$label" --arg detail "$detail" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": ("BLOCKED: sensitive read: " + $label + " — " + $detail + ".\n\nThis could expose secret values in the conversation context. Use a safe alternative: pass/op/git-crypt/security for auth-managed secrets; ls/stat/file/wc for metadata; grep/rg with -l, -c, -q, or -L for filename/count/quiet checks; or a purpose-built helper that returns only non-secret metadata.")
    }
  }'
  exit 0
}

sensitive_label_for_text() {
  local text="$1"

  if   echo "$text" | grep -qE '(^|[[:space:]/])\.mcp\.json\b|(^|[[:space:]/])mcp\.json\b'; then
    printf '%s\n' "MCP credential config"
  elif echo "$text" | grep -qE '\.zshenv-secrets\b'; then
    printf '%s\n' "shell secrets file"
  elif echo "$text" | grep -qE '\.password-store/'; then
    printf '%s\n' "password store"
  elif echo "$text" | grep -qE '(^|[ /=])\.ssh/id_[A-Za-z0-9_]+\b' && \
       ! echo "$text" | grep -qE '\.ssh/id_[A-Za-z0-9_]+\.pub\b'; then
    printf '%s\n' "SSH private key"
  elif echo "$text" | grep -qE '\.gnupg/'; then
    printf '%s\n' "GPG keyring"
  elif echo "$text" | grep -qE '(^|[[:space:]])?/?[^[:space:]]*\.config/[^/[:space:]]+/tokens\.json\b'; then
    printf '%s\n' "OAuth tokens"
  elif echo "$text" | grep -qE '(^|[[:space:]])?/?[^[:space:]]*\.gmail-mcp-epoch/credentials/'; then
    printf '%s\n' "Gmail MCP credentials"
  elif echo "$text" | grep -qE '(^|[[:space:]])?/?[^[:space:]]*\.config/[^/[:space:]]+/(secret\.json|client_secret[^"[:space:]]*\.json)\b'; then
    printf '%s\n' "OAuth client secret"
  elif echo "$text" | grep -qE '(^|[[:space:]/])(credentials\.json|service-account[^/[:space:]]*\.json|tokens\.json)\b'; then
    printf '%s\n' "credential JSON"
  elif echo "$text" | grep -qE '(^|[[:space:]/])\.env(\.|[[:space:]]|$)|(^|[[:space:]/])\.envrc\b'; then
    printf '%s\n' "environment secrets file"
  fi
}

case "$TOOL_NAME" in
  Read)
    FILE_PATH=$(codex_tool_input_field "$INPUT" file_path)
    [ -z "$FILE_PATH" ] && FILE_PATH=$(codex_tool_input_field "$INPUT" path)
    [ -z "$FILE_PATH" ] && exit 0
    EXPANDED_PATH="${FILE_PATH/#\~/$HOME}"
    LABEL=$(sensitive_label_for_text "$EXPANDED_PATH")
    [ -n "$LABEL" ] && deny "$LABEL" "$FILE_PATH"
    ;;

  Bash|exec_command|functions.exec_command)
    COMMAND=$(codex_shell_command "$INPUT")
    [ -z "$COMMAND" ] && exit 0

    LABEL=$(sensitive_label_for_text "$COMMAND")
    [ -z "$LABEL" ] && exit 0

    # Auth-aware tools manage secrets safely.
    if echo "$COMMAND" | grep -qE '^[[:space:]]*(pass[[:space:]]|op[[:space:]]|git-crypt[[:space:]]|security[[:space:]])'; then
      exit 0
    fi

    # Pure metadata commands do not emit file content.
    if echo "$COMMAND" | grep -qE '^[[:space:]]*(ls|stat|file|basename|dirname|realpath)([[:space:]]|$)'; then
      exit 0
    fi

    # wc reports counts only.
    if echo "$COMMAND" | grep -qE '^[[:space:]]*wc([[:space:]]+-[lcwmL]+)*([[:space:]]|$)'; then
      exit 0
    fi

    # grep/rg/ripgrep with non-content output flags only.
    if echo "$COMMAND" | grep -qE '^[[:space:]]*(grep|rg|ripgrep)[[:space:]]+(-[a-zA-Z]*[lcqL][a-zA-Z]*[[:space:]]|--(files-with-matches|files-without-match|count|quiet|silent)[[:space:]])'; then
      exit 0
    fi

    # Existence tests do not read content.
    if echo "$COMMAND" | grep -qE '^[[:space:]]*(\[\[?|test)[[:space:]]+-[fedrwxs][[:space:]]'; then
      exit 0
    fi

    deny "$LABEL" "command appears to read or process the contents of $LABEL"
    ;;
esac

exit 0
