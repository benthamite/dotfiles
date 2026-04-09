#!/bin/bash
# PreToolUse hook: block Bash commands and Write operations that would
# expose secrets in terminal output or write them to unencrypted files.
#
# Matches common secret patterns (AWS keys, GitHub tokens, Slack tokens,
# API keys, private keys, etc.) in tool arguments. This is a compensating
# control for bypassPermissions mode — it enforces what CLAUDE.md's
# "never echo or print secrets" instruction cannot guarantee.
#
# Matchers: Bash, Write

set -euo pipefail

INPUT=$(cat)

TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')

# Extract the content to scan based on tool type
CONTENT=""
case "$TOOL_NAME" in
  Bash)
    CONTENT=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
    ;;
  Write)
    CONTENT=$(printf '%s' "$INPUT" | jq -r '(.tool_input.file_path // "") + "\n" + (.tool_input.content // "")')
    ;;
  *)
    exit 0
    ;;
esac

[ -z "$CONTENT" ] && exit 0

# --- Allowlist: commands that legitimately read secrets ---
# pass, op, security (Keychain), git-crypt, and secret-scanning tools themselves
if [ "$TOOL_NAME" = "Bash" ]; then
  # Allow pass/op/security/git-crypt commands
  if echo "$CONTENT" | grep -qE '^\s*(pass|op |security |git-crypt )'; then
    exit 0
  fi
  # Allow piping FROM pass/op (e.g. `pass show foo | some-command`)
  if echo "$CONTENT" | grep -qE '^\s*(pass|op )\s.*\|'; then
    exit 0
  fi
  # Allow grep/rg scanning for patterns (the audit skill itself)
  if echo "$CONTENT" | grep -qE '^\s*(grep|rg|ripgrep)\s'; then
    exit 0
  fi
  # Allow environment variable references (not the values themselves)
  # e.g. `echo $API_KEY` or `export API_KEY=op://...`
  if echo "$CONTENT" | grep -qE '\$\{?[A-Z_]+\}?' && ! echo "$CONTENT" | grep -qE '(AKIA|ghp_|ghs_|github_pat_|xox[bporca]-|sk-[a-zA-Z0-9]{20,}|-----BEGIN)'; then
    exit 0
  fi
  # Allow writing to encrypted/gitignored secret files
  if echo "$CONTENT" | grep -qE '\.zshenv-secrets|\.env\.op'; then
    exit 0
  fi
fi

# For Write tool, allow writing to known secret files
if [ "$TOOL_NAME" = "Write" ]; then
  FILE_PATH=$(printf '%s' "$INPUT" | jq -r '.tool_input.file_path // empty')
  case "$FILE_PATH" in
    *.zshenv-secrets|*.env.op|*.env.local|*/.password-store/*)
      exit 0
      ;;
  esac
fi

# --- Secret patterns ---
# Each pattern is tested independently for clear error messages.

check_pattern() {
  local pattern="$1"
  local label="$2"
  if echo "$CONTENT" | grep -qE -e "$pattern"; then
    jq -n --arg label "$label" --arg tool "$TOOL_NAME" '{
      "hookSpecificOutput": {
        "hookEventName": "PreToolUse",
        "permissionDecision": "deny",
        "permissionDecisionReason": ("BLOCKED: " + $tool + " command would expose a secret (" + $label + ").\n\nUse environment variables, `pass`, or `op://` references instead of literal secret values.\n\nIf this is a false positive (e.g. you are scanning for patterns, not echoing actual secrets), and you are confident the command is safe, tell the user and ask them to run it manually with `!`.")
      }
    }'
    exit 0
  fi
}

# AWS access key
check_pattern 'AKIA[0-9A-Z]{16}' 'AWS access key'

# GitHub tokens
check_pattern 'gh[ps]_[A-Za-z0-9_]{36,}' 'GitHub token'
check_pattern 'github_pat_[A-Za-z0-9_]{22,}' 'GitHub personal access token'

# Slack tokens
check_pattern 'xox[bporca]-[A-Za-z0-9-]{10,}' 'Slack token'

# OpenAI / Anthropic / generic sk- keys
check_pattern 'sk-[a-zA-Z0-9_-]{20,}' 'API secret key (sk-...)'

# Private keys
check_pattern '-----BEGIN (RSA |EC |OPENSSH )?PRIVATE KEY-----' 'private key'

# Google OAuth client secret (the secret, not the ID)
# Client IDs are semi-public; secrets are not.
check_pattern '"client_secret"\s*:\s*"[A-Za-z0-9_-]{20,}"' 'Google OAuth client secret'

# Generic high-entropy tokens assigned to known secret variable names
# This catches: API_KEY=abc123..., secret: "abc123...", token = "abc123..."
# The {32,} threshold reduces false positives from short values.
check_pattern "(api[_-]?key|api[_-]?secret|secret[_-]?key|access[_-]?token|auth[_-]?token)\s*[=:]\s*['\"][A-Za-z0-9/+=_-]{32,}" 'hardcoded secret value'

# If no patterns matched, allow the operation
exit 0
