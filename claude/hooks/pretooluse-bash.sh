#!/bin/bash
# PreToolUse dispatcher for the Bash matcher.
#
# Consolidates the per-command Bash PreToolUse hooks into ONE process: read
# stdin once, parse JSON once, run each check in-process, and emit a single
# aggregated hookSpecificOutput. Replaces ~13 bash+jq spawns per Bash command
# with one.
#
# Claude Code runs all hooks matched to an event and aggregates their outputs
# with precedence deny > ask > allow; permissionDecision and updatedInput may
# coexist. A single dispatcher that emits one aggregated decision is therefore
# behaviorally equivalent to the separate hooks.
#
# Self-contained, always-run checks are ported here (the only way to drop their
# per-command spawn). Checks that do git/filesystem work behind a cheap regex
# gate are DELEGATED to their unchanged standalone scripts, and only when their
# gate matches — so they add no spawn on the common path and their behavior is
# identical to today. wrap-bash-output runs last.
#
# The standalone hooks remain in this directory: they are the source of truth
# for the delegated checks, the Edit/Write/Read/Grep registrations of the
# shared guards, and a rollback path for the ported ones.
#
# Matcher in settings.json: Bash
set -euo pipefail

DIR=$(cd -- "$(dirname -- "$0")" && pwd)
INPUT=$(cat)

TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')
[ "$TOOL_NAME" = "Bash" ] || exit 0
COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
[ -z "$COMMAND" ] && exit 0
SESSION_ID=$(printf '%s' "$INPUT" | jq -r '.session_id // empty')

# ----------------------------------------------------------------------------
# Decision accumulators. Each ported check adds at most one decision (matching
# a standalone hook, which emits at most one). All checks run; we aggregate at
# the end with precedence deny > ask > allow.
# ----------------------------------------------------------------------------
DENY_REASONS=()
ASK_REASONS=()
ALLOW_CONTEXT=""
UPDATED_CMD=""
add_deny() { DENY_REASONS+=("$1"); }
add_ask()  { ASK_REASONS+=("$1"); }

updated_input_json() {
  printf '%s' "$INPUT" | jq -c --arg cmd "$UPDATED_CMD" '.tool_input + {command:$cmd}'
}

# Delegate to an unchanged standalone hook; fold any deny it emits into the
# accumulator. Used for checks that do git/fs work behind a cheap gate, so they
# only spawn when relevant.
delegate() {
  local out reason
  out=$(printf '%s' "$INPUT" | bash "$DIR/$1" 2>/dev/null) || true
  if printf '%s' "$out" | grep -q '"permissionDecision": "deny"'; then
    reason=$(printf '%s' "$out" | jq -r '.hookSpecificOutput.permissionDecisionReason // empty' 2>/dev/null || true)
    add_deny "$reason"
  fi
}

# ============================================================================
# Ported checks (verbatim logic from the standalone hooks; Bash branch only).
# ============================================================================

# --- block-secret-leak.sh (Bash branch) ---
check_secret_leak() {
  local CONTENT="$COMMAND"
  [ -z "$CONTENT" ] && return 0

  # Standalone `op item get ... --reveal` prints the revealed field.
  if echo "$CONTENT" | grep -qE '^\s*op\s+item\s+get\b' && \
     echo "$CONTENT" | grep -qE -- '(^|[[:space:]])--reveal([[:space:]]|$)' && \
     ! echo "$CONTENT" | grep -qE '[|>]'; then
    add_deny "BLOCKED: Bash command would print a revealed 1Password field.

Do not run standalone \`op item get ... --reveal\` commands. Capture the value through command substitution, pass it through stdin/env/temp files with restricted permissions, or use \`op://\` references where supported."
    return 0
  fi
  # Allowlist: auth-aware tools, pipes from them, scanners, env-var refs, secret files.
  echo "$CONTENT" | grep -qE '^\s*(pass|op |security |git-crypt )' && return 0
  echo "$CONTENT" | grep -qE '^\s*(pass|op )\s.*\|' && return 0
  echo "$CONTENT" | grep -qE '^\s*(grep|rg|ripgrep)\s' && return 0
  if echo "$CONTENT" | grep -qE '\$\{?[A-Z_]+\}?' && ! echo "$CONTENT" | grep -qE '(AKIA|ghp_|ghs_|github_pat_|xox[bporca]-|sk-[a-zA-Z0-9]{20,}|-----BEGIN)'; then
    return 0
  fi
  echo "$CONTENT" | grep -qE '\.zshenv-secrets|\.env\.op' && return 0

  local SECRET_PATTERNS SECRET_LABELS
  SECRET_PATTERNS=(
    'AKIA[0-9A-Z]{16}'
    'gh[ps]_[A-Za-z0-9_]{36,}'
    'github_pat_[A-Za-z0-9_]{22,}'
    'xox[bporca]-[A-Za-z0-9-]{10,}'
    'sk-ant-[A-Za-z0-9_-]{40,}'
    '(^|[^A-Za-z0-9])sk-[a-zA-Z0-9_-]{20,}'
    '(sk|pk|rk)_(live|test)_[A-Za-z0-9]{20,}'
    'lin_api_[A-Za-z0-9]{40,}'
    'pat[A-Za-z0-9]{14}\.[a-f0-9]{64}'
    '-----BEGIN (RSA |EC |OPENSSH )?PRIVATE KEY-----'
    '"client_secret"\s*:\s*"[A-Za-z0-9_-]{20,}"'
    'AIza[0-9A-Za-z_-]{35}'
    'glpat-[A-Za-z0-9_-]{20,}'
    'eyJ[A-Za-z0-9_-]+\.eyJ[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+'
    '(postgres|postgresql|mysql|mongodb|mongodb\+srv|redis|amqp|amqps|mssql)://[^:/ ]+:[^@/ ]+@'
    "(api[_-]?key|api[_-]?secret|secret[_-]?key|access[_-]?token|auth[_-]?token)\s*[=:]\s*['\"][A-Za-z0-9/+=_-]{32,}"
  )
  SECRET_LABELS=(
    'AWS access key'
    'GitHub token'
    'GitHub personal access token'
    'Slack token'
    'Anthropic API key'
    'API secret key (sk-...)'
    'Stripe API key'
    'Linear API key'
    'Airtable personal access token'
    'private key'
    'Google OAuth client secret'
    'Google API key'
    'GitLab token'
    'JWT'
    'database connection string with embedded credentials'
    'hardcoded secret value'
  )
  local gate_args=() _pat _i
  for _pat in "${SECRET_PATTERNS[@]}"; do gate_args+=(-e "$_pat"); done
  if echo "$CONTENT" | grep -qE "${gate_args[@]}"; then
    for _i in "${!SECRET_PATTERNS[@]}"; do
      if echo "$CONTENT" | grep -qE -e "${SECRET_PATTERNS[$_i]}"; then
        add_deny "BLOCKED: Bash command would expose a secret (${SECRET_LABELS[$_i]}).

Use environment variables, \`pass\`, or \`op://\` references instead of literal secret values.

If this is a false positive (e.g. you are scanning for patterns, not echoing actual secrets), and you are confident the command is safe, tell the user and ask them to run it manually with \`!\`."
        return 0
      fi
    done
  fi

  # Exfiltration patterns.
  local SENSITIVE_PATH_RE='\.(ssh/id_|zshenv-secrets|password-store|gnupg/)|tokens\.json'
  if echo "$CONTENT" | grep -qE "(cat|base64|xxd)\s+[^\|;]*${SENSITIVE_PATH_RE}" && \
     echo "$CONTENT" | grep -qE '\|\s*(curl|wget|nc|ncat)\b'; then
    add_deny "BLOCKED: Bash command would expose a secret (sensitive file piped to network tool (exfiltration risk))."
    return 0
  fi
  if echo "$CONTENT" | grep -qE 'curl\s' && \
     echo "$CONTENT" | grep -qE '(-d\s*@|--data[a-z-]*\s*@)' && \
     echo "$CONTENT" | grep -qE "$SENSITIVE_PATH_RE"; then
    add_deny "BLOCKED: Bash command would expose a secret (curl uploading sensitive file (exfiltration risk))."
    return 0
  fi
  if echo "$CONTENT" | grep -qE '\b(curl|wget|nc|ncat|python[23]?\s.*urllib|node\s.*fetch)\b'; then
    local HIGH_ENTROPY HAS_UPPER HAS_LOWER HAS_SYMBOL CLASSES
    HIGH_ENTROPY=$(echo "$CONTENT" | \
      sed -E 's/0x[a-fA-F0-9]{40}([^a-fA-F0-9]|$)/\1/g' | \
      grep -oE '[A-Za-z0-9/+=_-]{30,}' | \
      grep -vE '^https?://' | \
      grep -vE '^/' | \
      grep -vE '^[a-z]+$' | \
      grep -vE '[a-zA-Z]+/[a-zA-Z]+/[a-zA-Z]+' | \
      head -1 || true)
    if [ -n "$HIGH_ENTROPY" ]; then
      if echo "$HIGH_ENTROPY" | grep -q '[0-9]'; then
        HAS_UPPER=$(echo "$HIGH_ENTROPY" | grep -c '[A-Z]' || true)
        HAS_LOWER=$(echo "$HIGH_ENTROPY" | grep -c '[a-z]' || true)
        HAS_SYMBOL=$(echo "$HIGH_ENTROPY" | grep -c '[/+=_-]' || true)
        CLASSES=1
        [ "$HAS_UPPER" -gt 0 ] && CLASSES=$((CLASSES + 1))
        [ "$HAS_LOWER" -gt 0 ] && CLASSES=$((CLASSES + 1))
        [ "$HAS_SYMBOL" -gt 0 ] && CLASSES=$((CLASSES + 1))
        if [ "$CLASSES" -ge 3 ]; then
          add_deny "BLOCKED: Bash command would expose a secret (network command with inline secret-like string (exfiltration risk))."
          return 0
        fi
      fi
    fi
  fi
  return 0
}

# --- block-unguarded-ahrefs-api.sh ---
check_ahrefs() {
  echo "$COMMAND" | grep -q 'api\.ahrefs\.com' || return 0
  echo "$COMMAND" | grep -qE '(^|[[:space:]/])ahrefs-api-guard([[:space:]]|$)' && return 0
  if echo "$COMMAND" | grep -q 'subscription-info/limits-and-usage' && \
     ! echo "$COMMAND" | grep -qE 'site-explorer|site-audit|keywords-explorer|rank-tracker|web-analytics|brand-radar|gsc|serp-overview|batch-analysis|public-crawler|management'; then
    return 0
  fi
  add_deny "BLOCKED: raw Ahrefs API call detected. Ahrefs API units are shared across Epoch automations. Use \`ahrefs-api-guard request ...\` so the free quota probe runs and blocks the call if remaining units are below the required reserve."
  return 0
}

# --- block-sensitive-read.sh (Bash branch) ---
sr_is_safe_env_loader() {
  local command="$1"
  echo "$command" | grep -qE '(^|[[:space:]&;|(])(\.|source)[[:space:]]+[^&;|[:space:]]*\.env([.[:space:]"'"'"';&|)]|$)|(^|[[:space:]&;|(])(\.|source)[[:space:]]+[^&;|[:space:]]*\.envrc\b' || return 1
  echo "$command" | grep -qE '(^|[[:space:]&;|])(env|printenv|declare|typeset)([[:space:]&;|]|$)' && return 1
  echo "$command" | grep -qE '(^|[[:space:]&;|])export[[:space:]]+-p([[:space:]&;|]|$)' && return 1
  echo "$command" | grep -qE '(^|[[:space:]&;|])set[[:space:]]*([;&|)]|$)' && return 1
  echo "$command" | grep -qE '\b(cat|sed|awk|perl|python[0-9.]*|ruby|node|head|tail|less|more|grep|rg|ripgrep)\b[^&;|]*\.env([.[:space:]"'"'"';&|)]|$)' && return 1
  echo "$command" | grep -qE '\b(cat|sed|awk|perl|python[0-9.]*|ruby|node|head|tail|less|more|grep|rg|ripgrep)\b[^&;|]*\.envrc\b' && return 1
  return 0
}
check_sensitive_read() {
  local SENSITIVE_LABEL=""
  if   echo "$COMMAND" | grep -qE '\.zshenv-secrets\b'; then
    SENSITIVE_LABEL="shell secrets file"
  elif echo "$COMMAND" | grep -qE '\.password-store/'; then
    SENSITIVE_LABEL="password store (GPG-encrypted secrets)"
  elif echo "$COMMAND" | grep -qE '(^|[[:space:]/])\.mcp\.json\b|(^|[[:space:]/])mcp\.json\b'; then
    SENSITIVE_LABEL="MCP credential config"
  elif echo "$COMMAND" | grep -qE '(^|[[:space:]/])\.env([.[:space:]"'"'"';&|)]|$)|(^|[[:space:]/])\.envrc\b'; then
    SENSITIVE_LABEL="environment secrets file"
  elif echo "$COMMAND" | grep -qE '(^|[ /=])\.ssh/id_[A-Za-z0-9_]+\b' && \
       ! echo "$COMMAND" | grep -qE '\.ssh/id_[A-Za-z0-9_]+\.pub\b'; then
    SENSITIVE_LABEL="SSH private key"
  elif echo "$COMMAND" | grep -qE '\.gnupg/'; then
    SENSITIVE_LABEL="GPG keyring"
  elif echo "$COMMAND" | grep -qE '(^|[[:space:]])?/?[^[:space:]]*\.config/[^/[:space:]]+/tokens\.json\b'; then
    SENSITIVE_LABEL="OAuth tokens"
  elif echo "$COMMAND" | grep -qE '(^|[[:space:]])?/?[^[:space:]]*\.gmail-mcp-epoch/credentials/'; then
    SENSITIVE_LABEL="Gmail MCP credentials"
  elif echo "$COMMAND" | grep -qE '(^|[[:space:]])?/?[^[:space:]]*\.config/[^/[:space:]]+/(secret\.json|client_secret[^"[:space:]]*\.json)\b'; then
    SENSITIVE_LABEL="OAuth client secret"
  elif echo "$COMMAND" | grep -qE '(^|[[:space:]/])(credentials\.json|service-account[^/[:space:]]*\.json|tokens\.json)\b'; then
    SENSITIVE_LABEL="credential JSON"
  fi
  [ -z "$SENSITIVE_LABEL" ] && return 0

  if [ "$SENSITIVE_LABEL" = "environment secrets file" ] && sr_is_safe_env_loader "$COMMAND"; then
    ALLOW_CONTEXT="Sensitive environment file loading was allowed for this Bash command. Do not print, inspect, summarize, or quote loaded environment values; use them only as process environment for the requested command."
    return 0
  fi
  echo "$COMMAND" | grep -qE '^[[:space:]]*(pass[[:space:]]|op[[:space:]]|git-crypt[[:space:]]|security[[:space:]])' && return 0
  echo "$COMMAND" | grep -qE '^[[:space:]]*(ls|stat|file|basename|dirname|realpath)([[:space:]]|$)' && return 0
  echo "$COMMAND" | grep -qE '^[[:space:]]*wc([[:space:]]+-[lcwmL]+)*([[:space:]]|$)' && return 0
  echo "$COMMAND" | grep -qE '^[[:space:]]*(grep|rg|ripgrep)[[:space:]]+(-[a-zA-Z]*[lcqL][a-zA-Z]*[[:space:]]|--(files-with-matches|files-without-match|count|quiet|silent)[[:space:]])' && return 0
  echo "$COMMAND" | grep -qE '^[[:space:]]*(\[\[?|test)[[:space:]]+-[fedrwxs][[:space:]]' && return 0

  add_deny "BLOCKED: sensitive read: ${SENSITIVE_LABEL} — Bash command appears to read or process the contents of ${SENSITIVE_LABEL}.

This could expose secret values in the conversation context. Use a safe alternative: pass/op/git-crypt for auth-managed secrets; ls/stat/file/wc -l/-c for metadata; grep -l/-c/-q/-L for filename/count/quiet matches. Loading environment files into a subprocess is allowed only when the command does not print or inspect the loaded secrets."
  return 0
}

# --- block-destructive-command.sh ---
dc_deny() { add_deny "BLOCKED: $1.

$2

If you are certain this is the right action, ask the user to run it manually with \`!\`."; }
check_destructive() {
  if echo "$COMMAND" | grep -qE '\brm\s+(-[a-zA-Z]*r[a-zA-Z]*f|(-[a-zA-Z]*f[a-zA-Z]*r)|-rf|-fr)\b'; then
    dc_deny "rm -rf detected" "Use 'trash' instead of 'rm -rf' to allow recovery."; return 0
  fi
  if echo "$COMMAND" | grep -qE '\bgit\s+push\b' && \
     echo "$COMMAND" | grep -qE '(\s-f\b|\s--force\b)' && \
     ! echo "$COMMAND" | grep -qF -- '--force-with-lease'; then
    dc_deny "git push --force detected" "Force-pushing can overwrite upstream history. Use --force-with-lease for branch-scoped pushes, or confirm with the user."; return 0
  fi
  if echo "$COMMAND" | grep -qE '\b(git\s+clone|gh\s+repo\s+clone)\b'; then
    add_ask "git clone detected. Only clone repositories the user has explicitly requested."; return 0
  fi
  if echo "$COMMAND" | grep -qE '\bgit\s+reset\s+--hard\b'; then
    dc_deny "git reset --hard detected" "This discards uncommitted changes irreversibly. Consider 'git stash' instead."; return 0
  fi
  if echo "$COMMAND" | grep -qE '\bgit\s+clean\s+.*-[a-zA-Z]*f'; then
    dc_deny "git clean -f detected" "This permanently deletes untracked files. Review 'git clean -n' first."; return 0
  fi
  if echo "$COMMAND" | grep -qE '\bgit\s+checkout\s+--\s+\.'; then
    dc_deny "git checkout -- . detected" "This discards all unstaged changes. Consider 'git stash' instead."; return 0
  fi
  if echo "$COMMAND" | grep -qE '\bgit\s+branch\s+-D\b'; then
    dc_deny "git branch -D detected" "Force-deleting a branch can lose unmerged work. Use -d for safe delete."; return 0
  fi
  if echo "$COMMAND" | grep -qE '\bgh\s+api\s+.*repos/.*visibility|gh\s+repo\s+edit\s+.*--visibility'; then
    dc_deny "GitHub repo visibility change detected" "Toggling a repo private permanently destroys all stars and watchers. NEVER do this."; return 0
  fi
  if echo "$COMMAND" | grep -qE '\bgh\s+repo\s+delete\b'; then
    dc_deny "gh repo delete detected" "Deleting a GitHub repo is irreversible and destroys all stars, forks, and history. Confirm with the user first."; return 0
  fi
  if echo "$COMMAND" | grep -qE '\bdropdb\b'; then
    dc_deny "dropdb detected" "Dropping a database is irreversible. Confirm with the user first."; return 0
  fi
  if echo "$COMMAND" | grep -qE '\bbq\s+rm\b'; then
    dc_deny "bq rm detected" "BigQuery rm is irreversible. Confirm with the user first."; return 0
  fi
  if echo "$COMMAND" | grep -qE '\baws\s+s3\s+rm\b' && \
     echo "$COMMAND" | grep -qE '(--recursive\b|\s-r\b)'; then
    dc_deny "aws s3 rm --recursive detected" "Recursive S3 deletion is irreversible. Confirm with the user first."; return 0
  fi
  if echo "$COMMAND" | grep -qE '\bop\s+item\s+(delete|remove)\b'; then
    dc_deny "op item delete detected" "Deleting a 1Password item is irreversible. Confirm with the user first."; return 0
  fi
  return 0
}

# --- block-walk-list-access.sh (Bash branch) ---
check_walk_list() {
  local EXPANDED_HOME="$HOME/.claude/walk-list-data"
  if ! echo "$COMMAND" | grep -qE "(\.claude/walk-list-data|~/\.claude/walk-list-data|$EXPANDED_HOME)"; then
    return 0
  fi
  if echo "$COMMAND" | grep -qE '(^|[;&|[:space:]])python[0-9.]*[[:space:]]+(-[a-zA-Z]+[[:space:]]+)*([^[:space:]]+/)?walk\.py([[:space:]]|$)'; then
    return 0
  fi
  add_deny "BLOCKED: Bash command references ~/.claude/walk-list-data/ outside of walk.py: $COMMAND

The walk-list protected store at ~/.claude/walk-list-data/ is unreadable by any tool other than \`python ~/.claude/skills/walk-list/walk.py\`. Use \`walk.py next <input-file> <decision>\` to advance one item; the script will print exactly the one item you are permitted to see. Do NOT attempt to circumvent."
  return 0
}

# --- require-elisp-verify-after-commit.sh (marker-gated, always-run) ---
check_elisp_verify() {
  local MARKER="/tmp/claude-elisp-verify-needed-${SESSION_ID}"
  [ -f "$MARKER" ] || return 0
  echo "$COMMAND" | grep -qE '\bemacsclient\b' && return 0
  echo "$COMMAND" | grep -qE 'batch-test\.sh' && return 0
  add_deny "BLOCKED: You committed Elisp changes but have not verified them in the running Emacs. Wait for the async post-commit rebuild+reload to finish if it is still pending, then run \`emacsclient -e\` (or \`emacsclient --eval\`) to exercise the changed code path before continuing. A reload status poll alone is not live verification."
  return 0
}

# --- wrap-bash-output.sh (must run last; sets updatedInput) ---
check_wrap() {
  local REDACTOR="$HOME/My Drive/dotfiles/claude/hooks/redact-secrets.sh"
  case "$COMMAND" in
    *"redact-secrets.sh"*) return 0 ;;
  esac
  printf '%s' "$COMMAND" | grep -qE '<<-?[[:space:]]*[A-Za-z_'"'"'"]' && return 0
  printf '%s' "$COMMAND" | grep -qE '&[[:space:]]*$' && return 0
  UPDATED_CMD="{ ${COMMAND}; } 2>&1 | \"${REDACTOR}\"; exit \"\${PIPESTATUS[0]}\""
}

# ============================================================================
# Dispatch in settings.json order; reason concatenation follows the same order.
# ============================================================================
IS_COMMIT=0
echo "$COMMAND" | grep -qE '\bcommit\b' && IS_COMMIT=1

check_secret_leak
check_ahrefs
if echo "$COMMAND" | grep -qE '(^|[[:space:];|&])(git[[:space:]]+push|gh[[:space:]]+(api|pr|issue|secret|variable|workflow|run|release|repo|label|milestone|gist)[[:space:]])|agents/github-write-allowlist\.txt|codex/(hooks/block-github-write-command\.sh|hooks/block-github-guard-edit\.sh|hooks\.json)|claude/(hooks/block-github-write-command\.sh|hooks/block-github-guard-edit\.sh|hooks/pretooluse-bash\.sh)|\.codex/hooks\.json|\.claude/settings\.json'; then
  delegate block-github-write-command.sh
fi
check_sensitive_read
check_destructive
check_walk_list
[ "$IS_COMMIT" -eq 1 ] && delegate require-elisp-test-before-commit.sh
[ "$IS_COMMIT" -eq 1 ] && delegate require-doc-update.sh
[ "$IS_COMMIT" -eq 1 ] && delegate require-readme-update.sh
check_elisp_verify
if echo "$COMMAND" | grep -qE '\bemacsclient\b' && echo "$COMMAND" | grep -qE 'elpaca-rebuild|elpaca-extras-reload'; then
  delegate block-elpaca-rebuild-uncommitted.sh
fi
[ "$IS_COMMIT" -eq 1 ] && delegate require-ai-config-sync.sh
if echo "$COMMAND" | grep -qE '(python3?|nohup)[^|]*setup_db\.py[^|]*(--full|--target)' || \
   echo "$COMMAND" | grep -qE '(bash|sh|nohup|\./|source )[^|]*refresh_data\.sh'; then
  delegate block-long-pipeline-run-unchecked.sh
fi
check_wrap

# ============================================================================
# Aggregate and emit a single hookSpecificOutput (precedence deny > ask > allow).
# ============================================================================
join_reasons() {
  local sep="" r
  for r in "$@"; do printf '%s%s' "$sep" "$r"; sep=$'\n\n'; done
}

if [ "${#DENY_REASONS[@]}" -gt 0 ]; then
  reason=$(join_reasons "${DENY_REASONS[@]}")
  jq -n --arg r "$reason" '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"deny",permissionDecisionReason:$r}}'
elif [ "${#ASK_REASONS[@]}" -gt 0 ]; then
  reason=$(join_reasons "${ASK_REASONS[@]}")
  if [ -n "$UPDATED_CMD" ]; then
    jq -nc --arg r "$reason" --argjson input "$(updated_input_json)" '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"ask",permissionDecisionReason:$r,updatedInput:$input}}'
  else
    jq -n --arg r "$reason" '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"ask",permissionDecisionReason:$r}}'
  fi
elif [ -n "$ALLOW_CONTEXT" ]; then
  if [ -n "$UPDATED_CMD" ]; then
    jq -nc --arg ctx "$ALLOW_CONTEXT" --argjson input "$(updated_input_json)" '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"allow",additionalContext:$ctx,updatedInput:$input}}'
  else
    jq -n --arg ctx "$ALLOW_CONTEXT" '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"allow",additionalContext:$ctx}}'
  fi
elif [ -n "$UPDATED_CMD" ]; then
  jq -nc --argjson input "$(updated_input_json)" '{hookSpecificOutput:{hookEventName:"PreToolUse",updatedInput:$input}}'
fi
exit 0
