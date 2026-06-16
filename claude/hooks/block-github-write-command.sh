#!/bin/bash
# PreToolUse hook: block GitHub write operations outside an explicit allowlist.
#
# This is a hard gate for the incident class where an agent creates PRs,
# pushes branches, sets secrets, or otherwise mutates an organization repo
# after inferring permission from context. Read-only GitHub inspection remains
# allowed. Write operations are allowed only when the target repo appears in
# agents/github-write-allowlist.txt.
#
# Matcher: Bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
INPUT=$(cat)

TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')
[ "$TOOL_NAME" = "Bash" ] || exit 0

COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
[ -n "$COMMAND" ] || exit 0

ALLOWLIST="$SCRIPT_DIR/../../agents/github-write-allowlist.txt"

deny() {
  local label="$1"
  local detail="$2"
  jq -n --arg label "$label" --arg detail "$detail" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": ("BLOCKED: " + $label + ".\n\n" + $detail + "\n\nGitHub writes are allowed only for repos listed in `~/My Drive/dotfiles/agents/github-write-allowlist.txt`. The allowlist is empty by default and should contain only repos Pablo personally created.")
    }
  }'
  exit 0
}

normalize_repo() {
  local value="$1"
  value="${value#https://github.com/}"
  value="${value#http://github.com/}"
  value="${value#ssh://git@github.com/}"
  value="${value#git@github.com:}"
  value="${value%.git}"
  value="${value%%/}"
  printf '%s' "$value" | tr '[:upper:]' '[:lower:]'
}

repo_from_urlish() {
  local text="$1"
  if [[ "$text" =~ github\.com[:/]([A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+)(\.git)? ]]; then
    normalize_repo "${BASH_REMATCH[1]}"
    return 0
  fi
  return 1
}

repo_from_gh_repo_flag() {
  if [[ "$COMMAND" =~ (^|[[:space:]])(--repo|-R)(=|[[:space:]]+)([A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+) ]]; then
    normalize_repo "${BASH_REMATCH[4]}"
    return 0
  fi
  repo_from_urlish "$COMMAND" || true
}

repo_from_gh_api_endpoint() {
  if [[ "$COMMAND" =~ (^|[[:space:]])repos/([A-Za-z0-9_.-]+)/([A-Za-z0-9_.-]+)(/|[[:space:]]|$) ]]; then
    normalize_repo "${BASH_REMATCH[2]}/${BASH_REMATCH[3]}"
    return 0
  fi
  return 1
}

repo_from_local_git() {
  # shellcheck source=lib-repo-root.sh
  source "$SCRIPT_DIR/lib-repo-root.sh"
  [ -n "${REPO_ROOT:-}" ] || return 1

  local remote url repo
  for remote in origin upstream; do
    url=$(git remote get-url "$remote" 2>/dev/null || true)
    [ -n "$url" ] || continue
    repo=$(repo_from_urlish "$url" || true)
    if [ -n "$repo" ]; then
      printf '%s' "$repo"
      return 0
    fi
  done

  while IFS= read -r url; do
    repo=$(repo_from_urlish "$url" || true)
    if [ -n "$repo" ]; then
      printf '%s' "$repo"
      return 0
    fi
  done < <(git remote -v 2>/dev/null | awk '{print $2}' | sort -u)

  return 1
}

repo_allowed_p() {
  local repo
  repo=$(normalize_repo "$1")
  [ -f "$ALLOWLIST" ] || return 1
  awk '
    /^[[:space:]]*($|#)/ { next }
    {
      repo=$1
      sub(/#.*/, "", repo)
      gsub(/[[:space:]]/, "", repo)
      gsub(/\.git$/, "", repo)
      print tolower(repo)
    }
  ' "$ALLOWLIST" | grep -Fxq "$repo"
}

require_allowed_repo() {
  local action="$1"
  local repo="$2"
  if [ -z "$repo" ]; then
    deny "$action has no unambiguous repository target" "The guard blocks ambiguous GitHub writes. Make the target repo explicit and add it to the allowlist only if Pablo personally created it."
  fi
  if ! repo_allowed_p "$repo"; then
    deny "$action targets non-allowlisted repo $repo" "Do not infer write permission from org membership, affected-repo context, maintainer requests, or a general \"proceed\"."
  fi
}

target_repo_for_gh() {
  local repo
  repo=$(repo_from_gh_repo_flag || true)
  if [ -n "$repo" ]; then
    printf '%s' "$repo"
    return 0
  fi
  repo=$(repo_from_local_git || true)
  [ -n "$repo" ] && printf '%s' "$repo"
}

target_repo_for_api() {
  local repo
  repo=$(repo_from_gh_api_endpoint || true)
  if [ -n "$repo" ]; then
    printf '%s' "$repo"
    return 0
  fi
  target_repo_for_gh
}

is_gh_api_write() {
  echo "$COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+api\b' || return 1

  if echo "$COMMAND" | grep -qE '(^|[[:space:]])(--method|-X)(=|[[:space:]]+)(POST|PUT|PATCH|DELETE)\b'; then
    return 0
  fi
  if echo "$COMMAND" | grep -qE '(^|[[:space:]])(DELETE|PATCH|POST|PUT)\b'; then
    return 0
  fi
  if echo "$COMMAND" | grep -qE '(^|[[:space:]])graphql([[:space:]]|$)' && \
     echo "$COMMAND" | grep -qE '\bmutation\b'; then
    return 0
  fi
  if echo "$COMMAND" | grep -qE '(^|[[:space:]])(-f|-F|--field|--raw-field|--input)(=|[[:space:]]+)'; then
    if echo "$COMMAND" | grep -qE '(^|[[:space:]])(--method|-X)(=|[[:space:]]+)GET\b'; then
      return 1
    fi
    if echo "$COMMAND" | grep -qE '(^|[[:space:]])graphql([[:space:]]|$)' && \
       ! echo "$COMMAND" | grep -qE '\bmutation\b'; then
      return 1
    fi
    return 0
  fi
  return 1
}

contains_protected_guard_path() {
  echo "$COMMAND" | grep -qE '(agents/github-write-allowlist\.txt|codex/(hooks/block-github-write-command\.sh|hooks/block-github-guard-edit\.sh|hooks\.json)|claude/(hooks/block-github-write-command\.sh|hooks/block-github-guard-edit\.sh|hooks/pretooluse-bash\.sh)|\.codex/hooks\.json|\.claude/settings\.json)'
}

if contains_protected_guard_path && \
   echo "$COMMAND" | grep -qE '(^|[[:space:];|&])(rm|trash|mv|cp|install|sed[[:space:]].*-i|perl[[:space:]].*-pi|git[[:space:]]+(restore|checkout))[[:space:]]|>>?|tee[[:space:]]'; then
  deny "attempt to modify GitHub write-guard files" "Those files are self-protected. Edit them manually outside Claude if the policy needs to change."
fi

if echo "$COMMAND" | grep -qE '(^|[[:space:];|&])git[[:space:]]+push\b'; then
  if echo "$COMMAND" | grep -qE '(^|[[:space:]])--dry-run([[:space:]]|$)'; then
    exit 0
  fi
  repo=$(repo_from_urlish "$COMMAND" || true)
  if [ -z "$repo" ]; then
    repo=$(repo_from_local_git || true)
  fi
  require_allowed_repo "git push" "$repo"
fi

if echo "$COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+pr[[:space:]]+(create|close|reopen|merge|comment|review|edit|ready|lock|unlock|update-branch)\b'; then
  require_allowed_repo "gh pr write operation" "$(target_repo_for_gh)"
fi

if echo "$COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+issue[[:space:]]+(create|close|reopen|comment|edit|lock|unlock|transfer|delete|pin|unpin|develop)\b'; then
  require_allowed_repo "gh issue write operation" "$(target_repo_for_gh)"
fi

if echo "$COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+(secret|variable)[[:space:]]+(set|delete|remove)\b'; then
  if echo "$COMMAND" | grep -qE '(^|[[:space:]])--(org|env|app)(=|[[:space:]]+)'; then
    deny "organization/environment/app GitHub secret or variable mutation" "This operation is not repo-scoped, so the repo allowlist cannot authorize it."
  fi
  require_allowed_repo "gh secret/variable write operation" "$(target_repo_for_gh)"
fi

if echo "$COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+workflow[[:space:]]+(run|enable|disable)\b'; then
  require_allowed_repo "gh workflow write operation" "$(target_repo_for_gh)"
fi

if echo "$COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+run[[:space:]]+(cancel|delete|rerun)\b'; then
  require_allowed_repo "gh run write operation" "$(target_repo_for_gh)"
fi

if echo "$COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+release[[:space:]]+(create|delete|edit|upload)\b'; then
  require_allowed_repo "gh release write operation" "$(target_repo_for_gh)"
fi

if echo "$COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+repo[[:space:]]+(create|delete|edit|rename|archive|unarchive|sync)\b'; then
  require_allowed_repo "gh repo write operation" "$(target_repo_for_gh)"
fi

if echo "$COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+(label|milestone)[[:space:]]+(create|delete|edit)\b'; then
  require_allowed_repo "gh label/milestone write operation" "$(target_repo_for_gh)"
fi

if echo "$COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+gist[[:space:]]+(create|delete|edit)\b'; then
  deny "gh gist write operation" "Gists are not repo-scoped, so the repo allowlist cannot authorize them."
fi

if is_gh_api_write; then
  require_allowed_repo "gh api write operation" "$(target_repo_for_api)"
fi

exit 0
