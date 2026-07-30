#!/bin/bash
# PreToolUse hook: block GitHub write operations outside an explicit allowlist.
#
# This is a hard gate for the incident class where an agent creates PRs,
# pushes branches, sets secrets, or otherwise mutates an organization repo
# after inferring permission from context. Read-only GitHub inspection remains
# allowed. Write operations are allowed when the target repo matches an exact
# OWNER/REPO entry or an OWNER/* account wildcard in
# agents/github-write-allowlist.txt.
#
# Matcher: Bash|exec_command|functions.exec|functions.exec_command

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-hook-json.sh
source "$SCRIPT_DIR/lib-codex-hook-json.sh"

INPUT=$(cat)

TOOL_NAME=$(codex_tool_name "$INPUT")
codex_shell_tool_p "$TOOL_NAME" || exit 0

CMD=$(codex_shell_command "$INPUT")
[ -n "$CMD" ] || exit 0
COMMAND="$CMD"

ALLOWLIST="$SCRIPT_DIR/../../agents/github-write-allowlist.txt"

deny() {
  local label="$1"
  local detail="$2"
  jq -n --arg label "$label" --arg detail "$detail" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": ("BLOCKED: " + $label + ".\n\n" + $detail + "\n\nGitHub writes are allowed only when the target matches an exact OWNER/REPO entry or OWNER/* account wildcard in `~/My Drive/dotfiles/agents/github-write-allowlist.txt`.")
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
  if [[ "$CMD" =~ (^|[[:space:]])(--repo|-R)(=|[[:space:]]+)([A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+) ]]; then
    normalize_repo "${BASH_REMATCH[4]}"
    return 0
  fi
  repo_from_urlish "$CMD" || true
}

repo_from_gh_api_endpoint() {
  if [[ "$CMD" =~ (^|[[:space:]])repos/([A-Za-z0-9_.-]+)/([A-Za-z0-9_.-]+)(/|[[:space:]]|$) ]]; then
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
  local repo pattern
  repo=$(normalize_repo "$1")
  [ -f "$ALLOWLIST" ] || return 1
  while IFS= read -r pattern; do
      case "$pattern" in
	  */\*)
              [[ "$repo" == "${pattern%\*}"* ]] && return 0
              ;;
	  *)
              [ "$repo" = "$pattern" ] && return 0
              ;;
      esac
  done < <(awk '
    /^[[:space:]]*($|#)/ { next }
    {
      repo=$1
      sub(/#.*/, "", repo)
      gsub(/[[:space:]]/, "", repo)
      gsub(/\.git$/, "", repo)
      print tolower(repo)
    }
  ' "$ALLOWLIST")
  return 1
}

require_allowed_repo() {
    local action="$1"
    local repo="$2"
    if [ -z "$repo" ]; then
	deny "$action has no unambiguous repository target" "The guard blocks ambiguous GitHub writes. Make the target repo explicit; use the allowlist only after Pablo explicitly authorizes agent writes to it."
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
    echo "$CMD" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+api\b' || return 1

    # Explicit read methods are allowed unless write fields are present without
    # --method GET. `gh api graphql -f query=...` is read-only unless the query
    # includes a GraphQL mutation.
    if echo "$CMD" | grep -qE '(^|[[:space:]])(--method|-X)(=|[[:space:]]+)(POST|PUT|PATCH|DELETE)\b'; then
	return 0
    fi
    if echo "$CMD" | grep -qE '(^|[[:space:]])(DELETE|PATCH|POST|PUT)\b'; then
	return 0
    fi
    if echo "$CMD" | grep -qE '(^|[[:space:]])graphql([[:space:]]|$)' && \
	    echo "$CMD" | grep -qE '\bmutation\b'; then
	return 0
    fi
    if echo "$CMD" | grep -qE '(^|[[:space:]])(-f|-F|--field|--raw-field|--input)(=|[[:space:]]+)'; then
	if echo "$CMD" | grep -qE '(^|[[:space:]])(--method|-X)(=|[[:space:]]+)GET\b'; then
	    return 1
	fi
	if echo "$CMD" | grep -qE '(^|[[:space:]])graphql([[:space:]]|$)' && \
		! echo "$CMD" | grep -qE '\bmutation\b'; then
	    return 1
	fi
	return 0
    fi
    return 1
}

# Extract the single literal directory this command will run in, if it has one.
# Prints the path and returns 0. Returns 1 when the command changes directory in
# a way this guard cannot resolve without evaluating the shell, and 2 when it
# changes no directory at all, so the ambient directory is the right answer.
resolved_run_dir() {
    local changes=0 target=""

    changes=$(printf '%s\n' "$COMMAND" \
	| grep -oE '(^|[[:space:];|&(])(cd|pushd)[[:space:]]' \
	| grep -c . || true)

    local dash_c=""
    if [[ "$COMMAND" =~ (^|[[:space:];|\&])git[[:space:]]+-C[[:space:]]+([^[:space:];\|\&]+) ]]; then
	dash_c="${BASH_REMATCH[2]}"
    fi

    if [ "$changes" -eq 0 ] && [ -z "$dash_c" ]; then
	return 2
    fi
    if [ "$changes" -gt 1 ]; then
	return 1
    fi

    if [ -n "$dash_c" ]; then
	target="$dash_c"
    elif [[ "$COMMAND" =~ (^|[[:space:];|\&])(cd|pushd)[[:space:]]+(.*) ]]; then
	target="${BASH_REMATCH[3]}"
	# Keep only this command, not the rest of the chain.
	target="${target%%&&*}"
	target="${target%%;*}"
	target="${target%%|*}"
	# Undo quoting so a path containing spaces still resolves. Handles
	# "a b", 'a b', ~/"a b" and a\ b alike.
	target="${target//\"/}"
	target="${target//\'/}"
	target="${target//\\ / }"
	# Trim trailing whitespace.
	target="${target%"${target##*[![:space:]]}"}"
    else
	return 1
    fi

    # $HOME and ~ are unambiguous, so expand them rather than refusing.
    target="${target//\$\{HOME\}/$HOME}"
    target="${target//\$HOME/$HOME}"
    target="${target/#\~/$HOME}"

    # Anything still needing shell evaluation is not a literal path.
    case "$target" in
	*'$'* | *'`'* | *'*'* | *'?'*) return 1 ;;
    esac

    [ -d "$target" ] || return 1
    printf '%s' "$target"
}

# Read the GitHub repo of the remote in a specific directory.
repo_from_git_dir() {
    local dir="$1" remote url repo
    for remote in origin upstream; do
	url=$(git -C "$dir" remote get-url "$remote" 2>/dev/null || true)
	[ -n "$url" ] || continue
	repo=$(repo_from_urlish "$url" || true)
	if [ -n "$repo" ]; then
	    printf '%s' "$repo"
	    return 0
	fi
    done
    return 1
}

# True when the command looks like it would modify a guard file. Redirects that
# cannot write to a file -- fd duplications and writes to /dev/null -- are
# stripped first, so an ordinary read carrying 2>/dev/null is not mistaken for
# an edit.
guard_modification_p() {
    local sanitized
    sanitized=$(printf '%s' "$COMMAND" \
	| sed -E 's/[0-9]*>>?[[:space:]]*&[0-9-]+//g' \
	| sed -E 's/[0-9]*>>?[[:space:]]*\/dev\/null//g')
    printf '%s' "$sanitized" \
	| grep -qE '(^|[[:space:];|&])(rm|trash|mv|cp|install|sed[[:space:]].*-i|perl[[:space:]].*-pi|git[[:space:]]+(restore|checkout))[[:space:]]|>>?|tee[[:space:]]'
}

contains_protected_guard_path() {
  echo "$CMD" | grep -qE '(agents/github-write-allowlist\.txt|codex/(hooks/block-github-write-command\.sh|hooks/block-github-guard-edit\.sh|hooks\.json)|claude/(hooks/block-github-write-command\.sh|hooks/block-github-guard-edit\.sh|hooks/pretooluse-bash\.sh)|\.codex/hooks\.json|\.claude/settings\.json)'
}

if contains_protected_guard_path && guard_modification_p; then
  deny "attempt to modify GitHub write-guard files" "Those files are self-protected. Edit them manually outside Codex if the policy needs to change."
fi

if echo "$CMD" | grep -qE '(^|[[:space:];|&])git[[:space:]]+push\b'; then
  if echo "$CMD" | grep -qE '(^|[[:space:]])--dry-run([[:space:]]|$)'; then
    exit 0
  fi
  repo=$(repo_from_urlish "$CMD" || true)
  if [ -z "$repo" ]; then
    # Resolve the directory the push will run in. Never fall back to the
    # ambient directory when the command moves somewhere else first: that
    # reads the wrong repo's remote and can approve a push to a repo the
    # user has no rights over.
    run_dir=$(resolved_run_dir) || run_dir_status=$?
    case "${run_dir_status:-0}" in
      1)
        deny "git push target directory cannot be resolved" "The command changes directory and does not name a repository, so this guard cannot tell which repo the push goes to without evaluating the shell. Name the target explicitly, e.g. git push https://github.com/OWNER/REPO.git HEAD:BRANCH."
        ;;
      2)
        repo=$(repo_from_local_git || true)
        ;;
      *)
        repo=$(repo_from_git_dir "$run_dir" || true)
        ;;
    esac
  fi
  require_allowed_repo "git push" "$repo"
fi

if echo "$CMD" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+pr[[:space:]]+(create|close|reopen|merge|comment|review|edit|ready|lock|unlock|update-branch)\b'; then
  require_allowed_repo "gh pr write operation" "$(target_repo_for_gh)"
fi

if echo "$CMD" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+issue[[:space:]]+(create|close|reopen|comment|edit|lock|unlock|transfer|delete|pin|unpin|develop)\b'; then
  require_allowed_repo "gh issue write operation" "$(target_repo_for_gh)"
fi

if echo "$CMD" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+(secret|variable)[[:space:]]+(set|delete|remove)\b'; then
  if echo "$CMD" | grep -qE '(^|[[:space:]])--(org|env|app)(=|[[:space:]]+)'; then
    deny "organization/environment/app GitHub secret or variable mutation" "This operation is not repo-scoped, so the repo allowlist cannot authorize it."
  fi
  require_allowed_repo "gh secret/variable write operation" "$(target_repo_for_gh)"
fi

if echo "$CMD" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+workflow[[:space:]]+(run|enable|disable)\b'; then
  require_allowed_repo "gh workflow write operation" "$(target_repo_for_gh)"
fi

if echo "$CMD" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+run[[:space:]]+(cancel|delete|rerun)\b'; then
  require_allowed_repo "gh run write operation" "$(target_repo_for_gh)"
fi

if echo "$CMD" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+release[[:space:]]+(create|delete|edit|upload)\b'; then
  require_allowed_repo "gh release write operation" "$(target_repo_for_gh)"
fi

if echo "$CMD" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+repo[[:space:]]+(create|delete|edit|rename|archive|unarchive|sync)\b'; then
  require_allowed_repo "gh repo write operation" "$(target_repo_for_gh)"
fi

if echo "$CMD" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+(label|milestone)[[:space:]]+(create|delete|edit)\b'; then
  require_allowed_repo "gh label/milestone write operation" "$(target_repo_for_gh)"
fi

if echo "$CMD" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+gist[[:space:]]+(create|delete|edit)\b'; then
  deny "gh gist write operation" "Gists are not repo-scoped, so the repo allowlist cannot authorize them."
fi

if is_gh_api_write; then
  require_allowed_repo "gh api write operation" "$(target_repo_for_api)"
fi

exit 0
