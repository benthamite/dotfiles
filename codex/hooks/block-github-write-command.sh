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
      "permissionDecisionReason": ("BLOCKED: " + $label + ".\n\n" + $detail + "\n\nGitHub writes are allowed only when the target matches an exact OWNER/REPO entry or OWNER/* account wildcard in `~/My Drive/dotfiles/agents/github-write-allowlist.txt`, or is declared by an Epoch project via :REPOS: and committed to the automations registry.")
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

# Repos declared by Epoch project docs, resolved at decision time.
#
# Ownership lives in each project's :REPOS: drawer property and is collected
# into the `repos` field of the registry below by `make import`. Consulting it
# here is what keeps the allowlist from needing a second copy of that list:
# declaring a repo on the project that owns it is enough to make it writable.
#
# The committed version is read, never the working tree. An agent can edit a
# tracked file freely, so trusting the working tree would let a blocked agent
# add its own target and retry. Requiring a commit means widening this gate
# always leaves a trail in history.
#
# The paths are fixed rather than overridable by environment variable, which
# would let a caller point this at a registry it controls -- the same bypass.
DECLARED_REPOS_REPO="$HOME/My Drive/Epoch/projects/automations-dashboard/repo"
DECLARED_REPOS_PATH="data/automations.json"

declared_repos() {
  [ -d "$DECLARED_REPOS_REPO" ] || return 0
  git -C "$DECLARED_REPOS_REPO" show "HEAD:$DECLARED_REPOS_PATH" 2>/dev/null \
      | jq -r '.projects[]?.repos[]? // empty' 2>/dev/null \
      | tr '[:upper:]' '[:lower:]'
}

repo_allowed_p() {
  local repo pattern
  repo=$(normalize_repo "$1")
  if [ -f "$ALLOWLIST" ]; then
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
  fi

  # Declared entries are exact repos, never wildcards.
  while IFS= read -r pattern; do
    [ "$repo" = "$pattern" ] && return 0
  done < <(declared_repos)

  return 1
}

require_allowed_repo() {
    local action="$1"
    local repo="$2"
    if [ -z "$repo" ]; then
	deny "$action has no unambiguous repository target" "The guard blocks ambiguous GitHub writes. Make the target repo explicit; use the allowlist only after Pablo explicitly authorizes agent writes to it."
    fi
    if ! repo_allowed_p "$repo"; then
	deny "$action targets non-allowlisted repo $repo" "Do not infer write permission from org membership, affected-repo context, maintainer requests, or a general \"proceed\". If this is one of Pablo's own repos, declare it in the owning project's :REPOS: drawer property, re-run 'make import' in automations-dashboard, and commit the registry -- declarations are read from the committed registry, so an uncommitted one has no effect."
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
# A protected path merely *mentioned* is not a modification of it. This used to
# be two independent tests — does the command name a protected path anywhere, and
# does it contain a redirection or file-mutating verb anywhere — so any '>' made
# the second one true. On 2026-07-31 a commit was refused because its message
# contained the phrase "deny > allow" while it also staged one of these files by
# name, which is a read plus prose and not a write at all.
#
# The operator now has to name the path. That removes the false-positive class and
# is strictly more precise: an operator somewhere else in the command was never
# evidence about this path. [^|;&]* keeps the match inside one command segment, so
# a redirection before a separator cannot pair with a path after it, and it
# tolerates quotes and backslash-escaped spaces in the path that a
# character-class approach would have missed.
#
# Deliberately NOT solved with the quoted-content masking used by
# block-destructive-command.sh: that is ~90 lines of awk, a third hand-synced copy
# would be a liability in a hard gate, and factoring it into a shared library that
# is not itself self-protected would let an agent neuter all three guards by
# editing the library.
PROTECTED_GUARD_PATH_RE='(agents/github-write-allowlist\.txt|codex/(hooks/block-github-write-command\.sh|hooks/block-github-guard-edit\.sh|hooks\.json)|claude/(hooks/block-github-write-command\.sh|hooks/block-github-guard-edit\.sh|hooks/pretooluse-bash\.sh)|\.codex/hooks\.json|\.claude/settings\.json)'

guard_modification_p() {
    local sanitized
    sanitized=$(printf '%s' "$COMMAND" \
	| sed -E 's/[0-9]*>>?[[:space:]]*&[0-9-]+//g' \
	| sed -E 's/[0-9]*>>?[[:space:]]*\/dev\/null//g')

    # Redirection whose target is a protected path.
    printf '%s' "$sanitized" \
	| grep -qE ">>?[^|;&]*${PROTECTED_GUARD_PATH_RE}" && return 0

    # File-mutating verbs naming a protected path as an argument.
    printf '%s' "$sanitized" \
	| grep -qE "(^|[[:space:];|&])(rm|trash|mv|cp|install|tee)[[:space:]][^|;&]*${PROTECTED_GUARD_PATH_RE}" && return 0

    # In-place editors, which need their flag before the path.
    printf '%s' "$sanitized" \
	| grep -qE "(^|[[:space:];|&])sed[[:space:]][^|;&]*-i[^|;&]*${PROTECTED_GUARD_PATH_RE}" && return 0
    printf '%s' "$sanitized" \
	| grep -qE "(^|[[:space:];|&])perl[[:space:]][^|;&]*-pi[^|;&]*${PROTECTED_GUARD_PATH_RE}" && return 0

    # git commands that overwrite a working-tree file.
    printf '%s' "$sanitized" \
	| grep -qE "(^|[[:space:];|&])git[[:space:]]+(restore|checkout)[[:space:]][^|;&]*${PROTECTED_GUARD_PATH_RE}" && return 0

    return 1
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
