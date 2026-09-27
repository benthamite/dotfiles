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
# Matcher: Bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-heredoc.sh
source "$SCRIPT_DIR/lib-heredoc.sh"
INPUT=$(cat)

TOOL_NAME=$(printf '%s' "$INPUT" | jq -r '.tool_name // empty')
[ "$TOOL_NAME" = "Bash" ] || exit 0

COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')
[ -n "$COMMAND" ] || exit 0

# Quoted commit messages and heredoc bodies fed to data sinks are data, not
# commands: a note that mentions a push or a pull request must not trip the
# write detection below. Target extraction still reads the raw command, and
# a heredoc fed to a shell or interpreter stays in the scan (lib-heredoc.sh).
SCAN_COMMAND=$(mask_git_commit_messages "$(mask_heredoc_bodies "$COMMAND")")
# A shell's `-c` argument is a command, not data: drop the quote after `-c`
# so `sh -c 'git push …'` is read as the push it is. (The detectors below
# require a separator before `git`/`gh`, so a quoted body used to hide it.)
SCAN_COMMAND=$(printf '%s' "$SCAN_COMMAND" | sed -E "s/((^|[[:space:];|&(])(bash|sh|zsh|dash|ksh)[[:space:]]+-[a-zA-Z]*c[a-zA-Z]*[[:space:]]+)['\"]/\1/g")

# Read from the committed blob, never the working tree: an agent that is
# blocked can append its own target to a tracked file and retry, as one did on
# 2026-08-31. The registry below is read the same way, for the same reason.
ALLOWLIST_REPO="$SCRIPT_DIR/../.."
ALLOWLIST_PATH="agents/github-write-allowlist.txt"

allowlist_entries() {
    git -C "$ALLOWLIST_REPO" show "HEAD:$ALLOWLIST_PATH" 2>/dev/null
}

deny() {
    local label="$1"
    local detail="$2"
    jq -n --arg label "$label" --arg detail "$detail" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": ("BLOCKED: " + $label + ".\n\n" + $detail + "\n\nAfter explicit user authorization, supported fork/PR commands may use committed scoped grants (agents/github-operation-authorizations.md). Otherwise GitHub writes require a target that matches an exact OWNER/REPO entry or OWNER/* account wildcard in the committed `~/My Drive/dotfiles/agents/github-write-allowlist.txt` (working-tree edits do not count), or is declared by an Epoch project via :REPOS: and committed to the automations registry.")
    }
  }'
    exit 0
}

# Scoped contribution grants are separate from broad repository write access.
# The checker reads committed, expiring records and never executes the command.
operation_status=0
printf '%s' "$COMMAND" | python3 "$SCRIPT_DIR/../../bin/github-operation-authorization" check || operation_status=$?
case "$operation_status" in
  0) exit 0 ;;
  1) ;;
  *) deny "scoped GitHub authorization check failed" "Repair or remove the invalid grant; do not bypass the guard." ;;
esac

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

repo_from_gh_repo_env() {
    local env_command
    env_command=$(printf '%s' "$COMMAND" | sed -E "s/GH_REPO=['\"]([^'\"]+)['\"]/GH_REPO=\\1/g")
    if [[ "$env_command" =~ (^|[[:space:];|&])GH_REPO=([A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+)($|[[:space:]]) ]]; then
	normalize_repo "${BASH_REMATCH[2]}"
	return 0
    fi
    return 1
}

# `gh repo` names its target positionally rather than through --repo. Without
# this the target falls through to the surrounding checkout's remote, which
# both denies allowlisted targets outside a checkout and lets an unrelated
# OWNER/REPO inherit the ambient repo's authorization.
repo_from_gh_repo_positional() {
    if [[ "$COMMAND" =~ (^|[[:space:]])gh[[:space:]]+repo[[:space:]]+(create|delete|edit|rename|archive|unarchive|sync)[[:space:]]+([A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+) ]]; then
    	normalize_repo "${BASH_REMATCH[3]}"
    	return 0
    fi
    return 1
}

# The endpoint is commonly written `/repos/…` or quoted, so anchor on any
# character that cannot be part of the path rather than on whitespace alone.
repo_from_gh_api_endpoint() {
    if [[ "$COMMAND" =~ (^|[^A-Za-z0-9_.-])/?repos/([A-Za-z0-9_.-]+)/([A-Za-z0-9_.-]+)($|[^A-Za-z0-9_.-]) ]]; then
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
DECLARED_REPOS_REPO="$HOME/repos/epoch/automations-dashboard"
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
  ' < <(allowlist_entries))

    # Declared entries are exact repos, never wildcards.
    while IFS= read -r pattern; do
        [ "$repo" = "$pattern" ] && return 0
    done < <(declared_repos)

    return 1
}

# Target extraction is command-wide. A compound command could let an unrelated
# or earlier segment lend its repository to a write, so every write path
# refuses one before anything else; read-only GitHub commands may be piped or
# chained freely.
deny_if_compound() {
    if compound_shell_command_p; then
	deny "compound GitHub command has no segment-local repository target" "Run each GitHub write in a separate tool call so the guard can bind it to exactly one repository."
    fi
}

require_allowed_repo() {
    deny_if_compound
    require_allowed_segment_repo "$@"
}

# The allowlist check alone, for a caller that has already bound REPO to the
# one segment that writes.
require_allowed_segment_repo() {
    local action="$1"
    local repo="$2"
    if [ -z "$repo" ]; then
	deny "$action has no unambiguous repository target" "The guard blocks ambiguous GitHub writes. Make the target repo explicit and add it to the allowlist only if Pablo personally created it."
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
    repo=$(repo_from_gh_repo_env || true)
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
    echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+api\b' || return 1

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
    if [[ "$COMMAND" =~ (^|[[:space:];|\&])([^[:space:];|\&]*/)?git[[:space:]]+-C[[:space:]]+([^[:space:];\|\&]+) ]]; then
	dash_c="${BASH_REMATCH[3]}"
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

# Accept Git's executable path and global options before the subcommand. These
# are ordinary Git forms, for example `/usr/bin/git push` and
# `git -C /path push`.
git_push_command_p() {
    local git_re
    git_re="(^|[[:space:];|&])(\"[^\"]*/git\"|'[^']*/git'|[^[:space:];|&]*/git|git)([[:space:]]+(-C|-c|--git-dir|--work-tree|--namespace)(=|[[:space:]]+)[^[:space:];|&]+|[[:space:]]+-[pP]|[[:space:]]+--(paginate|no-pager|bare|no-replace-objects|literal-pathspecs|glob-pathspecs|noglob-pathspecs|icase-pathspecs))*[[:space:]]+push\\b"
    printf '%s' "$SCAN_COMMAND" | grep -qE "$git_re"
}

compound_shell_command_p() {
    printf '%s' "$COMMAND" | python3 -c '
import sys

import shlex

# A write piped only into these readers of its output keeps one target: they
# cannot run another command, write a file or name a repository.
OUTPUT_FILTERS = {"tail", "head", "grep", "wc", "cat"}


def output_filters_p(rest):
    """Return non-nil when REST is a chain of plain output filters."""
    if any(char in rest for char in ";&<>$`\\\n(){}"):
        return False
    for segment in rest.split("|"):
        try:
            words = shlex.split(segment)
        except ValueError:
            return False
        if not words or words[0] not in OUTPUT_FILTERS:
            return False
    return True


source = sys.stdin.read()
quote = None
escaped = False
for index, char in enumerate(source):
    if escaped:
        escaped = False
        continue
    if char == "\\":
        escaped = True
        continue
    if quote is not None:
        if char == quote:
            quote = None
        continue
    if char in ("\"", chr(39), "`"):
        quote = char
        continue
    if char == "&" and source[index - 1:index] == ">":
        continue
    if char == "|" and source[index + 1:index + 2] not in ("|", "&"):
        head = source[:index].rstrip()
        if head.endswith("2>&1"):
            head = head[:-4]
        if "&" not in head and output_filters_p(source[index + 1:]):
            raise SystemExit(1)
        raise SystemExit(0)
    if char in (";", "|", "&", "\n"):
        raise SystemExit(0)
raise SystemExit(1)
'
}

# The one compound write this guard accepts: the sanctioned
# `BROKER read REF | gh secret set NAME -R OWNER/REPO` pipeline from
# context/secrets.md. The first stage runs no GitHub command and names no
# repository, so the second stage's literal -R/--repo is the only target and
# nothing can be lent to it. Anything beyond exactly these two stages (another
# pipe, `;`, `&&`, `||`, a subshell, a substitution, a redirect, an assignment,
# a comment, a glob, any other gh or git command, any other gh secret flag)
# fails the match and meets the ordinary compound denial. Prints the lowercase
# OWNER/REPO and exits 0 on a match, exits 1 otherwise.
broker_secret_pipeline_repo() {
    printf '%s' "$COMMAND" | python3 -c '
import re
import shlex
import sys

source = sys.stdin.read()
if any(char in source for char in ";&<>()$`\\\n{}#*?[]~!") or source.count("|") != 1:
    raise SystemExit(1)
first, second = source.split("|")
try:
    reader = shlex.split(first)
    writer = shlex.split(second)
except ValueError:
    raise SystemExit(1)

REF = re.compile(r"op://[^\s]+")
ACCOUNTS = {"@epoch", "@personal", "@tlon"}
# `read` options that change neither where the value goes nor which item is
# read. --no-newline matters here: without it the trailing newline becomes
# part of the stored secret.
NO_NEWLINE = {"--no-newline", "-n"}


def read_args_ok(rest, allow_account):
    """True when `rest` (after `read`) is only harmless options and one REF."""
    while rest:
        if rest[0] in NO_NEWLINE:
            rest = rest[1:]
        elif allow_account and rest[0] == "--account" and len(rest) >= 2:
            rest = rest[2:]
        elif allow_account and rest[0].startswith("--account="):
            rest = rest[1:]
        else:
            break
    return len(rest) == 1 and bool(REF.fullmatch(rest[0]))


if reader[:1] == ["op-automations"]:
    rest = reader[1:]
    if rest[:1] and rest[0] in ACCOUNTS:
        rest = rest[1:]
    reader_ok = rest[:1] == ["read"] and read_args_ok(rest[1:], allow_account=False)
elif reader[:1] == ["op-desktop"]:
    rest = reader[1:]
    reader_ok = rest[:1] == ["read"] and read_args_ok(rest[1:], allow_account=True)
else:
    reader_ok = False
if not reader_ok:
    raise SystemExit(1)

if writer[:3] != ["gh", "secret", "set"]:
    raise SystemExit(1)
REPO = re.compile(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+")
names, repos, index, args = [], [], 0, writer[3:]
while index < len(args):
    word = args[index]
    if word in ("-R", "--repo"):
        if index + 1 >= len(args):
            raise SystemExit(1)
        repos.append(args[index + 1])
        index += 2
        continue
    if word.startswith("--repo="):
        repos.append(word[len("--repo="):])
    elif word.startswith("-"):
        raise SystemExit(1)
    else:
        names.append(word)
    index += 1
if len(names) != 1 or not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", names[0]):
    raise SystemExit(1)
if len(repos) != 1 or not REPO.fullmatch(repos[0]):
    raise SystemExit(1)
print(repos[0].lower(), end="")
'
}

# The 1Password classifier must approve the same command; otherwise the
# pipeline gets no exception here and falls through to the compound denial.
if pipeline_repo=$(broker_secret_pipeline_repo) &&
	printf '%s' "$COMMAND" | python3 "$SCRIPT_DIR/lib-op-policy.py" | grep -q '"decision": "allow"'; then
    require_allowed_segment_repo "gh secret/variable write operation" "$pipeline_repo"
    exit 0
fi

if git_push_command_p; then
    # The dry-run shortcut reads the whole command, so a compound command
    # could pair a dry run with a real push; refuse it before the shortcut.
    deny_if_compound
    if echo "$COMMAND" | grep -qE '(^|[[:space:]])--dry-run([[:space:]]|$)'; then
	exit 0
    fi
    repo=$(repo_from_urlish "$COMMAND" || true)
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

# All mutating PR and issue operations require an allowed repository. Read-only
# view, list, status, checks, and diff operations remain allowed by omission.
if echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+pr[[:space:]]+(close|comment|create|edit|reopen|merge|revert|review|ready|lock|unlock|update-branch)\b'; then
    require_allowed_repo "gh pr write operation" "$(target_repo_for_gh)"
fi

if echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+issue[[:space:]]+(close|comment|create|reopen|edit|lock|unlock|transfer|delete|pin|unpin|develop)\b'; then
    require_allowed_repo "gh issue write operation" "$(target_repo_for_gh)"
fi

if echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+(secret|variable)[[:space:]]+(set|delete|remove)\b'; then
    if echo "$COMMAND" | grep -qE '(^|[[:space:]])--(org|env|app)(=|[[:space:]]+)'; then
	deny "organization/environment/app GitHub secret or variable mutation" "This operation is not repo-scoped, so the repo allowlist cannot authorize it."
    fi
    require_allowed_repo "gh secret/variable write operation" "$(target_repo_for_gh)"
fi

if echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+workflow[[:space:]]+(run|enable|disable)\b'; then
    require_allowed_repo "gh workflow write operation" "$(target_repo_for_gh)"
fi

if echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+run[[:space:]]+(cancel|delete|rerun)\b'; then
    require_allowed_repo "gh run write operation" "$(target_repo_for_gh)"
fi

if echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+release[[:space:]]+(create|delete|delete-asset|edit|upload)\b'; then
    require_allowed_repo "gh release write operation" "$(target_repo_for_gh)"
fi

if echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+repo[[:space:]]+(create|delete|edit|rename|archive|unarchive|sync)\b'; then
    repo=$(repo_from_gh_repo_positional || true)
    if [ -z "$repo" ] && echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+repo[[:space:]]+create\b'; then
    	# Creation never acts on the surrounding checkout, so its remote must
    	# not stand in for a target the command did not name.
    	deny "gh repo create without an explicit OWNER/REPO target" "A repo creation does not act on the current directory's repository, so that repository's allowlist entry cannot authorize it. Name the target as OWNER/REPO."
    fi
    [ -n "$repo" ] || repo=$(target_repo_for_gh)
    require_allowed_repo "gh repo write operation" "$repo"
fi

if echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+(label|milestone)[[:space:]]+(create|delete|edit)\b'; then
    require_allowed_repo "gh label/milestone write operation" "$(target_repo_for_gh)"
fi

if echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+gist[[:space:]]+(create|delete|edit|rename)\b'; then
    deny "gh gist write operation" "Gists are not repo-scoped, so the repo allowlist cannot authorize them."
fi

if echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+(cache[[:space:]]+delete|discussion[[:space:]]+(comment|create|edit)|repo[[:space:]]+(autolink[[:space:]]+(create|delete)|deploy-key[[:space:]]+(add|delete)))\b'; then
    require_allowed_repo "gh repository write operation" "$(target_repo_for_gh)"
fi

if echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+(agent-task[[:space:]]+create|label[[:space:]]+clone)\b'; then
    require_allowed_repo "gh repository write operation" "$(target_repo_for_gh)"
fi

# These mutations act on an account, organization, codespace, project, or a
# newly created fork rather than one unambiguous existing repository. A repo
# allowlist entry cannot authorize them.
if echo "$SCAN_COMMAND" | grep -qE '(^|[[:space:];|&])gh[[:space:]]+(codespace[[:space:]]+(create|delete|edit|rebuild|stop)|gpg-key[[:space:]]+(add|delete)|ssh-key[[:space:]]+(add|delete)|project[[:space:]]+(close|copy|create|delete|edit|field-create|field-delete|item-add|item-archive|item-create|item-delete|item-edit|link|mark-template|unlink)|repo[[:space:]]+fork)\b'; then
    deny "non-repository-scoped gh write operation" "This operation has no single existing repository target that the repo allowlist can authorize."
fi

if is_gh_api_write; then
    require_allowed_repo "gh api write operation" "$(target_repo_for_api)"
fi

exit 0
