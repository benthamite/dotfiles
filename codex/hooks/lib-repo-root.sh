#!/bin/bash
# Shared helper: resolve the git repo root from the command context.
#
# PreToolUse hooks run before the bash command executes, so the hook's
# cwd may differ from the command's effective cwd.  If the command
# starts with `cd <path> &&`, this helper resolves the repo root from
# that path instead of the hook's cwd.  If Codex supplies a structured
# tool cwd/workdir in the hook payload, use that as the fallback context
# before falling back to the hook process cwd.
#
# Usage: source this file after setting $COMMAND, then use $REPO_ROOT.
# The script also cds into $REPO_ROOT so subsequent git commands
# (including lib-staged-files.sh) operate on the correct repo.
#
#   COMMAND=$(codex_shell_command "$INPUT")
#   source "$(dirname "$0")/lib-repo-root.sh"
#   # $REPO_ROOT is set and cwd is $REPO_ROOT

# Extract a structured tool cwd/workdir from hook JSON when available.
_repo_context_dir="${REPO_CONTEXT_DIR:-}"
if [ -z "$_repo_context_dir" ] && [ -n "${INPUT:-}" ] && command -v jq >/dev/null 2>&1; then
  _repo_context_dir=$(
    printf '%s' "$INPUT" | jq -r '
      def normalized_tool_input:
        .tool_input? as $input |
        if ($input | type) == "object" then $input
        elif ($input | type) == "string" then
          ($input as $raw |
           (try ($raw | fromjson) catch {}) |
           if type == "object" then . else {} end)
        else {}
        end;
      normalized_tool_input.workdir //
      normalized_tool_input.cwd //
      normalized_tool_input.working_directory //
      normalized_tool_input.working_dir //
      .workdir //
      .cwd //
      .working_directory //
      .working_dir //
      empty
    ' 2>/dev/null || true
  )
fi

# Extract the cd target from "cd <path> ..." patterns without re-evaluating the
# command in a subshell.  Pure parameter expansion (no `[[ =~ ]]`) so the
# behaviour is identical whether this file is sourced into bash or zsh -- the
# `[[ =~ ]]` ERE engine differs between the two and silently failed to capture
# quoted paths under zsh, falling back to the hook's cwd.
#
# Every command head is scanned, not just the first, and the last `cd` wins.  A
# preamble before changing directory is ordinary -- writing a commit message to a
# file, setting a variable -- and the repository the command acts on is the same
# either way.  Heads are the start of the command and whatever follows `&&`,
# `||`, `;` or a newline.  Shell-group openers are skipped because the
# output-redaction wrapper rewrites every command to `{ CMD; } 2>&1 | ...`.
#
# Command substitution is rejected in the extracted target below rather than in
# the command as a whole: the target is the only value that reaches `git -C`, and
# rejecting the whole command meant a `$(...)` anywhere in it discarded the
# target.  A multi-line commit message written as
# `git commit -m "$(cat <<EOF ...)"` did exactly that, so a commit made in one
# repository was evaluated against the session's own.
_cd_target=""
_dq='"'
_sq="'"
_scan="$COMMAND"
while [ -n "$_scan" ]; do
  _head="${_scan#"${_scan%%[![:space:]]*}"}"              # ltrim
  while :; do
    case "$_head" in
      '{'* | '('* | '&'* | '|'* | ';'* )
        _head="${_head#?}"; _head="${_head#"${_head%%[![:space:]]*}"}" ;;
      * ) break ;;
    esac
  done
  case "$_head" in
    cd[[:space:]]*)
      _rest="${_head#cd}"
      _rest="${_rest#"${_rest%%[![:space:]]*}"}"
      case "$_rest" in
        "$_dq"*) _rest="${_rest#"$_dq"}"; _cd_target="${_rest%%"$_dq"*}" ;;
        "$_sq"*) _rest="${_rest#"$_sq"}"; _cd_target="${_rest%%"$_sq"*}" ;;
        *)       _cd_target="${_rest%%[[:space:]]*}"
                 _cd_target="${_cd_target%%'&'*}"
                 _cd_target="${_cd_target%%';'*}"
                 _cd_target="${_cd_target%%'|'*}"
                 _cd_target="${_cd_target/#\~/$HOME}"
                 _cd_target="${_cd_target//'\ '/ }" ;;
      esac
      ;;
  esac
  # Advance to the next command head.
  case "$_scan" in
    *[\&\|\;$'\n']* )
      _scan="${_scan#*[&|;$'\n']}" ;;
    * ) _scan="" ;;
  esac
done
unset _scan _head 2>/dev/null || true
unset _dq _sq _rest 2>/dev/null || true

# Discard a target carrying command substitution, so an injected `$(...)` or
# backtick cannot reach `git -C`.
case "$_cd_target" in
  *'$('* | *'`'* ) _cd_target="" ;;
esac

# Expand $HOME, which is how these paths are ordinarily written.  Only this one
# variable is expanded, and by substitution rather than evaluation.  A target
# still holding a `$` after that cannot be resolved safely, so it is marked
# unresolvable rather than guessed at.
_cd_unresolvable=""
if [ -n "$_cd_target" ]; then
  _cd_target="${_cd_target//'${HOME}'/$HOME}"
  _cd_target="${_cd_target//'$HOME'/$HOME}"
  case "$_cd_target" in
    *'$'* ) _cd_unresolvable=yes; _cd_target="" ;;
  esac
fi

# When the command changes directory, that target decides the repository. If it
# is present but unresolvable, leave REPO_ROOT empty instead of falling back to
# the hook's own context: answering from a different repository is unsound in
# both directions, refusing correct commits and approving ones that should be
# refused whenever the other tree happens to satisfy the check. Callers already
# treat an empty REPO_ROOT as "not a repository" and stand down.
if [ -n "$_cd_target" ] && [ -d "$_cd_target" ]; then
  REPO_ROOT=$(git -C "$_cd_target" rev-parse --show-toplevel 2>/dev/null || true)
elif [ -n "$_cd_target" ] || [ -n "$_cd_unresolvable" ]; then
  REPO_ROOT=""
elif [ -n "$_repo_context_dir" ] && [ -d "$_repo_context_dir" ]; then
  REPO_ROOT=$(git -C "$_repo_context_dir" rev-parse --show-toplevel 2>/dev/null || true)
else
  REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || true)
fi
unset _cd_target
unset _cd_unresolvable
unset _repo_context_dir

if [ -n "$REPO_ROOT" ]; then
  cd "$REPO_ROOT"
fi
