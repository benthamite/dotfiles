#!/bin/bash
# PreToolUse hook: block git commit when .el files are staged but the
# package manual is not also staged.
#
# Applies to repos that have either:
#   - a doc/ directory (at root or nested) → expects a doc/*.org file staged
#   - a README.org at the root             → expects README.org staged
#   - only README.md at the root           → expects README.md staged
#
# NOTE: README.md is normally the GitHub-facing intro, not the manual. This
# hook treats it as a manual only for repos that have no doc/ directory and no
# root README.org.
#
# Reads JSON from stdin (Claude Code PreToolUse format).
# Outputs JSON with permissionDecision to allow or deny.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)

INPUT=$(cat)

COMMAND=$(printf '%s' "$INPUT" | jq -r '.tool_input.command // empty')

# Only intercept git commit commands
if ! echo "$COMMAND" | grep -qE '\bgit\s+commit\b'; then
  exit 0
fi

# Find the repo root; bail if not in a git repo
# shellcheck source=lib-repo-root.sh
source "$SCRIPT_DIR/lib-repo-root.sh"
if [ -z "$REPO_ROOT" ]; then
  exit 0
fi

# Skip during rebase, merge, or cherry-pick — the commit content is
# predetermined, and the manual can be updated in a follow-up commit.
GIT_DIR="$REPO_ROOT/.git"
# Handle worktrees where .git is a file pointing to the real gitdir
if [ -f "$GIT_DIR" ]; then
  GIT_DIR=$(sed -n 's/^gitdir: //p' "$GIT_DIR")
fi
if [ -d "$GIT_DIR/rebase-merge" ] || [ -d "$GIT_DIR/rebase-apply" ] || \
   [ -f "$GIT_DIR/MERGE_HEAD" ] || [ -f "$GIT_DIR/CHERRY_PICK_HEAD" ]; then
  exit 0
fi

# Allow deferring the manual requirement on a feature branch via local
# git config, for multi-commit work that updates documentation at
# milestone boundaries instead of per commit:
#   git config branch.<branch>.deferDocUpdates true     # enable
#   git config --unset branch.<branch>.deferDocUpdates  # remove on merge
# Ignored on main/master so the guard cannot be disabled on mainline.
CURRENT_BRANCH=$(git -C "$REPO_ROOT" branch --show-current 2>/dev/null || true)
if [ -n "$CURRENT_BRANCH" ] && \
   [ "$CURRENT_BRANCH" != "main" ] && [ "$CURRENT_BRANCH" != "master" ] && \
   [ "$(git -C "$REPO_ROOT" config --bool "branch.$CURRENT_BRANCH.deferDocUpdates" 2>/dev/null || echo false)" = "true" ]; then
  exit 0
fi

# Determine which documentation pattern applies
HAS_DOC_DIR=false
HAS_README_ORG=false
HAS_README_MD=false

# Check for doc/ at root or nested (up to 3 levels deep). Skip vendored
# trees: node_modules/*/doc made a website repo look like an Elisp package
# with a manual, so README.org could never satisfy the gate there.
if [ -n "$(find "$REPO_ROOT" -maxdepth 4 -type d -name doc \
      -not -path '*/node_modules/*' -not -path '*/.git/*' \
      -print -quit 2>/dev/null)" ]; then
  HAS_DOC_DIR=true
fi
if [ -f "$REPO_ROOT/README.org" ]; then
  HAS_README_ORG=true
fi
if [ "$HAS_DOC_DIR" = false ] && [ "$HAS_README_ORG" = false ] && [ -f "$REPO_ROOT/README.md" ]; then
  HAS_README_MD=true
fi

# Only apply in repos that have some form of manual
if [ "$HAS_DOC_DIR" = false ] && [ "$HAS_README_ORG" = false ] && [ "$HAS_README_MD" = false ]; then
  exit 0
fi

# .el files exempt from the manual requirement: machine-generated files
# and test files, neither of which changes documented behavior.
is_doc_exempt_el() {
  local file="$1"
  case "$file" in
    test/*.el | */test/*.el | tests/*.el | */tests/*.el) return 0 ;;
  esac
  case "${file##*/}" in
    lockfile.el | *-autoloads.el | *-pkg.el) return 0 ;;
    *-test.el | *-tests.el | test-*.el) return 0 ;;
    *) return 1 ;;
  esac
}

# A staged .el change that only rewrites the `;; Version:' header is
# also exempt: it records a release, and changes no behavior the manual
# could describe.  Release tooling makes that bump as a commit of its
# own, which otherwise could not be committed without inventing a
# documentation edit to satisfy this gate.
is_version_bump_only() {
  local file="$1" diff body
  if [ "${STAGED_SELECTION:-0}" = 1 ]; then
    diff=$(printf '%s' "$STAGED_ELISP_DIFFS" | jq -r --arg file "$file" '.[$file] // empty')
  elif [ -n "${STAGED_BASE:-}" ]; then
    diff=$(git -C "$REPO_ROOT" diff --cached --unified=0 "$STAGED_BASE" -- "$file" 2>/dev/null || true)
  else
    diff=$(git -C "$REPO_ROOT" diff --cached --unified=0 -- "$file" 2>/dev/null || true)
  fi
  # In a combined `git add ... && git commit' the file is not in the
  # index yet, so judge it by the working-tree diff instead.
  if [ -z "$diff" ]; then
    diff=$(git -C "$REPO_ROOT" diff --unified=0 -- "$file" 2>/dev/null || true)
  fi
  [ -n "$diff" ] || return 1
  # Keep the added and removed content lines, dropping file headers.
  body=$(printf '%s\n' "$diff" | grep -E '^[+-]' | grep -Ev '^(\+\+\+|---)' || true)
  [ -n "$body" ] || return 1
  # Exempt only when every changed line is a version header.
  ! printf '%s\n' "$body" |
    grep -qEv '^[+-];;[[:space:]]*Version:[[:space:]]*[0-9][0-9A-Za-z.+-]*[[:space:]]*$'
}

git_add_elisp_paths() {
  python3 -c '
import os
import re
import shlex
import sys


def without_heredoc_bodies(command):
    """Drop heredoc bodies, which are data rather than command arguments.

    A commit message written through a heredoc is prose. An apostrophe in it
    ("the hook own context") makes the lexer below raise, and this parser fails
    closed, so a commit staging no Elisp at all used to be refused.
    """
    kept, lines, index = [], command.split("\n"), 0
    while index < len(lines):
        line = lines[index]
        kept.append(line)
        index += 1
        for match in re.finditer(r"<<-?\s*([\x27\"]?)([A-Za-z_][A-Za-z0-9_]*)\1", line):
            delimiter = match.group(2)
            while index < len(lines) and lines[index].strip() != delimiter:
                index += 1
            index += 1  # drop the delimiter line too
    return "\n".join(kept)


suffix = None if "--all" in sys.argv[1:] else ".el"

try:
    lexer = shlex.shlex(without_heredoc_bodies(sys.stdin.read()),
                        posix=True, punctuation_chars=";&|")
    lexer.whitespace_split = True
    lexer.commenters = ""
    arguments = list(lexer)
except ValueError:
    sys.stdout.buffer.write(b".unparsed-command.el\0")
    raise SystemExit(0)

i = 0
while i + 1 < len(arguments):
    if arguments[i] != "git" or arguments[i + 1] != "add":
        i += 1
        continue
    i += 2
    while i < len(arguments):
        path = arguments[i]
        if path and all(char in ";&|" for char in path):
            break
        path = os.path.normpath(path)
        if suffix is None or path.endswith(suffix):
            sys.stdout.buffer.write(path.encode() + b"\0")
        i += 1
' "$@"
}

# Paths the command is about to stage, whatever their extension.  The hook runs
# before the `git add` it is inspecting, so a file this command stages still
# looks unstaged to git.
git_add_paths() {
  git_add_elisp_paths --all
}

# Check staged files (amend-aware: see lib-staged-files.sh)
# shellcheck source=lib-staged-files.sh
source "$SCRIPT_DIR/lib-staged-files.sh"

HAS_EL=false
HAS_DOC_ORG=false
HAS_README_ORG_STAGED=false
HAS_README_MD_STAGED=false
if [ -n "$STAGED" ]; then
  while IFS= read -r file; do
    case "$file" in
      *.el)
        is_doc_exempt_el "$file" || is_version_bump_only "$file" || HAS_EL=true
        ;;
      doc/*.org | */doc/*.org)
        HAS_DOC_ORG=true
        ;;
      README.org)
        HAS_README_ORG_STAGED=true
        ;;
      README.md)
        HAS_README_MD_STAGED=true
        ;;
    esac
  done <<< "$STAGED"
fi

# Also catch `git add ... && git commit` in a single bash command.
# When staging and committing happen in one call, git diff --cached
# sees nothing yet at hook-fire time, so scan the command string too.
# Extract only the `git add` arguments to avoid false positives from
# commit messages or other parts of the command that mention .el files.
ADD_ARGS=$(echo "$COMMAND" | grep -oE 'git\s+add\s+[^;&|]*' || true)
if [ "$STAGED_SELECTION" = 1 ]; then
  ADD_ARGS=""
fi
if [ -n "$ADD_ARGS" ]; then
  if [ "$HAS_EL" = false ]; then
    # Extract literal .el paths from git add args without evaluating the shell.
    while IFS= read -r -d '' el_file; do
      if ! is_doc_exempt_el "$el_file" && ! is_version_bump_only "$el_file"; then
        HAS_EL=true
        break
      fi
    done < <(printf '%s' "$COMMAND" | git_add_elisp_paths)
  fi
  if [ "$HAS_DOC_ORG" = false ] && echo "$ADD_ARGS" | grep -qE '(^|/)doc/[^ ]*\.org'; then
    # Verify at least one doc/*.org file has actual modifications
    for doc_file in $(echo "$ADD_ARGS" | grep -oE '(^|[/ ])[^ ]*doc/[^ ]*\.org' || true); do
      if [ -n "$(git diff --name-only -- "$doc_file" 2>/dev/null)" ] || \
         [ -n "$(git diff --cached --name-only -- "$doc_file" 2>/dev/null)" ]; then
        HAS_DOC_ORG=true
        break
      fi
    done
  fi
  if [ "$HAS_README_ORG_STAGED" = false ] && echo "$ADD_ARGS" | grep -qF 'README.org'; then
    # Only count README.org if it has actual modifications (staged or unstaged)
    if [ -n "$(git diff --name-only -- README.org 2>/dev/null)" ] || \
       [ -n "$(git diff --cached --name-only -- README.org 2>/dev/null)" ]; then
      HAS_README_ORG_STAGED=true
    fi
  fi
  if [ "$HAS_README_MD_STAGED" = false ] && echo "$ADD_ARGS" | grep -qF 'README.md'; then
    # Only count README.md if it has actual modifications (staged or unstaged)
    if [ -n "$(git diff --name-only -- README.md 2>/dev/null)" ] || \
       [ -n "$(git diff --cached --name-only -- README.md 2>/dev/null)" ]; then
      HAS_README_MD_STAGED=true
    fi
  fi
fi

texinfo_manual_outputs() {
  local source_rel="$1"
  local source_abs="$REPO_ROOT/$source_rel"
  local source_dir_rel target texi info out rel
  [ -f "$source_abs" ] || return 0
  target=$(
    awk 'BEGIN { IGNORECASE = 1 }
         /^#\+(texinfo_filename|export_file_name):/ {
           sub(/^[^:]*:[ \t]*/, "", $0)
           gsub(/^[ \t]+|[ \t]+$/, "", $0)
           print
           exit
         }' "$source_abs"
  )
  [ -n "$target" ] || return 0
  case "$target" in
    *.info)
      texi="${target%.info}.texi"
      info="$target"
      ;;
    *.texi)
      texi="$target"
      info="${target%.texi}.info"
      ;;
    *)
      texi="$target.texi"
      info="$target.info"
      ;;
  esac
  source_dir_rel=$(dirname "$source_rel")
  for out in "$texi" "$info"; do
    case "$out" in
      /*)
        case "$out" in
          "$REPO_ROOT"/*) rel="${out#$REPO_ROOT/}" ;;
          *) continue ;;
        esac
        ;;
      *)
        if [ "$source_dir_rel" = "." ]; then
          rel="$out"
        else
          rel="$source_dir_rel/$out"
        fi
        ;;
    esac
    if [ -e "$REPO_ROOT/$rel" ] || git -C "$REPO_ROOT" ls-files --error-unmatch "$rel" >/dev/null 2>&1; then
      printf '%s\n' "$rel"
    fi
  done
}

PENDING_ADDS=""
if [ "$STAGED_SELECTION" = 1 ]; then
  PENDING_ADDS="$STAGED"$'\n'
else
  while IFS= read -r -d '' _pending; do
    PENDING_ADDS="$PENDING_ADDS$_pending"$'\n'
  done < <(printf '%s' "$COMMAND" | git_add_paths)
fi
unset _pending

pending_add_p() {
  case $'\n'"$PENDING_ADDS" in
    *$'\n'"$1"$'\n'* ) return 0 ;;
    * ) return 1 ;;
  esac
}

DIRTY_GENERATED_DOCS=()
check_texinfo_manual_source() {
  local file="$1"
  case "$file" in
    README.org | doc/*.org | */doc/*.org)
      while IFS= read -r generated; do
        [ -n "$generated" ] || continue
        pending_add_p "$generated" && continue
        if ! git -C "$REPO_ROOT" diff --quiet -- "$generated" || \
           [ -n "$(git -C "$REPO_ROOT" ls-files --others --exclude-standard -- "$generated")" ]; then
          DIRTY_GENERATED_DOCS+=("$generated")
        fi
      done < <(texinfo_manual_outputs "$file")
      ;;
  esac
}

if [ -n "$STAGED" ]; then
  while IFS= read -r file; do
    check_texinfo_manual_source "$file"
  done <<< "$STAGED"
fi

if [ -n "$ADD_ARGS" ]; then
  for file in $(echo "$ADD_ARGS" | grep -oE '([^ ]*/)?README\.org|([^ ]*/)?doc/[^ ]*\.org' || true); do
    check_texinfo_manual_source "$file"
  done
fi

if [ "${#DIRTY_GENERATED_DOCS[@]}" -gt 0 ]; then
  REASON=$(printf 'BLOCKED: generated Texinfo files have unstaged changes after the org manual update. Stage these generated files too: %s' "${DIRTY_GENERATED_DOCS[*]}")
  jq -n --arg reason "$REASON" '{
    "hookSpecificOutput": {
      "permissionDecision": "deny",
      "permissionDecisionReason": $reason
    }
  }'
  exit 0
fi

if [ "$HAS_EL" = false ]; then
  exit 0
fi

# Accept if any documentation file is staged:
# - doc/*.org (for repos with a doc/ directory)
# - README.org (for repos that use README.org as the manual)
# - README.md (fallback for repos that have no Org manual)
if [ "$HAS_DOC_ORG" = true ] || [ "$HAS_README_ORG_STAGED" = true ] || [ "$HAS_README_MD_STAGED" = true ]; then
  exit 0
fi

# Block the commit
if [ "$HAS_DOC_DIR" = true ]; then
  REASON="BLOCKED: Elisp files are staged but no doc/*.org file is included. Update the org manual in the relevant doc/ directory to reflect your changes, then try again. Use /doc-elisp to generate or update documentation."
elif [ "$HAS_README_ORG" = true ]; then
  REASON="BLOCKED: Elisp files are staged but README.org is not included. Update the manual (README.org) to reflect your changes, then try again. Use /doc-elisp to update the manual. README.md is the GitHub intro, not the manual — only update it when the high-level picture changes."
else
  REASON="BLOCKED: Elisp files are staged but README.md is not included. This repo has no Org manual, so update README.md to reflect your changes, then try again."
fi

jq -n --arg reason "$REASON" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": $reason
  }
}'
