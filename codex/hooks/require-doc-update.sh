#!/bin/bash
# PreToolUse hook: block git commit when .el files are staged but the
# package manual is not also staged.
#
# Applies to repos that have either:
#   - a doc/ directory (at root or nested) → expects a doc/*.org file staged
#   - a README.org at the root             → expects README.org staged
#
# NOTE: README.md is the GitHub-facing intro, NOT the manual. It only
# needs updating when the high-level picture changes (new major features,
# new dependencies, changed installation, etc.). This hook enforces
# manual (README.org or doc/*.org) updates only.
#
# Reads JSON from stdin (Claude Code PreToolUse format).
# Outputs JSON with permissionDecision to allow or deny.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-hook-json.sh
source "$SCRIPT_DIR/lib-codex-hook-json.sh"

INPUT=$(cat)

COMMAND=$(codex_tool_input_field "$INPUT" command)

# Only intercept git commit commands
if ! echo "$COMMAND" | grep -qE '\bgit\s+commit\b'; then
  exit 0
fi

# Find the repo root; bail if not in a git repo
# shellcheck source=lib-repo-root.sh
source "$(dirname "$0")/lib-repo-root.sh"
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

# Determine which documentation pattern applies
HAS_DOC_DIR=false
HAS_README_ORG=false

# Check for doc/ at root or nested (up to 3 levels deep)
if [ -d "$REPO_ROOT/doc" ] || \
   compgen -G "$REPO_ROOT/*/doc" > /dev/null 2>&1 || \
   compgen -G "$REPO_ROOT/*/*/doc" > /dev/null 2>&1 || \
   compgen -G "$REPO_ROOT/*/*/*/doc" > /dev/null 2>&1; then
  HAS_DOC_DIR=true
fi
if [ -f "$REPO_ROOT/README.org" ]; then
  HAS_README_ORG=true
fi

# Only apply in repos that have some form of manual
if [ "$HAS_DOC_DIR" = false ] && [ "$HAS_README_ORG" = false ]; then
  exit 0
fi

# Machine-generated .el files that have no corresponding manual
is_generated_el() {
  local file="$1"
  case "${file##*/}" in
    lockfile.el | *-autoloads.el | *-pkg.el) return 0 ;;
    *) return 1 ;;
  esac
}

# Check staged files (amend-aware: see lib-staged-files.sh)
# shellcheck source=lib-staged-files.sh
source "$(dirname "$0")/lib-staged-files.sh"

HAS_EL=false
HAS_DOC_ORG=false
HAS_README_ORG_STAGED=false
if [ -n "$STAGED" ]; then
  while IFS= read -r file; do
    case "$file" in
      *.el)
        is_generated_el "$file" || HAS_EL=true
        ;;
      doc/*.org | */doc/*.org)
        HAS_DOC_ORG=true
        ;;
      README.org)
        HAS_README_ORG_STAGED=true
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
if [ -n "$ADD_ARGS" ]; then
  if [ "$HAS_EL" = false ]; then
    # Extract .el filenames from git add args, skipping machine-generated ones
    for el_file in $(echo "$ADD_ARGS" | grep -oE '[^ ]*\.el\b' || true); do
      if ! is_generated_el "$el_file"; then
        HAS_EL=true
        break
      fi
    done
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

DIRTY_GENERATED_DOCS=()
check_texinfo_manual_source() {
  local file="$1"
  case "$file" in
    README.org | doc/*.org | */doc/*.org)
      while IFS= read -r generated; do
        [ -n "$generated" ] || continue
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
      "hookEventName": "PreToolUse",
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
if [ "$HAS_DOC_ORG" = true ] || [ "$HAS_README_ORG_STAGED" = true ]; then
  exit 0
fi

# Block the commit
if [ "$HAS_DOC_DIR" = true ]; then
  REASON="BLOCKED: Elisp files are staged but no doc/*.org file is included. Update the org manual in the relevant doc/ directory to reflect your changes, then try again. Use /doc-elisp to generate or update documentation."
else
  REASON="BLOCKED: Elisp files are staged but README.org is not included. Update the manual (README.org) to reflect your changes, then try again. Use /doc-elisp to update the manual. README.md is the GitHub intro, not the manual — only update it when the high-level picture changes."
fi

jq -n --arg reason "$REASON" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": $reason
  }
}'
