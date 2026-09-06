#!/bin/bash
# PreToolUse hook: require owning documentation for committed Elisp changes.
#
# The package-manual rule applies to repos that have either:
#   - a doc/ directory (at root or nested) → expects a doc/*.org file staged
#   - a README.org at the root             → expects README.org staged
#   - only README.md at the root           → expects README.md staged
#
# NOTE: README.md is normally the GitHub-facing intro, not the manual. This
# hook treats it as a manual only for repos that have no doc/ directory and no
# root README.org.
#
# Standard skill scripts require their own selected SKILL.md or reference
# Markdown instead. Package and per-skill requirements remain independent.
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
SKILL_HELPER_OWNERS=()
HAS_DOC_ORG=false
HAS_README_ORG_STAGED=false
HAS_README_MD_STAGED=false

# Batch helpers inside a standard skill's scripts directory belong to that
# skill's documentation, not to an unrelated package manual in the same repo.
skill_script_owner() {
  local file="$1" root relative skill
  case "$file" in
    "$REPO_ROOT"/*) file=${file#"$REPO_ROOT"/} ;;
  esac
  case "$file" in /*) return 1 ;; esac
  case "/$file/" in */../* | */./*) return 1 ;; esac
  case "$file" in
    claude/skills/* | codex/skills/* | .claude/skills/* | .codex/skills/*)
      root="${file%%/skills/*}/skills" ;;
    */.claude/skills/*) root="${file%/.claude/skills/*}/.claude/skills" ;;
    */.codex/skills/*) root="${file%/.codex/skills/*}/.codex/skills" ;;
    *) return 1 ;;
  esac
  relative=${file#"$root"/}
  skill=${relative%%/*}
  case "$skill" in "" | . | ..) return 1 ;; esac
  relative=${relative#*/}
  case "/$relative/" in */../* | */./*) return 1 ;; esac
  case "$relative" in
    scripts/*.el) printf '%s/%s\n' "$root" "$skill" ;;
    *) return 1 ;;
  esac
}

record_elisp_requirement() {
  local file="$1" owner recorded
  is_doc_exempt_el "$file" && return 0
  is_version_bump_only "$file" && return 0
  if owner=$(skill_script_owner "$file"); then
    if [ "${#SKILL_HELPER_OWNERS[@]}" -gt 0 ]; then
      for recorded in "${SKILL_HELPER_OWNERS[@]}"; do
        [ "$recorded" != "$owner" ] || return 0
      done
    fi
    SKILL_HELPER_OWNERS+=("$owner")
  else
    HAS_EL=true
  fi
}

skill_document_selected() {
  local owner="$1" status before after file
  # Name-status comes from the actual index or lib-staged-files' proposed
  # commit. A deleted or unselected Markdown file cannot satisfy this owner.
  while IFS=$'\t' read -r status before after; do
    case "$status" in
      A | M | T | R[0-9]* | C[0-9]*) ;;
      *) continue ;;
    esac
    file=${after:-$before}
    case "$file" in
      "$owner"/SKILL.md | "$owner"/references/*.md) return 0 ;;
    esac
  done <<< "$STAGED_STATUS"
  return 1
}

if [ -n "$STAGED" ]; then
  while IFS= read -r file; do
    case "$file" in
      *.el)
        record_elisp_requirement "$file"
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
  # Inspect every literal Elisp path: a mixed commit has independent package
  # and per-skill requirements, even after its first production file is found.
  while IFS= read -r -d '' el_file; do
    record_elisp_requirement "$el_file"
  done < <(printf '%s' "$COMMAND" | git_add_elisp_paths)
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
  local source_rel="$1" source_kind="$2" outputs rel
  # Parse the manual version selected for the commit, never an unrelated
  # unstaged header. No Org visit, filter, or source evaluation is needed.
  if ! outputs=$(python3 - "$REPO_ROOT" "$source_rel" "$source_kind" 3<<<"$STAGED_MANUAL_CONTENTS" <<'PY'
import json
import os
from pathlib import Path
import re
import subprocess
import sys

def index_contents(root, relative):
    def git(*args):
        return subprocess.check_output(["git", "-C", str(root), *args],
                                       stderr=subprocess.PIPE)
    entry = git("ls-files", "--stage", "-z", "--", ":(literal)" + relative).rstrip(b"\0")
    if not entry:
        return None
    metadata, actual = entry.split(b"\t", 1)
    mode, oid, stage = metadata.split()
    if actual.decode() != relative or stage != b"0" or b"\0" in entry:
        raise ValueError("Unresolved index manual")
    return git("cat-file", "blob", oid.decode()).decode("utf-8")

def working_contents(source, root):
    try:
        source.resolve(strict=True).relative_to(root)
        return source.read_text(encoding="utf-8")
    except FileNotFoundError:
        return None

def output_paths(contents, source, root):
    if contents is None:
        return []
    declarations = {"EXPORT_FILE_NAME": [], "TEXINFO_FILENAME": []}
    literal = None
    for line in contents.splitlines():
        if literal:
            if re.match(r"^[ \t]*#\+end_" + literal + r"[ \t]*$", line, re.I):
                literal = None
            continue
        block = re.match(r"^[ \t]*#\+begin_(src|example|comment|export)(?:[ \t]|$)", line, re.I)
        if block:
            literal = block.group(1).lower()
            continue
        match = re.match(r"^[ \t]*#\+(EXPORT_FILE_NAME|TEXINFO_FILENAME):[ \t]*(.*)$", line, re.I)
        if match:
            declarations[match.group(1).upper()].append(match.group(2).strip())
    exports, infos = declarations.values()
    if not exports and not infos:
        return []
    if len(exports) > 1 or len(infos) > 1:
        raise ValueError("Ambiguous declarations")
    export = exports[0] if exports else source.name
    texi = os.path.splitext(export)[0] + ".texi"
    info = infos[0] if infos else os.path.splitext(texi)[0] + ".info"
    # Only TEXINFO_FILENAME passes through org-strip-quotes.
    if len(info) >= 2 and info.startswith('"') and info.endswith('"'):
        info = info[1:-1]
    for name in (export, texi, info):
        if (not name or name != name.strip() or name.startswith("~")
                or re.search(r'[\x00-\x1f\x7f@"\x27{}\\$]', name)):
            raise ValueError("Unsupported output name")
    paths = []
    for name in (texi, info):
        target = (source.parent / name).resolve(strict=False)
        relative = target.relative_to(root)
        if target == root or target == source:
            raise ValueError("Invalid output destination")
        paths.append(relative.as_posix())
    return paths

try:
    root = Path(sys.argv[1]).resolve(strict=True)
    relative, kind = sys.argv[2:]
    source = root / relative
    source.parent.resolve(strict=False).relative_to(root)
    if kind == "selection":
        contents = json.load(os.fdopen(3))
        if relative not in contents or not (contents[relative] is None or isinstance(contents[relative], str)):
            raise ValueError("Missing candidate manual")
        paths = output_paths(contents[relative], source, root)
    elif kind == "worktree":
        paths = output_paths(working_contents(source, root), source, root)
    else:
        paths = output_paths(index_contents(root, relative), source, root)
        if kind == "uncertain":
            other = output_paths(working_contents(source, root), source, root)
            if paths != other:
                raise ValueError("Stage pending manual changes separately")
    for path in paths:
        print(path)
except (OSError, UnicodeError, ValueError, RuntimeError, subprocess.CalledProcessError):
    sys.exit(1)
PY
  ); then
    return 1
  fi
  while IFS= read -r rel; do
    [ -n "$rel" ] || continue
    if [ -e "$REPO_ROOT/$rel" ] || git -C "$REPO_ROOT" ls-files --error-unmatch -- "$rel" >/dev/null 2>&1; then
      printf '%s\n' "$rel"
    fi
  done <<< "$outputs"
}

PENDING_ADDS=""
PENDING_UNCERTAIN_ADDS=""
if [ "$STAGED_SELECTION" = 1 ]; then
  PENDING_ADDS="$STAGED"$'\n'
else
  while IFS= read -r -d '' _pending; do
    case "$_pending" in
      -- | --force | -f) continue ;;
      "$REPO_ROOT"/*) _pending=${_pending#"$REPO_ROOT"/} ;;
      /* | -* | *'$'* | *'`'* | *'*'* | *'?'* | *'['*)
        PENDING_UNCERTAIN_ADDS="$PENDING_UNCERTAIN_ADDS."$'\n'
        continue
        ;;
    esac
    if [ -d "$REPO_ROOT/$_pending" ]; then
      PENDING_UNCERTAIN_ADDS="$PENDING_UNCERTAIN_ADDS$_pending"$'\n'
      continue
    fi
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

pending_source_uncertain_p() {
  local prefix
  while IFS= read -r prefix; do
    [ -n "$prefix" ] || continue
    case "$1" in
      "$prefix" | "$prefix"/*) return 0 ;;
    esac
    [ "$prefix" != . ] || return 0
  done <<< "$PENDING_UNCERTAIN_ADDS"
  return 1
}

DIRTY_GENERATED_DOCS=()
UNRESOLVED_MANUAL_OUTPUTS=()
check_texinfo_manual_source() {
  local file="$1" outputs source_kind=index
  case "$file" in
    README.org | doc/*.org | */doc/*.org)
      if [ "$STAGED_SELECTION" = 1 ]; then
        source_kind=selection
      elif pending_add_p "$file"; then
        source_kind=worktree
      elif pending_source_uncertain_p "$file"; then
        source_kind=uncertain
      fi
      if ! outputs=$(texinfo_manual_outputs "$file" "$source_kind"); then
        UNRESOLVED_MANUAL_OUTPUTS+=("$file")
        return 0
      fi
      while IFS= read -r generated; do
        [ -n "$generated" ] || continue
        pending_add_p "$generated" && continue
        if ! git -C "$REPO_ROOT" diff --quiet -- "$generated" || \
           [ -n "$(git -C "$REPO_ROOT" ls-files --others --exclude-standard -- "$generated")" ]; then
          DIRTY_GENERATED_DOCS+=("$generated")
        fi
      done <<< "$outputs"
      ;;
  esac
}

if [ -n "$STAGED" ]; then
  while IFS= read -r file; do
    check_texinfo_manual_source "$file"
  done <<< "$STAGED"
fi

if [ -n "$ADD_ARGS" ]; then
  while IFS= read -r file; do
    check_texinfo_manual_source "$file"
  done <<< "$PENDING_ADDS"
fi

# Directory/options/dynamic pending adds are not exact file selections. Inspect
# potentially affected changed manuals, but do not guess which header wins.
if [ -n "$PENDING_UNCERTAIN_ADDS" ]; then
  if ! pending_changed=$(git -C "$REPO_ROOT" diff --no-ext-diff --no-textconv --name-only -- &&
       git -C "$REPO_ROOT" ls-files --others --exclude-standard); then
    UNRESOLVED_MANUAL_OUTPUTS+=("pending git add")
  else
    while IFS= read -r file; do
      if pending_source_uncertain_p "$file"; then
        if skill_script_owner "$file" >/dev/null; then
          record_elisp_requirement "$file"
        fi
        check_texinfo_manual_source "$file"
      fi
    done <<< "$pending_changed"
  fi
fi

if [ "${#UNRESOLVED_MANUAL_OUTPUTS[@]}" -gt 0 ]; then
  REASON=$(printf 'BLOCKED: cannot safely determine generated Texinfo output names for: %s. Review ambiguous or unsupported declarations, keep destinations inside the repository, and stage pending manual changes separately when their candidate headers are uncertain.' "${UNRESOLVED_MANUAL_OUTPUTS[*]}")
  jq -n --arg reason "$REASON" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": $reason
    }
  }'
  exit 0
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

MISSING_SKILL_DOCS=()
if [ "${#SKILL_HELPER_OWNERS[@]}" -gt 0 ]; then
  for owner in "${SKILL_HELPER_OWNERS[@]}"; do
    if ! skill_document_selected "$owner"; then
      MISSING_SKILL_DOCS+=("$owner")
    fi
  done
fi
if [ "${#SKILL_HELPER_OWNERS[@]}" -gt 0 ] && [ -n "$ADD_ARGS" ]; then
  REASON="BLOCKED: stage skill-helper changes and their owning skill documentation separately before committing. Combined staging has no resolved final documentation selection."
elif [ "${#MISSING_SKILL_DOCS[@]}" -gt 0 ]; then
  REASON="BLOCKED: skill helper changes require a changed, non-deleted SKILL.md or references/*.md from each owning skill in this commit: ${MISSING_SKILL_DOCS[*]}. An unrelated package manual or another skill's documentation cannot satisfy this requirement."
else
  REASON=""
fi
if [ -n "$REASON" ]; then
  jq -n --arg reason "$REASON" '{hookSpecificOutput: {
    hookEventName: "PreToolUse", permissionDecision: "deny",
    permissionDecisionReason: $reason}}'
  exit 0
fi

if [ "$HAS_EL" = false ]; then
  exit 0
fi

# Ordinary production Elisp retains the existing package-manual rule.
if [ "$HAS_DOC_DIR" = false ] && [ "$HAS_README_ORG" = false ] && [ "$HAS_README_MD" = false ]; then
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
  REASON="BLOCKED: Elisp files are staged but no doc/*.org file is included. Update the org manual in the relevant doc/ directory to reflect your changes, then try again. Use document-elisp-package to generate or update documentation."
elif [ "$HAS_README_ORG" = true ]; then
  REASON="BLOCKED: Elisp files are staged but README.org is not included. Update the manual (README.org) to reflect your changes, then try again. Use document-elisp-package to update the manual. README.md is the GitHub intro, not the manual — only update it when the high-level picture changes."
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
