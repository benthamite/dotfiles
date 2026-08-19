#!/bin/bash
# PreToolUse hook: block git commit when Elisp files are staged
# but no test has been run in this session.
#
# Reads JSON from stdin (Claude Code PreToolUse format).
# Outputs JSON with permissionDecision to allow or deny.

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# shellcheck source=lib-codex-hook-json.sh
source "$SCRIPT_DIR/lib-codex-hook-json.sh"

INPUT=$(cat)

COMMAND=$(codex_shell_command "$INPUT")
SESSION_ID=$(codex_session_id "$INPUT")

# Only intercept git commit commands
if ! echo "$COMMAND" | grep -qE '\bgit\s+commit\b'; then
  exit 0
fi

# Inspect staged files in the repo targeted by the command, not the hook cwd.
# shellcheck source=lib-repo-root.sh
source "$SCRIPT_DIR/lib-repo-root.sh"
if [ -z "$REPO_ROOT" ]; then
  exit 0
fi

# Check if any staged files are Elisp-related (amend-aware: see lib-staged-files.sh)
# shellcheck source=lib-staged-files.sh
source "$SCRIPT_DIR/lib-staged-files.sh"

HAS_ELISP=false
if [ -n "$STAGED" ]; then
  while IFS= read -r file; do
    case "$file" in
      *.el|emacs/config.org)
        HAS_ELISP=true
        break
        ;;
    esac
  done <<< "$STAGED"
fi

# A combined add+commit call cannot be checked against the future index. Make
# the agent split the operations so the commit gate can identify exact bytes.
ADD_ARGS=$(echo "$COMMAND" | grep -oE 'git\s+add\s+[^;&|]*' || true)
if [ -n "$ADD_ARGS" ] &&
   { echo "$ADD_ARGS" | grep -qE '\.el([[:space:]]|$)' ||
     echo "$ADD_ARGS" | grep -qF 'emacs/config.org'; }; then
  REASON="BLOCKED: Stage Elisp source in a separate command before git commit. A combined git add and git commit call cannot bind test evidence to the future index."
  jq -n --arg reason "$REASON" '{
    "hookSpecificOutput": {
      "hookEventName": "PreToolUse",
      "permissionDecision": "deny",
      "permissionDecisionReason": $reason
    }
  }'
  exit 0
fi

if [ "$HAS_ELISP" = false ]; then
  exit 0
fi

# Check for evidence tied to this repository, package, and source revision.
MARKER="/tmp/claude-elisp-tested-${SESSION_ID}"
DOTFILES_ROOT=$(cd -- "$SCRIPT_DIR/../.." && pwd)
REVISION_HELPER="$DOTFILES_ROOT/claude/bin/elisp-source-revision"
REVISION=$("$REVISION_HELPER" "$REPO_ROOT")
REPO_B64=$(printf '%s' "$REPO_ROOT" | base64 | tr -d '\n')

evidence_matches_package() {
  local package="$1" package_b64
  package_b64=$(printf '%s' "$package" | base64 | tr -d '\n')
  [ -f "$MARKER" ] && grep -qxF "$REPO_B64:$package_b64:$REVISION" "$MARKER"
}

EXPECTED_PACKAGES=()
INDEX_DIVERGENCE=""
while IFS= read -r file; do
  case "$file" in
    emacs/extras/*.el)
      package=$(basename "$file" .el)
      package=${package#test-}
      package=${package%-tests}
      package=${package%-test}
      EXPECTED_PACKAGES+=("$package")
      ;;
    emacs/config.org) EXPECTED_PACKAGES+=("file:emacs/config.org") ;;
    .dir-locals.el|*/.dir-locals.el|lockfile.el|*/lockfile.el|*-autoloads.el|*-pkg.el)
      EXPECTED_PACKAGES+=("file:$file")
      ;;
    *.el)
      if [ "$REPO_ROOT" = "$DOTFILES_ROOT" ]; then
        EXPECTED_PACKAGES+=("file:$file")
      else
        EXPECTED_PACKAGES+=("$(basename "$REPO_ROOT")")
      fi
      ;;
  esac
  case "$file" in
    *.el|emacs/config.org)
      if ! git -C "$REPO_ROOT" diff --quiet -- "$file"; then
        INDEX_DIVERGENCE="$file"
      fi
      ;;
  esac
done <<< "$STAGED"

USE_STAGED_EVIDENCE=false
if [ -n "$INDEX_DIVERGENCE" ]; then
  for package in "${EXPECTED_PACKAGES[@]}"; do
    if [[ "$package" != file:* ]]; then
      REASON="BLOCKED: The staged and working-tree versions differ for ${INDEX_DIVERGENCE}. Package batch evidence must test the same bytes that the commit will contain."
      jq -n --arg reason "$REASON" '{
        "hookSpecificOutput": {
          "hookEventName": "PreToolUse",
          "permissionDecision": "deny",
          "permissionDecisionReason": $reason
        }
      }'
      exit 0
    fi
  done
  REVISION=$("$REVISION_HELPER" --index "$REPO_ROOT")
  USE_STAGED_EVIDENCE=true
fi

MISSING_PACKAGE=""
for package in "${EXPECTED_PACKAGES[@]}"; do
  if ! evidence_matches_package "$package"; then
    MISSING_PACKAGE="$package"
    break
  fi
done
if [ -z "$MISSING_PACKAGE" ]; then
  exit 0
fi

# Block the commit
# Extract the first staged .el filename to suggest a ready-to-paste command
STAGED_EL=$(echo "$STAGED" | grep '\.el$' | head -1 || true)
PKG_NAME=""
if [ -n "$STAGED_EL" ]; then
  PKG_NAME=$(basename "$STAGED_EL" .el)
fi

if [ -n "$MISSING_PACKAGE" ]; then
  PKG_NAME="$MISSING_PACKAGE"
fi
if [[ "$PKG_NAME" == file:* ]]; then
  FILE_LABEL=${PKG_NAME#file:}
  STAGED_FLAG=""
  [ "$USE_STAGED_EVIDENCE" = true ] && STAGED_FLAG="--staged "
  EXAMPLE_CMD="\"$DOTFILES_ROOT/claude/bin/elisp-check-evidence\" ${STAGED_FLAG}file:${FILE_LABEL} -- PROJECT-CHECK"
elif [ -n "$PKG_NAME" ]; then
  EXAMPLE_CMD="\"$DOTFILES_ROOT/claude/bin/batch-test.sh\" ${PKG_NAME}"
else
  EXAMPLE_CMD="\"$DOTFILES_ROOT/claude/bin/batch-test.sh\" YOUR-PACKAGE"
fi

REASON="BLOCKED: The staged Elisp source does not have matching test evidence for this repository, package or file label, and source revision.\n\nCommand:\n${EXAMPLE_CMD}\n\nReplace PROJECT-CHECK with a tracked executable in the owning repository. Run the test after the final source edit. For config.org, use the exact file:emacs/config.org label and a tracked check that tangles and validates the affected output."
jq -n --arg reason "$REASON" '{
  "hookSpecificOutput": {
    "hookEventName": "PreToolUse",
    "permissionDecision": "deny",
    "permissionDecisionReason": $reason
  }
}'
