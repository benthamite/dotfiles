#!/bin/bash
# Run an emacs --batch test for an Elisp package.
#
# Usage: batch-test.sh <package> [elisp-expressions...]
#
# Handles two package layouts:
#
#   1. Dotfiles extras (canonical source in ~/My Drive/dotfiles/emacs/extras/,
#      separate elpaca clone). Removes the canonical .elc, adds all elpaca
#      builds to load-path, then prepends emacs/extras so the edited source
#      wins over the stale .elc in the elpaca clone.
#
#   2. Standalone elpaca/sources/<pkg> packages (canonical source IS the
#      elpaca clone). Loads the requested library from that source checkout,
#      never from elpaca/builds/<pkg>/.
#
# Examples:
#   batch-test.sh agent-log
#   batch-test.sh sgn '(message "result: %S" (sgn-some-fn))'

set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: batch-test.sh <package> [elisp-expressions...]" >&2
  exit 1
fi

PACKAGE="$1"
shift

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
REVISION_HELPER="$REPO_ROOT/claude/bin/elisp-source-revision"
PROFILE=$(emacsclient -e 'init-current-profile' 2>/dev/null | tr -d '"')
ELPACA="$HOME/.config/emacs-profiles/$PROFILE/elpaca"
EXTRAS="$REPO_ROOT/emacs/extras"

# Detect package layout: extras (canonical .el in dotfiles/emacs/extras)
# vs standalone (canonical .el directly under elpaca/sources/<pkg>/).
IS_EXTRAS=false
if [ -f "$EXTRAS/$PACKAGE.el" ]; then
  IS_EXTRAS=true
fi

ARGS=(--batch)

# All elpaca builds on load-path (both layouts need this for dependencies).
ARGS+=(--eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))")

if [ "$IS_EXTRAS" = true ]; then
  SOURCE_DIR="$EXTRAS"
  SOURCE_FILE="$EXTRAS/$PACKAGE.el"
  SOURCE_REPO="$REPO_ROOT"
else
  if [ -d "$ELPACA/sources/$PACKAGE" ]; then
    SOURCE_DIR="$ELPACA/sources/$PACKAGE"
  elif [ -d "$ELPACA/repos/$PACKAGE" ]; then
    SOURCE_DIR="$ELPACA/repos/$PACKAGE"
  else
    echo "Cannot find the Elpaca source checkout for $PACKAGE" >&2
    exit 1
  fi
  SOURCE_FILE="$SOURCE_DIR/$PACKAGE.el"
  SOURCE_REPO=$(git -C "$SOURCE_DIR" rev-parse --show-toplevel)
fi

if [ ! -f "$SOURCE_FILE" ]; then
  echo "Cannot find canonical source file: $SOURCE_FILE" >&2
  exit 1
fi

# Bind the result to the exact source snapshot that the check starts with.
# A concurrent edit must invalidate the run instead of receiving evidence for
# bytes that Emacs never loaded.
REVISION_BEFORE=$("$REVISION_HELPER" "$SOURCE_REPO")

# Load the canonical source file explicitly. Dependencies still resolve from
# elpaca/builds, but a stale package build can never shadow the requested file.
SOURCE_FILE_B64=$(printf '%s' "$SOURCE_FILE" | base64 | tr -d '\n')
ARGS+=(--eval "(load (decode-coding-string (base64-decode-string \"$SOURCE_FILE_B64\") 'utf-8) nil nil t)")
ARGS+=(--eval "(require '$PACKAGE)")

# User expressions or default success message
if [ $# -eq 0 ]; then
  ARGS+=(--eval "(message \"$PACKAGE loaded successfully\")")
else
  for expr in "$@"; do
    ARGS+=(--eval "$expr")
  done
fi

OUTPUT_FILE=$(mktemp "${TMPDIR:-/tmp}/batch-test.XXXXXX")
cleanup() {
  rm -f "$OUTPUT_FILE"
}
trap cleanup EXIT

set +e
emacs "${ARGS[@]}" >"$OUTPUT_FILE" 2>&1
STATUS=$?
set -e
sed -n '1,$p' "$OUTPUT_FILE"

if [ "$STATUS" -ne 0 ]; then
  exit "$STATUS"
fi
if grep -qE 'newer than byte-compiled file|using older file' "$OUTPUT_FILE"; then
  echo "batch-test.sh rejected a stale-load warning" >&2
  exit 1
fi

REVISION_AFTER=$("$REVISION_HELPER" "$SOURCE_REPO")
if [ "$REVISION_AFTER" != "$REVISION_BEFORE" ]; then
  echo "batch-test.sh rejected a source change that occurred during the check" >&2
  exit 1
fi
REPO_B64=$(printf '%s' "$SOURCE_REPO" | base64 | tr -d '\n')
PACKAGE_B64=$(printf '%s' "$PACKAGE" | base64 | tr -d '\n')
printf 'ELISP_TEST_EVIDENCE_V1:%s:%s:%s\n' "$REPO_B64" "$PACKAGE_B64" "$REVISION_BEFORE"
