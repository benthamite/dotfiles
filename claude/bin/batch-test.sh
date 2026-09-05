#!/usr/bin/env bash
# Clean batch loading of canonical Elisp, with source-bound test evidence.
# Usage: batch-test.sh PACKAGE [ELISP-EXPRESSION...]
# The private --ert mode shares source/dependency setup with elisp-ert.

set -euo pipefail

ERT_MODE=false
if [ "${1:-}" = --ert ]; then ERT_MODE=true; shift; fi
if [ "$#" -lt 1 ] || { [ "$ERT_MODE" = true ] && { [ "$#" -lt 2 ] || [ "$#" -gt 3 ]; }; }; then
  echo "Usage: batch-test.sh PACKAGE [ELISP-EXPRESSION...]" >&2
  exit 2
fi
IDENTIFIER=$1
shift
case "$IDENTIFIER" in
  *[!A-Za-z0-9+_.-]* | "") echo "Invalid package identifier" >&2; exit 2 ;;
esac
if [ "$ERT_MODE" = true ]; then
  TEST_FILE=$1
  TEST_NAME=${2:-}
  [ -f "$TEST_FILE" ] || { echo "Test file not found" >&2; exit 2; }
  case "$TEST_NAME" in
    *[!A-Za-z0-9+_./:-]*) echo "Invalid ERT test name" >&2; exit 2 ;;
  esac
fi

TOOL_ROOT=$(cd -- "$(dirname -- "$0")/../.." && pwd -P)
REVISION_HELPER="$TOOL_ROOT/claude/bin/elisp-source-revision"
# shellcheck source=../hooks/lib-elisp-evidence.sh
source "$TOOL_ROOT/claude/hooks/lib-elisp-evidence.sh"
if ! RESOLUTION=$("$TOOL_ROOT/bin/elpaca-package-resolve" "$IDENTIFIER" 2>/dev/null); then
  echo "Cannot resolve the canonical package source" >&2
  exit 1
fi
if ! printf '%s' "$RESOLUTION" | jq -e '
  type == "object" and
  (.id | type == "string" and test("^[A-Za-z0-9+_.-]+$")) and
  ([.source, .repo] | all(.[]; type == "string" and startswith("/") and
    (test("[[:cntrl:]]") | not)))' >/dev/null; then
  echo "Invalid package resolution" >&2
  exit 1
fi
PACKAGE=$(printf '%s' "$RESOLUTION" | jq -er '.id')
SOURCE_DIR=$(printf '%s' "$RESOLUTION" | jq -er '.source')
SOURCE_REPO=$(printf '%s' "$RESOLUTION" | jq -er '.repo')
if [ ! -d "$SOURCE_DIR" ] || [ ! -d "$SOURCE_REPO" ]; then
  echo "Canonical package source is unavailable; refusing build-only fallback" >&2
  exit 1
fi
SOURCE_DIR=$(cd -- "$SOURCE_DIR" && pwd -P)
SOURCE_REPO=$(cd -- "$SOURCE_REPO" && pwd -P)
case "$SOURCE_DIR/" in
  "$SOURCE_REPO/"*) ;;
  *) echo "Canonical source is outside its repository" >&2; exit 1 ;;
esac

# Query actual dependency locations, not a guessed profile filesystem layout.
QUERY="(progn (require 'json) (require 'seq)
  ;; elpaca-batch-context-v1
  (let* ((root (and (boundp 'elpaca-builds-directory) elpaca-builds-directory))
         (entry (elpaca-get '$PACKAGE))
         (source (and entry (or (elpaca<-source-dir entry) (elpaca-source-dir entry))))
         (build (and entry (elpaca<-build-dir entry))))
    (unless (and root (not (file-remote-p root)) (file-directory-p root))
      (error \"No local Elpaca builds directory\"))
    (unless (and entry (stringp source) (not (file-remote-p source))
                 (file-directory-p source) (stringp build)
                 (file-name-absolute-p build) (not (file-remote-p build)))
      (error \"Cannot identify package source and own build\"))
    (base64-encode-string
      (encode-coding-string
        (json-encode
          (list (cons 'id (symbol-name '$PACKAGE))
            (cons 'source (file-truename source))
            (cons 'builds (vconcat
            (mapcar #'file-truename
              (seq-filter #'file-directory-p
                (directory-files root t directory-files-no-dot-files-regexp)))))
            (cons 'package_build (file-truename build))))
        'utf-8) t)))"
if ! RAW_CONTEXT=$(timeout 10 emacsclient -e "$QUERY" 2>/dev/null); then
  echo "Cannot query active Elpaca dependency paths" >&2
  exit 1
fi
if ! CONTEXT=$(printf '%s' "$RAW_CONTEXT" | jq -er --arg id "$PACKAGE" '
  strings | @base64d | fromjson |
  select(type == "object" and .id == $id and
    (.source | type == "string" and startswith("/") and (test("[[:cntrl:]]") | not)) and
    (.builds | type == "array" and all(.[]; type == "string" and startswith("/") and
      (test("[[:cntrl:]]") | not))) and
    (.package_build | type == "string" and startswith("/") and
      (test("[[:cntrl:]]") | not)))'); then
  echo "Invalid Elpaca dependency metadata" >&2
  exit 1
fi
RUNTIME_SOURCE=$(printf '%s' "$CONTEXT" | jq -er '.source')
if ! RUNTIME_SOURCE=$(cd -- "$RUNTIME_SOURCE" && pwd -P); then
  echo "Active registry source is unavailable" >&2
  exit 1
fi
CANONICAL_DOTFILES=$(cd -- "${DOTFILES_ROOT:-$TOOL_ROOT}" && pwd -P)
if { [ "$SOURCE_REPO" != "$CANONICAL_DOTFILES" ] ||
     [ "$SOURCE_DIR" != "$CANONICAL_DOTFILES/emacs/extras" ]; } &&
   [ "$RUNTIME_SOURCE" != "$SOURCE_DIR" ]; then
  echo "Registry source changed while resolving dependency paths" >&2
  exit 1
fi
# Exclude the package's own build entirely, not just its main library.
DEPENDENCIES=$(printf '%s' "$CONTEXT" | jq -c '
  .package_build as $own | [.builds[] | select(rtrimstr("/") != ($own // "" | rtrimstr("/")))]')
DEPENDENCIES_B64=$(printf '%s' "$DEPENDENCIES" | base64 | tr -d '\n')
ARGS=(-Q --batch)
ARGS+=(--eval "(progn (require 'json)
  (setq load-prefer-newer nil load-suffixes '(\".el\" \".elc\"))
  (setq load-path (append
    (json-parse-string
      (decode-coding-string (base64-decode-string \"$DEPENDENCIES_B64\") 'utf-8)
      :array-type 'list) load-path)))")
if [ -d "$SOURCE_DIR/lisp" ]; then
  LISP_DIR_B64=$(printf '%s' "$SOURCE_DIR/lisp" | base64 | tr -d '\n')
  ARGS+=(--eval "(add-to-list 'load-path (decode-coding-string (base64-decode-string \"$LISP_DIR_B64\") 'utf-8))")
fi
SOURCE_DIR_B64=$(printf '%s' "$SOURCE_DIR" | base64 | tr -d '\n')
ARGS+=(--eval "(add-to-list 'load-path (decode-coding-string (base64-decode-string \"$SOURCE_DIR_B64\") 'utf-8))")
REVISION_BEFORE=$("$REVISION_HELPER" "$SOURCE_REPO")

if [ "$ERT_MODE" = true ]; then
  ARGS+=(--eval "(require 'ert)" -l "$TEST_FILE")
  if [ -n "$TEST_NAME" ]; then
    ARGS+=(--eval "(ert-run-tests-batch-and-exit '$TEST_NAME)")
  else
    ARGS+=(-f ert-run-tests-batch-and-exit)
  fi
else
  CANDIDATES=()
  for candidate in "$SOURCE_DIR/$PACKAGE.el" "$SOURCE_DIR/lisp/$PACKAGE.el"; do
    [ ! -f "$candidate" ] || CANDIDATES+=("$candidate")
  done
  if [ "${#CANDIDATES[@]}" -ne 1 ]; then
    echo "Expected one canonical PACKAGE.el at the source root or lisp/; found ${#CANDIDATES[@]}" >&2
    exit 1
  fi
  SOURCE_FILE_B64=$(printf '%s' "${CANDIDATES[0]}" | base64 | tr -d '\n')
  ARGS+=(--eval "(load (decode-coding-string (base64-decode-string \"$SOURCE_FILE_B64\") 'utf-8) nil nil t)")
  ARGS+=(--eval "(require '$PACKAGE)")
  if [ "$#" -eq 0 ]; then
    ARGS+=(--eval "(message \"%s loaded successfully\" '$PACKAGE)")
  else
    for expression in "$@"; do ARGS+=(--eval "$expression"); done
  fi
fi

OUTPUT_FILE=$(mktemp "${TMPDIR:-/tmp}/batch-test.XXXXXX")
trap 'rm -f "$OUTPUT_FILE"' EXIT
set +e
emacs "${ARGS[@]}" >"$OUTPUT_FILE" 2>&1
STATUS=$?
set -e
sed -n '1,$p' "$OUTPUT_FILE"
[ "$STATUS" -eq 0 ] || exit "$STATUS"
if grep -qE 'newer than byte-compiled file|using older file' "$OUTPUT_FILE"; then
  echo "Batch runner rejected a stale-load warning" >&2
  exit 1
fi
REVISION_AFTER=$("$REVISION_HELPER" "$SOURCE_REPO")
if [ "$REVISION_AFTER" != "$REVISION_BEFORE" ]; then
  echo "Batch runner rejected a source change that occurred during the check" >&2
  exit 1
fi
if [ "$ERT_MODE" = false ]; then
  REPO_B64=$(printf '%s' "$SOURCE_REPO" | base64 | tr -d '\n')
  PACKAGE_B64=$(printf '%s' "$IDENTIFIER" | base64 | tr -d '\n')
  elisp_evidence_emit test "$REPO_B64" "$PACKAGE_B64" "$REVISION_BEFORE"
fi
