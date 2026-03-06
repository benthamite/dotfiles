#!/usr/bin/env bash
# Run ERT tests for emacs extras packages.
# Usage: ./run-tests.sh [test-file ...]
# Without arguments, runs all *-test.el files in this directory.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EXTRAS_DIR="$(dirname "$SCRIPT_DIR")"
DOTFILES_DIR="$(dirname "$(dirname "$EXTRAS_DIR")")"

# Resolve elpaca profile
PROFILE=$(emacsclient -e 'init-current-profile' 2>/dev/null | tr -d '"' || echo "")
if [[ -z "$PROFILE" ]]; then
    echo "Warning: could not determine elpaca profile, trying without elpaca deps" >&2
    ELPACA=""
else
    ELPACA="/Users/pablostafforini/.config/emacs-profiles/${PROFILE}/elpaca"
fi

# Collect test files
if [[ $# -gt 0 ]]; then
    TEST_FILES=("$@")
else
    TEST_FILES=("$SCRIPT_DIR"/*-test.el)
fi

# Build load args
LOAD_ARGS=()
for f in "${TEST_FILES[@]}"; do
    LOAD_ARGS+=(-l "$f")
done

echo "Running tests: ${TEST_FILES[*]}"
echo "---"

emacs --batch \
    --eval "(setq debug-on-error nil)" \
    ${ELPACA:+--eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))"} \
    --eval "(push \"$EXTRAS_DIR\" load-path)" \
    --eval "(push \"$SCRIPT_DIR\" load-path)" \
    -l ert \
    "${LOAD_ARGS[@]}" \
    --eval "(ert-run-tests-batch-and-exit)" \
    2>&1
