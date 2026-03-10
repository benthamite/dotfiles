#!/usr/bin/env bash
# Run ERT tests for emacs extras packages.
# Usage: ./run-tests.sh [--ci] [test-file ...]
# Without arguments, runs all *-test.el files in this directory.
# --ci: run each test file in its own Emacs process (tolerates missing deps).

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EXTRAS_DIR="$(dirname "$SCRIPT_DIR")"
DOTFILES_DIR="$(dirname "$(dirname "$EXTRAS_DIR")")"
CI_MODE=false

# Parse flags
args=()
for arg in "$@"; do
    if [[ "$arg" == "--ci" ]]; then
        CI_MODE=true
    else
        args+=("$arg")
    fi
done
set -- "${args[@]+"${args[@]}"}"

# Resolve elpaca profile (skipped in CI mode)
if [[ "$CI_MODE" == true ]]; then
    ELPACA=""
else
    PROFILE=$(emacsclient -e 'init-current-profile' 2>/dev/null | tr -d '"' || echo "")
    if [[ -z "$PROFILE" ]]; then
        echo "Warning: could not determine elpaca profile, trying without elpaca deps" >&2
        ELPACA=""
    else
        ELPACA="$HOME/.config/emacs-profiles/${PROFILE}/elpaca"
    fi
fi

# Collect test files
if [[ $# -gt 0 ]]; then
    TEST_FILES=("$@")
else
    TEST_FILES=("$SCRIPT_DIR"/*-test.el)
fi

# Build common Emacs args
COMMON_ARGS=(
    --eval "(setq debug-on-error nil)"
    ${ELPACA:+--eval "(dolist (dir (file-expand-wildcards \"$ELPACA/builds/*/\")) (add-to-list 'load-path dir))"}
    --eval "(push \"$EXTRAS_DIR\" load-path)"
    --eval "(push \"$SCRIPT_DIR\" load-path)"
    -l ert
)

if [[ "$CI_MODE" == true ]]; then
    # CI mode: run each test file in its own Emacs process
    total=0
    passed=0
    failed=0
    skipped=0
    failed_files=()

    for f in "${TEST_FILES[@]}"; do
        name="$(basename "$f")"
        total=$((total + 1))

        if output=$(emacs --batch "${COMMON_ARGS[@]}" -l "$f" \
            --eval "(ert-run-tests-batch-and-exit)" 2>&1); then
            # Extract test count from output
            count=$(echo "$output" | sed -n 's/.*Ran \([0-9]*\) tests.*/\1/p' | head -1)
            count=${count:-?}
            echo "PASS  $name ($count tests)"
            passed=$((passed + 1))
        else
            rc=$?
            if echo "$output" | grep -q "Cannot open load file\|Required feature"; then
                echo "SKIP  $name (missing dependency)"
                skipped=$((skipped + 1))
            else
                echo "FAIL  $name"
                echo "$output" | grep -E "FAILED|unexpected|condition:" | head -5
                failed=$((failed + 1))
                failed_files+=("$name")
            fi
        fi
    done

    echo ""
    echo "=== CI Summary ==="
    echo "Total: $total  Passed: $passed  Skipped: $skipped  Failed: $failed"

    if [[ $failed -gt 0 ]]; then
        echo "Failed files: ${failed_files[*]}"
        exit 1
    fi
else
    # Normal mode: run all tests in a single Emacs process
    LOAD_ARGS=()
    for f in "${TEST_FILES[@]}"; do
        LOAD_ARGS+=(-l "$f")
    done

    echo "Running tests: ${TEST_FILES[*]}"
    echo "---"

    emacs --batch \
        "${COMMON_ARGS[@]}" \
        "${LOAD_ARGS[@]}" \
        --eval "(ert-run-tests-batch-and-exit)" \
        2>&1
fi
