#!/bin/sh

set -eu

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
runtime_dir=$(node "$script_dir/resolve-runtime-dir.mjs" --runtime-root)
export PROOFREAD_RUNTIME_DIR="$runtime_dir"
node_modules_dir=$(node "$script_dir/resolve-runtime-dir.mjs")
tsx_cli="$node_modules_dir/tsx/dist/cli.mjs"

if [ ! -f "$tsx_cli" ]; then
  printf '%s\n' \
    "Proofread dependencies are not installed outside the sync root." \
    "Run: yarn -s setup-runtime" \
    >&2
  exit 1
fi

exec node "$tsx_cli" "$@"
