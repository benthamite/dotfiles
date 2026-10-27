#!/bin/sh

set -eu

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
skill_dir=$(dirname "$script_dir")
runtime_dir=$(node "$script_dir/resolve-runtime-dir.mjs" --runtime-root)
export PROOFREAD_RUNTIME_DIR="$runtime_dir"
node_modules_dir=$(node "$script_dir/resolve-runtime-dir.mjs")

if ! command -v npm >/dev/null 2>&1; then
  printf '%s\n' "Error: npm 7 or later is required; no package-manager fallback is used." >&2
  exit 1
fi
case "$(npm --version)" in
  [0-6].*) printf '%s\n' "Error: npm 7 or later is required for this lockfile." >&2; exit 1 ;;
esac
if ! command -v trash >/dev/null 2>&1; then
  printf '%s\n' "Error: trash is required to clean the owned install staging directory." >&2
  exit 1
fi

# npm ci installs into the project's own node_modules, so the project must be
# outside Drive. Stage only these public manifests; never copy the skill or its
# ignored files.
mkdir -p "$runtime_dir"
install_dir=$(mktemp -d "$runtime_dir/.install-XXXXXX")
cleanup() {
  install_status=$?
  trap - 0
  if ! trash "$install_dir"; then
    printf '%s\n' "Error: could not clean the owned install staging directory." >&2
    if [ "$install_status" -eq 0 ]; then install_status=1; fi
  fi
  exit "$install_status"
}
trap cleanup 0
trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM
cp "$skill_dir/package.json" "$install_dir/package.json"
cp "$skill_dir/package-lock.json" "$install_dir/package-lock.json"
mkdir "$install_dir/tmp"
export TMPDIR="$install_dir/tmp"
cd "$install_dir"
npm ci --ignore-scripts --include=dev --no-audit --no-fund --cache "$install_dir/cache"

# Swap the finished tree into the resolved destination. A previous tree moves
# into staging, so the cleanup trap trashes it with the rest of the install.
if [ -e "$node_modules_dir" ]; then
  mv "$node_modules_dir" "$install_dir/previous-node_modules"
fi
mkdir -p "$(dirname "$node_modules_dir")"
mv "$install_dir/node_modules" "$node_modules_dir"
