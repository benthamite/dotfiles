#!/bin/sh

set -eu

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd -P)
skill_dir=$(dirname "$script_dir")
runtime_dir=$(node "$script_dir/resolve-runtime-dir.mjs" --runtime-root)
export PROOFREAD_RUNTIME_DIR="$runtime_dir"
node_modules_dir=$(node "$script_dir/resolve-runtime-dir.mjs")

if ! command -v yarn >/dev/null 2>&1; then
  printf '%s\n' "Error: Yarn Classic 1.x is required; no package-manager fallback is used." >&2
  exit 1
fi
case "$(yarn --no-default-rc --version)" in
  1.*) ;;
  *) printf '%s\n' "Error: Yarn Classic 1.x is required for this frozen lockfile." >&2; exit 1 ;;
esac
if ! command -v trash >/dev/null 2>&1; then
  printf '%s\n' "Error: trash is required to clean the owned install staging directory." >&2
  exit 1
fi

# --modules-folder alone does not relocate the project cwd or install state.
# Stage only these public manifests; never copy the skill or its ignored files.
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
cp "$skill_dir/yarn.lock" "$install_dir/yarn.lock"
mkdir "$install_dir/tmp"
export TMPDIR="$install_dir/tmp"
cd "$install_dir"
yarn --no-default-rc install --frozen-lockfile --non-interactive --production=false \
  --modules-folder "$node_modules_dir" --cache-folder "$install_dir/cache"
