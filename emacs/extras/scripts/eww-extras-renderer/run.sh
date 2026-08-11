#!/usr/bin/env bash

set -euo pipefail

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
repo_root=$(cd "$script_dir/../../../.." && pwd -P)
drive_root=$(cd "$HOME/My Drive" 2>/dev/null && pwd -P || true)
cache_root=${EWW_EXTRAS_RENDERER_CACHE_DIR:-"$HOME/Library/Caches/eww-extras-renderer"}
node_program=${EWW_EXTRAS_RENDERER_NODE:-node}
npm_program=${EWW_EXTRAS_RENDERER_NPM:-npm}

case "$($node_program -p 'Number(process.versions.node.split(".")[0]) >= 20')" in
  true) ;;
  *) echo "bootstrap: Node.js 20 or later is required" >&2; exit 1 ;;
esac

mkdir -p "$cache_root"
chmod 700 "$cache_root"
cache_root=$(cd "$cache_root" && pwd -P)
case "$cache_root/" in
  "$repo_root/"*|"$drive_root/"*)
    echo "bootstrap: dependency cache must be outside Google Drive" >&2
    exit 1
    ;;
esac

lock_hash=$(shasum -a 256 "$script_dir/package-lock.json" | awk '{print $1}')
completed="$cache_root/$lock_hash"
lock_dir="$cache_root/.lock-$lock_hash"
staging=""
owns_lock=false

cache_valid() {
  [ -f "$completed/.complete" ] &&
    [ "$(cat "$completed/.complete")" = "$lock_hash" ] &&
    [ -f "$completed/node_modules/playwright-core/package.json" ] &&
    [ -f "$completed/node_modules/@duckduckgo/autoconsent/package.json" ]
}

cleanup_bootstrap() {
  if [ -n "$staging" ] && [ -d "$staging" ]; then
    trash "$staging" >/dev/null 2>&1 || true
  fi
  if $owns_lock && [ -d "$lock_dir" ]; then
    find "$lock_dir" -type f -delete
    rmdir "$lock_dir" 2>/dev/null || true
  fi
}
trap cleanup_bootstrap EXIT HUP INT TERM

if ! cache_valid; then
  for _ in $(seq 1 120); do
    if mkdir "$lock_dir" 2>/dev/null; then
      owns_lock=true
      printf '%s\n' "$$" > "$lock_dir/pid"
      break
    fi
    cache_valid && break
    sleep 1
  done
  if ! cache_valid; then
    $owns_lock || { echo "bootstrap: timed out waiting for dependency cache" >&2; exit 1; }
    staging=$(mktemp -d "$cache_root/.staging-$lock_hash.XXXXXX")
    chmod 700 "$staging"
    cp "$script_dir/package.json" "$script_dir/package-lock.json" "$staging/"
    if command -v gtimeout >/dev/null 2>&1; then
      install_timeout=gtimeout
    elif command -v timeout >/dev/null 2>&1; then
      install_timeout=timeout
    else
      echo "bootstrap: timeout or gtimeout is required" >&2
      exit 1
    fi
    (cd "$staging" && "$install_timeout" 120s "$npm_program" ci --ignore-scripts)
    printf '%s\n' "$lock_hash" > "$staging/.complete"
    if [ -e "$completed" ]; then
      trash "$completed"
    fi
    mv "$staging" "$completed"
    staging=""
  fi
fi

"$node_program" "$script_dir/self-check.js" "$completed"
if $owns_lock; then
  find "$lock_dir" -type f -delete
  rmdir "$lock_dir"
  owns_lock=false
fi
trap - EXIT HUP INT TERM

mode=${1:-}
shift || true
case "$mode" in
  self-check) exit 0 ;;
  test)
    EWW_EXTRAS_RENDERER_MODULE_ROOT="$completed" \
      exec "$node_program" --test "$script_dir/test/renderer.test.js"
    ;;
  test-browser)
    [ "${1:-}" = "--chrome-program" ] && [ -n "${2:-}" ] || {
      echo "bootstrap: test-browser requires --chrome-program PATH" >&2
      exit 1
    }
    EWW_EXTRAS_RENDERER_MODULE_ROOT="$completed" \
    EWW_EXTRAS_RENDERER_CHROME_PROGRAM="$2" \
      exec "$node_program" --test "$script_dir/test/renderer.test.js"
    ;;
  serve-fixtures)
    exec "$node_program" "$script_dir/test/fixture-server.js" "$@"
    ;;
  render)
    if command -v gtimeout >/dev/null 2>&1; then
      render_timeout=gtimeout
    elif command -v timeout >/dev/null 2>&1; then
      render_timeout=timeout
    else
      echo "bootstrap: timeout or gtimeout is required" >&2
      exit 1
    fi
    exec "$render_timeout" --signal=TERM --kill-after=2s 30s \
      "$node_program" "$script_dir/../eww-extras-render-url.js" \
      --module-root "$completed" "$@"
    ;;
  *)
    echo "Usage: run.sh {self-check|test|test-browser|serve-fixtures|render}" >&2
    exit 2
    ;;
esac
