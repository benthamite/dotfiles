#!/bin/bash
# Shared helper: recognize dependency lockfiles, which the README commit gates
# do not count as configuration changes that need documenting.
#
# A lockfile records resolved dependency versions (a Dependabot security bump,
# for one) and no behavior a README could describe. Manifests such as
# package.json declare dependencies and scripts, so they still count.
#
# Sourced by require-readme-update.sh and
# codex/hooks/require-codex-readme-update.sh. bin/ai-config-sync keeps the same
# names as DEPENDENCY_LOCKFILE_NAMES for project-local skills;
# tests/test_ai_config_sync_audit.py checks that the two lists match.

DEPENDENCY_LOCKFILE_NAMES=(
  # JavaScript
  package-lock.json npm-shrinkwrap.json yarn.lock pnpm-lock.yaml bun.lock bun.lockb
  # Python
  uv.lock poetry.lock Pipfile.lock
  # Rust and Ruby
  Cargo.lock Gemfile.lock
)

# Succeed when the basename of PATH is a dependency lockfile.
is_dependency_lockfile() {
  local name="${1##*/}" lockfile
  for lockfile in "${DEPENDENCY_LOCKFILE_NAMES[@]}"; do
    if [ "$name" = "$lockfile" ]; then
      return 0
    fi
  done
  return 1
}
