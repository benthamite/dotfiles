#!/usr/bin/env bash
# Install live Claude and Codex skill symlink farms from the tracked skill trees.

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
exec "$repo_root/bin/skill-prune" install
