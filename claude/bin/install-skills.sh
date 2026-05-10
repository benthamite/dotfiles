#!/usr/bin/env bash
# Link the Claude home skill directory to the tracked skill ports.

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
source_dir="$repo_root/claude/skills"
claude_home="${CLAUDE_CONFIG_DIR:-$HOME/.claude}"
target_dir="$claude_home/skills"

if [ ! -d "$source_dir" ]; then
  printf 'Missing tracked Claude skills directory: %s\n' "$source_dir" >&2
  exit 1
fi

if [ -L "$target_dir" ]; then
  current=$(readlink "$target_dir")
  if [ "$current" = "$source_dir" ]; then
    printf 'Claude skills directory already linked: %s -> %s\n' "$target_dir" "$source_dir"
    "$repo_root/bin/skill-prune" install
    exit 0
  fi
  unlink "$target_dir"
elif [ -e "$target_dir" ]; then
  backup="$target_dir.pre-symlink.$(date +%Y%m%d%H%M%S)"
  mv "$target_dir" "$backup"
  printf 'Moved existing Claude skills directory to backup: %s\n' "$backup" >&2
fi

ln -s "$source_dir" "$target_dir"
printf 'Claude skills directory linked: %s -> %s\n' "$target_dir" "$source_dir"
"$repo_root/bin/skill-prune" install
