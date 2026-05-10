#!/usr/bin/env bash
# Link the Claude home skill directory to the tracked skill ports.

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
source_dir="$repo_root/claude/skills"
programmatic_source_dir="$repo_root/claude/programmatic-skills"
claude_home="${CLAUDE_CONFIG_DIR:-$HOME/.claude}"
target_dir="$claude_home/skills"
programmatic_target_dir="$claude_home/programmatic-skills"

if [ ! -d "$source_dir" ]; then
  printf 'Missing tracked Claude skills directory: %s\n' "$source_dir" >&2
  exit 1
fi

if [ -L "$target_dir" ]; then
  current=$(readlink "$target_dir")
  if [ "$current" = "$source_dir" ]; then
    printf 'Claude skills directory already linked: %s -> %s\n' "$target_dir" "$source_dir"
  else
    unlink "$target_dir"
    ln -s "$source_dir" "$target_dir"
    printf 'Claude skills directory linked: %s -> %s\n' "$target_dir" "$source_dir"
  fi
elif [ -e "$target_dir" ]; then
  backup="$target_dir.pre-symlink.$(date +%Y%m%d%H%M%S)"
  mv "$target_dir" "$backup"
  printf 'Moved existing Claude skills directory to backup: %s\n' "$backup" >&2
  ln -s "$source_dir" "$target_dir"
  printf 'Claude skills directory linked: %s -> %s\n' "$target_dir" "$source_dir"
else
  ln -s "$source_dir" "$target_dir"
  printf 'Claude skills directory linked: %s -> %s\n' "$target_dir" "$source_dir"
fi

if [ -d "$programmatic_source_dir" ]; then
  if [ -L "$programmatic_target_dir" ]; then
    unlink "$programmatic_target_dir"
  elif [ -e "$programmatic_target_dir" ]; then
    backup="$programmatic_target_dir.pre-symlink.$(date +%Y%m%d%H%M%S)"
    mv "$programmatic_target_dir" "$backup"
    printf 'Moved existing Claude programmatic skills directory to backup: %s\n' "$backup" >&2
  fi
  ln -s "$programmatic_source_dir" "$programmatic_target_dir"
  printf 'Claude programmatic skills directory linked: %s -> %s\n' "$programmatic_target_dir" "$programmatic_source_dir"
fi
"$repo_root/bin/skill-prune" install
