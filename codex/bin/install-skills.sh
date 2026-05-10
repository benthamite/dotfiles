#!/usr/bin/env bash
# Link the Codex home skill directory to the tracked skill ports.

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
source_dir="$repo_root/codex/skills"
codex_home="${CODEX_HOME:-$HOME/.codex}"
target_dir="$codex_home/skills"

if [ ! -d "$source_dir" ]; then
  printf 'Missing tracked Codex skills directory: %s\n' "$source_dir" >&2
  exit 1
fi

resolve_link() {
  local link="$1" target
  target=$(readlink "$link")
  case "$target" in
    /*) printf '%s\n' "$target" ;;
    *) printf '%s\n' "$(cd "$(dirname "$link")" && pwd)/$target" ;;
  esac
}

mkdir -p "$codex_home"

if [ -L "$target_dir" ]; then
  current=$(resolve_link "$target_dir")
  if [ "$current" = "$source_dir" ]; then
    printf 'Codex skills directory already linked: %s -> %s\n' "$target_dir" "$source_dir"
    "$repo_root/bin/skill-prune" install
    exit 0
  fi
  unlink "$target_dir"
elif [ -e "$target_dir" ]; then
  backup="$target_dir.pre-symlink.$(date +%Y%m%d%H%M%S)"
  mv "$target_dir" "$backup"
  printf 'Moved existing Codex skills directory to backup: %s\n' "$backup" >&2
  if [ -d "$backup/.system" ] && [ ! -e "$source_dir/.system" ]; then
    mv "$backup/.system" "$source_dir/.system"
    printf 'Preserved Codex system skills under ignored path: %s\n' "$source_dir/.system" >&2
  fi
fi

ln -s "$source_dir" "$target_dir"
printf 'Codex skills directory linked: %s -> %s\n' "$target_dir" "$source_dir"
"$repo_root/bin/skill-prune" install
