#!/usr/bin/env bash
# Link tracked Codex skill ports into the Codex home skill directory.

set -euo pipefail

repo_root=$(cd "$(dirname "$0")/../.." && pwd)
source_dir="$repo_root/codex/skills"
target_dir="${CODEX_HOME:-$HOME/.codex}/skills"

mkdir -p "$target_dir"

created=0
updated=0
skipped=0

for skill_dir in "$source_dir"/*; do
  [ -d "$skill_dir" ] || continue
  [ -f "$skill_dir/SKILL.md" ] || continue

  name=$(basename "$skill_dir")
  target="$target_dir/$name"

  if [ -L "$target" ]; then
    current=$(readlink "$target")
    if [ "$current" = "$skill_dir" ]; then
      continue
    fi
    rm "$target"
    ln -s "$skill_dir" "$target"
    updated=$((updated + 1))
    continue
  fi

  if [ -e "$target" ]; then
    printf 'Skipping existing non-symlink: %s\n' "$target" >&2
    skipped=$((skipped + 1))
    continue
  fi

  ln -s "$skill_dir" "$target"
  created=$((created + 1))
done

printf 'Codex skills linked: %d created, %d updated, %d skipped.\n' "$created" "$updated" "$skipped"

if [ "$skipped" -gt 0 ]; then
  exit 1
fi
