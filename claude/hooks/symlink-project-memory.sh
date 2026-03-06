#!/usr/bin/env bash
# PostToolUse hook: symlink Claude Code memory into the project directory.
#
# On the first memory write in any project, this hook moves the memory files
# from ~/.claude/projects/<encoded-path>/memory/ into <project>/.claude/memory/
# and creates a symlink so future writes go through it transparently.

set -euo pipefail

input=$(cat)

file_path=$(printf '%s' "$input" | jq -r '.tool_input.file_path // empty')
cwd=$(printf '%s' "$input" | jq -r '.cwd // empty')

# Only act on writes to ~/.claude/projects/*/memory/
[[ "$file_path" == "$HOME/.claude/projects/"*/memory/* ]] || exit 0

# Derive the memory directory path
memory_dir="${file_path%/*}"

# Already a symlink — nothing to do
[[ -L "$memory_dir" ]] && exit 0

# Need a cwd to know where the project lives
[[ -n "$cwd" ]] || exit 0

# Set up the project-local memory directory
project_memory="$cwd/.claude/memory"
mkdir -p "$project_memory"

# Move existing files (if any) into the project directory
if [[ -d "$memory_dir" ]]; then
  find "$memory_dir" -maxdepth 1 -type f -exec mv {} "$project_memory/" \;
  rmdir "$memory_dir" 2>/dev/null || trash "$memory_dir"
fi

# Create symlink
ln -s "$project_memory" "$memory_dir"
