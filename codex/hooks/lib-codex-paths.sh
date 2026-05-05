#!/usr/bin/env bash
# Helpers for Codex hook scripts that need paths from Edit/Write/apply_patch.

# shellcheck source=lib-codex-hook-json.sh
source "$(dirname "${BASH_SOURCE[0]}")/lib-codex-hook-json.sh"

codex_hook_input_json() {
  cat
}

codex_patch_text() {
  local input="$1"
  codex_hook_jq "$input" '
    codex_tool_input.patch //
    codex_tool_input.command //
    codex_tool_input.input //
    codex_tool_input.text //
    empty
  '
}

codex_direct_file_path() {
  local input="$1"
  codex_hook_jq "$input" '
    codex_tool_input.file_path //
    codex_tool_input.filePath //
    codex_tool_input.path //
    codex_tool_input.notebook_path //
    empty
  '
}

codex_changed_paths() {
  local input="$1"
  local direct patch

  direct=$(codex_direct_file_path "$input")
  if [ -n "$direct" ]; then
    printf '%s\n' "$direct"
  fi

  patch=$(codex_patch_text "$input")
  if [ -z "$patch" ]; then
    return 0
  fi

  printf '%s\n' "$patch" | awk '
    /^\*\*\* (Add|Update|Delete) File: / {
      sub(/^\*\*\* (Add|Update|Delete) File: /, "")
      print
    }
    /^\*\*\* Move to: / {
      sub(/^\*\*\* Move to: /, "")
      print
    }
    /^diff --git a\// {
      old=$3
      new=$4
      sub(/^a\//, "", old)
      sub(/^b\//, "", new)
      if (old != "/dev/null") print old
      if (new != "/dev/null") print new
    }
  ' | sort -u
}

codex_patch_content_for_scan() {
  local input="$1"
  local direct patch

  direct=$(codex_direct_file_path "$input")
  patch=$(codex_patch_text "$input")
  printf '%s\n%s\n' "$direct" "$patch"
}
