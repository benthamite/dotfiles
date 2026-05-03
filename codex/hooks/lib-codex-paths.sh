#!/usr/bin/env bash
# Helpers for Codex hook scripts that need paths from Edit/Write/apply_patch.

codex_hook_input_json() {
  cat
}

codex_tool_name() {
  local input="$1"
  printf '%s' "$input" | jq -r '.tool_name // empty'
}

codex_patch_text() {
  local input="$1"
  printf '%s' "$input" | jq -r '
    .tool_input.patch //
    .tool_input.command //
    .tool_input.input //
    .tool_input.text //
    empty
  '
}

codex_direct_file_path() {
  local input="$1"
  printf '%s' "$input" | jq -r '
    .tool_input.file_path //
    .tool_input.filePath //
    .tool_input.path //
    .tool_input.notebook_path //
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
