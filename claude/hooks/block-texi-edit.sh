#!/usr/bin/env bash
# PreToolUse hook: block editing .texi files (auto-generated from .org via ox-texinfo)

FILE_PATH=$(jq -r '.tool_input.file_path // .tool_input.filePath // empty' 2>/dev/null)
[ -z "$FILE_PATH" ] && exit 0

if [[ "$FILE_PATH" == *.texi ]]; then
  echo '{"decision":"block","reason":"BLOCKED: .texi files are auto-generated from .org via ox-texinfo. Edit the corresponding .org file instead, then regenerate."}'
  exit 0
fi
