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
  local direct patch tool

  tool=$(codex_tool_name "$input")
  direct=$(codex_direct_file_path "$input")
  patch=$(codex_patch_text "$input")
  if [ "$tool" = "apply_patch" ]; then
    # Exempt only the content belonging to a proven secret-file destination.
    # Buffer each section until its optional Move destination is known. Never
    # read target files: a move out of a secret store may carry unchanged
    # secrets absent from the patch, so that operation cannot be classified.
    printf '%s\n' "$patch" | python3 -c '
import sys

# Match Rust str::trim/trim_end and lines, including Unicode White_Space.
# In Update state leading spaces belong to context, never a new file header.
space = " \t\n\r\v\f\u0085\u00a0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u2028\u2029\u202f\u205f\u3000"

def exempt(path):
    return path.endswith((".zshenv-secrets", ".env.op", ".env.local")) or "/.password-store/" in path

def project(text):
    lines = [line.removesuffix("\r") for line in text.strip(space).split("\n")]
    if not lines or lines[0].strip(space) != "*** Begin Patch":
        raise ValueError
    source = destination = operation = ""
    section = []
    output = []
    moved = body = ended = False

    def flush():
        if source:
            if moved and exempt(source) and not exempt(destination):
                raise ValueError
            if not exempt(destination):
                output.extend(section)

    for index, line in enumerate(lines[1:], 1):
        if ended:
            raise ValueError
        metadata = line.rstrip(space) if operation == "Update" else line.strip(space)
        # The native parser also trims both ends of the final End marker.
        if index == len(lines) - 1 and line.strip(space) == "*** End Patch":
            metadata = "*** End Patch"
        if metadata == "*** End Patch":
            flush()
            ended = True
            continue
        header = next((kind for kind in ("Add", "Update", "Delete")
                       if metadata.startswith("*** " + kind + " File: ")), None)
        if header:
            flush()
            operation = header
            source = destination = metadata[len("*** " + header + " File: "):]
            if not source:
                raise ValueError
            section = [line]
            moved = body = False
            continue
        if metadata.startswith("*** Move to: "):
            if operation != "Update" or moved or body:
                raise ValueError
            destination = metadata[len("*** Move to: "):]
            if not destination:
                raise ValueError
            moved = True
        elif not source or operation == "Delete":
            raise ValueError
        elif operation == "Add" and not line.startswith("+"):
            raise ValueError
        elif operation == "Update" and metadata and not (
                line.startswith((" ", "+", "-")) or metadata == "@@" or
                metadata.startswith("@@ ") or metadata == "*** End of File"):
            raise ValueError
        else:
            body = True
        section.append(line)
    if not ended:
        raise ValueError
    return "\n".join(output)

try:
    print(project(sys.stdin.read()))
except ValueError:
    print("Secret patch classification failed; tool execution denied.", file=sys.stderr)
    sys.exit(2)
    '
    return
  fi
  # Native Write/Edit inputs have one destination. Embedded patch-looking
  # content must not introduce another destination or gain its exemption.
  case "$direct" in
    *.zshenv-secrets|*.env.op|*.env.local|*/.password-store/*) return 0 ;;
  esac
  printf '%s\n%s\n' "$direct" "$patch"
  # Native Write/Edit payloads carry their new text outside the patch fields.
  codex_hook_jq "$input" '
    [codex_tool_input.content // "", codex_tool_input.new_string // ""] |
    join("\n")
  '
}
