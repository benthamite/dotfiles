#!/usr/bin/env bash
# Shared JSON helpers for Codex hook payloads.

CODEX_HOOK_JQ_DEFS='
  def codex_tool_input:
    .tool_input? as $input |
    if ($input | type) == "object" then $input
    elif ($input | type) == "string" then
      ($input as $raw |
       (try ($raw | fromjson) catch {"command": $raw}) |
       if type == "object" then . else {"command": $raw} end)
    else {}
    end;

  def codex_tool_response:
    .tool_response? as $response |
    if ($response | type) == "object" then $response
    elif ($response | type) == "string" then
      ($response as $raw |
       (try ($raw | fromjson) catch {"output": $raw}) |
       if type == "object" then . else {"output": $raw} end)
    else {}
    end;
'

codex_hook_jq() {
  local input="$1"
  local filter="$2"
  printf '%s' "$input" | jq -r "${CODEX_HOOK_JQ_DEFS}
${filter}"
}

codex_tool_name() {
  local input="$1"
  codex_hook_jq "$input" '.tool_name // empty'
}

codex_session_id() {
  local input="$1"
  codex_hook_jq "$input" '.session_id // empty'
}

codex_tool_input_field() {
  local input="$1"
  local field="$2"
  printf '%s' "$input" | jq -r --arg field "$field" "${CODEX_HOOK_JQ_DEFS}
codex_tool_input[\$field] // empty"
}

codex_shell_command() {
  local input="$1"
  codex_hook_jq "$input" '
    codex_tool_input.command //
    codex_tool_input.cmd //
    codex_tool_input.input //
    empty
  '
}

codex_shell_tool_p() {
  local tool_name="$1"
  case "$tool_name" in
    Bash|exec_command|functions.exec|functions.exec_command) return 0 ;;
    *) return 1 ;;
  esac
}

# Extract literal nested exec_command calls from a functions.exec JavaScript
# program without evaluating it. Generated calls use an object literal with a
# literal cmd and, when needed, a literal workdir. Records and commands are
# NUL-delimited so embedded whitespace and newlines survive the trip to Bash.
_codex_nested_exec_values() {
  python3 -c '
import json
import sys

mode = sys.argv[1]
source = sys.stdin.read()
n = len(source)


def skip_space(pos):
    while pos < n:
        if source[pos].isspace():
            pos += 1
        elif source.startswith("//", pos):
            newline = source.find("\n", pos + 2)
            pos = n if newline < 0 else newline + 1
        elif source.startswith("/*", pos):
            end = source.find("*/", pos + 2)
            pos = n if end < 0 else end + 2
        else:
            break
    return pos


def read_string(pos):
    quote = source[pos]
    pos += 1
    value = []
    escapes = {"n": "\n", "r": "\r", "t": "\t", "b": "\b", "f": "\f", "v": "\v"}
    while pos < n:
        char = source[pos]
        if char == quote:
            return "".join(value), pos + 1
        if char == "\\" and pos + 1 < n:
            escaped = source[pos + 1]
            if escaped == "x" and pos + 3 < n:
                try:
                    value.append(chr(int(source[pos + 2:pos + 4], 16)))
                    pos += 4
                    continue
                except ValueError:
                    pass
            if escaped == "u" and pos + 5 < n:
                try:
                    value.append(chr(int(source[pos + 2:pos + 6], 16)))
                    pos += 6
                    continue
                except ValueError:
                    pass
            if escaped == "\n":
                pos += 2
                continue
            value.append(escapes.get(escaped, escaped))
            pos += 2
            continue
        value.append(char)
        pos += 1
    return None, n


def read_identifier(pos):
    start = pos
    while pos < n and (source[pos].isalnum() or source[pos] in "_$"):
        pos += 1
    return source[start:pos], pos


def matching_brace(pos):
    depth = 0
    while pos < n:
        char = source[pos]
        if char in ("\"", chr(39), "`"):
            _, pos = read_string(pos)
            continue
        if source.startswith("//", pos) or source.startswith("/*", pos):
            pos = skip_space(pos)
            continue
        if char == "{":
            depth += 1
        elif char == "}":
            depth -= 1
            if depth == 0:
                return pos
        pos += 1
    return None


def parse_object(start, end):
    fields = {}
    seen = set()
    ambiguous = set()
    relevant = {"cmd", "workdir", "cwd", "working_directory", "working_dir"}
    pos = start + 1
    while pos < end:
        pos = skip_space(pos)
        if pos >= end:
            break
        if source[pos] == ",":
            pos += 1
            continue
        if source.startswith("...", pos) or source[pos] == "[":
            # A spread or computed property can override any direct field.
            ambiguous.update(relevant)
            nested = 0
            while pos < end:
                if source[pos] in ("\"", chr(39), "`"):
                    _, pos = read_string(pos)
                    continue
                if source[pos] in "([{":
                    nested += 1
                elif source[pos] in ")]}" and nested:
                    nested -= 1
                elif source[pos] == "," and nested == 0:
                    break
                pos += 1
            continue
        if source[pos] in ("\"", chr(39)):
            key, pos = read_string(pos)
        elif source[pos].isalpha() or source[pos] in "_$":
            key, pos = read_identifier(pos)
        else:
            pos += 1
            continue
        pos = skip_space(pos)
        if pos >= end or source[pos] != ":":
            continue
        if key in relevant:
            if key in seen:
                ambiguous.add(key)
            seen.add(key)
        pos = skip_space(pos + 1)
        if pos < end and source[pos] in ("\"", chr(39), "`"):
            quote = source[pos]
            value, pos = read_string(pos)
            if value is not None and key in relevant:
                fields[key] = value
                if quote == "`" and "${" in value:
                    ambiguous.add(key)
        else:
            if key in relevant:
                ambiguous.add(key)
            nested = 0
            while pos < end:
                if source[pos] in ("\"", chr(39), "`"):
                    _, pos = read_string(pos)
                    continue
                if source[pos] in "([{":
                    nested += 1
                elif source[pos] in ")]}" and nested:
                    nested -= 1
                elif source[pos] == "," and nested == 0:
                    break
                pos += 1
    context_keys = seen.intersection({"workdir", "cwd", "working_directory", "working_dir"})
    if len(context_keys) > 1:
        ambiguous.update(context_keys)
    return fields, ambiguous


pos = 0
needle = "tools.exec_command"
while pos < n:
    if source[pos] in ("\"", chr(39), "`"):
        _, pos = read_string(pos)
        continue
    if source.startswith("//", pos) or source.startswith("/*", pos):
        pos = skip_space(pos)
        continue
    if not source.startswith(needle, pos):
        pos += 1
        continue
    before_ok = pos == 0 or not (source[pos - 1].isalnum() or source[pos - 1] in "_$.")
    after = pos + len(needle)
    after_ok = after == n or not (source[after].isalnum() or source[after] in "_$")
    if not before_ok or not after_ok:
        pos += 1
        continue
    call = skip_space(after)
    if call >= n or source[call] != "(":
        pos = after
        continue
    obj = skip_space(call + 1)
    if obj >= n or source[obj] != "{":
        pos = call + 1
        continue
    end = matching_brace(obj)
    if end is None:
        break
    fields, ambiguous = parse_object(obj, end)
    command = fields.get("cmd")
    if mode == "commands":
        if command is not None:
            sys.stdout.buffer.write(command.encode("utf-8") + b"\0")
    else:
        workdir = next((fields[key] for key in ("workdir", "cwd", "working_directory", "working_dir") if key in fields), None)
        output = json.dumps(
            {
                "cmd": command,
                "workdir": workdir,
                "ambiguous": command is None or bool(ambiguous),
            },
            separators=(",", ":"),
        )
        sys.stdout.buffer.write(output.encode("utf-8") + b"\0")
    pos = end + 1
' "$1"
}

codex_nested_exec_commands() {
  _codex_nested_exec_values commands
}

codex_nested_exec_contexts() {
  _codex_nested_exec_values contexts
}

codex_parent_exec_workdir() {
  local input="$1"
  local command="$2"
  local transcript source context nested_command nested_workdir
  local found found_workdir invalid

  transcript=$(codex_hook_jq "$input" '.transcript_path // empty')
  [ -f "$transcript" ] || return 1

  while IFS= read -r -d '' source; do
    found=false
    found_workdir=""
    invalid=false
    while IFS= read -r -d '' context; do
      nested_command=$(printf '%s' "$context" | jq -r '.cmd // empty')
      [ "$nested_command" = "$command" ] || continue
      found=true
      if [ "$(printf '%s' "$context" | jq -r '.ambiguous')" = "true" ]; then
        invalid=true
        continue
      fi
      nested_workdir=$(printf '%s' "$context" | jq -r '.workdir // empty')
      if [ -z "$nested_workdir" ] || [ ! -d "$nested_workdir" ]; then
        invalid=true
      elif [ -z "$found_workdir" ]; then
        found_workdir="$nested_workdir"
      elif [ "$found_workdir" != "$nested_workdir" ]; then
        invalid=true
      fi
    done < <(printf '%s' "$source" | codex_nested_exec_contexts)
    if [ "$found" = true ]; then
      if [ "$invalid" = false ] && [ -n "$found_workdir" ]; then
        printf '%s' "$found_workdir"
        return 0
      fi
      return 1
    fi
  done < <(python3 -c '
import json
import subprocess
import sys

result = subprocess.run(
    ["tail", "-n", "64", sys.argv[1]],
    check=False,
    capture_output=True,
    text=True,
)
for line in reversed(result.stdout.splitlines()):
    try:
        event = json.loads(line)
    except json.JSONDecodeError:
        continue
    payload = event.get("payload", {})
    if (payload.get("type") == "custom_tool_call"
            and payload.get("name") == "exec"
            and isinstance(payload.get("input"), str)):
        sys.stdout.buffer.write(payload["input"].encode("utf-8") + b"\0")
' "$transcript")
  return 1
}

# Classify executable-looking shell syntax while ignoring inert quoted text.
# Git classification stays conservative when an interpreter can execute a
# quoted payload. Emacsclient classification requires the executable at a
# command boundary, so quoted commit messages can never clear verification.
_codex_shell_syntax_count() {
  python3 -c '
import re
import sys

mode = sys.argv[1]
source = sys.stdin.read()
interpreter = re.compile(r"(^|[ \t|;&(/])(eval|bash|sh|zsh|dash|ksh|xargs|ssh)([ \t]|$)")
out = []
pos = 0
n = len(source)
while pos < n:
    char = source[pos]
    if char == "\\" and pos + 1 < n:
        out.append(source[pos:pos + 2])
        pos += 2
        continue
    if char == chr(39):
        end = source.find(chr(39), pos + 1)
        if end < 0:
            out.append(source[pos:])
            break
        out.append(chr(39) + chr(39))
        pos = end + 1
        continue
    if char == "\"":
        end = pos + 1
        while end < n:
            if source[end] == "\\" and end + 1 < n:
                end += 2
                continue
            if source[end] == "\"":
                break
            end += 1
        if end >= n:
            out.append(source[pos:])
            break
        span = source[pos + 1:end]
        preserve_expansion = mode == "git" and ("$" in span or "`" in span)
        out.append(source[pos:end + 1] if preserve_expansion else "\"\"")
        pos = end + 1
        continue
    if char == "#" and (pos == 0 or source[pos - 1].isspace()):
        end = source.find("\n", pos + 1)
        if end < 0:
            break
        out.append("\n")
        pos = end + 1
        continue
    out.append(char)
    pos += 1
masked = "".join(out)

if mode == "git":
    scan = source if interpreter.search(source) else masked
    pattern = r"\bgit\s+commit\b"
else:
    scan = masked
    if "elpaca-extras-format-build-reload-status" in source:
        print(0)
        raise SystemExit
    pattern = r"(?:^|[;&|({]\s*)(?:(?:command|exec)\s+)?(?:/(?:opt/homebrew|usr/local|usr)/bin/)?emacsclient(?:\s+[^;&|]*)?\s(?:-e|--eval)\b"

print(len(re.findall(pattern, scan, flags=re.MULTILINE)))
' "$1"
}

codex_git_commit_count() {
  _codex_shell_syntax_count git
}

codex_emacsclient_eval_count() {
  _codex_shell_syntax_count emacsclient
}
