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
    elif ($response | type) == "array" then
      {"output": ([$response[]? |
                    if type == "object" then (.text // .output // empty)
                    elif type == "string" then .
                    else empty
                    end] | join(""))}
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
import re
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
            # JavaScript unicode escapes carry UTF-16 code units. Combine a
            # valid surrogate pair before UTF-8 serialization; reject lone
            # surrogates rather than losing the entire extracted command.
            decoded = "".join(value).encode("utf-16-le", "surrogatepass").decode("utf-16-le")
            return decoded, pos + 1
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


def mask_non_code(text):
    masked = list(text)
    pos = 0
    limit = len(text)
    while pos < limit:
        if text[pos] in ("\"", chr(39), "`"):
            quote = text[pos]
            end = pos + 1
            while end < limit:
                if text[end] == "\\" and end + 1 < limit:
                    end += 2
                    continue
                if text[end] == quote:
                    end += 1
                    break
                end += 1
            for index in range(pos, min(end, limit)):
                masked[index] = " "
            pos = end
            continue
        if text.startswith("//", pos):
            end = text.find("\n", pos + 2)
            end = limit if end < 0 else end
            for index in range(pos, end):
                masked[index] = " "
            pos = end
            continue
        if text.startswith("/*", pos):
            end = text.find("*/", pos + 2)
            end = limit if end < 0 else min(limit, end + 2)
            for index in range(pos, end):
                masked[index] = " "
            pos = end
            continue
        pos += 1
    return "".join(masked)


def literal_object_binding(name, before):
    prefix = source[:before]
    masked = mask_non_code(prefix)
    pattern = re.compile(
        r"\b(?:const|let|var)\s+" + re.escape(name) + r"\s*=\s*\{"
    )
    for match in reversed(list(pattern.finditer(masked))):
        start = masked.find("{", match.start(), match.end())
        end = matching_brace(start)
        if end is None or end >= before:
            continue
        intervening = mask_non_code(source[end + 1:before])
        if re.search(r"\b" + re.escape(name) + r"\b", intervening):
            return None
        return start, end
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
                # A literal is safe only when it is the complete value
                # expression. Concatenation, property access, calls, and other
                # trailing syntax can change the value that reaches the tool.
                value_end = skip_space(pos)
                if value_end < end and source[value_end] not in ",}":
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
    if source.startswith("tools", pos):
        before_ok = pos == 0 or not (source[pos - 1].isalnum() or source[pos - 1] in "_$")
        after_tools = pos + len("tools")
        after_ok = after_tools == n or not (source[after_tools].isalnum() or source[after_tools] in "_$")
        if before_ok and after_ok and not source.startswith("tools.", pos):
            if mode == "contexts":
                sys.stdout.buffer.write(
                    json.dumps(
                        {"cmd": None, "workdir": None, "ambiguous": True},
                        separators=(",", ":"),
                    ).encode("utf-8") + b"\0"
                )
            pos = after_tools
            continue
    # Computed tool access cannot be tied to a literal tool name without
    # evaluating JavaScript. Reject it instead of trying to recognize selected
    # spellings such as tools["exec_command"].
    if source.startswith("tools[", pos):
        if mode == "contexts":
            sys.stdout.buffer.write(
                json.dumps(
                    {"cmd": None, "workdir": None, "ambiguous": True},
                    separators=(",", ":"),
                ).encode("utf-8") + b"\0"
            )
        pos += len("tools[")
        continue
    # Destructuring and other indirect references can alias exec_command
    # without spelling tools.exec_command at the call site.
    if source.startswith("exec_command", pos):
        before_ok = pos == 0 or not (source[pos - 1].isalnum() or source[pos - 1] in "_$")
        after = pos + len("exec_command")
        after_ok = after == n or not (source[after].isalnum() or source[after] in "_$")
        if before_ok and after_ok:
            if mode == "contexts":
                sys.stdout.buffer.write(
                    json.dumps(
                        {"cmd": None, "workdir": None, "ambiguous": True},
                        separators=(",", ":"),
                    ).encode("utf-8") + b"\0"
                )
            pos = after
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
        if mode == "contexts":
            sys.stdout.buffer.write(
                json.dumps(
                    {"cmd": None, "workdir": None, "ambiguous": True},
                    separators=(",", ":"),
                ).encode("utf-8") + b"\0"
            )
        pos = after
        continue
    obj = skip_space(call + 1)
    call_end = None
    if obj < n and (source[obj].isalpha() or source[obj] in "_$"):
        name, argument_end = read_identifier(obj)
        closing = skip_space(argument_end)
        binding = literal_object_binding(name, obj) if closing < n and source[closing] == ")" else None
        if binding is not None:
            obj, end = binding
            call_end = closing
        else:
            if mode == "contexts":
                sys.stdout.buffer.write(
                    json.dumps(
                        {"cmd": None, "workdir": None, "ambiguous": True},
                        separators=(",", ":"),
                    ).encode("utf-8") + b"\0"
                )
            pos = argument_end
            continue
    elif obj >= n or source[obj] != "{":
        if mode == "contexts":
            sys.stdout.buffer.write(
                json.dumps(
                    {"cmd": None, "workdir": None, "ambiguous": True},
                    separators=(",", ":"),
                ).encode("utf-8") + b"\0"
            )
        pos = call + 1
        continue
    else:
        end = matching_brace(obj)
    if end is None:
        if mode == "contexts":
            sys.stdout.buffer.write(
                json.dumps(
                    {"cmd": None, "workdir": None, "ambiguous": True},
                    separators=(",", ":"),
                ).encode("utf-8") + b"\0"
            )
        break
    fields, ambiguous = parse_object(obj, end)
    if any("\0" in value for value in fields.values()):
        raise ValueError("NUL is unsupported in nested command or workdir")
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
    pos = call_end + 1 if call_end is not None else end + 1
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

# Emit the Git invocations in a shell program as NUL-delimited JSON records.
# Each record contains the subcommand, its arguments, and the Git global
# options that precede it. This lets callers recognize forms such as
# `git -C repo commit` without evaluating the command.
codex_git_invocations() {
  python3 -c '
import json
import os
import shlex
import subprocess
import sys

source = sys.stdin.read()
context_dir = sys.argv[1] if len(sys.argv) > 1 and os.path.isdir(sys.argv[1]) else os.getcwd()
boundaries = {";", ";;", "&", "&&", "|", "||", "(", ")", "\n"}
interpreters = {"bash", "sh", "zsh", "dash", "ksh"}
global_value_options = {
    "-C", "-c", "--git-dir", "--work-tree", "--namespace",
    "--super-prefix", "--config-env", "--exec-path",
}
global_flag_options = {
    "--bare", "--no-pager", "--paginate", "-p", "--literal-pathspecs",
    "--glob-pathspecs", "--noglob-pathspecs", "--icase-pathspecs",
    "--no-optional-locks", "--no-replace-objects",
}
terminal_global_options = {"--version", "--help", "-h"}


def load_builtin_commands():
    try:
        result = subprocess.run(
            ["git", "--list-cmds=builtins"],
            check=True,
            capture_output=True,
            text=True,
            timeout=2,
        )
        return set(result.stdout.split())
    except (OSError, subprocess.SubprocessError):
        # With no command inventory, unknown Git subcommands must remain
        # ambiguous. Literal core commands are added so routine hooks still
        # classify the operations they are designed to inspect.
        return {
            "add", "commit", "config", "diff", "diff-tree", "ls-files",
            "rev-parse", "status",
        }


builtin_commands = load_builtin_commands()


def without_literal_heredocs(program):
    """Remove quoted commit-message input before identifying Git arguments.

    Only a literal git commit reading its message from stdin is a proven data
    sink here. Shells, interpreters, unknown sinks, and unquoted heredocs remain
    visible to the existing conservative parser.
    """
    masked = list(program)
    pending = []
    quote = None
    pos = 0
    while pos < len(program):
        char = program[pos]
        if quote:
            if char == chr(92) and quote != chr(39):
                pos += 2
                continue
            if char == quote:
                quote = None
        elif char == chr(92):
            pos += 2
            continue
        elif char in (chr(39), chr(34), chr(96)):
            quote = char
        elif char == "#":
            end = program.find("\n", pos)
            pos = len(program) if end < 0 else end
            continue
        elif program.startswith("<<", pos) and not program.startswith("<<<", pos):
            start = program.rfind("\n", 0, pos) + 1
            try:
                prefix_lexer = shlex.shlex(program[start:pos], posix=True,
                                           punctuation_chars=";&|()")
                prefix_lexer.whitespace_split = True
                words = []
                for token in prefix_lexer:
                    if token and all(c in ";&|()" for c in token):
                        words = []
                    else:
                        words.append(token)
            except ValueError:
                return program
            message_stdin = any(words[i] in {"-F", "--file"} and words[i + 1] == "-"
                                for i in range(len(words) - 1)) or "--file=-" in words
            if words[:2] != ["git", "commit"] or not message_stdin:
                pos += 2
                continue
            end = pos + 2
            strip_tabs = end < len(program) and program[end] == "-"
            if strip_tabs:
                end += 1
            while end < len(program) and program[end] in " \t":
                end += 1
            if end < len(program) and program[end] in (chr(39), chr(34)):
                closing = program.find(program[end], end + 1)
                delimiter = program[end + 1:closing] if closing >= 0 else ""
                if delimiter and all(c.isalnum() or c == "_" for c in delimiter):
                    pending.append((delimiter, strip_tabs))
                    for index in range(pos, closing + 1):
                        masked[index] = " "
                    pos = closing + 1
                    continue
        if char == "\n" and pending and quote is None:
            body_start = pos + 1
            for delimiter, strip_tabs in pending:
                cursor = body_start
                while cursor < len(program):
                    end = program.find("\n", cursor)
                    if end < 0:
                        end = len(program)
                    line = program[cursor:end]
                    if (line.lstrip("\t") if strip_tabs else line) == delimiter:
                        for index in range(body_start, end):
                            if masked[index] != "\n":
                                masked[index] = " "
                        body_start = end + (end < len(program))
                        break
                    cursor = end + 1
                else:
                    return program
            pending = []
            pos = body_start
            continue
        pos += 1
    return program if pending else "".join(masked)


def tokenize(program):
    try:
        lexer = shlex.shlex(without_literal_heredocs(program), posix=True, punctuation_chars=";&|()\n")
        lexer.whitespace = " \t\r"
        lexer.whitespace_split = True
        lexer.commenters = "#"
        return list(lexer)
    except ValueError:
        return []


def is_assignment(token):
    if "=" not in token or token.startswith(("/", "./", "../")):
        return False
    name = token.partition("=")[0]
    return bool(name) and (name[0].isalpha() or name[0] == "_") and all(
        char.isalnum() or char == "_" for char in name
    )


def split_segments(tokens):
    segment = []
    for token in tokens:
        if token in boundaries or (token and all(char in ";&|()\n" for char in token)):
            if segment:
                yield segment, token
                segment = []
        else:
            segment.append(token)
    if segment:
        yield segment, None


def ambiguous_commit(
    reason, assignments=None, global_args=None, invoked=None, invocation_dir=None
):
    record = {
        "subcommand": "commit",
        "args": [],
        "global_args": global_args or [],
        "assignments": assignments or [],
        "ambiguous": True,
        "ambiguity": reason,
        "context_dir": invocation_dir or context_dir,
    }
    if invoked is not None:
        record["invoked_subcommand"] = invoked
    return record


def assignment_name(token):
    return token.partition("=")[0]


def git_assignments_ambiguous(assignments):
    return any(assignment_name(token).startswith("GIT_") for token in assignments)


def config_aliases(global_args):
    aliases = {}
    pos = 0
    while pos < len(global_args):
        token = global_args[pos]
        value = None
        if token == "-c" and pos + 1 < len(global_args):
            value = global_args[pos + 1]
            pos += 2
        elif token.startswith("-c") and token != "-c":
            value = token[2:]
            pos += 1
        else:
            pos += 1
        if value is None or "=" not in value:
            continue
        key, _, alias_value = value.partition("=")
        if key.lower().startswith("alias.") and len(key) > len("alias."):
            aliases[key[len("alias."):]] = alias_value
    return aliases


def configured_alias(name, global_args, assignments, invocation_dir):
    effective_env = os.environ.copy()
    for assignment in assignments:
        key, _, value = assignment.partition("=")
        effective_env[key] = value
    try:
        result = subprocess.run(
            ["git"] + global_args + ["config", "--get", "alias." + name],
            cwd=invocation_dir,
            env=effective_env,
            check=False,
            capture_output=True,
            text=True,
            timeout=2,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if result.returncode != 0:
        return None
    return result.stdout.rstrip("\n")


def parse_git(
    words,
    assignments=None,
    inherited_aliases=None,
    alias_depth=0,
    invocation_dir=None,
):
    assignments = assignments or []
    invocation_dir = invocation_dir or context_dir
    global_args = []
    ambiguous = False
    pos = 1
    while pos < len(words):
        token = words[pos]
        if token in terminal_global_options:
            return None
        if token == "--":
            global_args.append(token)
            pos += 1
            break
        if token in global_value_options:
            global_args.append(token)
            pos += 1
            if pos >= len(words):
                return None
            global_args.append(words[pos])
            pos += 1
            continue
        if any(token.startswith(option + "=") for option in global_value_options if option.startswith("--")):
            global_args.append(token)
            pos += 1
            continue
        if token.startswith("-C") and token != "-C":
            global_args.append(token)
            pos += 1
            continue
        if token.startswith("-c") and token != "-c":
            global_args.append(token)
            pos += 1
            continue
        if token in global_flag_options:
            global_args.append(token)
            pos += 1
            continue
        if token.startswith("-"):
            # An unknown option may or may not consume the next token. Keep
            # scanning, but mark a later commit token as ambiguous so policy
            # gates can fail closed instead of mistaking an option value for
            # the subcommand.
            global_args.append(token)
            ambiguous = True
            pos += 1
            continue
        if ambiguous and "commit" in words[pos:]:
            commit_pos = words.index("commit", pos)
            record = {
                "subcommand": "commit",
                "args": words[commit_pos + 1:],
                "global_args": words[1:commit_pos],
                "assignments": assignments,
                "ambiguous": True,
                "ambiguity": "unknown-git-global-option",
                "context_dir": invocation_dir,
            }
            return record

        aliases = dict(inherited_aliases or {})
        aliases.update(config_aliases(global_args))
        if token not in aliases and token not in builtin_commands:
            alias_value = configured_alias(
                token, global_args, assignments, invocation_dir
            )
            if alias_value is not None:
                aliases[token] = alias_value
        if token in aliases:
            if alias_depth >= 8:
                return ambiguous_commit(
                    "git-alias-depth", assignments, global_args, token,
                    invocation_dir
                )
            alias_value = aliases[token]
            if alias_value.startswith("!"):
                return ambiguous_commit(
                    "git-shell-alias", assignments, global_args, token,
                    invocation_dir
                )
            try:
                alias_words = shlex.split(alias_value, posix=True)
            except ValueError:
                return ambiguous_commit(
                    "invalid-git-alias", assignments, global_args, token,
                    invocation_dir
                )
            if not alias_words:
                return ambiguous_commit(
                    "empty-git-alias", assignments, global_args, token,
                    invocation_dir
                )
            record = parse_git(
                ["git"] + alias_words + words[pos + 1:],
                assignments,
                aliases,
                alias_depth + 1,
                invocation_dir,
            )
            if record is None:
                return None
            record["global_args"] = global_args
            record["invoked_subcommand"] = token
            if git_assignments_ambiguous(assignments):
                record["ambiguous"] = True
                record["ambiguity"] = "git-environment"
            return record

        dynamic_subcommand = any(char in token for char in "$`{}*?[")
        if dynamic_subcommand:
            return ambiguous_commit(
                "dynamic-git-subcommand", assignments, global_args, token,
                invocation_dir
            )
        record = {
            "subcommand": token,
            "args": words[pos + 1:],
            "global_args": global_args,
            "assignments": assignments,
            "ambiguous": git_assignments_ambiguous(assignments),
            "context_dir": invocation_dir,
        }
        if record["ambiguous"]:
            record["ambiguity"] = "git-environment"
        return record
    return None


def consume_command_wrapper(words, pos):
    wrapper = words[pos]
    pos += 1
    if wrapper == "command":
        while pos < len(words):
            token = words[pos]
            if token == "--":
                return pos + 1, False
            if token in {"-v", "-V"}:
                return pos + 1, True
            if token == "-p":
                pos += 1
                continue
            break
        return pos, False
    while pos < len(words):
        token = words[pos]
        if token == "--":
            return pos + 1, False
        if token in {"-c", "-l"}:
            pos += 1
            continue
        if token == "-a":
            return pos + 2, pos + 1 >= len(words)
        if token.startswith("-a") and token != "-a":
            pos += 1
            continue
        break
    return pos, False


def interpreter_command_index(command_words):
    pos = 1
    while pos < len(command_words):
        option = command_words[pos]
        if option == "--":
            return None
        if option in {"--norc", "--noprofile", "--posix", "--restricted", "--verbose"}:
            pos += 1
            continue
        if option in {"--rcfile", "--init-file"}:
            pos += 2
            continue
        if option.startswith("--rcfile=") or option.startswith("--init-file="):
            pos += 1
            continue
        if option in {"-o", "+o", "-O", "+O"}:
            pos += 2
            continue
        if option.startswith("-") and not option.startswith("--") and "c" in option[1:]:
            return pos + 1
        if option.startswith("+") and "c" in option[1:]:
            return pos + 1
        if option.startswith("-") or option.startswith("+"):
            pos += 1
            continue
        return None
    return None


def extract_substitutions(program):
    masked = []
    commands = []
    pos = 0
    single_quoted = False
    double_quoted = False
    while pos < len(program):
        char = program[pos]
        if char == "\\" and pos + 1 < len(program):
            masked.append(program[pos:pos + 2])
            pos += 2
            continue
        if char == chr(39) and not double_quoted:
            single_quoted = not single_quoted
            masked.append(char)
            pos += 1
            continue
        if char == "\"" and not single_quoted:
            double_quoted = not double_quoted
            masked.append(char)
            pos += 1
            continue
        if not single_quoted and program.startswith("$(", pos):
            inner = pos + 2
            end = inner
            depth = 1
            inner_single = False
            inner_double = False
            while end < len(program):
                current = program[end]
                if current == "\\" and end + 1 < len(program):
                    end += 2
                    continue
                if current == chr(39) and not inner_double:
                    inner_single = not inner_single
                    end += 1
                    continue
                if current == "\"" and not inner_single:
                    inner_double = not inner_double
                    end += 1
                    continue
                if not inner_single and not inner_double:
                    if program.startswith("$(", end):
                        depth += 1
                        end += 2
                        continue
                    if current == ")":
                        depth -= 1
                        if depth == 0:
                            break
                end += 1
            if depth != 0:
                return program, [], True
            commands.append(program[inner:end])
            masked.append("__CODEX_SUB_" + str(len(commands) - 1) + "__")
            pos = end + 1
            continue
        if not single_quoted and char == "`":
            end = pos + 1
            while end < len(program):
                if program[end] == "\\" and end + 1 < len(program):
                    end += 2
                    continue
                if program[end] == "`":
                    break
                end += 1
            if end >= len(program):
                return program, [], True
            commands.append(program[pos + 1:end])
            masked.append("__CODEX_SUB_" + str(len(commands) - 1) + "__")
            pos = end + 1
            continue
        masked.append(char)
        pos += 1
    return "".join(masked), commands, False


def static_substitution_output(program):
    segments = list(split_segments(tokenize(program)))
    if len(segments) != 1 or segments[0][1] is not None:
        return None
    words = segments[0][0]
    if not words or os.path.basename(words[0]) != "printf":
        return None
    if len(words) == 2 and words[1] in {"commit", "status"}:
        return words[1]
    if len(words) == 3 and words[1] in {"%s", "%s\\n"}:
        if words[2] in {"commit", "status"}:
            return words[2]
    return None


def resolved_context(base_dir, value):
    if not value or any(char in value for char in "$`"):
        return None
    value = os.path.expanduser(value)
    if not os.path.isabs(value):
        value = os.path.join(base_dir, value)
    return os.path.abspath(value)


def mark_records_ambiguous(records, reason):
    for record in records:
        if record.get("subcommand") == "commit":
            record["ambiguous"] = True
            record["ambiguity"] = reason
    return records


def scan(program, depth=0, initial_dir=None):
    initial_dir = initial_dir or context_dir
    if depth > 4:
        return [ambiguous_commit(
            "shell-recursion-limit", invocation_dir=initial_dir
        )]
    program = program.replace(chr(92) + "\n", "")
    records = []
    masked_program, substitutions, invalid_substitution = extract_substitutions(program)
    if invalid_substitution:
        return [ambiguous_commit(
            "invalid-command-substitution", invocation_dir=initial_dir
        )]
    for substitution_index, nested_program in enumerate(substitutions):
        placeholder = "__CODEX_SUB_" + str(substitution_index) + "__"
        static_output = static_substitution_output(nested_program)
        if static_output is not None:
            masked_program = masked_program.replace(placeholder, static_output)
            continue
        masked_program = masked_program.replace(
            placeholder, "$CODEX_DYNAMIC_SUB_" + str(substitution_index)
        )
        nested_records = scan(nested_program, depth + 1, initial_dir)
        records.extend(mark_records_ambiguous(
            nested_records, "shell-command-substitution"
        ))
    shell_variables = {}
    current_dir = initial_dir
    context_unknown = False
    segments = list(split_segments(tokenize(masked_program)))
    for segment_index, (words, terminator) in enumerate(segments):
        pos = 0
        assignments = []
        control_ambiguous = False
        while pos < len(words) and is_assignment(words[pos]):
            assignments.append(words[pos])
            pos += 1

        if pos >= len(words):
            if terminator in {";", ";;", "&&", "||", "\n"}:
                for assignment in assignments:
                    name, _, value = assignment.partition("=")
                    if not any(char in value for char in "$`"):
                        shell_variables[name] = value
            continue

        if words[pos] == "function" and pos + 2 < len(words):
            try:
                body_start = words.index("{", pos + 2) + 1
            except ValueError:
                continue
            nested = scan(" ".join(words[body_start:]), depth + 1, current_dir)
            records.extend(mark_records_ambiguous(nested, "shell-function"))
            continue

        while pos < len(words) and words[pos] in {"!", "if", "then", "elif", "while", "until", "do", "{"}:
            if words[pos] in {"!", "if", "elif", "while", "until"}:
                control_ambiguous = True
            pos += 1
        if pos < len(words) and words[pos] in {"time", "noglob"}:
            pos += 1
            while pos < len(words) and words[pos] in {"-p", "--"}:
                pos += 1
        while pos < len(words) and words[pos] in {"command", "exec"}:
            pos, terminal = consume_command_wrapper(words, pos)
            if terminal:
                pos = len(words)
                break
        if pos >= len(words):
            continue

        invocation_dir = current_dir
        invocation_context_unknown = context_unknown
        if words[pos] == "env":
            pos += 1
            while pos < len(words):
                token = words[pos]
                if token == "--":
                    pos += 1
                    break
                if token in {"-i", "--ignore-environment", "-0", "--null"}:
                    pos += 1
                    continue
                if token in {"-u", "--unset"}:
                    pos += 2
                    continue
                if token in {"-C", "--chdir"}:
                    if pos + 1 >= len(words):
                        invocation_context_unknown = True
                        pos += 1
                        continue
                    target_dir = resolved_context(invocation_dir, words[pos + 1])
                    if target_dir is None:
                        invocation_context_unknown = True
                    else:
                        invocation_dir = target_dir
                    pos += 2
                    continue
                if token.startswith("--chdir="):
                    target_dir = resolved_context(invocation_dir, token.partition("=")[2])
                    if target_dir is None:
                        invocation_context_unknown = True
                    else:
                        invocation_dir = target_dir
                    pos += 1
                    continue
                if token in {"-S", "--split-string"}:
                    if pos + 1 >= len(words):
                        break
                    try:
                        split_words = shlex.split(words[pos + 1], posix=True)
                    except ValueError:
                        break
                    words = words[:pos] + split_words + words[pos + 2:]
                    continue
                if token.startswith("--split-string="):
                    try:
                        split_words = shlex.split(token.partition("=")[2], posix=True)
                    except ValueError:
                        break
                    words = words[:pos] + split_words + words[pos + 1:]
                    continue
                if token.startswith("--unset="):
                    pos += 1
                    continue
                if is_assignment(token):
                    assignments.append(token)
                    pos += 1
                    continue
                break
            if pos >= len(words):
                continue

        if os.path.basename(words[pos]) == "nice":
            pos += 1
            while pos < len(words):
                token = words[pos]
                if token == "--":
                    pos += 1
                    break
                if token in {"-n", "--adjustment"}:
                    pos += 2
                    continue
                if token.startswith("--adjustment=") or (
                    token.startswith("-") and token[1:].lstrip("+").isdigit()
                ):
                    pos += 1
                    continue
                break
        if pos < len(words) and os.path.basename(words[pos]) == "nohup":
            pos += 1
            if pos < len(words) and words[pos] == "--":
                pos += 1
            elif pos < len(words) and words[pos] in {"--help", "--version"}:
                continue
        if pos >= len(words):
            continue

        executable = os.path.basename(words[pos])
        command_words = words[pos:]
        if executable == "cd":
            cd_pos = 1
            if cd_pos < len(command_words) and command_words[cd_pos] == "--":
                cd_pos += 1
            target_dir = (
                resolved_context(current_dir, command_words[cd_pos])
                if cd_pos < len(command_words) else None
            )
            if target_dir is None:
                context_unknown = True
            else:
                current_dir = target_dir
                context_unknown = False
            continue
        if executable == "git":
            if len(command_words) > 1:
                subcommand = command_words[1]
                variable_name = None
                if subcommand.startswith("${") and subcommand.endswith("}"):
                    variable_name = subcommand[2:-1]
                elif subcommand.startswith("$"):
                    variable_name = subcommand[1:]
                if variable_name in shell_variables:
                    command_words = [
                        command_words[0], shell_variables[variable_name]
                    ] + command_words[2:]
                elif (
                    subcommand.startswith("{")
                    and subcommand.endswith("}")
                    and "," in subcommand
                ):
                    command_words = [command_words[0]] + subcommand[1:-1].split(",") + command_words[2:]
            record = parse_git(
                command_words, assignments, invocation_dir=invocation_dir
            )
            if record is not None:
                if record["subcommand"] == "commit" and invocation_context_unknown:
                    record["ambiguous"] = True
                    record["ambiguity"] = "dynamic-shell-context"
                if record["subcommand"] == "commit" and control_ambiguous:
                    record["ambiguous"] = True
                    record["ambiguity"] = "shell-control-flow"
                later_command = segment_index + 1 < len(segments)
                status_decoupled = terminator in {"|", "||", "&", "&&"} or (
                    terminator in {";", ";;", "\n"} and later_command
                )
                if (
                    record["subcommand"] == "commit"
                    and status_decoupled
                    and not record["ambiguous"]
                ):
                    record["ambiguous"] = True
                    record["ambiguity"] = "shell-status-decoupled"
                    record["status_operator"] = terminator
                records.append(record)
            continue
        if executable == "eval":
            if len(command_words) < 2:
                continue
            nested = scan(" ".join(command_words[1:]), depth + 1, invocation_dir)
            records.extend(mark_records_ambiguous(nested, "shell-eval"))
            continue
        if executable in interpreters:
            command_index = interpreter_command_index(command_words)
            if command_index is not None and command_index < len(command_words):
                records.extend(scan(
                    command_words[command_index], depth + 1, invocation_dir
                ))
            elif command_index is not None:
                records.append(ambiguous_commit(
                    "missing-interpreter-command", invocation_dir=invocation_dir
                ))
            continue
        if executable == "xargs":
            inner_pos = 1
            value_options = {
                "-a", "--arg-file", "-E", "--eof", "-I", "--replace",
                "-L", "--max-lines", "-n", "--max-args", "-P", "--max-procs",
                "-s", "--max-chars",
            }
            while inner_pos < len(command_words):
                token = command_words[inner_pos]
                if token == "--":
                    inner_pos += 1
                    break
                if token in value_options:
                    inner_pos += 2
                    continue
                if token.startswith("--") and "=" in token:
                    inner_pos += 1
                    continue
                if token.startswith(("-I", "-L", "-n", "-P", "-s")):
                    inner_pos += 1
                    continue
                if token.startswith("-"):
                    inner_pos += 1
                    continue
                break
            if inner_pos < len(command_words):
                nested = scan(
                    " ".join(command_words[inner_pos:]), depth + 1, invocation_dir
                )
                records.extend(mark_records_ambiguous(nested, "shell-xargs"))
            continue
        if executable == "find" and "-exec" in command_words:
            inner_pos = command_words.index("-exec") + 1
            if inner_pos < len(command_words):
                nested = scan(
                    " ".join(command_words[inner_pos:]), depth + 1, invocation_dir
                )
                records.extend(mark_records_ambiguous(
                    nested, "shell-find-exec"
                ))
            continue
        if executable.startswith("$") or executable.startswith("`"):
            if "commit" in command_words[1:]:
                records.append(ambiguous_commit(
                    "dynamic-shell-executable", invocation_dir=invocation_dir
                ))
    return records


for sequence, record in enumerate(scan(source)):
    record["sequence"] = sequence
    sys.stdout.buffer.write(
        json.dumps(record, separators=(",", ":")).encode("utf-8") + b"\0"
    )
' "${1:-${CODEX_GIT_PARSE_CONTEXT:-}}"
}

codex_git_subcommand_count() {
  local target="$1"
  local context_dir="${2:-${CODEX_GIT_PARSE_CONTEXT:-}}"
  local record count=0
  while IFS= read -r -d '' record; do
    if [ "$(printf '%s' "$record" | jq -r '.subcommand')" = "$target" ]; then
      count=$((count + 1))
    fi
  done < <(codex_git_invocations "$context_dir")
  printf '%s\n' "$count"
}

codex_git_invocation_repo() {
  local record="$1"
  local context_dir="$2"
  local value have_global_args=false have_assignments=false
  local -a global_args=() assignments=()
  while IFS= read -r -d '' value; do
    global_args+=("$value")
    have_global_args=true
  done < <(printf '%s' "$record" | jq -j '.global_args[] | ., "\u0000"')
  while IFS= read -r -d '' value; do
    assignments+=("$value")
    have_assignments=true
  done < <(printf '%s' "$record" | jq -j '.assignments[] | ., "\u0000"')
  (
    cd -- "$context_dir" 2>/dev/null || exit 1
    if [ "$have_global_args" = true ] && [ "$have_assignments" = true ]; then
      env "${assignments[@]}" git "${global_args[@]}" rev-parse --show-toplevel 2>/dev/null
    elif [ "$have_global_args" = true ]; then
      git "${global_args[@]}" rev-parse --show-toplevel 2>/dev/null
    elif [ "$have_assignments" = true ]; then
      env "${assignments[@]}" git rev-parse --show-toplevel 2>/dev/null
    else
      git rev-parse --show-toplevel 2>/dev/null
    fi
  )
}

# Reconstruct one parsed Git record as a shell-safe literal command. Callers
# use this only to route that record through an existing per-command policy
# check; no reconstructed text is evaluated in this helper.
codex_git_record_command() {
  local record="$1" value
  while IFS= read -r -d '' value; do
    printf '%q ' "$value"
  done < <(printf '%s' "$record" | jq -j '.assignments[] | ., "\u0000"')
  printf 'git '
  while IFS= read -r -d '' value; do
    printf '%q ' "$value"
  done < <(printf '%s' "$record" | jq -j '.global_args[] | ., "\u0000"')
  value=$(printf '%s' "$record" | jq -r '.subcommand')
  printf '%q ' "$value"
  while IFS= read -r -d '' value; do
    printf '%q ' "$value"
  done < <(printf '%s' "$record" | jq -j '.args[] | ., "\u0000"')
}

# Return success when a parsed `git add` invocation would select a changed
# Elisp source or emacs/config.org. This is read-only: it asks Git which changed
# tracked and untracked paths match the invocation pathspecs.
codex_git_add_selects_elisp() {
  local repo="$1"
  local record="$2"
  local value argument tracked_only=false all_paths=false
  local expect_value=false pathspec_from_file=false after_double_dash=false
  local -a add_args=() pathspecs=() list_args=(-m -d)

  while IFS= read -r -d '' value; do
    add_args+=("$value")
  done < <(printf '%s' "$record" | jq -j '.args[] | ., "\u0000"')
  for argument in "${add_args[@]}"; do
    if [ "$after_double_dash" = true ]; then
      pathspecs+=("$argument")
      continue
    fi
    if [ "$expect_value" = true ]; then
      expect_value=false
      pathspec_from_file=true
      continue
    fi
    case "$argument" in
      --) after_double_dash=true ;;
      -A|--all) all_paths=true ;;
      -u|--update) tracked_only=true; all_paths=true ;;
      -p|--patch|-i|--interactive|-e|--edit|-U|--unified|--unified=*|-U*) all_paths=true ;;
      --pathspec-from-file) expect_value=true ;;
      --pathspec-from-file=*) pathspec_from_file=true ;;
      --chmod) expect_value=true ;;
      --chmod=*|-N|--intent-to-add|-f|--force|--ignore-errors|--ignore-missing|--renormalize|--sparse|--refresh|--no-all|--no-ignore-removal) ;;
      -*A*|--all=*) all_paths=true ;;
      -*u*) tracked_only=true; all_paths=true ;;
      -*) ;;
      *) pathspecs+=("$argument") ;;
    esac
  done
  if [ "$pathspec_from_file" = true ]; then
    all_paths=true
    pathspecs=()
  fi
  if [ ${#pathspecs[@]} -eq 0 ] && [ "$all_paths" = false ]; then
    return 1
  fi
  if [ "$tracked_only" = false ]; then
    list_args+=(-o --exclude-standard)
  fi
  if [ ${#pathspecs[@]} -gt 0 ]; then
    list_args+=(-- "${pathspecs[@]}")
  fi
  while IFS= read -r -d '' value; do
    case "$value" in
      *.el|emacs/config.org) return 0 ;;
    esac
  done < <(git -C "$repo" ls-files -z "${list_args[@]}" 2>/dev/null || true)
  # Include paths that are already changed in the index. A repeated explicit
  # add still forms one compound stage-and-commit request and must be split.
  local -a diff_args=(--cached --name-only -z)
  if [ ${#pathspecs[@]} -gt 0 ]; then
    diff_args+=(-- "${pathspecs[@]}")
  fi
  while IFS= read -r -d '' value; do
    case "$value" in
      *.el|emacs/config.org) return 0 ;;
    esac
  done < <(git -C "$repo" diff "${diff_args[@]}" 2>/dev/null || true)
  return 1
}

# Classify executable-looking shell syntax while ignoring inert quoted text.
# Emacsclient classification requires the executable at a command boundary, so
# quoted commit messages can never clear verification.
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

# Return success only for a parsed commit invocation that cannot create a
# commit. Consume option values and honor -- so message text and pathspecs
# named --dry-run never suppress a real post-commit obligation.
codex_git_commit_inspection_p() {
  printf '%s' "$1" | python3 -c '
import json
import sys

args = json.load(sys.stdin).get("args", [])
values = {
    "--message", "--file", "--reuse-message", "--reedit-message",
    "--author", "--date", "--cleanup", "--trailer", "--fixup", "--squash",
    "--template", "--pathspec-from-file", "--unified", "--inter-hunk-context",
}
flags = {
    "--all", "--include", "--only", "--amend", "--no-edit", "--edit",
    "--quiet", "--verbose", "--signoff", "--no-verify", "--verify",
    "--allow-empty", "--allow-empty-message", "--reset-author",
    "--no-post-rewrite", "--post-rewrite", "--gpg-sign", "--no-gpg-sign",
    "--status", "--no-status", "--branch", "--no-branch",
    "--ahead-behind", "--no-ahead-behind", "--interactive", "--patch",
    "--pathspec-file-nul", "--untracked-files", "--no-untracked-files",
}
dry_run = False
status_output = False
position = 0
while position < len(args):
    arg = args[position]
    position += 1
    if arg == "--":
        break
    if arg in {"--help", "-h"}:
        sys.exit(0)
    if arg == "--dry-run":
        dry_run = True
    elif arg == "--no-dry-run":
        dry_run = False
    elif arg in {"--short", "--long", "--porcelain"}:
        status_output = True
    elif arg in values:
        position += 1
    elif arg.partition("=")[0] in values | {"--gpg-sign", "--untracked-files"}:
        pass
    elif arg in flags:
        pass
    elif arg.startswith("--"):
        # Unknown options, including abbreviations and negated status modes,
        # do not establish that this invocation was only an inspection.
        sys.exit(1)
    elif arg.startswith("-"):
        for offset, flag in enumerate(arg[1:], 1):
            if flag in "mFCctU":
                if offset == len(arg) - 1:
                    position += 1
                break
            if flag in "Su":
                break  # Optional short-option values must be attached.
            if flag == "h":
                sys.exit(0)
            if flag not in "qvsenaiop":
                sys.exit(1)
sys.exit(0 if dry_run or status_output else 1)
'
}

codex_git_commit_count() {
  codex_git_subcommand_count commit "${1:-${CODEX_GIT_PARSE_CONTEXT:-}}"
}

codex_emacsclient_eval_count() {
  _codex_shell_syntax_count emacsclient
}

# Count a named executable in command position while ignoring quoted arguments,
# comments, and later words in the same shell segment. This is used for helper
# allowlists where a mere textual mention must not bypass a gate.
codex_executable_count() {
  local executable="$1"
  python3 -c '
import os
import shlex
import sys

target = sys.argv[1]
source = sys.stdin.read()
lexer = shlex.shlex(source, posix=True, punctuation_chars=";&|(){}")
lexer.whitespace_split = True
lexer.commenters = "#"
tokens = list(lexer)
boundaries = {";", ";;", "&", "&&", "|", "||", "(", ")", "{", "}"}
wrappers = {"command", "exec"}
count = 0
at_start = True
skip_wrapper = False
for token in tokens:
    if token in boundaries or all(char in ";&|(){}" for char in token):
        at_start = True
        skip_wrapper = False
        continue
    if not at_start:
        continue
    if "=" in token and not token.startswith(("/", "./", "../")):
        name, _, _ = token.partition("=")
        if name.replace("_", "a").isalnum():
            continue
    if token in wrappers and not skip_wrapper:
        skip_wrapper = True
        continue
    at_start = False
    if os.path.basename(token) == target:
        count += 1
print(count)
' "$executable"
}

# Emit the evidence label for each supported Elisp wrapper that appears in
# executable position. This binds output to the label in the observed command;
# it is not an authentication boundary against another same-user process.
codex_elisp_evidence_labels() {
  local kind="$1"
  python3 -c '
import os
import shlex
import sys

kind = sys.argv[1]
source = sys.stdin.read()
lexer = shlex.shlex(source, posix=True, punctuation_chars=";&|(){}")
lexer.whitespace_split = True
lexer.commenters = "#"
try:
    tokens = list(lexer)
except ValueError:
    raise SystemExit

boundaries = {";", ";;", "&", "&&", "|", "||", "(", ")", "{", "}"}
wrappers = {"command", "exec"}

def is_assignment(token):
    if "=" not in token or token.startswith(("/", "./", "../")):
        return False
    name = token.partition("=")[0]
    return bool(name) and name.replace("_", "a").isalnum()

segments = []
segment = []
for token in tokens:
    if token in boundaries or all(char in ";&|(){}" for char in token):
        if segment:
            segments.append(segment)
            segment = []
    else:
        segment.append(token)
if segment:
    segments.append(segment)

for words in segments:
    pos = 0
    while pos < len(words) and is_assignment(words[pos]):
        pos += 1
    if pos < len(words) and words[pos] in wrappers:
        pos += 1
        if pos < len(words) and words[pos] == "--":
            pos += 1
    if pos >= len(words):
        continue
    executable = os.path.basename(words[pos])
    arguments = words[pos + 1:]
    label = None
    if kind == "test" and executable == "batch-test.sh" and arguments:
        label = arguments[0]
    elif kind == "test" and executable == "elisp-check-evidence":
        if arguments and arguments[0] == "--staged":
            arguments = arguments[1:]
        if len(arguments) >= 2 and arguments[1] == "--":
            label = arguments[0]
    elif kind == "live" and executable == "elisp-live-verify":
        if len(arguments) == 3 and arguments[1] == "--":
            label = arguments[0]
    if label and not any(char in label for char in "\n\r$`"):
        sys.stdout.buffer.write(label.encode() + b"\0")
' "$kind"
}

codex_shell_executables() {
  python3 -c '
import os
import shlex
import sys

source = sys.stdin.read()
lexer = shlex.shlex(source, posix=True, punctuation_chars=";&|(){}")
lexer.whitespace_split = True
lexer.commenters = "#"
tokens = list(lexer)
boundaries = {";", ";;", "&", "&&", "|", "||", "(", ")", "{", "}"}
wrappers = {"command", "exec"}
at_start = True
skip_wrapper = False
for token in tokens:
    if token in boundaries or all(char in ";&|(){}" for char in token):
        at_start = True
        skip_wrapper = False
        continue
    if not at_start:
        continue
    if "=" in token and not token.startswith(("/", "./", "../")):
        name, _, _ = token.partition("=")
        if name.replace("_", "a").isalnum():
            continue
    if token in wrappers and not skip_wrapper:
        skip_wrapper = True
        continue
    at_start = False
    sys.stdout.buffer.write(os.path.basename(token).encode() + b"\0")
'
}
