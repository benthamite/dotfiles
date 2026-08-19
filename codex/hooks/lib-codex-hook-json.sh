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
    if obj >= n or source[obj] != "{":
        if mode == "contexts":
            sys.stdout.buffer.write(
                json.dumps(
                    {"cmd": None, "workdir": None, "ambiguous": True},
                    separators=(",", ":"),
                ).encode("utf-8") + b"\0"
            )
        pos = call + 1
        continue
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


def tokenize(program):
    try:
        lexer = shlex.shlex(program, posix=True, punctuation_chars=";&|()\n")
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


def ambiguous_commit(reason, assignments=None, global_args=None, invoked=None):
    record = {
        "subcommand": "commit",
        "args": [],
        "global_args": global_args or [],
        "assignments": assignments or [],
        "ambiguous": True,
        "ambiguity": reason,
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


def configured_alias(name, global_args):
    try:
        result = subprocess.run(
            ["git"] + global_args + ["config", "--get", "alias." + name],
            cwd=context_dir,
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


def parse_git(words, assignments=None, inherited_aliases=None, alias_depth=0):
    assignments = assignments or []
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
            }
            return record

        aliases = dict(inherited_aliases or {})
        aliases.update(config_aliases(global_args))
        if token not in aliases and token not in builtin_commands:
            alias_value = configured_alias(token, global_args)
            if alias_value is not None:
                aliases[token] = alias_value
        if token in aliases:
            if alias_depth >= 8:
                return ambiguous_commit(
                    "git-alias-depth", assignments, global_args, token
                )
            alias_value = aliases[token]
            if alias_value.startswith("!"):
                return ambiguous_commit(
                    "git-shell-alias", assignments, global_args, token
                )
            try:
                alias_words = shlex.split(alias_value, posix=True)
            except ValueError:
                return ambiguous_commit(
                    "invalid-git-alias", assignments, global_args, token
                )
            if not alias_words:
                return ambiguous_commit(
                    "empty-git-alias", assignments, global_args, token
                )
            record = parse_git(
                ["git"] + alias_words + words[pos + 1:],
                assignments,
                aliases,
                alias_depth + 1,
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
                "dynamic-git-subcommand", assignments, global_args, token
            )
        record = {
            "subcommand": token,
            "args": words[pos + 1:],
            "global_args": global_args,
            "assignments": assignments,
            "ambiguous": git_assignments_ambiguous(assignments),
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
            masked.append("SUBSTITUTION")
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
            masked.append("SUBSTITUTION")
            pos = end + 1
            continue
        masked.append(char)
        pos += 1
    return "".join(masked), commands, False


def scan(program, depth=0):
    if depth > 4:
        return [ambiguous_commit("shell-recursion-limit")]
    records = []
    masked_program, substitutions, invalid_substitution = extract_substitutions(program)
    if invalid_substitution:
        return [ambiguous_commit("invalid-command-substitution")]
    for nested_program in substitutions:
        nested_records = scan(nested_program, depth + 1)
        for record in nested_records:
            record["ambiguous"] = True
            record["ambiguity"] = "shell-command-substitution"
        records.extend(nested_records)
    shell_variables = {}
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

        while pos < len(words) and words[pos] in {"!", "if", "then", "elif", "while", "until", "do", "{"}:
            if words[pos] in {"!", "if", "elif", "while", "until"}:
                control_ambiguous = True
            pos += 1
        if pos < len(words) and words[pos] == "time":
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
                if token in {"-u", "--unset", "-C", "--chdir", "-S", "--split-string"}:
                    pos += 2
                    continue
                if any(token.startswith(prefix) for prefix in ("--unset=", "--chdir=", "--split-string=")):
                    pos += 1
                    continue
                if is_assignment(token):
                    assignments.append(token)
                    pos += 1
                    continue
                break
            if pos >= len(words):
                continue

        executable = os.path.basename(words[pos])
        command_words = words[pos:]
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
            record = parse_git(command_words, assignments)
            if record is not None:
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
                records.append(record)
            continue
        if executable == "eval":
            if len(command_words) < 2:
                continue
            nested = scan(" ".join(command_words[1:]), depth + 1)
            for record in nested:
                record["ambiguous"] = True
                record["ambiguity"] = "shell-eval"
            records.extend(nested)
            continue
        if executable in interpreters:
            command_index = interpreter_command_index(command_words)
            if command_index is not None and command_index < len(command_words):
                records.extend(scan(command_words[command_index], depth + 1))
            elif command_index is not None:
                records.append(ambiguous_commit("missing-interpreter-command"))
            continue
        if executable.startswith("$") or executable.startswith("`"):
            if "commit" in command_words[1:]:
                records.append(ambiguous_commit("dynamic-shell-executable"))
    return records


for record in scan(source):
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
  local value have_global_args=false
  local -a global_args=()
  while IFS= read -r -d '' value; do
    global_args+=("$value")
    have_global_args=true
  done < <(printf '%s' "$record" | jq -j '.global_args[] | ., "\u0000"')
  (
    cd -- "$context_dir" 2>/dev/null || exit 1
    if [ "$have_global_args" = true ]; then
      git "${global_args[@]}" rev-parse --show-toplevel 2>/dev/null
    else
      git rev-parse --show-toplevel 2>/dev/null
    fi
  )
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
