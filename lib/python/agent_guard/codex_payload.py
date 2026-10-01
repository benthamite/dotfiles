"""Codex hook payload helpers, ported from codex/hooks/lib-codex-hook-json.sh.

Each function returns what the shell helper printed for the same input, with
NUL-delimited records returned as lists. Two shell mechanisms are reproduced
because they changed the values callers saw:

- `jq -r` rendered non-string values as JSON and treated null and false as
  missing under `//` (`jq_raw`, `jq_alternative`);
- every caller captured output with `$(...)`, which drops NUL bytes and
  trailing newlines (`command_substitution`). Functions return the raw output;
  ports of callers apply `command_substitution` where the shell did.
"""

from __future__ import annotations

import ctypes
import functools
import importlib.util
import json
import os
import re
import shlex
import subprocess

from .core import CODEX_SHELL_TOOLS, DOTFILES, HookFailure, codex_tool_input


class PartialOutput(HookFailure):
    """A helper exited non-zero after printing `values`; some callers used them."""

    def __init__(self, message: str, values: list):
        super().__init__(message)
        self.values = values


def command_substitution(text: str) -> str:
    """The value of `$(...)` capturing `text`: NUL bytes and trailing LFs removed."""
    return text.replace("\0", "").rstrip("\n")


# --- jq ----------------------------------------------------------------------

_JQ_ESCAPES = {'"': '\\"', "\\": "\\\\", "\b": "\\b", "\f": "\\f", "\n": "\\n", "\r": "\\r", "\t": "\\t"}


def _jq_string(text: str) -> str:
    """A JSON string literal as jq prints one: control characters and DEL escaped."""
    out = []
    for char in text:
        if char in _JQ_ESCAPES:
            out.append(_JQ_ESCAPES[char])
        elif char < " " or char == "\x7f":
            out.append("\\u%04x" % ord(char))
        else:
            out.append(char)
    return '"' + "".join(out) + '"'


def _jq_number(value) -> str:
    if isinstance(value, int):
        return str(value)
    if value != value:
        return "null"
    if value in (float("inf"), float("-inf")):
        return ("-" if value < 0 else "") + "1.7976931348623157e+308"
    return repr(value)


def _jq_json(value, indent: str = "") -> str:
    """`value` pretty-printed as jq's default (two-space indented) output."""
    if value is None:
        return "null"
    if value is True:
        return "true"
    if value is False:
        return "false"
    if isinstance(value, str):
        return _jq_string(value)
    if isinstance(value, (int, float)):
        return _jq_number(value)
    inner = indent + "  "
    if isinstance(value, dict):
        if not value:
            return "{}"
        items = [inner + _jq_string(str(key)) + ": " + _jq_json(item, inner) for key, item in value.items()]
        return "{\n" + ",\n".join(items) + "\n" + indent + "}"
    if not value:
        return "[]"
    return "[\n" + ",\n".join(inner + _jq_json(item, inner) for item in value) + "\n" + indent + "]"


def jq_raw(value) -> str:
    """`value` as `jq -r` prints it, without the trailing newline."""
    return value if isinstance(value, str) else _jq_json(value)


def jq_alternative(*values):
    """jq's `a // b // ...`: the first value that is neither null nor false."""
    for value in values:
        if value is not None and value is not False:
            return value
    return None


def jq_text(value) -> str:
    """`jq -r 'EXPR // empty'` output for EXPR's value."""
    value = jq_alternative(value)
    return "" if value is None else jq_raw(value)


# --- payload fields ------------------------------------------------------------

def tool_input(payload: dict) -> dict:
    return codex_tool_input(payload.get("tool_input"))


def tool_response(payload: dict) -> dict:
    """`codex_tool_response`: Codex sends results as objects, text-chunk arrays or JSON text."""
    response = payload.get("tool_response")
    if isinstance(response, dict):
        return response
    if isinstance(response, list):
        output = ""
        for item in response:
            if isinstance(item, dict):
                item = jq_alternative(item.get("text"), item.get("output"))
                if item is None:
                    continue
            elif not isinstance(item, str):
                continue
            if isinstance(item, (dict, list)):
                raise HookFailure("jq join: cannot add a %s to a string" % type(item).__name__)
            output += item if isinstance(item, str) else _jq_json(item)
        return {"output": output}
    if isinstance(response, str):
        try:
            decoded = json.loads(response)
        except ValueError:
            return {"output": response}
        return decoded if isinstance(decoded, dict) else {"output": response}
    return {}


def tool_name(payload: dict) -> str:
    return jq_text(payload.get("tool_name"))


def session_id(payload: dict) -> str:
    return jq_text(payload.get("session_id"))


def tool_input_field(payload: dict, field: str) -> str:
    return jq_text(tool_input(payload).get(field))


def shell_command(payload: dict) -> str:
    """`codex_shell_command`: the first of command, cmd and input that jq considers set."""
    fields = tool_input(payload)
    return jq_text(jq_alternative(fields.get("command"), fields.get("cmd"), fields.get("input")))


def is_shell_tool(name: str) -> bool:
    return name in CODEX_SHELL_TOOLS


# --- functions.exec programs ------------------------------------------------------

_AMBIGUOUS_CONTEXT = {"cmd": None, "workdir": None, "ambiguous": True}
_CONTEXT_KEYS = ("workdir", "cwd", "working_directory", "working_dir")


def _literal_exec_wrapper(source: str) -> bool:
    """One complete `await tools.exec_command({...})` call with literal fields only.

    A deliberately smaller grammar than command extraction: the document-edit
    exception needs one orchestration with no executable metadata, dynamic
    expressions, extra calls or residual JavaScript. Nothing is evaluated.
    """
    opener = re.match(r"\s*(text\(\s*)?await\s+tools\.exec_command\(\s*\{", source)
    if opener is None:
        return False
    pos = opener.end()
    fields = {}
    decoder = json.JSONDecoder()
    try:
        while True:
            key = re.match(r"\s*(\"?)(cmd|workdir|max_output_tokens|yield_time_ms)\1\s*:\s*", source[pos:])
            if key is None or key[2] in fields:
                return False
            pos += key.end()
            value, pos = decoder.raw_decode(source, pos)
            expected = str if key[2] in {"cmd", "workdir"} else int
            if type(value) is not expected:
                return False
            fields[key[2]] = value
            separator = re.match(r"\s*([,}])", source[pos:])
            if separator is None:
                return False
            pos += separator.end()
            if separator[1] == "}":
                break
        tail = r"\s*\)\s*" + (r"\)\s*" if opener[1] else "") + r";?\s*"
        return "cmd" in fields and re.fullmatch(tail, source[pos:]) is not None
    except (ValueError, UnicodeError, RecursionError):
        return False


class _ExecProgram:
    """Lexical helpers over one functions.exec JavaScript program."""

    def __init__(self, source: str):
        self.source = source
        self.n = len(source)

    def skip_space(self, pos):
        source, n = self.source, self.n
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

    def read_string(self, pos):
        source, n = self.source, self.n
        quote = source[pos]
        pos += 1
        value = []
        escapes = {"n": "\n", "r": "\r", "t": "\t", "b": "\b", "f": "\f", "v": "\v"}
        while pos < n:
            char = source[pos]
            if char == quote:
                # JavaScript unicode escapes carry UTF-16 code units. Combine a
                # valid surrogate pair; a lone surrogate raises, as it did in
                # the shell helper, rather than losing the extracted command.
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

    def read_identifier(self, pos):
        source, n = self.source, self.n
        start = pos
        while pos < n and (source[pos].isalnum() or source[pos] in "_$"):
            pos += 1
        return source[start:pos], pos

    def matching_brace(self, pos):
        source, n = self.source, self.n
        depth = 0
        while pos < n:
            char = source[pos]
            if char in ("\"", "'", "`"):
                _, pos = self.read_string(pos)
                continue
            if source.startswith("//", pos) or source.startswith("/*", pos):
                pos = self.skip_space(pos)
                continue
            if char == "{":
                depth += 1
            elif char == "}":
                depth -= 1
                if depth == 0:
                    return pos
            pos += 1
        return None

    @staticmethod
    def mask_non_code(text):
        masked = list(text)
        pos = 0
        limit = len(text)
        while pos < limit:
            if text[pos] in ("\"", "'", "`"):
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

    def literal_object_binding(self, name, before):
        source = self.source
        masked = self.mask_non_code(source[:before])
        pattern = re.compile(r"\b(?:const|let|var)\s+" + re.escape(name) + r"\s*=\s*\{")
        for match in reversed(list(pattern.finditer(masked))):
            start = masked.find("{", match.start(), match.end())
            end = self.matching_brace(start)
            if end is None or end >= before:
                continue
            intervening = self.mask_non_code(source[end + 1:before])
            if re.search(r"\b" + re.escape(name) + r"\b", intervening):
                return None
            return start, end
        return None

    def skip_expression(self, pos, end):
        """Advance past one property value or spread up to its top-level comma."""
        source = self.source
        nested = 0
        while pos < end:
            if source[pos] in ("\"", "'", "`"):
                _, pos = self.read_string(pos)
                continue
            if source[pos] in "([{":
                nested += 1
            elif source[pos] in ")]}" and nested:
                nested -= 1
            elif source[pos] == "," and nested == 0:
                break
            pos += 1
        return pos

    def parse_object(self, start, end):
        source = self.source
        fields = {}
        seen = set()
        ambiguous = set()
        relevant = {"cmd", *_CONTEXT_KEYS}
        pos = start + 1
        while pos < end:
            pos = self.skip_space(pos)
            if pos >= end:
                break
            if source[pos] == ",":
                pos += 1
                continue
            if source.startswith("...", pos) or source[pos] == "[":
                # A spread or computed property can override any direct field.
                ambiguous.update(relevant)
                pos = self.skip_expression(pos, end)
                continue
            if source[pos] in ("\"", "'"):
                key, pos = self.read_string(pos)
            elif source[pos].isalpha() or source[pos] in "_$":
                key, pos = self.read_identifier(pos)
            else:
                pos += 1
                continue
            pos = self.skip_space(pos)
            if pos >= end or source[pos] != ":":
                continue
            if key in relevant:
                if key in seen:
                    ambiguous.add(key)
                seen.add(key)
            pos = self.skip_space(pos + 1)
            if pos < end and source[pos] in ("\"", "'", "`"):
                quote = source[pos]
                value, pos = self.read_string(pos)
                if value is not None and key in relevant:
                    fields[key] = value
                    if quote == "`" and "${" in value:
                        ambiguous.add(key)
                    # A literal is safe only when it is the complete value
                    # expression. Concatenation, property access, calls, and
                    # other trailing syntax can change the value that reaches
                    # the tool.
                    value_end = self.skip_space(pos)
                    if value_end < end and source[value_end] not in ",}":
                        ambiguous.add(key)
            else:
                if key in relevant:
                    ambiguous.add(key)
                pos = self.skip_expression(pos, end)
        context_keys = seen.intersection(_CONTEXT_KEYS)
        if len(context_keys) > 1:
            ambiguous.update(context_keys)
        return fields, ambiguous

    def is_word_boundary(self, pos, extra=""):
        return pos < 0 or pos >= self.n or not (self.source[pos].isalnum() or self.source[pos] in "_$" + extra)

    def calls(self, mode, out):
        """Append each literal `tools.exec_command` call to `out` in `mode`.

        Raises ValueError or UnicodeDecodeError where the shell helper did,
        after appending the records it had already printed.
        """
        source, n = self.source, self.n
        contexts = mode == "contexts"
        pos = 0
        needle = "tools.exec_command"
        while pos < n:
            if source[pos] in ("\"", "'", "`"):
                _, pos = self.read_string(pos)
                continue
            if source.startswith("//", pos) or source.startswith("/*", pos):
                pos = self.skip_space(pos)
                continue
            if source.startswith("tools", pos):
                after_tools = pos + len("tools")
                if (self.is_word_boundary(pos - 1) and self.is_word_boundary(after_tools)
                        and not source.startswith("tools.", pos)):
                    if contexts:
                        out.append(dict(_AMBIGUOUS_CONTEXT))
                    pos = after_tools
                    continue
            # Computed tool access cannot be tied to a literal tool name without
            # evaluating JavaScript. Reject it instead of trying to recognize
            # selected spellings such as tools["exec_command"].
            if source.startswith("tools[", pos):
                if contexts:
                    out.append(dict(_AMBIGUOUS_CONTEXT))
                pos += len("tools[")
                continue
            # Destructuring and other indirect references can alias
            # exec_command without spelling tools.exec_command at the call site.
            if source.startswith("exec_command", pos):
                after = pos + len("exec_command")
                if self.is_word_boundary(pos - 1) and self.is_word_boundary(after):
                    if contexts:
                        out.append(dict(_AMBIGUOUS_CONTEXT))
                    pos = after
                    continue
            if not source.startswith(needle, pos):
                pos += 1
                continue
            after = pos + len(needle)
            if not self.is_word_boundary(pos - 1, ".") or not self.is_word_boundary(after):
                pos += 1
                continue
            call = self.skip_space(after)
            if call >= n or source[call] != "(":
                if contexts:
                    out.append(dict(_AMBIGUOUS_CONTEXT))
                pos = after
                continue
            obj = self.skip_space(call + 1)
            call_end = None
            if obj < n and (source[obj].isalpha() or source[obj] in "_$"):
                name, argument_end = self.read_identifier(obj)
                closing = self.skip_space(argument_end)
                binding = self.literal_object_binding(name, obj) if closing < n and source[closing] == ")" else None
                if binding is None:
                    if contexts:
                        out.append(dict(_AMBIGUOUS_CONTEXT))
                    pos = argument_end
                    continue
                obj, end = binding
                call_end = closing
            elif obj >= n or source[obj] != "{":
                if contexts:
                    out.append(dict(_AMBIGUOUS_CONTEXT))
                pos = call + 1
                continue
            else:
                end = self.matching_brace(obj)
            if end is None:
                if contexts:
                    out.append(dict(_AMBIGUOUS_CONTEXT))
                break
            fields, ambiguous = self.parse_object(obj, end)
            if any("\0" in value for value in fields.values()):
                raise ValueError("NUL is unsupported in nested command or workdir")
            command = fields.get("cmd")
            if not contexts:
                if command is not None:
                    out.append(command)
            else:
                workdir = next((fields[key] for key in _CONTEXT_KEYS if key in fields), None)
                out.append({"cmd": command, "workdir": workdir, "ambiguous": command is None or bool(ambiguous)})
            pos = call_end + 1 if call_end is not None else end + 1


def _nested_exec_values(source: str, mode: str):
    """Literal nested exec_command calls in a functions.exec program, unevaluated.

    Generated calls use an object literal with a literal cmd and, when needed,
    a literal workdir. `mode` "commands" returns the cmd strings, "contexts"
    one {"cmd", "workdir", "ambiguous"} dict per call or unresolvable
    reference, and "literal-wrapper" whether the whole program is one literal
    call. Extraction failures raise PartialOutput with the values found so far.
    """
    if mode == "literal-wrapper":
        return _literal_exec_wrapper(source)
    values = []
    try:
        _ExecProgram(source).calls(mode, values)
    except (ValueError, UnicodeDecodeError) as error:
        raise PartialOutput("nested exec extraction failed: %s" % error, values) from error
    return values


def nested_exec_commands(source: str) -> list[str]:
    return _nested_exec_values(source, "commands")


def nested_exec_contexts(source: str) -> list[dict]:
    return _nested_exec_values(source, "contexts")


def literal_exec_wrapper(source: str) -> bool:
    return _nested_exec_values(source, "literal-wrapper")


def _tail(path: str, count: int) -> bytes:
    """The bytes `tail -n COUNT path` prints, reading only the end of the file."""
    with open(path, "rb") as stream:
        position = stream.seek(0, os.SEEK_END)
        data = b""
        while position > 0:
            step = min(1 << 16, position)
            position -= step
            stream.seek(position)
            data = stream.read(step) + data
            if data.count(b"\n", 0, len(data) - 1) >= count:
                break
    body = data[:-1] if data.endswith(b"\n") else data
    start = len(body)
    for _ in range(count):
        start = body.rfind(b"\n", 0, start)
        if start < 0:
            return data
    return data[start + 1:]


def _transcript_exec_programs(path: str):
    """functions.exec programs among the transcript's last 64 lines, newest first.

    Mirrors the shell's reader: lines that are not JSON are skipped, and the
    first event shape it could not handle ended the stream silently.
    """
    try:
        text = _tail(path, 64).decode("utf-8")
    except (OSError, UnicodeDecodeError):
        return
    # The reader captured tail's output with subprocess text=True, which
    # translates CR and CRLF to LF.
    for line in reversed(text.replace("\r\n", "\n").replace("\r", "\n").splitlines()):
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            continue
        except RecursionError:
            return
        if not isinstance(event, dict):
            return
        payload = event.get("payload", {})
        if not isinstance(payload, dict):
            return
        if (payload.get("type") == "custom_tool_call"
                and payload.get("name") == "exec"
                and isinstance(payload.get("input"), str)):
            program = payload["input"]
            try:
                program.encode("utf-8")
            except UnicodeEncodeError:
                return
            # The shell framed programs with NUL, so an embedded NUL split one.
            yield from program.split("\0")


def parent_exec_workdir(payload: dict, command: str) -> str | None:
    """The literal workdir of the functions.exec call that ran `command`.

    Searches the newest exec program in the transcript that contains a nested
    call whose cmd is `command`; returns None unless every such call names the
    same existing directory unambiguously.
    """
    transcript = command_substitution(jq_text(payload.get("transcript_path")))
    if not os.path.isfile(transcript):
        return None
    for program in _transcript_exec_programs(transcript):
        found = False
        found_workdir = ""
        invalid = False
        try:
            contexts = nested_exec_contexts(program)
        except PartialOutput as failure:
            contexts = failure.values
        for context in contexts:
            cmd = context["cmd"]
            if command_substitution("" if cmd is None else cmd) != command:
                continue
            found = True
            if context["ambiguous"]:
                invalid = True
                continue
            workdir = context["workdir"]
            workdir = "" if workdir is None else command_substitution(workdir)
            if not workdir or not os.path.isdir(workdir):
                invalid = True
            elif not found_workdir:
                found_workdir = workdir
            elif found_workdir != workdir:
                invalid = True
        if found:
            return found_workdir if not invalid and found_workdir else None
    return None


# --- Git invocations -------------------------------------------------------------

_BOUNDARIES = {";", ";;", "&", "&&", "|", "||", "(", ")", "\n"}
_INTERPRETERS = {"bash", "sh", "zsh", "dash", "ksh"}
_GLOBAL_VALUE_OPTIONS = {
    "-C", "-c", "--git-dir", "--work-tree", "--namespace",
    "--super-prefix", "--config-env", "--exec-path",
}
_GLOBAL_FLAG_OPTIONS = {
    "--bare", "--no-pager", "--paginate", "-p", "--literal-pathspecs",
    "--glob-pathspecs", "--noglob-pathspecs", "--icase-pathspecs",
    "--no-optional-locks", "--no-replace-objects",
}
_TERMINAL_GLOBAL_OPTIONS = {"--version", "--help", "-h"}


@functools.lru_cache(maxsize=None)
def _python_heredoc():
    path = DOTFILES / "codex/hooks/lib-python-heredoc.py"
    spec = importlib.util.spec_from_file_location("python_heredoc", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@functools.lru_cache(maxsize=None)
def _builtin_commands() -> frozenset:
    try:
        result = subprocess.run(["git", "--list-cmds=builtins"], check=True, capture_output=True, text=True,
                                timeout=2)
        return frozenset(result.stdout.split())
    except (OSError, subprocess.SubprocessError):
        # With no command inventory, unknown Git subcommands must remain
        # ambiguous. Literal core commands are added so routine hooks still
        # classify the operations they are designed to inspect.
        return frozenset({"add", "commit", "config", "diff", "diff-tree", "ls-files", "rev-parse", "status"})


def _without_literal_heredocs(program):
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
            if char == "\\" and quote != "'":
                pos += 2
                continue
            if char == quote:
                quote = None
        elif char == "\\":
            pos += 2
            continue
        elif char in ("'", '"', "`"):
            quote = char
        elif char == "#":
            end = program.find("\n", pos)
            pos = len(program) if end < 0 else end
            continue
        elif program.startswith("<<", pos) and not program.startswith("<<<", pos):
            start = program.rfind("\n", 0, pos) + 1
            try:
                prefix_lexer = shlex.shlex(program[start:pos], posix=True, punctuation_chars=";&|()")
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
            if end < len(program) and program[end] in ("'", '"'):
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


def _tokenize(program):
    try:
        lexer = shlex.shlex(_without_literal_heredocs(program), posix=True, punctuation_chars=";&|()\n")
        lexer.whitespace = " \t\r"
        lexer.whitespace_split = True
        lexer.commenters = "#"
        return list(lexer)
    except ValueError:
        return []


def _is_assignment(token):
    if "=" not in token or token.startswith(("/", "./", "../")):
        return False
    name = token.partition("=")[0]
    return bool(name) and (name[0].isalpha() or name[0] == "_") and all(
        char.isalnum() or char == "_" for char in name
    )


def _split_segments(tokens):
    segment = []
    for token in tokens:
        if token in _BOUNDARIES or (token and all(char in ";&|()\n" for char in token)):
            if segment:
                yield segment, token
                segment = []
        else:
            segment.append(token)
    if segment:
        yield segment, None


def _git_assignments_ambiguous(assignments):
    return any(token.partition("=")[0].startswith("GIT_") for token in assignments)


def _config_aliases(global_args):
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


def _consume_command_wrapper(words, pos):
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


def _interpreter_command_index(command_words):
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


def _extract_substitutions(program):
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
        if char == "'" and not double_quoted:
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
                if current == "'" and not inner_double:
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


def _static_substitution_output(program):
    segments = list(_split_segments(_tokenize(program)))
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


def _resolved_context(base_dir, value):
    if not value or any(char in value for char in "$`"):
        return None
    value = os.path.expanduser(value)
    if not os.path.isabs(value):
        value = os.path.join(base_dir, value)
    return os.path.abspath(value)


def _mark_records_ambiguous(records, reason):
    for record in records:
        if record.get("subcommand") == "commit":
            record["ambiguous"] = True
            record["ambiguity"] = reason
    return records


class _GitScan:
    """Git invocation parser bound to a context directory and environment."""

    def __init__(self, context_dir: str, env: dict):
        self.context_dir = context_dir
        self.env = env
        self.builtin_commands = _builtin_commands()

    def ambiguous_commit(self, reason, assignments=None, global_args=None, invoked=None, invocation_dir=None):
        record = {
            "subcommand": "commit",
            "args": [],
            "global_args": global_args or [],
            "assignments": assignments or [],
            "ambiguous": True,
            "ambiguity": reason,
            "context_dir": invocation_dir or self.context_dir,
        }
        if invoked is not None:
            record["invoked_subcommand"] = invoked
        return record

    def configured_alias(self, name, global_args, assignments, invocation_dir):
        effective_env = dict(self.env)
        for assignment in assignments:
            key, _, value = assignment.partition("=")
            effective_env[key] = value
        try:
            result = subprocess.run(
                ["git"] + global_args + ["config", "--get", "alias." + name],
                cwd=invocation_dir, env=effective_env, check=False, capture_output=True, text=True, timeout=2,
            )
        except (OSError, subprocess.SubprocessError):
            return None
        if result.returncode != 0:
            return None
        return result.stdout.rstrip("\n")

    def parse_git(self, words, assignments=None, inherited_aliases=None, alias_depth=0, invocation_dir=None):
        assignments = assignments or []
        invocation_dir = invocation_dir or self.context_dir
        global_args = []
        ambiguous = False
        pos = 1
        while pos < len(words):
            token = words[pos]
            if token in _TERMINAL_GLOBAL_OPTIONS:
                return None
            if token == "--":
                global_args.append(token)
                pos += 1
                break
            if token in _GLOBAL_VALUE_OPTIONS:
                global_args.append(token)
                pos += 1
                if pos >= len(words):
                    return None
                global_args.append(words[pos])
                pos += 1
                continue
            if any(token.startswith(option + "=") for option in _GLOBAL_VALUE_OPTIONS if option.startswith("--")):
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
            if token in _GLOBAL_FLAG_OPTIONS:
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
                return {
                    "subcommand": "commit",
                    "args": words[commit_pos + 1:],
                    "global_args": words[1:commit_pos],
                    "assignments": assignments,
                    "ambiguous": True,
                    "ambiguity": "unknown-git-global-option",
                    "context_dir": invocation_dir,
                }

            aliases = dict(inherited_aliases or {})
            aliases.update(_config_aliases(global_args))
            if token not in aliases and token not in self.builtin_commands:
                alias_value = self.configured_alias(token, global_args, assignments, invocation_dir)
                if alias_value is not None:
                    aliases[token] = alias_value
            if token in aliases:
                if alias_depth >= 8:
                    return self.ambiguous_commit("git-alias-depth", assignments, global_args, token, invocation_dir)
                alias_value = aliases[token]
                if alias_value.startswith("!"):
                    return self.ambiguous_commit("git-shell-alias", assignments, global_args, token, invocation_dir)
                try:
                    alias_words = shlex.split(alias_value, posix=True)
                except ValueError:
                    return self.ambiguous_commit("invalid-git-alias", assignments, global_args, token, invocation_dir)
                if not alias_words:
                    return self.ambiguous_commit("empty-git-alias", assignments, global_args, token, invocation_dir)
                record = self.parse_git(["git"] + alias_words + words[pos + 1:], assignments, aliases,
                                        alias_depth + 1, invocation_dir)
                if record is None:
                    return None
                record["global_args"] = global_args
                record["invoked_subcommand"] = token
                if _git_assignments_ambiguous(assignments):
                    record["ambiguous"] = True
                    record["ambiguity"] = "git-environment"
                return record

            if any(char in token for char in "$`{}*?["):
                return self.ambiguous_commit("dynamic-git-subcommand", assignments, global_args, token,
                                             invocation_dir)
            record = {
                "subcommand": token,
                "args": words[pos + 1:],
                "global_args": global_args,
                "assignments": assignments,
                "ambiguous": _git_assignments_ambiguous(assignments),
                "context_dir": invocation_dir,
            }
            if record["ambiguous"]:
                record["ambiguity"] = "git-environment"
            return record
        return None

    def scan(self, program, depth=0, initial_dir=None):
        initial_dir = initial_dir or self.context_dir
        if depth > 4:
            return [self.ambiguous_commit("shell-recursion-limit", invocation_dir=initial_dir)]
        program = program.replace("\\\n", "")
        records = []
        program = _python_heredoc().project_git_shell(program)
        program = _without_literal_heredocs(program)
        masked_program, substitutions, invalid_substitution = _extract_substitutions(program)
        if invalid_substitution:
            return [self.ambiguous_commit("invalid-command-substitution", invocation_dir=initial_dir)]
        for substitution_index, nested_program in enumerate(substitutions):
            placeholder = "__CODEX_SUB_" + str(substitution_index) + "__"
            static_output = _static_substitution_output(nested_program)
            if static_output is not None:
                masked_program = masked_program.replace(placeholder, static_output)
                continue
            masked_program = masked_program.replace(placeholder, "$CODEX_DYNAMIC_SUB_" + str(substitution_index))
            nested_records = self.scan(nested_program, depth + 1, initial_dir)
            records.extend(_mark_records_ambiguous(nested_records, "shell-command-substitution"))
        shell_variables = {}
        current_dir = initial_dir
        context_unknown = False
        segments = list(_split_segments(_tokenize(masked_program)))
        for segment_index, (words, terminator) in enumerate(segments):
            pos = 0
            assignments = []
            control_ambiguous = False
            while pos < len(words) and _is_assignment(words[pos]):
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
                nested = self.scan(" ".join(words[body_start:]), depth + 1, current_dir)
                records.extend(_mark_records_ambiguous(nested, "shell-function"))
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
                pos, terminal = _consume_command_wrapper(words, pos)
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
                        target_dir = _resolved_context(invocation_dir, words[pos + 1])
                        if target_dir is None:
                            invocation_context_unknown = True
                        else:
                            invocation_dir = target_dir
                        pos += 2
                        continue
                    if token.startswith("--chdir="):
                        target_dir = _resolved_context(invocation_dir, token.partition("=")[2])
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
                    if _is_assignment(token):
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
                    if token.startswith("--adjustment=") or (token.startswith("-") and token[1:].lstrip("+").isdigit()):
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
                target_dir = (_resolved_context(current_dir, command_words[cd_pos])
                              if cd_pos < len(command_words) else None)
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
                        command_words = [command_words[0], shell_variables[variable_name]] + command_words[2:]
                    elif subcommand.startswith("{") and subcommand.endswith("}") and "," in subcommand:
                        command_words = [command_words[0]] + subcommand[1:-1].split(",") + command_words[2:]
                record = self.parse_git(command_words, assignments, invocation_dir=invocation_dir)
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
                    if record["subcommand"] == "commit" and status_decoupled and not record["ambiguous"]:
                        record["ambiguous"] = True
                        record["ambiguity"] = "shell-status-decoupled"
                        record["status_operator"] = terminator
                    records.append(record)
                continue
            if executable == "eval":
                if len(command_words) < 2:
                    continue
                nested = self.scan(" ".join(command_words[1:]), depth + 1, invocation_dir)
                records.extend(_mark_records_ambiguous(nested, "shell-eval"))
                continue
            if executable in _INTERPRETERS:
                command_index = _interpreter_command_index(command_words)
                if command_index is not None and command_index < len(command_words):
                    records.extend(self.scan(command_words[command_index], depth + 1, invocation_dir))
                elif command_index is not None:
                    records.append(self.ambiguous_commit("missing-interpreter-command", invocation_dir=invocation_dir))
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
                    nested = self.scan(" ".join(command_words[inner_pos:]), depth + 1, invocation_dir)
                    records.extend(_mark_records_ambiguous(nested, "shell-xargs"))
                continue
            if executable == "find" and "-exec" in command_words:
                inner_pos = command_words.index("-exec") + 1
                if inner_pos < len(command_words):
                    nested = self.scan(" ".join(command_words[inner_pos:]), depth + 1, invocation_dir)
                    records.extend(_mark_records_ambiguous(nested, "shell-find-exec"))
                continue
            if executable.startswith("$") or executable.startswith("`"):
                if "commit" in command_words[1:]:
                    records.append(self.ambiguous_commit("dynamic-shell-executable", invocation_dir=invocation_dir))
        return records


def git_invocations(source: str, context: str = "", env: dict | None = None) -> list[dict]:
    """The Git invocations in a shell program, without evaluating it.

    Each record holds the subcommand, its arguments, the Git global options
    before it and the assignments that prefix it, so callers can recognize
    forms such as `git -C repo commit`. Raises HookFailure where the shell
    helper exited non-zero, which it did before printing any record.
    """
    env = os.environ if env is None else env
    context = context or env.get("CODEX_GIT_PARSE_CONTEXT", "")
    scanner = _GitScan(context if os.path.isdir(context) else os.getcwd(), env)
    try:
        records = scanner.scan(source)
    except Exception as error:
        raise HookFailure("Git invocation parser failed: %r" % error) from error
    for sequence, record in enumerate(records):
        record["sequence"] = sequence
    return records


def git_subcommand_count(source: str, target: str, context: str = "", env: dict | None = None) -> int:
    try:
        records = git_invocations(source, context, env)
    except HookFailure:
        return 0  # The shell read the parser through a process substitution and ignored its status.
    return sum(command_substitution(jq_raw(record["subcommand"])) == target for record in records)


def git_commit_count(source: str, context: str = "", env: dict | None = None) -> int:
    return git_subcommand_count(source, "commit", context, env)


def bash_pwd(env: dict) -> str:
    """The directory bash starts in: $PWD when it names the cwd, else getcwd()."""
    pwd = env.get("PWD", "")
    try:
        if pwd.startswith("/") and os.path.samefile(pwd, "."):
            return pwd
    except OSError:
        pass
    return os.getcwd()


def _canonical_path(path: str) -> str | None:
    """bash's sh_canonpath with PATH_CHECKDOTDOT|PATH_CHECKEXISTS, or None."""
    parts = []
    for part in path.split("/"):
        if part in ("", "."):
            continue
        if part == "..":
            if not os.path.isdir("/" + "/".join(parts)):
                return None
            parts = parts[:-1]
        else:
            parts.append(part)
    prefix = "//" if path.startswith("//") and not path.startswith("///") else "/"
    canonical = prefix + "/".join(parts)
    return canonical if os.path.exists(canonical) else None


def _bash_cd(target: str, env: dict) -> tuple[str, str] | None:
    """Where `cd -- TARGET` in a bash subshell lands, and what it prints.

    Logical mode: the canonicalized path is tried first, then TARGET verbatim;
    `-` and non-empty CDPATH entries print the new directory.
    """
    if not target:
        return None
    printed = False
    if target == "-":
        target = env.get("OLDPWD", "")
        if not target:
            return None
        printed = True
    base = bash_pwd(env)

    def change(directory):
        absolute = directory if directory.startswith("/") else base.rstrip("/") + "/" + directory
        canonical = _canonical_path(absolute)
        logical = canonical or absolute
        if os.path.isdir(logical) and os.access(logical, os.X_OK):
            return logical
        if os.path.isdir(directory) and os.access(directory, os.X_OK):
            return os.path.realpath(directory)
        return None

    explicit = target.startswith("/") or target in (".", "..") or target.startswith(("./", "../"))
    cdpath = env.get("CDPATH", "")
    if cdpath and not explicit and not printed:
        for entry in cdpath.split(":"):
            landed = change((entry or ".").rstrip("/") + "/" + target)
            if landed is not None:
                return landed, (landed + "\n" if entry else "")
    landed = change(target)
    if landed is None:
        return None
    return landed, (landed + "\n" if printed else "")


def git_invocation_repo(record: dict, context_dir: str, env: dict | None = None) -> str:
    """stdout of `git rev-parse --show-toplevel` run as the record's invocation would run.

    Applies the record's global options and assignments from `context_dir`.
    Empty when that directory cannot be entered; the exit status is not
    returned because the only caller discarded it.
    """
    env = dict(os.environ if env is None else env)
    landed = _bash_cd(context_dir, env)
    if landed is None:
        return ""
    cwd, printed = landed
    env["PWD"] = cwd
    for assignment in record["assignments"]:
        key, _, value = assignment.partition("=")
        env[key] = value
    result = subprocess.run(
        ["git", *record["global_args"], "rev-parse", "--show-toplevel"],
        cwd=cwd, env=env, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, check=False,
    )
    return printed + result.stdout.decode("utf-8", "surrogateescape")


# Characters bash's printf %q backslash-escapes (lib/sh/shquote.c bstab).
_BASH_BACKSLASHED = set(" \t\n!\"$&'()*,;<>?[\\]^`{|}")
_ANSIC_ESCAPES = {"\x1b": "E", "\a": "a", "\v": "v", "\b": "b", "\f": "f", "\n": "n", "\r": "r", "\t": "t",
                  "\\": "\\", "'": "'"}


@functools.lru_cache(maxsize=None)
def _iswprint():
    function = ctypes.CDLL(None).iswprint
    function.argtypes = [ctypes.c_int]
    return function


def _bash_printable(char: str) -> bool:
    """bash's test: isprint for ASCII, libc iswprint (current locale) beyond it."""
    return " " <= char < "\x7f" if char < "\x80" else bool(_iswprint()(ord(char)))


def _bash_quote(value: str) -> str:
    """bash 5 `printf %q` in a UTF-8 locale."""
    if not value:
        return "''"
    if not all(_bash_printable(char) for char in value):
        out = []
        for char in value:
            if char in _ANSIC_ESCAPES:
                out.append("\\" + _ANSIC_ESCAPES[char])
            elif _bash_printable(char):
                out.append(char)
            else:
                out.extend("\\%03o" % byte for byte in char.encode("utf-8", "surrogateescape"))
        return "$'" + "".join(out) + "'"
    out = []
    for index, char in enumerate(value):
        if (char in _BASH_BACKSLASHED or (char == "#" and index == 0)
                or (char == "~" and (index == 0 or value[index - 1] in ":="))):
            out.append("\\")
        out.append(char)
    return "".join(out)


def git_record_command(record: dict) -> str:
    """One parsed Git record as a shell-safe literal command.

    Callers use this only to route that record through an existing
    per-command policy check; the text is never evaluated.
    """
    def quoted(words):
        return "".join(_bash_quote(word) + " " for word in words)

    subcommand = command_substitution(jq_raw(record["subcommand"]))
    return quoted(record["assignments"]) + "git " + quoted([*record["global_args"], subcommand, *record["args"]])


def git_add_selects_elisp(repo: str, record: dict, env: dict | None = None) -> bool:
    """Whether a parsed `git add` would stage a changed Elisp file or emacs/config.org.

    Read-only: asks Git which changed tracked and untracked paths match the
    invocation's pathspecs.
    """
    tracked_only = all_paths = expect_value = pathspec_from_file = after_double_dash = False
    pathspecs = []
    for argument in record["args"]:
        if after_double_dash:
            pathspecs.append(argument)
            continue
        if expect_value:
            expect_value = False
            pathspec_from_file = True
            continue
        if argument == "--":
            after_double_dash = True
        elif argument in ("-A", "--all"):
            all_paths = True
        elif argument in ("-u", "--update"):
            tracked_only = all_paths = True
        elif (argument in ("-p", "--patch", "-i", "--interactive", "-e", "--edit", "-U", "--unified")
              or argument.startswith(("--unified=", "-U"))):
            all_paths = True
        elif argument in ("--pathspec-from-file", "--chmod"):
            expect_value = True
        elif argument.startswith("--pathspec-from-file="):
            pathspec_from_file = True
        elif argument.startswith("--chmod=") or argument in (
                "-N", "--intent-to-add", "-f", "--force", "--ignore-errors", "--ignore-missing", "--renormalize",
                "--sparse", "--refresh", "--no-all", "--no-ignore-removal"):
            pass
        elif (argument.startswith("-") and "A" in argument[1:]) or argument.startswith("--all="):
            all_paths = True
        elif argument.startswith("-") and "u" in argument[1:]:
            tracked_only = all_paths = True
        elif not argument.startswith("-"):
            pathspecs.append(argument)
    if pathspec_from_file:
        all_paths = True
        pathspecs = []
    if not pathspecs and not all_paths:
        return False
    selection = ["--", *pathspecs] if pathspecs else []
    list_args = ["-m", "-d"] + ([] if tracked_only else ["-o", "--exclude-standard"])
    # Paths already changed in the index count too: a repeated explicit add
    # still forms one compound stage-and-commit request and must be split.
    for git_args in (["ls-files", "-z", *list_args, *selection],
                     ["diff", "--cached", "--name-only", "-z", *selection]):
        output = subprocess.run(["git", "-C", repo, *git_args], env=env, stdout=subprocess.PIPE,
                                stderr=subprocess.DEVNULL, check=False).stdout
        # read -d '' sees only NUL-terminated entries.
        if any(path.endswith(b".el") or path == b"emacs/config.org" for path in output.split(b"\0")[:-1]):
            return True
    return False


# --- shell syntax ------------------------------------------------------------------

_INTERPRETER = re.compile(r"(^|[ \t|;&(/])(eval|bash|sh|zsh|dash|ksh|xargs|ssh)([ \t]|$)")
_EMACSCLIENT_EVAL = (r"(?:^|[;&|({]\s*)(?:(?:command|exec)\s+)?(?:/(?:opt/homebrew|usr/local|usr)/bin/)?"
                     r"emacsclient(?:\s+[^;&|]*)?\s(?:-e|--eval)\b")


def _shell_syntax_count(source: str, mode: str) -> int:
    """Count executable-looking shell syntax, ignoring inert quoted text.

    Emacsclient classification requires the executable at a command boundary,
    so quoted commit messages can never clear verification.
    """
    out = []
    pos = 0
    n = len(source)
    while pos < n:
        char = source[pos]
        if char == "\\" and pos + 1 < n:
            out.append(source[pos:pos + 2])
            pos += 2
            continue
        if char == "'":
            end = source.find("'", pos + 1)
            if end < 0:
                out.append(source[pos:])
                break
            out.append("''")
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
        scan = source if _INTERPRETER.search(source) else masked
        pattern = r"\bgit\s+commit\b"
    else:
        if "elpaca-extras-format-build-reload-status" in source:
            return 0
        scan = masked
        pattern = _EMACSCLIENT_EVAL
    return len(re.findall(pattern, scan, flags=re.MULTILINE))


def emacsclient_eval_count(source: str) -> int:
    return _shell_syntax_count(source, "emacsclient")


_COMMIT_VALUE_OPTIONS = {
    "--message", "--file", "--reuse-message", "--reedit-message",
    "--author", "--date", "--cleanup", "--trailer", "--fixup", "--squash",
    "--template", "--pathspec-from-file", "--unified", "--inter-hunk-context",
}
_COMMIT_FLAGS = {
    "--all", "--include", "--only", "--amend", "--no-edit", "--edit",
    "--quiet", "--verbose", "--signoff", "--no-verify", "--verify",
    "--allow-empty", "--allow-empty-message", "--reset-author",
    "--no-post-rewrite", "--post-rewrite", "--gpg-sign", "--no-gpg-sign",
    "--status", "--no-status", "--branch", "--no-branch",
    "--ahead-behind", "--no-ahead-behind", "--interactive", "--patch",
    "--pathspec-file-nul", "--untracked-files", "--no-untracked-files",
}


def git_commit_inspection_p(record: dict) -> bool:
    """Whether a parsed commit invocation cannot create a commit.

    Option values are consumed and -- is honoured, so message text and
    pathspecs named --dry-run never suppress a real post-commit obligation.
    """
    args = record.get("args", [])
    dry_run = False
    status_output = False
    position = 0
    while position < len(args):
        arg = args[position]
        position += 1
        if arg == "--":
            break
        if arg in {"--help", "-h"}:
            return True
        if arg == "--dry-run":
            dry_run = True
        elif arg == "--no-dry-run":
            dry_run = False
        elif arg in {"--short", "--long", "--porcelain"}:
            status_output = True
        elif arg in _COMMIT_VALUE_OPTIONS:
            position += 1
        elif arg.partition("=")[0] in _COMMIT_VALUE_OPTIONS | {"--gpg-sign", "--untracked-files"}:
            pass
        elif arg in _COMMIT_FLAGS:
            pass
        elif arg.startswith("--"):
            # Unknown options, including abbreviations and negated status
            # modes, do not establish that this invocation was only an inspection.
            return False
        elif arg.startswith("-"):
            for offset, flag in enumerate(arg[1:], 1):
                if flag in "mFCctU":
                    if offset == len(arg) - 1:
                        position += 1
                    break
                if flag in "Su":
                    break  # Optional short-option values must be attached.
                if flag == "h":
                    return True
                if flag not in "qvsenaiop":
                    return False
    return dry_run or status_output


_SEGMENT_BOUNDARIES = {";", ";;", "&", "&&", "|", "||", "(", ")", "{", "}"}
_WRAPPERS = {"command", "exec"}


def _command_tokens(source: str) -> list[str]:
    lexer = shlex.shlex(source, posix=True, punctuation_chars=";&|(){}")
    lexer.whitespace_split = True
    lexer.commenters = "#"
    return list(lexer)


def _is_word_assignment(token: str) -> bool:
    if "=" not in token or token.startswith(("/", "./", "../")):
        return False
    return token.partition("=")[0].replace("_", "a").isalnum()


def _is_segment_boundary(token: str) -> bool:
    return token in _SEGMENT_BOUNDARIES or all(char in ";&|(){}" for char in token)


def _command_position_words(source: str):
    """Each word in command position, skipping assignments and one wrapper.

    Raises HookFailure on unbalanced quotes, where the shell helpers exited
    non-zero without output.
    """
    try:
        tokens = _command_tokens(source)
    except ValueError as error:
        raise HookFailure("shell tokenizer failed: %s" % error) from error
    at_start = True
    skip_wrapper = False
    for token in tokens:
        if _is_segment_boundary(token):
            at_start = True
            skip_wrapper = False
            continue
        if not at_start:
            continue
        if _is_word_assignment(token):
            continue
        if token in _WRAPPERS and not skip_wrapper:
            skip_wrapper = True
            continue
        at_start = False
        yield token


def executable_count(source: str, executable: str) -> int:
    """Count `executable` in command position, ignoring quoted text, comments
    and later words, so a textual mention cannot bypass a helper allowlist."""
    return sum(os.path.basename(token) == executable for token in list(_command_position_words(source)))


def shell_executables(source: str) -> list[str]:
    return [os.path.basename(token) for token in list(_command_position_words(source))]


def elisp_evidence_labels(source: str, kind: str) -> list[str]:
    """The evidence label of each supported Elisp wrapper in executable position.

    This binds output to the label in the observed command; it is not an
    authentication boundary against another same-user process.
    """
    try:
        tokens = _command_tokens(source)
    except ValueError:
        return []
    segments = []
    segment = []
    for token in tokens:
        if _is_segment_boundary(token):
            if segment:
                segments.append(segment)
                segment = []
        else:
            segment.append(token)
    if segment:
        segments.append(segment)
    labels = []
    for words in segments:
        pos = 0
        while pos < len(words) and _is_word_assignment(words[pos]):
            pos += 1
        if pos < len(words) and words[pos] in _WRAPPERS:
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
            labels.append(label)
    return labels
