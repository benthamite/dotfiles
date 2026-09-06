#!/usr/bin/env python3
"""Inspect shell exports without executing the file or printing values.

Output is a JSON list containing only names, line numbers and analysis metadata.
Export classifications: credential-literal, credential-store-backed,
credential-indirect, credential-empty, identity, non-secret. Scopes: global,
function, function-local, subshell. Certainty: unconditional or conditional.
Ordinary function exports can affect their caller: function scope is potential
ambient exposure, not local or safe. Whether functions are invoked is unknown.
Names are only heuristics: non-secret means an unrecognized name, not a checked
secret-free value. Each export row identifies this classification basis.
Assignments to variables already observed as exported also produce export rows.
Conditional writes invalidate known values; they cannot establish a guaranteed
local declaration. No branch condition is evaluated.

The bounded supported subset includes simple assignments, multiple export
operands, local/declare/typeset export attributes, functions, subshells and simple
if/loop/AND/OR command lists. Quotes, continuations and substitutions are words,
never executed. A not-checked entry has a fixed reason instead of a name and
means coverage is incomplete. Heredocs, case, arithmetic, pipelines/redirections
and malformed syntax stop analysis at that point; dynamic evaluation and
unsupported declaration operands/options are reported without claiming to have
inspected their effects. This is not an interpreter or a data-flow proof.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import stat
from dataclasses import dataclass, field
from pathlib import Path


NAME_RE = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")
ASSIGNMENT_RE = re.compile(r"^([A-Za-z_][A-Za-z0-9_]*)=(.*)$", re.S)
SECRET_NAME_RE = re.compile(
    r"(?:^|_)(?:API_?KEY|TOKEN|SECRET|PASSWORD|PASSWD|CREDENTIALS?|"
    r"AUTH(?:_TOKEN)?|PRIVATE_KEY)(?:_|$)", re.I,
)
IDENTITY_NAME_RE = re.compile(
    r"(?:^|_)(?:EMAIL|USERNAME|USER_NAME|PHONE|MOBILE)(?:_|$)", re.I,
)
STORE_COMMAND_RE = re.compile(
    r"^\s*(?:command\s+)?(?:pass|op|op-desktop|op-automations|security|envchain)\s"
)
MAX_INPUT_BYTES = 1024 * 1024
MAX_TOKENS = 50000
MAX_FRAMES = 128
# Identifier-compatible forms of patterns already used by the repository's
# redaction guard. Arbitrary secrets embedded in names cannot be recognized.
SENSITIVE_NAME_RE = re.compile(
    r"(?:AKIA|ASIA)[A-Z0-9]{16}|gh[pousr]_[A-Za-z0-9_]{36,}|"
    r"github_pat_[A-Za-z0-9_]{22,}|(?:sk|pk|rk)_(?:live|test)_[A-Za-z0-9]{20,}|"
    r"lin_api_[A-Za-z0-9]{40,}|AIza[0-9A-Za-z_]{35}"
)


class Unsupported(Exception):
    def __init__(self, line: int, reason: str):
        self.line, self.reason = line, reason


@dataclass
class Token:
    raw: str
    line: int
    operator: bool = False


def fragment_end(text: str, start: int, depth: int = 0) -> int:
    """Skip one quote/expansion, including nested quoted substitutions."""
    if depth > 64:
        raise ValueError
    if text[start] in "'\"`":
        delimiter, index = text[start], start + 1
        while index < len(text):
            if text[index] == delimiter:
                return index + 1
            if delimiter != "'" and text[index] == "\\":
                index += 2
            elif delimiter == '"' and text.startswith(("$(", "${"), index):
                index = fragment_end(text, index, depth + 1)
            else:
                index += 1
        raise ValueError
    closing = ")" if text.startswith("$(", start) else "}"
    opening = "(" if closing == ")" else "{"
    index, nesting = start + 2, 1
    while index < len(text):
        if text[index] == "\\":
            index += 2
        elif text[index] in "'\"`" or text.startswith(("$(", "${"), index):
            index = fragment_end(text, index, depth + 1)
        else:
            if text[index] == opening:
                nesting += 1
            elif text[index] == closing:
                nesting -= 1
                if not nesting:
                    return index + 1
            index += 1
    raise ValueError


def tokenize(text: str):
    """Yield shell words/operators, stopping before unsupported syntax."""
    index, line = 0, 1
    while index < len(text):
        if text[index] in " \t\r":
            index += 1
            continue
        if text.startswith("\\\n", index):
            index += 2
            line += 1
            continue
        if text[index] == "#":
            end = text.find("\n", index)
            index = len(text) if end < 0 else end
            continue
        if text.startswith(("<<", "((", "<(", ">("), index):
            raise Unsupported(line, "unsupported-heredoc-arithmetic-or-process-substitution")
        if text[index] in ";\n(){}&|<>":
            raw = text[index]
            if text[index:index + 2] in {"&&", "||", ">>", ";;"}:
                raw = text[index:index + 2]
            yield Token(raw, line, True)
            index += len(raw)
            line += raw.count("\n")
            continue
        start, start_line = index, line
        while index < len(text) and text[index] not in " \t\r\n;()&|<>":
            if text[index] == "\\":
                if index + 1 == len(text):
                    raise Unsupported(start_line, "unfinished-shell-escape")
                index += 2
            elif text[index] in "'\"`" or text.startswith(("$(", "${"), index):
                try:
                    index = fragment_end(text, index)
                except ValueError:
                    raise Unsupported(start_line, "unclosed-or-overnested-shell-word") from None
            else:
                index += 1
        raw = text[start:index]
        line += raw.count("\n")
        # Remove only unquoted continuations. A backslash and newline inside
        # single quotes are literal data, not an empty assignment.
        pieces, position = [], 0
        while position < len(raw):
            if raw[position] in "'\"`" or raw.startswith(("$(", "${"), position):
                end = fragment_end(raw, position)
                pieces.append(raw[position:end])
                position = end
            elif raw.startswith("\\\n", position):
                position += 2
            else:
                pieces.append(raw[position:position + 2] if raw[position] == "\\" else raw[position])
                position += 2 if raw[position] == "\\" else 1
        yield Token("".join(pieces), start_line)


def value_kind(value: str | None) -> str:
    if value is None:
        return "indirect"
    if value in {"", "''", '""'}:
        return "empty"
    # A command prefix alone cannot establish the provenance of concatenated
    # literals, pipelines, or subsequent commands in the substitution.
    whole = value
    if whole.startswith('"') and fragment_end(whole, 0) == len(whole):
        whole = whole[1:-1]
    if whole.startswith(("$(", "`")) and fragment_end(whole, 0) == len(whole):
        content = whole[1 if whole.startswith("`") else 2:-1]
        if STORE_COMMAND_RE.match(content):
            try:
                command_tokens = list(tokenize(content))
            except Unsupported:
                command_tokens = []
            if command_tokens and not any(token.operator for token in command_tokens):
                return "store-backed"
    index, quote, dynamic, literal = 0, None, False, False
    while index < len(value):
        character = value[index]
        if character == "\\" and quote != "'":
            literal |= value[index:index + 2] != "\\\n"
            index += 2
            continue
        if character == "'" and quote != '"':
            quote = None if quote else "'"
        elif character == '"' and quote != "'":
            quote = None if quote else '"'
        elif quote != "'" and (character == "`" or value.startswith("$(", index)):
            end = fragment_end(value, index)
            dynamic = True
            index = end
            continue
        elif quote != "'" and character == "$":
            dynamic = True
        else:
            literal = True
        index += 1
    return "indirect" if dynamic else "literal" if literal else "empty"


def has_assignment_expansion(word: str, quote: str | None = None) -> bool:
    """Notice caller-mutating expansions, without evaluating their contents."""
    index = 0
    while index < len(word):
        character = word[index]
        if character == "\\":
            index += 2
            continue
        if character == "'" and quote is None:
            index = fragment_end(word, index)
            continue
        if character == '"':
            quote = None if quote else '"'
            index += 1
            continue
        if word.startswith("$((", index):
            return True
        if word.startswith("${", index):
            end = fragment_end(word, index)
            body = word[index + 2:end - 1]
            if re.match(r"[A-Za-z_][A-Za-z0-9_]*(?:\[[^]]*\])?:?=", body) or has_assignment_expansion(body, quote):
                return True
            index = end
            continue
        if character == "`" or word.startswith("$(", index):
            # Ordinary command substitutions run in a child shell; mutations
            # inside them do not establish a new caller value.
            index = fragment_end(word, index)
            continue
        index += 1
    return False


def read_source(path: Path) -> str:
    """Read one bounded regular file without following a final symlink."""
    descriptor = os.open(path, os.O_RDONLY | os.O_NONBLOCK | os.O_NOFOLLOW)
    try:
        before = os.fstat(descriptor)
        if not stat.S_ISREG(before.st_mode) or before.st_size > MAX_INPUT_BYTES:
            raise Unsupported(1, "input-type-or-size-limit")
        chunks, remaining = [], MAX_INPUT_BYTES + 1
        while remaining:
            chunk = os.read(descriptor, min(65536, remaining))
            if not chunk:
                break
            chunks.append(chunk)
            remaining -= len(chunk)
        after = os.fstat(descriptor)
        data = b"".join(chunks)
        identity = lambda info: (info.st_dev, info.st_ino, info.st_mode, info.st_size,
                                 info.st_mtime_ns, info.st_ctime_ns)
        if len(data) > MAX_INPUT_BYTES or len(data) != before.st_size or identity(before) != identity(after):
            raise Unsupported(1, "input-changed-or-size-limit")
        return data.decode("utf-8")
    finally:
        os.close(descriptor)


def classify(name: str, kind: str) -> str:
    if SECRET_NAME_RE.search(name):
        return "credential-" + kind
    return "identity" if IDENTITY_NAME_RE.search(name) else "non-secret"


@dataclass
class Frame:
    kind: str
    conditional: bool = False
    values: dict[str, str] = field(default_factory=dict)
    local_names: set[str] = field(default_factory=set)
    exported_names: dict[str, bool] = field(default_factory=dict)


def inspect(path: Path) -> list[dict[str, object]]:
    findings: list[dict[str, object]] = []
    tokens: list[Token] = []
    incomplete: Unsupported | None = None
    try:
        for token in tokenize(read_source(path)):
            if len(tokens) >= MAX_TOKENS:
                raise Unsupported(token.line, "token-limit")
            tokens.append(token)
    except Unsupported as error:
        incomplete = error
        # Discard an incomplete command, retaining earlier complete statements.
        while tokens and tokens[-1].raw not in {";", "\n", "}", ")"}:
            tokens.pop()
    frames = [Frame("global")]
    statement: list[Token] = []
    conditional_next = False
    pending_function = False
    function_names: set[str] = set()

    def diagnostic(line: int, reason: str):
        findings.append({"classification": "not-checked", "line": line,
                         "scope": "unknown", "reason": reason})

    def unknown_effects(line: int, reason: str):
        diagnostic(line, reason)
        for frame in frames:
            frame.values.update({name: "indirect" for name in frame.values})
            frame.local_names.clear()
            frame.exported_names.update({name: True for name in frame.exported_names})

    def close_frame():
        closed = frames.pop()
        if closed.kind == "conditional" or (closed.kind == "group" and closed.conditional):
            # A branch may have run. Retain possible exports and invalidate
            # values, but never promote conditional locals to guaranteed ones.
            parent = next(frame for frame in reversed(frames)
                          if frame.kind != "group" or frame.conditional)
            parent.values.update({name: "indirect" for name in closed.values})
            for name in closed.exported_names:
                parent.exported_names.setdefault(name, True)

    def command(words: list[Token], conditional: bool = False, depth: int = 0):
        if not words:
            return
        if depth > 64:
            diagnostic(words[0].line, "overnested-command-prefixes")
            return
        # Ordinary brace groups do not introduce a shell variable scope.
        binding_frame = next(frame for frame in reversed(frames)
                             if frame.kind != "group" or frame.conditional)
        head, operands = words[0].raw, words[1:]
        if any(has_assignment_expansion(word.raw) for word in words):
            unknown_effects(words[0].line, "dynamic-expansion-effects")
        if head == "!":
            # Negation changes exit status, not whether this command runs.
            command(operands, conditional, depth + 1)
            return
        if not ASSIGNMENT_RE.fullmatch(head) and any(character in head for character in "'\"\\$`"):
            unknown_effects(words[0].line, "dynamic-or-quoted-command")
            return
        if head in function_names:
            unknown_effects(words[0].line, "function-invocation-effects")
            return
        if head in {"source", ".", "eval", "alias", "unalias", "set", "setopt", "unsetopt", "unset",
                    "read", "getopts", "mapfile", "readarray", "let", "return", "break", "continue", "exit", "exec"} or (
                        head == "printf" and any(word.raw == "-v" for word in operands)):
            unknown_effects(words[0].line, "dynamic-shell-effects")
            return
        if head in {"command", "builtin"} and operands:
            if operands[0].raw in {"-v", "-V"} and head == "command":
                return
            while operands and operands[0].raw in {"--", "-p"}:
                operands.pop(0)
            if operands and operands[0].raw.startswith("-"):
                unknown_effects(words[0].line, "unsupported-command-options")
                return
            command(operands, conditional, depth + 1)
            return
        declarations = {"export", "local", "typeset", "declare", "readonly"}
        if head not in declarations:
            if re.match(r"[A-Za-z_][A-Za-z0-9_]*(?:\+|\[[^]]*\])=", head):
                unknown_effects(words[0].line, "unsupported-assignment-mutation")
                return
            # Assignment-only commands set shell variables; command-prefixed
            # assignments inject values into that process, not the caller.
            assignments = [ASSIGNMENT_RE.fullmatch(word.raw) for word in words]
            if all(assignments):
                for operand, assignment in zip(words, assignments):
                    name = assignment[1]
                    previous_export = next((frame.exported_names[name]
                                            for frame in reversed(frames)
                                            if name in frame.exported_names), None)
                    if previous_export is not None:
                        command([Token("export", operand.line), operand],
                                conditional or previous_export, depth + 1)
                    else:
                        binding_frame.values[name] = "indirect" if conditional else value_kind(assignment[2])
            elif assignments[0]:
                first_command = next(index for index, assignment in enumerate(assignments)
                                     if assignment is None)
                command(words[first_command:], conditional, depth + 1)
            return
        flags = ""
        while operands and operands[0].raw.startswith("-"):
            option = operands.pop(0).raw
            if option == "--":
                break
            flags += option[1:]
        if any(flag not in "xgr" for flag in flags):
            unknown_effects(words[0].line, "unsupported-declaration-options")
            return
        exporting = head == "export" or "x" in flags
        function = next((frame for frame in reversed(frames)
                         if frame.kind in {"function", "function-subshell"}), None)
        # A nested function may be invoked outside its defining function. Its
        # caller's locals therefore cannot establish a guaranteed local scope.
        function_index = next((index for index, frame in enumerate(frames)
                               if frame is function), 0)
        active_frames = frames[function_index:]
        local = function is not None and head in {"local", "typeset", "declare"} and "g" not in flags
        if head == "local" and function is None:
            diagnostic(words[0].line, "local-declaration-outside-function")
            return
        for operand in operands:
            assignment = ASSIGNMENT_RE.fullmatch(operand.raw)
            name = assignment[1] if assignment else operand.raw
            if not NAME_RE.fullmatch(name):
                unknown_effects(operand.line, "unsupported-export-operand")
                continue
            if len(name) > 128 or SENSITIVE_NAME_RE.search(name):
                unknown_effects(operand.line, "sensitive-or-oversized-identifier")
                continue
            previous_export = next((frame.exported_names[name] for frame in reversed(frames)
                                    if name in frame.exported_names), None)
            if local and not conditional:
                binding_frame.local_names.add(name)
            if assignment:
                kind = value_kind(assignment[2])
                binding_frame.values[name] = "indirect" if conditional else kind
            else:
                kind = "indirect" if local else next((frame.values[name] for frame in reversed(active_frames)
                             if name in frame.values), "indirect")
                if local:
                    binding_frame.values[name] = "indirect"
            if not exporting and not (assignment and previous_export is not None):
                continue
            export_conditional = conditional or (not exporting and bool(previous_export))
            binding_frame.exported_names[name] = (
                binding_frame.exported_names.get(name, True) and export_conditional
            )
            if any(frame.kind in {"subshell", "function-subshell"} for frame in frames):
                scope = "subshell"
            elif local or ("g" not in flags and any(name in frame.local_names for frame in active_frames)):
                scope = "function-local"
            else:
                scope = "function" if function else "global"
            certainty = "conditional" if export_conditional or any(
                frame.conditional or frame.kind in {"function", "function-subshell", "conditional"}
                for frame in frames
            ) else "unconditional"
            findings.append({"classification": classify(name, kind), "classification_basis": "name-heuristic",
                             "line": operand.line,
                             "name": name, "scope": scope, "certainty": certainty})

    index = 0
    while index < len(tokens):
        token = tokens[index]
        raw = token.raw
        if pending_function and not token.operator:
            diagnostic(token.line, "unsupported-function-body")
            break
        if not statement and not token.operator:
            if raw == "case":
                diagnostic(token.line, "unsupported-case-statement")
                break
            if NAME_RE.fullmatch(raw) and [item.raw for item in tokens[index + 1:index + 3]] == ["(", ")"]:
                function_names.add(raw)
                pending_function = True
                index += 3
                continue
            if raw == "function":
                if index + 1 >= len(tokens) or not NAME_RE.fullmatch(tokens[index + 1].raw):
                    diagnostic(token.line, "unsupported-function-declaration")
                    break
                pending_function = True
                function_names.add(tokens[index + 1].raw)
                index += 2
                if [item.raw for item in tokens[index:index + 2]] == ["(", ")"]:
                    index += 2
                continue
            if raw in {"if", "while", "until", "for", "select"}:
                if len(frames) >= MAX_FRAMES:
                    diagnostic(token.line, "shell-frame-limit")
                    break
                frames.append(Frame("conditional"))
                if raw in {"for", "select"}:
                    if index + 1 >= len(tokens) or not NAME_RE.fullmatch(tokens[index + 1].raw):
                        diagnostic(token.line, "unsupported-loop-binding")
                        break
                    variable = tokens[index + 1]
                    frames[-1].values[variable.raw] = "indirect"
                    if any(variable.raw in frame.exported_names for frame in frames):
                        command([Token("export", variable.line), variable], conditional=True)
                index += 1
                continue
            if raw in {"then", "do", "else", "elif"}:
                if raw in {"else", "elif"} and frames[-1].kind == "conditional":
                    close_frame()
                    frames.append(Frame("conditional"))
                index += 1
                continue
            if raw in {"fi", "done"}:
                if frames[-1].kind != "conditional":
                    diagnostic(token.line, "unbalanced-shell-structure")
                    break
                close_frame()
                index += 1
                continue
        if token.operator:
            if raw in {"{", "("}:
                if statement:
                    diagnostic(token.line, "unsupported-compound-command")
                    break
                if len(frames) >= MAX_FRAMES:
                    diagnostic(token.line, "shell-frame-limit")
                    break
                kind = "function-subshell" if pending_function and raw == "(" else "function" if pending_function else "subshell" if raw == "(" else "group"
                frames.append(Frame(kind, conditional=conditional_next))
                conditional_next = False
                pending_function = False
            elif raw in {"}", ")"}:
                command(statement, conditional_next)
                statement, conditional_next = [], False
                expected = {"group", "function"} if raw == "}" else {"subshell", "function-subshell"}
                if len(frames) == 1 or frames[-1].kind not in expected:
                    diagnostic(token.line, "unbalanced-shell-structure")
                    break
                close_frame()
            elif raw in {";", "\n", "&&", "||"}:
                command(statement, conditional_next)
                had_statement = bool(statement)
                statement = []
                if had_statement or raw != "\n":
                    conditional_next = raw in {"&&", "||"}
            elif raw in {"|", "&", "<", ">", ">>", ";;"}:
                diagnostic(token.line, "unsupported-pipeline-background-or-redirection")
                break
        else:
            if pending_function:
                diagnostic(token.line, "unsupported-function-body")
                break
            statement.append(token)
        index += 1
    else:
        command(statement, conditional_next)
        if len(frames) != 1 or pending_function:
            diagnostic(tokens[-1].line if tokens else 1, "unclosed-shell-structure")
    if incomplete:
        diagnostic(incomplete.line, incomplete.reason)
    return findings


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("path", type=Path)
    args = parser.parse_args()
    try:
        findings = inspect(args.path)
    except (OSError, UnicodeError):
        findings = [{"classification": "not-checked", "line": 1,
                     "scope": "unknown", "reason": "unreadable-input"}]
    print(json.dumps(findings, indent=2))
    return 2 if any(row["classification"] == "not-checked" for row in findings) else 0


if __name__ == "__main__":
    raise SystemExit(main())
