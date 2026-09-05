#!/usr/bin/env python3
"""Project supported Python heredoc syntax for the secret-output guard.

First classify a closed, quoted Python stdin heredoc as Python: only genuine
ast.Pass spans are exempt from protected-name scanning. Protected names
elsewhere (including decoded strings, comments and safe documentation mentions)
are conservatively denied; shell quoting cannot erase a subprocess argument.
Then mask the validated body for the outer shell command-word/glob scan, where
Python operators are not shell syntax. Everything outside that body is
byte-preserved. Callers retain original input for all other secret checks.

This is not a Python sandbox or a general shell parser. Unknown shell shapes,
multiple heredocs and malformed Python remain unchanged for the existing guard.
No source is evaluated, imported, or executed.
"""

from __future__ import annotations

import ast
import re
import shlex
import sys
from pathlib import PurePosixPath

PROTECTED = re.compile(r"(?<![A-Za-z0-9_-])(?:pass|security|pbpaste)(?![A-Za-z0-9_-])")
ASSIGNMENT = re.compile(r"[A-Za-z_][A-Za-z0-9_]*=.*", re.DOTALL)
PYTHON = re.compile(r"python(?:3(?:\.[0-9]+)?)?")
HEADER = re.compile(
    r"(?P<prefix>.+?)(?<![0-9])<<(?P<strip>-?)[ \t]*"
    r"(?P<quoted>'[A-Za-z_][A-Za-z0-9_]*'|\"[A-Za-z_][A-Za-z0-9_]*\")"
    r"[ \t]*(?:\#.*)?"
)


class ProtectedPythonReference(ValueError):
    """A protected tool name remains in interpreter source."""


def _python_stdin(prefix: str) -> bool:
    # Do not reinterpret nested shells, expansions, continuations or pipelines.
    if re.search(r"[;&|<>()`$\\\r\n#]", prefix):
        return False
    try:
        words = shlex.split(prefix, posix=True)
    except ValueError:
        return False
    while words and ASSIGNMENT.fullmatch(words[0]):
        words.pop(0)
    if words and PurePosixPath(words[0]).name == "env":
        words.pop(0)
        while words:
            if words[0] in ("-i", "--ignore-environment"):
                words.pop(0)
            elif words[0] == "-u" and len(words) > 1 and re.fullmatch(
                    r"[A-Za-z_][A-Za-z0-9_]*", words[1]):
                del words[:2]
            elif words[0] == "--":
                words.pop(0)
                break
            elif ASSIGNMENT.fullmatch(words[0]):
                words.pop(0)
            else:
                break
    if words and words[0] == "command":
        words.pop(0)
        if words and words[0] == "--":
            words.pop(0)
    if not words or not PYTHON.fullmatch(PurePosixPath(words.pop(0)).name):
        return False
    while words:
        word = words.pop(0)
        if word == "-":
            return True  # Following words are script arguments, not a program.
        if re.fullmatch(r"-[BEIqsuSO]+", word):
            continue
        # -c/-m, a script filename, unknown flags and option arguments are not
        # the stdin-source contract recognized here.
        return False
    return True  # With no script argument, redirected stdin is Python source.


def _project_body(body: str, strip_tabs: bool) -> str | None:
    original_lines = body.split("\n")
    runtime_lines = [line.lstrip("\t") if strip_tabs else line for line in original_lines]
    runtime_body = "\n".join(runtime_lines)
    try:
        tree = ast.parse(runtime_body)
    except (SyntaxError, ValueError):
        return None

    original_bytes = body.encode("utf-8")
    starts = []
    position = 0
    for line in original_lines:
        starts.append(position)
        position += len(line.encode("utf-8")) + 1
    spans = []
    for node in ast.walk(tree):
        if isinstance(node, ast.Pass):
            if node.end_lineno != node.lineno:
                return None
            line = node.lineno - 1
            stripped = len(original_lines[line].encode("utf-8")) - len(
                runtime_lines[line].encode("utf-8"))
            start = starts[line] + stripped + node.col_offset
            end = starts[line] + stripped + node.end_col_offset
            if original_bytes[start:end] != b"pass":
                return None
            spans.append((start, end))
        elif isinstance(node, ast.Constant) and isinstance(node.value, (str, bytes)):
            value = node.value.decode("utf-8", "replace") if isinstance(node.value, bytes) else node.value
            if PROTECTED.search(value):
                raise ProtectedPythonReference
    projected = bytearray(original_bytes)
    for start, end in spans:
        projected[start:end] = b" " * (end - start)
    result = projected.decode("utf-8")
    if PROTECTED.search(result):
        raise ProtectedPythonReference
    return result


def project_command(command: str) -> str:
    # This deliberately recognizes one unambiguous heredoc only. In particular,
    # a fake Python opener inside another heredoc cannot gain an exemption.
    if command.count("<<") != 1:
        return command
    lines = command.split("\n")  # The shell separates lines on LF, not Unicode separators.
    opener = next((index for index, line in enumerate(lines) if line.strip()), None)
    if opener is None:
        return command
    match = HEADER.fullmatch(lines[opener])
    if match is None or not _python_stdin(match["prefix"]):
        return command
    delimiter = match["quoted"][1:-1]
    strip_tabs = bool(match["strip"])
    closing = next((index for index in range(opener + 1, len(lines))
                    if (lines[index].lstrip("\t") if strip_tabs else lines[index]) == delimiter), None)
    if closing is None:
        return command
    body = "\n".join(lines[opener + 1:closing])
    if closing > opener + 1:
        body += "\n"
    projected = _project_body(body, strip_tabs)
    if projected is None:
        return command
    prefix = "\n".join(lines[:opener + 1]) + "\n"
    suffix = "\n".join(lines[closing:])
    # Python has already received its own protected-reference classification.
    # Its *args, **kwargs, multiplication, and indexing are not shell globs.
    # Do not expose this body to the outer shell lexer a second time.
    masked = bytes(10 if byte == 10 else 32 for byte in body.encode("utf-8")).decode("ascii")
    return prefix + masked + suffix


def main() -> int:
    try:
        projected = project_command(sys.stdin.read())
    except ProtectedPythonReference:
        print("Python heredoc names a protected credential or clipboard tool.", file=sys.stderr)
        return 2
    sys.stdout.write(projected)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
