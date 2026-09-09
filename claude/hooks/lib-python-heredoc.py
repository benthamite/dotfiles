#!/usr/bin/env python3
"""Project supported Python heredoc syntax for shell guards.

First classify a closed, quoted Python stdin heredoc as Python: only genuine
ast.Pass spans are exempt from protected-name scanning. Protected names
elsewhere (including decoded strings, comments and safe documentation mentions)
are conservatively denied; shell quoting cannot erase a subprocess argument.
Then mask the validated body for the outer shell command-word/glob scan, where
Python operators are not shell syntax. Everything outside that body is
byte-preserved. Callers retain original input for all other secret checks.

This is not a Python sandbox or a general shell parser. Unknown shell shapes,
multiple redirections on one header and malformed Python remain unchanged for
the existing guard. Consecutive supported Python heredocs are classified in order.
The Git projection only neutralizes shell-substitution markers in valid quoted
Python bodies and retains other source for the existing Git classifier.
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
    # Earlier simple commands in a sequence (`cd x && python3 -`, `A=1; python3 -`)
    # do not change which program reads the heredoc: only the last one does.
    prefix = re.split(r"\|\||&&|;", prefix)[-1]
    # Do not reinterpret nested shells, pipelines, redirections or continuations.
    if re.search(r"[&|<>()`\\\r\n#]", prefix):
        return False
    try:
        words = shlex.split(prefix, posix=True)
    except ValueError:
        return False
    # An expansion may name a script argument after `-`, never the interpreter.
    try:
        stdin_marker = words.index("-")
    except ValueError:
        stdin_marker = len(words)
    if any("$" in word for word in words[:stdin_marker + 1]):
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
    # `pyenv exec <command> ...` selects the Python installation and forwards
    # stdin and arguments to that command. Recognize only this literal shape;
    # other pyenv subcommands and dynamic interpreter names stay unsupported.
    if len(words) >= 2 and PurePosixPath(words[0]).name == "pyenv" and words[1] == "exec":
        del words[:2]
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


def _git_shell_body(body: str, strip_tabs: bool) -> str | None:
    """Keep Python visible to Git scanning without inventing shell expansions."""
    runtime_body = "\n".join(line.lstrip("\t") if strip_tabs else line
                             for line in body.split("\n"))
    try:
        ast.parse(runtime_body)
    except (SyntaxError, ValueError):
        return None
    # Keep Git-bearing interpreter source conservative: a subprocess may run
    # strings as shell code. This is deliberately not Python execution analysis.
    if re.search(r"\b(?:git|commit)\b", body):
        return body
    # Quoted stdin never undergoes shell expansion. These characters in valid
    # Python are strings/comments, not shell command substitutions. Retain all
    # other source so this projection does not hide existing Git detections.
    return body.replace(chr(96), " ").replace("$(", "__")


def project_git_shell(command: str) -> str:
    return _project_command(command, git_shell=True)


def project_command(command: str) -> str:
    return _project_command(command, git_shell=False)


def _project_command(command: str, *, git_shell: bool) -> str:
    # Walk only consecutive known Python programs, never search arbitrary shell
    # text for an opener: it might be inside a quote or another program's body.
    lines = command.split("\n")  # Shell lines use LF, not Unicode separators.
    opener = 0
    while opener < len(lines):
        if not lines[opener].strip():
            opener += 1
            continue
        # Multiple redirects on one shell header need a heredoc queue. Keep
        # those unsupported, including a different sink before a Python command.
        if lines[opener].count("<<") != 1:
            break
        match = HEADER.fullmatch(lines[opener])
        if match is None or not _python_stdin(match["prefix"]):
            break
        delimiter = match["quoted"][1:-1]
        strip_tabs = bool(match["strip"])
        closing = next((index for index in range(opener + 1, len(lines))
                        if (lines[index].lstrip("\t") if strip_tabs else lines[index]) == delimiter), None)
        if closing is None:
            break
        body = "\n".join(lines[opener + 1:closing])
        projected = (_git_shell_body(body, strip_tabs) if git_shell
                     else _project_body(body, strip_tabs))
        if projected is None:
            break
        # The secret projection masks its classified body; the Git projection
        # preserves source except for inert substitution markers. Both retain
        # all bytes outside each supported body.
        for index, projected_line in enumerate(projected.split("\n"), opener + 1):
            lines[index] = (projected_line if git_shell else
                            " " * len(lines[index].encode("utf-8")))
        opener = closing + 1
    return "\n".join(lines)


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
