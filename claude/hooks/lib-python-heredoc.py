#!/usr/bin/env python3
"""Project supported Python heredoc syntax for shell guards.

First classify a closed, quoted Python stdin heredoc as Python: only genuine
ast.Pass spans are exempt from protected-name scanning. A separate closed AST
classifier permits text/JSON document edits in a standalone heredoc. Protected
names in every other Python shape remain conservatively denied; shell quoting
cannot erase a subprocess argument.
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


def _document_edit(tree: ast.Module) -> bool:
    """Recognize a closed text/JSON editing language, never evaluate source.

    Imports bind only the standard pathlib.Path and json interfaces. Values
    cannot become callables; every call and every statement must be recognized.
    Like the enclosing token guard, this assumes normal interpreter/stdlib
    resolution, not a hostile Python installation or filesystem sandbox.
    """
    bindings: dict[str, str] = {}

    def literal(node):
        return isinstance(node, ast.Constant) and type(node.value) in (str, int, float, bool, type(None))

    def keywords(call, allowed):
        return (len({kw.arg for kw in call.keywords}) == len(call.keywords)
                and all(kw.arg in allowed and literal(kw.value)
                        and type(kw.value.value) is allowed[kw.arg] for kw in call.keywords))

    def value(node):
        if literal(node):
            return "text" if isinstance(node.value, str) else "data"
        if isinstance(node, ast.Name):
            kind = bindings.get(node.id)
            return kind if kind in {"text", "data", "path"} else None
        if isinstance(node, (ast.List, ast.Tuple)):
            return "data" if all(value(item) in {"text", "data"} for item in node.elts) else None
        if isinstance(node, ast.Dict):
            return "data" if all(key is not None and value(key) in {"text", "data"}
                                 and value(item) in {"text", "data"}
                                 for key, item in zip(node.keys, node.values)) else None
        if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Add):
            return "text" if value(node.left) == value(node.right) == "text" else None
        if isinstance(node, ast.Compare) and len(node.ops) == 1:
            if (isinstance(node.ops[0], (ast.In, ast.NotIn, ast.Eq, ast.NotEq))
                    and value(node.left) == value(node.comparators[0]) == "text"):
                return "data"
        if not isinstance(node, ast.Call):
            return None
        if isinstance(node.func, ast.Name) and bindings.get(node.func.id) == "Path-import":
            if (len(node.args) == 1 and not node.keywords and literal(node.args[0])
                    and isinstance(node.args[0].value, str)
                    and PurePosixPath(node.args[0].value).suffix in {".md", ".org", ".txt", ".json"}):
                return "path"
            return None
        if not isinstance(node.func, ast.Attribute):
            return None
        receiver, method = node.func.value, node.func.attr
        if isinstance(receiver, ast.Name) and bindings.get(receiver.id) == "json-import":
            if len(node.args) != 1:
                return None
            if method == "loads" and value(node.args[0]) == "text" and not node.keywords:
                return "data"
            if (method == "dumps" and value(node.args[0]) in {"text", "data"}
                    and keywords(node, {"ensure_ascii": bool, "indent": int, "sort_keys": bool})):
                return "text"
            return None
        kind = value(receiver)
        if (kind == "data" and isinstance(receiver, ast.Name) and method == "update"
                and len(node.args) == 1 and isinstance(node.args[0], ast.Dict)
                and not node.keywords and value(node.args[0]) == "data"):
            return "mutated"
        if kind == "path" and keywords(node, {"encoding": str}):
            if method == "read_text" and not node.args:
                return "text"
            if method == "write_text" and len(node.args) == 1 and value(node.args[0]) == "text":
                return "written"
        if (kind == "text" and method == "replace" and len(node.args) == 2
                and not node.keywords and all(value(arg) == "text" for arg in node.args)):
            return "text"
        return None

    wrote = False
    for statement in tree.body:
        if isinstance(statement, ast.Import) and len(statement.names) == 1:
            name = statement.names[0]
            if name.name != "json" or name.asname or "json" in bindings:
                return False
            bindings["json"] = "json-import"
        elif isinstance(statement, ast.ImportFrom):
            if (statement.module != "pathlib" or statement.level or len(statement.names) != 1
                    or statement.names[0].name != "Path" or statement.names[0].asname
                    or "Path" in bindings):
                return False
            bindings["Path"] = "Path-import"
        elif isinstance(statement, ast.Assign) and len(statement.targets) == 1:
            target = statement.targets[0]
            kind = value(statement.value)
            if kind not in {"path", "text", "data"}:
                return False
            if isinstance(target, ast.Name):
                if target.id in {"json", "Path"}:
                    return False
                bindings[target.id] = kind
            elif (isinstance(target, ast.Subscript) and isinstance(target.value, ast.Name)
                  and bindings.get(target.value.id) == "data" and literal(target.slice)
                  and kind in {"text", "data"}):
                continue
            else:
                return False
        elif isinstance(statement, ast.Assert):
            if value(statement.test) != "data" or (statement.msg is not None and value(statement.msg) != "text"):
                return False
        elif isinstance(statement, ast.Expr):
            kind = value(statement.value)
            if kind == "written":
                wrote = True
            elif kind != "mutated":
                return False
        else:
            return False
    return wrote


def _project_body(body: str, strip_tabs: bool, *, document_edit: bool = False) -> str | None:
    original_lines = body.split("\n")
    runtime_lines = [line.lstrip("\t") if strip_tabs else line for line in original_lines]
    runtime_body = "\n".join(runtime_lines)
    try:
        tree = ast.parse(runtime_body)
    except (SyntaxError, ValueError):
        return None

    if document_edit and _document_edit(tree):
        return body

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


def project_command(command: str, *, allow_document_edit: bool = True) -> str:
    return _project_command(command, git_shell=False, allow_document_edit=allow_document_edit)


def _project_command(command: str, *, git_shell: bool, allow_document_edit: bool = True) -> str:
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
        # A document could otherwise be executed by a later shell command.
        # This new exception therefore requires a single complete heredoc and
        # a direct literal interpreter invocation, without environment wrappers.
        document_edit = (allow_document_edit and not any(line.strip() for line in lines[:opener])
                         and not any(line.strip() for line in lines[closing + 1:])
                         and re.fullmatch(r"\s*(?:/[A-Za-z0-9_./-]+/)?python(?:3(?:\.[0-9]+)?)?\s+-\s*",
                                          match["prefix"]) is not None)
        projected = (_git_shell_body(body, strip_tabs) if git_shell
                     else _project_body(body, strip_tabs, document_edit=document_edit))
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
    if sys.argv[1:] not in ([], ["--no-document-edits"], ["--allow-document-edits"]):
        return 2
    try:
        projected = project_command(sys.stdin.read(), allow_document_edit=sys.argv[1:] != ["--no-document-edits"])
    except ProtectedPythonReference:
        print("Python heredoc names a protected credential or clipboard tool.", file=sys.stderr)
        return 2
    sys.stdout.write(projected)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
