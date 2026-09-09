#!/usr/bin/env python3
"""Mask inert mentions of protected secret tools in the secret-output guard.

The guard denies any command that names `pbpaste`, `pass` or `security`
because those programs print the secret itself. A name is only a leak when
something can *run* it. This helper reads the quoted-literal-masked command on
stdin and replaces the protected word with `INERT_MENTION` when it is a plain
argument of a read-only text tool (`grep -rn pass docs/`, `wc -l pbpaste.md`,
`git log -S pbpaste`), so a search or a listing is not mistaken for a
credential read. Everything else is printed byte-for-byte: the command word
itself, arguments of shells, `xargs`, `find -exec`, `env`, `sudo` and every
program not in the read-only list, `=`-joined values such as
`git -c core.pager=pbpaste`, and `git` unless its subcommand is read-only and
no `-c` option precedes it. Those keep the fail-closed denial in the caller.

This is a token-level projection of shell text, not a shell parser. When the
input cannot be tokenized the original text is returned unchanged so the
caller's conservative rules still apply.
"""

from __future__ import annotations

import re
import shlex
import sys

PROTECTED = re.compile(r"(?<![A-Za-z0-9_=-])(?:pbpaste|pass|security)(?![A-Za-z0-9_-])")
# Programs that only read their arguments as text, patterns or paths.
TEXT_TOOLS = {
    "grep", "egrep", "fgrep", "rg", "ag", "ack", "wc", "cat", "less", "more",
    "head", "tail", "diff", "cmp", "comm", "ls", "stat", "file", "man",
    "apropos", "whatis", "tldr", "sort", "uniq", "cut", "tr", "column", "nl",
    "od", "xxd", "hexdump", "strings", "fold", "test", "[",
}
GIT_READ_ONLY = {
    "grep", "log", "show", "diff", "blame", "status", "ls-files", "rev-list",
    "rev-parse", "branch", "tag", "describe", "shortlog", "cat-file",
    "ls-tree", "name-rev", "reflog", "stash", "whatchanged",
}
WRAPPERS = {"command", "env", "sudo", "timeout", "nice", "exec", "nohup", "time", "builtin"}
ASSIGNMENT = re.compile(r"[A-Za-z_][A-Za-z0-9_]*=")
# Boundaries between simple commands. `$(` and backtick open a nested command;
# a protected word right after them is a command word and is never masked.
SEPARATOR = re.compile(r"(\|\||&&|[;|&\n()`]|\$\()")


def _command_word(tokens: list[str]) -> tuple[str | None, int]:
    index = 0
    while index < len(tokens):
        token = tokens[index]
        if ASSIGNMENT.match(token) or token.rsplit("/", 1)[-1] in WRAPPERS:
            index += 1
            continue
        if index and tokens[index - 1].rsplit("/", 1)[-1] in WRAPPERS and token.startswith("-"):
            index += 1
            continue
        return token.rsplit("/", 1)[-1], index
    return None, index


def _mask_segment(segment: str) -> str:
    if not PROTECTED.search(segment):
        return segment
    tokens = segment.split()
    if not tokens:
        return segment
    word, position = _command_word(tokens)
    if word is None:
        return segment
    args = tokens[position + 1:]
    if word == "git":
        if any(arg == "-c" or arg.startswith("--exec-path") or arg.startswith("--git-dir") for arg in args):
            return segment
        subcommand = next((arg for arg in args if not arg.startswith("-")), None)
        if subcommand not in GIT_READ_ONLY:
            return segment
    elif word not in TEXT_TOOLS:
        return segment
    # Mask only the argument region; the command word region is left intact.
    head_len = 0
    seen = 0
    for match in re.finditer(r"\S+", segment):
        if seen == position:
            head_len = match.end()
            break
        seen += 1
    head, rest = segment[:head_len], segment[head_len:]
    return head + PROTECTED.sub("INERT_MENTION", rest)


def mask_inert_mentions(command: str) -> str:
    parts = SEPARATOR.split(command)
    return "".join(part if SEPARATOR.fullmatch(part or "") else _mask_segment(part)
                   for part in parts if part is not None)


def mask_local_read_paths(command: str) -> str:
    """Project literal, isolated file reads out of the entropy scan only.

    Shell expansions, pipelines, redirects, wrappers and unknown read options
    receive no exemption. Known-secret and sensitive-file checks still use the
    original command in the caller. Tokenize quotes before recognizing command
    boundaries so shell source passed as an argument cannot look like a read.
    """
    if any(char in command for char in "$`\\"):
        return command
    # shlex removes quotes: quoted punctuation must not become a command
    # boundary (for example a network payload containing a literal semicolon).
    if any(re.search(r"[;&|()<>\n]", quoted)
           for quoted in re.findall(r"'[^']*'|\"[^\"]*\"", command)):
        return command
    lexer = shlex.shlex(command, posix=True, punctuation_chars=";&|()<>\n")
    lexer.whitespace = " \t\r"
    lexer.whitespace_split = True
    lexer.commenters = ""
    try:
        tokens = list(lexer)
    except ValueError:
        return command
    boundaries = {";", "&&", "\n"}
    start = 0
    changed = False
    for end in range(len(tokens) + 1):
        if end < len(tokens) and tokens[end] not in boundaries:
            continue
        segment = tokens[start:end]
        if segment and segment[0] in {"cat", "head", "tail"}:
            paths = []
            index = 1
            while index < len(segment):
                arg = segment[index]
                if segment[0] in {"head", "tail"} and arg in {"-n", "-c"}:
                    index += 1
                    if index >= len(segment) or not re.fullmatch(r"[0-9]+", segment[index]):
                        break
                elif segment[0] in {"head", "tail"} and re.fullmatch(r"-[0-9]+", arg):
                    pass
                elif re.fullmatch(r"(?:\.?\.?/)?[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)+", arg) and not arg.startswith("-"):
                    paths.append(start + index)
                else:
                    break
                index += 1
            else:
                for position in paths:
                    tokens[position] = "LOCAL_READ_PATH"
                    changed = True
        start = end + 1
    return shlex.join(tokens) if changed else command


def main() -> int:
    text = sys.stdin.read()
    try:
        projection = mask_local_read_paths if sys.argv[1:] == ["--local-read-paths"] else mask_inert_mentions
        sys.stdout.write(projection(text))
    except Exception:  # noqa: BLE001 - fail closed: return the original text.
        sys.stdout.write(text)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
