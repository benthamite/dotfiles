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


def _network_file_operands(tokens: list[str]) -> list[tuple[int, str]]:
    """Return file-value positions only for a fully recognized curl/wget argv.

    Unknown options or shell syntax receive no exemptions. Payload arguments
    are consumed as values, so a literal '-o' body cannot nominate a filename.
    """
    program = tokens[0] if tokens else ""
    if program not in {"curl", "wget"}:
        return []
    curl = program == "curl"
    files = ({"-o", "--output", "-T", "--upload-file", "-K", "--config"} if curl
             else {"-O", "--output-document", "-o", "--output-file", "--post-file", "--body-file", "-i", "--input-file"})
    values = ({"-H", "--header", "-d", "--data", "--data-ascii", "--data-binary",
               "--data-raw", "--data-urlencode", "--json", "-F", "--form", "--form-string",
               "-X", "--request", "--url", "--max-time", "--connect-timeout", "--retry",
               "-A", "--user-agent", "-e", "--referer", "-u", "--user"} if curl
              else {"--header", "--post-data", "--body-data", "--method", "--timeout", "--tries"})
    switches = ({"--silent", "--show-error", "--location", "--fail", "--head", "--include", "--verbose", "--insecure", "--no-buffer"} if curl
                else {"--quiet", "--no-verbose", "--verbose"})
    paths = []
    index = 1
    while index < len(tokens):
        token = tokens[index]
        if token.startswith(("https://", "http://")):
            index += 1
            continue
        if token in switches or (curl and re.fullmatch(r"-[sSLfqIivkN]+", token)) or (not curl and token in {"-q", "-nv", "-v"}):
            index += 1
            continue
        option, separator, value = token.partition("=")
        joined = token.startswith("--") and bool(separator)
        if option in files | values and joined:
            pass
        elif token in files | values:
            option = token
            index += 1
            if index >= len(tokens):
                return []
            value = tokens[index]
        else:
            # Combined options and other shell forms remain fully scanned.
            return []
        is_file = option in files
        replacement = "LOCAL_NETWORK_FILE"
        if curl and option == "--data-urlencode":
            # curl selects name=value before its name@file/@file forms.
            is_file = "=" not in value and "@" in value
            if is_file:
                replacement = value.partition("@")[0] + "@LOCAL_NETWORK_FILE"
        elif curl and option in {"-H", "--header", "-d", "--data", "--data-ascii", "--data-binary", "--json"}:
            is_file = value.startswith("@")
        elif curl and option in {"-F", "--form"}:
            # Only the simple file form; multipart metadata stays scanned.
            is_file = bool(re.fullmatch(r"[^=]+=[@<][A-Za-z0-9_./-]+", value))
            if is_file:
                replacement = value.partition("=")[0] + "=" + value.partition("=")[2][0] + "LOCAL_NETWORK_FILE"
        if is_file:
            paths.append((index, option + "=" + replacement if joined else replacement))
        index += 1
    return paths


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
        elif segment and segment[0] in {"curl", "wget"}:
            for position, replacement in _network_file_operands(segment):
                tokens[start + position] = replacement
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
