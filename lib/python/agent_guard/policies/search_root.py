"""Searches rooted at a huge tree: Claude's Grep and Glob, and shell grep -r,
rg, ugrep, find, fd and bfs, whose root is /, $HOME, ~/My Drive, /Users,
/Volumes or another root in HUGE_ROOTS.

On 2026-10-06 agent sessions' searches read the disk at about 2 GB/s and
pushed the load average to ~150; several were rooted at / or ~/My Drive (16,500
PDFs in library-pdf alone). Claude Code's `find` and `grep` are its embedded
bfs and ugrep, so a shell search is the usual form. A search inside a specific
subdirectory (~/My Drive/dotfiles, ~/repos/x) is allowed, and so is a shallow
one (a depth limit of at most MAX_SHALLOW_DEPTH).

Roots are compared after expanding ~ and $HOME, resolving a relative path
against the working directory (the call's cwd, a Codex workdir, or an earlier
`cd` in the same command) and resolving symlinks and `..`. A root that is an
ancestor of a huge root (~/Library contains ~/Library/CloudStorage) is huge
too. A root the guard cannot resolve statically (another variable, a command
substitution, a glob in a middle component) is allowed: this guards against
accidental full-disk scans, not deliberate evasion, and a command the shell
tokenizer rejects is likewise allowed.

session_search_cleanup.py terminates a session's leftover searches
(SEARCH_TOOLS) when the session ends.
"""

from __future__ import annotations

import os

from ..codex_payload import (PartialOutput, _extract_substitutions, _interpreter_command_index, _is_assignment,
                             _split_segments, _tokenize, jq_alternative, jq_text, nested_exec_contexts)
from ..core import CLAUDE, Call, Result
from ..shell_text import mask_heredoc_bodies

# The one list of huge roots. `~` is the call's $HOME.
HUGE_ROOTS = ("/", "/Users", "/Volumes", "/System/Volumes/Data", "~", "~/My Drive", "~/Library/CloudStorage")
# find -maxdepth, fd/rg --max-depth and ugrep --depth at or below this stay allowed at a huge root.
MAX_SHALLOW_DEPTH = 2
# Process names (ps comm) of the search tools a session-end cleanup terminates.
SEARCH_TOOLS = frozenset({"bfs", "find", "fd", "fdfind", "rg", "ugrep", "ug", "grep", "egrep", "fgrep", "ggrep"})

_FIND = frozenset({"find", "gfind", "bfs"})
_GREP = frozenset({"grep", "egrep", "fgrep", "ggrep", "ugrep", "ug"})
_RG = frozenset({"rg"})
_FD = frozenset({"fd", "fdfind"})
_SHELLS = frozenset({"sh", "bash", "zsh", "dash", "ksh"})
_PREFIXES = frozenset({"command", "exec", "builtin", "nohup", "noglob", "time", "sudo", "caffeinate"})
_KEYWORDS = frozenset({"{", "}", "!", "if", "then", "elif", "else", "fi", "while", "until", "do", "done"})
_GLOB = frozenset("*?[")
_CONTEXT_KEYS = ("workdir", "cwd", "working_directory", "working_dir")

# Short options that take a value, per tool; a value may be attached (-m5) or the next word.
_SHORT_VALUES = {
    "grep": "ABCDdefm",
    "ugrep": "ABCDdefgJKmMNOt",
    "rg": "ABCdEefgjMmrTt",
    "fd": "cdEeojSt",
}
_LONG_VALUES = {
    "grep": {"regexp", "file", "after-context", "before-context", "context", "max-count", "include", "exclude",
             "include-dir", "exclude-dir", "exclude-from", "directories", "devices", "label", "binary-files",
             "group-separator"},
    "ugrep": {"regexp", "file", "after-context", "before-context", "context", "max-count", "max-files", "include",
              "exclude", "include-dir", "exclude-dir", "include-from", "exclude-from", "include-fs", "exclude-fs",
              "directories", "devices", "label", "binary-files", "glob", "iglob", "file-extension", "file-type",
              "file-magic", "jobs", "encoding", "format", "group-separator", "depth", "config"},
    "rg": {"regexp", "file", "glob", "iglob", "type", "type-not", "type-add", "type-clear", "max-count",
           "after-context", "before-context", "context", "encoding", "threads", "max-columns", "replace",
           "max-depth", "maxdepth", "max-filesize", "pre", "pre-glob", "sort", "sortr", "colors", "color",
           "context-separator", "field-context-separator", "field-match-separator", "path-separator",
           "dfa-size-limit", "regex-size-limit", "engine", "ignore-file", "hostname-bin", "hyperlink-format",
           "generate"},
    "fd": {"max-depth", "maxdepth", "min-depth", "exact-depth", "exclude", "extension", "type", "color", "threads",
           "size", "changed-within", "changed-before", "change-newer-than", "change-older-than", "newer", "older",
           "owner", "max-results", "base-directory", "search-path", "path-separator", "batch-size", "ignore-file",
           "format", "and"},
}
_ADVICE = ("Name a narrower directory as the search root: the project or subdirectory that holds what you are "
           "looking for (e.g. ~/My Drive/dotfiles, ~/repos/<name>), or cd there first. A shallow search is allowed "
           f"at these roots (find -maxdepth {MAX_SHALLOW_DEPTH}, fd/rg --max-depth {MAX_SHALLOW_DEPTH}). "
           f"The huge roots are configured in HUGE_ROOTS ({__name__}).")


def _home(call: Call) -> str:
    return call.env.get("HOME") or os.path.expanduser("~")


def huge_roots(home: str) -> list[tuple[str, str]]:
    """(configured spelling, resolved path) for each root in HUGE_ROOTS."""
    return [(root, os.path.realpath(_expand_home(root, home))) for root in HUGE_ROOTS]


def _expand_home(path: str, home: str) -> str:
    for prefix in ("${HOME}", "$HOME", "~"):
        if path == prefix or path.startswith(prefix + "/"):
            return home + path[len(prefix):]
    return path


def resolve(path: str, cwd: str | None, home: str) -> str | None:
    """PATH as an absolute, symlink-free directory, or None when it cannot be
    resolved statically. A glob in the last component searches its parent."""
    if not path or "__CODEX_SUB_" in path:
        return None
    path = _expand_home(path, home)
    if "$" in path or "`" in path or path.startswith("~"):
        return None
    head, _, last = path.rstrip("/").rpartition("/") if path.rstrip("/") else ("", "", "")
    if _GLOB & set(last):
        path = head or ("/" if path.startswith("/") else ".")
    if _GLOB & set(path):
        return None
    if not path.startswith("/"):
        if cwd is None:
            return None
        path = os.path.join(cwd, path)
    return os.path.realpath(path)


def matched_root(resolved: str, home: str) -> str | None:
    """The configured huge root that RESOLVED is, or is an ancestor of."""
    prefix = resolved.rstrip("/") + "/"
    for spelling, root in huge_roots(home):
        if resolved == root or root.startswith(prefix):
            return spelling
    return None


def _depth(value: str | None) -> int | None:
    """A numeric depth limit; ugrep's --depth=MIN,MAX keeps MAX."""
    if value is None:
        return None
    value = value.rpartition(",")[2]
    return int(value) if value.isdigit() else None


class _Parse:
    """The paths, recursion and depth limit of one search command's arguments."""

    def __init__(self):
        self.paths: list[str] = []
        self.recursive = False
        self.depth: int | None = None
        self.default_cwd = True    # no path operand searches the working directory
        self.searches = True       # False for --help, --version, --type-list


def _options(args: list[str], family: str, on_option, positional) -> None:
    """Walk ARGS with getopt-style permutation: options anywhere before `--`,
    short clusters, attached or separate values. ON_OPTION(name, value)
    returns True to stop the walk."""
    short_values, long_values = _SHORT_VALUES[family], _LONG_VALUES[family]
    pos = 0
    while pos < len(args):
        word = args[pos]
        pos += 1
        if word == "--":
            for rest in args[pos:]:
                positional(rest)
            return
        if word.startswith("--"):
            name, eq, value = word[2:].partition("=")
            if not eq and name in long_values and pos < len(args):
                value, pos = args[pos], pos + 1
            if on_option("--" + name, value if (eq or name in long_values) else None):
                return
        elif word.startswith("-") and len(word) > 1:
            if word[1:].isdigit():
                if on_option(word, None):
                    return
                continue
            for index, char in enumerate(word[1:], start=1):
                if char in short_values:
                    value = word[index + 1:]
                    if not value and pos < len(args):
                        value, pos = args[pos], pos + 1
                    if on_option("-" + char, value):
                        return
                    break
                if on_option("-" + char, None):
                    return
        else:
            positional(word)


def _parse_grep(tool: str, args: list[str]) -> _Parse:
    family = "ugrep" if tool in ("ugrep", "ug") else "grep"
    parse, positionals, patterns = _Parse(), [], []

    def on_option(name, value):
        if name in ("-r", "-R", "--recursive", "--dereference-recursive"):
            parse.recursive = True
        elif name in ("-d", "--directories") and value in ("recurse", "recurse-dir"):
            parse.recursive = True
        elif name in ("-e", "-f", "--regexp", "--file"):
            patterns.append(value)
        elif family == "ugrep" and (name == "--depth" or (name[1:].isdigit() and len(name) == 2)):
            parse.depth = _depth(value if name == "--depth" else name[1:])
        elif name in ("--help", "--version", "-V"):
            parse.searches = False
        return False

    _options(args, family, on_option, positionals.append)
    parse.paths = positionals if patterns else positionals[1:]
    return parse


def _parse_rg(args: list[str]) -> _Parse:
    parse, positionals, patterns = _Parse(), [], []
    parse.recursive = True
    files_mode = False

    def on_option(name, value):
        nonlocal files_mode
        if name in ("-e", "-f", "--regexp", "--file"):
            patterns.append(value)
        elif name in ("-d", "--max-depth", "--maxdepth"):
            parse.depth = _depth(value)
        elif name == "--files":
            files_mode = True
        elif name in ("--help", "-h", "--version", "-V", "--type-list", "--generate"):
            parse.searches = False
        return False

    _options(args, "rg", on_option, positionals.append)
    parse.paths = positionals if (patterns or files_mode) else positionals[1:]
    return parse


def _parse_fd(args: list[str]) -> _Parse:
    parse, positionals, search_paths = _Parse(), [], []
    parse.recursive = True
    base = None

    def on_option(name, value):
        nonlocal base
        if name in ("-d", "--max-depth", "--maxdepth", "--exact-depth"):
            parse.depth = _depth(value)
        elif name == "--search-path":
            search_paths.append(value)
        elif name == "--base-directory":
            base = value
        elif name in ("-x", "-X", "--exec", "--exec-batch"):
            return True    # the rest of the line is the command to run
        elif name in ("--help", "-h", "--version", "-V"):
            parse.searches = False
        return False

    _options(args, "fd", on_option, positionals.append)
    parse.paths = search_paths or positionals[1:]
    if base is not None:
        parse.paths = [os.path.join(base, path) for path in parse.paths] or [base]
    return parse


def _parse_find(args: list[str]) -> _Parse:
    parse = _Parse()
    parse.recursive = True
    pos = 0
    while pos < len(args) and args[pos].startswith("-"):
        word = args[pos]
        if word == "-f" and pos + 1 < len(args):    # BSD: -f PATH
            parse.paths.append(args[pos + 1])
            pos += 2
        elif word in ("-D", "-S") and pos + 1 < len(args):    # GNU/bfs debug, bfs strategy
            pos += 2
        elif word in ("-H", "-L", "-P", "-E", "-X", "-d", "-s", "-x") or word[:2] in ("-O", "-j"):
            pos += 1
        else:
            break
    while pos < len(args) and not args[pos].startswith("-") and args[pos] not in ("!", "(", ")", ","):
        parse.paths.append(args[pos])
        pos += 1
    rest = args[pos:]
    for index, word in enumerate(rest[:-1]):
        if word == "-maxdepth":
            parse.depth = _depth(rest[index + 1])
    if rest[:1] in (["--help"], ["--version"], ["-help"], ["-version"]):
        parse.searches = False
    return parse


def _parse(tool: str, args: list[str]) -> _Parse | None:
    if tool in _FIND:
        return _parse_find(args)
    if tool in _GREP:
        return _parse_grep(tool, args)
    if tool in _RG:
        return _parse_rg(args)
    if tool in _FD:
        return _parse_fd(args)
    return None


def _cd_target(words: list[str], home: str) -> str:
    args = [word for word in words[1:] if word not in ("-L", "-P", "-e", "-@", "--")]
    return args[0] if args else home


def _command_start(words: list[str]) -> int:
    """Index of the command word after assignments and prefix commands."""
    pos = 0
    while pos < len(words):
        word = words[pos]
        name = os.path.basename(word)
        if _is_assignment(word) or word in _KEYWORDS:
            pos += 1
        elif name in _PREFIXES:
            pos += 1
            while pos < len(words) and words[pos].startswith("-"):
                pos += 1
        elif name in ("env", "nice", "timeout", "gtimeout"):
            pos += 1
            while pos < len(words) and (words[pos].startswith("-") or _is_assignment(words[pos])):
                takes_value = words[pos] in ("-u", "-n", "-s", "-k", "-S", "-C", "--signal", "--kill-after")
                pos += 2 if takes_value else 1
            if name in ("timeout", "gtimeout") and pos < len(words):
                pos += 1    # the duration
        else:
            return pos
    return pos


def shell_findings(command: str, cwd: str | None, home: str, depth: int = 0) -> list[tuple[str, str, str, str]]:
    """(tool, root as written, resolved root, matched huge root) for each
    search in COMMAND rooted at a huge tree."""
    if depth > 4 or not command:
        return []
    command = mask_heredoc_bodies(command.replace("\\\n", ""))
    masked, substitutions, invalid = _extract_substitutions(command)
    if invalid:
        return []
    findings = []
    for nested in substitutions:
        findings += shell_findings(nested, cwd, home, depth + 1)
    previous = None
    for words, terminator in _split_segments(_tokenize(masked)):
        piped = previous == "|"
        previous = terminator
        pos = _command_start(words)
        if pos >= len(words):
            continue
        tool = os.path.basename(words[pos].lstrip("\\"))
        args = words[pos + 1:]
        if tool in ("cd", "pushd"):
            target = _cd_target(words[pos:], home)
            cwd = resolve(target, cwd, home) if target != "-" else None
            continue
        if tool in _SHELLS:
            index = _interpreter_command_index(words[pos:])
            if index is not None and pos + index < len(words):
                findings += shell_findings(words[pos + index], cwd, home, depth + 1)
            continue
        parse = _parse(tool, args)
        if parse is None or not parse.searches or not parse.recursive:
            continue
        if parse.depth is not None and parse.depth <= MAX_SHALLOW_DEPTH:
            continue
        paths = parse.paths
        if not paths:
            if piped and tool not in _FIND and tool not in _FD:
                continue    # grep -r and rg read the pipe, not the directory
            paths = [""]
        for path in paths:
            resolved = resolve(path or ".", cwd, home)
            root = matched_root(resolved, home) if resolved else None
            if root:
                findings.append((tool, path, resolved, root))
    return findings


def _describe(tool: str, written: str, resolved: str, root: str) -> str:
    if not written:
        shown = f"its working directory {resolved}"
    else:
        shown = written if written == resolved else f"{written} (resolves to {resolved})"
    return f"`{tool}` searches {shown}, which is or contains the huge root {root}."


def _deny(lines: list[str]) -> Result:
    return Result.deny("BLOCKED: search rooted at a huge tree. " + " ".join(dict.fromkeys(lines)) + "\n\n" + _ADVICE)


def _field(call: Call, key: str) -> str:
    return jq_text(jq_alternative(call.tool_input.get(key)))


def _tool_findings(call: Call, home: str) -> list[str]:
    """Claude's Grep and Glob: the `path` parameter, else the session cwd;
    an absolute Glob pattern supplies its own root."""
    path = _field(call, "path")
    if call.tool_name == "Glob":
        pattern = _field(call, "pattern")
        if "**" not in pattern:
            return []    # a pattern without ** has a fixed depth
        if pattern.startswith(("/", "~", "$HOME")):
            path = pattern.split("**", 1)[0]
            path = path if not _GLOB & set(path) else path[:min(path.find(c) for c in _GLOB if c in path)]
    resolved = resolve(path or ".", call.cwd, home)
    root = matched_root(resolved, home) if resolved else None
    return [_describe(call.tool_name, path, resolved, root)] if root else []


def _shell_contexts(call: Call) -> list[tuple[str, str]]:
    """(command, working directory) pairs: the shell command, or each literal
    exec_command nested in a Codex functions.exec program."""
    tool_input = call.tool_input
    workdir = jq_text(jq_alternative(*(tool_input.get(key) for key in _CONTEXT_KEYS))) or call.cwd
    command = call.shell_command()
    if call.runtime != CLAUDE and call.tool_name == "functions.exec":
        try:
            contexts = nested_exec_contexts(command)
        except PartialOutput as partial:
            contexts = partial.values
        return [(context["cmd"], context["workdir"] if isinstance(context.get("workdir"), str) else workdir)
                for context in contexts if isinstance(context.get("cmd"), str)]
    return [(command, workdir)]


def decide(call: Call) -> Result:
    home = _home(call)
    if call.runtime == CLAUDE and call.tool_name in ("Grep", "Glob"):
        lines = _tool_findings(call, home)
    elif call.is_shell:
        lines = []
        for command, workdir in _shell_contexts(call):
            cwd = resolve(workdir, None, home) if workdir else None
            lines += [_describe(*finding) for finding in shell_findings(command, cwd, home)]
    else:
        return Result.allow()
    return _deny(lines) if lines else Result.allow()
