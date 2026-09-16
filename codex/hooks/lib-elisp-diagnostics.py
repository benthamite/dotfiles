#!/usr/bin/env python3
"""Recognize literal commands that may run while a live check is pending.

Two kinds qualify without clearing the debt: read-only inspection, and the
`git stash` operations that move uncommitted work aside and back.  A live
check certifies committed code, so it needs a tree whose Elisp is clean;
stashing reaches that state without executing anything.
"""

from __future__ import annotations

import base64
import re
import shlex
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
RECOVERY_TESTS = frozenset(str(ROOT / "tests" / name) for name in (
    "test_elpaca_rebuild_protocol.py", "test_elisp_live_diagnostics.py",
))
PACKAGE = r"[a-z][a-z0-9-]*"
TOKEN = r"[A-Za-z0-9][A-Za-z0-9_.:-]*"
STATUS_QUERY = (
    r'\(elpaca-extras-build-reload-status "' + TOKEN + r'"\)'
)
REGISTRY_QUERY = (
    r'\(let \(rows\) \(maphash \(lambda \(token status\) '
    r'\(when \(eq \(plist-get status :package\) \(quote ' + PACKAGE + r'\)\) '
    r'\(push \(list token \(plist-get status :state\)\) rows\)\)\) '
    r'elpaca-extras--build-reload-statuses\) \(seq-take rows 10\)\)'
)


def literal_options(args, flags, values, required=frozenset()):
    """Consume option values before counting required switches; reject unknowns."""
    seen = set()
    i = 0
    while i < len(args):
        arg = args[i]
        if arg == "--":
            break
        if not arg.startswith("-") or arg == "-":
            i += 1
            continue
        if arg in flags:
            seen.add(arg)
        elif arg in values:
            i += 1
            if i >= len(args):
                return False
        elif "=" in arg and arg.partition("=")[0] in values:
            pass
        elif re.fullmatch(r"-[0-9]+", arg):
            pass
        else:
            return False
        i += 1
    return required <= seen


def nested_inspection_source(source):
    """Require every tools reference to be a direct, exactly spelled call.

    Strings/comments are data; templates and escaped identifiers are unsupported.
    The existing nested parser separately validates each literal command object.
    """
    tokens = []
    i = 0
    while i < len(source):
        char = source[i]
        if char.isspace():
            i += 1
        elif source.startswith("//", i):
            end = source.find("\n", i + 2)
            i = len(source) if end < 0 else end + 1
        elif source.startswith("/*", i):
            end = source.find("*/", i + 2)
            if end < 0:
                return False
            i = end + 2
        elif char in "\"'":
            quote = char
            i += 1
            while i < len(source) and source[i] != quote:
                if source[i] in "\n\r":
                    return False
                i += 2 if source[i] == "\\" else 1
            if i >= len(source):
                return False
            i += 1
            tokens.append(("STRING", None))
        elif char in "`\\/":
            return False
        elif char.isalpha() or char in "_$":
            start = i
            i += 1
            while i < len(source) and (source[i].isalnum() or source[i] in "_$"):
                i += 1
            tokens.append((source[start:i], start))
        else:
            tokens.append((char, i))
            i += 1
    found = False
    for index, (token, start) in enumerate(tokens):
        if token in {"globalThis", "eval", "Function", "import", "constructor"}:
            return False
        if token != "tools":
            continue
        if index and tokens[index - 1][0] in {".", "?"}:
            return False
        if (not source.startswith("tools.exec_command", start)
                or [item[0] for item in tokens[index + 1:index + 4]]
                != [".", "exec_command", "("]):
            return False
        found = True
    return found


STASH_OPERATIONS = frozenset({"push", "pop", "apply", "drop", "list", "show"})


def stash_command(args) -> bool:
    """Accept literal `git stash` forms that move work aside or back.

    Interactive (`--patch`), branch-creating and ref-naming forms stay out:
    only the operations needed to reach a clean tree and restore it qualify.
    """
    operation, rest = (args[0], args[1:]) if args and not args[0].startswith("-") else ("push", args)
    if operation not in STASH_OPERATIONS:
        return False
    return literal_options(rest,
        {"-u", "--include-untracked", "-k", "--keep-index", "--no-keep-index",
         "-S", "--staged", "-q", "--quiet", "--index", "-a", "--all", "--stat"},
        {"-m", "--message", "--pathspec-from-file"})


def inspection_command(command: str) -> bool:
    """Accept a closed set of single, literal command forms.

    Read-only diagnostics qualify, and so do the `git stash` operations
    checked by `stash_command`.
    """
    command = command.strip()
    if not command or any(char in command for char in "\n\r$`"):
        return False
    quote = None
    for char in command:
        if quote:
            if char == quote:
                quote = None
        elif char in "\"'":
            quote = char
        elif char in "*?[]{}~\\":
            return False
    try:
        lexer = shlex.shlex(command, posix=True, punctuation_chars=";&|<>()")
        lexer.whitespace_split = True
        lexer.commenters = ""
        words = list(lexer)
    except ValueError:
        return False
    if not words or any(re.fullmatch(r"[;&|<>()]+", word) for word in words):
        return False
    executable, *args = words
    program = Path(executable).name
    if "/" in executable and str(Path(executable).parent) not in {
        "/bin", "/usr/bin", "/usr/local/bin", "/opt/homebrew/bin",
    }:
        return False
    if program in {"cat", "head", "tail", "wc", "ls", "stat", "ps", "pgrep"}:
        return True
    if program in {"grep", "egrep", "fgrep"}:
        return True
    # These recovery suites use synthetic processes/registries and temporary
    # state. Isolate Python startup, forbid extra arguments, and retain debt.
    if program == "python3":
        return (len(args) == 3 and args[:2] == ["-I", "-B"]
                and args[2] in RECOVERY_TESTS)
    if program == "emacsclient":
        return (len(args) == 2 and args[0] == "-e"
                and any(re.fullmatch(pattern, args[1])
                        for pattern in (STATUS_QUERY, REGISTRY_QUERY)))
    if program == "rg":
        return literal_options(args,
            {"--no-config", "-n", "--line-number", "-i", "--ignore-case",
             "-l", "--files-with-matches", "--files", "--hidden", "--no-ignore",
             "-F", "--fixed-strings", "-w", "--word-regexp", "-c", "--count",
             "--count-matches", "-o", "--only-matching", "--heading", "--no-heading",
             "--json", "--stats", "--with-filename", "-H", "--no-filename", "-h"},
            {"-e", "--regexp", "-f", "--file", "-g", "--glob", "--iglob", "-t",
             "--type", "-T", "--type-not", "-A", "--after-context", "-B",
             "--before-context", "-C", "--context", "-m", "--max-count",
             "--max-depth", "--color", "--colors", "--sort", "--sortr"},
            {"--no-config"})
    if program == "sed":
        return (len(args) >= 3 and args[0] == "-n"
                and re.fullmatch(r"[0-9]+(?:,(?:[0-9]+|\$))?p", args[1]) is not None
                and all(not arg.startswith("-") for arg in args[2:]))
    if program != "git":
        return False
    globals_seen = set()
    while args and args[0] in {"--no-pager", "--no-optional-locks", "-C", "-c"}:
        option = args.pop(0)
        if option in {"-C", "-c"}:
            if not args:
                return False
            value = args.pop(0)
            if option == "-c":
                if value != "core.fsmonitor=false":
                    return False
                globals_seen.add(value)
        else:
            globals_seen.add(option)
    if not args:
        return False
    subcommand, *args = args
    if subcommand == "stash":
        return stash_command(args)
    if "--no-pager" not in globals_seen:
        return False
    if subcommand in {"status", "ls-files", "diff"} and not {
            "--no-optional-locks", "core.fsmonitor=false"} <= globals_seen:
        return False
    if subcommand in {"status", "ls-files", "rev-parse", "ls-tree"}:
        return literal_options(args,
            {"--short", "--porcelain", "--branch", "-s", "-b", "-z", "--cached",
             "--modified", "--others", "--exclude-standard", "--stage", "--unmerged",
             "--deleted", "--error-unmatch", "--full-name", "--show-toplevel",
             "--show-prefix", "--git-dir", "--show-cdup", "--verify", "--quiet",
             "--is-inside-work-tree", "--abbrev-ref", "--symbolic-full-name",
             "--name-only", "--name-status", "-r", "-t", "--full-tree"},
            {"--untracked-files", "--ignore-submodules", "--format"})
    if subcommand in {"diff", "log", "show"}:
        return literal_options(args,
            {"--no-ext-diff", "--no-textconv", "--stat", "--numstat", "--shortstat",
             "--name-only", "--name-status", "--oneline", "--summary", "--check",
             "--cached", "--staged", "--raw", "--patch", "-p", "-u", "--exit-code",
             "--quiet", "--reverse", "--no-merges", "--merges", "--first-parent",
             "--all", "--full-history", "--follow", "--relative", "--no-renames",
             "--color", "--no-color", "--decorate", "--no-decorate"},
            {"-n", "--max-count", "--since", "--until", "--author", "--grep",
             "--format", "--pretty", "--date", "--unified", "-U"},
            {"--no-ext-diff", "--no-textconv"})
    return False


def pending_description(path: str) -> str:
    """Describe validated pending identities without interpreting their text."""
    rows = []
    try:
        for line in Path(path).read_text().splitlines():
            encoded_repo, commit, encoded_label = line.split(":")
            repo = base64.b64decode(encoded_repo, validate=True).decode()
            label = base64.b64decode(encoded_label, validate=True).decode()
            if (not re.fullmatch(r"[0-9a-f]{40,64}", commit)
                    or not repo.startswith("/") or not label
                    or any(char in repo + label for char in "\r\n\0")):
                raise ValueError("invalid pending identity")
            rows.append(f"  {label} at {commit[:12]} in {repo}")
    except (OSError, ValueError, UnicodeError):
        return f"Pending verification record needs inspection: {path}"
    return "Pending live checks:\n" + "\n".join(rows) if rows else "Pending verification record is empty."


if __name__ == "__main__":
    if len(sys.argv) == 3 and sys.argv[1] == "--pending":
        print(pending_description(sys.argv[2]))
    elif len(sys.argv) == 2 and sys.argv[1] == "--nested":
        raise SystemExit(0 if nested_inspection_source(sys.stdin.read()) else 1)
    elif len(sys.argv) == 1:
        raise SystemExit(0 if inspection_command(sys.stdin.read()) else 1)
    else:
        raise SystemExit(2)
