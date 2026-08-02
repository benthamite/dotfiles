#!/usr/bin/env python3
"""Conservative shell-command parser/policy for the Drive runtime-state guard.

This module is the SINGLE parser behind the paired PreToolUse guards
claude/hooks/block-drive-runtime-command.sh and
codex/hooks/block-drive-runtime-command.sh. The guards do no shell parsing of
their own; they invoke this module as

    python3 -I lib/drive_runtime_command.py --cwd CWD --command COMMAND

and consume a JSON array of execution segments. Each segment carries a
``verdict`` of ``allow``, ``deny``, or ``indeterminate`` plus a human-readable
``rule``. The guard denies unless every segment's verdict is ``allow``.

Policy (fail-closed):

- State-creating commands — npm ci/install/run build|dev|test, uv sync /
  project-bound uv run, python -m venv, every pip install form, Python without
  bytecode prevention, pytest without bytecode AND cache prevention, git
  worktree add — are DENIED whenever their effective project, cwd, or explicit
  target lies under ~/My Drive.
- The parser tracks ``cd`` and subshell cwd scopes, strips only syntactically
  valid environment prefixes, and resolves ``git -C``, ``npm --prefix``,
  ``uv --project``, worktree destinations, and explicit path arguments.
- Grammar the module does not model — unterminated quotes, here-documents,
  command/process substitution, shell functions, shell keywords, redirections,
  expansions, multi-line commands, unsupported operators — yields a single
  ``indeterminate`` segment, never a partial parse. Indeterminate is denied by
  the guard; there is no silent allow fallback.

Python 3.11 stdlib only.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shlex
import sys
from pathlib import Path

HOME = str(Path.home())
DRIVE_ROOT = os.path.join(HOME, "My Drive")

PUNCTUATION = ";&|()"
# Longest-match first. ";;" is case syntax and unsupported; "|&" and "&|" are
# treated as plain pipes (stderr routing does not change targets).
KNOWN_OPERATORS = ("&&", "||", ";;", "|&", "&|", "(", ")", ";", "|", "&")
SEPARATORS = {"&&", "||", ";", "|", "&", "|&", "&|"}

ASSIGNMENT_RE = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*=")
PYTHON_RE = re.compile(r"^python([0-9]+(\.[0-9]+)*)?$")
PIP_RE = re.compile(r"^pip[0-9.]*$")

# Substitution / heredoc markers anywhere in the raw command → indeterminate.
RAW_MARKERS = ("<<", "$(", "<(", ">(", "`")
# Tokens containing these characters carry expansions or redirections the
# module does not model. shlex strips quotes, so a quoted literal "$" is also
# rejected — conservative by design.
FORBIDDEN_TOKEN_CHARS = ("$", "`", "<", ">")

SHELL_KEYWORDS = {
    "if", "then", "elif", "else", "fi", "while", "until", "do", "done",
    "for", "in", "case", "esac", "function", "{", "}", "!", "time",
    "coproc", "select", "[[", "]]",
}

# Wrappers that execute their arguments in ways the module does not model.
EXEC_WRAPPERS = {
    "bash", "sh", "zsh", "dash", "ksh", "fish", "eval", "xargs", "ssh",
    "sudo", "doas", "timeout", "gtimeout", "nice", "stdbuf", "setsid",
    "caffeinate", "script", "watch", "source", ".", "pushd", "popd",
}

# Transparent wrappers that run their remaining argv unchanged. "pyenv exec"
# is handled alongside these in _evaluate: only its exec subcommand is
# transparent (it runs the remaining argv through the selected interpreter
# unchanged); every other pyenv subcommand keeps unknown-command handling.
TRANSPARENT_WRAPPERS = {"command", "exec", "nohup"}

# Proven read-only (or at least never runtime-state-creating) commands whose
# arguments need no further inspection. Membership requires that the command
# has NO argv-level exec-delegation flag (a flag whose value is another
# command to run): find/fd are therefore NOT here — they are dispatched to
# _eval_find_fd, which allows them only when no delegation flag is present or
# every delegated command is itself benign. xargs/watch and similar wrappers
# are EXEC_WRAPPERS (indeterminate), and an unknown command whose arguments
# mention a state-creating word is indeterminate too, so this set is the only
# allow-shortcut and must stay delegation-free.
READ_ONLY_COMMANDS = {
    "echo", "printf", "ls", "cat", "head", "tail", "wc", "grep", "egrep",
    "fgrep", "rg", "ripgrep", "ag", "stat", "file", "which",
    "type", "test", "[", "true", "false", "pwd", "date", "sleep", "basename",
    "dirname", "realpath", "readlink", "ps", "pgrep", "diff", "cmp", "sort",
    "uniq", "cut", "tr", "column", "less", "more", "man", "du", "df",
    "uname", "hostname", "whoami", "id", "sed", "awk", "jq", "yq", "curl",
    "wget", "gh", "tee", "xattr", "sw_vers", "sysctl", "defaults", "mdfind",
    "open", "osascript", "say", "printenv", "tty", "uptime", "cal", "seq",
}

# Exec-delegation flags: the delegated tokens are a command line, not data.
FIND_EXEC_FLAGS = {"-exec", "-execdir", "-ok", "-okdir"}
FD_EXEC_FLAGS = {"-x", "--exec", "-X", "--exec-batch"}


def _segment(argv: list[str], cwd: str, verdict: str, rule: str) -> dict:
    return {"argv": argv, "cwd": cwd, "verdict": verdict, "rule": rule}


def _indeterminate(rule: str) -> list[dict]:
    return [{"argv": [], "cwd": "", "verdict": "indeterminate", "rule": rule}]


def _resolve(path: str, cwd: str) -> str | None:
    """Resolve PATH against CWD; None when it cannot be resolved safely."""
    if path.startswith("~"):
        expanded = os.path.expanduser(path)
        if expanded.startswith("~"):
            return None  # unknown ~user form
        path = expanded
    if not os.path.isabs(path):
        path = os.path.join(cwd, path)
    return os.path.normpath(path)


def _under_drive(path: str | None) -> bool:
    if path is None:
        return False
    for candidate in (os.path.normpath(path), os.path.realpath(path)):
        if candidate == DRIVE_ROOT or candidate.startswith(DRIVE_ROOT + os.sep):
            return True
    return False


def _looks_like_path(token: str) -> bool:
    return "/" in token or token.startswith("~") or token.endswith(".py")


def _split_operator_run(token: str) -> list[str] | None:
    """Split a run of punctuation characters into known operators."""
    ops: list[str] = []
    i = 0
    while i < len(token):
        for op in KNOWN_OPERATORS:
            if token.startswith(op, i):
                ops.append(op)
                i += len(op)
                break
        else:
            return None
    if ";;" in ops:
        return None
    return ops


def _tokenize(command: str) -> list[str] | None:
    lex = shlex.shlex(command, posix=True, punctuation_chars=PUNCTUATION)
    lex.whitespace_split = True
    lex.commenters = ""
    try:
        raw = list(lex)
    except ValueError:
        return None
    tokens: list[str] = []
    for tok in raw:
        if tok and all(ch in PUNCTUATION for ch in tok):
            split = _split_operator_run(tok)
            if split is None:
                return None
            tokens.extend(split)
        else:
            tokens.append(tok)
    return tokens


# ---------------------------------------------------------------------------
# Per-command policy
# ---------------------------------------------------------------------------


def _drive_deny(rule: str, argv: list[str], cwd: str) -> dict:
    return _segment(argv, cwd, "deny", rule)


def _allow(rule: str, argv: list[str], cwd: str) -> dict:
    return _segment(argv, cwd, "allow", rule)


def _indet_seg(rule: str, argv: list[str], cwd: str) -> dict:
    return _segment(argv, cwd, "indeterminate", rule)


def _bytecode_safe(flags: list[str], env: dict[str, str]) -> bool:
    if env.get("PYTHONDONTWRITEBYTECODE", "") not in ("", "0"):
        return True
    for flag in flags:
        if flag.startswith("-") and not flag.startswith("--") and "B" in flag[1:]:
            return True
    return False


def _pytest_cache_safe(args: list[str], env: dict[str, str]) -> bool:
    if "no:cacheprovider" in env.get("PYTEST_ADDOPTS", ""):
        return True
    for i, arg in enumerate(args):
        if arg == "-p" and i + 1 < len(args) and args[i + 1] == "no:cacheprovider":
            return True
        if arg == "-pno:cacheprovider":
            return True
    return False


def _path_flag_values(
    args: list[str], flags_with_value: set[str], flag_prefixes: set[str]
) -> list[str]:
    values: list[str] = []
    i = 0
    while i < len(args):
        arg = args[i]
        if arg in flags_with_value and i + 1 < len(args):
            values.append(args[i + 1])
            i += 2
            continue
        for prefix in flag_prefixes:
            if arg.startswith(prefix + "="):
                values.append(arg[len(prefix) + 1 :])
                break
        i += 1
    return values


def _eval_npm(argv: list[str], cwd: str) -> dict:
    project = cwd
    rest: list[str] = []
    i = 1
    while i < len(argv):
        arg = argv[i]
        if arg == "--prefix" and i + 1 < len(argv):
            resolved = _resolve(argv[i + 1], cwd)
            if resolved is None:
                return _indet_seg("unresolvable npm --prefix", argv, cwd)
            project = resolved
            i += 2
            continue
        if arg.startswith("--prefix="):
            resolved = _resolve(arg[len("--prefix=") :], cwd)
            if resolved is None:
                return _indet_seg("unresolvable npm --prefix", argv, cwd)
            project = resolved
            i += 1
            continue
        rest.append(arg)
        i += 1
    positionals = [a for a in rest if not a.startswith("-")]
    sub = positionals[0] if positionals else ""
    state_subs = {"install", "i", "ci", "add", "update", "up", "install-ci-test",
                  "install-test", "link", "rebuild"}
    if sub in state_subs:
        if _under_drive(project):
            return _drive_deny("npm dependency state under Drive", argv, cwd)
        return _allow("npm state command with external project", argv, cwd)
    if sub in ("run", "run-script") and len(positionals) > 1 and positionals[1] in (
        "build",
        "dev",
        "test",
    ):
        if _under_drive(project):
            return _drive_deny("npm build/dev/test state under Drive", argv, cwd)
        return _allow("npm run with external project", argv, cwd)
    return _allow("npm non-state subcommand", argv, cwd)


def _eval_pip(argv: list[str], cwd: str, sub_args: list[str] | None = None) -> dict:
    args = sub_args if sub_args is not None else argv[1:]
    positionals = [a for a in args if not a.startswith("-")]
    if not positionals or positionals[0] != "install":
        return _allow("pip non-install subcommand", argv, cwd)
    if _under_drive(cwd):
        return _drive_deny("pip install with Drive cwd", argv, cwd)
    targets = _path_flag_values(
        args,
        {"--target", "-t", "--prefix", "--root", "--src", "-e", "--editable"},
        {"--target", "--prefix", "--root", "--src", "--editable"},
    )
    install_args = positionals[1:]
    targets.extend(a for a in install_args if _looks_like_path(a))
    for target in targets:
        resolved = _resolve(target, cwd)
        if resolved is None:
            return _indet_seg("unresolvable pip install target", argv, cwd)
        if _under_drive(resolved):
            return _drive_deny("pip install target under Drive", argv, cwd)
    return _allow("pip install with external project and targets", argv, cwd)


def _eval_venv_target(
    argv: list[str], cwd: str, args: list[str], default: str | None
) -> dict:
    value_flags = {"--prompt", "--python", "-p"}
    target: str | None = None
    i = 0
    while i < len(args):
        arg = args[i]
        if arg in value_flags and i + 1 < len(args):
            i += 2
            continue
        if arg.startswith("-"):
            i += 1
            continue
        target = arg
        break
    if target is None:
        target = default
    if target is None:
        return _allow("venv command without a destination", argv, cwd)
    resolved = _resolve(target, cwd)
    if resolved is None:
        return _indet_seg("unresolvable venv destination", argv, cwd)
    if _under_drive(resolved):
        return _drive_deny("virtualenv destination under Drive", argv, cwd)
    return _allow("venv with external destination", argv, cwd)


def _eval_pytest(
    argv: list[str], cwd: str, args: list[str], env: dict[str, str], bytecode: bool
) -> dict:
    safe = bytecode and _pytest_cache_safe(args, env)
    drive_involved = _under_drive(cwd)
    if not drive_involved:
        for arg in args:
            if not arg.startswith("-") and _looks_like_path(arg):
                resolved = _resolve(arg, cwd)
                if resolved is not None and _under_drive(resolved):
                    drive_involved = True
                    break
    if drive_involved and not safe:
        return _drive_deny(
            "pytest without both bytecode and cache prevention under Drive",
            argv,
            cwd,
        )
    return _allow("pytest", argv, cwd)


def _eval_python(argv: list[str], cwd: str, env: dict[str, str]) -> dict:
    args = argv[1:]
    flags: list[str] = []
    module: str | None = None
    code = False
    positionals: list[str] = []
    i = 0
    while i < len(args):
        arg = args[i]
        if module is None and not code and arg == "-m" and i + 1 < len(args):
            module = args[i + 1]
            positionals = args[i + 2 :]
            break
        if module is None and not code and arg.startswith("-m") and len(arg) > 2:
            module = arg[2:]
            positionals = args[i + 1 :]
            break
        if not code and arg == "-c":
            code = True
            positionals = args[i + 1 :]
            break
        if arg.startswith("-") and arg != "-":
            flags.append(arg)
            i += 1
            continue
        positionals = args[i:]
        break
    bytecode = _bytecode_safe(flags, env)
    if module == "venv":
        target_check = _eval_venv_target(argv, cwd, positionals, None)
        if target_check["verdict"] != "allow":
            return target_check
    elif module == "pip":
        pip_check = _eval_pip(argv, cwd, sub_args=positionals)
        if pip_check["verdict"] != "allow":
            return pip_check
    elif module == "pytest":
        return _eval_pytest(argv, cwd, positionals, env, bytecode)
    if bytecode:
        return _allow("python with bytecode prevention", argv, cwd)
    if not module and not code and not positionals:
        info_only = {"-V", "--version", "-h", "--help", "-VV"}
        if all(flag in info_only for flag in flags):
            return _allow("informational python invocation", argv, cwd)
    if "-" in args:
        return _indet_seg("python reading a script from stdin", argv, cwd)
    if _under_drive(cwd):
        return _drive_deny("python without bytecode prevention under Drive", argv, cwd)
    for arg in positionals:
        if _looks_like_path(arg):
            resolved = _resolve(arg, cwd)
            if resolved is not None and _under_drive(resolved):
                return _drive_deny(
                    "python without bytecode prevention on a Drive path", argv, cwd
                )
    return _allow("python outside Drive", argv, cwd)


def _eval_uv(argv: list[str], cwd: str, env: dict[str, str]) -> dict:
    project = cwd
    rest: list[str] = []
    i = 1
    while i < len(argv):
        arg = argv[i]
        if arg in ("--project", "--directory") and i + 1 < len(argv):
            resolved = _resolve(argv[i + 1], cwd)
            if resolved is None:
                return _indet_seg("unresolvable uv project", argv, cwd)
            project = resolved
            i += 2
            continue
        matched = False
        for prefix in ("--project", "--directory"):
            if arg.startswith(prefix + "="):
                resolved = _resolve(arg[len(prefix) + 1 :], cwd)
                if resolved is None:
                    return _indet_seg("unresolvable uv project", argv, cwd)
                project = resolved
                matched = True
                break
        if matched:
            i += 1
            continue
        rest.append(arg)
        i += 1
    positionals = [a for a in rest if not a.startswith("-")]
    sub = positionals[0] if positionals else ""
    if sub == "pip":
        sub_index = rest.index("pip")
        return _eval_pip(argv, project, sub_args=rest[sub_index + 1 :])
    if sub == "venv":
        sub_index = rest.index("venv")
        return _eval_venv_target(
            argv, project, rest[sub_index + 1 :], os.path.join(project, ".venv")
        )
    if sub in ("sync", "lock", "add", "remove", "run", "build", "tool"):
        if sub == "tool":
            return _allow("uv tool runs outside the project", argv, cwd)
        if _under_drive(project):
            return _drive_deny("uv project state under Drive", argv, cwd)
        return _allow("uv state command with external project", argv, cwd)
    return _allow("uv non-state subcommand", argv, cwd)


def _eval_git(argv: list[str], cwd: str) -> dict:
    base = cwd
    i = 1
    while i < len(argv):
        arg = argv[i]
        if arg == "-C" and i + 1 < len(argv):
            resolved = _resolve(argv[i + 1], base)
            if resolved is None:
                return _indet_seg("unresolvable git -C path", argv, cwd)
            base = resolved
            i += 2
            continue
        if arg == "-c" and i + 1 < len(argv):
            i += 2
            continue
        if arg in ("--no-pager", "-P", "-p", "--paginate"):
            i += 1
            continue
        if arg.startswith("-"):
            return _indet_seg("unsupported git global option", argv, cwd)
        break
    if i >= len(argv):
        return _allow("bare git", argv, cwd)
    if argv[i] != "worktree":
        return _allow("git non-worktree subcommand", argv, cwd)
    rest = argv[i + 1 :]
    if not rest or rest[0] != "add":
        return _allow("git worktree non-add subcommand", argv, cwd)
    args = rest[1:]
    value_flags = {"-b", "-B", "--reason", "--lock-reason", "--orphan"}
    path: str | None = None
    j = 0
    while j < len(args):
        arg = args[j]
        if arg in value_flags and j + 1 < len(args):
            j += 2
            continue
        if arg.startswith("-"):
            j += 1
            continue
        path = arg
        break
    if path is None:
        return _indet_seg("git worktree add without a destination", argv, cwd)
    resolved = _resolve(path, base)
    if resolved is None:
        return _indet_seg("unresolvable git worktree destination", argv, cwd)
    if _under_drive(resolved):
        return _drive_deny("git worktree destination under Drive", argv, cwd)
    return _allow("git worktree add with external destination", argv, cwd)


def _mentions_state_command(args: list[str]) -> bool:
    for arg in args:
        name = os.path.basename(arg)
        if name in ("npm", "uv", "pytest", "virtualenv", "npx"):
            return True
        if PYTHON_RE.match(name) or PIP_RE.match(name):
            return True
        if arg == "worktree":
            return True
    return False


def _delegated_command_verdict(
    delegated: list[str], cwds: list[str], env: dict[str, str], argv: list[str]
) -> dict | None:
    """Evaluate an exec-delegated command line against every candidate cwd.

    Returns a non-allow segment to propagate, or None when the delegation is
    benign. Placeholder tokens ({}, {/}, ...) cannot be resolved, so a
    delegated command containing one is allowed only when it is a proven
    read-only command with no state-creating words; otherwise indeterminate.
    """
    if not delegated:
        return _indet_seg("exec delegation without a command", argv, cwds[0])
    if any("{" in a or "}" in a for a in delegated):
        name = os.path.basename(delegated[0])
        if name in READ_ONLY_COMMANDS and not _mentions_state_command(delegated[1:]):
            return None
        return _indet_seg(
            "placeholder in exec-delegated non-read-only command", argv, cwds[0]
        )
    for eff_cwd in cwds:
        seg = _evaluate(list(delegated), {"cwd": eff_cwd, "env": dict(env)})
        if seg["verdict"] != "allow":
            return _segment(
                argv, cwds[0], seg["verdict"], "exec delegation: " + seg["rule"]
            )
    return None


def _eval_find_fd(name: str, argv: list[str], cwd: str, env: dict[str, str]) -> dict:
    """find/fd: read-only unless an exec-delegation flag hands off a command."""
    is_find = name in ("find", "gfind")
    exec_flags = FIND_EXEC_FLAGS if is_find else FD_EXEC_FLAGS
    exec_indices = [i for i, a in enumerate(argv) if a in exec_flags]
    equals_forms = [
        i
        for i, a in enumerate(argv)
        if not is_find and (a.startswith("--exec=") or a.startswith("--exec-batch="))
    ]
    if not exec_indices and not equals_forms:
        return _allow("read-only command", argv, cwd)

    # Candidate cwds for the delegated command: the invoking cwd plus every
    # explicit path-looking search root (conservative: -execdir runs inside
    # matched directories, and even -exec against a Drive tree is treated as
    # Drive-targeting).
    first_flag = min(exec_indices + equals_forms)
    cwds = [cwd]
    for a in argv[1:first_flag]:
        if not a.startswith("-") and _looks_like_path(a):
            resolved = _resolve(a, cwd)
            if resolved is None:
                return _indet_seg("unresolvable search path", argv, cwd)
            cwds.append(resolved)

    for idx in exec_indices:
        if is_find:
            delegated: list[str] = []
            j = idx + 1
            while j < len(argv) and argv[j] not in (";", "+"):
                delegated.append(argv[j])
                j += 1
        else:
            # fd's exec command runs to the end of the invocation.
            delegated = [a for a in argv[idx + 1 :] if a not in (";", "+")]
        problem = _delegated_command_verdict(delegated, cwds, env, argv)
        if problem is not None:
            return problem
    for idx in equals_forms:
        flag_value = argv[idx].split("=", 1)[1]
        delegated = [flag_value] + [a for a in argv[idx + 1 :] if a not in (";", "+")]
        problem = _delegated_command_verdict(delegated, cwds, env, argv)
        if problem is not None:
            return problem
    return _allow("find/fd with benign exec delegation", argv, cwd)


def _evaluate(argv: list[str], scope: dict) -> dict:
    cwd = scope["cwd"]
    env: dict[str, str] = dict(scope["env"])

    # Environment prefix: strip only syntactically valid assignments.
    prefix_env: dict[str, str] = {}
    while argv and ASSIGNMENT_RE.match(argv[0]):
        name, _, value = argv[0].partition("=")
        prefix_env[name] = value
        argv = argv[1:]
    env.update(prefix_env)
    if not argv:
        scope["env"].update(prefix_env)
        return _allow("environment assignment", argv, cwd)

    if argv[0] == "export":
        rest = argv[1:]
        if rest and all(ASSIGNMENT_RE.match(a) for a in rest):
            for a in rest:
                name, _, value = a.partition("=")
                scope["env"][name] = value
            return _allow("export assignment", argv, cwd)
        return _indet_seg("unsupported export form", argv, cwd)

    while argv:
        base = os.path.basename(argv[0])
        if base in TRANSPARENT_WRAPPERS:
            if argv[0] == "command" and len(argv) > 1 and argv[1] in ("-v", "-V"):
                return _allow("command lookup", argv, cwd)
            argv = argv[1:]
            continue
        # ONLY "pyenv exec" is transparent; other pyenv subcommands fall
        # through to normal classification below.
        if base == "pyenv" and len(argv) > 1 and argv[1] == "exec":
            argv = argv[2:]
            continue
        break
    if not argv:
        return _allow("empty command", argv, cwd)

    if os.path.basename(argv[0]) == "env":
        argv = argv[1:]
        while argv and ASSIGNMENT_RE.match(argv[0]):
            name, _, value = argv[0].partition("=")
            env[name] = value
            argv = argv[1:]
        if argv and argv[0].startswith("-"):
            return _indet_seg("unsupported env option", argv, cwd)
        if not argv:
            return _allow("env listing", argv, cwd)

    name = os.path.basename(argv[0])

    if name in SHELL_KEYWORDS or argv[0] in SHELL_KEYWORDS:
        return _indet_seg("shell keyword", argv, cwd)
    if name in EXEC_WRAPPERS or argv[0] in EXEC_WRAPPERS:
        return _indet_seg("unmodeled execution wrapper", argv, cwd)

    if name == "cd":
        args = [a for a in argv[1:] if a not in ("-P", "-L", "-e", "-@")]
        if not args:
            scope["cwd"] = HOME
            return _allow("cd to home", argv, cwd)
        if len(args) > 1 or args[0].startswith("-"):
            return _indet_seg("unsupported cd form", argv, cwd)
        resolved = _resolve(args[0], cwd)
        if resolved is None:
            return _indet_seg("unresolvable cd target", argv, cwd)
        scope["cwd"] = resolved
        return _allow("cd", argv, cwd)

    if name == "npm":
        return _eval_npm(argv, cwd)
    if name == "npx":
        if _under_drive(cwd):
            return _drive_deny("npx execution with Drive cwd", argv, cwd)
        return _allow("npx outside Drive", argv, cwd)
    if name == "uv":
        return _eval_uv(argv, cwd, env)
    if PIP_RE.match(name) or name == "pipx":
        return _eval_pip(argv, cwd)
    if PYTHON_RE.match(name):
        return _eval_python(argv, cwd, env)
    if name == "pytest":
        return _eval_pytest(argv, cwd, argv[1:], env, _bytecode_safe([], env))
    if name == "virtualenv":
        return _eval_venv_target(argv, cwd, argv[1:], None)
    if name == "git":
        return _eval_git(argv, cwd)
    if name in ("find", "gfind", "fd", "fdfind"):
        return _eval_find_fd(name, argv, cwd, env)

    if name in READ_ONLY_COMMANDS:
        return _allow("read-only command", argv, cwd)
    if _mentions_state_command(argv[1:]):
        return _indet_seg("unknown command wrapping a state-creating word", argv, cwd)
    return _allow("command outside the state-creating classes", argv, cwd)


def parse_command(command: str, cwd: str) -> list[dict]:
    """Parse COMMAND (run from CWD) into policy-evaluated execution segments."""
    if "\n" in command or "\r" in command:
        return _indeterminate("multi-line command")
    for marker in RAW_MARKERS:
        if marker in command:
            return _indeterminate(f"unsupported construct: {marker}")
    tokens = _tokenize(command)
    if tokens is None:
        return _indeterminate("unterminated quote or unsupported operator")

    scopes: list[dict] = [{"cwd": os.path.normpath(cwd), "env": {}}]
    segments: list[dict] = []
    words: list[str] = []

    def flush() -> None:
        if words:
            segments.append(_evaluate(words.copy(), scopes[-1]))
            words.clear()

    for tok in tokens:
        if tok == "(":
            if words:
                return _indeterminate("function definition or unsupported '('")
            scopes.append(
                {"cwd": scopes[-1]["cwd"], "env": dict(scopes[-1]["env"])}
            )
        elif tok == ")":
            flush()
            if len(scopes) == 1:
                return _indeterminate("unbalanced parenthesis")
            scopes.pop()
        elif tok in SEPARATORS:
            flush()
        else:
            if any(ch in tok for ch in FORBIDDEN_TOKEN_CHARS):
                return _indeterminate(
                    "expansion, substitution, or redirection token"
                )
            words.append(tok)
    flush()
    if len(scopes) != 1:
        return _indeterminate("unbalanced parenthesis")
    if not segments:
        return [_allow("empty command", [], os.path.normpath(cwd))]
    # Never emit a partially parsed command: one indeterminate segment
    # collapses the whole parse to a single indeterminate result.
    for seg in segments:
        if seg["verdict"] == "indeterminate":
            return _indeterminate(seg["rule"])
    return segments


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cwd", required=True)
    parser.add_argument("--command", required=True)
    args = parser.parse_args(argv)
    print(json.dumps(parse_command(args.command, args.cwd)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
