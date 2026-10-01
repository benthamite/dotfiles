"""Repository and staged-file context of a commit command.

Port of lib-repo-root.sh and lib-staged-files.sh (claude/hooks and
codex/hooks). The two runtimes differ only in what the Codex copy could call
from lib-codex-hook-json.sh: a `functions.exec` parent's workdir and the
literal `git -C/--git-dir/--work-tree` options of the first commit, both
consulted only when RUNTIME is "codex".

The shell libraries ran in the hook process and `cd`'d into the repository.
Here relative paths still resolve against the process directory, and the
hook's later git commands run in ``RepoContext.cwd``.
"""

from __future__ import annotations

import json
import os
import subprocess
from dataclasses import dataclass, field

from . import codex_payload
from .codex_payload import bash_pwd, command_substitution, jq_alternative, jq_raw, jq_text
from .core import CODEX, DOTFILES, HookFailure
from .ere import ere, grep
from .shell_text import BASH_SPACE

# grep -E '(^|[[:space:]])--amend([[:space:]=]|$)' over the lines of `echo "$COMMAND"`.
_AMEND = ere(r"(^|[[:space:]])--amend([[:space:]=]|$)")
_CONTEXT_KEYS = ("workdir", "cwd", "working_directory", "working_dir")
_SELECTION_FAILED = "Cannot determine proposed commit files: selection helper failed or returned invalid data"
_INVALID_MANUAL = "Cannot determine proposed commit files: invalid candidate manual contents"


@dataclass
class RepoContext:
    root: str              # REPO_ROOT; "" means not a repository
    command_context: str   # REPO_COMMAND_CONTEXT
    pwd: str               # the hook's directory, before the shell's `cd` into root
    cwd: str               # where the hook's later git commands ran


@dataclass
class Staged:
    files: str = ""                 # STAGED
    status: str = ""                # STAGED_STATUS
    base: str = ""                  # STAGED_BASE
    selection: bool = False         # STAGED_SELECTION
    elisp_diffs: object = None      # STAGED_ELISP_DIFFS (selection only)
    manual_contents: object = field(default_factory=dict)  # STAGED_MANUAL_CONTENTS
    deny_reason: str = ""           # set when the shell library denied and exited


def _toplevel(args: list[str], env: dict) -> str:
    result = subprocess.run(["git", *args, "rev-parse", "--show-toplevel"], env=env,
                            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    return command_substitution(result.stdout.decode("utf-8", "surrogateescape"))


def _payload_context_dir(payload: dict) -> str:
    """The jq lookup of a structured workdir: tool_input keys (a string
    tool_input counts only if it decodes to an object), then top-level keys."""
    tool_input = payload.get("tool_input")
    if isinstance(tool_input, str):
        try:
            tool_input = json.loads(tool_input)
        except ValueError:
            tool_input = {}
    if not isinstance(tool_input, dict):
        tool_input = {}
    return command_substitution(jq_text(jq_alternative(*(tool_input.get(key) for key in _CONTEXT_KEYS),
                                                         *(payload.get(key) for key in _CONTEXT_KEYS))))


def _ltrim(text: str) -> str:
    return text.lstrip(BASH_SPACE)


def _cut(text: str, stops: str) -> str:
    for index, char in enumerate(text):
        if char in stops:
            return text[:index]
    return text


def _skip(text: str, pos: int, chars: str) -> int:
    """The first index at or after POS whose character is not in CHARS."""
    end = len(text)
    while pos < end and text[pos] in chars:
        pos += 1
    return pos


def _until(text: str, pos: int, stops: str) -> int:
    """The first index at or after POS whose character is in STOPS (or the end)."""
    end = len(text)
    while pos < end and text[pos] not in stops:
        pos += 1
    return pos


def cd_target(command: str, home: str) -> tuple[str, bool]:
    """The last `cd` target among the command heads, and whether it was
    present but unresolvable (a `$` left after expanding $HOME).

    Walks indexes into COMMAND rather than slicing the remainder at each head,
    so a large command costs linear rather than quadratic time.
    """
    target = ""
    pos, end = 0, len(command)
    while pos < end:
        head = _skip(command, pos, BASH_SPACE)
        while head < end and command[head] in "{(&|;":
            head = _skip(command, head + 1, BASH_SPACE)
        if command.startswith("cd", head) and head + 2 < end and command[head + 2] in BASH_SPACE:
            rest = _skip(command, head + 2, BASH_SPACE)
            quote = command[rest:rest + 1]
            if quote in ('"', "'"):
                close = command.find(quote, rest + 1)
                target = command[rest + 1:] if close < 0 else command[rest + 1:close]
            else:
                target = command[rest:_until(command, rest, BASH_SPACE + "&;|")]
                if target.startswith("~"):
                    target = home + target[1:]
                target = target.replace("\\ ", " ")
        stop = _until(command, pos, "&|;\n")
        pos = stop + 1 if stop < end else end
    # An injected `$(...)` or backtick must not reach `git -C`.
    if "$(" in target or "`" in target:
        target = ""
    if target:
        target = target.replace("${HOME}", home).replace("$HOME", home)
        if "$" in target:
            return "", True
    return target, False


def repo_root(command: str, payload: dict, runtime: str, env: dict | None = None) -> RepoContext:
    """REPO_ROOT and REPO_COMMAND_CONTEXT as lib-repo-root.sh set them.

    When the command changes directory, that target decides the repository;
    an unresolvable target gives no repository rather than the hook's own.
    """
    env = dict(os.environ if env is None else env)
    context_dir = env.get("REPO_CONTEXT_DIR", "") or _payload_context_dir(payload)
    if runtime == CODEX and command:
        parent = command_substitution(codex_payload.parent_exec_workdir(payload, command) or "")
        if parent:
            context_dir = parent
    target, unresolvable = cd_target(command, env.get("HOME", ""))
    if target and os.path.isdir(target):
        root = _toplevel(["-C", target], env)
    elif target or unresolvable:
        root = ""
    elif context_dir and os.path.isdir(context_dir):
        root = _toplevel(["-C", context_dir], env)
    else:
        root = _toplevel([], env)
    pwd = bash_pwd(env)
    command_context = target or context_dir or pwd
    if runtime == CODEX and not unresolvable:
        try:
            records = codex_payload.git_invocations(command, env=env)
        except HookFailure:
            records = []  # The shell read the parser through a process substitution and ignored its status.
        commit = next((record for record in records
                       if command_substitution(jq_raw(record["subcommand"])) == "commit"), None)
        if commit is not None:
            root = command_substitution(codex_payload.git_invocation_repo(commit, command_context, env)) or root
    if root and not os.path.isdir(root):
        raise HookFailure(f"cd {root}: not a directory")
    return RepoContext(root=root, command_context=command_context, pwd=pwd,
                       cwd=os.path.abspath(root) if root else pwd)


def _git_output(args: list[str], ctx: RepoContext, env: dict) -> tuple[int, str]:
    result = subprocess.run(["git", *args], cwd=ctx.cwd, env=env, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    return result.returncode, command_substitution(result.stdout.decode("utf-8", "surrogateescape"))


def _valid_selection(value) -> bool:
    """The `jq -e` shape check of the selection helper's answer."""
    if not isinstance(value, dict):
        return False
    return ("error" in value or value.get("mode") == "index"
            or (value.get("mode") in ("selection", "inspection") and isinstance(value.get("staged"), str)
                and isinstance(value.get("status"), str) and isinstance(value.get("diffs"), dict)))


def staged_files(command: str, ctx: RepoContext, runtime: str, env: dict | None = None) -> Staged:
    """The files a commit command would record, as lib-staged-files.sh found
    them: an explicit-path commit through commit-file-selection.py's
    disposable index, an ordinary one from the index, against HEAD~1 when
    the command amends.

    Intended difference: the shell handed the helper a relative command
    context after `cd`-ing into the repository, so `cd sub && git commit -m x
    PATH` was always denied. The context is resolved against the hook's own
    directory here, where the command's git would run.

    The helper stays a subprocess: it reads COMMIT_FILE_CWD, COMMIT_FILE_RECORD
    and the working directory from process globals and starts its own
    classifier process, so it cannot run in-process without changing it.
    """
    env = dict(os.environ if env is None else env, PWD=ctx.cwd)
    helper = DOTFILES / ("codex" if runtime == CODEX else "claude") / "hooks/commit-file-selection.py"
    helper_env = dict(env, COMMIT_FILE_RECORD=env.get("COMMIT_RECORD", ""),
                      COMMIT_FILE_CWD=os.path.join(ctx.pwd, ctx.command_context or ctx.root or ctx.cwd))
    result = subprocess.run(["python3", str(helper)], input=command.encode("utf-8", "surrogateescape"),
                            cwd=ctx.cwd, env=helper_env, stdout=subprocess.PIPE)
    try:
        selection = json.loads(command_substitution(result.stdout.decode("utf-8", "surrogateescape")))
    except ValueError:
        selection = None
    if result.returncode != 0 or not _valid_selection(selection):
        return Staged(deny_reason=_SELECTION_FAILED)
    manual = selection.get("manual_contents")
    if manual is None or manual is False:
        manual = {}
    if not (isinstance(manual, dict) and all(v is None or isinstance(v, str) for v in manual.values())):
        return Staged(deny_reason=_INVALID_MANUAL)
    error = selection.get("error")
    if error is not None and error is not False and command_substitution(jq_text(error)):
        return Staged(deny_reason=jq_raw(error))
    if command_substitution(jq_raw(selection.get("mode"))) != "index":
        return Staged(files=command_substitution(jq_raw(selection.get("staged"))),
                      status=command_substitution(jq_raw(selection.get("status"))),
                      selection=True, elisp_diffs=selection.get("diffs"), manual_contents=manual)
    base = ""
    if grep(_AMEND, command, echo=True) and _git_output(["rev-parse", "HEAD~1"], ctx, env)[0] == 0:
        base = "HEAD~1"
    revision = [base] if base else []
    return Staged(files=_git_output(["diff", "--cached", "--name-only", *revision], ctx, env)[1],
                  status=_git_output(["diff", "--cached", "--name-status", "-M", *revision], ctx, env)[1],
                  base=base)
