"""History rewrites over a dirty tree: block-dirty-tree-rewrite.sh (both runtimes).

`git filter-repo` and `git filter-branch` hard-reset the working tree, so an
uncommitted change to a tracked file is destroyed with nothing to recover it
from (2026-07-27). Untracked files survive a hard reset and are not counted.
The two hooks differ only in how they read the command.
"""

from __future__ import annotations

import re
import subprocess

from .. import ere
from ..codex_payload import bash_pwd, command_substitution, jq_text
from ..core import Call, Result

_PREFILTER = ere.ere(r"filter-repo|filter-branch")
_ESCAPE_HATCH = ere.ere(r"(^|[[:space:]])ALLOW_DIRTY_REWRITE=1([[:space:]]|$)")
_PATH = r"\"[^\"]+\"|'[^']+'|[^\s;&|]+"
_REWRITE = re.compile(
    r"(?:^|[\n;&|(])\s*"
    r"(?:[A-Za-z_][A-Za-z0-9_]*=\S*\s+)*"
    r"(?:git(?:\s+-C\s+(?P<cdir>" + _PATH + r"))?\s+filter-(?:repo|branch)"
    r"|git-filter-repo)\b"
)
_CD = re.compile(r"(?:^|[\n;&|(])\s*cd\s+(" + _PATH + r")")
_OPENER = re.compile(r"<<-?\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1")


def _target(cmd: str) -> str | None:
    """The hook's embedded Python: the directory a real rewrite invocation
    acts on ("" for the hook's own), or None when the phrase is only
    mentioned, for example in a heredoc commit message."""
    kept, marker = [], None
    for line in cmd.split("\n"):
        if marker is not None:
            if line.strip() == marker:
                marker = None
            continue
        kept.append(line)
        opener = _OPENER.search(line)
        if opener:
            marker = opener.group(2)
    stripped = "\n".join(kept)
    match = _REWRITE.search(stripped)
    if not match:
        return None
    target = (match.group("cdir") or "").strip("\"'")
    if not target:
        for cd in _CD.finditer(stripped):
            target = cd.group(1).strip("\"'")
    return command_substitution(target)


def _git(target: str, args: list[str], env: dict) -> subprocess.CompletedProcess:
    return subprocess.run(["git", "-C", target, *args], env=env, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)


def _text(data: bytes) -> str:
    """Git output as `jq --arg` stored it: invalid UTF-8 replaced."""
    return data.decode("utf-8", "replace")


def decide(call: Call) -> Result:
    cmd = call.shell_command()
    if not cmd or not ere.grep(_PREFILTER, cmd) or ere.grep(_ESCAPE_HATCH, cmd):
        return Result.allow()
    target = _target(cmd)
    if target is None:
        return Result.allow()
    if not target:
        target = command_substitution(jq_text(call.raw.get("cwd"))) or bash_pwd(call.env)
    dirty = command_substitution(_text(_git(target, ["status", "--porcelain", "--untracked-files=no"], call.env).stdout))
    if not dirty:
        return Result.allow()
    toplevel = _git(target, ["rev-parse", "--show-toplevel"], call.env)
    repo = command_substitution(_text(toplevel.stdout) + (target if toplevel.returncode else ""))
    count = sum(1 for line in dirty.split("\n") if line)
    return Result.deny(f"""BLOCKED: git history rewrite with a dirty working tree in {repo}

filter-repo and filter-branch hard-reset the working tree. The {count} uncommitted change(s) below would be destroyed, and since they were never committed, git cannot recover them — no reflog, no stash, no backup bundle.

{dirty}

Commit them, or stash with `git stash push -u`, then rerun the rewrite and `git stash pop` afterwards.

If the changes really are disposable, prefix the command with ALLOW_DIRTY_REWRITE=1.""")
