"""Elpaca rebuilds over uncommitted extras, ported from block-elpaca-rebuild-uncommitted.sh.

The Elpaca source clone syncs from the dotfiles repository through commits,
so a rebuild with uncommitted `emacs/extras/*.el` edits loads stale code.
Both runtimes' hooks differ only in how they read the command. Claude ran
this one only through the Bash dispatcher, behind the same two greps.
"""

from __future__ import annotations

import subprocess

from ..codex_payload import command_substitution
from ..core import Call, Result
from ..ere import ere, grep
from .launchagent import bash_home

_EMACSCLIENT = ere(r"\bemacsclient\b")
_REBUILD = ere(r"elpaca-rebuild|elpaca-extras-reload")


def decide(call: Call) -> Result:
    command = call.shell_command()
    if not grep(_EMACSCLIENT, command, echo=True) or not grep(_REBUILD, command, echo=True):
        return Result.allow()
    dotfiles = bash_home(call.env) + "/My Drive/dotfiles"
    result = subprocess.run(["git", "-C", dotfiles, "diff", "HEAD", "--name-only", "--", "emacs/extras/*.el"],
                            env=call.env, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    dirty = command_substitution(result.stdout.decode("utf-8", "surrogateescape"))
    if not dirty:
        return Result.allow()
    # Intended difference: the hook wrote "\n" inside double quotes, so its
    # reason showed backslash-n where these line breaks are.
    return Result.deny(
        "BLOCKED: You have uncommitted .el changes in the dotfiles repo. The elpaca source clone only has committed "
        "code, so elpaca-rebuild would load stale definitions.\n\nUncommitted files:\n" + dirty
        + "\n\nCommit first, then rebuild.")
