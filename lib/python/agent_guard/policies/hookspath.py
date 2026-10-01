"""core.hooksPath overrides: codex/hooks/block-hookspath-override.sh and
check_hookspath_override in claude/hooks/pretooluse-bash.sh.

Elisp test and live-verification evidence is enforced by the global Git hooks
in claude/git-hooks and by the Stop hook, not by reading commands. Overriding
core.hooksPath would silently disable both, so only that command shape is
denied. The two copies are the same check; only the command source differs.
"""

from __future__ import annotations

from .. import ere
from ..core import Call, Result

REASON = ("BLOCKED: Git commands may not override core.hooksPath. The global hooks in "
          "~/My Drive/dotfiles/claude/git-hooks enforce Elisp test evidence and run each repository's own hooks.")

_OPTION = r"-c[[:space:]]*core\.hookspath"
_GIT_CONFIG = r"(^|[^[:alnum:]_-])git[[:space:]][^|;&]*config[[:space:]][^|;&]*core\.hookspath"
_CONFIG_OPTION = ere.ere(_OPTION, ignore_case=True)
_MENTION = ere.ere(_OPTION + "|" + _GIT_CONFIG, ignore_case=True)
_SEGMENT = ere.ere(_GIT_CONFIG + "[^|;&]*", ignore_case=True)
_WRITE_FORM = ere.ere(r"--(unset|unset-all|add|replace-all|rename-section|remove-section|edit)([[:space:]=]|$)"
                      r"|(^|[[:space:]])-e([[:space:]]|$)", ignore_case=True)
_GET_FORM = ere.ere(r"--get(-all|-regexp|-urlmatch)?([[:space:]=]|$)", ignore_case=True)
_BARE_KEY = ere.ere(r"core\.hookspath[[:space:]]*$", ignore_case=True)


def _read_only(cmd: str) -> bool:
    """`hookspath_read_only_p`: every `git config` segment naming the key is a
    `--get` form or the bare key; a `-c` override is never a read."""
    if ere.grep(_CONFIG_OPTION, cmd):
        return False
    segments = ere.grep_only(_SEGMENT, cmd)
    if not segments:
        return False
    for segment in segments:
        if ere.grep(_WRITE_FORM, segment):
            return False
        if ere.grep(_GET_FORM, segment) or ere.grep(_BARE_KEY, segment):
            continue
        return False
    return True


def _overrides(cmd: str) -> bool:
    return ere.grep(_MENTION, cmd) and not _read_only(cmd)


def decide(call: Call) -> Result:
    cmd = call.shell_command()
    return Result.deny(REASON) if cmd and _overrides(cmd) else Result.allow()


def dispatcher_check(call: Call) -> list[str]:
    """check_hookspath_override: the same check on the dispatcher's command."""
    return [REASON] if _overrides(call.shell_command()) else []
