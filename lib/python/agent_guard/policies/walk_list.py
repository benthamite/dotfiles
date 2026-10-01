"""Walk-list protected store: block-walk-list-access.sh (both runtimes) and
check_walk_list in claude/hooks/pretooluse-bash.sh.

walk-list moves user-supplied lists into ~/.claude/walk-list-data/<uuid>/ so
that only the walk.py CLI sees their contents, one item at a time. No other
tool may read, search or edit that directory; a shell command may name it
only to run walk.py.
"""

from __future__ import annotations

from ..codex_paths import codex_changed_paths, codex_patch_text
from ..codex_payload import command_substitution, jq_alternative, jq_text
from ..core import CLAUDE, Call, Result
from ..ere import ere, grep
from ..shell_text import mask_git_commit_messages, mask_heredoc_bodies
from .sensitive_read import command, expand_tilde, home, tool_name

_ADVICE = ("The walk-list protected store at ~/.claude/walk-list-data/ is unreadable by any tool other than "
           "`python ~/.claude/skills/walk-list/walk.py`. Use `walk.py next <input-file> <decision>` to advance one "
           "item; the script will print exactly the one item you are permitted to see. Do NOT attempt to circumvent.")
_WALK_PY = ere(r"(^|[;&|[:space:]])python[0-9.]*[[:space:]]+(-[a-zA-Z]+[[:space:]]+)*([^[:space:]]+/)?walk\.py"
               r"([[:space:]]|$)")


def _deny(reason: str) -> Result:
    return Result.deny(f"BLOCKED: {reason}\n\n{_ADVICE}")


def _store(home_dir: str) -> str:
    return f"{home_dir}/.claude/walk-list-data"


def _mentions_store(text: str, home_dir: str) -> bool:
    """`echo TEXT | grep -qE "(…|$EXPANDED_HOME)"`: the home path is spliced
    into the regex unescaped, so its dots match any character."""
    return grep(r"(\.claude/walk-list-data|~/\.claude/walk-list-data|" + _store(home_dir) + ")", text, echo=True)


def path_is_guarded(path: str, home_dir: str) -> bool:
    """The store itself, anything under it, or any path naming a
    `/.claude/walk-list-data` directory elsewhere.

    Intended difference, stricter than the hook: its second test,
    `*"$GUARDED_PATTERN"*` with GUARDED_PATTERN='/\\.claude/walk-list-data',
    quoted the pattern, so it matched only a literal backslash and never
    fired; `/tmp/.claude/walk-list-data` and `~/.claude/walk-list-datax` were
    allowed. The substring test it meant is applied here.
    """
    if not path:
        return False
    path = expand_tilde(path, home_dir)
    store = _store(home_dir)
    return path == store or path.startswith(store + "/") or "/.claude/walk-list-data" in path


def _shell_reason(command_text: str, home_dir: str) -> str:
    """The Bash branch's denial reason, or "" to allow. A path named only in
    a commit message or a sink-fed heredoc is prose; walk.py legitimately
    holds the data and never prints more than one item."""
    if not command_text:
        return ""
    if not _mentions_store(mask_git_commit_messages(mask_heredoc_bodies(command_text)), home_dir):
        return ""
    if grep(_WALK_PY, command_text, echo=True):
        return ""
    return f"Bash command references ~/.claude/walk-list-data/ outside of walk.py: {command_text}"


def _field(call: Call, *keys: str) -> str:
    """`$(jq -r '.tool_input.K1 // .tool_input.K2 // ... // empty')`, or the
    same over `codex_tool_input`."""
    fields = call.tool_input
    return command_substitution(jq_text(jq_alternative(*(fields.get(key) for key in keys))))


def decide(call: Call) -> Result:
    name = tool_name(call)
    if not name:
        return Result.allow()
    home_dir = home(call)
    if name in ("Read", "Edit", "Write", "NotebookEdit"):
        field = _field(call, "file_path", "notebook_path", "path")
        if path_is_guarded(field, home_dir):
            return _deny(f"{name} of walk-list protected path: {field}")
    elif name in ("Grep", "Glob"):
        field = _field(call, "path")
        if path_is_guarded(field, home_dir):
            return _deny(f"{name} of walk-list protected path: {field}")
        pattern = _field(call, "pattern")
        if path_is_guarded(pattern, home_dir):
            return _deny(f"{name} pattern references walk-list protected path: {pattern}")
    elif name == "Bash" or (call.runtime != CLAUDE and name in ("exec_command", "functions.exec",
                                                                    "functions.exec_command")):
        reason = _shell_reason(command(call), home_dir)
        if reason:
            return _deny(reason)
    elif name == "apply_patch" and call.runtime != CLAUDE:
        for field in codex_changed_paths(call.raw):
            if path_is_guarded(field, home_dir):
                return _deny(f"apply_patch of walk-list protected path: {field}")
        if _mentions_store(codex_patch_text(call.raw), home_dir):
            return _deny("apply_patch references ~/.claude/walk-list-data/")
    return Result.allow()


def dispatcher_check(call: Call) -> list[str]:
    """check_walk_list in pretooluse-bash.sh: the Claude Bash branch verbatim."""
    reason = _shell_reason(command(call), home(call))
    return [_deny(reason).reason] if reason else []
