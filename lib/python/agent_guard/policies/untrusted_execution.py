"""Recognizable untrusted execution: block-untrusted-execution.sh (both runtimes).

Routes transient package runners, downloaded shell input and the untrusted
staging directory to bin/untrusted-run; lib-untrusted-execution.py classifies
each (command, working directory) context without running it. Claude sends one
context from tool_input; Codex one per shell call, or one per literal nested
exec_command of a functions.exec program, any of which may be ambiguous.
"""

from __future__ import annotations

from pathlib import Path

from .. import classifiers
from ..codex_payload import bash_pwd, command_substitution, jq_alternative, jq_text, nested_exec_contexts, \
    shell_command, tool_input
from ..core import CLAUDE, CODEX_SHELL_TOOLS, Call, HookFailure, Result
from .sensitive_read import tool_name

ADVICE = (". Use the canonical dotfiles bin/untrusted-run with explicit minimal inputs and a fresh output "
          "directory. If the required VM or runtime is unavailable, stop this execution; do not retry it on the host.")


def _claude_contexts(call: Call) -> list | None:
    """`jq -ce '[.tool_input | {cmd: .command, workdir: (.workdir // $cwd)}]'`,
    or None for a call that is not Bash. A tool_name that is not a string fails
    `jq -e '.tool_name | strings'`."""
    name = call.raw.get("tool_name")
    if not isinstance(name, str):
        raise HookFailure("tool_name is not a string")
    if command_substitution(name) != "Bash":
        return None
    fields = call.tool_input
    return [{"cmd": fields.get("command"), "workdir": jq_alternative(fields.get("workdir"), bash_pwd(call.env))}]


def _codex_contexts(call: Call) -> list | None:
    name = tool_name(call)
    if not name:
        raise HookFailure("tool_name is empty")
    if name not in CODEX_SHELL_TOOLS:
        return None
    fields = tool_input(call.raw)
    if not isinstance(jq_alternative(fields.get("command"), fields.get("cmd"), fields.get("input")), str):
        raise HookFailure("shell command is not a string")
    if not isinstance(jq_alternative(fields.get("workdir"), call.raw.get("cwd"), ""), str):
        raise HookFailure("workdir is not a string")
    command = command_substitution(shell_command(call.raw))
    cwd = command_substitution(jq_text(jq_alternative(fields.get("workdir"), call.raw.get("cwd")))) \
        or bash_pwd(call.env)
    if name != "functions.exec":
        return [{"cmd": command, "workdir": cwd}]
    # A parser failure fails the shell pipeline under pipefail: no partial contexts.
    return [dict(context, workdir=jq_alternative(context.get("workdir"), cwd))
            for context in nested_exec_contexts(command)]


def decide(call: Call) -> Result:
    contexts = _claude_contexts(call) if call.runtime == CLAUDE else _codex_contexts(call)
    if contexts is None:
        return Result.allow()
    lib = classifiers.load("untrusted-execution")
    try:
        for context in contexts:
            if context.get("ambiguous"):
                raise lib.Denied("a nested execution context cannot be classified")
            command, cwd = context.get("cmd"), context.get("workdir")
            if not isinstance(command, str) or not isinstance(cwd, str) or not cwd:
                raise HookFailure("context fields")
            lib.classify(command, Path(cwd).resolve(strict=False))
    except lib.Denied as error:
        return Result.deny("BLOCKED: " + str(error) + (error.advice or ADVICE))
    except (ValueError, TypeError, KeyError, OSError) as error:
        raise HookFailure(f"untrusted-execution classifier: {error}") from error
    return Result.allow()
