"""Route credential-prone Codex shell output through the secret redactor.

Codex's PostToolUse masking cannot reach output streamed from a still-running
`exec_command` session (`write_stdin` polls) or results Codex marks as errors
(docs/secret-guard.md). Rewriting the command itself covers both. The rule is
Claude's dispatcher's `check_wrap` (dispatcher.wrapped): only commands whose
output may carry credentials are wrapped, so ordinary and interactive commands
keep their output untouched.

Codex applies `updatedInput` to Bash-shaped calls (code-mode nested calls
arrive as `Bash`), replacing the `cmd` argument; the engine renders it with the
required explicit allow.
"""

from __future__ import annotations

from ..core import CODEX, Call, Result
from .dispatcher import wrapped

REWRITABLE = ("Bash", "exec_command", "functions.exec_command")


def decide(call: Call) -> Result:
    if call.runtime != CODEX or call.tool_name not in REWRITABLE:
        return Result.allow()
    new_command = wrapped(call.shell_command())
    if new_command is None:
        return Result.allow()
    return Result.rewrite({"command": new_command})
