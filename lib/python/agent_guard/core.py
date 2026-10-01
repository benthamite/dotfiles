"""Call model and policy result shared by every guard policy."""

from __future__ import annotations

import json
import os
from dataclasses import dataclass, field
from pathlib import Path

DOTFILES = Path(__file__).resolve().parents[3]
HOME = Path.home()

CLAUDE = "claude"
CODEX = "codex"

# Codex shell tool names, as codex/hooks/lib-codex-hook-json.sh recognised them.
CODEX_SHELL_TOOLS = ("Bash", "exec_command", "functions.exec", "functions.exec_command")


class HookFailure(Exception):
    """A policy could not decide; the engine denies the call.

    `status` is the exit status the shell hook died with (2 by its EXIT trap;
    some Claude hooks had none and exited with the failing command's status),
    which the Claude dispatcher quoted when a delegated hook failed.
    """

    def __init__(self, message: str = "", status: int = 2):
        super().__init__(message)
        self.status = status


@dataclass
class Call:
    """One PreToolUse payload, parsed once for every policy."""

    runtime: str
    tool_name: str
    tool_input: dict
    cwd: str
    session_id: str = ""
    raw: dict = field(default_factory=dict)
    env: dict = field(default_factory=lambda: dict(os.environ))

    @classmethod
    def from_payload(cls, runtime: str, payload: dict, env: dict | None = None) -> "Call":
        tool_input = payload.get("tool_input")
        if runtime == CODEX:
            tool_input = codex_tool_input(tool_input)
        elif tool_input is None:
            tool_input = {}
        elif not isinstance(tool_input, dict):
            # jq `.tool_input.X` errored on a non-object, so every Claude hook failed closed.
            raise HookFailure("tool_input is not an object")
        return cls(runtime=runtime, tool_name=str(payload.get("tool_name") or ""), tool_input=tool_input,
                   cwd=str(payload.get("cwd") or os.getcwd()), session_id=str(payload.get("session_id") or ""),
                   raw=payload, env=dict(env if env is not None else os.environ))

    @property
    def is_shell(self) -> bool:
        if self.runtime == CLAUDE:
            return self.tool_name == "Bash"
        return self.tool_name in CODEX_SHELL_TOOLS

    def shell_command(self) -> str:
        """The command text, as `codex_shell_command` / `.tool_input.command` read it."""
        from . import codex_payload  # noqa: PLC0415 - codex_payload imports this module

        # The hooks read it as COMMAND=$(jq -r ...): jq's `//` keeps an empty
        # string, -r prints numbers, and $(...) drops NULs and trailing newlines.
        if self.runtime == CODEX:
            raw = codex_payload.shell_command(self.raw)
        else:
            raw = codex_payload.jq_text(codex_payload.jq_alternative(self.tool_input.get("command")))
        return codex_payload.command_substitution(raw)


def codex_tool_input(value) -> dict:
    """`codex_tool_input` from lib-codex-hook-json.sh: objects pass through,
    JSON strings are decoded, any other string becomes {"command": raw}."""
    if isinstance(value, dict):
        return value
    if isinstance(value, str):
        try:
            decoded = json.loads(value)
        except ValueError:
            return {"command": value}
        return decoded if isinstance(decoded, dict) else {"command": value}
    return {}


@dataclass
class Result:
    decision: str = "allow"          # "allow" | "deny"
    reason: str = ""
    updated_input: dict | None = None
    context: str = ""                # additionalContext on an explicit allow

    @classmethod
    def allow(cls) -> "Result":
        return cls()

    @classmethod
    def deny(cls, reason: str) -> "Result":
        return cls("deny", reason)

    @classmethod
    def rewrite(cls, updated_input: dict) -> "Result":
        return cls("allow", "", updated_input)

