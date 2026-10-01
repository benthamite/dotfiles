"""Deny agent writes and commands that refer to the retired password-store CLI.

Every secret moved to 1Password in September 2026 and the old CLI, its store
and its Emacs integration were removed. Agents kept reintroducing calls to it
and prose that names it, which then broke unattended jobs and resurfaced as
stale warnings. This module is the single policy both runtimes share; the
guard engine runs it as ``agent_guard.policies.retired_secrets``.

The patterns live only here and in ``tests/test_retired_secrets_cli.py``;
those two files are the only places allowed to name the retired CLI. File and
module names avoid the store's name so they can be referenced anywhere.

Scope:

- Native writes (Write, Edit, MultiEdit, NotebookEdit; Codex apply_patch and
  Write/Edit): deny when the *new* text refers to the retired CLI. Text being
  removed (``old_string``, patch ``-`` lines) is never inspected, so deleting a
  stale reference always works.
- Shell commands: deny an invocation of the CLI, and deny a command that both
  contains a reference and writes (redirect, ``tee``, in-place edit, heredoc,
  interpreter write, copy/move, ``git commit``). Read-only searches for the old
  text stay allowed so stale references can be found and removed.
"""

from __future__ import annotations

import json
import re
import sys
from pathlib import Path
from typing import Any, Iterable

DOTFILES = Path(__file__).resolve().parents[2]
EXEMPT_PATHS = frozenset(
    {
        DOTFILES / "lib/python/retired_secrets_cli.py",
        DOTFILES / "tests/test_retired_secrets_cli.py",
    }
)

_SUBCOMMANDS = (
    r"show|insert|edit|generate|ls|list|rm|mv|cp|git|init|otp|find|grep|search"
)
_PROSE_FOLLOWERS_ALLOWED = (
    # Ordinary English or compiler uses of the word after a preposition.
    r"\d|rate|through|criteria|condition|band|mark|/fail|-through|over|by"
)

# References in written text (files, commit messages, written prose).
REFERENCE_PATTERNS: tuple[tuple[str, re.Pattern[str]], ...] = (
    ("CLI subcommand", re.compile(rf"\bpass\s+(?:{_SUBCOMMANDS})\b")),
    (
        "argument vector",
        re.compile(rf"""["']pass["']\s*,\s*["'](?:{_SUBCOMMANDS})["']"""),
    ),
    ("marked-up tool name", re.compile(r"(?:`pass`|=pass=|~pass~|\bpass\(1\))")),
    ("store directory or name", re.compile(r"(?i)\bpassword[-_ ]store\b")),
    ("store environment variable", re.compile(r"\bPASSWORD_STORE_[A-Z_]+")),
    (
        "Emacs integration",
        re.compile(r"\b(?:pass-extras|auth-source-pass|password-store\.el)\b"),
    ),
    ("store entry", re.compile(r"\bpass\s+(?:entry|entries|store|CLI|vault)\b")),
    (
        "binary path",
        re.compile(r"(?:/opt/homebrew|/usr/local|/usr)/bin/pass(?![\w.-])"),
    ),
    ("package install", re.compile(r"\bbrew\s+(?:install|reinstall|upgrade)\s+pass\b")),
    (
        "prose source",
        re.compile(
            r"\b(?:from|in|via|into|using|through)\s+[`=~]?pass[`=~]?"
            rf"(?![\w-])(?!\s*(?:{_PROSE_FOLLOWERS_ALLOWED}))"
        ),
    ),
)

# A shell command word that runs the CLI itself.
_INVOCATION = re.compile(
    r"(?:^|[;&|({`\n]|\$\(|\bthen\b|\bdo\b|\belse\b|\bxargs\b|\bexec\b|\bcommand\b"
    r"|\bwhich\b|\bsudo\b|\benv\b)\s*(?:[A-Za-z_][A-Za-z0-9_]*=\S*\s+)*"
    r"(?:\S*/)?pass(?=\s|$|[;&|)`])"
)

_WRITE_SHAPES = re.compile(
    r"(?:"
    r"(?<![0-9&])>>?(?!\s*/dev/null)(?!&)"  # stdout redirect to a file
    r"|\btee\b"
    r"|\bsed\s+(?:-[a-zA-Z]*\s+)*-i"
    r"|\bperl\s+(?:-[a-zA-Z]*\s+)*-[a-zA-Z]*i"
    r"|<<"
    r"|\bwrite_text\b|\.write\(|\bopen\([^)]*['\"][wa]"
    r"|\bgit\s+(?:-C\s+\S+\s+)?commit\b"
    r"|\bgit\s+(?:-C\s+\S+\s+)?(?:tag|notes)\b"
    r"|\bgh\s+(?:pr|issue|release)\s+(?:create|edit|comment)\b"
    r"|\b(?:cp|mv|install)\b"
    r")"
)

DENY_REASON = (
    "BLOCKED: this refers to the retired password-store CLI ({what}: {sample!r}). "
    "All secrets are in 1Password; read them with op-automations "
    "(Automation vaults, unattended) or op-desktop (anything else), in the "
    "shapes described in ~/My Drive/dotfiles/claude/context/secrets.md. Do not "
    "name the retired CLI in code, docs, logs or commit messages: describe a "
    "removed reference as a 1Password lookup, or delete it."
)


def find_reference(text: str) -> tuple[str, str] | None:
    """Return (pattern label, matched text) for the first reference in TEXT."""
    for label, pattern in REFERENCE_PATTERNS:
        match = pattern.search(text)
        if match:
            return label, match.group(0)
    return None


def _mask_quoted(command: str) -> str:
    """Blank out quoted strings so their contents cannot look like commands."""
    return re.sub(r"'[^']*'|\"(?:\\.|[^\"\\])*\"", lambda m: " " * len(m.group(0)), command)


_HEREDOC_START = re.compile(r"<<-?\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1[^\n]*\n")


def _mask_heredoc_bodies(command: str) -> str:
    """Drop heredoc bodies: they are data (often Python, where the word is a statement)."""
    out, pos = [], 0
    for match in _HEREDOC_START.finditer(command):
        if match.start() < pos:
            continue
        end = re.compile(rf"^\s*{re.escape(match.group(2))}\s*$", re.M).search(command, match.end())
        out.append(command[pos:match.end()])
        pos = end.start() if end else len(command)
    out.append(command[pos:])
    return "".join(out)


def find_invocation(command: str) -> str | None:
    match = _INVOCATION.search(_mask_quoted(_mask_heredoc_bodies(command)))
    return match.group(0).strip() if match else None


def classify_command(command: str) -> tuple[str, str] | None:
    """Return a denial (label, sample) for a shell command, or None."""
    invoked = find_invocation(command)
    if invoked:
        return "CLI invocation", invoked
    reference = find_reference(command)
    if reference and _WRITE_SHAPES.search(command):
        return reference
    return None


def _added_patch_lines(patch: str) -> str:
    lines = []
    for line in patch.splitlines():
        if line.startswith("+") and not line.startswith("+++"):
            lines.append(line[1:])
    return "\n".join(lines)


def _patch_paths(patch: str) -> list[str]:
    return re.findall(r"^\*\*\* (?:Add|Update|Delete) File: (.+)$", patch, re.M)


def _is_exempt(path: str | None) -> bool:
    if not path:
        return False
    try:
        return Path(path).expanduser().resolve() in EXEMPT_PATHS
    except (OSError, RuntimeError):
        return False


def _tool_input(payload: dict[str, Any]) -> dict[str, Any]:
    raw = payload.get("tool_input")
    if isinstance(raw, dict):
        return raw
    if isinstance(raw, str):
        try:
            parsed = json.loads(raw)
        except ValueError:
            return {"command": raw}
        return parsed if isinstance(parsed, dict) else {"command": raw}
    return {}


def _strings(values: Iterable[Any]) -> list[str]:
    return [value for value in values if isinstance(value, str) and value]


SHELL_TOOLS = frozenset(
    {"Bash", "exec_command", "functions.exec", "functions.exec_command", "shell"}
)
PATCH_TOOLS = frozenset({"apply_patch", "functions.apply_patch"})


def decide(payload: dict[str, Any]) -> str | None:
    """Return a denial reason for PAYLOAD, or None to allow it."""
    tool = payload.get("tool_name") or ""
    data = _tool_input(payload)
    path = data.get("file_path") or data.get("filePath") or data.get("notebook_path") or data.get("path")

    if tool in SHELL_TOOLS:
        commands = _strings([data.get("command"), data.get("cmd")])
        if isinstance(data.get("command"), list):
            commands.append(" ".join(str(part) for part in data["command"]))
        for command in commands:
            found = classify_command(command)
            if found:
                return DENY_REASON.format(what=found[0], sample=found[1])
        return None

    if tool in PATCH_TOOLS or (
        tool in {"Edit", "Write"} and not path and isinstance(data.get("patch") or data.get("input"), str)
    ):
        patch = next(iter(_strings([data.get("patch"), data.get("input"), data.get("command"), data.get("text")])), "")
        if _patch_paths(patch) and all(_is_exempt(p) for p in _patch_paths(patch)):
            return None
        new_text = _added_patch_lines(patch)
    else:
        if _is_exempt(path):
            return None
        pieces = _strings([data.get("content"), data.get("new_string"), data.get("new_source")])
        for edit in data.get("edits") or []:
            if isinstance(edit, dict):
                pieces.extend(_strings([edit.get("new_string")]))
        new_text = "\n".join(pieces)

    found = find_reference(new_text)
    if found:
        return DENY_REASON.format(what=found[0], sample=found[1])
    return None


def main() -> int:
    try:
        payload = json.loads(sys.stdin.read() or "{}")
        if not isinstance(payload, dict):
            raise ValueError("payload is not an object")
        reason = decide(payload)
    except Exception:  # noqa: BLE001 - a guard that cannot decide must deny
        print("Retired secrets-CLI guard failed; tool execution denied.", file=sys.stderr)
        return 2
    if reason:
        json.dump(
            {
                "hookSpecificOutput": {
                    "hookEventName": "PreToolUse",
                    "permissionDecision": "deny",
                    "permissionDecisionReason": reason,
                }
            },
            sys.stdout,
        )
        sys.stdout.write("\n")
    return 0
