"""Run the policies a runtime registered for a tool, and render one response.

The chains mirror the former hook registrations exactly (~/.claude/settings.json
PreToolUse and codex/hooks.json), in their order. A denial from any policy
denies; reasons are joined in chain order. Only Claude's Bash dispatcher
rewrites commands or adds context, and Codex's
credential-prone shell commands are wrapped (policies.codex_wrap).
"""

from __future__ import annotations

import json
import locale
import os
import re
import sys
import traceback

from .core import CLAUDE, CODEX, CODEX_SHELL_TOOLS, Call, HookFailure, Result
from .policies import (ahrefs, codex_wrap, destructive, dirty_tree, dispatcher, elisp_skill, elpaca_dotfiles_edit, elpaca_rebuild,
                       github_write, hookspath, launchagent, retired_secrets, secret_leak, sensitive_read,
                       texi_edit, untrusted_execution, walk_list)

CHAINS = {
    CLAUDE: [
        ("Bash", (dispatcher, dirty_tree, launchagent, retired_secrets)),
        ("Read", (sensitive_read, walk_list)),
        ("Edit|Write|NotebookEdit", (secret_leak, retired_secrets, elpaca_dotfiles_edit, elisp_skill, texi_edit, walk_list,
                                     launchagent)),
        ("Grep", (sensitive_read, walk_list)),
        ("Glob", (walk_list,)),
    ],
    CODEX: [
        ("|".join(re.escape(t) for t in CODEX_SHELL_TOOLS),
         (sensitive_read, secret_leak, ahrefs, github_write, destructive, walk_list, elpaca_rebuild, dirty_tree,
          launchagent, untrusted_execution, hookspath, retired_secrets, codex_wrap)),
        ("apply_patch|Edit|Write", (secret_leak, elpaca_dotfiles_edit, texi_edit, walk_list, launchagent, retired_secrets)),
    ],
}

FAILED = "Security hook failed; tool execution denied."


def policies_for(runtime: str, tool: str):
    for matcher, chain in CHAINS[runtime]:
        if re.fullmatch(matcher, tool):
            yield from chain


def decide(call: Call) -> Result:
    """The combined decision. A policy failure propagates (the caller denies)."""
    reasons: list[str] = []
    updated, context = None, ""
    for policy in policies_for(call.runtime, call.tool_name):
        result = policy.decide(call)
        if result.decision == "deny":
            reasons.append(result.reason)
        updated = updated or result.updated_input
        context = context or result.context
    if reasons:
        return Result.deny("\n\n".join(reasons))
    return Result(decision="allow", updated_input=updated, context=context)


def render(runtime: str, result: Result) -> dict | None:
    """The hook response, or None to stay silent (allow)."""
    out: dict = {"hookEventName": "PreToolUse"}
    if result.decision == "deny":
        out.update(permissionDecision="deny", permissionDecisionReason=result.reason)
    elif result.context or (result.updated_input is not None and runtime == CODEX):
        # Codex requires an explicit allow alongside updatedInput.
        out["permissionDecision"] = "allow"
        if result.context:
            out["additionalContext"] = result.context
        if result.updated_input is not None:
            out["updatedInput"] = result.updated_input
    elif result.updated_input is not None:
        out["updatedInput"] = result.updated_input
    else:
        return None
    return {"hookSpecificOutput": out}


def set_collation() -> None:
    """Adopt the environment's LC_COLLATE, as the former shell guards' `sort -u` did.

    Labeled fallback: the pyenv interpreters link Homebrew's libintl, whose
    setlocale rejects "" when no LC_ALL, LC_COLLATE or LANG is set, as under
    launchd. libc's setlocale, which `sort` used, resolves that environment
    to "C", so C is the shell guards' collation there, not a weaker default.
    Raising instead denied every tool call of every unattended agent.
    """
    try:
        locale.setlocale(locale.LC_COLLATE, "")
    except locale.Error:
        if any(os.environ.get(name) for name in ("LC_ALL", "LC_COLLATE", "LANG")):
            raise
        locale.setlocale(locale.LC_COLLATE, "C")


READ_ONLY_TOOLS = {CLAUDE: ("Read", "Grep", "Glob"), CODEX: ()}


def crash_warning(runtime: str, tool: str, cause: str) -> dict | None:
    """The allow-with-warning response for a read-only TOOL when the guard
    crashed, or None when the call must stay denied.

    A crash used to deny every tool, so an agent could not even read the guard
    to diagnose it (2026-10-01/02). Read, Grep and Glob change nothing, so a
    crash lets them through unguarded (sensitive_read and walk_list do not
    run) with a warning to the user and the agent; Bash, Edit, Write and every
    other tool stay denied. There is no permissionDecision, so the normal
    permission flow still applies. Codex has no such tools.
    claude/hooks/agent-guard.sh applies the same rule when the engine cannot
    even start.
    """
    if tool not in READ_ONLY_TOOLS[runtime]:
        return None
    message = (f"WARNING: agent-guard crashed ({cause}). {tool} was allowed WITHOUT its guard checks "
               "(sensitive-read, walk-list); Bash, Edit, Write and other tools stay blocked until the guard is fixed.")
    return {"systemMessage": message,
            "hookSpecificOutput": {"hookEventName": "PreToolUse", "additionalContext": message}}


def _crash_cause(error: Exception) -> str:
    """The exception type and innermost frame, never its message, which could
    quote what the call was reading."""
    frames = traceback.extract_tb(error.__traceback__)
    where = f" at {os.path.basename(frames[-1].filename)}:{frames[-1].lineno}" if frames else ""
    return type(error).__name__ + where


def main(runtime: str) -> int:
    """Hook entry point: stdin payload, stdout response, exit 2 on any failure,
    except a crash on a read-only tool (crash_warning).

    A HookFailure is a policy's deliberate "cannot decide", not a crash, so it
    denies every tool. So does a payload that is not a JSON object, whose tool
    is unknown.
    """
    payload = None
    try:
        text = sys.stdin.read()
        # jq rejected lone-surrogate escapes (\ud800), so every shell guard
        # failed closed on them; json.loads accepts them. Keep failing closed.
        text.encode("utf-8")
        payload = json.loads(text)
        json.dumps(payload, ensure_ascii=False).encode("utf-8")
        if not isinstance(payload, dict):
            raise HookFailure("payload is not an object")
        set_collation()
        response = render(runtime, decide(Call.from_payload(runtime, payload)))
    except HookFailure:
        print(FAILED, file=sys.stderr)
        return 2
    except Exception as error:  # noqa: BLE001 - any other failure denies, as the shell guards' EXIT traps did
        tool = payload.get("tool_name") if isinstance(payload, dict) else None
        warning = crash_warning(runtime, tool, _crash_cause(error)) if isinstance(tool, str) else None
        if warning is None:
            print(FAILED, file=sys.stderr)
            return 2
        print(warning["systemMessage"], file=sys.stderr)
        print(json.dumps(warning))
        return 0
    if response is not None:
        print(json.dumps(response))
    return 0
