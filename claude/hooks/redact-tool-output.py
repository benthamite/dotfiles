#!/usr/bin/env python3
"""Mask credentials in tool output before the model (and the transcript) sees it.

PostToolUse hook for Bash and every MCP tool. Credentials reached the model
through outputs no guard treated as sensitive: a Claude in Chrome `find`
result echoed a freshly created Cloudflare token, and a 1Password title
listing printed a Mullvad account number that was part of an item title.
Guards that inspect commands cannot see what a command or page will return,
so this filters the returned text itself, through the same patterns as
redact-secrets.sh (the single source of what counts as a secret), and hands
Claude Code the masked result:

- Bash: `updatedToolOutput` with the structured result, stdout/stderr masked;
- MCP tools: `updatedMCPToolOutput` with each text block masked.

Claude Code stores only the replaced output in the session transcript
(verified 2026-09-27). Images (screenshots) cannot be masked here. The hook
prints nothing when nothing needed masking, and fails open (no replacement)
if the redactor cannot run, since blocking every tool call would be worse.
"""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

REDACTOR = Path(__file__).resolve().parent / "redact-secrets.sh"


def redact(text: str) -> str:
    r = subprocess.run([str(REDACTOR)], input=text, capture_output=True, text=True, timeout=20)
    if r.returncode != 0:
        raise RuntimeError(r.stderr.strip())
    return r.stdout


def replacement(event: dict) -> dict | None:
    """The hookSpecificOutput to emit, or None when nothing changed."""
    tool, response = event.get("tool_name", ""), event.get("tool_response")
    if tool == "Bash" and isinstance(response, dict):
        masked = dict(response)
        for key in ("stdout", "stderr"):
            if isinstance(response.get(key), str) and response[key]:
                masked[key] = redact(response[key])
        if masked != response:
            return {"hookEventName": "PostToolUse", "updatedToolOutput": masked}
    elif tool.startswith("mcp__") and isinstance(response, list):
        masked = [dict(b, text=redact(b["text"])) if b.get("type") == "text" and isinstance(b.get("text"), str) else b
                  for b in response]
        if masked != response:
            return {"hookEventName": "PostToolUse", "updatedMCPToolOutput": masked}
    elif tool.startswith("mcp__") and isinstance(response, str):
        masked = redact(response)
        if masked != response:
            return {"hookEventName": "PostToolUse", "updatedMCPToolOutput": masked}
    return None


def main() -> int:
    try:
        event = json.load(sys.stdin)
        out = replacement(event)
    except Exception:  # noqa: BLE001 - fail open; see module docstring
        return 0
    if out:
        print(json.dumps({"hookSpecificOutput": out}))
    return 0


if __name__ == "__main__":
    sys.exit(main())
