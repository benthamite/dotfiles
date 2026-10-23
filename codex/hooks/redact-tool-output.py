#!/usr/bin/env python3
"""Mask credentials in Codex tool output before the model sees it.

PostToolUse hook for shell and MCP tools. Codex has no field that rewrites a
tool result, but `{"decision": "block", "reason": TEXT}` replaces the result
the model sees with TEXT (Codex 0.158.0, hooks/src/events/post_tool_use.rs and
core/src/tools/registry.rs). So when masking changes anything, this blocks and
puts the masked output in the reason. Clean output passes untouched.

Masking uses the same two layers as the Claude hook: the shape patterns of
redact-secrets.sh and the keyed-fingerprint index of this machine's actual
secret values (lib/python/secret_index.py). It fails closed: if masking cannot
run, the output is withheld.

Not covered (Codex limitations, see docs/secret-guard.md): output chunks of a
still-running exec_command session read through write_stdin, results Codex
marks as errors (PostToolUse does not run for them), and Codex's own raw
telemetry/rollout logs.
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path

sys.dont_write_bytecode = True
HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parents[1] / "lib/python"))

REDACTOR = HERE.parents[1] / "claude/hooks/redact-secrets.sh"
WITHHELD = "[output withheld: the secret redactor failed; rerun once it is fixed]"
NOTICE = "[tool output shown with secrets masked]\n"
SEPARATOR = "\x00\x1eREDACT-SPLIT\x1e\x00"


def mask_strings(strings: list[str]) -> list[str]:
    import secret_index  # noqa: PLC0415 - path set above

    if not strings:
        return []
    result = subprocess.run([str(REDACTOR)], input=SEPARATOR.join(strings), capture_output=True,
                            text=True, timeout=20, env={**os.environ, "REDACT_SKIP_INDEX": "1"})
    if result.returncode != 0:
        raise RuntimeError("redact-secrets.sh failed")
    masked = result.stdout.split(SEPARATOR)
    if len(masked) != len(strings):
        raise RuntimeError("redactor changed the string count")
    index = secret_index.Index.load()
    if index is not None:
        masked = [index.mask(s)[0] for s in masked]
    return masked


def _strings(value, out: list[str]) -> None:
    if isinstance(value, str):
        out.append(value)
    elif isinstance(value, dict):
        for v in value.values():
            _strings(v, out)
    elif isinstance(value, list):
        for v in value:
            _strings(v, out)


def _rebuild(value, new):
    if isinstance(value, str):
        return next(new)
    if isinstance(value, dict):
        return {k: _rebuild(v, new) for k, v in value.items()}
    if isinstance(value, list):
        return [_rebuild(v, new) for v in value]
    return value


def render(response) -> str:
    """What the model sees in place of the result."""
    if isinstance(response, str):
        return response
    if isinstance(response, dict) and isinstance(response.get("output"), str):
        extra = {k: v for k, v in response.items() if k != "output"}
        return response["output"] + (f"\n[{json.dumps(extra)}]" if extra else "")
    if isinstance(response, list) and all(isinstance(b, dict) and b.get("type") == "text" for b in response):
        return "\n".join(b.get("text", "") for b in response)
    return json.dumps(response, indent=1)


def decision(event: dict) -> dict | None:
    response = event.get("tool_response")
    if response is None:
        return None
    try:
        import credential_pages  # noqa: PLC0415 - path set above

        unsafe_images = credential_pages.images_unsafe(event.get("tool_input"), response)
        if unsafe_images:
            response = credential_pages.strip_images(response)
        strings: list[str] = []
        _strings(_without_images(response), strings)
        masked = mask_strings(strings)
        if credential_pages.text_unsafe(event.get("tool_name", ""), event.get("tool_input"), response):
            masked = [credential_pages.mask_opaque(x) for x in masked]
    except Exception:  # noqa: BLE001 - fail closed
        return {"decision": "block", "reason": WITHHELD}
    if masked == strings and not unsafe_images:
        return None
    return {"decision": "block", "reason": NOTICE + render(_rebuild(_without_images(response), iter(masked)))}


def _without_images(value):
    """Image data is not text; the block reason carries text only."""
    if isinstance(value, dict):
        if value.get("type") == "image":
            return {"type": "text", "text": "[image omitted]"}
        return {k: _without_images(v) for k, v in value.items()}
    if isinstance(value, list):
        return [_without_images(v) for v in value]
    return value


def main() -> int:
    try:
        event = json.load(sys.stdin)
    except ValueError:
        return 0
    out = decision(event)
    if out:
        print(json.dumps(out))
    return 0


if __name__ == "__main__":
    sys.exit(main())
