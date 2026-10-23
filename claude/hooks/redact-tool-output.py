#!/usr/bin/env python3
"""Mask credentials in tool output before the model (and the transcript) sees it.

PostToolUse hook for every tool. Credentials reached the model through outputs
no guard treated as sensitive: a Claude in Chrome `find` result echoed a fresh
Cloudflare token, a 1Password title listing printed a Mullvad account number,
a file read showed a token in a config file (docs/secret-guard.md). Guards that
inspect commands cannot see what a command, page or file will return, so this
filters the returned result itself, through two layers:

- the shape patterns of redact-secrets.sh (known prefixes such as `ghp_`);
- the keyed-fingerprint index of this machine's actual secret values
  (lib/python/secret_index.py, built by `bin/secret-index refresh`), which
  catches secrets with no recognisable shape.

Every string in the tool result is masked and the structure is preserved, so
the replacement has the shape Claude Code expects for that tool:
`updatedMCPToolOutput` for MCP tools (verified 2026-09-27), `updatedToolOutput`
for the rest. Claude Code stores only the replaced output in the transcript.

Fails closed: if masking cannot run (redactor error, unreadable index or key),
every string in the result is replaced by a notice, because Claude Code itself
keeps the original output when a PostToolUse hook fails. Images cannot be
masked here; screenshots of credential pages are handled separately.
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

REDACTOR = HERE / "redact-secrets.sh"
WITHHELD = "[output withheld: the secret redactor failed; rerun once it is fixed]"
SEPARATOR = "\x00\x1eREDACT-SPLIT\x1e\x00"


def _strings(value, out: list[str]) -> None:
    if isinstance(value, str):
        out.append(value)
    elif isinstance(value, dict):
        for v in value.values():
            _strings(v, out)
    elif isinstance(value, list):
        for v in value:
            _strings(v, out)


def _replace(value, replacements: iter):
    if isinstance(value, str):
        return next(replacements)
    if isinstance(value, dict):
        return {k: _replace(v, replacements) for k, v in value.items()}
    if isinstance(value, list):
        return [_replace(v, replacements) for v in value]
    return value


def _is_image_block(value) -> bool:
    return isinstance(value, dict) and value.get("type") == "image"


def _withhold(value, key: str | None = None):
    """Replace every string with WITHHELD except `type` discriminators.

    Keeping `type` keeps the result's shape valid; Claude Code ignores a
    replacement of the wrong shape and would show the original output.
    """
    if isinstance(value, str):
        return value if key == "type" else WITHHELD
    if isinstance(value, dict):
        return {k: _withhold(v, k) for k, v in value.items()}
    if isinstance(value, list):
        return [_withhold(v, key) for v in value]
    return value


def _strip_all_images(value):
    """Failure path: no image survives when the checks could not run."""
    if _is_image_block(value):
        return {"type": "text", "text": WITHHELD}
    if isinstance(value, dict):
        return {k: _strip_all_images(v) for k, v in value.items()}
    if isinstance(value, list):
        return [_strip_all_images(v) for v in value]
    return value


def _skip_images(value):
    """Strings inside image blocks (base64 data) are not text to mask."""
    if _is_image_block(value):
        return None
    if isinstance(value, dict):
        return {k: _skip_images(v) for k, v in value.items()}
    if isinstance(value, list):
        return [_skip_images(v) for v in value]
    return value


def mask_strings(strings: list[str]) -> list[str]:
    """Mask each string through both layers; raises on any failure."""
    import secret_index  # noqa: PLC0415 - path set above

    if not strings:
        return []
    joined = SEPARATOR.join(strings)
    result = subprocess.run([str(REDACTOR)], input=joined, capture_output=True, text=True, timeout=20,
                            env={**os.environ, "REDACT_SKIP_INDEX": "1"})
    if result.returncode != 0:
        raise RuntimeError("redact-secrets.sh failed")
    masked = result.stdout.split(SEPARATOR)
    if len(masked) != len(strings):
        raise RuntimeError("redactor changed the string count")
    index = secret_index.Index.load()
    if index is not None:
        masked = [index.mask(s)[0] for s in masked]
    return masked


def _transform(response, fn):
    """Apply fn to every non-image string, preserving structure."""
    if _is_image_block(response):
        return response
    if isinstance(response, str):
        return fn([response])[0]
    view = _skip_images(response)
    strings: list[str] = []
    _strings(view, strings)
    new = iter(fn(strings))

    def rebuild(value, shadow):
        if _is_image_block(value):
            return value
        if isinstance(value, str):
            return next(new)
        if isinstance(value, dict):
            return {k: rebuild(v, shadow[k]) for k, v in value.items()}
        if isinstance(value, list):
            return [rebuild(v, s) for v, s in zip(value, shadow)]
        return value

    return rebuild(response, view)


def replacement(event: dict) -> dict | None:
    """The hookSpecificOutput to emit, or None when nothing changed."""
    tool, response = event.get("tool_name", ""), event.get("tool_response")
    if response is None:
        return None
    original = response
    try:
        import credential_pages  # noqa: PLC0415 - path set above

        if credential_pages.images_unsafe(event.get("tool_input"), response):
            response = credential_pages.strip_images(response)
        masked = _transform(response, mask_strings)
        if credential_pages.text_unsafe(tool, event.get("tool_input"), response):
            masked = _transform(masked, lambda ss: [credential_pages.mask_opaque(x) for x in ss])
    except Exception:  # noqa: BLE001 - fail closed; see module docstring
        masked = _withhold(_strip_all_images(original))
    response = original
    if masked == response:
        return None
    field = "updatedMCPToolOutput" if tool.startswith("mcp__") else "updatedToolOutput"
    return {"hookEventName": "PostToolUse", field: masked}


def main() -> int:
    try:
        event = json.load(sys.stdin)
    except ValueError:
        return 0  # no tool result to protect
    out = replacement(event)
    if out:
        print(json.dumps({"hookSpecificOutput": out}))
    return 0


if __name__ == "__main__":
    sys.exit(main())
