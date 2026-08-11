#!/usr/bin/env python3
"""Classify shell exports without printing their values."""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path


EXPORT_RE = re.compile(
    r"^\s*export\s+([A-Za-z_][A-Za-z0-9_]*)(?:\s*=\s*(.*))?\s*$"
)
FUNCTION_RE = re.compile(
    r"^\s*(?:(?:function\s+)?[A-Za-z_][A-Za-z0-9_]*\s*\(\s*\)"
    r"|function\s+[A-Za-z_][A-Za-z0-9_]*)\s*\{"
)
SECRET_NAME_RE = re.compile(
    r"(?:^|_)(?:API_?KEY|TOKEN|SECRET|PASSWORD|PASSWD|CREDENTIALS?|"
    r"AUTH(?:_TOKEN)?|PRIVATE_KEY)(?:_|$)"
)
IDENTITY_NAME_RE = re.compile(
    r"(?:^|_)(?:EMAIL|USERNAME|USER_NAME|PHONE|MOBILE)(?:_|$)"
)
STORE_REFERENCE_RE = re.compile(
    r"(?:^|[^A-Za-z0-9_-])(?:pass|op|op-desktop|op-automations|security|envchain)"
    r"(?:[^A-Za-z0-9_-]|$)"
)


def brace_delta(line: str) -> int:
    """Count structural braces while ignoring quotes and shell comments."""
    delta = 0
    quote: str | None = None
    escaped = False
    for character in line:
        if escaped:
            escaped = False
            continue
        if character == "\\" and quote != "'":
            escaped = True
            continue
        if quote:
            if character == quote:
                quote = None
            continue
        if character in {"'", '"'}:
            quote = character
        elif character == "#":
            break
        elif character == "{":
            delta += 1
        elif character == "}":
            delta -= 1
    return delta


def classify(name: str, value: str | None, scope: str) -> str:
    """Return a value-free classification for one exported variable."""
    if SECRET_NAME_RE.search(name):
        if scope == "function":
            return "function-local-credential"
        if value and STORE_REFERENCE_RE.search(value):
            return "credential-store-backed"
        if not value or "$" in value:
            return "credential-indirect"
        return "credential-literal"
    if IDENTITY_NAME_RE.search(name):
        return "identity"
    return "non-secret"


def inspect(path: Path) -> list[dict[str, object]]:
    """Return export metadata without retaining values in the result."""
    findings: list[dict[str, object]] = []
    function_depth: int | None = None
    depth = 0

    for line_number, line in enumerate(path.read_text().splitlines(), start=1):
        starts_function = FUNCTION_RE.match(line) is not None
        if starts_function and function_depth is None:
            function_depth = depth

        scope = "function" if function_depth is not None else "global"
        match = EXPORT_RE.match(line)
        if match:
            name, value = match.groups()
            findings.append(
                {
                    "classification": classify(name, value, scope),
                    "line": line_number,
                    "name": name,
                    "scope": scope,
                }
            )

        depth += brace_delta(line)
        if function_depth is not None and depth <= function_depth:
            function_depth = None

    return findings


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Classify shell exports without printing secret values."
    )
    parser.add_argument("path", type=Path)
    args = parser.parse_args()
    print(json.dumps(inspect(args.path), indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
