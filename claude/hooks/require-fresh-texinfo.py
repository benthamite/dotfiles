#!/usr/bin/env python3
"""Commit gate: a staged Texinfo manual must match a fresh export of its Org source.

A `.texi` file is generated from its Org manual.  Hand edits to it, and Org
edits made without the after-edit hook (by a script, sed or git), leave the
two out of step; the next export then rewrites far more than the change at
hand, and agents took that diff for noise and hand-edited again.  This gate
runs from git pre-commit through `claude/bin/agent-commit-gates`.  For every
staged Org manual, and every staged `.texi` whose Org manual is tracked beside
it, it exports the staged Org bytes with the after-edit hook in a private
directory and compares the result with the staged `.texi`.

Manuals the hook refuses to export automatically (unreviewed directives) are
not checked here: they need a reviewed export, which this gate cannot judge.

Reads the PreToolUse JSON payload on stdin only for `--amend`; prints a deny
decision when a manual is stale.
"""

from __future__ import annotations

import json
import re
import subprocess
import sys
import tempfile
from pathlib import Path, PurePosixPath

HOOK = Path(__file__).resolve().parent / "regenerate-manual-after-edit.sh"
TRIGGER = re.compile(
    r"^[ \t]*#\+(texinfo_filename:[ \t]*\S|export_file_name:[ \t]*.+\.info[ \t]*$)",
    re.IGNORECASE | re.MULTILINE)
EXPORT_NAME = re.compile(r"^[ \t]*#\+export_file_name:[ \t]*(.+?)[ \t]*$", re.IGNORECASE | re.MULTILINE)


def git(*args: str, text: bool = True) -> str | bytes:
    return subprocess.run(["git", *args], capture_output=True, text=text, check=True).stdout


def staged_blob(path: str) -> bytes | None:
    """Return the index bytes of PATH, or None when it is not in the index."""
    result = subprocess.run(["git", "cat-file", "blob", f":{path}"], capture_output=True)
    return result.stdout if result.returncode == 0 else None


def texi_for(org: str, content: str) -> str:
    """Return the repository path of the .texi that Org manual ORG exports to."""
    match = EXPORT_NAME.search(content)
    name = match.group(1).strip('"') if match else PurePosixPath(org).name
    return str(PurePosixPath(org).parent / (PurePosixPath(name).stem + ".texi"))


def manual_source(org: str) -> str | None:
    """Return the staged text of ORG when it declares a Texinfo manual."""
    blob = staged_blob(org)
    if blob is None:
        return None
    content = blob.decode("utf-8", errors="replace")
    return content if TRIGGER.search(content) else None


def candidates(staged: list[str]) -> dict[str, str]:
    """Map each Org manual the commit touches to its .texi path."""
    pairs: dict[str, str] = {}
    for path in staged:
        if path.endswith(".org") and (content := manual_source(path)) is not None:
            pairs[path] = texi_for(path, content)
    for path in staged:
        if not path.endswith(".texi") or path in pairs.values():
            continue
        directory = str(PurePosixPath(path).parent)
        siblings = git("ls-files", "--", f"{directory}/*.org" if directory != "." else "*.org").splitlines()
        for org in siblings:
            if PurePosixPath(org).parent != PurePosixPath(path).parent:
                continue
            content = manual_source(org)
            if content is not None and texi_for(org, content) == path:
                pairs[org] = path
    return pairs


def fresh_export(org: str, content: bytes) -> bytes | None:
    """Export CONTENT, the staged bytes of ORG, and return the .texi bytes.
    Return None when the hook will not export it automatically."""
    with tempfile.TemporaryDirectory(prefix="fresh-texinfo-") as tmp:
        source = Path(tmp) / PurePosixPath(org).name
        source.write_bytes(content)
        payload = json.dumps({"tool_name": "Edit", "tool_input": {"file_path": str(source)}})
        result = subprocess.run(["bash", str(HOOK)], input=payload, capture_output=True, text=True)
        try:
            report = json.loads(result.stdout)["hookSpecificOutput"]["additionalContext"]
        except (json.JSONDecodeError, KeyError, TypeError):
            return None
        if "Failures: 0" not in report:
            return None
        output = Path(tmp) / PurePosixPath(texi_for(org, content.decode("utf-8", "replace"))).name
        return output.read_bytes() if output.exists() else None


def main() -> int:
    payload = sys.stdin.read()
    try:
        command = json.loads(payload).get("tool_input", {}).get("command", "")
    except (json.JSONDecodeError, AttributeError):
        command = ""
    base = ["HEAD~1"] if "--amend" in command.split() else []
    staged = git("diff", "--cached", "--name-only", "--diff-filter=ACMR", *base).splitlines()
    stale = []
    for org, texi in sorted(candidates(staged).items()):
        committed_texi = staged_blob(texi)
        if committed_texi is None:
            continue  # The manual keeps no tracked .texi.
        exported = fresh_export(org, staged_blob(org) or b"")
        if exported is not None and exported != committed_texi:
            stale.append((org, texi))
    if not stale:
        return 0
    listing = "; ".join(f"{texi} (from {org})" for org, texi in stale)
    orgs = " ".join(org for org, _ in stale)
    reason = (
        f"BLOCKED: staged Texinfo does not match a fresh export of its staged Org manual: {listing}. "
        "A .texi is generated: never edit it by hand. Edit the Org source, then regenerate with "
        f"`~/repos/dotfiles/claude/bin/regenerate-manual {orgs}` (run from the repository root) and stage "
        "the .texi. @comma{} and spelled-out @ref titles are what current Org produces; commit them.")
    print(json.dumps({"hookSpecificOutput": {
        "hookEventName": "PreToolUse", "permissionDecision": "deny",
        "permissionDecisionReason": reason}}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
