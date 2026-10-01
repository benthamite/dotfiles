"""Paths and scan text of Codex Edit/Write/apply_patch calls.

Port of codex/hooks/lib-codex-paths.sh. Every function takes the decoded hook
payload and returns what the shell guards received from ``$(...)`` or a
``while read`` loop over the function's output. `codex_hook_input_json` (a
bare `cat` no hook called) is not ported.
"""

from __future__ import annotations

import functools
import locale
import re

from .codex_payload import command_substitution, jq_alternative, jq_raw, jq_text, tool_input, tool_name
from .core import HookFailure

_SECRET_SUFFIXES = (".zshenv-secrets", ".env.op", ".env.local")
_CLASSIFICATION_FAILED = "Secret patch classification failed; tool execution denied."

# Not a shell `[[:space:]]` set: lib-codex-paths.sh's embedded Python copied
# Rust's str::trim (Unicode White_Space), as Codex's apply_patch parser trims.
_RUST_WHITESPACE = (" \t\n\r\v\f\u0085\u00a0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009"
          "\u200a\u2028\u2029\u202f\u205f\u3000")


def _first(fields: dict, *keys: str) -> str:
    """`$(jq -r '.k1 // .k2 // ... // empty')` over FIELDS."""
    return command_substitution(jq_text(jq_alternative(*(fields.get(key) for key in keys))))


def codex_patch_text(payload: dict) -> str:
    return _first(tool_input(payload), "patch", "command", "input", "text")


def codex_direct_file_path(payload: dict) -> str:
    return _first(tool_input(payload), "file_path", "filePath", "path", "notebook_path")


def _patch_file_paths(patch: str) -> list[str]:
    """The awk scan of `*** Add/Update/Delete File:`, `*** Move to:` and
    `diff --git` lines. Each rule sees the line as earlier rules rewrote it."""
    found = []
    for line in patch.split("\n"):
        for kind in ("Add", "Update", "Delete"):
            header = f"*** {kind} File: "
            if line.startswith(header):
                line = line[len(header):]
                found.append(line)
                break
        if line.startswith("*** Move to: "):
            line = line[len("*** Move to: "):]
            found.append(line)
        if line.startswith("diff --git a/"):
            fields = re.split(r"[ \t]+", line.strip(" \t")) + ["", ""]
            old, new = fields[2], fields[3]
            old = old[2:] if old.startswith("a/") else old
            new = new[2:] if new.startswith("b/") else new
            found += [path for path in (old, new) if path != "/dev/null"]
    return found


def _collation_key(path: str):
    """`sort -u` order under the process's LC_COLLATE, ties broken by code
    point. The engine entry point calls `locale.setlocale(locale.LC_COLLATE,
    "")` once, so this matches the hook environment's collation."""
    return functools.cmp_to_key(locale.strcoll)(path), path


def codex_changed_paths(payload: dict) -> list[str]:
    """The lines a `while read` loop got from `codex_changed_paths`: the
    direct path first (a newline in it starts another line), then the patch's
    paths in `sort -u` order.

    Intended difference, stricter than `sort -u`: macOS collation gives most non-Latin
    characters no weight, so of two paths differing only in such characters
    `sort -u` kept one and the guards never saw the other. Only identical
    paths are merged here.
    """
    direct = codex_direct_file_path(payload)
    lines = direct.split("\n") if direct else []
    patch = codex_patch_text(payload)
    if not patch:
        return lines
    return lines + sorted(set(_patch_file_paths(patch)), key=_collation_key)


def _exempt(path: str) -> bool:
    return path.endswith(_SECRET_SUFFIXES)


def _project_patch(text: str) -> str:
    """apply_patch text minus the sections written to a proven secret-file
    destination; ValueError for anything the native parser would read
    differently or a move out of a secret store."""
    lines = [line.removesuffix("\r") for line in text.strip(_RUST_WHITESPACE).split("\n")]
    if not lines or lines[0].strip(_RUST_WHITESPACE) != "*** Begin Patch":
        raise ValueError
    source = destination = operation = ""
    section: list[str] = []
    output: list[str] = []
    moved = body = ended = False

    def flush():
        if source:
            if moved and _exempt(source) and not _exempt(destination):
                raise ValueError
            if not _exempt(destination):
                output.extend(section)

    for index, line in enumerate(lines[1:], 1):
        if ended:
            raise ValueError
        metadata = line.rstrip(_RUST_WHITESPACE) if operation == "Update" else line.strip(_RUST_WHITESPACE)
        # The native parser also trims both ends of the final End marker.
        if index == len(lines) - 1 and line.strip(_RUST_WHITESPACE) == "*** End Patch":
            metadata = "*** End Patch"
        if metadata == "*** End Patch":
            flush()
            ended = True
            continue
        header = next((kind for kind in ("Add", "Update", "Delete")
                       if metadata.startswith("*** " + kind + " File: ")), None)
        if header:
            flush()
            operation = header
            source = destination = metadata[len("*** " + header + " File: "):]
            if not source:
                raise ValueError
            section = [line]
            moved = body = False
            continue
        if metadata.startswith("*** Move to: "):
            if operation != "Update" or moved or body:
                raise ValueError
            destination = metadata[len("*** Move to: "):]
            if not destination:
                raise ValueError
            moved = True
        elif not source or operation == "Delete":
            raise ValueError
        elif operation == "Add" and not line.startswith("+"):
            raise ValueError
        elif operation == "Update" and metadata and not (
                line.startswith((" ", "+", "-")) or metadata == "@@" or
                metadata.startswith("@@ ") or metadata == "*** End of File"):
            raise ValueError
        else:
            body = True
        section.append(line)
    if not ended:
        raise ValueError
    return "\n".join(output)


def codex_patch_content_for_scan(payload: dict) -> str:
    """The text the secret guard scans for an Edit/Write/apply_patch call.

    Raises HookFailure where the shell function exited nonzero: an
    unclassifiable apply_patch, or a content/new_string that `join` rejects.
    """
    fields = tool_input(payload)
    direct = codex_direct_file_path(payload)
    patch = codex_patch_text(payload)
    if command_substitution(tool_name(payload)) == "apply_patch":
        try:
            return command_substitution(_project_patch(patch + "\n"))
        except ValueError:
            raise HookFailure(_CLASSIFICATION_FAILED) from None
    # Native Write/Edit inputs have one destination. Embedded patch-looking
    # content must not introduce another destination or gain its exemption.
    if _exempt(direct):
        return ""
    parts = []
    for key in ("content", "new_string"):
        value = fields.get(key)
        if value is None or value is False:
            value = ""
        if isinstance(value, (dict, list)):
            raise HookFailure(f"jq join cannot add {type(value).__name__} {key}")
        parts.append(jq_raw(value))
    return command_substitution(f"{direct}\n{patch}\n" + "\n".join(parts))
