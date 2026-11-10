#!/usr/bin/env python3
"""Decide whether an Elisp diff only changes text no manual describes.

Usage: lib-elisp-string-change.py REPO_ROOT < UNIFIED_DIFF

The diff is one file's `git diff --unified=0` output, headers included. Exit 0
when every change replaces the contents of string literals and leaves every
other token (symbols, numbers, parentheses, comments) as it was, and when the
replaced text appears in no manual of the repository. Exit 1 otherwise, and on
any error, so the documentation gate keeps blocking.

A string-only change can still alter behavior a manual describes, such as a
documented default value. The manual search catches the case where the manual
quotes the old text; prose that paraphrases a value without quoting it is not
detected. Searching every manual the gate recognizes, rather than guessing the
owning one, errs toward requiring documentation.
"""

from __future__ import annotations

import re
import subprocess
import sys

HUNK = re.compile(r"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@")
INDEX = re.compile(r"^index ([0-9a-f]+)\.\.([0-9a-f]+)")
DELIMITERS = set("()[]'`,")
MANUALS = ("doc/*.org", ":(glob)**/doc/*.org", "README.org", "README.md",
           ":(glob)**/README.org")
WORD = re.compile(r"\w")


def main() -> int:
    root = sys.argv[1]
    diff = sys.stdin.read()
    old = old_contents(root, diff)
    if old is None:
        return 1
    new = apply_hunks(old, diff)
    if new is None:
        return 1
    pairs = changed_strings(tokenize(old), tokenize(new))
    if not pairs:
        return 1
    for before, after in pairs:
        terms = search_terms(before, after)
        if not terms or any(in_manual(root, term) for term in terms):
            return 1
    return 0


def old_contents(root: str, diff: str) -> str | None:
    """Return the pre-change file named by DIFF's index line in ROOT."""
    for line in diff.splitlines():
        match = INDEX.match(line)
        if match:
            blob = match.group(1)
            if not blob.strip("0"):
                return None  # A new file is never a value-only change.
            return subprocess.run(
                ["git", "-C", root, "cat-file", "blob", blob],
                capture_output=True, text=True, check=True).stdout
    return None


def apply_hunks(old: str, diff: str) -> str | None:
    """Return OLD with DIFF's zero-context hunks applied, or None on mismatch."""
    lines = old.split("\n")
    if old.endswith("\n"):
        lines.pop()
    result: list[str] = []
    cursor = 0
    hunks = parse_hunks(diff)
    if not hunks:
        return None
    for start, count, removed, added in hunks:
        keep = start if count == 0 else start - 1
        if keep < cursor or keep + count > len(lines):
            return None
        result.extend(lines[cursor:keep])
        if lines[keep:keep + count] != removed:
            return None
        result.extend(added)
        cursor = keep + count
    result.extend(lines[cursor:])
    return "\n".join(result) + "\n"


def parse_hunks(diff: str) -> list[tuple[int, int, list[str], list[str]]]:
    """Return DIFF's hunks as (old start, old count, removed, added)."""
    hunks: list[tuple[int, int, list[str], list[str]]] = []
    in_hunk = False
    # Split on newlines only: splitlines() would also split on the form feeds
    # that separate Elisp pages, misaligning the hunks.
    for line in diff.split("\n"):
        match = HUNK.match(line)
        if match:
            count = 1 if match.group(2) is None else int(match.group(2))
            hunks.append((int(match.group(1)), count, [], []))
            in_hunk = True
        elif in_hunk and line.startswith("-"):
            hunks[-1][2].append(line[1:])
        elif in_hunk and line.startswith("+"):
            hunks[-1][3].append(line[1:])
    return hunks


def tokenize(text: str) -> list[tuple[str, str]]:
    """Return TEXT's Elisp tokens as (kind, source text) pairs."""
    tokens: list[tuple[str, str]] = []
    i = 0
    while i < len(text):
        char = text[i]
        if char.isspace():
            i += 1
        elif char == ";":
            end = text.find("\n", i)
            end = len(text) if end == -1 else end
            tokens.append(("comment", text[i:end]))
            i = end
        elif char == '"':
            end = string_end(text, i + 1)
            tokens.append(("string", text[i + 1:end]))
            i = end + 1
        elif char in DELIMITERS:
            tokens.append(("delimiter", char))
            i += 1
        else:
            end = atom_end(text, i)
            tokens.append(("atom", text[i:end]))
            i = end
    return tokens


def string_end(text: str, i: int) -> int:
    """Return the index of the quote closing the string whose body starts at I."""
    while i < len(text):
        if text[i] == "\\":
            i += 2
        elif text[i] == '"':
            return i
        else:
            i += 1
    raise ValueError("unterminated string")


def atom_end(text: str, i: int) -> int:
    """Return the index just past the symbol, number or character at I."""
    if text[i] == "?":
        # A character literal such as ?\" or ?( takes its next character
        # whatever it is.
        i += 3 if text[i + 1:i + 2] == "\\" else 2
    while i < len(text):
        char = text[i]
        if char == "\\":
            i += 2
        elif char.isspace() or char in DELIMITERS or char in '";':
            return i
        else:
            i += 1
    return len(text)


def changed_strings(old, new) -> list[tuple[str, str]]:
    """Return the (old, new) string pairs that differ between OLD and NEW.

    Return an empty list when any non-string token differs.
    """
    if len(old) != len(new):
        return []
    pairs = []
    for before, after in zip(old, new):
        if before[0] != after[0]:
            return []
        if before == after:
            continue
        if before[0] != "string":
            return []
        pairs.append((before[1], after[1]))
    return pairs


def search_terms(before: str, after: str) -> list[str]:
    """Return the text of BEFORE a manual would quote if it described it.

    These are BEFORE's source text when it fits on one line, and the whole
    words of decoded BEFORE that AFTER replaces, one term per line.
    """
    terms = [] if "\n" in before or not before.strip() else [before]
    old, new = decode(before), decode(after)
    start = common_prefix(old, new)
    end = len(old) - common_prefix(old[start:][::-1], new[start:][::-1])
    while start > 0 and WORD.match(old[start - 1]):
        start -= 1
    while end < len(old) and WORD.match(old[end]):
        end += 1
    changed = [piece.strip() for piece in old[start:end].split("\n")]
    terms.extend(piece for piece in changed if piece)
    return terms


def decode(source: str) -> str:
    """Return the string value of string literal body SOURCE."""
    escapes = {"n": "\n", "t": "\t", "\n": ""}
    out = []
    i = 0
    while i < len(source):
        if source[i] == "\\" and i + 1 < len(source):
            out.append(escapes.get(source[i + 1], source[i + 1]))
            i += 2
        else:
            out.append(source[i])
            i += 1
    return "".join(out)


def common_prefix(a: str, b: str) -> int:
    """Return the length of the common prefix of A and B."""
    n = 0
    while n < len(a) and n < len(b) and a[n] == b[n]:
        n += 1
    return n


def in_manual(root: str, term: str) -> bool:
    """Return True when TERM occurs in a staged manual of ROOT."""
    result = subprocess.run(
        ["git", "-C", root, "grep", "--cached", "-q", "-F", "-e", term, "--",
         *MANUALS],
        capture_output=True, text=True, check=False)
    if result.returncode not in (0, 1):
        raise RuntimeError(result.stderr)
    return result.returncode == 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception:  # Fail closed: an unreadable change needs documentation.
        sys.exit(1)
