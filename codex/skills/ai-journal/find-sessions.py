#!/usr/bin/env python3
"""List Claude Code and Codex sessions modified since a cutoff.

Usage:
    find-sessions.py [CUTOFF_ISO]

If CUTOFF_ISO is omitted, reads `last-run.txt` next to this script.
Exits 2 if no cutoff is available or parseable.

Output: one row per session, tab-separated:
    kind<TAB>start_iso<TAB>cwd<TAB>path<TAB>first_user_excerpt

`kind` is `claude` or `codex`. The excerpt is the first non-wrapper user
message in the session, truncated to 300 chars and flattened to one line.
"""

from __future__ import annotations

import json
import sys
from datetime import datetime
from pathlib import Path

HOME = Path.home()
CLAUDE_DIRS = [HOME / ".claude" / "projects", HOME / ".claude-epoch" / "projects"]
CODEX_DIR = HOME / ".codex" / "sessions"

EXCERPT_LEN = 300
# Wrapper/system content that doesn't reflect the user's intent.
WRAPPER_PREFIXES = (
    "<command-name>",
    "<local-command-",
    "<command-message>",
    "<command-args>",
    "Caveat: The messages below were generated",
    "# AGENTS.md",
    "<INSTRUCTIONS>",
    "<user-prompt-submit-hook>",
    "<system-reminder>",
)


def parse_iso(s: str) -> datetime | None:
    s = s.strip()
    if not s:
        return None
    if s.endswith("Z"):
        s = s[:-1] + "+00:00"
    try:
        return datetime.fromisoformat(s)
    except ValueError:
        return None


def is_wrapper(text: str) -> bool:
    t = text.lstrip()
    return any(t.startswith(p) for p in WRAPPER_PREFIXES)


def flatten_claude_content(content) -> str:
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        parts = []
        for c in content:
            if isinstance(c, dict) and c.get("type") == "text":
                parts.append(c.get("text", ""))
            elif isinstance(c, str):
                parts.append(c)
        return "\n".join(parts)
    return ""


def first_user_claude(path: Path):
    try:
        with path.open("r", encoding="utf-8", errors="replace") as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                try:
                    d = json.loads(line)
                except json.JSONDecodeError:
                    continue
                if d.get("type") != "user":
                    continue
                msg = d.get("message", {})
                if not isinstance(msg, dict) or msg.get("role") != "user":
                    continue
                text = flatten_claude_content(msg.get("content", "")).strip()
                if not text or is_wrapper(text):
                    continue
                return d.get("timestamp", ""), d.get("cwd", ""), text
    except OSError:
        return None
    return None


def first_user_codex(path: Path):
    try:
        cwd = ""
        with path.open("r", encoding="utf-8", errors="replace") as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                try:
                    d = json.loads(line)
                except json.JSONDecodeError:
                    continue
                t = d.get("type")
                if t == "session_meta":
                    cwd = d.get("payload", {}).get("cwd", cwd) or cwd
                    continue
                if t != "response_item":
                    continue
                p = d.get("payload", {})
                if p.get("type") != "message" or p.get("role") != "user":
                    continue
                items = p.get("content", [])
                parts = [
                    c.get("text", "")
                    for c in items
                    if isinstance(c, dict) and c.get("type") in ("input_text", "text")
                ]
                text = "\n".join(parts).strip()
                if not text or is_wrapper(text):
                    continue
                return d.get("timestamp", ""), cwd, text
    except OSError:
        return None
    return None


def shorten(text: str) -> str:
    one = " ".join(text.split())
    return one[:EXCERPT_LEN]


def main() -> int:
    skill_dir = Path(__file__).resolve().parent
    cutoff_str = sys.argv[1] if len(sys.argv) > 1 else None
    if not cutoff_str:
        last_run = skill_dir / "last-run.txt"
        if last_run.is_file():
            cutoff_str = last_run.read_text().strip()
    cutoff = parse_iso(cutoff_str) if cutoff_str else None
    if cutoff is None:
        print("ERROR: cutoff missing or unparseable", file=sys.stderr)
        return 2
    cutoff_epoch = cutoff.timestamp()

    rows = []
    seen: set[str] = set()

    def add(kind: str, paths, handler):
        for p in paths:
            try:
                real = str(p.resolve())
            except OSError:
                continue
            if real in seen:
                continue
            seen.add(real)
            try:
                if p.stat().st_mtime < cutoff_epoch:
                    continue
            except OSError:
                continue
            info = handler(p)
            if info is None:
                continue
            ts, cwd, text = info
            rows.append((kind, ts, cwd, str(p), shorten(text)))

    for base in CLAUDE_DIRS:
        if not base.is_dir():
            continue
        add("claude", base.rglob("*.jsonl"), first_user_claude)

    if CODEX_DIR.is_dir():
        add("codex", CODEX_DIR.rglob("rollout-*.jsonl"), first_user_codex)

    rows.sort(key=lambda r: r[1])
    for r in rows:
        print("\t".join(r))
    return 0


if __name__ == "__main__":
    sys.exit(main())
