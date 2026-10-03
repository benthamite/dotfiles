#!/usr/bin/env python3
"""Print the last user and assistant text messages of a Claude or Codex transcript.

Read-only: transcripts are opened for reading and never modified.

Usage:
  session_tail.py TRANSCRIPT.jsonl [-n N]
  session_tail.py --latest PROJECT_DIR [--codex] [--exclude SESSION_ID] [-n N]

With --latest, PROJECT_DIR is the peer session's working directory (or a
Claude transcript directory). Claude transcripts are looked up in
~/.claude/projects/<slug>/, where the slug replaces every character other than
a letter, digit or "-" with "-". With --codex, the newest rollout under
~/.codex/sessions/ whose session_meta cwd equals PROJECT_DIR is used.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from collections import deque
from datetime import datetime
from pathlib import Path

CLAUDE_PROJECTS = Path.home() / ".claude" / "projects"
CODEX_SESSIONS = Path.home() / ".codex" / "sessions"
CODEX_SCAN_LIMIT = 400  # newest rollouts inspected when matching a cwd
INJECTED_PREFIXES = ("# AGENTS.md instructions", "<environment_context>",
                     "<user_instructions>", "<permissions instructions>")


def local_time(ts: str) -> str:
    try:
        return datetime.fromisoformat(ts.replace("Z", "+00:00")).astimezone().strftime("%Y-%m-%d %H:%M:%S")
    except ValueError:
        return ts


def claude_slug(cwd: str) -> str:
    return re.sub(r"[^A-Za-z0-9-]", "-", str(Path(cwd).expanduser().resolve()))


def clean(text: str) -> str:
    text = re.sub(r"<system-reminder>.*?</system-reminder>", "", text, flags=re.S)
    return text.strip()


def texts(content, kinds) -> str:
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return "\n".join(part.get("text", "") for part in content
                         if isinstance(part, dict) and part.get("type") in kinds)
    return ""


def claude_messages(records):
    for r in records:
        role = r.get("type")
        if role not in ("user", "assistant") or r.get("isMeta"):
            continue
        content = (r.get("message") or {}).get("content")
        yield r.get("timestamp", ""), role, texts(content, {"text"})


def codex_messages(records):
    events, items = [], []
    for r in records:
        p = r.get("payload") or {}
        ts = r.get("timestamp", "")
        if r.get("type") == "event_msg":
            kind = p.get("type")
            if kind in ("user_message", "agent_message"):
                role = "user" if kind == "user_message" else "assistant"
                events.append((ts, role, p.get("message", "")))
            elif kind == "item_completed":
                item = p.get("item") or {}
                role = {"UserMessage": "user", "AgentMessage": "assistant"}.get(item.get("type"))
                if role:
                    events.append((ts, role, texts(item.get("content"), {"text", "Text"})))
        elif r.get("type") == "response_item" and p.get("type") == "message":
            role = p.get("role")
            if role in ("user", "assistant"):
                text = texts(p.get("content"), {"input_text", "output_text"})
                if not text.lstrip().startswith(INJECTED_PREFIXES):
                    items.append((ts, role, text))
    # Event records carry the clean conversation; response items duplicate them
    # and are used only for rollouts that predate event-level messages.
    return events or items


def read_records(path: Path):
    with path.open("r", encoding="utf-8", errors="replace") as fh:
        for line in fh:
            try:
                yield json.loads(line)
            except json.JSONDecodeError:
                continue


def is_codex(path: Path) -> bool:
    for r in read_records(path):
        return r.get("type") == "session_meta"
    return False


def title_and_cwd(path: Path, codex: bool):
    title = cwd = None
    for r in read_records(path):
        if codex and r.get("type") == "session_meta":
            cwd = (r.get("payload") or {}).get("cwd")
        elif not codex:
            if r.get("type") in ("custom-title", "ai-title"):
                title = r.get("customTitle") or r.get("aiTitle") or r.get("title") or title
            cwd = r.get("cwd") or cwd
    return title, cwd


def latest(project: str, codex: bool, exclude: str | None) -> Path:
    def keep(p: Path) -> bool:
        return not (exclude and exclude in p.name)

    if codex:
        target = str(Path(project).expanduser().resolve())
        rollouts = sorted(CODEX_SESSIONS.glob("*/*/*/rollout-*.jsonl"),
                          key=lambda p: p.stat().st_mtime, reverse=True)
        for p in rollouts[:CODEX_SCAN_LIMIT]:
            if not keep(p):
                continue
            for r in read_records(p):
                if r.get("type") == "session_meta" and (r.get("payload") or {}).get("cwd") == target:
                    return p
                break
        sys.exit(f"no Codex rollout with cwd {target} among the newest {CODEX_SCAN_LIMIT}")
    d = Path(project).expanduser()
    if not any(d.glob("*.jsonl")):
        d = CLAUDE_PROJECTS / claude_slug(project)
    candidates = [p for p in d.glob("*.jsonl") if keep(p)]
    if not candidates:
        sys.exit(f"no Claude transcripts in {d}")
    return max(candidates, key=lambda p: p.stat().st_mtime)


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("transcript", nargs="?", help="transcript JSONL path")
    ap.add_argument("--latest", metavar="PROJECT_DIR", help="pick the newest transcript for this project")
    ap.add_argument("--codex", action="store_true", help="with --latest, search Codex rollouts")
    ap.add_argument("--exclude", metavar="SESSION_ID", help="with --latest, skip this session (e.g. your own)")
    ap.add_argument("-n", type=int, default=8, help="messages to print (default 8)")
    ap.add_argument("--chars", type=int, default=700, help="truncate each message (default 700)")
    args = ap.parse_args()
    if bool(args.transcript) == bool(args.latest):
        ap.error("give exactly one of TRANSCRIPT or --latest")

    path = Path(args.transcript).expanduser() if args.transcript else latest(args.latest, args.codex, args.exclude)
    codex = is_codex(path)
    records = read_records(path)
    msgs = codex_messages(records) if codex else claude_messages(records)
    tail = deque(((ts, role, clean(t)) for ts, role, t in msgs if clean(t)), maxlen=args.n)

    title, cwd = title_and_cwd(path, codex)
    mtime = datetime.fromtimestamp(path.stat().st_mtime).strftime("%Y-%m-%d %H:%M:%S")
    print(f"# {'Codex' if codex else 'Claude'} transcript {path}")
    print(f"# last written {mtime}" + (f"; cwd {cwd}" if cwd else "") + (f"; title {title!r}" if title else ""))
    for ts, role, text in tail:
        if len(text) > args.chars:
            text = text[: args.chars].rstrip() + " [...]"
        print(f"\n[{local_time(ts)}] {role}:\n{text}")


if __name__ == "__main__":
    main()
