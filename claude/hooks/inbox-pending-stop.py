#!/usr/bin/env python3
"""Stop hook: handle inbox-monitor messages that arrived while the session was busy.

The inbox monitor wakes a project's session only when it is idle, so a session
that works for a long stretch never sees the messages routed to its project in
the meantime. Before such a session stops, this hook checks the project's queue
and, if anything is waiting, keeps the turn going with an instruction to read
and handle it.

It applies only to agent.el sessions that accept wake-ups (`W` in agent-menu)
and whose directory is inside a monitored project. It fails open: any error, a
missing Emacs or a missing database lets the session stop. `stop_hook_active`
(a stop already continued by a hook) also lets it stop, so it cannot loop.
"""

import json
import os
import sqlite3
import subprocess
import sys
from pathlib import Path

MONITOR = Path.home() / "repos/inbox-monitor"
DB = Path.home() / ".local/share/inbox-monitor/inbox.db"


def main() -> int:
    try:
        payload = json.load(sys.stdin)
    except ValueError:
        return 0
    if payload.get("stop_hook_active"):
        return 0
    buffer = os.environ.get("CLAUDE_BUFFER_NAME")
    if not buffer or not DB.exists():
        return 0
    project = owning_project(payload.get("cwd") or os.getcwd())
    if not project or not accepts_wake_ups(buffer):
        return 0
    waiting = pending(project)
    if waiting:
        print(json.dumps({
            "decision": "block",
            "reason": (f"[inbox-monitor] {waiting} item(s) for project {project} arrived "
                       "while you were busy. Run `inbox-read --project "
                       f"{project}` and handle them under the project's rules before "
                       "finishing (this notice comes from the monitor, not from Pablo)."),
        }))
    return 0


def owning_project(cwd: str):
    """Return the name of the deepest monitored project whose repo holds CWD."""
    sys.path.insert(0, str(MONITOR))
    from inbox_monitor import config
    here = Path(cwd).expanduser().resolve()
    best, depth = None, -1
    for p in config.load_projects():
        if p.repo is None or p.fallback:
            continue
        repo = p.repo.expanduser().resolve()
        if (here == repo or repo in here.parents) and len(repo.parts) > depth:
            best, depth = p.name, len(repo.parts)
    return best


def accepts_wake_ups(buffer: str) -> bool:
    expr = ("(let ((b (get-buffer %s))) (and b (fboundp 'agent-session-accepts-wake-ups-p) "
            "(agent-session-accepts-wake-ups-p b) t))" % json.dumps(buffer))
    try:
        out = subprocess.run(["emacsclient", "--alternate-editor=/usr/bin/false", "--eval", expr],
                             capture_output=True, text=True, timeout=4)
    except (OSError, subprocess.SubprocessError):
        return False
    return out.stdout.strip() == "t"


def pending(project: str) -> int:
    conn = sqlite3.connect(f"file:{DB}?mode=ro", uri=True, timeout=5)
    try:
        messages = conn.execute("SELECT count(*) FROM deliveries WHERE project=? "
                                "AND state != 'acked'", (project,)).fetchone()[0]
        notices = conn.execute("SELECT count(*) FROM waits WHERE project=? AND notice_state "
                               "IN ('pending', 'notified')", (project,)).fetchone()[0]
    finally:
        conn.close()
    return messages + notices


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception:  # fail open: never trap a session because the check broke
        sys.exit(0)
