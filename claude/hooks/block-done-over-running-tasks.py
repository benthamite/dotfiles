#!/usr/bin/env python3
"""Stop hook: refuse `Status: done' while background tasks are running.

An agent that ends a turn with `Status: done' has declared that nothing is
left open, yet Claude Code may still list background shells, Monitors or
background agents it started, including ones its subagents left behind.  A
task that never exits sends no notification, so nothing else makes the agent
notice it, and agent.el shows the finished session as busy.  This hook
refuses that stop and names the tasks, so the agent either stops them or
reports the work as still open.

Teammates are left out: Claude Code reports an idle teammate as running.

A turn is refused at most MAX_BLOCKS times.  The count is this hook's own,
keyed by session and prompt, because `stop_hook_active' is also set by other
Stop hooks' continuations.  Once it is used up the stop is allowed with a
message to the user naming the tasks still running.

See docs/plans/2026-10-10-stale-background-jobs.md.
"""

from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import re
import sys
import time

MAX_BLOCKS = 2
COUNTER_EXPIRY_SECONDS = 24 * 3600
STATUS_LINE = re.compile(r"^\W*Status:")
DONE_LINE = re.compile(r"^\W*Status:\W*done\W*$")


def state_dir() -> Path:
    configured = os.environ.get("AGENT_STOP_TASKS_STATE_DIR")
    base = (Path(configured) if configured
            else Path.home() / ".local" / "state" / "agent-stop-background-tasks")
    base.mkdir(parents=True, exist_ok=True)
    return base


def declares_done(message: str) -> bool:
    """Return whether MESSAGE's last status line is exactly `Status: done'."""
    status = [line.strip() for line in message.splitlines()
              if STATUS_LINE.match(line.strip())]
    return bool(status) and bool(DONE_LINE.match(status[-1]))


def running_tasks(payload: dict) -> list[dict]:
    return [task for task in payload["background_tasks"]
            if isinstance(task, dict)
            and task.get("status") == "running"
            and task.get("type") != "teammate"]


def describe(tasks: list[dict]) -> str:
    return "\n".join(
        f"- {task.get('id')} ({task.get('type')}): {task.get('description') or '(no description)'}"
        for task in tasks)


def counter_file(payload: dict) -> Path:
    key = f"{payload.get('session_id')}:{payload.get('prompt_id')}"
    directory = state_dir()
    now = time.time()
    for old in directory.glob("*.blocks"):
        if now - old.stat().st_mtime > COUNTER_EXPIRY_SECONDS:
            old.unlink(missing_ok=True)
    return directory / (hashlib.sha256(key.encode()).hexdigest() + ".blocks")


def main() -> int:
    try:
        payload = json.load(sys.stdin)
    except ValueError as err:
        print(f"block-done-over-running-tasks: unreadable payload: {err}", file=sys.stderr)
        return 0
    if not isinstance(payload, dict):
        return 0
    if not declares_done(payload.get("last_assistant_message") or ""):
        return 0
    if not isinstance(payload.get("background_tasks"), list):
        print("block-done-over-running-tasks: Stop payload has no background_tasks list",
              file=sys.stderr)
        return 0
    tasks = running_tasks(payload)
    counter = counter_file(payload)
    if not tasks:
        counter.unlink(missing_ok=True)
        return 0
    blocks = int(counter.read_text() or 0) if counter.exists() else 0
    if blocks >= MAX_BLOCKS:
        counter.unlink(missing_ok=True)
        print(json.dumps({
            "systemMessage": ("The agent reported Status: done, but these background "
                              "tasks are still running:\n" + describe(tasks)),
        }))
        return 0
    counter.write_text(str(blocks + 1))
    print(json.dumps({
        "decision": "block",
        "reason": (
            "Your status line says `Status: done`, but these background tasks are "
            "still running:\n" + describe(tasks) + "\n"
            "For each one, either stop it with TaskStop because it is no longer "
            "needed (check its output first if its result matters), or keep it and "
            "change the status line to `Status: in progress: ...` or `Status: done, "
            "pending verification: ...` naming what you are waiting on."),
    }))
    return 0


if __name__ == "__main__":
    sys.exit(main())
