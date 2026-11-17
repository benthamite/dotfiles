#!/usr/bin/env python3
"""Stop hook: refuse `Status: in progress' when nothing will resume the agent.

An agent that ends a turn stops.  `Status: in progress' is true only when a
background task or a scheduled wake-up the agent started will bring it back
without the user.  Written over nothing, it tells the user that work is under
way when none is (2026-11-17: three times in one session).  This hook refuses
that stop, so the agent either keeps working in the same turn or reports the
truth.

What counts as something that will resume the agent:
- a running background task in the Stop payload (shell, Monitor, background
  agent, workflow); teammates are left out, since Claude Code reports an idle
  one as running;
- a wake-up or scheduled trigger the agent set in this turn (a ScheduleWakeup
  or CronCreate call after the last user message in the transcript).

A turn is refused at most MAX_BLOCKS times, counted by session and prompt as
block-done-over-running-tasks.py does.  Once the count is used up the stop is
allowed with a message that tells the user nothing is running.
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
IN_PROGRESS_LINE = re.compile(r"^\W*Status:\W*in progress\b", re.IGNORECASE)
WAKE_TOOLS = {"ScheduleWakeup", "CronCreate"}
TRANSCRIPT_TAIL_BYTES = 4 * 1024 * 1024


def state_dir() -> Path:
    configured = os.environ.get("AGENT_STOP_PROGRESS_STATE_DIR")
    base = (Path(configured) if configured
            else Path.home() / ".local" / "state" / "agent-stop-in-progress")
    base.mkdir(parents=True, exist_ok=True)
    return base


def declares_in_progress(message: str) -> bool:
    """Return whether MESSAGE's last status line says `Status: in progress'."""
    status = [line.strip() for line in message.splitlines()
              if STATUS_LINE.match(line.strip())]
    return bool(status) and bool(IN_PROGRESS_LINE.match(status[-1]))


def running_tasks(payload: dict) -> list[dict]:
    return [task for task in payload["background_tasks"]
            if isinstance(task, dict)
            and task.get("status") == "running"
            and task.get("type") != "teammate"]


def is_user_prompt(record: dict) -> bool:
    """Return whether RECORD is something the user typed, not a tool result."""
    if record.get("type") != "user":
        return False
    content = (record.get("message") or {}).get("content")
    if isinstance(content, str):
        return True
    return isinstance(content, list) and not any(
        isinstance(part, dict) and part.get("type") == "tool_result" for part in content)


def wake_set_this_turn(transcript: str | None) -> bool:
    """Return whether the agent set a wake-up since the user's last message."""
    if not transcript:
        return False
    path = Path(transcript).expanduser()
    try:
        with path.open("rb") as handle:
            handle.seek(0, os.SEEK_END)
            handle.seek(max(0, handle.tell() - TRANSCRIPT_TAIL_BYTES))
            lines = handle.read().decode("utf-8", errors="replace").splitlines()
    except OSError:
        return False
    found = False
    for line in lines:
        try:
            record = json.loads(line)
        except ValueError:
            continue
        if not isinstance(record, dict):
            continue
        if is_user_prompt(record):
            found = False
        elif record.get("type") == "assistant":
            content = (record.get("message") or {}).get("content")
            if isinstance(content, list) and any(
                    isinstance(part, dict) and part.get("type") == "tool_use"
                    and part.get("name") in WAKE_TOOLS
                    and not (part.get("input") or {}).get("stop")
                    for part in content):
                found = True
    return found


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
        print(f"block-in-progress-with-nothing-running: unreadable payload: {err}",
              file=sys.stderr)
        return 0
    if not isinstance(payload, dict):
        return 0
    if not declares_in_progress(payload.get("last_assistant_message") or ""):
        return 0
    if not isinstance(payload.get("background_tasks"), list):
        print("block-in-progress-with-nothing-running: Stop payload has no "
              "background_tasks list", file=sys.stderr)
        return 0
    counter = counter_file(payload)
    if running_tasks(payload) or wake_set_this_turn(payload.get("transcript_path")):
        counter.unlink(missing_ok=True)
        return 0
    blocks = int(counter.read_text() or 0) if counter.exists() else 0
    if blocks >= MAX_BLOCKS:
        counter.unlink(missing_ok=True)
        print(json.dumps({
            "systemMessage": ("The agent reported Status: in progress, but nothing is "
                              "running that will resume it: it has stopped."),
        }))
        return 0
    counter.write_text(str(blocks + 1))
    print(json.dumps({
        "decision": "block",
        "reason": (
            "Your status line says `Status: in progress`, but no background task is "
            "running and you set no wake-up in this turn, so nothing will resume you: "
            "once this reply ends you stop. Either keep working now, in this turn, "
            "until the work is finished or genuinely blocked, or start the background "
            "task or wake-up you meant to rely on, or change the status line to what "
            "is true (`Status: waiting on you: ...`, `Status: done, pending "
            "verification: ...` or `Status: done`). Do not describe work you intend "
            "to do as work that is under way."),
    }))
    return 0


if __name__ == "__main__":
    sys.exit(main())
