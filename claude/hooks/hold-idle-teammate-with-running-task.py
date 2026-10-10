#!/usr/bin/env python3
"""TeammateIdle hook: keep a teammate working while its own background task runs.

Claude Code can queue a teammate's task-completion notice without ever
delivering it.  A teammate that went idle to wait for that notice then never
wakes, and its work silently stalls (2026-10-10, Epoch/meetings).  So a
teammate may not go idle while a background shell it started is still
running: the hook refuses (exit 2) and tells it to wait in the foreground.
A task that ended without the teammate receiving its notice is reported to
the teammate once, with its output path, and then left alone.

A task is running while some process holds its output file open for writing
(lsof access mode w or u).  Text the command prints, such as a copy of
"[exited with code N]", plays no part.

Unknown or missing inputs allow the idle and are reported on stderr; they
never block.

See docs/plans/2026-10-10-teammate-idle-with-running-task.md.
"""

from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys

LAUNCH = (
    re.compile(r"Command running in background with ID: (\w+)\. "
               r"Output is being written to: (\S+?\.output)"),
    re.compile(r"moved to the background \(ID: (\w+)\)\. "
               r"Output is being written to: (\S+?\.output)"),
)
AGENT_ID = re.compile(r"\A[\w-]+\Z")


def note(message: str) -> None:
    print(f"hold-idle-teammate-with-running-task: {message}", file=sys.stderr)


def state_file(session: str, agent: str) -> Path:
    configured = os.environ.get("AGENT_TEAMMATE_IDLE_STATE_DIR")
    base = (Path(configured) if configured
            else Path.home() / ".local" / "state" / "agent-teammate-idle")
    base.mkdir(parents=True, exist_ok=True)
    return base / (hashlib.sha256(f"{session}:{agent}".encode()).hexdigest() + ".json")


def strings(value):
    """Yield every string inside a decoded JSON VALUE."""
    if isinstance(value, str):
        yield value
    elif isinstance(value, list):
        for item in value:
            yield from strings(item)
    elif isinstance(value, dict):
        for item in value.values():
            yield from strings(item)


def teammate_transcript(payload: dict, agent: str) -> Path | None:
    main = payload.get("transcript_path")
    if not isinstance(main, str) or not main.endswith(".jsonl"):
        return None
    subagents = Path(main[:-len(".jsonl")]) / "subagents"
    for name in (f"agent-{agent}.jsonl", f"agent-a{agent}.jsonl"):
        if (subagents / name).is_file():
            return subagents / name
    return None


def scan(transcript: Path):
    """Return (launches, notified, stopped) recorded in TRANSCRIPT.
    LAUNCHES maps task id to output path, in launch order."""
    launches: dict[str, str] = {}
    notified: set[str] = set()
    stopped: set[str] = set()
    for line in transcript.open(errors="replace"):
        try:
            record = json.loads(line)
        except ValueError:
            continue
        message = record.get("message")
        if not isinstance(message, dict):
            continue
        content = message.get("content")
        if record.get("type") == "user":
            for text in strings(content):
                for pattern in LAUNCH:
                    for task, path in pattern.findall(text):
                        launches.setdefault(task, path)
                notified.update(re.findall(r"<task-notification>\s*<task-id>(\w+)</task-id>", text))
        elif record.get("type") == "assistant" and isinstance(content, list):
            for block in content:
                if (isinstance(block, dict) and block.get("type") == "tool_use"
                        and block.get("name") == "TaskStop"):
                    task = (block.get("input") or {}).get("task_id")
                    if isinstance(task, str):
                        stopped.add(task)
    return launches, notified, stopped


def has_writer(path: str) -> bool:
    """Return whether some process holds PATH open for writing."""
    result = subprocess.run(["lsof", "-F", "a", path], capture_output=True, text=True)
    return any(line.startswith("a") and ("w" in line or "u" in line)
               for line in result.stdout.splitlines())


def main() -> int:
    try:
        payload = json.load(sys.stdin)
    except ValueError as err:
        note(f"unreadable payload: {err}")
        return 0
    if not isinstance(payload, dict):
        return 0
    agent = payload.get("agent_id")
    session = payload.get("session_id")
    if not (isinstance(agent, str) and AGENT_ID.match(agent) and isinstance(session, str)):
        note("payload has no usable agent_id or session_id")
        return 0
    transcript = teammate_transcript(payload, agent)
    if transcript is None:
        note(f"no transcript found for teammate {agent}")
        return 0
    launches, notified, stopped = scan(transcript)
    state = state_file(session, agent)
    try:
        acknowledged = set(json.loads(state.read_text())) if state.exists() else set()
    except ValueError:
        acknowledged = set()

    running, ended = [], []
    for task, path in launches.items():
        if task in notified or task in stopped or task in acknowledged:
            continue
        if Path(path).exists() and has_writer(path):
            running.append((task, path))
        else:
            ended.append((task, path))

    if not running and not ended:
        return 0
    lines = []
    if running:
        lines.append("Do not go idle: these background tasks you started are still running, "
                     "and their completion notices may never reach you.")
        for task, path in running:
            lines.append(f"- {task}: output {path}")
        lines.append(
            "Wait for each in the foreground: call Bash with timeout 600000 and a command such as "
            "`for i in $(seq 1 100); do lsof -F a PATH | grep -q '^a.*[wu]' || break; sleep 5; done; "
            "tail -40 PATH`. Repeat until no process writes the file, then read the output and carry "
            "on. If you no longer need a task, stop it with TaskStop instead.")
    if ended:
        lines.append("These background tasks you started have ended, but their completion notices "
                     "never reached you. Read their output and act on it:")
        for task, path in ended:
            lines.append(f"- {task}: output {path}" + ("" if Path(path).exists() else " (file missing)"))
        acknowledged.update(task for task, _ in ended)
        state.write_text(json.dumps(sorted(acknowledged)))
    print("\n".join(lines), file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main())
