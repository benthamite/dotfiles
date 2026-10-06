"""Terminate the search processes an agent session left running.

On 2026-10-06 a `bfs` from an ended Claude session ran for 35 minutes,
orphaned (ppid 1), while agent searches held the disk at about 2 GB/s.
claude/hooks/session-search-cleanup.sh and codex/hooks/session-search-cleanup.sh
run this module at SessionEnd (Claude also at SessionStart). It terminates a
process only when its name (ps comm) is a search tool
(agent_guard.policies.search_root.SEARCH_TOOLS) and one of these ties it to an
ended session; a name alone never suffices, so Pablo's own searches survive:

- its environment carries the ending session's id: CLAUDE_CODE_SESSION_ID
  (Claude sets it in every Bash command, including those run by its
  background pty host) or CODEX_THREAD_ID (Codex's exec_command);
- Claude only, at SessionEnd: it descends from the Claude process running the
  hook, or sits in that process's terminal session (getsid) when Claude leads
  it, which covers the embedded ugrep and bfs of the Grep and Glob tools after
  their parent exits;
- Claude only, at either event: its environment names, in CLAUDE_PID, a Claude
  process that is gone (absent, no longer Claude, or started after the
  search). Claude does not run SessionEnd when a signal ends it (as killing its
  Emacs buffer does), so this is how such a session's searches are found: by
  the next session that starts or ends.

Codex sets no owner pid, and one app-server process can host several threads,
so for Codex only the thread-id match applies; searches orphaned by a Codex
process that died without SessionEnd are not found.

Reading another process's environment needs KERN_PROCARGS2 (same user only).
Only the id variables are extracted; nothing else leaves this module. macOS
withholds the environment of Apple platform binaries (/usr/bin/find,
/usr/bin/grep), so those are found only by the Claude ancestry rules; Claude's
own `find` and `grep` (its embedded bfs and ugrep) and Homebrew's rg and fd
are readable.
Every termination is logged to LOG. The hook never fails the session: any
error is reported on stderr and the exit status is 0.
"""

from __future__ import annotations

import ctypes
import json
import os
import re
import signal
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from agent_guard.policies.search_root import SEARCH_TOOLS  # noqa: E402

LOG = Path.home() / ".local/state/agent-guard/search-cleanup.log"
GRACE = 0.4    # seconds between SIGTERM and SIGKILL; Codex gives SessionEnd hooks 1 s
_CTL_KERN, _KERN_ARGMAX, _KERN_PROCARGS2 = 1, 8, 49
_ETIME = re.compile(r"(?:(?:(\d+)-)?(\d+):)?(\d+):(\d+)")
_SESSION_VARIABLE = {"claude": "CLAUDE_CODE_SESSION_ID", "codex": "CODEX_THREAD_ID"}


@dataclass
class Proc:
    pid: int
    ppid: int
    age: int      # seconds since start (ps etime)
    comm: str

    @property
    def name(self) -> str:
        return os.path.basename(self.comm)


def _seconds(etime: str) -> int | None:
    match = _ETIME.fullmatch(etime)
    if not match:
        return None
    days, hours, minutes, seconds = (int(part or 0) for part in match.groups())
    return ((days * 24 + hours) * 60 + minutes) * 60 + seconds


def process_table() -> dict[int, Proc]:
    """Every process, from ps's safe columns (no arguments or environment)."""
    output = subprocess.run(["/bin/ps", "-axo", "pid=,ppid=,etime=,comm="], stdout=subprocess.PIPE,
                            stderr=subprocess.DEVNULL, text=True, errors="surrogateescape", check=True).stdout
    table = {}
    for line in output.splitlines():
        fields = line.split(None, 3)
        if len(fields) == 4 and fields[0].isdigit() and fields[1].isdigit():
            age = _seconds(fields[2])
            if age is not None:
                table[int(fields[0])] = Proc(int(fields[0]), int(fields[1]), age, fields[3])
    return table


def environment_values(pid: int, names: tuple[str, ...]) -> dict[str, str]:
    """NAMES' values in PID's initial environment, or {} when it is unreadable."""
    libc = ctypes.CDLL(None, use_errno=True)
    argmax = ctypes.c_int()
    size = ctypes.c_size_t(ctypes.sizeof(argmax))
    mib = (ctypes.c_int * 2)(_CTL_KERN, _KERN_ARGMAX)
    if libc.sysctl(mib, 2, ctypes.byref(argmax), ctypes.byref(size), None, 0) != 0:
        return {}
    buffer = ctypes.create_string_buffer(argmax.value)
    size = ctypes.c_size_t(argmax.value)
    mib = (ctypes.c_int * 3)(_CTL_KERN, _KERN_PROCARGS2, pid)
    if libc.sysctl(mib, 3, buffer, ctypes.byref(size), None, 0) != 0:
        return {}
    data = buffer.raw[:size.value]
    if len(data) < 4:
        return {}
    argc = int.from_bytes(data[:4], sys.byteorder)
    # argc, the executable path, NUL padding, argc arguments, then the environment.
    strings = [part for part in data[4:].split(b"\0") if part]
    prefixes = {name: name.encode() + b"=" for name in names}
    values = {}
    for entry in strings[1 + argc:]:
        for name, prefix in prefixes.items():
            if entry.startswith(prefix) and name not in values:
                values[name] = entry[len(prefix):].decode("utf-8", "replace")
    return values


def _is_claude(proc: Proc | None) -> bool:
    return proc is not None and (proc.name == "claude" or "/claude/versions/" in proc.comm)


def _is_agent(proc: Proc, runtime: str) -> bool:
    return _is_claude(proc) if runtime == "claude" else proc.name == "codex"


def agent_process(table: dict[int, Proc], runtime: str, start: int) -> Proc | None:
    """The nearest ancestor of START (the hook's parent) that is the agent."""
    pid, seen = start, set()
    while pid > 1 and pid not in seen and pid in table:
        seen.add(pid)
        if _is_agent(table[pid], runtime):
            return table[pid]
        pid = table[pid].ppid
    return None


def _descendants(table: dict[int, Proc], root: int) -> set[int]:
    children: dict[int, list[int]] = {}
    for proc in table.values():
        children.setdefault(proc.ppid, []).append(proc.pid)
    found, stack = set(), [root]
    while stack:
        for child in children.get(stack.pop(), ()):
            if child not in found:
                found.add(child)
                stack.append(child)
    return found


def _sid(pid: int) -> int | None:
    try:
        return os.getsid(pid)
    except OSError:
        return None


def _owner_gone(table: dict[int, Proc], search: Proc, owner_pid: str) -> bool:
    """True when CLAUDE_PID names no live Claude process that predates SEARCH.
    Ages are whole seconds, so a tie counts as alive."""
    if not owner_pid.isdigit():
        return False
    owner = table.get(int(owner_pid))
    return not _is_claude(owner) or owner.age < search.age


def targets(table: dict[int, Proc], runtime: str, event: str, session_id: str, hook_pid: int,
            environ=environment_values) -> list[tuple[Proc, str]]:
    """(process, why) for each search process the ended session owns."""
    protected, pid = set(), hook_pid
    while pid > 1 and pid in table and pid not in protected:
        protected.add(pid)
        pid = table[pid].ppid
    protected.add(hook_pid)
    agent = agent_process(table, runtime, table[hook_pid].ppid) if hook_pid in table else None
    tree: set[int] = set()
    leader = None
    if runtime == "claude" and event == "SessionEnd" and agent is not None:
        tree = _descendants(table, agent.pid)
        leader = agent.pid if _sid(agent.pid) == agent.pid else None
    variable = _SESSION_VARIABLE[runtime]
    found = []
    for proc in table.values():
        if proc.name not in SEARCH_TOOLS or proc.pid in protected:
            continue
        values = environ(proc.pid, (variable, "CLAUDE_PID"))
        if event == "SessionEnd" and session_id and values.get(variable) == session_id:
            found.append((proc, f"{variable} is the ended session"))
        elif proc.pid in tree:
            found.append((proc, f"descends from the ending Claude process {agent.pid}"))
        elif leader is not None and _sid(proc.pid) == leader:
            found.append((proc, f"in the ending Claude process's terminal session {leader}"))
        elif runtime == "claude" and _owner_gone(table, proc, values.get("CLAUDE_PID", "")):
            found.append((proc, f"its Claude process {values['CLAUDE_PID']} is gone"))
    return found


def terminate(found: list[tuple[Proc, str]], grace: float = GRACE) -> list[int]:
    """SIGTERM each process, then SIGKILL those still running after GRACE."""
    signalled = []
    for proc, _ in found:
        try:
            os.kill(proc.pid, signal.SIGTERM)
            signalled.append(proc.pid)
        except ProcessLookupError:
            pass
    deadline = time.monotonic() + grace
    remaining = list(signalled)
    while remaining and time.monotonic() < deadline:
        time.sleep(0.05)
        remaining = [pid for pid in remaining if _alive(pid)]
    for pid in remaining:
        try:
            os.kill(pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
    return signalled


def _alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    # A zombie child of ours would also answer; these are never our children.
    return True


def _log(runtime: str, event: str, session_id: str, found: list[tuple[Proc, str]]) -> None:
    LOG.parent.mkdir(parents=True, exist_ok=True)
    stamp = time.strftime("%Y-%m-%dT%H:%M:%S%z")
    with LOG.open("a", encoding="utf-8") as log:
        for proc, why in found:
            log.write(f"{stamp} {runtime} {event} session={session_id} pid={proc.pid} name={proc.name} "
                      f"age={proc.age}s: {why}\n")


def main(runtime: str, dry_run: bool = False) -> int:
    """Hook entry point: the payload on stdin. DRY_RUN prints the targets
    (pid, name, reason) instead of terminating them."""
    try:
        payload = json.loads(sys.stdin.read() or "{}")
        if not isinstance(payload, dict):
            raise ValueError("payload is not an object")
        event = str(payload.get("hook_event_name") or "")
        session_id = str(payload.get("session_id") or "")
        if event not in ("SessionStart", "SessionEnd"):
            return 0
        found = targets(process_table(), runtime, event, session_id, os.getpid())
        if dry_run:
            for proc, why in found:
                print(f"{proc.pid} {proc.name} age={proc.age}s: {why}")
        elif found:
            terminate(found)
            _log(runtime, event, session_id, found)
    except Exception as error:  # noqa: BLE001 - cleanup must never fail the session
        print(f"session-search-cleanup: {type(error).__name__}: {error}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    # session_search_cleanup.py {claude|codex} [--dry-run] < payload
    sys.exit(main(sys.argv[1], dry_run="--dry-run" in sys.argv[2:]))
