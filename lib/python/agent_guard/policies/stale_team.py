"""SendMessage to an agent bound to a stale team: Claude only.

Claude Code keeps one agent team per process at <config>/teams/session-<id8>/
(config.json lists the members; inboxes/<name>.json holds their mail). The
team is named after the session id the process started with, so after a
restart that resumes a session the process gets a new team while the
session id, and with it the payload's session_id, stays the old one. An agent
of the earlier process, resumed by name or agentId, is still bound to the old
team: its SendMessage to "team-lead" reports success but lands in the old
team's inbox, which nothing reads. On 2026-10-06 that stranded 17 reports for
two hours (current team session-655d8c5f, session and stale team c361463a).

The current team is therefore found from the process, not the session id:
<config>/sessions/<pid>.json records each live process's sessionId and
startedAt, and the process creates its team within a second before that
(measured 0.3-0.8 s over six live sessions). A SendMessage whose `to` names a
member (by name or agentId) of another of this session's teams (those its
subagent records name, and those it led) but not of the current team is
denied.
"main", cross-session peers (names in no team) and current-team members pass.

The policy only reads, and allows the call whenever it cannot decide (a missing
or unparsable file, no single current team): a wrong denial would block the
lead from its own agents, while a missed one only loses a report.
"""

from __future__ import annotations

import json
import os
from pathlib import Path

from ..core import CLAUDE, Call, Result

# A process's team is created at most this long before the process records startedAt.
TEAM_CREATION_WINDOW_MS = 5000


def _load(path: Path):
    with path.open(encoding="utf-8") as handle:
        return json.load(handle)


def config_dir(call: Call) -> Path | None:
    """The Claude config dir: the parent of `projects/` in transcript_path,
    else $CLAUDE_CONFIG_DIR, else ~/.claude."""
    transcript = call.raw.get("transcript_path")
    if isinstance(transcript, str) and "/projects/" in transcript:
        return Path(transcript.rsplit("/projects/", 1)[0])
    configured = call.env.get("CLAUDE_CONFIG_DIR")
    if configured:
        return Path(configured)
    home = call.env.get("HOME")
    return Path(home) / ".claude" if home else None


def _alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def session_start(config: Path, session_id: str) -> int | None:
    """startedAt (ms) of the one live process running SESSION_ID, else None."""
    starts = []
    for path in (config / "sessions").glob("*.json"):
        record = _load(path)
        pid = record.get("pid")
        if record.get("sessionId") == session_id and isinstance(pid, int) and _alive(pid):
            starts.append(record.get("startedAt"))
    if len(starts) != 1 or not isinstance(starts[0], int):
        return None
    return starts[0]


def teams(config: Path) -> dict[str, dict]:
    """Every team config, by team name."""
    found = {}
    for path in (config / "teams").glob("*/config.json"):
        team = _load(path)
        if not isinstance(team, dict) or not isinstance(team.get("name"), str):
            raise ValueError(f"malformed team config {path}")
        found[team["name"]] = team
    return found


def current_team(all_teams: dict[str, dict], started_at: int) -> str | None:
    """The one team created within TEAM_CREATION_WINDOW_MS before STARTED_AT."""
    names = [name for name, team in all_teams.items()
             if isinstance(team.get("createdAt"), int)
             and 0 <= started_at - team["createdAt"] <= TEAM_CREATION_WINDOW_MS]
    return names[0] if len(names) == 1 else None


def _member_keys(team: dict) -> set[str]:
    keys = set()
    for member in team.get("members") or ():
        if isinstance(member, dict):
            keys.update(value for value in (member.get("name"), member.get("agentId")) if isinstance(value, str))
    return keys


def session_agents(call: Call) -> dict[str, tuple[str, str, str]]:
    """This session's recorded agents: name and agentId -> (teamName, name, transcript path)."""
    transcript = call.raw.get("transcript_path")
    if not isinstance(transcript, str) or not transcript.endswith(".jsonl"):
        return {}
    agents = {}
    for meta in (Path(transcript[:-len(".jsonl")]) / "subagents").glob("agent-*.meta.json"):
        record = _load(meta)
        team = record.get("teamName") if isinstance(record, dict) else None
        if not isinstance(team, str):
            continue
        name = record.get("name")
        if not isinstance(name, str):
            continue
        agent_id = meta.name[len("agent-"):-len(".meta.json")]
        entry = (team, name, str(meta.with_name(f"agent-{agent_id}.jsonl")))
        agents[agent_id] = entry
        agents.setdefault(name, entry)
    return agents


def _decide(call: Call) -> Result:
    target = call.tool_input.get("to")
    if not isinstance(target, str) or not target or target == "main":
        return Result.allow()
    target = target.split(" [", 1)[0]    # "name [ref]"
    config = config_dir(call)
    if config is None or not call.session_id:
        return Result.allow()
    started_at = session_start(config, call.session_id)
    if started_at is None:
        return Result.allow()
    all_teams = teams(config)
    current = current_team(all_teams, started_at)
    if current is None or target in _member_keys(all_teams[current]):
        return Result.allow()
    agents = session_agents(call)
    # Only this session's teams: its agents' records and the teams it led. A
    # member of an unrelated session's team cannot be resumed from here, and
    # the name may belong to a live cross-session peer.
    own = {team for team, _, _ in agents.values()}
    own |= {name for name, team in all_teams.items() if team.get("leadSessionId") == call.session_id}
    recorded = agents.get(target)
    # An agentId from the subagent records ("aheartbeat-137d…") names its team member.
    keys = {target, recorded[1]} if recorded and recorded[0] != current else {target}
    stale = sorted(name for name in own - {current}
                   if name in all_teams and keys & _member_keys(all_teams[name]))
    if not stale:
        return Result.allow()
    transcript = f" Its transcript: {recorded[2]}." if recorded and os.path.exists(recorded[2]) else ""
    return Result.deny(
        f"BLOCKED: `{target}` is bound to a stale agent team ({', '.join(stale)}), not this process's current "
        f"team {current}. A resumed agent stays in its old team: its SendMessage to the lead reports success but "
        "lands in the old team's inbox, which nothing reads, so its reports would be lost. Spawn a fresh agent "
        f"instead and give it the old agent's state (its task and progress, or its transcript to read).{transcript}")


def decide(call: Call) -> Result:
    if call.runtime != CLAUDE or call.tool_name != "SendMessage":
        return Result.allow()
    try:
        return _decide(call)
    except (OSError, ValueError, TypeError, AttributeError):
        # Read-only and fail-open by design (module docstring); json.JSONDecodeError is a ValueError.
        return Result.allow()
