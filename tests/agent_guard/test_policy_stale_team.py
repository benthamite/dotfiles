import json
import os
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard import engine  # noqa: E402
from agent_guard.core import Call  # noqa: E402
from agent_guard.policies import stale_team  # noqa: E402

SESSION = "c361463a-de47-45b4-b547-14cbdf8adcfd"
STARTED = 1_791_304_079_662
ALIVE = os.getpid()


class StaleTeamCase(unittest.TestCase):
    """A config dir reproducing 2026-10-06: session c361463a resumed in a new
    process whose team is session-655d8c5f; the earlier process's team,
    session-c361463a, still lists heartbeat and pdf-browser."""

    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.config = Path(tmp.name) / ".claude-epoch"
        self.write("sessions/40424.json", {"pid": ALIVE, "sessionId": SESSION, "startedAt": STARTED})
        self.team("session-655d8c5f", "655d8c5f-7dc7", STARTED - 555, ["team-lead", "search-guard"])
        self.team("session-c361463a", SESSION, STARTED - 12_420_000, ["team-lead", "heartbeat", "pdf-browser"])
        self.team("session-ab12cd34", "ab12cd34-0000", STARTED - 99_000_000, ["team-lead", "reviewer"])
        self.transcript = self.config / f"projects/-Users-u-repo/{SESSION}.jsonl"
        self.write(f"projects/-Users-u-repo/{SESSION}.jsonl", {})
        self.agent("aheartbeat-137d302d1f36c1c7", "heartbeat", "session-c361463a")
        self.agent("apdf-browser-c7f67a51efc95515", "pdf-browser", "session-c361463a")
        self.agent("asearch-guard-63b92500ce564596", "search-guard", "session-655d8c5f")
        self.agent("a3d435f6e3f40dda6", None, None)

    def write(self, relative, data):
        path = self.config / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(data) if not isinstance(data, str) else data)
        return path

    def team(self, name, lead_session, created, members):
        self.write(f"teams/{name}/config.json", {
            "name": name, "createdAt": created, "leadSessionId": lead_session, "leadAgentId": f"team-lead@{name}",
            "members": [{"name": member, "agentId": f"{member}@{name}"} for member in members]})

    def agent(self, agent_id, name, team):
        base = f"projects/-Users-u-repo/{SESSION}/subagents/agent-{agent_id}"
        self.write(base + ".meta.json", {"name": name, "teamName": team, "agentType": name or "general-purpose"})
        self.write(base + ".jsonl", "")

    def decide(self, to, session=SESSION, transcript=True, tool="SendMessage", runtime="claude", env=None):
        payload = {"tool_name": tool, "tool_input": {"to": to, "message": "hi"}, "session_id": session,
                   "cwd": "/Users/u/repo"}
        if transcript:
            payload["transcript_path"] = str(self.transcript)
        return stale_team.decide(Call.from_payload(runtime, payload, env=env or {"HOME": "/nonexistent"}))


class DecisionTests(StaleTeamCase):
    def test_stale_members_denied_by_name_and_agent_id(self):
        for to in ("heartbeat", "pdf-browser", "heartbeat@session-c361463a", "aheartbeat-137d302d1f36c1c7",
                   "heartbeat [08a2f6]"):
            result = self.decide(to)
            self.assertEqual(result.decision, "deny", to)
            self.assertIn("stale agent team (session-c361463a)", result.reason)
            self.assertIn("current team session-655d8c5f", result.reason)
            self.assertIn("Spawn a fresh agent", result.reason)

    def test_denial_names_the_transcript(self):
        reason = self.decide("heartbeat").reason
        self.assertIn(f"{SESSION}/subagents/agent-aheartbeat-137d302d1f36c1c7.jsonl", reason)

    def test_allowed_targets(self):
        for to in ("main", "team-lead", "search-guard", "search-guard@session-655d8c5f",
                   "asearch-guard-63b92500ce564596", "a3d435f6e3f40dda6", "launchd-91", "worker [3fa9c1]", ""):
            self.assertEqual(self.decide(to).decision, "allow", to)

    def test_unrelated_sessions_teams_ignored(self):
        # `reviewer` belongs to another session's team; here it may be a cross-session peer.
        self.assertEqual(self.decide("reviewer").decision, "allow")

    def test_respawned_name_in_current_team_allowed(self):
        self.team("session-655d8c5f", "655d8c5f-7dc7", STARTED - 555, ["team-lead", "search-guard", "heartbeat"])
        self.assertEqual(self.decide("heartbeat").decision, "allow")
        # The old agentId still points at the stale agent.
        self.assertEqual(self.decide("aheartbeat-137d302d1f36c1c7").decision, "deny")

    def test_recorded_agent_without_team_config(self):
        # The stale team's config is gone: nothing to check against, so allow.
        self.agent("aold-1111", "old-worker", "session-0000dead")
        self.assertEqual(self.decide("old-worker").decision, "allow")
        self.assertEqual(self.decide("aold-1111").decision, "allow")

    def test_session_never_restarted(self):
        # Fresh process: the team is named after the session id itself.
        (self.config / "teams/session-655d8c5f/config.json").unlink()
        self.team("session-c361463a", SESSION, STARTED - 400, ["team-lead", "heartbeat", "pdf-browser"])
        for to in ("heartbeat", "pdf-browser", "aheartbeat-137d302d1f36c1c7"):
            self.assertEqual(self.decide(to).decision, "allow", to)

    def test_only_claude_sendmessage(self):
        self.assertEqual(self.decide("heartbeat", tool="Bash").decision, "allow")
        self.assertEqual(self.decide("heartbeat", runtime="codex").decision, "allow")

    def test_config_dir_from_environment(self):
        env = {"CLAUDE_CONFIG_DIR": str(self.config), "HOME": "/nonexistent"}
        self.assertEqual(self.decide("pdf-browser", transcript=False, env=env).decision, "deny")


class FailOpenTests(StaleTeamCase):
    def test_no_live_process_for_the_session(self):
        self.write("sessions/40424.json", {"pid": 999_999_999, "sessionId": SESSION, "startedAt": STARTED})
        self.assertEqual(self.decide("heartbeat").decision, "allow")

    def test_two_live_processes_for_the_session(self):
        self.write("sessions/50000.json", {"pid": os.getppid(), "sessionId": SESSION, "startedAt": STARTED + 9})
        self.assertEqual(self.decide("heartbeat").decision, "allow")

    def test_no_single_current_team(self):
        self.team("session-77777777", "77777777-0000", STARTED - 900, ["team-lead"])
        self.assertEqual(self.decide("heartbeat").decision, "allow")
        (self.config / "teams/session-77777777/config.json").unlink()
        (self.config / "teams/session-655d8c5f/config.json").unlink()
        self.assertEqual(self.decide("heartbeat").decision, "allow")

    def test_missing_or_broken_files(self):
        for broken in ("sessions/40424.json", "teams/session-655d8c5f/config.json",
                       "teams/session-c361463a/config.json"):
            with self.subTest(broken=broken):
                path = self.config / broken
                original = path.read_text()
                path.write_text("{not json")
                self.assertEqual(self.decide("heartbeat").decision, "allow")
                path.write_text("[]")
                self.assertEqual(self.decide("heartbeat").decision, "allow")
                path.write_text(original)
        self.assertEqual(self.decide("heartbeat", session="").decision, "allow")
        self.assertEqual(self.decide("heartbeat", transcript=False).decision, "allow")

    def test_non_string_target(self):
        payload = {"tool_name": "SendMessage", "tool_input": {"to": {"x": 1}}, "session_id": SESSION,
                   "transcript_path": str(self.transcript)}
        self.assertEqual(stale_team.decide(Call.from_payload("claude", payload)).decision, "allow")


class EngineTests(StaleTeamCase):
    def test_registered_for_claude_sendmessage(self):
        self.assertEqual(list(engine.policies_for("claude", "SendMessage")), [stale_team])
        payload = {"tool_name": "SendMessage", "tool_input": {"to": "heartbeat", "message": "hi"},
                   "session_id": SESSION, "transcript_path": str(self.transcript)}
        response = engine.render("claude", engine.decide(Call.from_payload("claude", payload)))
        self.assertEqual(response["hookSpecificOutput"]["permissionDecision"], "deny")


if __name__ == "__main__":
    unittest.main()
