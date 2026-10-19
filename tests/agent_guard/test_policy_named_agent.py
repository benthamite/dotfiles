import json
import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard import engine  # noqa: E402
from agent_guard.core import Call  # noqa: E402
from agent_guard.policies import named_agent  # noqa: E402


def decide(tool_input, tool="Agent", runtime="claude"):
    payload = {"tool_name": tool, "tool_input": tool_input, "session_id": "s1", "cwd": "/Users/u/repo"}
    return named_agent.decide(Call.from_payload(runtime, payload, env={"HOME": "/nonexistent"}))


class NamedAgentTests(unittest.TestCase):
    def test_named_agent_denied(self):
        result = decide({"description": "d", "prompt": "p", "name": "fi-tan103"})
        self.assertEqual(result.decision, "deny")
        self.assertIn("without `name`", result.reason)

    def test_unnamed_agent_allowed(self):
        for tool_input in ({"description": "d", "prompt": "p"}, {"description": "d", "prompt": "p", "name": ""},
                           {"description": "d", "prompt": "p", "name": "  "}):
            self.assertEqual(decide(tool_input).decision, "allow", tool_input)

    def test_other_tools_and_runtimes_ignored(self):
        self.assertEqual(decide({"name": "x"}, tool="SendMessage").decision, "allow")
        self.assertEqual(decide({"name": "x"}, runtime="codex").decision, "allow")

    def test_engine_routes_agent_calls(self):
        self.assertEqual(list(engine.policies_for("claude", "Agent")), [named_agent])
        payload = {"tool_name": "Agent", "tool_input": {"description": "d", "prompt": "p", "name": "w"},
                   "session_id": "s1", "cwd": "/Users/u/repo"}
        response = engine.render("claude", engine.decide(Call.from_payload("claude", payload)))
        self.assertIn("deny", json.dumps(response))


if __name__ == "__main__":
    unittest.main()
