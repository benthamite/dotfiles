import json
import os
import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import CLAUDE, CODEX, Call, HookFailure  # noqa: E402
from agent_guard.policies import untrusted_execution  # noqa: E402

CWD = str(Path.home())


def decide(runtime, payload):
    return untrusted_execution.decide(Call.from_payload(runtime, {"cwd": CWD, **payload}, env=dict(os.environ, PWD=CWD)))


class UntrustedExecutionTests(unittest.TestCase):
    def test_transient_runner_denied_in_both_runtimes(self):
        claude = decide(CLAUDE, {"tool_name": "Bash", "tool_input": {"command": "npx cowsay"}})
        codex = decide(CODEX, {"tool_name": "exec_command", "tool_input": {"cmd": "npx cowsay"}})
        self.assertEqual(claude.decision, "deny")
        self.assertEqual(claude.reason, codex.reason)
        self.assertTrue(claude.reason.startswith("BLOCKED: "))
        self.assertTrue(claude.reason.endswith(untrusted_execution.ADVICE))

    def test_ordinary_commands_and_other_tools_allowed(self):
        self.assertEqual(decide(CLAUDE, {"tool_name": "Bash", "tool_input": {"command": "git status"}}).decision, "allow")
        self.assertEqual(decide(CLAUDE, {"tool_name": "Read", "tool_input": {"command": "npx x"}}).decision, "allow")
        self.assertEqual(decide(CODEX, {"tool_name": "apply_patch", "tool_input": {"cmd": "npx x"}}).decision, "allow")

    def test_claude_payload_shape_fails_closed(self):
        for payload in ({"tool_name": 5, "tool_input": {"command": "ls"}},
                        {"tool_input": {"command": "ls"}},
                        {"tool_name": "Bash", "tool_input": {}},
                        {"tool_name": "Bash", "tool_input": {"command": "ls", "workdir": ""}},
                        {"tool_name": "Bash", "tool_input": {"command": "ls", "workdir": 3}}):
            with self.assertRaises(HookFailure, msg=json.dumps(payload)):
                decide(CLAUDE, payload)
        # `$(...)` dropped the trailing newline of the tool name.
        self.assertEqual(decide(CLAUDE, {"tool_name": "Bash\n", "tool_input": {"command": "npx x"}}).decision, "deny")

    def test_codex_payload_shape_fails_closed(self):
        for payload in ({"tool_input": {"cmd": "ls"}},
                        {"tool_name": "exec_command", "tool_input": {"cmd": 5}},
                        {"tool_name": "exec_command", "tool_input": {"cmd": "ls", "workdir": 3}}):
            with self.assertRaises(HookFailure, msg=json.dumps(payload)):
                decide(CODEX, payload)

    def test_functions_exec_contexts(self):
        def exec_call(program):
            return decide(CODEX, {"tool_name": "functions.exec", "tool_input": {"input": program}})

        self.assertEqual(exec_call('await tools.exec_command({cmd: "npx x"})').decision, "deny")
        self.assertEqual(exec_call('await tools.exec_command({cmd: "ls"})').decision, "allow")
        ambiguous = exec_call('const c = "x"; await tools.exec_command({cmd: c})')
        self.assertTrue(ambiguous.reason.startswith("BLOCKED: a nested execution context cannot be classified."))


if __name__ == "__main__":
    unittest.main()
