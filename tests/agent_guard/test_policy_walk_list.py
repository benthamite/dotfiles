import sys
import unittest
from unittest import mock
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import Call, HookFailure  # noqa: E402
from agent_guard.policies import walk_list  # noqa: E402

HOME = "/Users/tester"
STORE = f"{HOME}/.claude/walk-list-data"


def call(runtime, tool, tool_input):
    return Call.from_payload(runtime, {"tool_name": tool, "tool_input": tool_input, "cwd": "/tmp"}, env={"HOME": HOME})


def decide(runtime, tool, tool_input):
    return walk_list.decide(call(runtime, tool, tool_input))


class PathTests(unittest.TestCase):
    def test_file_tools_denied_on_store(self):
        for tool in ("Read", "Edit", "Write", "NotebookEdit"):
            result = decide("claude", tool, {"file_path": "~/.claude/walk-list-data/x/list.txt"})
            self.assertTrue(result.reason.startswith(
                f"BLOCKED: {tool} of walk-list protected path: ~/.claude/walk-list-data/x/list.txt\n\n"
                "The walk-list protected store"), tool)
        self.assertEqual(decide("claude", "Read", {"file_path": "/tmp/notes.txt"}).decision, "allow")
        self.assertEqual(decide("claude", "Read", {"file_path": "/elsewhere/.claude/walk-list-data"}).decision, "deny")

    def test_jq_alternative_keeps_empty_string(self):
        # jq's `//` skips null and false but not "", so an empty file_path wins.
        self.assertEqual(decide("claude", "Write", {"file_path": "", "notebook_path": STORE}).decision, "allow")
        self.assertEqual(decide("claude", "Write", {"file_path": None, "notebook_path": STORE}).decision, "deny")

    def test_grep_glob_path_and_pattern(self):
        self.assertIn("Grep of walk-list protected path", decide("claude", "Grep", {"path": STORE}).reason)
        self.assertIn("Glob pattern references walk-list protected path: ~/.claude/walk-list-data/**",
                      decide("codex", "Glob", {"pattern": "~/.claude/walk-list-data/**"}).reason)

    def test_claude_non_object_input_fails_closed(self):
        with self.assertRaises(HookFailure):
            decide("claude", "Read", "x")


class FailureTests(unittest.TestCase):
    def test_helper_failures_propagate(self):
        for helper in ("mask_heredoc_bodies", "mask_git_commit_messages", "grep"):
            with self.subTest(helper=helper), mock.patch.object(walk_list, helper, side_effect=RuntimeError):
                with self.assertRaises(RuntimeError):
                    decide("claude", "Bash", {"command": "ls"})
        with mock.patch.object(walk_list, "codex_changed_paths", side_effect=RuntimeError):
            with self.assertRaises(RuntimeError):
                decide("codex", "apply_patch", {"patch": "*** Add File: x\n"})


class ShellTests(unittest.TestCase):
    def test_store_only_via_walk_py(self):
        self.assertIn("Bash command references ~/.claude/walk-list-data/ outside of walk.py: cat ~/.claude/walk-list-data/a",
                      decide("claude", "Bash", {"command": "cat ~/.claude/walk-list-data/a"}).reason)
        self.assertEqual(decide("codex", "exec_command", {"cmd": "python3 -u x/walk.py next ~/.claude/walk-list-data/a"})
                         .decision, "allow")
        self.assertEqual(decide("claude", "Bash", {"command": "git commit -m 'see ~/.claude/walk-list-data'"}).decision,
                         "allow")

    def test_home_path_dots_are_regex_wildcards(self):
        command = f"ls {HOME}/Xclaude/walk-list-data"
        self.assertEqual(decide("claude", "Bash", {"command": command}).decision, "deny")

    def test_shell_tools_by_runtime(self):
        self.assertEqual(decide("claude", "exec_command", {"cmd": f"cat {STORE}/a"}).decision, "allow")
        self.assertEqual(decide("codex", "exec_command", {"cmd": f"cat {STORE}/a"}).decision, "deny")

    def test_apply_patch_paths_and_text(self):
        patch = "*** Begin Patch\n*** Add File: /tmp/.claude/walk-list-data/x\n+a\n*** End Patch"
        self.assertIn("apply_patch of walk-list protected path: /tmp/.claude/walk-list-data/x",
                      decide("codex", "apply_patch", {"patch": patch}).reason)
        patch = "*** Begin Patch\n*** Add File: notes.md\n+see ~/.claude/walk-list-data\n*** End Patch"
        self.assertIn("apply_patch references ~/.claude/walk-list-data/", decide("codex", "apply_patch", patch).reason)
        self.assertEqual(decide("claude", "apply_patch", {"patch": patch}).decision, "allow")

    def test_dispatcher_check_matches_bash_branch(self):
        c = call("claude", "Bash", {"command": f"cat {STORE}/a"})
        self.assertEqual(walk_list.dispatcher_check(c), [walk_list.decide(c).reason])
        self.assertEqual(walk_list.dispatcher_check(call("claude", "Bash", {"command": "ls"})), [])


if __name__ == "__main__":
    unittest.main()
