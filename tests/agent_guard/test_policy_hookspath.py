import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import CLAUDE, CODEX, Call  # noqa: E402
from agent_guard.policies import hookspath  # noqa: E402


def codex(command):
    return hookspath.decide(Call.from_payload(CODEX, {"tool_name": "exec_command", "tool_input": {"cmd": command}}))


def claude(command):
    return hookspath.dispatcher_check(Call.from_payload(CLAUDE, {"tool_name": "Bash", "tool_input": {"command": command}}))


DENIED = ["git config core.hooksPath /x", "git -c core.hooksPath=/x commit", "git -ccore.hookspath=x status",
          "git config --unset core.hooksPath", "git config -e core.hookspath",
          "git config --get core.hooksPath; git config core.hooksPath /y", "(git config core.hookspath x)",
          "git config --get core.hookspath\ngit config core.hookspath x"]
ALLOWED = ["git config --get core.hooksPath", "git config core.hooksPath", "git config core.hookspath   ",
           "git config --get-all core.hooksPath", "echo core.hooksPath",
           # Not --get forms, but each segment ends at the bare key.
           "git config --getx core.hookspath", "git config core.hookspath | cat", "mygit config core.hookspath x",
           "x-git config core.hookspath x", "égit config core.hookspath x", "git config --list | grep core.hookspath",
           ""]


class HooksPathTests(unittest.TestCase):
    def test_overrides_denied_in_both_copies(self):
        for command in DENIED:
            self.assertEqual(codex(command).reason, hookspath.REASON, command)
            self.assertEqual(claude(command), [hookspath.REASON], command)

    def test_reads_and_mentions_allowed(self):
        for command in ALLOWED:
            self.assertEqual(codex(command).decision, "allow", command)
            self.assertEqual(claude(command), [], command)

    def test_punctuation_before_git_counts_as_a_boundary(self):
        self.assertEqual(codex("·git config core.hookspath x").decision, "deny")


if __name__ == "__main__":
    unittest.main()
