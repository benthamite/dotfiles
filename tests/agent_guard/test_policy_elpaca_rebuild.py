import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import CLAUDE, CODEX, Call  # noqa: E402
from agent_guard.policies import elpaca_rebuild  # noqa: E402

GIT_ENV = {"GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t", "GIT_COMMITTER_NAME": "t",
           "GIT_COMMITTER_EMAIL": "t@t", "GIT_CONFIG_GLOBAL": os.devnull, "GIT_CONFIG_NOSYSTEM": "1"}
REBUILD = "emacsclient -e '(elpaca-rebuild)'"


class ElpacaRebuildTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.env = {**os.environ, **GIT_ENV, "HOME": self.tmp.name}
        self.repo = Path(self.tmp.name, "My Drive/dotfiles")
        (self.repo / "emacs/extras").mkdir(parents=True)
        (self.repo / "emacs/extras/a.el").write_text("a\n")
        (self.repo / "emacs/b.el").write_text("b\n")
        for args in (["init", "-q"], ["add", "-A"], ["commit", "-qm", "init"]):
            subprocess.run(["git", *args], cwd=self.repo, env=self.env, check=True)

    def tearDown(self):
        self.tmp.cleanup()

    def decide(self, command, runtime=CLAUDE):
        call = Call.from_payload(runtime, {"tool_name": "Bash", "tool_input": {"command": command}}, env=self.env)
        return elpaca_rebuild.decide(call)

    def test_clean_tree_allows(self):
        self.assertEqual(self.decide(REBUILD).decision, "allow")

    def test_dirty_extras_deny_with_line_breaks(self):
        (self.repo / "emacs/extras/a.el").write_text("changed\n")
        for runtime in (CLAUDE, CODEX):
            result = self.decide(REBUILD, runtime)
            self.assertEqual(result.decision, "deny")
            self.assertIn("\n\nUncommitted files:\nemacs/extras/a.el\n\nCommit first, then rebuild.",
                          result.reason)

    def test_changes_outside_extras_and_non_rebuild_commands(self):
        (self.repo / "emacs/b.el").write_text("changed\n")
        self.assertEqual(self.decide(REBUILD).decision, "allow")
        (self.repo / "emacs/extras/a.el").write_text("changed\n")
        for command in ("emacsclient -e t", "echo elpaca-rebuild", "xemacsclient elpaca-rebuild",
                        "emacsclient\U00011f02 elpaca-rebuild"):
            self.assertEqual(self.decide(command).decision, "allow", command)
        self.assertEqual(self.decide("²emacsclient elpaca-extras-reload").decision, "deny")


class GitFailureTests(unittest.TestCase):
    """`DIRTY=$(git ... 2>/dev/null || true)`: a failing git leaves DIRTY empty."""

    def test_git_error_allows_like_the_hook(self):
        with tempfile.TemporaryDirectory() as home:  # no repository at HOME/My Drive/dotfiles
            call = Call.from_payload(CLAUDE, {"tool_name": "Bash", "tool_input": {"command": REBUILD}},
                                     env={**os.environ, "HOME": home})
            self.assertEqual(elpaca_rebuild.decide(call).decision, "allow")

    def test_missing_git_raises(self):
        # Stricter than the hook (whose `|| true` allowed): the engine denies on the exception.
        call = Call.from_payload(CLAUDE, {"tool_name": "Bash", "tool_input": {"command": REBUILD}},
                                 env={**os.environ, "PATH": "/nonexistent"})
        with self.assertRaises(OSError):
            elpaca_rebuild.decide(call)


if __name__ == "__main__":
    unittest.main()
