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
from agent_guard.policies import dirty_tree  # noqa: E402

GIT_ENV = {"GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t", "GIT_COMMITTER_NAME": "t",
           "GIT_COMMITTER_EMAIL": "t@t", "GIT_CONFIG_GLOBAL": os.devnull, "GIT_CONFIG_NOSYSTEM": "1"}


def git(*args, cwd):
    subprocess.run(["git", *args], cwd=cwd, check=True, capture_output=True, env=dict(os.environ, **GIT_ENV))


class DirtyTreeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tmp = tempfile.TemporaryDirectory()
        base = Path(cls.tmp.name).resolve()
        cls.dirty, cls.clean = str(base / "dirty"), str(base / "clean")
        for path in (cls.dirty, cls.clean):
            os.makedirs(path)
            git("init", "-q", cwd=path)
            Path(path, "f").write_text("a\n")
            git("add", "f", cwd=path)
            git("commit", "-q", "-m", "x", cwd=path)
        Path(cls.dirty, "f").write_text("b\n")
        Path(cls.clean, "untracked").write_text("u\n")

    @classmethod
    def tearDownClass(cls):
        cls.tmp.cleanup()

    def decide(self, runtime, command, cwd, **payload):
        key = "command" if runtime == CLAUDE else "cmd"
        payload = {"tool_name": "Bash", "tool_input": {key: command}, "cwd": cwd, **payload}
        return dirty_tree.decide(Call.from_payload(runtime, payload, env=dict(os.environ, **GIT_ENV)))

    def test_rewrite_over_dirty_tree_denied(self):
        for runtime in (CLAUDE, CODEX):
            result = self.decide(runtime, "git filter-repo --path x", self.dirty)
            self.assertEqual(result.decision, "deny")
            self.assertTrue(result.reason.startswith(f"BLOCKED: git history rewrite with a dirty working tree in {self.dirty}\n\n"))
            self.assertIn("The 1 uncommitted change(s) below", result.reason)
            self.assertIn("\n\n M f\n\n", result.reason)

    def test_target_from_dash_c_then_last_cd(self):
        self.assertEqual(self.decide(CLAUDE, f"git -C {self.dirty} filter-branch", self.clean).decision, "deny")
        self.assertEqual(self.decide(CLAUDE, f"cd {self.clean}; cd '{self.dirty}' && git filter-repo", self.clean).decision,
                         "deny")
        self.assertEqual(self.decide(CLAUDE, f"git -C {self.clean} filter-repo", self.dirty).decision, "allow")

    def test_allowed_shapes(self):
        for command in ("ALLOW_DIRTY_REWRITE=1 git filter-repo", "echo git filter-repo", "git log --grep filter-repo",
                        "git commit -F- <<EOF\ngit filter-repo\nEOF", "git status"):
            self.assertEqual(self.decide(CODEX, command, self.dirty).decision, "allow", command)
        # Untracked files survive a hard reset.
        self.assertEqual(self.decide(CODEX, "git filter-repo", self.clean).decision, "allow")

    def test_command_position_and_assignments(self):
        self.assertEqual(self.decide(CLAUDE, "X=1 Y=2 git-filter-repo --force", self.dirty).decision, "deny")
        self.assertEqual(self.decide(CLAUDE, "ls; (git filter-branch)", self.dirty).decision, "deny")


if __name__ == "__main__":
    unittest.main()
