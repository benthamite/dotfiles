"""Tests for the documentation commit gates run from the global Git pre-commit hook."""

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
HOOKS = ROOT / "claude" / "git-hooks"


class AgentCommitGatesTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        base = Path(self.temp.name).resolve()
        (base / "gitconfig").write_text(
            f"[core]\n\thooksPath = {HOOKS}\n[user]\n\tname = T\n\temail = t@example.com\n"
        )
        self.env = {k: v for k, v in os.environ.items()
                    if k not in ("CLAUDE_CODE_SESSION_ID", "CODEX_THREAD_ID") and not k.startswith("GIT_")}
        self.env.update(GIT_CONFIG_GLOBAL=str(base / "gitconfig"), GIT_CONFIG_NOSYSTEM="1",
                        AGENT_ELISP_EVIDENCE_DIR=str(base / "state"), CLAUDE_CODE_SESSION_ID="s",
                        AGENT_ELISP_EVIDENCE_CHECK_TEMP="1")
        self.repo = base / "repo"
        self.repo.mkdir()
        self.shell("git init -q && mkdir -p claude/hooks && echo a > claude/hooks/x.sh "
                   "&& echo a > claude/README.org && git add -A "
                   "&& CLAUDE_CODE_SESSION_ID= git commit -q -m init")

    def tearDown(self) -> None:
        self.temp.cleanup()

    def shell(self, command: str) -> subprocess.CompletedProcess:
        return subprocess.run(["bash", "-c", command], cwd=self.repo, env=self.env,
                              capture_output=True, text=True)

    def test_hook_change_without_readme_is_blocked(self) -> None:
        result = self.shell("echo b > claude/hooks/x.sh && git add -A && git commit -q -m change")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("claude/README.org", result.stderr)

    def test_chained_add_and_commit_with_readme_passes(self) -> None:
        result = self.shell("echo b > claude/hooks/x.sh && echo b > claude/README.org "
                            "&& git add claude && git commit -q -m 'change\n\nbody' && git log --oneline -1")
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_amend_is_judged_against_the_parent(self) -> None:
        first = self.shell("echo b > claude/hooks/x.sh && echo b > claude/README.org "
                           "&& git add -A && git commit -q -m change")
        self.assertEqual(first.returncode, 0, first.stderr)
        amend = self.shell("echo c > claude/hooks/x.sh && git commit -q -a --amend --no-edit")
        self.assertEqual(amend.returncode, 0, amend.stderr)

    def test_non_agent_commits_are_not_gated(self) -> None:
        result = self.shell("echo b > claude/hooks/x.sh && CLAUDE_CODE_SESSION_ID= git commit -q -am x")
        self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    unittest.main()
