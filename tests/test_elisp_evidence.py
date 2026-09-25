"""Tests for content-bound Elisp evidence enforced by Git hooks and a Stop hook.

Commits go through real `git commit` with the global hook directory, so the
checks see whatever the command spelling produced, never the spelling itself.
"""

from __future__ import annotations

import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
EVIDENCE = ROOT / "claude" / "bin" / "elisp-evidence"
HOOKS = ROOT / "claude" / "git-hooks"
SESSION = "11111111-2222-3333-4444-555555555555"


class ElispEvidenceTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.base = Path(self.temp.name).resolve()
        self.state = self.base / "state"
        self.env = {
            key: value
            for key, value in os.environ.items()
            if key not in ("CLAUDE_CODE_SESSION_ID", "CODEX_THREAD_ID")
            and not key.startswith("GIT_")
        }
        self.env.update({
            "AGENT_ELISP_EVIDENCE_DIR": str(self.state),
            "GIT_CONFIG_GLOBAL": str(self.base / "gitconfig"),
            "GIT_CONFIG_NOSYSTEM": "1",
            "CLAUDE_CODE_SESSION_ID": SESSION,
            "AGENT_ELISP_EVIDENCE_CHECK_TEMP": "1",
        })
        (self.base / "gitconfig").write_text(
            f"[core]\n\thooksPath = {HOOKS}\n[user]\n\tname = T\n\temail = t@example.com\n"
            "[init]\n\tdefaultBranch = main\n"
        )

    def tearDown(self) -> None:
        self.temp.cleanup()

    # --- helpers ------------------------------------------------------------

    def make_repo(self, relative: str, files: dict[str, str]) -> Path:
        repo = self.base / relative
        repo.mkdir(parents=True)
        self.git(repo, "init", "-q")
        for path, text in files.items():
            self.write(repo, path, text)
        self.git(repo, "add", "-A")
        self.git(repo, "commit", "-q", "-m", "init", env={"CLAUDE_CODE_SESSION_ID": ""})
        return repo

    def write(self, repo: Path, path: str, text: str) -> None:
        target = repo / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text)

    def run_cmd(self, argv, cwd: Path, env: dict | None = None, input: str | None = None):
        return subprocess.run(argv, cwd=cwd, env={**self.env, **(env or {})},
                              capture_output=True, text=True, input=input)

    def git(self, repo: Path, *args: str, env: dict | None = None, check: bool = True):
        result = self.run_cmd(["git", *args], repo, env)
        if check:
            self.assertEqual(result.returncode, 0, result.stderr)
        return result

    def shell(self, repo: Path, command: str):
        return self.run_cmd(["bash", "-c", command], repo)

    def evidence(self, repo: Path, *args: str, input: str | None = None):
        return self.run_cmd([str(EVIDENCE), *args], repo, input=input)

    def record_test(self, repo: Path, label: str) -> None:
        digest = self.evidence(repo, "snapshot", str(repo)).stdout.strip()
        result = self.evidence(repo, "record-test", str(repo), label, digest)
        self.assertEqual(result.returncode, 0, result.stderr)

    def stop(self, repo: Path, active: bool = False) -> dict | None:
        payload = json.dumps({"session_id": SESSION, "stop_hook_active": active})
        result = self.evidence(repo, "stop-check", "--agent", "claude", input=payload)
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads(result.stdout) if result.stdout.strip() else None

    def head(self, repo: Path) -> str:
        return self.git(repo, "rev-parse", "HEAD").stdout.strip()

    # --- pre-commit ---------------------------------------------------------

    def test_untested_elisp_commit_is_blocked(self) -> None:
        repo = self.make_repo("pkg", {"pkg.el": "(provide 'pkg)\n"})
        self.write(repo, "pkg.el", "(provide 'pkg) ;; changed\n")
        self.git(repo, "add", "pkg.el")
        result = self.git(repo, "commit", "-q", "-m", "change", check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("batch-test.sh", result.stderr)
        self.assertIn("pkg", result.stderr)

    def test_tested_commit_passes_whatever_the_command_shape(self) -> None:
        repo = self.make_repo("pkg", {"pkg.el": "(provide 'pkg)\n"})
        shapes = [
            "git add pkg.el && git commit -q -m one",
            f"cd {repo} && git commit -qam 'two\n\nbody line' && git log --oneline -1",
            f"git -C {repo} commit -q -a -m three -m 'more' 2>&1 | tail -1",
        ]
        for number, command in enumerate(shapes):
            with self.subTest(command=command):
                self.write(repo, "pkg.el", f"(provide 'pkg) ;; {number}\n")
                self.record_test(repo, "pkg")
                result = self.shell(repo, command)
                self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
                self.assertIn(f";; {number}", self.git(repo, "show", "HEAD:pkg.el").stdout)

    def test_evidence_for_other_contents_does_not_count(self) -> None:
        repo = self.make_repo("pkg", {"pkg.el": "(provide 'pkg)\n"})
        self.write(repo, "pkg.el", "(provide 'pkg) ;; tested\n")
        self.record_test(repo, "pkg")
        self.write(repo, "pkg.el", "(provide 'pkg) ;; edited after the test\n")
        self.git(repo, "add", "pkg.el")
        result = self.git(repo, "commit", "-q", "-m", "x", check=False)
        self.assertNotEqual(result.returncode, 0)

    def test_non_agent_commits_are_not_checked(self) -> None:
        repo = self.make_repo("pkg", {"pkg.el": "(provide 'pkg)\n"})
        self.write(repo, "pkg.el", "(provide 'pkg) ;; by hand\n")
        self.git(repo, "add", "pkg.el")
        self.git(repo, "commit", "-q", "-m", "x", env={"CLAUDE_CODE_SESSION_ID": ""})

    def test_non_elisp_commit_needs_no_evidence(self) -> None:
        repo = self.make_repo("pkg", {"pkg.el": "(provide 'pkg)\n", "README": "a\n"})
        self.write(repo, "README", "b\n")
        self.git(repo, "commit", "-q", "-am", "docs")
        self.assertIsNone(self.stop(repo))

    def test_generated_elisp_needs_no_evidence(self) -> None:
        repo = self.make_repo("pkg", {"pkg.el": "(provide 'pkg)\n", "lockfile.el": "()\n"})
        self.write(repo, "lockfile.el", "(x)\n")
        self.git(repo, "commit", "-q", "-am", "lock")

    def test_dotfiles_extra_ignores_unrelated_dirty_extras(self) -> None:
        repo = self.make_repo("dotfiles", {
            "emacs/extras/alpha.el": "(provide 'alpha)\n",
            "emacs/extras/beta.el": "(provide 'beta)\n",
            "claude/bin/.keep": "",
        })
        self.write(repo, "emacs/extras/alpha.el", "(provide 'alpha) ;; new\n")
        self.write(repo, "emacs/extras/beta.el", "(provide 'beta) ;; another session\n")
        self.record_test(repo, "alpha")
        self.git(repo, "add", "emacs/extras/alpha.el")
        self.git(repo, "commit", "-q", "-m", "alpha")

    def test_deleted_file_needs_evidence_of_absence(self) -> None:
        repo = self.make_repo("proj", {"scripts/a.el": "(a)\n", "scripts/b.el": "(b)\n"})
        self.git(repo, "rm", "-q", "scripts/a.el")
        result = self.git(repo, "commit", "-q", "-m", "rm", check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("file:scripts/a.el", result.stderr)
        self.record_test(repo, "file:scripts/a.el")
        self.git(repo, "commit", "-q", "-m", "rm")

    def test_main_checkout_evidence_covers_same_content_in_a_worktree(self) -> None:
        main = self.make_repo("pkg", {"pkg.el": "(provide 'pkg)\n"})
        self.write(main, "pkg.el", "(provide 'pkg) ;; feature\n")
        self.record_test(main, "pkg")
        worktree = self.base / "worktrees" / "feature"
        self.git(main, "worktree", "add", "-q", "-b", "feature", str(worktree))
        self.write(worktree, "pkg.el", "(provide 'pkg) ;; feature\n")
        self.git(worktree, "commit", "-q", "-am", "feature")
        self.write(worktree, "pkg.el", "(provide 'pkg) ;; untested\n")
        result = self.git(worktree, "commit", "-q", "-am", "untested", check=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertRegex(result.stderr, r"batch-test\.sh'? pkg")

    def test_temporary_repositories_are_not_checked(self) -> None:
        repo = self.make_repo("pkg", {"pkg.el": "(provide 'pkg)\n"})
        self.write(repo, "pkg.el", "(provide 'pkg) ;; fixture\n")
        self.git(repo, "commit", "-q", "-am", "fixture", env={"AGENT_ELISP_EVIDENCE_CHECK_TEMP": ""})
        self.assertIsNone(self.stop(repo))

    # --- post-commit and Stop -----------------------------------------------

    def test_no_verify_commit_leaves_a_test_debt_until_tested(self) -> None:
        repo = self.make_repo("pkg", {"pkg.el": "(provide 'pkg)\n"})
        self.write(repo, "pkg.el", "(provide 'pkg) ;; sneaky\n")
        self.git(repo, "commit", "-q", "--no-verify", "-am", "x")
        decision = self.stop(repo)
        self.assertEqual(decision["decision"], "block")
        self.assertIn("batch-test.sh", decision["reason"])
        self.record_test(repo, "pkg")
        self.assertIsNone(self.stop(repo))

    def test_live_debt_blocks_stop_until_verified(self) -> None:
        repo = self.make_repo("elpaca/sources/pkg", {"pkg.el": "(provide 'pkg)\n"})
        self.write(repo, "pkg.el", "(provide 'pkg) ;; v2\n")
        self.record_test(repo, "pkg")
        self.git(repo, "commit", "-q", "-am", "v2")
        decision = self.stop(repo)
        self.assertEqual(decision["decision"], "block")
        self.assertIn("elisp-live-verify", decision["reason"])
        self.evidence(repo, "record-live", str(repo), "pkg", "pkg", self.head(repo))
        self.assertIsNone(self.stop(repo))

    def test_verifying_a_later_commit_covers_earlier_ones(self) -> None:
        repo = self.make_repo("elpaca/sources/pkg", {"pkg.el": "(provide 'pkg)\n"})
        for number in range(2):
            self.write(repo, "pkg.el", f"(provide 'pkg) ;; {number}\n")
            self.record_test(repo, "pkg")
            self.git(repo, "commit", "-q", "-am", str(number))
        self.evidence(repo, "record-live", str(repo), "pkg", "pkg", self.head(repo))
        self.assertIsNone(self.stop(repo))

    def test_amended_commit_supersedes_its_debt(self) -> None:
        repo = self.make_repo("elpaca/sources/pkg", {"pkg.el": "(provide 'pkg)\n"})
        self.write(repo, "pkg.el", "(provide 'pkg) ;; a\n")
        self.record_test(repo, "pkg")
        self.git(repo, "commit", "-q", "-am", "a")
        self.write(repo, "pkg.el", "(provide 'pkg) ;; b\n")
        self.record_test(repo, "pkg")
        self.git(repo, "commit", "-q", "--amend", "-am", "b")
        self.evidence(repo, "record-live", str(repo), "pkg", "pkg", self.head(repo))
        self.assertIsNone(self.stop(repo))

    def test_stop_releases_after_repeated_blocks(self) -> None:
        repo = self.make_repo("elpaca/sources/pkg", {"pkg.el": "(provide 'pkg)\n"})
        self.write(repo, "pkg.el", "(provide 'pkg) ;; v2\n")
        self.record_test(repo, "pkg")
        self.git(repo, "commit", "-q", "-am", "v2")
        self.assertEqual(self.stop(repo)["decision"], "block")
        self.assertEqual(self.stop(repo, active=True)["decision"], "block")
        released = self.stop(repo, active=True)
        self.assertNotIn("decision", released)
        self.assertIn("Unverified Elisp", released["systemMessage"])
        self.assertEqual(self.stop(repo)["decision"], "block")

    def test_other_sessions_debts_do_not_block(self) -> None:
        repo = self.make_repo("elpaca/sources/pkg", {"pkg.el": "(provide 'pkg)\n"})
        self.write(repo, "pkg.el", "(provide 'pkg) ;; v2\n")
        self.record_test(repo, "pkg")
        self.git(repo, "commit", "-q", "-am", "v2", env={"CLAUDE_CODE_SESSION_ID": "other"})
        self.assertIsNone(self.stop(repo))

    def test_test_files_create_no_live_debt(self) -> None:
        repo = self.make_repo("elpaca/sources/pkg", {"pkg.el": "(provide 'pkg)\n", "test/pkg-test.el": "(t)\n"})
        self.write(repo, "test/pkg-test.el", "(t2)\n")
        self.record_test(repo, "pkg")
        self.git(repo, "commit", "-q", "-am", "tests")
        self.assertIsNone(self.stop(repo))

    # --- hook chaining ------------------------------------------------------

    def test_repository_hooks_still_run(self) -> None:
        repo = self.make_repo("pkg", {"README": "a\n"})
        marker = self.base / "local-hook-ran"
        local = repo / ".git" / "hooks" / "post-commit"
        local.write_text(f"#!/bin/sh\ntouch {marker}\n")
        local.chmod(0o755)
        self.write(repo, "README", "b\n")
        self.git(repo, "commit", "-q", "-am", "x", env={"CLAUDE_CODE_SESSION_ID": ""})
        self.assertTrue(marker.exists())

    def test_repository_pre_commit_can_still_reject(self) -> None:
        repo = self.make_repo("pkg", {"README": "a\n"})
        local = repo / ".git" / "hooks" / "pre-commit"
        local.write_text("#!/bin/sh\nexit 1\n")
        local.chmod(0o755)
        self.write(repo, "README", "b\n")
        result = self.git(repo, "commit", "-q", "-am", "x", check=False)
        self.assertNotEqual(result.returncode, 0)


if __name__ == "__main__":
    unittest.main()
