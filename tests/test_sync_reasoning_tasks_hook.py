"""Run the real SessionStart sync hook against scratch repositories.

Earlier coverage only exercised the bulk wrapper with a fake sync script, so
the hook's guards, its local-settings registration, and its merge path had no
test that executed them.  These tests point every HOME-derived path at a
temporary directory so nothing touches the live machine.
"""
from __future__ import annotations

import json
import os
import pathlib
import subprocess
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
HOOK = ROOT / "claude" / "hooks" / "sync-reasoning-tasks-worktree.sh"
MATCHING_ORIGIN = "trajectory-labs-pbc/reasoning-tasks"


def run(command, **kwargs):
    return subprocess.run(command, check=True, text=True, capture_output=True, **kwargs)


class SyncReasoningTasksHookTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = pathlib.Path(self.temp.name)
        self.home = self.root / "home"
        self.home.mkdir()
        # The freshness guard is registered only when this exact path exists.
        self.freshness = self.home / "My Drive/dotfiles/claude/hooks/cr-issue-comment-freshness.sh"
        self.freshness.parent.mkdir(parents=True)
        self.freshness.write_text("#!/bin/sh\nexit 0\n")
        self.freshness.chmod(0o755)

    def make_repo(self, origin_suffix: str) -> tuple[pathlib.Path, pathlib.Path]:
        origin = self.root / "remotes" / f"{origin_suffix}.git"
        origin.parent.mkdir(parents=True, exist_ok=True)
        run(["git", "init", "-q", "--bare", "-b", "main", str(origin)])
        seed = self.root / "seed"
        run(["git", "clone", "-q", str(origin), str(seed)])
        run(["git", "-C", str(seed), "config", "user.name", "Test User"])
        run(["git", "-C", str(seed), "config", "user.email", "test@example.com"])
        (seed / "README.md").write_text("reasoning-tasks\n")
        run(["git", "-C", str(seed), "add", "README.md"])
        run(["git", "-C", str(seed), "commit", "-q", "-m", "initial"])
        run(["git", "-C", str(seed), "push", "-q", "origin", "HEAD:main"])
        work = self.root / "work"
        run(["git", "clone", "-q", str(origin), str(work)])
        run(["git", "-C", str(work), "config", "user.name", "Test User"])
        run(["git", "-C", str(work), "config", "user.email", "test@example.com"])
        run(["git", "-C", str(work), "checkout", "-q", "-b", "pablo/task-a"])
        return origin, work

    def advance_origin(self, origin: pathlib.Path) -> None:
        seed = self.root / "seed"
        (seed / "NEWS.md").write_text("upstream change\n")
        run(["git", "-C", str(seed), "add", "NEWS.md"])
        run(["git", "-C", str(seed), "commit", "-q", "-m", "upstream"])
        run(["git", "-C", str(seed), "push", "-q", "origin", "HEAD:main"])

    def run_hook(self, cwd: pathlib.Path) -> subprocess.CompletedProcess:
        env = os.environ.copy()
        env.update(
            {
                "HOME": str(self.home),
                "CLAUDE_PROJECT_DIR": str(cwd),
                "SYNC_REASONING_TASKS_GC_SCRIPT": str(self.root / "no-gc"),
                "SYNC_REASONING_TASKS_OVERLAY_DIR": str(self.root / "no-overlay"),
                "SYNC_REASONING_TASKS_PRIVATE_SKILLS_DIR": str(self.root / "no-skills"),
            }
        )
        return subprocess.run(
            ["bash", str(HOOK)],
            input="{}",
            cwd=str(cwd),
            env=env,
            text=True,
            capture_output=True,
            check=False,
            timeout=60,
        )

    def test_non_matching_repo_is_a_silent_no_op(self):
        _origin, work = self.make_repo("someone-else/other-repo")
        before = sorted(p.relative_to(work).as_posix() for p in work.rglob("*") if ".git" not in p.parts)

        result = self.run_hook(work)

        self.assertEqual(0, result.returncode, result.stderr)
        self.assertEqual("", result.stdout)
        after = sorted(p.relative_to(work).as_posix() for p in work.rglob("*") if ".git" not in p.parts)
        self.assertEqual(before, after)
        self.assertFalse((work / ".claude").exists())

    def test_matching_repo_registers_guard_once_and_merges_upstream(self):
        origin, work = self.make_repo(MATCHING_ORIGIN)
        self.advance_origin(origin)

        first = self.run_hook(work)
        self.assertEqual(0, first.returncode, first.stderr)
        self.assertIn("merged origin/main into pablo/task-a", first.stdout)
        self.assertTrue((work / "NEWS.md").exists())
        self.assertFalse((work / ".git" / "MERGE_HEAD").exists())

        second = self.run_hook(work)
        self.assertEqual(0, second.returncode, second.stderr)
        self.assertNotIn("merged origin/main", second.stdout)

        settings = json.loads((work / ".claude" / "settings.local.json").read_text())
        registrations = [
            hook["command"]
            for entry in settings["hooks"]["PreToolUse"]
            for hook in entry["hooks"]
            if "cr-issue-comment-freshness.sh" in hook["command"]
        ]
        self.assertEqual(1, len(registrations), settings)
        self.assertFalse((work / ".claude" / "settings.local.json.tmp").exists())

    def test_hook_does_not_leave_a_half_merge_when_interrupted(self):
        """SIGTERM while the merge runs must leave no MERGE_HEAD behind."""
        origin, work = self.make_repo(MATCHING_ORIGIN)
        self.advance_origin(origin)
        # Point `git` at a wrapper that leaves the merge half-finished (MERGE_HEAD
        # plus a staged index, as a merge interrupted by a kill would) and then
        # stalls, so the hook's TERM lands while the merge is in flight.
        fake_bin = self.root / "bin"
        fake_bin.mkdir()
        real_git = subprocess.run(["which", "git"], text=True, capture_output=True, check=True).stdout.strip()
        (fake_bin / "git").write_text(
            "#!/bin/sh\n"
            'if [ "$1" = merge ] && [ "$2" = --no-edit ]; then\n'
            f'  "{real_git}" merge --no-commit --no-ff origin/main >/dev/null 2>&1\n'
            "  sleep 30\n"
            "  exit 1\n"
            "fi\n"
            f'exec "{real_git}" "$@"\n'
        )
        (fake_bin / "git").chmod(0o755)
        env = os.environ.copy()
        env.update(
            {
                "HOME": str(self.home),
                "CLAUDE_PROJECT_DIR": str(work),
                "PATH": f"{fake_bin}:{env['PATH']}",
                "SYNC_REASONING_TASKS_GC_SCRIPT": str(self.root / "no-gc"),
                "SYNC_REASONING_TASKS_OVERLAY_DIR": str(self.root / "no-overlay"),
                "SYNC_REASONING_TASKS_PRIVATE_SKILLS_DIR": str(self.root / "no-skills"),
            }
        )
        # Own process group so the stalled wrapper can be reaped afterwards; the
        # TERM itself goes to the hook's bash alone, the harder of the two ways a
        # host can kill a hook (a group kill would stop git for us).
        process = subprocess.Popen(
            ["bash", str(HOOK)], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL, cwd=str(work), env=env, text=True,
            start_new_session=True,
        )
        def reap():
            try:
                os.killpg(process.pid, 9)
            except ProcessLookupError:
                pass
        self.addCleanup(reap)
        process.stdin.write("{}")
        process.stdin.close()  # the hook drains stdin with cat before doing anything
        # Wait until the half-finished merge is visible, then kill the hook.
        head_before = subprocess.run(
            [real_git, "-C", str(work), "rev-parse", "HEAD"], text=True, capture_output=True, check=True
        ).stdout
        merge_head = work / ".git" / "MERGE_HEAD"
        for _ in range(400):
            if merge_head.exists():
                break
            subprocess.run(["sleep", "0.05"])
        self.assertTrue(merge_head.exists(), "fixture never reached the in-flight merge state")
        process.terminate()
        process.wait(timeout=20)
        stdout = process.stdout.read()
        process.stdout.close()

        self.assertIn("interrupted during merge", stdout)
        self.assertFalse(merge_head.exists())
        head_after = subprocess.run(
            [real_git, "-C", str(work), "rev-parse", "HEAD"], text=True, capture_output=True, check=True
        ).stdout
        self.assertEqual(head_before, head_after)
        status = subprocess.run(
            [real_git, "-C", str(work), "status", "--porcelain", "--untracked-files=no"],
            text=True, capture_output=True, check=True,
        ).stdout
        self.assertEqual("", status)


if __name__ == "__main__":
    unittest.main()
