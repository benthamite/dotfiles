import os
import pathlib
import plistlib
import subprocess
import tempfile
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "sync-agent-c-worktrees"
PLIST = ROOT / "macos" / "LaunchAgents" / "com.stafforini.agent-c-worktree-sync.plist"


def run(command, **kwargs):
    return subprocess.run(command, check=True, text=True, **kwargs)


class AgentCWorktreeSyncTest(unittest.TestCase):
    def test_bulk_wrapper_runs_sync_for_task_worktrees_only(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            temp = pathlib.Path(temp_dir)
            root = temp / "agent-c"
            main = root / "main"
            task_a = root / "task-a"
            task_b = root / "task-b"
            cr_studio = root / "agent-c-cr-studio"
            root.mkdir()

            run(["git", "init", "-q", str(main)])
            run(["git", "-C", str(main), "config", "user.name", "Test User"])
            run(["git", "-C", str(main), "config", "user.email", "test@example.com"])
            (main / "README.md").write_text("agent-c\n")
            run(["git", "-C", str(main), "add", "README.md"])
            run(["git", "-C", str(main), "commit", "-q", "-m", "initial"])
            run(["git", "-C", str(main), "worktree", "add", "-q", "-b", "pablo/task-a", str(task_a)])
            run(["git", "-C", str(main), "worktree", "add", "-q", "-b", "pablo/task-b", str(task_b)])
            run(["git", "-C", str(main), "worktree", "add", "-q", "--detach", str(cr_studio), "HEAD"])

            calls_file = temp / "calls.txt"
            fake_sync = temp / "fake-sync.sh"
            fake_sync.write_text(
                "#!/bin/sh\n"
                "printf '%s\\n' \"$CLAUDE_PROJECT_DIR\" >> \"$CALLS_FILE\"\n"
            )
            fake_sync.chmod(0o755)

            env = os.environ.copy()
            env.update(
                {
                    "AGENT_C_WORKTREES_ROOT": str(root),
                    "AGENT_C_MAIN_WORKTREE": str(main),
                    "AGENT_C_SYNC_SCRIPT": str(fake_sync),
                    "AGENT_C_SYNC_LOG_DIR": str(temp / "logs"),
                    "AGENT_C_BULK_SKIP_FETCH": "1",
                    "CALLS_FILE": str(calls_file),
                }
            )

            run([str(SCRIPT)], env=env)

            self.assertEqual(
                {os.path.realpath(path) for path in calls_file.read_text().splitlines()},
                {os.path.realpath(task_a), os.path.realpath(task_b)},
            )

    def test_launchd_job_runs_bulk_wrapper_nightly(self):
        with PLIST.open("rb") as handle:
            job = plistlib.load(handle)

        self.assertEqual(job["Label"], "com.stafforini.agent-c-worktree-sync")
        self.assertEqual(job["ProgramArguments"], [str(SCRIPT)])
        self.assertEqual(job["StartCalendarInterval"], {"Hour": 3, "Minute": 30})
        self.assertIn("agent-c-worktree-sync", job["StandardOutPath"])
        self.assertIn("agent-c-worktree-sync", job["StandardErrorPath"])
