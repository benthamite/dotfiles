"""Tests for the Stop hook that refuses `Status: done' over running tasks."""

from __future__ import annotations

import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
HOOK = ROOT / "claude" / "hooks" / "block-done-over-running-tasks.py"

# The final message and task of the 2026-10-10 14:32 dotfiles Stop, whose
# background shell had hung on stdin for hours.
DOTFILES_MESSAGE = ("I didn't commit the `android` workspace; that's left to your "
                    "usual sync.\n\nStatus: done")
HUNG_SHELL = {"id": "bam9tx09h", "type": "shell", "status": "running",
              "description": "Verify detached sample survives job process-group kill"}
TEAMMATE = {"id": "t1", "type": "teammate", "status": "running",
            "description": "Phase 2A"}


class BlockDoneOverRunningTasksTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.env = dict(os.environ, AGENT_STOP_TASKS_STATE_DIR=self.temp.name)

    def tearDown(self) -> None:
        self.temp.cleanup()

    def run_hook(self, message=DOTFILES_MESSAGE, tasks=(HUNG_SHELL,),
                 prompt="p1", **extra):
        payload = {"hook_event_name": "Stop", "session_id": "s1",
                   "prompt_id": prompt, "last_assistant_message": message,
                   **extra}
        if tasks is not None:
            payload["background_tasks"] = list(tasks)
        result = subprocess.run([str(HOOK)], input=json.dumps(payload),
                                capture_output=True, text=True, env=self.env,
                                check=True)
        return json.loads(result.stdout) if result.stdout.strip() else None, result.stderr

    def test_replayed_dotfiles_stop_is_blocked(self) -> None:
        output, _ = self.run_hook()
        self.assertEqual(output["decision"], "block")
        self.assertIn("bam9tx09h", output["reason"])
        self.assertIn("TaskStop", output["reason"])

    def test_markdown_status_line_is_recognized(self) -> None:
        output, _ = self.run_hook(message="Done.\n\n**Status: done**")
        self.assertEqual(output["decision"], "block")

    def test_open_statuses_are_allowed(self) -> None:
        for status in ("Status: done, pending verification: CI at 16:00",
                       "Status: in progress: tests next",
                       "Status: waiting on you: approve 1-3"):
            with self.subTest(status=status):
                self.assertEqual(self.run_hook(message=f"x\n\n{status}")[0], None)

    def test_no_status_line_is_allowed(self) -> None:
        self.assertIsNone(self.run_hook(message="Here is the answer.")[0])

    def test_last_status_line_decides(self) -> None:
        message = "Status: done\n\nOne more thing.\n\nStatus: in progress: CI next"
        self.assertIsNone(self.run_hook(message=message)[0])

    def test_no_running_tasks_is_allowed(self) -> None:
        finished = dict(HUNG_SHELL, status="completed")
        self.assertIsNone(self.run_hook(tasks=(finished,))[0])

    def test_teammates_alone_are_allowed(self) -> None:
        self.assertIsNone(self.run_hook(tasks=(TEAMMATE,))[0])

    def test_missing_field_is_reported_not_read_as_empty(self) -> None:
        output, stderr = self.run_hook(tasks=None)
        self.assertIsNone(output)
        self.assertIn("no background_tasks", stderr)

    def test_unreadable_payload_is_allowed(self) -> None:
        result = subprocess.run([str(HOOK)], input="{", capture_output=True,
                                text=True, env=self.env, check=True)
        self.assertEqual(result.stdout, "")

    def test_exhaustion_allows_with_visible_message(self) -> None:
        self.assertEqual(self.run_hook()[0]["decision"], "block")
        # A continuation from another Stop hook must not reset this count.
        self.assertEqual(self.run_hook(stop_hook_active=False)[0]["decision"], "block")
        output, _ = self.run_hook(stop_hook_active=True)
        self.assertNotIn("decision", output)
        self.assertIn("bam9tx09h", output["systemMessage"])
        # The count starts over afterwards.
        self.assertEqual(self.run_hook()[0]["decision"], "block")

    def test_new_prompt_resets_count(self) -> None:
        self.run_hook(prompt="p1")
        self.run_hook(prompt="p1")
        self.assertEqual(self.run_hook(prompt="p2")[0]["decision"], "block")

    def test_clean_stop_resets_count(self) -> None:
        self.run_hook()
        self.run_hook(tasks=())
        self.run_hook()
        self.assertEqual(self.run_hook()[0]["decision"], "block")


if __name__ == "__main__":
    unittest.main()
