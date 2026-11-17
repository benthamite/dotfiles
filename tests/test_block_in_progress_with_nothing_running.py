"""Tests for the Stop hook that refuses `Status: in progress' over nothing."""

from __future__ import annotations

import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
HOOK = ROOT / "claude" / "hooks" / "block-in-progress-with-nothing-running.py"

# The last line of a reply of 2026-11-17, sent with nothing running.
MESSAGE = ("The cutover stays on hold.\n\nStatus: in progress: confirming org-linear "
           "completes a clean sync with the fix, then committing it with a test")
SHELL = {"id": "b1", "type": "shell", "status": "running", "description": "watch"}
TEAMMATE = {"id": "t1", "type": "teammate", "status": "running", "description": "x"}


def user(text):
    return {"type": "user", "message": {"content": text}}


def tool_result():
    return {"type": "user", "message": {"content": [{"type": "tool_result", "content": "ok"}]}}


def call(name, **tool_input):
    return {"type": "assistant", "message": {"content": [
        {"type": "tool_use", "name": name, "input": tool_input}]}}


class BlockInProgressWithNothingRunningTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.env = dict(os.environ, AGENT_STOP_PROGRESS_STATE_DIR=self.temp.name)

    def tearDown(self) -> None:
        self.temp.cleanup()

    def transcript(self, *records) -> str:
        path = Path(self.temp.name) / "transcript.jsonl"
        path.write_text("".join(json.dumps(r) + "\n" for r in records))
        return str(path)

    def run_hook(self, message=MESSAGE, tasks=(), prompt="p1", **extra):
        payload = {"hook_event_name": "Stop", "session_id": "s1", "prompt_id": prompt,
                   "last_assistant_message": message, **extra}
        if tasks is not None:
            payload["background_tasks"] = list(tasks)
        result = subprocess.run([str(HOOK)], input=json.dumps(payload),
                                capture_output=True, text=True, env=self.env, check=True)
        return json.loads(result.stdout) if result.stdout.strip() else None, result.stderr

    def test_in_progress_over_nothing_is_blocked(self) -> None:
        output, _ = self.run_hook()
        self.assertEqual(output["decision"], "block")
        self.assertIn("nothing will resume you", output["reason"])

    def test_markdown_status_line_is_recognized(self) -> None:
        self.assertEqual(
            self.run_hook(message="x\n\n**Status: in progress: tests next**")[0]["decision"],
            "block")

    def test_a_running_task_allows_it(self) -> None:
        self.assertIsNone(self.run_hook(tasks=(SHELL,))[0])

    def test_a_finished_task_does_not(self) -> None:
        self.assertEqual(
            self.run_hook(tasks=(dict(SHELL, status="completed"),))[0]["decision"], "block")

    def test_a_teammate_alone_does_not(self) -> None:
        self.assertEqual(self.run_hook(tasks=(TEAMMATE,))[0]["decision"], "block")

    def test_a_wake_up_set_this_turn_allows_it(self) -> None:
        for name in ("ScheduleWakeup", "CronCreate"):
            with self.subTest(tool=name):
                path = self.transcript(user("go"), call(name, delaySeconds=600), tool_result())
                self.assertIsNone(self.run_hook(transcript_path=path)[0])

    def test_a_wake_up_from_an_earlier_turn_does_not(self) -> None:
        path = self.transcript(user("go"), call("ScheduleWakeup", delaySeconds=600),
                               tool_result(), user("and now?"), call("Bash", command="ls"))
        self.assertEqual(self.run_hook(transcript_path=path)[0]["decision"], "block")

    def test_ending_a_loop_is_not_a_wake_up(self) -> None:
        path = self.transcript(user("go"), call("ScheduleWakeup", stop=True))
        self.assertEqual(self.run_hook(transcript_path=path)[0]["decision"], "block")

    def test_a_missing_transcript_is_no_wake_up(self) -> None:
        self.assertEqual(
            self.run_hook(transcript_path=str(Path(self.temp.name) / "none"))[0]["decision"],
            "block")

    def test_other_statuses_are_allowed(self) -> None:
        for status in ("Status: done", "Status: waiting on you: approve 1-3",
                       "Status: done, pending verification: CI at 16:00"):
            with self.subTest(status=status):
                self.assertIsNone(self.run_hook(message=f"x\n\n{status}")[0])

    def test_no_status_line_is_allowed(self) -> None:
        self.assertIsNone(self.run_hook(message="Here is the answer.")[0])

    def test_last_status_line_decides(self) -> None:
        message = "Status: in progress: x next\n\nMore.\n\nStatus: waiting on you: y"
        self.assertIsNone(self.run_hook(message=message)[0])

    def test_missing_field_is_reported_not_read_as_empty(self) -> None:
        output, stderr = self.run_hook(tasks=None)
        self.assertIsNone(output)
        self.assertIn("no background_tasks", stderr)

    def test_unreadable_payload_is_allowed(self) -> None:
        result = subprocess.run([str(HOOK)], input="{", capture_output=True, text=True,
                                env=self.env, check=True)
        self.assertEqual(result.stdout, "")

    def test_exhaustion_allows_and_tells_the_user(self) -> None:
        self.assertEqual(self.run_hook()[0]["decision"], "block")
        self.assertEqual(self.run_hook()[0]["decision"], "block")
        output, _ = self.run_hook()
        self.assertNotIn("decision", output)
        self.assertIn("it has stopped", output["systemMessage"])
        self.assertEqual(self.run_hook()[0]["decision"], "block")

    def test_new_prompt_resets_count(self) -> None:
        self.run_hook(prompt="p1")
        self.run_hook(prompt="p1")
        self.assertEqual(self.run_hook(prompt="p2")[0]["decision"], "block")


if __name__ == "__main__":
    unittest.main()
