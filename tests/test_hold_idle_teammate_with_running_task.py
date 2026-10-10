"""Tests for the TeammateIdle hook that holds a teammate while its task runs."""

from __future__ import annotations

import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
HOOK = ROOT / "claude" / "hooks" / "hold-idle-teammate-with-running-task.py"
AGENT = "aphase2c-9a5cda2a2b7de383"


def launch_result(task: str, path: Path, moved: bool = False) -> dict:
    text = (f"Command did not complete within its 200s timeout and was moved to the background "
            f"(ID: {task}). Output is being written to: {path}. You will be notified when it completes."
            if moved else
            f"Command running in background with ID: {task}. Output is being written to: {path}. "
            f"You will be notified when it completes.")
    return {"type": "user", "message": {"role": "user", "content": [
        {"type": "tool_result", "tool_use_id": "t", "content": text}]}}


def notice(task: str) -> dict:
    return {"type": "user", "message": {"role": "user", "content":
            f"<task-notification>\n<task-id>{task}</task-id>\n<status>completed</status>\n</task-notification>"}}


def task_stop(task: str) -> dict:
    return {"type": "assistant", "message": {"role": "assistant", "content": [
        {"type": "tool_use", "name": "TaskStop", "input": {"task_id": task}}]}}


class HoldIdleTeammateTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.base = Path(self.temp.name)
        self.main = self.base / "session.jsonl"
        self.main.write_text("")
        (self.base / "session" / "subagents").mkdir(parents=True)
        self.transcript = self.base / "session" / "subagents" / f"agent-{AGENT}.jsonl"
        self.env = dict(os.environ, AGENT_TEAMMATE_IDLE_STATE_DIR=str(self.base / "state"))
        self.writers: list[subprocess.Popen] = []

    def tearDown(self) -> None:
        for proc in self.writers:
            proc.kill()
            proc.wait()
        self.temp.cleanup()

    def output(self, name: str, running: bool = False, text: str = "") -> Path:
        path = self.base / f"{name}.output"
        path.write_text(text)
        if running:
            handle = path.open("a")
            self.writers.append(subprocess.Popen(["sleep", "30"], stdout=handle))
            handle.close()
        return path

    def write(self, *records: dict) -> None:
        self.transcript.write_text("".join(json.dumps(r) + "\n" for r in records))

    def run_hook(self, payload: dict | None = None, raw: str | None = None):
        if payload is None:
            payload = {"hook_event_name": "TeammateIdle", "session_id": "s1",
                       "transcript_path": str(self.main), "agent_id": AGENT,
                       "teammate_name": "phase2c"}
        result = subprocess.run([str(HOOK)], input=raw if raw is not None else json.dumps(payload),
                                capture_output=True, text=True, env=self.env)
        return result.returncode, result.stderr

    def test_running_task_blocks(self) -> None:
        self.write(launch_result("b1", self.output("b1", running=True)))
        code, err = self.run_hook()
        self.assertEqual(code, 2)
        self.assertIn("Do not go idle", err)
        self.assertIn("b1", err)
        self.assertIn("timeout 600000", err)

    def test_running_task_printing_fake_marker_still_blocks(self) -> None:
        self.write(launch_result("b1", self.output("b1", running=True, text="[exited with code 0]\n")))
        self.assertEqual(self.run_hook()[0], 2)

    def test_timeout_moved_launch_is_recognized(self) -> None:
        self.write(launch_result("b1", self.output("b1", running=True), moved=True))
        self.assertEqual(self.run_hook()[0], 2)

    def test_running_task_keeps_blocking(self) -> None:
        self.write(launch_result("b1", self.output("b1", running=True)))
        self.assertEqual(self.run_hook()[0], 2)
        self.assertEqual(self.run_hook()[0], 2)

    def test_ended_undelivered_task_is_reported_once(self) -> None:
        self.write(launch_result("b1", self.output("b1", text="ok\n[exited with code 0]\n")))
        code, err = self.run_hook()
        self.assertEqual(code, 2)
        self.assertIn("have ended", err)
        self.assertIn("b1.output", err)
        # The notice never arrives; the teammate may still go idle next time.
        self.assertEqual(self.run_hook()[0], 0)

    def test_delivered_notice_allows(self) -> None:
        self.write(launch_result("b1", self.output("b1")), notice("b1"))
        self.assertEqual(self.run_hook(), (0, ""))

    def test_task_stopped_by_teammate_allows(self) -> None:
        self.write(launch_result("b1", self.output("b1")), task_stop("b1"))
        self.assertEqual(self.run_hook(), (0, ""))

    def test_missing_output_file_is_reported_once(self) -> None:
        self.write(launch_result("b1", self.base / "gone.output"))
        code, err = self.run_hook()
        self.assertEqual(code, 2)
        self.assertIn("file missing", err)
        self.assertEqual(self.run_hook()[0], 0)

    def test_other_agents_tasks_are_ignored(self) -> None:
        self.write()
        other = self.base / "session" / "subagents" / "agent-other.jsonl"
        other.write_text(json.dumps(launch_result("b9", self.output("b9", running=True))) + "\n")
        self.assertEqual(self.run_hook(), (0, ""))

    def test_missing_agent_id_allows_and_reports(self) -> None:
        code, err = self.run_hook({"session_id": "s1", "transcript_path": str(self.main)})
        self.assertEqual(code, 0)
        self.assertIn("agent_id", err)

    def test_missing_transcript_allows_and_reports(self) -> None:
        code, err = self.run_hook()
        self.assertEqual(code, 0)
        self.assertIn("no transcript", err)

    def test_malformed_payload_allows(self) -> None:
        self.assertEqual(self.run_hook(raw="{")[0], 0)


if __name__ == "__main__":
    unittest.main()
