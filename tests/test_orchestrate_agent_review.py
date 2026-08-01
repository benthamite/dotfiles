import argparse
import importlib.util
import io
import json
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
CODEX_SCRIPT = (
    ROOT / "codex/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py"
)
CLAUDE_SCRIPT = (
    ROOT / "claude/skills/orchestrate-agent-review/scripts/orchestrate_agent_review.py"
)


def load_module():
    spec = importlib.util.spec_from_file_location(
        "orchestrate_agent_review", CODEX_SCRIPT
    )
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


orchestrator = load_module()


class WatchVerdictTests(unittest.TestCase):
    def test_paired_helpers_stay_identical(self):
        self.assertEqual(CODEX_SCRIPT.read_bytes(), CLAUDE_SCRIPT.read_bytes())

    def render_once(self, current):
        args = argparse.Namespace(json=False, interval=0)
        output = io.StringIO()
        with (
            mock.patch.object(orchestrator, "status", return_value=current),
            mock.patch.object(
                orchestrator.time, "sleep", side_effect=StopIteration
            ),
            redirect_stdout(output),
            self.assertRaises(StopIteration),
        ):
            orchestrator.watch(args)
        return output.getvalue()

    def test_reviewer_prompt_does_not_become_a_verdict(self):
        output = self.render_once(
            {
                "repo": {"head": "abc123 plan"},
                "reviewer_transcript": {
                    "mtime": 1.0,
                    "latest": [
                        {
                            "kind": "user",
                            "text": (
                                "Answer IMPLEMENTATION-READY or NOT READY "
                                "after reviewing the plan."
                            ),
                        }
                    ],
                },
            }
        )

        self.assertNotIn(" IMPLEMENTATION-READY", output)
        self.assertNotIn(" NOT READY", output)

    def test_reviewer_answer_at_the_start_is_reported(self):
        output = self.render_once(
            {
                "repo": {"head": "abc123 plan"},
                "reviewer_transcript": {
                    "mtime": 1.0,
                    "latest": [
                        {
                            "kind": "assistant",
                            "text": "NOT READY\n\n1. Fix the focused test command.",
                        }
                    ],
                },
            }
        )

        self.assertIn(" NOT READY", output)

    def test_implementation_ready_at_the_start_is_reported(self):
        output = self.render_once(
            {
                "repo": {"head": "abc123 plan"},
                "reviewer_transcript": {
                    "mtime": 1.0,
                    "latest": [
                        {
                            "kind": "assistant",
                            "text": "IMPLEMENTATION-READY\n\nAll blockers are resolved.",
                        }
                    ],
                },
            }
        )

        self.assertIn(" IMPLEMENTATION-READY", output)

    def test_first_line_not_ready_wins_over_ready_in_the_body(self):
        output = self.render_once(
            {
                "repo": {"head": "abc123 plan"},
                "reviewer_transcript": {
                    "mtime": 1.0,
                    "latest": [
                        {
                            "kind": "assistant",
                            "text": (
                                "NOT READY\n\nFix this before the plan can be "
                                "IMPLEMENTATION-READY."
                            ),
                        }
                    ],
                },
            }
        )

        self.assertIn(" NOT READY", output)
        self.assertNotIn(" IMPLEMENTATION-READY", output)

    def test_planner_discussion_does_not_become_a_verdict(self):
        output = self.render_once(
            {
                "repo": {"head": "abc123 plan"},
                "planner_transcript": {
                    "mtime": 1.0,
                    "latest": [
                        {
                            "kind": "message",
                            "text": "The reviewer should now return IMPLEMENTATION-READY.",
                        }
                    ],
                },
            }
        )

        self.assertNotIn(" IMPLEMENTATION-READY", output)

    def test_incidental_reviewer_discussion_does_not_become_a_verdict(self):
        output = self.render_once(
            {
                "repo": {"head": "abc123 plan"},
                "reviewer_transcript": {
                    "mtime": 1.0,
                    "latest": [
                        {
                            "kind": "assistant",
                            "text": (
                                "I checked whether this should be "
                                "IMPLEMENTATION-READY, but one blocker remains."
                            ),
                        }
                    ],
                },
            }
        )

        self.assertNotIn(" IMPLEMENTATION-READY", output)


class TranscriptMessageTests(unittest.TestCase):
    def test_claude_user_prompts_are_not_assistant_messages(self):
        records = [
            {
                "timestamp": "2026-08-01T16:17:01.630Z",
                "type": "user",
                "message": {
                    "role": "user",
                    "content": "Answer IMPLEMENTATION-READY or NOT READY.",
                },
            },
            {
                "timestamp": "2026-08-01T16:18:26.361Z",
                "type": "assistant",
                "message": {
                    "role": "assistant",
                    "content": [{"type": "text", "text": "NOT READY\n\nBlocker"}],
                },
            },
        ]
        with tempfile.TemporaryDirectory() as directory:
            transcript = Path(directory) / "claude.jsonl"
            transcript.write_text(
                "".join(json.dumps(record) + "\n" for record in records),
                encoding="utf-8",
            )

            messages = orchestrator.transcript_messages(transcript)

        self.assertEqual(
            messages,
            [
                {
                    "timestamp": "2026-08-01T16:18:26.361Z",
                    "kind": "assistant",
                    "text": "NOT READY\n\nBlocker",
                }
            ],
        )

if __name__ == "__main__":
    unittest.main()
