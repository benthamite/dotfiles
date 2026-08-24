import importlib.util
import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
LIB_FILE = ROOT / "lib/python/agent_session_lib.py"


def load_module():
    spec = importlib.util.spec_from_file_location("agent_session_lib", LIB_FILE)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


session = load_module()


class ElispStringTests(unittest.TestCase):
    def test_escapes_quotes_backslashes_and_newlines(self):
        value = 'a "quoted" \\ path\nsecond line'
        encoded = session.elisp_string(value)
        self.assertEqual(json.loads(encoded), value)
        self.assertTrue(encoded.startswith('"') and encoded.endswith('"'))


class TranscriptAdvanceTests(unittest.TestCase):
    def test_missing_transcript_has_not_advanced(self):
        self.assertFalse(session._transcript_advanced("/nonexistent/t.jsonl", 0))

    def test_growth_past_offset_counts_as_advanced(self):
        with tempfile.TemporaryDirectory() as directory:
            transcript = Path(directory) / "t.jsonl"
            transcript.write_text("one line\n", encoding="utf-8")
            size = transcript.stat().st_size
            self.assertFalse(session._transcript_advanced(str(transcript), size))
            self.assertTrue(session._transcript_advanced(str(transcript), size - 1))


class TranscriptOffsetTests(unittest.TestCase):
    def test_missing_transcript_reports_zero(self):
        state = {"reviewer": {"transcript": "/nonexistent/t.jsonl"}}
        self.assertEqual(session._transcript_offset(state, "reviewer"), 0)

    def test_existing_transcript_reports_size(self):
        with tempfile.TemporaryDirectory() as directory:
            transcript = Path(directory) / "t.jsonl"
            transcript.write_text("payload\n", encoding="utf-8")
            state = {"reviewer": {"transcript": str(transcript)}}
            self.assertEqual(
                session._transcript_offset(state, "reviewer"),
                transcript.stat().st_size,
            )


class UserMarkerOffsetTests(unittest.TestCase):
    def write_transcript(self, directory, records):
        transcript = Path(directory) / "codex.jsonl"
        transcript.write_text(
            "".join(json.dumps(record) + "\n" for record in records),
            encoding="utf-8",
        )
        return transcript

    def user_record(self, text):
        return {
            "type": "response_item",
            "payload": {
                "type": "message",
                "role": "user",
                "content": [{"type": "input_text", "text": text}],
            },
        }

    def test_returns_byte_offset_of_marker_bearing_user_message(self):
        with tempfile.TemporaryDirectory() as directory:
            first = self.user_record("unrelated prompt")
            second = self.user_record("body\nREVIEW COMPLETE: plan")
            transcript = self.write_transcript(directory, [first, second])
            expected = len(json.dumps(first)) + 1
            self.assertEqual(
                session._user_marker_offset(transcript, "REVIEW COMPLETE: plan"),
                expected,
            )

    def test_missing_marker_and_missing_file_return_none(self):
        with tempfile.TemporaryDirectory() as directory:
            transcript = self.write_transcript(
                directory, [self.user_record("nothing here")]
            )
            self.assertIsNone(session._user_marker_offset(transcript, "MARKER"))
        self.assertIsNone(
            session._user_marker_offset("/nonexistent/t.jsonl", "MARKER")
        )

    def test_undecodable_lines_are_skipped_without_losing_offsets(self):
        with tempfile.TemporaryDirectory() as directory:
            transcript = Path(directory) / "codex.jsonl"
            garbage = b"not json at all\n"
            record = json.dumps(self.user_record("MARKER")).encode() + b"\n"
            transcript.write_bytes(garbage + record)
            self.assertEqual(
                session._user_marker_offset(transcript, "MARKER"), len(garbage)
            )


class TranscriptMessageTests(unittest.TestCase):
    def test_reads_claude_codex_and_task_complete_shapes(self):
        records = [
            {
                "timestamp": "2026-08-24T10:00:00Z",
                "message": {"role": "user", "content": "a user prompt"},
            },
            {
                "timestamp": "2026-08-24T10:01:00Z",
                "message": {
                    "role": "assistant",
                    "content": [{"type": "text", "text": "claude reply"}],
                },
            },
            {
                "timestamp": "2026-08-24T10:02:00Z",
                "type": "response_item",
                "payload": {
                    "type": "message",
                    "content": [{"type": "output_text", "text": "codex reply"}],
                },
            },
            {
                "timestamp": "2026-08-24T10:03:00Z",
                "type": "event_msg",
                "payload": {
                    "type": "task_complete",
                    "last_agent_message": "final answer",
                },
            },
        ]
        with tempfile.TemporaryDirectory() as directory:
            transcript = Path(directory) / "t.jsonl"
            transcript.write_text(
                "".join(json.dumps(record) + "\n" for record in records),
                encoding="utf-8",
            )
            messages = session.transcript_messages(transcript)

        self.assertEqual(
            [(m["kind"], m["text"]) for m in messages],
            [
                ("assistant", "claude reply"),
                ("message", "codex reply"),
                ("complete", "final answer"),
            ],
        )

    def test_offset_bounds_the_read_and_stale_offset_returns_nothing(self):
        record = {
            "timestamp": "2026-08-24T10:00:00Z",
            "message": {"role": "assistant", "content": "bounded"},
        }
        with tempfile.TemporaryDirectory() as directory:
            transcript = Path(directory) / "t.jsonl"
            line = json.dumps(record) + "\n"
            transcript.write_text(line * 2, encoding="utf-8")
            self.assertEqual(
                len(session.transcript_messages(transcript, offset=len(line))), 1
            )
            self.assertEqual(
                session.transcript_messages(transcript, offset=len(line) * 2 + 1),
                [],
            )


class EmacsBoundaryTests(unittest.TestCase):
    def test_run_emacs_json_round_trips_through_private_temp_file(self):
        def emulate_emacs(expr):
            marker = '(let ((out '
            start = expr.index(marker) + len(marker)
            end = expr.index('))', start)
            path = Path(json.loads(expr[start:end]))
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)
            path.write_text('{"state":"busy"}', encoding="utf-8")
            return "nil"

        with mock.patch.object(session, "run_emacs_eval", side_effect=emulate_emacs):
            self.assertEqual(session.run_emacs_json("'x"), {"state": "busy"})

    def test_buffer_backend_reports_detected_symbol_or_none(self):
        with mock.patch.object(session, "run_emacs_eval", return_value="codex"):
            self.assertEqual(session.buffer_backend("*codex:x*"), "codex")
        with mock.patch.object(session, "run_emacs_eval", return_value="none"):
            self.assertIsNone(session.buffer_backend("*scratch*"))

    def test_submit_function_rejects_unknown_backend(self):
        with self.assertRaises(SystemExit):
            session._submit_function("gemini")
        self.assertEqual(session._submit_function("claude-code"), "agent-submit")
        self.assertEqual(session._submit_function("codex"), "agent-submit")


if __name__ == "__main__":
    unittest.main()
