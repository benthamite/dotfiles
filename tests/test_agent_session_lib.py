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


class MarkerDeliveredTests(unittest.TestCase):
    def write(self, directory, records):
        transcript = Path(directory) / "t.jsonl"
        transcript.write_text(
            "".join(json.dumps(r) + "\n" for r in records), encoding="utf-8"
        )
        return transcript

    @staticmethod
    def claude_user(text):
        return {"type": "user", "message": {"role": "user", "content": text}}

    @staticmethod
    def codex_user(text):
        return {
            "type": "response_item",
            "payload": {
                "type": "message",
                "role": "user",
                "content": [{"type": "input_text", "text": text}],
            },
        }

    def test_missing_transcript_is_not_delivered(self):
        self.assertFalse(session._marker_delivered("/nonexistent/t.jsonl", 0, "M"))

    def test_marker_bearing_user_message_past_offset_is_delivered(self):
        with tempfile.TemporaryDirectory() as directory:
            first = self.claude_user("earlier prompt")
            transcript = self.write(directory, [first])
            offset = transcript.stat().st_size
            with transcript.open("a", encoding="utf-8") as stream:
                stream.write(json.dumps(self.claude_user("ORCHESTRATOR RULING x")) + "\n")
            self.assertTrue(
                session._marker_delivered(str(transcript), offset, "ORCHESTRATOR RULING")
            )
            self.assertTrue(
                session._marker_delivered(str(transcript), 0, "earlier prompt")
            )

    def test_growth_without_the_marker_is_not_delivered(self):
        with tempfile.TemporaryDirectory() as directory:
            transcript = self.write(directory, [self.claude_user("earlier prompt")])
            offset = transcript.stat().st_size
            bookkeeping = [
                {"type": "attachment", "attachment": {"type": "agent_listing_delta"}},
                {"type": "system", "subtype": "compact_boundary"},
                {"type": "assistant", "message": {"role": "assistant",
                                                  "content": [{"type": "text", "text": "MARKER"}]}},
            ]
            with transcript.open("a", encoding="utf-8") as stream:
                for record in bookkeeping:
                    stream.write(json.dumps(record) + "\n")
            self.assertGreater(transcript.stat().st_size, offset)
            self.assertFalse(session._marker_delivered(str(transcript), offset, "MARKER"))

    def test_marker_before_the_offset_does_not_count(self):
        with tempfile.TemporaryDirectory() as directory:
            transcript = self.write(directory, [self.claude_user("MARKER old")])
            offset = transcript.stat().st_size
            self.assertFalse(session._marker_delivered(str(transcript), offset, "MARKER"))

    def test_codex_user_shape_is_recognized(self):
        with tempfile.TemporaryDirectory() as directory:
            transcript = self.write(directory, [self.codex_user("body\nREVIEW: go")])
            self.assertTrue(session._marker_delivered(str(transcript), 0, "REVIEW: go"))


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
