import importlib.util
import hashlib
import json
import os
import subprocess
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


class CorrelatedEvidenceTests(unittest.TestCase):
    def setUp(self):
        self.scratch = tempfile.TemporaryDirectory(prefix="agent-evidence-test-", dir="/tmp")
        self.addCleanup(self.scratch.cleanup)
        self.path = Path(self.scratch.name) / "synthetic.jsonl"
        self.prompt = "Review synthetic artifact A\nRECEIPT: unique-attempt"
        self.sha = hashlib.sha256(self.prompt.encode()).hexdigest()

    def write(self, *records):
        self.path.write_text("".join(json.dumps(obj) + "\n" for obj in records))

    def user(self, text=None):
        return MarkerDeliveredTests.claude_user(self.prompt if text is None else text)

    def assistant(self, text="PHASE COMPLETE: spec", stop_reason=None, extra=None):
        content = [{"type": "text", "text": text}]
        if extra:
            content.append(extra)
        return {"type": "assistant", "message": {
            "role": "assistant", "stop_reason": stop_reason, "content": content}}

    def codex(self, kind="message", **values):
        return {"type": "response_item", "payload": {"type": kind, **values}}

    def terminal(self):
        return session.latest_transcript_return(self.path, expected_prompt_sha256=self.sha)

    def test_same_marker_different_prompt_cannot_acknowledge(self):
        self.write(self.user(self.prompt.replace("artifact A", "artifact B")))
        self.assertFalse(session._marker_delivered(
            str(self.path), 0, "RECEIPT:", expected_prompt_sha256=self.sha))
        self.assertIsNone(session._user_marker_offset(
            self.path, "RECEIPT:", expected_prompt_sha256=self.sha))

    def test_exact_full_prompt_receipt_supports_both_schemas(self):
        for record in (self.user(), MarkerDeliveredTests.codex_user(self.prompt)):
            with self.subTest(record=record["type"]):
                self.write(record)
                self.assertTrue(session._marker_delivered(
                    str(self.path), 0, "RECEIPT:", expected_prompt_sha256=self.sha))
                self.assertEqual(session._user_marker_offset(
                    self.path, "RECEIPT:", expected_prompt_sha256=self.sha), 0)

    def test_claude_null_stop_reason_text_is_a_terminal_candidate(self):
        self.write(self.user(), self.assistant())
        self.assertEqual(self.terminal()["text"], "PHASE COMPLETE: spec")

    def test_claude_tool_use_text_is_output_but_not_a_return(self):
        self.write(self.user(), self.assistant(stop_reason="tool_use", extra={
            "type": "tool_use", "id": "toolu_fixture", "name": "Read", "input": {}}))
        self.assertTrue(session.transcript_has_output(self.path))
        self.assertIsNone(self.terminal())

    def test_explicit_claude_terminal_thinking_and_text_returns_only_visible_text(self):
        for reason in ("end_turn", "stop_sequence"):
            for block in ({"type": "thinking", "thinking": "private fixture reasoning"},
                          {"type": "redacted_thinking", "data": "opaque"}):
                with self.subTest(reason=reason, block=block["type"]):
                    self.write(self.user(), self.assistant(stop_reason=reason, extra=block))
                    self.assertEqual(self.terminal()["text"], "PHASE COMPLETE: spec")

    def test_thinking_only_and_null_stop_mixed_blocks_are_not_terminal(self):
        self.write(self.user(), self.assistant(extra={"type": "thinking", "thinking": "fixture"}))
        self.assertIsNone(self.terminal())
        self.write(self.user(), {"type": "assistant", "message": {
            "role": "assistant", "stop_reason": "end_turn", "content": [
                {"type": "thinking", "thinking": "fixture"}]}})
        self.assertIsNone(self.terminal())
        self.assertTrue(session.transcript_has_output(self.path))

    def test_explicit_end_turn_never_overrides_tool_use_block(self):
        self.write(self.user(), self.assistant(stop_reason="end_turn", extra={
            "type": "tool_use", "id": "fixture", "name": "Read", "input": {}}))
        self.assertIsNone(self.terminal())

    def test_explicit_nonterminal_stop_reasons_are_not_returns(self):
        for reason in ("tool_use", "max_tokens", "pause_turn"):
            with self.subTest(reason=reason):
                self.write(self.user(), self.assistant(stop_reason=reason))
                self.assertIsNone(self.terminal())

    def test_tool_only_codex_output_blocks_restart(self):
        self.write(self.user(), self.codex("function_call", name="exec_command",
                                          arguments="{}", call_id="fixture"))
        self.assertTrue(session.transcript_has_output(self.path))
        self.assertIsNone(self.terminal())

    def test_codex_final_phase_and_channel_are_required_when_recorded(self):
        for values, accepted in (({"phase": "final_answer"}, True),
                                 ({"channel": "final"}, True),
                                 ({}, True), ({"phase": "commentary"}, False),
                                 ({"channel": "analysis"}, False)):
            with self.subTest(values=values):
                self.write(self.user(), self.codex(role="assistant", **values,
                           content=[{"type": "output_text", "text": "COMPLETE"}]))
                self.assertEqual(self.terminal() is not None, accepted)

    def test_codex_nonassistant_output_text_cannot_finish(self):
        self.write(self.user(), self.codex(role="user", phase="final_answer",
                   content=[{"type": "output_text", "text": "COMPLETE"}]))
        self.assertIsNone(self.terminal())

    def test_later_activity_invalidates_earlier_final_candidate(self):
        later = [self.codex("reasoning", summary=[]),
                 self.codex("function_call_output", output="fixture"),
                 self.assistant("thinking", stop_reason="tool_use"),
                 {"type": "user", "message": {"role": "user", "content": [
                     {"type": "tool_result", "tool_use_id": "fixture", "content": "result"}]}}]
        for record in later:
            with self.subTest(record=record):
                self.write(self.user(), self.assistant(), record)
                self.assertIsNone(self.terminal())

    def test_later_unrelated_user_and_matching_final_cannot_finish_old_turn(self):
        self.write(self.user(), self.assistant(), self.user("unrelated user task"),
                   self.assistant())
        self.assertIsNone(self.terminal())

    def test_codex_user_event_resets_turn_correlation(self):
        self.write(self.user(), self.assistant(), {"type": "event_msg", "payload": {
            "type": "user_message", "message": "unrelated user task"}},
            {"type": "event_msg", "payload": {
                "type": "task_complete", "last_agent_message": "PHASE COMPLETE: spec"}})
        self.assertIsNone(self.terminal())

    def test_task_complete_is_terminal_for_matching_prompt(self):
        self.write(self.user(), self.codex("reasoning", summary=[]),
                   {"type": "event_msg", "payload": {
                       "type": "task_complete", "last_agent_message": "COMPLETE"}})
        self.assertEqual(self.terminal()["kind"], "complete")

    def test_lost_unreadable_truncated_or_malformed_evidence_refuses(self):
        for function in (session.transcript_has_output, session.latest_transcript_return):
            with self.subTest(function=function.__name__):
                with self.assertRaises(session.EmacsClientError):
                    function(self.path)
                with mock.patch.object(Path, "open", side_effect=PermissionError):
                    with self.assertRaises(session.EmacsClientError):
                        function(self.path)
                for content, offset in ((b"", 1), (b"{}", 0), (b"no-json\n", 0),
                                        (b"[]\n", 0), (b"{}\n", 1), (b"\xff\n", 0)):
                    self.path.write_bytes(content)
                    with self.assertRaises(session.EmacsClientError):
                        function(self.path, offset=offset)
                self.path.unlink()

    def test_empty_complete_transcript_can_prove_no_output(self):
        self.write(self.user(), {"type": "system", "subtype": "compact_boundary"})
        self.assertFalse(session.transcript_has_output(self.path))
        self.assertIsNone(self.terminal())

    def startup(self, backend="codex"):
        return session.transcript_is_startup_only(self.path, backend=backend,
            expected_session_id="fixture-id", expected_directory=self.scratch.name)

    def metadata(self, **changes):
        return {"type": "session_meta", "payload": {
            "id": "fixture-id", "cwd": self.scratch.name, **changes}}

    def test_startup_accepts_empty_or_exact_codex_metadata(self):
        self.write()
        self.assertTrue(self.startup())
        self.assertTrue(self.startup("claude-code"))
        self.write(self.metadata(cwd=str(Path(self.scratch.name).resolve())))
        self.assertTrue(self.startup())

    def test_startup_rejects_wrong_identity_directory_backend_and_unknown_history(self):
        for record in (self.metadata(id="other"), self.metadata(cwd="/other"),
                       self.metadata(cwd="relative"), self.metadata(id=None),
                       self.user(), self.assistant(), {"type": "turn_context", "payload": {}},
                       {"type": "system", "subtype": "init"},
                       self.codex("function_call", name="Read")):
            with self.subTest(record=record):
                self.write(self.metadata(), record)
                self.assertFalse(self.startup())
        self.write(self.metadata())
        self.assertFalse(self.startup("claude-code"))

    def test_startup_refuses_missing_and_invalid_evidence(self):
        with self.assertRaises(session.EmacsClientError):
            self.startup()
        self.path.write_bytes(b'{"type":"session_meta"}')
        with self.assertRaises(session.EmacsClientError):
            self.startup()


class GuardedDispatchTests(unittest.TestCase):
    def setUp(self):
        self.scratch = tempfile.TemporaryDirectory(prefix="agent-dispatch-test-", dir="/tmp")
        self.addCleanup(self.scratch.cleanup)
        self.path = Path(self.scratch.name) / "synthetic.jsonl"
        self.path.write_bytes(b"")
        self.identity = {"buffer": "*fixture*", "backend": "codex",
                         "directory": str(Path(self.scratch.name).resolve()),
                         "transcript": str(self.path.resolve()), "session_id": "fixture-id",
                         "state": "awaiting-input"}

    def submit(self, **kwargs):
        args = {"transcript": str(self.path), "transcript_offset": 0,
                "delivery_marker": "RECEIPT", "expected_identity": self.identity}
        args.update(kwargs)
        return session.submit_to_agent("*fixture*", "codex", "body\nRECEIPT", **args)

    def test_actor_identity_is_a_single_canonical_sample(self):
        with mock.patch.object(session, "run_emacs_json", return_value=self.identity.copy()) as rpc:
            self.assertEqual(session.actor_identity("*fixture*"), self.identity)
            self.assertEqual(rpc.call_count, 1)
            self.assertIn("agent-session-display-state", rpc.call_args.args[0])
            self.assertIn("codex-session-identity", rpc.call_args.args[0])

    def test_submit_checks_exact_receipt_and_returns_canonical_path(self):
        with mock.patch.object(session, "run_emacs_eval", return_value="submitted") as rpc, \
             mock.patch.object(session, "actor_identity", return_value=self.identity), \
             mock.patch.object(session, "_wait_for_delivery", return_value=True) as wait:
            self.assertEqual(self.submit(), str(self.path.resolve()))
            self.assertIn("agent-claude-submit-retries 0", rpc.call_args.args[0])
            self.assertIn("actor identity or lifecycle changed", rpc.call_args.args[0])
            self.assertEqual(wait.call_args.kwargs["expected_prompt_sha256"],
                             hashlib.sha256(b"body\nRECEIPT").hexdigest())

    def test_fresh_session_adopts_only_same_identity_transcript(self):
        fresh = {**self.identity, "transcript": None}
        with mock.patch.object(session, "run_emacs_eval", return_value="submitted"), \
             mock.patch.object(session, "actor_identity", return_value=self.identity), \
             mock.patch.object(session, "_wait_for_delivery", return_value=True):
            self.assertEqual(self.submit(transcript=None, expected_identity=fresh),
                             str(self.path.resolve()))

    def test_fresh_session_without_id_never_dispatches(self):
        with mock.patch.object(session, "run_emacs_eval") as rpc:
            with self.assertRaises(session.EmacsClientError):
                self.submit(transcript=None, expected_identity={**self.identity, "session_id": None})
            rpc.assert_not_called()

    def test_identity_change_during_delivery_leaves_outcome_pending(self):
        with mock.patch.object(session, "run_emacs_eval", return_value="submitted"), \
             mock.patch.object(session, "actor_identity", return_value={**self.identity, "session_id": "other"}), \
             mock.patch.object(session, "_wait_for_delivery") as wait:
            with self.assertRaises(session.EmacsClientError):
                self.submit()
            wait.assert_not_called()

    def test_new_transcript_path_is_pinned_during_receipt_wait(self):
        fresh = {**self.identity, "transcript": None}
        with mock.patch.object(session, "run_emacs_eval", return_value="submitted"), \
             mock.patch.object(session, "actor_identity", side_effect=[self.identity,
                 {**self.identity, "transcript": str(self.path) + "-other"}]), \
             mock.patch.object(session, "_wait_for_delivery", return_value=True):
            with self.assertRaises(session.EmacsClientError):
                self.submit(transcript=None, expected_identity=fresh)

    def test_one_pass_never_retries_ambiguous_delivery(self):
        with mock.patch.object(session, "run_emacs_eval", return_value="submitted"), \
             mock.patch.object(session, "actor_identity", return_value=self.identity), \
             mock.patch.object(session, "_wait_for_delivery", return_value=False), \
             mock.patch.object(session, "send_return_to_agent") as retry:
            with self.assertRaises(session.EmacsClientError):
                self.submit(one_pass=True)
            retry.assert_not_called()

    def test_hashed_claude_composer_retry_refuses_without_rpc(self):
        with mock.patch.object(session, "run_emacs_eval") as rpc:
            self.assertFalse(session.pending_prompt_contains("*fixture*", "claude-code", "RECEIPT",
                                                             expected_prompt_sha256="a" * 64))
            with self.assertRaises(session.EmacsClientError):
                session.send_return_to_agent("*fixture*", "claude-code", expected_prompt_sha256="a" * 64)
            rpc.assert_not_called()

    def native_eval(self, expr):
        binary = os.environ.get("EMACS_TEST_BINARY", "/opt/homebrew/bin/emacs")
        if not Path(binary).is_file():
            self.skipTest("a standalone Emacs test binary is unavailable")
        return subprocess.run([binary, "-Q", "--batch", "--eval", expr],
                              text=True, capture_output=True, timeout=15, check=True).stdout

    def native_prelude(self):
        return f'''
          (defvar fixture-dispatched 0)
          (defvar fixture-returns 0)
          (defvar fixture-input "original")
          (defvar fixture-session-id "fixture-id")
          (defvar agent-claude-submit-retries 3)
          (defun agent--detect-backend (&rest _) 'codex)
          (defun agent-session-display-state (&rest _) 'waiting)
          (defun codex-session-identity (&rest _) (list :session-id fixture-session-id))
          (defun codex--find-session-transcript (&rest _) {session.elisp_string(str(self.path.resolve()))})
          (defun codex-prompt-input (&rest _) fixture-input)
          (defun agent-send-return (target) (setq fixture-returns (1+ fixture-returns)) target)
          (defun agent-submit (_prompt target)
            (setq fixture-dispatched (1+ fixture-dispatched))
            (run-at-time 0.01 nil (lambda (remaining)
              (when (> remaining 0) (setq fixture-returns (1+ fixture-returns))))
              agent-claude-submit-retries)
            target)
          (with-current-buffer (get-buffer-create "*fixture*")
            (setq default-directory {session.elisp_string(self.identity["directory"] + "/")}))
        '''

    def test_native_guarded_submit_disables_backend_delayed_returns(self):
        def rpc(expr):
            output = self.native_eval(f"(progn {self.native_prelude()} {expr} "
                                      '(sleep-for 0.05) (princ (format "|%s|%s" fixture-dispatched fixture-returns)))')
            self.assertEqual(output, "submitted|1|0")
            return "submitted"
        with mock.patch.object(session, "run_emacs_eval", side_effect=rpc), \
             mock.patch.object(session, "actor_identity", return_value=self.identity), \
             mock.patch.object(session, "_wait_for_delivery", return_value=True):
            self.submit(one_pass=True)

    def test_native_same_rpc_guard_refuses_actor_switch_before_dispatch(self):
        def rpc(expr):
            output = self.native_eval(f"(progn {self.native_prelude()} "
                '(setq fixture-session-id "other") '
                f'(condition-case nil {expr} (error (princ "refused"))) '
                '(princ (format "|%s" fixture-dispatched)))')
            self.assertEqual(output, "refused|0")
            raise session.EmacsClientError("fixture identity switched")
        with mock.patch.object(session, "run_emacs_eval", side_effect=rpc):
            with self.assertRaises(session.EmacsClientError):
                self.submit()

    def test_native_return_rechecks_full_composer_hash_before_dispatch(self):
        def rpc(expr):
            output = self.native_eval(f"(progn {self.native_prelude()} "
                '(setq fixture-input "edited composer") '
                f'(condition-case nil {expr} (error (princ "refused"))) '
                '(princ (format "|%s" fixture-returns)))')
            self.assertEqual(output, "refused|0")
            raise session.EmacsClientError("fixture composer changed")
        with mock.patch.object(session, "run_emacs_eval", side_effect=rpc):
            with self.assertRaises(session.EmacsClientError):
                session.send_return_to_agent("*fixture*", "codex", expected_identity=self.identity,
                    expected_prompt_sha256=hashlib.sha256(b"original").hexdigest())


if __name__ == "__main__":
    unittest.main()
