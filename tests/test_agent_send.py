import importlib.machinery
import importlib.util
import io
import json
import os
import re
import stat
import subprocess
import sys
import tempfile
import textwrap
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin/agent-send"


def load_module():
    loader = importlib.machinery.SourceFileLoader("agent_send", str(SCRIPT))
    spec = importlib.util.spec_from_loader("agent_send", loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


agent_send = load_module()
session = agent_send.session


def record(buffer, directory, *, backend="codex", instance="default",
           session_id="sid", transcript=None, state="awaiting-input"):
    return {"buffer": buffer, "backend": backend, "directory": directory,
            "instance": instance, "session_id": session_id,
            "transcript": transcript, "state": state}


SESSIONS = [
    record("*codex:~/repos/alpha/:default*", "/Users/me/repos/alpha", session_id="a"),
    record("*claude:~/repos/beta/:default*", "/Users/me/repos/beta",
           backend="claude-code", session_id="b"),
    record("*claude:~/repos/beta/:review*", "/Users/me/repos/beta",
           backend="claude-code", instance="review", session_id="c"),
    {"buffer": "*codex:~/broken/:default*", "error": "boom"},
]


class ResolveTargetTests(unittest.TestCase):
    def resolve(self, target, own=frozenset()):
        return agent_send.resolve_target(target, SESSIONS, set(own))

    def test_exact_buffer_name_wins_even_when_substrings_are_ambiguous(self):
        chosen = self.resolve("*claude:~/repos/beta/:default*")
        self.assertEqual(chosen["session_id"], "b")

    def test_unique_instance_name(self):
        self.assertEqual(self.resolve("review")["session_id"], "c")

    def test_unique_directory_substring(self):
        self.assertEqual(self.resolve("alpha")["session_id"], "a")

    def test_abbreviated_directory_from_buffer_name(self):
        self.assertEqual(self.resolve("~/repos/alpha")["session_id"], "a")

    def test_ambiguous_match_lists_only_the_matches(self):
        with self.assertRaises(agent_send.Refusal) as caught:
            self.resolve("beta")
        message = str(caught.exception)
        self.assertIn("several sessions match", message)
        self.assertIn("*claude:~/repos/beta/:default*", message)
        self.assertIn("*claude:~/repos/beta/:review*", message)
        self.assertNotIn("alpha", message)

    def test_shared_instance_name_is_ambiguous(self):
        with self.assertRaises(agent_send.Refusal):
            self.resolve("default")

    def test_missing_target_lists_all_live_candidates(self):
        with self.assertRaises(agent_send.Refusal) as caught:
            self.resolve("gamma")
        self.assertIn("no live session matches", str(caught.exception))
        self.assertIn("*codex:~/repos/alpha/:default*", str(caught.exception))

    def test_errored_buffers_are_never_targets(self):
        with self.assertRaises(agent_send.Refusal):
            self.resolve("broken")

    def test_self_send_is_refused(self):
        with self.assertRaises(agent_send.Refusal) as caught:
            self.resolve("alpha", own={"*codex:~/repos/alpha/:default*"})
        self.assertIn("own buffer", str(caught.exception))

    def test_own_buffers_come_from_both_variables(self):
        env = {"CLAUDE_BUFFER_NAME": "*claude:x*", "CODEX_BUFFER_NAME": "*codex:y*"}
        self.assertEqual(agent_send.own_buffers(env), {"*claude:x*", "*codex:y*"})
        self.assertEqual(agent_send.own_buffers({}), set())


class HeaderAndMessageTests(unittest.TestCase):
    def test_header_precedes_message_after_a_blank_line(self):
        prompt = agent_send.compose("hello", sender="*claude:~/x/:default*",
                                    cwd="/Users/me/x", message_id="agent-send-123")
        header, blank, body = prompt.split("\n", 2)
        self.assertEqual(blank, "")
        self.assertEqual(body, "hello")
        self.assertTrue(header.startswith("[Message from another agent session"))
        for part in ("*claude:~/x/:default*", "/Users/me/x", "agent-send-123",
                     "not typed by Pablo"):
            self.assertIn(part, header)

    def test_header_cannot_be_mistaken_for_a_harness_injection(self):
        prompt = agent_send.compose("m", sender="s", cwd="/", message_id="i")
        self.assertFalse(prompt.startswith(session._INJECTED_USER_PREFIXES))
        self.assertFalse(prompt.lstrip().startswith(("/", "!", "#")))

    def test_unknown_sender(self):
        self.assertEqual(agent_send.sender_label({}), "unknown session")

    def test_message_normalization(self):
        self.assertEqual(agent_send.normalize_message(b'a "q"\r\nb \\ c\n\n'),
                         'a "q"\nb \\ c')

    def test_empty_oversized_and_invalid_messages_are_refused(self):
        for raw in (b"", b" \n\t\n", b"x" * (agent_send.MAX_MESSAGE_BYTES + 1),
                    b"\xff\xfe"):
            with self.subTest(raw=raw[:8]), self.assertRaises(agent_send.Refusal):
                agent_send.normalize_message(raw)

    def test_message_ids_are_unique(self):
        self.assertNotEqual(agent_send.new_message_id(), agent_send.new_message_id())


def user_line(text, backend):
    if backend == "codex":
        obj = {"type": "response_item", "payload": {
            "type": "message", "role": "user",
            "content": [{"type": "input_text", "text": text}]}}
    else:
        obj = {"type": "user", "message": {"role": "user", "content": text}}
    return json.dumps(obj) + "\n"


class DeliveryTests(unittest.TestCase):
    """Drive deliver() against a fake Emacs that reads the prompt file."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.transcript = Path(self.tmp.name) / "t.jsonl"
        self.transcript.write_text(user_line("earlier", "codex"), encoding="utf-8")
        self.identity = record("*codex:~/repos/alpha/:default*",
                               "/Users/me/repos/alpha",
                               transcript=str(self.transcript.resolve()))
        self.expressions = []
        self.seen_prompt_modes = []
        patches = [
            mock.patch.object(session, "actor_identity", return_value=dict(self.identity)),
            mock.patch.object(session, "agent_transcript_path",
                              return_value=str(self.transcript)),
            mock.patch.object(session, "DELIVERY_POLL_SECONDS", 0.01),
        ]
        for patch in patches:
            patch.start()
            self.addCleanup(patch.stop)
        self.addCleanup(self.tmp.cleanup)

    def fake_emacs(self, recorded_text_fn):
        def run(expr):
            self.expressions.append(expr)
            match = re.search(r'\(insert-file-contents (".*?")\)', expr)
            path = json.loads(match.group(1))
            self.seen_prompt_modes.append(stat.S_IMODE(os.stat(path).st_mode))
            text = Path(path).read_text(encoding="utf-8")
            recorded = recorded_text_fn(text)
            if recorded is not None:
                with self.transcript.open("a", encoding="utf-8") as stream:
                    stream.write(user_line(recorded, "codex"))
            return "submitted"
        return run

    def send(self, message, recorded_text_fn, wait=0.3):
        prompt = agent_send.compose(message, sender="me", cwd="/c", message_id="agent-send-x1")
        with mock.patch.object(session, "run_emacs_eval",
                               side_effect=self.fake_emacs(recorded_text_fn)):
            return prompt, agent_send.deliver(dict(self.identity), prompt,
                                              "agent-send-x1", wait)

    def test_quotes_backslashes_and_newlines_cross_via_a_private_file(self):
        message = 'line "one"\n(error "x") \\ ` ,@\nline three'
        prompt, transcript = self.send(message, lambda text: text)
        self.assertEqual(transcript, str(self.transcript.resolve()))
        expr = self.expressions[0]
        self.assertNotIn("line three", expr)  # never an Elisp string literal
        self.assertEqual(self.seen_prompt_modes, [0o600])
        self.assertIn("save-window-excursion", expr)
        self.assertIn("agent-submit", expr)
        delivered = json.loads(self.transcript.read_text().splitlines()[-1])
        self.assertEqual(delivered["payload"]["content"][0]["text"], prompt)

    def test_prompt_file_is_removed(self):
        self.send("hi", lambda text: text)
        path = json.loads(re.search(r'\(insert-file-contents (".*?")\)',
                                    self.expressions[0]).group(1))
        self.assertFalse(Path(path).exists())

    def test_cli_normalized_text_is_confirmed_by_the_id(self):
        _, transcript = self.send("hi  there", lambda text: text.replace("  ", " "))
        self.assertEqual(Path(transcript), self.transcript.resolve())

    def test_missing_receipt_is_unconfirmed_and_sent_once(self):
        with self.assertRaises(session.EmacsClientError):
            self.send("hi", lambda text: None)
        self.assertEqual(len(self.expressions), 1)

    def test_old_transcript_records_are_not_a_receipt(self):
        self.transcript.write_text(user_line("[... id agent-send-x1 ...]", "codex"),
                                   encoding="utf-8")
        with self.assertRaises(session.EmacsClientError):
            self.send("hi", lambda text: None)

    def test_identity_change_before_send_is_a_refusal(self):
        target = dict(self.identity)
        changed = {**self.identity, "session_id": "other"}
        with mock.patch.object(session, "actor_identity", return_value=changed):
            with self.assertRaises(agent_send.Refusal):
                agent_send.confirmed_identity(target)

    def test_session_without_transcript_or_id_is_refused(self):
        target = {**self.identity, "transcript": None, "session_id": None}
        with mock.patch.object(session, "actor_identity", return_value=dict(target)):
            with self.assertRaises(agent_send.Refusal):
                agent_send.confirmed_identity(target)


FAKE_EMACSCLIENT = textwrap.dedent('''\
    #!/usr/bin/env python3
    import json, os, re, sys
    expr = sys.argv[-1]
    log = os.environ["FAKE_EMACS_LOG"]
    with open(log, "a") as stream:
        stream.write(json.dumps(expr) + "\\n")
    match = re.search(r'\\(let \\(\\(out (".*?")\\)\\)', expr)
    if match:
        with open(json.loads(match.group(1)), "w") as stream:
            stream.write(os.environ["FAKE_EMACS_SESSIONS"])
        print("nil")
    else:
        print('"submitted"')
''')


class CommandLineTests(unittest.TestCase):
    """Run the real executable against a fake emacsclient on PATH."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        bindir = Path(self.tmp.name)
        fake = bindir / "emacsclient"
        fake.write_text(FAKE_EMACSCLIENT, encoding="utf-8")
        fake.chmod(0o755)
        self.log = bindir / "log.jsonl"
        self.env = {
            **os.environ,
            "PATH": f"{bindir}:{os.environ['PATH']}",
            "FAKE_EMACS_LOG": str(self.log),
            "FAKE_EMACS_SESSIONS": json.dumps(SESSIONS[:3]),
            "CLAUDE_BUFFER_NAME": "*claude:~/repos/beta/:default*",
        }
        self.env.pop("CODEX_BUFFER_NAME", None)

    def run_cli(self, *argv, stdin=""):
        return subprocess.run([sys.executable, str(SCRIPT), *argv], input=stdin,
                              capture_output=True, text=True, env=self.env, timeout=60)

    def expressions(self):
        if not self.log.exists():
            return []
        return [json.loads(line) for line in self.log.read_text().splitlines()]

    def test_help_documents_the_contract(self):
        result = self.run_cli("--help")
        self.assertEqual(result.returncode, 0)
        for phrase in ("--list", "stdin", "not typed by Pablo", "UNCONFIRMED", "never resends",
                       "Remote Control"):
            self.assertIn(phrase.lower(), " ".join(result.stdout.lower().split()))

    def test_list_marks_this_session_and_states(self):
        result = self.run_cli("--list")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("*claude:~/repos/beta/:default* (this session)", result.stdout)
        self.assertIn("backend=codex state=idle instance=default session=a", result.stdout)
        self.assertIn("dir=/Users/me/repos/alpha", result.stdout)

    def test_list_json(self):
        result = self.run_cli("--list", "--json")
        records = json.loads(result.stdout)
        self.assertEqual([r["state"] for r in records], ["idle"] * 3)

    def test_message_in_arguments_is_refused_without_contacting_emacs(self):
        result = self.run_cli("alpha", "hello", "there")
        self.assertEqual(result.returncode, 1)
        self.assertIn("stdin", result.stderr)
        self.assertEqual(self.expressions(), [])

    def test_self_send_is_refused_before_any_submit(self):
        result = self.run_cli("*claude:~/repos/beta/:default*", stdin="hi\n")
        self.assertEqual(result.returncode, 1)
        self.assertIn("own buffer", result.stderr)
        self.assertFalse(any("agent-submit" in e for e in self.expressions()))

    def test_ambiguous_target_is_refused_before_any_submit(self):
        result = self.run_cli("repos", stdin="hi\n")
        self.assertEqual(result.returncode, 1)
        self.assertIn("several sessions match", result.stderr)
        self.assertFalse(any("agent-submit" in e for e in self.expressions()))

    def test_empty_stdin_is_refused(self):
        result = self.run_cli("alpha", stdin="")
        self.assertEqual(result.returncode, 1)
        self.assertIn("empty", result.stderr)


if __name__ == "__main__":
    unittest.main()
