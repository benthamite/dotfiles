"""Offline helper regressions: fake processes, clipboard, wire and socket trips."""
import argparse
import contextlib
import copy
import http.client
import importlib.machinery
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import unittest
from unittest import mock
import urllib.error
import urllib.request

ROOT = Path(__file__).resolve().parents[1]
MARKER = "offline-secret-marker-not-a-token"
ITEM = "a" * 26


def tripwire(*args, **kwargs):
    raise AssertionError("unmocked external process or network access")


def load(name, filename):
    loader = importlib.machinery.SourceFileLoader(name, str(ROOT / "claude/bin" / filename))
    module = importlib.util.module_from_spec(importlib.util.spec_from_loader(name, loader))
    with mock.patch("subprocess.run", side_effect=tripwire), mock.patch("socket.socket", side_effect=tripwire):
        loader.exec_module(module)
    return module


SLACK = load("slack_transport_fixture", "slack.py")
STORE = load("clipboard_store_fixture", "op-clipboard-store")


class Response:
    def __init__(self, body, content_type="application/json"):
        self.body, self.closed = body, False
        self.headers = {"Content-Type": content_type}

    def read(self):
        return self.body

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.closed = True


class OfflineCase(unittest.TestCase):
    def setUp(self):
        self.stack = contextlib.ExitStack()
        self.addCleanup(self.stack.close)
        for name in ("subprocess.run", "socket.socket", "socket.create_connection"):
            self.stack.enter_context(mock.patch(name, side_effect=tripwire))


class SlackTests(OfflineCase):
    def setUp(self):
        super().setUp()
        for name, value in (("_workspace", "epoch"), ("_xoxc", None), ("_xoxd", None), ("_token_workspace", None)):
            self.stack.enter_context(mock.patch.object(SLACK, name, value))

    def failure(self, callback, code=1):
        output = io.StringIO()
        with contextlib.redirect_stderr(output), self.assertRaises(SystemExit) as raised:
            callback()
        self.assertEqual(raised.exception.code, code)
        self.assertNotIn(MARKER, output.getvalue())
        return output.getvalue()

    def test_epoch_uses_only_broker(self):
        def broker(argv, **kw):
            self.assertEqual(argv[:2], ["op-automations", "read"])
            self.assertEqual(kw["timeout"], 30)
            return subprocess.CompletedProcess(argv, 0, MARKER, "")
        with mock.patch.object(SLACK.subprocess, "run", side_effect=broker) as run:
            self.assertEqual(SLACK._tokens(), (MARKER, MARKER))
            self.assertEqual(run.call_count, 2)

    def test_broker_failure_is_sanitized_and_partial_tokens_not_cached(self):
        good = subprocess.CompletedProcess([], 0, MARKER, "")
        bad = subprocess.CompletedProcess([], 1, MARKER, MARKER)
        with mock.patch.object(SLACK.subprocess, "run", side_effect=[good, bad]):
            self.failure(SLACK._tokens)
        self.assertIsNone(SLACK._xoxc)
        self.assertIsNone(SLACK._xoxd)

    def test_workspace_change_uses_personal_named_fields(self):
        with mock.patch.object(SLACK, "_op_read", return_value="epoch-token"):
            SLACK._tokens()
        SLACK._workspace = "trajectory"
        with mock.patch.object(SLACK, "_pass_field", side_effect=lambda entry, field: "personal-" + field) as read:
            self.assertEqual(SLACK._tokens(), ("personal-token", "personal-cookie"))
            self.assertEqual(read.call_args_list, [mock.call("trajectory/slack.com/trajectorylabs", "token"), mock.call("trajectory/slack.com/trajectorylabs", "cookie")])

    def test_invalid_methods_fail_before_credentials(self):
        with mock.patch.object(SLACK, "_tokens", side_effect=tripwire):
            for method in ["/users.info", "../auth.test", "auth.test?token=x", "auth.test#x", "https://evil.test", "auth.test\n", "auth%2etest", None]:
                with self.subTest(method=method):
                    self.failure(lambda: SLACK.call(method), 2)

    def test_invalid_api_origins_fail_before_credentials(self):
        with mock.patch.object(SLACK, "_tokens", side_effect=tripwire):
            for base in ["http://slack.com/api", "https://evil.test/api", "https://slack.com@evil.test/api", "https://slack.com/api/../api", "https://slack.com/api?x=1"]:
                with self.subTest(base=base), mock.patch.object(SLACK, "API", base):
                    self.failure(lambda: SLACK.call("auth.test"), 2)

    def test_success_preserves_form_and_closes_transport(self):
        response = Response(b'{"ok":true,"user_id":"U_FIXTURE"}')
        opener = mock.Mock()
        opener.open.return_value = response
        with mock.patch.object(SLACK, "_tokens", return_value=(MARKER, MARKER)), mock.patch.object(SLACK.urllib.request, "build_opener", return_value=opener) as build:
            self.assertEqual(SLACK.call("apps.manifest.create", manifest='{"x":1}', absent=None)["user_id"], "U_FIXTURE")
        self.assertIsInstance(build.call_args.args[0], SLACK._NoCredentialRedirects)
        request = opener.open.call_args.args[0]
        self.assertEqual(request.full_url, "https://slack.com/api/apps.manifest.create")
        self.assertEqual(request.data, b"manifest=%7B%22x%22%3A1%7D")
        self.assertEqual(opener.open.call_args.kwargs, {"timeout": 30})
        self.assertTrue(response.closed)

    def test_redirects_never_forward_credentials(self):
        req = urllib.request.Request("https://slack.com/api/auth.test", headers={"Authorization": MARKER})
        handler = SLACK._NoCredentialRedirects()
        for status in [301, 302, 303, 307, 308]:
            self.assertIsNone(handler.redirect_request(req, None, status, "redirect", {}, "https://evil.test/"))

    def test_http_error_does_not_read_response_body(self):
        payload = mock.Mock()
        payload.read.side_effect = AssertionError("must not read error body")
        error = urllib.error.HTTPError("https://slack.com/api/auth.test", 403, MARKER, {}, payload)
        opener = mock.Mock()
        opener.open.side_effect = error
        with mock.patch.object(SLACK, "_tokens", return_value=(MARKER, MARKER)), mock.patch.object(SLACK.urllib.request, "build_opener", return_value=opener):
            self.assertIn("HTTP 403", self.failure(lambda: SLACK.call("auth.test")))
        payload.read.assert_not_called()

    def test_transport_failure_is_sanitized_without_retry(self):
        for error in [urllib.error.URLError(MARKER), TimeoutError(MARKER)]:
            opener = mock.Mock()
            opener.open.side_effect = error
            with mock.patch.object(SLACK, "_tokens", return_value=(MARKER, MARKER)), mock.patch.object(SLACK.urllib.request, "build_opener", return_value=opener):
                self.failure(lambda: SLACK.call("auth.test"))
                self.assertEqual(opener.open.call_count, 1)

    def test_malformed_json_and_truthy_ok_are_not_success(self):
        for body in [b"not-json", b"[]", b"null", b'{"ok":"true"}', b'{"ok":1}', json.dumps({"ok": False, "error": MARKER}).encode()]:
            with self.subTest(body=body), mock.patch.object(SLACK, "_tokens", return_value=(MARKER, MARKER)), mock.patch.object(SLACK, "_open_authenticated", return_value=Response(body)):
                self.failure(lambda: SLACK.call("auth.test"))

    def test_missing_scope_remains_actionable(self):
        with mock.patch.object(SLACK, "_tokens", return_value=(MARKER, MARKER)), mock.patch.object(SLACK, "_open_authenticated", return_value=Response(b'{"ok":false,"error":"missing_scope"}')):
            self.assertIn("missing_scope", self.failure(lambda: SLACK.call("auth.test")))

    def test_file_origins_checked_before_credentials(self):
        with mock.patch.object(SLACK, "_tokens", side_effect=tripwire):
            for url in ["http://files.slack.com/x", "https://files.slack.com.evil.test/x", "https://files.slack.com:444/x", "https://user@files.slack.com/x", "https://files.slack.com/x#fragment", "https://files.slack.com\\@evil.test/x"]:
                with self.subTest(url=url):
                    self.failure(lambda: SLACK.cmd_file(argparse.Namespace(url=url, output="never-written")), 2)

    def test_file_uses_same_transport_and_sends_exact_bytes_to_sink(self):
        response = Response(b"offline bytes", "application/octet-stream")
        opener = mock.Mock()
        opener.open.return_value = response
        with mock.patch("builtins.open", mock.mock_open()) as sink, mock.patch.object(SLACK, "_tokens", return_value=(MARKER, MARKER)), mock.patch.object(SLACK.urllib.request, "build_opener", return_value=opener) as build, contextlib.redirect_stdout(io.StringIO()):
            SLACK.cmd_file(argparse.Namespace(url="https://files.slack.com/files-pri/offline", output="/fixture/output.bin"))
        sink.assert_called_once_with("/fixture/output.bin", "wb")
        sink().write.assert_called_once_with(b"offline bytes")
        self.assertTrue(response.closed)
        self.assertIsInstance(build.call_args.args[0], SLACK._NoCredentialRedirects)

    def test_subprocess_decoding_errors_do_not_expose_bytes(self):
        error = UnicodeDecodeError("utf-8", MARKER.encode(), 0, 1, "fixture")
        with mock.patch.object(SLACK.subprocess, "run", side_effect=error):
            self.failure(SLACK._tokens)

    def test_http_protocol_and_partial_read_errors_on_both_paths(self):
        class BrokenResponse(Response):
            def read(self):
                raise http.client.IncompleteRead(MARKER.encode())
        with mock.patch("builtins.open", side_effect=tripwire) as sink:
            callbacks = [lambda: SLACK.call("auth.test"), lambda: SLACK.cmd_file(argparse.Namespace(url="https://files.slack.com/private/file", output="/fixture/must-not-exist"))]
            for callback in callbacks:
                for error in [http.client.BadStatusLine(MARKER), http.client.IncompleteRead(MARKER.encode()), UnicodeDecodeError("utf-8", MARKER.encode(), 0, 1, "fixture")]:
                    opener = mock.Mock()
                    opener.open.side_effect = error
                    with mock.patch.object(SLACK, "_tokens", return_value=(MARKER, MARKER)), mock.patch.object(SLACK.urllib.request, "build_opener", return_value=opener):
                        self.failure(callback)
                with mock.patch.object(SLACK, "_tokens", return_value=(MARKER, MARKER)), mock.patch.object(SLACK, "_open_authenticated", return_value=BrokenResponse(b"")):
                    self.failure(callback)
            sink.assert_not_called()


class NotifierTests(OfflineCase):
    failure = SlackTests.failure

    def test_fixed_broker_and_bot_authorship_ignore_browser_and_environment(self):
        body = {"ok": True, "channel": "D123", "ts": "1.2", "message": {"text": "Exact\ntext"}}
        opener = mock.Mock()
        response = Response(json.dumps(body).encode())
        opener.open.return_value = response
        with mock.patch.dict(SLACK.os.environ, {"SLACK_BOT_TOKEN": "ambient", "SLACK_WORKSPACE": "trajectory"}), mock.patch.object(SLACK, "_tokens", side_effect=tripwire), mock.patch.object(SLACK.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, MARKER, "")) as broker, mock.patch.object(SLACK.urllib.request, "build_opener", return_value=opener) as build:
            out = SLACK.call_notifier("chat.postMessage", channel="U123", text="Exact\ntext", mrkdwn=False, parse="none", link_names=False)
        self.assertEqual(out, body)
        broker.assert_called_once_with(["op-automations", "read", "op://Automations/Slack - Epoch Notifier/credential"], check=False, capture_output=True, text=True, timeout=30)
        request = opener.open.call_args.args[0]
        self.assertEqual(request.get_header("Authorization"), f"Bearer {MARKER}")
        self.assertIsNone(request.get_header("Cookie"))
        self.assertEqual(SLACK.urllib.parse.parse_qs(request.data.decode()), {"channel": ["U123"], "text": ["Exact\ntext"], "mrkdwn": ["false"], "parse": ["none"], "link_names": ["false"]})
        self.assertIsInstance(build.call_args.args[0], SLACK._NoCredentialRedirects)
        self.assertEqual(opener.open.call_count, 1)
        self.assertEqual(opener.open.call_args.kwargs, {"timeout": 30})
        self.assertTrue(response.closed)

    def test_auth_and_enterprise_user_are_supported(self):
        with mock.patch.object(SLACK, "_op_read", return_value=MARKER), mock.patch.object(SLACK, "_open_authenticated", return_value=Response(b'{"ok":true,"bot_id":"B123"}')):
            self.assertEqual(SLACK.call_notifier("auth.test")["bot_id"], "B123")
            SLACK.call_notifier("chat.postMessage", channel="W123", text="Authorized")

    def test_disallowed_calls_fail_before_broker(self):
        calls = [
            ("conversations.open", {}), ("chat.update", {}), (None, {}),
            ("auth.test", {"token": MARKER}),
        ]
        for channel in [None, "C123", "D123", "#general", "U123,U456", "U123\n", "U", "u123", "U12 3"]:
            calls.append(("chat.postMessage", {"channel": channel, "text": "hello"}))
        for field in ["as_user", "username", "icon_url", "icon_emoji", "token", "blocks", "attachments"]:
            calls.append(("chat.postMessage", {"channel": "U123", "text": "hello", field: MARKER}))
        with mock.patch.object(SLACK, "_op_read", side_effect=tripwire):
            for method, params in calls:
                with self.subTest(method=method, params=params):
                    self.failure(lambda: SLACK.call_notifier(method, **params), 2)
            for base in ["https://evil.test/api", "http://slack.com/api", "https://slack.com/api?x=1"]:
                with mock.patch.object(SLACK, "API", base):
                    self.failure(lambda: SLACK.call_notifier("auth.test"), 2)

    def test_broker_failure_never_falls_back_or_sends(self):
        with mock.patch.object(SLACK.subprocess, "run", return_value=subprocess.CompletedProcess([], 1, MARKER, MARKER)), mock.patch.object(SLACK, "_tokens", side_effect=tripwire), mock.patch.object(SLACK, "_open_authenticated", side_effect=tripwire):
            self.failure(lambda: SLACK.call_notifier("auth.test"))

    def test_uncertain_delivery_has_one_attempt_and_safe_diagnostics(self):
        for error in [TimeoutError(MARKER), urllib.error.URLError(MARKER), http.client.IncompleteRead(MARKER.encode())]:
            opener = mock.Mock()
            opener.open.side_effect = error
            with mock.patch.object(SLACK, "_op_read", return_value=MARKER), mock.patch.object(SLACK.urllib.request, "build_opener", return_value=opener):
                self.failure(lambda: SLACK.call_notifier("chat.postMessage", channel="U123", text="Exact"))
            self.assertEqual(opener.open.call_count, 1)

    def test_error_responses_are_sanitized(self):
        for body in [MARKER.encode(), b'[]', b'{"ok":1}', json.dumps({"ok": False, "error": MARKER}).encode()]:
            with mock.patch.object(SLACK, "_op_read", return_value=MARKER), mock.patch.object(SLACK, "_open_authenticated", return_value=Response(body)):
                self.failure(lambda: SLACK.call_notifier("auth.test"))


class FakeBroker:
    def __init__(self, existing=False):
        self.calls, self.existing, self.clipboard = [], existing, MARKER
        self.failure, self.duplicate, self.readback_value = None, False, None
        self.concurrent_edit, self.duplicate_after_write, self.get_count = False, False, 0
        self.write_id, self.stored = ITEM, None
        self.template = {"category": "API_CREDENTIAL", "fields": [{"id": "credential", "label": "credential", "type": "CONCEALED", "value": "old"}, {"id": "other", "label": "other", "type": "STRING", "value": "preserve"}], "tags": ["owned-fixture"], "urls": [{"href": "old.example", "primary": True}]}
        if existing:
            self.template.update(id=ITEM, title="Slack - Fixture", vault={"id": "v" * 26, "name": "Automations"})

    def __call__(self, argv, **kw):
        self.calls.append((list(argv), kw))
        if any(MARKER in str(arg) for arg in argv):
            raise AssertionError("secret escaped into argv")
        if self.failure and self.failure in argv:
            return subprocess.CompletedProcess(argv, 1, MARKER, MARKER)
        if argv == ["pbpaste"]:
            return subprocess.CompletedProcess(argv, 0, self.clipboard, "")
        if argv[:3] == ["op-desktop", "item", "list"]:
            result = [{"id": ITEM, "title": "Slack - Fixture"}] if self.existing or self.stored else []
            if self.duplicate or (self.stored and self.duplicate_after_write):
                result *= 2
        elif argv[:4] == ["op-desktop", "item", "template", "get"]:
            result = copy.deepcopy(self.template)
        elif argv[:3] == ["op-desktop", "item", "get"]:
            self.get_count += 1
            if argv[3] != ITEM:
                raise AssertionError("get did not use exact ID")
            result = copy.deepcopy(self.stored or self.template)
            if self.concurrent_edit and self.get_count == 2:
                result["fields"][1]["value"] = "another editor changed this"
            if self.stored and self.readback_value is not None:
                result["fields"][0]["value"] = self.readback_value
        elif argv[:3] in (["op-desktop", "item", "create"], ["op-desktop", "item", "edit"]):
            if argv[3] != (ITEM if self.existing else "-"):
                raise AssertionError("wrong stdin operation shape")
            result = json.loads(kw["input"])
            result.update(id=self.write_id, title="Slack - Fixture", vault={"id": "v" * 26, "name": "Automations"})
            self.stored = copy.deepcopy(result)
        else:
            raise AssertionError("unexpected fake broker operation")
        return subprocess.CompletedProcess(argv, 0, json.dumps(result), "")


class ClipboardTests(OfflineCase):
    def invoke(self, broker, *extra):
        output, error = io.StringIO(), io.StringIO()
        with mock.patch.object(STORE.subprocess, "run", side_effect=broker), contextlib.redirect_stdout(output), contextlib.redirect_stderr(error):
            result = STORE.main(["--vault", "Automations", "--title", "Slack - Fixture", "--prefix", "offline-", *extra])
        self.assertNotIn(MARKER, output.getvalue() + error.getvalue())
        return result, output.getvalue(), error.getvalue()

    def test_create_uses_stdin_and_exact_readback(self):
        broker = FakeBroker()
        result, output, _ = self.invoke(broker)
        self.assertEqual(result, 0)
        self.assertEqual(output, "stored op://Automations/Slack - Fixture/credential\n")
        self.assertEqual(broker.stored["fields"][0]["value"], MARKER)
        writes = [(a, k) for a, k in broker.calls if "create" in a]
        self.assertEqual(len(writes), 1)
        self.assertIn(MARKER, writes[0][1]["input"])
        self.assertEqual(broker.calls[-2][0][3], ITEM)

    def test_force_preserves_metadata_and_uses_exact_id(self):
        broker = FakeBroker(True)
        self.assertEqual(self.invoke(broker, "--force", "--note", "new note", "--url", "new.example")[0], 0)
        self.assertEqual(broker.stored["fields"][1], broker.template["fields"][1])
        self.assertEqual(broker.stored["tags"], broker.template["tags"])
        self.assertEqual(broker.stored["vault"], broker.template["vault"])
        self.assertEqual(broker.stored["fields"][-1]["value"], "new note")
        self.assertEqual(broker.stored["urls"], [{"href": "old.example", "primary": False}, {"href": "new.example", "primary": True}])
        edit = next(a for a, _ in broker.calls if "edit" in a)
        self.assertEqual(edit[3], ITEM)
        self.assertNotIn("-", edit)

    def test_force_required_before_clipboard_intake(self):
        broker = FakeBroker(True)
        self.assertEqual(self.invoke(broker)[0], 3)
        self.assertEqual(len(broker.calls), 1)

    def test_lookup_error_is_not_absence(self):
        broker = FakeBroker()
        broker.failure = "list"
        self.assertEqual(self.invoke(broker)[0], 3)
        self.assertEqual(len(broker.calls), 1)

    def test_duplicate_titles_are_not_guessed(self):
        broker = FakeBroker(True)
        broker.duplicate = True
        self.assertEqual(self.invoke(broker, "--force")[0], 3)
        self.assertEqual(len(broker.calls), 1)

    def test_readback_mismatch_is_not_success(self):
        broker = FakeBroker()
        broker.readback_value = "wrong-value"
        result, output, _ = self.invoke(broker)
        self.assertEqual(result, 3)
        self.assertEqual(output, "")

    def test_invalid_arguments_touch_neither_clipboard_nor_broker(self):
        for extra in [("--min", "-1"), ("--max", "2"), ("--field", "x/y"), ("--title", "bad/title"), ("--vault", "bad\nvault"), ("--prefix", "bad\n")]:
            broker = FakeBroker()
            with self.subTest(extra=extra):
                self.assertEqual(self.invoke(broker, *extra)[0], 2)
                self.assertEqual(broker.calls, [])

    def test_bad_clipboard_shape_never_writes(self):
        for value in ["", "short", "wrong-prefix-long-enough", MARKER + "\n", MARKER + "\r", MARKER + "\t", "offline-" + "a" * 251]:
            broker = FakeBroker()
            broker.clipboard = value
            with self.subTest(shape=repr(value[-3:])):
                self.assertEqual(self.invoke(broker)[0], 1)
                self.assertIsNone(broker.stored)

    def test_ambiguous_fields_and_attachments_fail_before_intake(self):
        for mutate in [lambda t: t["fields"].append(copy.deepcopy(t["fields"][0])), lambda t: t.update(files=[{"id": "attachment"}]), lambda t: t["fields"][0].update(section={"id": "nested"})]:
            broker = FakeBroker(True)
            mutate(broker.template)
            self.assertEqual(self.invoke(broker, "--force")[0], 3)
            self.assertFalse(any(a == ["pbpaste"] for a, _ in broker.calls))

    def test_edit_identity_cannot_change(self):
        broker = FakeBroker(True)
        broker.write_id = "b" * 26
        self.assertEqual(self.invoke(broker, "--force")[0], 3)

    def test_write_failure_is_sanitized(self):
        broker = FakeBroker()
        broker.failure = "create"
        result, output, _ = self.invoke(broker)
        self.assertEqual(result, 3)
        self.assertEqual(output, "")

    def test_help_never_reads_clipboard(self):
        with contextlib.redirect_stdout(io.StringIO()), self.assertRaises(SystemExit) as raised:
            STORE.main(["--help"])
        self.assertEqual(raised.exception.code, 0)

    def test_parser_errors_do_not_echo_bad_arguments(self):
        for extra in [("--unknown", MARKER), ("--min", MARKER), ("--field",)]:
            broker = FakeBroker()
            self.assertEqual(self.invoke(broker, *extra)[0], 2)
            self.assertEqual(broker.calls, [])

    def test_concurrent_item_edit_is_not_overwritten(self):
        broker = FakeBroker(True)
        broker.concurrent_edit = True
        result, output, _ = self.invoke(broker, "--force")
        self.assertEqual(result, 3)
        self.assertIsNone(broker.stored)
        self.assertEqual(output, "")

    def test_concurrent_duplicate_after_write_is_not_reported_as_unique(self):
        broker = FakeBroker()
        broker.duplicate_after_write = True
        result, output, error = self.invoke(broker)
        self.assertEqual(result, 3)
        self.assertIsNotNone(broker.stored)
        self.assertEqual(output, "")
        self.assertNotIn("no item was changed", error)

    def test_wrong_vault_is_rejected_before_clipboard_intake(self):
        broker = FakeBroker(True)
        broker.template["vault"] = {"id": "w" * 26, "name": "Wrong vault"}
        self.assertEqual(self.invoke(broker, "--force")[0], 3)
        self.assertFalse(any(a == ["pbpaste"] for a, _ in broker.calls))

    def test_unicode_readback_and_broker_decoding_fail_safely(self):
        broker = FakeBroker()
        broker.readback_value = "\ud800"
        self.assertEqual(self.invoke(broker)[0], 3)
        error = UnicodeDecodeError("utf-8", MARKER.encode(), 0, 1, "fixture")
        self.assertEqual(self.invoke(mock.Mock(side_effect=error))[0], 3)

    def test_one_line_password_spaces_are_preserved(self):
        broker = FakeBroker()
        broker.clipboard = "offline-phrase with spaces"
        self.assertEqual(self.invoke(broker)[0], 0)
        self.assertEqual(broker.stored["fields"][0]["value"], broker.clipboard)


if __name__ == "__main__":
    unittest.main()
