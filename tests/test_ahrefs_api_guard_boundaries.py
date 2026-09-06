"""Offline guard regressions: replace credential/process/socket IO before loading."""

import contextlib
import email.message
import http.client
import importlib.machinery
import importlib.util
import io
import json
import os
import runpy
import subprocess
import sys
import unittest
import urllib.error
import urllib.request
import urllib.response
from pathlib import Path
from unittest import mock


SCRIPT = Path(__file__).resolve().parents[1] / "claude/bin/ahrefs-api-guard"
TOKEN = "synthetic-ahrefs-secret-never-live"
BASE = "https://api.ahrefs.com/v3"
USAGE = "/subscription-info/limits-and-usage"
PAID = "/site-explorer/top-pages"
REAL_OPENER_OPEN = urllib.request.OpenerDirector.open


def load_module():
    loader = importlib.machinery.SourceFileLoader("ahrefs_guard_boundaries", str(SCRIPT))
    spec = importlib.util.spec_from_loader("ahrefs_guard_boundaries", loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def usage_response(used=100_000, limit=400_000, key_used=10, key_limit=None):
    return {"limits_and_usage": {
        "units_usage_workspace": used,
        "units_limit_workspace": limit,
        "units_usage_api_key": key_used,
        "units_limit_api_key": key_limit,
        "usage_reset_date": "2026-07-07",
    }}


class FakeResponse:
    def __init__(self, payload=None, *, raw=None):
        self.raw = json.dumps(payload).encode() if raw is None else raw

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False

    def read(self):
        return self.raw


class GuardBoundaryTest(unittest.TestCase):
    def setUp(self):
        self.stack = contextlib.ExitStack()
        self.addCleanup(self.stack.close)
        for target in ("socket.socket", "socket.create_connection", "subprocess.run",
                       "urllib.request.urlopen", "urllib.request.OpenerDirector.open"):
            self.stack.enter_context(mock.patch(
                target, side_effect=AssertionError("unexpected external IO")))
        self.mod = load_module()

    def invoke(self, argv, responses=()):
        """Run the actual __main__ command path with synthetic IO only."""
        events, requests = [], []
        replies = iter(responses)

        def broker(command, **kwargs):
            events.append("credential")
            self.assertIn(command[0], ("op-automations", "op-desktop"))
            self.assertEqual(kwargs["stdout"], subprocess.PIPE)
            self.assertEqual(kwargs["stderr"], subprocess.PIPE)
            return subprocess.CompletedProcess(command, 0, TOKEN + "\n", "")

        def send(request, timeout):
            events.append(request.full_url.split("?", 1)[0])
            requests.append(request)
            self.assertEqual(timeout, 60)
            self.assertEqual(request.get_header("Authorization"), "Bearer " + TOKEN)
            result = next(replies)
            if isinstance(result, Exception):
                raise result
            return result

        out, err = io.StringIO(), io.StringIO()
        with (mock.patch.dict(os.environ, {}, clear=True),
              mock.patch.object(sys, "argv", [str(SCRIPT), *argv]),
              mock.patch("subprocess.run", side_effect=broker),
              mock.patch("urllib.request.urlopen", side_effect=send),
              mock.patch("urllib.request.OpenerDirector.open", side_effect=send),
              contextlib.redirect_stdout(out), contextlib.redirect_stderr(err)):
            with self.assertRaises(SystemExit) as raised:
                runpy.run_path(str(SCRIPT), run_name="__main__")
        return raised.exception.code, out.getvalue(), err.getvalue(), events, requests

    def paid_args(self, *extra):
        return ["request", PAID, "--estimated-cost", "100", *extra]

    def test_command_preflight_precedes_paid_request(self):
        code, out, err, events, _ = self.invoke(self.paid_args(), [
            FakeResponse(usage_response()), FakeResponse({"pages": []})])
        self.assertEqual(code, 0)
        self.assertEqual(events, ["credential", BASE + USAGE, BASE + PAID])
        self.assertEqual(json.loads(out), {"pages": []})
        self.assertIn("estimated cost 100", err)

    def test_usage_command_makes_only_free_request(self):
        code, out, _, events, _ = self.invoke(["usage"], [FakeResponse(usage_response())])
        self.assertEqual(code, 0)
        self.assertEqual(events, ["credential", BASE + USAGE])
        self.assertEqual(json.loads(out)["units_remaining_workspace"], 300_000)

    def test_free_request_does_not_require_estimate_or_second_probe(self):
        code, _, _, events, _ = self.invoke(["request", USAGE], [FakeResponse(usage_response())])
        self.assertEqual(code, 0)
        self.assertEqual(events, ["credential", BASE + USAGE])

    def test_invalid_command_inputs_stop_before_credentials_or_network(self):
        cases = [
            ["request", PAID],
            ["request", PAID, "--estimated-cost", "0"],
            ["request", PAID, "--estimated-cost", "-1"],
            self.paid_args("--min-remaining", "-1"),
            self.paid_args("--min-remaining", TOKEN),
            ["request", PAID, "--estimated-cost", TOKEN],
            self.paid_args("--param", TOKEN),
            self.paid_args("--param", "=" + TOKEN),
            self.paid_args("--json-body", TOKEN),
            self.paid_args("--json-body", ""),
            self.paid_args("--json-body", '{"n": NaN}'),
            self.paid_args("--param", "output=csv"),
            self.paid_args("--param", "output=xml"),
            self.paid_args("--param", "target=\udcff"),
            self.paid_args("--method", "DELETE"),
            self.paid_args("--method", TOKEN),
            ["request", USAGE, "--method", "POST"],
            ["request", USAGE, "--json-body", "{}"],
            self.paid_args("--unknown", TOKEN),
        ]
        for argv in cases:
            with self.subTest(argv=argv):
                code, out, err, events, _ = self.invoke(argv)
                self.assertEqual((code, out, events), (2, "", []))
                self.assertNotIn(TOKEN, err)
                self.assertNotIn("Traceback", err)

    def test_bad_destination_stops_before_credentials_or_network(self):
        bases = ["https://example.invalid/v3", "http://api.ahrefs.com/v3",
                 "https://api.ahrefs.com:443/v3", "https://api.ahrefs.com/v2",
                 "https://api.ahrefs.com.evil.invalid/v3",
                 "https://user@api.ahrefs.com/v3", BASE + "?q=" + TOKEN,
                 BASE + "#" + TOKEN, "\n" + BASE]
        paths = ["https://example.invalid/" + TOKEN, "//example.invalid/path",
                 "../admin", "site-explorer/../../admin", PAID + "?q=" + TOKEN,
                 PAID + "#" + TOKEN, "/site-explorer/%2e%2e/admin",
                 "/site-explorer//top-pages", "/site-explorer\\top-pages",
                 PAID + "\n", "", "/"]
        cases = [["--base-url", base, "usage"] for base in bases]
        cases += [["request", path, "--estimated-cost", "100"] for path in paths]
        for argv in cases:
            with self.subTest(argv=argv):
                code, out, err, events, _ = self.invoke(argv)
                self.assertEqual((code, out, events), (2, "", []))
                self.assertNotIn(TOKEN, err)
                self.assertNotIn("example.invalid", err)

    def test_invalid_probe_schema_blocks_paid_request(self):
        cases = [None, [], {}, {"limits_and_usage": None}, {"limits_and_usage": []}]
        for field in ("units_usage_workspace", "units_limit_workspace",
                      "units_usage_api_key", "units_limit_api_key"):
            payload = usage_response()
            del payload["limits_and_usage"][field]
            cases.append(payload)
            invalid = [True, False, -1, 1.5, "100", "", float("nan"), float("inf")]
            if field != "units_limit_api_key":
                invalid.append(None)
            if field == "units_limit_workspace":
                invalid.append(0)
            for value in invalid:
                payload = usage_response()
                payload["limits_and_usage"][field] = value
                cases.append(payload)
        for payload in cases:
            with self.subTest(payload=payload):
                code, out, err, events, _ = self.invoke(
                    self.paid_args(), [FakeResponse(payload)])
                self.assertEqual((code, out), (2, ""))
                self.assertEqual(events, ["credential", BASE + USAGE])
                self.assertNotIn("guard passed", err)
                self.assertNotIn("Traceback", err)

    def test_workspace_and_key_budget_bounds(self):
        cases = [
            (usage_response(199_900), 0),
            (usage_response(199_901), 2),
            (usage_response(400_001), 2),
            (usage_response(key_used=900, key_limit=1_000), 0),
            (usage_response(key_used=901, key_limit=1_000), 2),
            (usage_response(key_used=1_000, key_limit=1_000), 2),
            (usage_response(key_used=0, key_limit=0), 2),
            (usage_response(key_used=1_001, key_limit=1_000), 2),
            (usage_response(key_used=100_000, key_limit=None), 0),
        ]
        for payload, expected in cases:
            with self.subTest(payload=payload):
                code, _, _, events, _ = self.invoke(self.paid_args(), [
                    FakeResponse(payload), FakeResponse({"pages": []})])
                self.assertEqual(code, expected)
                self.assertEqual(len(events), 3 if expected == 0 else 2)

    def test_zero_reserve_is_valid_when_explicitly_chosen(self):
        code, _, _, _, _ = self.invoke(self.paid_args("--min-remaining", "0"), [
            FakeResponse(usage_response(399_900)), FakeResponse({"pages": []})])
        self.assertEqual(code, 0)

    def test_json_post_body_and_query_are_encoded(self):
        code, _, _, _, requests = self.invoke(self.paid_args(
            "--method", "POST", "--json-body", '{"targets": ["a&b"]}',
            "--param", "target=a&b", "--param", "output=json"), [
            FakeResponse(usage_response()), FakeResponse({"result": []})])
        self.assertEqual(code, 0)
        self.assertEqual(requests[1].get_method(), "POST")
        self.assertEqual(requests[1].get_header("Content-type"), "application/json")
        self.assertEqual(json.loads(requests[1].data), {"targets": ["a&b"]})
        self.assertIn("target=a%26b", requests[1].full_url)

    def test_probe_errors_do_not_leak_or_retry(self):
        failures = [
            urllib.error.HTTPError(BASE + "?q=" + TOKEN, code, TOKEN, {},
                                   io.BytesIO(TOKEN.encode()))
            for code in (301, 302, 303, 307, 308, 401, 403, 429, 500)
        ]
        failures += [FakeResponse(raw=TOKEN.encode()), FakeResponse(raw=b"\xff"),
                     FakeResponse(raw=b""), urllib.error.URLError(TOKEN),
                     TimeoutError(TOKEN), OSError(TOKEN)]
        for failure in failures:
            with self.subTest(failure=type(failure).__name__):
                code, out, err, events, _ = self.invoke(self.paid_args(), [failure])
                self.assertEqual((code, out), (2, ""))
                self.assertEqual(events, ["credential", BASE + USAGE])
                self.assertNotIn(TOKEN, err)
                self.assertNotIn(BASE, err)
                self.assertNotIn("Traceback", err)

    def test_paid_response_errors_do_not_leak_or_retry(self):
        for failure in (FakeResponse(raw=TOKEN.encode()),
                        FakeResponse(raw=b'{"invalid": NaN}'),
                        urllib.error.URLError(BASE + "?" + TOKEN),
                        http.client.BadStatusLine(TOKEN),
                        http.client.IncompleteRead(TOKEN.encode())):
            with self.subTest(failure=type(failure).__name__):
                code, out, err, events, _ = self.invoke(self.paid_args(), [
                    FakeResponse(usage_response()), failure])
                self.assertEqual((code, out), (2, ""))
                self.assertEqual(len(events), 3)
                self.assertNotIn(TOKEN, err)
                self.assertNotIn(BASE, err)

    def test_successful_echoed_token_is_redacted(self):
        code, out, err, _, _ = self.invoke(self.paid_args(), [
            FakeResponse(usage_response()),
            FakeResponse({TOKEN: ["prefix " + TOKEN, {"nested": TOKEN}]})])
        self.assertEqual(code, 0)
        self.assertNotIn(TOKEN, out + err)
        self.assertIn("REDACTED", out)

    def test_secret_broker_routing_is_preserved(self):
        for vault, broker in (("Automations", "op-automations"), ("Employee", "op-desktop")):
            ref = f"op://{vault}/item/credential"
            self.assertEqual(self.mod.op_reader_for(ref), [broker, "read", ref])

    def test_injected_request_transport_still_supported(self):
        send = mock.Mock(return_value=FakeResponse({"pages": []}))
        self.assertEqual(self.mod.request_json("GET", BASE + PAID, TOKEN, urlopen=send),
                         {"pages": []})
        send.assert_called_once()

    def test_direct_request_boundary_rejects_untrusted_urls_without_transport(self):
        for url in ("http://api.ahrefs.com/v3/x", "https://other.invalid/v3/x",
                    "https://api.ahrefs.com:443/v3/x", "https://[",
                    BASE + "/../x", BASE + "/%2e%2e/x", "\n" + BASE + PAID):
            with self.subTest(url=url):
                send = mock.Mock(side_effect=AssertionError("unexpected transport"))
                with self.assertRaises(self.mod.GuardError):
                    self.mod.request_json("GET", url, TOKEN, urlopen=send)
                send.assert_not_called()

    def test_budget_function_rejects_invalid_numeric_inputs(self):
        usage = self.mod.Usage(100_000, 400_000, 300_000, 10, None, None)
        for estimate in (None, 0, -1, True, False, 1.5, float("nan"), float("inf")):
            with self.subTest(estimate=estimate), self.assertRaises(self.mod.GuardError):
                self.mod.assert_budget_allows_call(usage, min_remaining=0, estimated_cost=estimate)
        for reserve in (-1, None, True, False, 1.5, float("nan"), float("inf")):
            with self.subTest(reserve=reserve), self.assertRaises(self.mod.GuardError):
                self.mod.assert_budget_allows_call(usage, min_remaining=reserve, estimated_cost=100)

    def test_actual_urllib_redirect_chain_refuses_before_second_request(self):
        # Real urllib response handling; fake HTTPS replaces the wire. Socket
        # tripwires stay active, including for a downgrade to HTTP.
        original_build = urllib.request.build_opener
        for status in (301, 302, 303, 307, 308):
            for destination in ("https://other.invalid/" + TOKEN,
                                "http://api.ahrefs.com/" + TOKEN, BASE + "/redirected"):
                with self.subTest(status=status, destination=destination):
                    seen = []

                    class Wire(urllib.request.HTTPSHandler):
                        def https_open(wire_self, request):
                            seen.append(request.full_url)
                            headers = email.message.Message()
                            headers["Location"] = destination
                            response = urllib.response.addinfourl(
                                io.BytesIO(b""), headers, request.full_url, status)
                            response.msg = "redirect"
                            if len(seen) > 1:
                                raise AssertionError("redirect forwarded credential")
                            return response

                    def build(*handlers):
                        return original_build(*handlers, Wire())

                    with (mock.patch("urllib.request.build_opener", side_effect=build),
                          mock.patch("urllib.request.OpenerDirector.open", REAL_OPENER_OPEN)):
                        send = lambda request, timeout: build().open(request, timeout=timeout)
                        with mock.patch("urllib.request.urlopen", side_effect=send):
                            module = load_module()
                        with self.assertRaises(module.GuardError) as raised:
                            module.request_json("GET", BASE + PAID, TOKEN)
                    self.assertEqual(seen, [BASE + PAID])
                    self.assertNotIn(TOKEN, str(raised.exception))
                    self.assertNotIn(destination, str(raised.exception))


if __name__ == "__main__":
    unittest.main()
