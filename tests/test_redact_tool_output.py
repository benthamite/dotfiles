"""redact-tool-output: credentials in Bash and MCP results never reach the model.

Synthetic values follow the shapes of the 2026-09-27 leaks (a Cloudflare user
token in a Chrome `find` result, a Mullvad account number inside a 1Password
item title, a Claude OAuth token, a Substack session cookie). They are built at
runtime so the file itself holds no secret-shaped literal.
"""
import importlib.util
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

HOOK = Path(__file__).resolve().parents[1] / "claude/hooks/redact-tool-output.py"
spec = importlib.util.spec_from_file_location("redact_tool_output", HOOK)
M = importlib.util.module_from_spec(spec)
spec.loader.exec_module(M)

CLOUDFLARE = "cf" + "ut_" + "Ab3" * 15
MULLVAD = "5" + "1234567" * 2 + "8"  # 16 digits
CLAUDE = "sk-" + "ant-" + "oat01-" + "Zx9_" * 20
SUBSTACK = "s%" + "3A" + "Qw8." * 12
# Public MusicBrainz recording: Carlos Gardel, El barbijo.
MBID = '27311990-9142-4478-8766-f0fdea1caa40'
# A URL credential with no known shape, like the 2026-11-15 Slack `frt`.
OPAQUE = "Qw8Zx" * 6
SLACK_SESSION = "xox" + "c-" + "1234-" * 6 + "ab"
# The websocket URL emacs-slack opens (slack-websocket.el, slack-ws-open), as
# websocket.el names its process, with the `frt` a reconnect URL adds.
SLACK_WS = ("websocket to wss://wss-primary.slack.com/?token=" + SLACK_SESSION
            + "&sync_desync=1&slack_client=desktop&start_args=%3Fagent%3Dclient%26org_wide_aware%3Dtrue"
            + "%26agent_version%3D1730299661&no_query_on_subscribe=1&flannel=3&lazy_channels=1"
            + "&gateway_server=T0SYNTH-4&batch_presence_aware=1&frt=" + OPAQUE + "&dp=1")
URL_MASK = "[URL_CREDENTIAL_REDACTED]"


def bash(stdout, stderr=""):
    return {"tool_name": "Bash", "tool_response": {"stdout": stdout, "stderr": stderr, "interrupted": False}}


def mcp(text):
    return {"tool_name": "mcp__claude-in-chrome__find", "tool_response": [{"type": "text", "text": text}]}


def codex_hook():
    codex_spec = importlib.util.spec_from_file_location(
        "codex_redact_tool_output", HOOK.parents[2] / "codex/hooks/redact-tool-output.py")
    codex = importlib.util.module_from_spec(codex_spec)
    codex_spec.loader.exec_module(codex)
    return codex


class RedactToolOutputTest(unittest.TestCase):
    def setUp(self):
        import secret_index
        # Synthetic indexes do not need account/repository/git discovery.
        discovery = patch.object(secret_index, "public_strings", return_value=set())
        discovery.start()
        self.addCleanup(discovery.stop)

    def test_todays_leak_shapes_are_masked_in_bash_output(self):
        out = M.replacement(bash(f"mullvad.net/{MULLVAD} | Core | LOGIN\n{CLAUDE}\n", stderr=SUBSTACK))
        masked = out["updatedToolOutput"]
        text = masked["stdout"] + masked["stderr"]
        for secret in (MULLVAD, CLAUDE, SUBSTACK):
            self.assertNotIn(secret, text)
        self.assertIn("mullvad.net/[16_DIGIT_NUMBER_REDACTED] | Core | LOGIN", masked["stdout"])
        self.assertFalse(masked["interrupted"])  # the rest of the result is kept

    def test_chrome_find_echo_is_masked(self):
        out = M.replacement(mcp(f'ref_314 contains the token "{CLOUDFLARE}"'))
        block = out["updatedMCPToolOutput"][0]
        self.assertNotIn(CLOUDFLARE, block["text"])
        self.assertIn("[CLOUDFLARE_TOKEN_REDACTED]", block["text"])

    def test_clean_output_is_left_alone(self):
        self.assertIsNone(M.replacement(bash("32 passed in 1.1s\n")))
        self.assertIsNone(M.replacement(mcp('[{"deviceId":"b92bfc41-8f89-4a0a-9a61-8cec63c6ac9f"}]')))
        # An encoded URL contains "s%3A" inside "https%3A"; it is not a session cookie.
        self.assertIsNone(M.replacement(bash("redirect=https%3A%2F%2Fhingedaily.substack.com%2Fpublish%2Fsettings\n")))
        # Longer digit runs (tweet ids, timestamps) are not 16-digit numbers.
        self.assertIsNone(M.replacement(bash("x.com/a/status/1790549176458123456 at 1790549176458\n")))

    def test_slack_websocket_url_parameters_are_masked_in_both_runtimes(self):
        # 2026-11-15: an emacsclient read of process names printed this URL;
        # `token` was masked by its xoxc shape, `frt` (no known shape) was not.
        event = bash(f'("{SLACK_WS}" "*slack-log*")\n')
        claude = M.replacement(event)["updatedToolOutput"]["stdout"]
        codex = codex_hook().decision(event)["reason"]
        for output in (claude, codex):
            self.assertNotIn(OPAQUE, output)
            self.assertNotIn(SLACK_SESSION, output)
            self.assertIn("&frt=" + URL_MASK + "&dp=1", output)
            self.assertIn("?token=" + URL_MASK + "&sync_desync=1", output)
            # The connection's ordinary parameters stay readable.
            for kept in ("slack_client=desktop", "start_args=%3Fagent%3Dclient%26org_wide_aware%3Dtrue%26agent_version",
                         "flannel=3", "gateway_server=T0SYNTH-4", "batch_presence_aware=1"):
                self.assertIn(kept, output)

    def test_credential_url_parameters_are_masked(self):
        totp_seed = "JBSW" + "Y3DP" * 4
        for name in ("token", "access_token", "refresh_token", "id_token", "auth_token", "session_token",
                     "oauth_token", "api_token", "api_key", "apikey", "API-KEY", "client_secret", "secret",
                     "password", "passwd", "sig", "signature", "X-Amz-Signature", "X-Amz-Security-Token",
                     "X-Goog-Signature", "oauth_signature", "ticket", "frt", "jwt"):
            output = M.replacement(bash(f"https://h.example/p?a=1&{name}={OPAQUE}&b=2\n"))["updatedToolOutput"]["stdout"]
            self.assertEqual(output, f"https://h.example/p?a=1&{name}={URL_MASK}&b=2\n", name)
        cases = {
            # Socket Mode connection URL (apps.connections.open).
            "wss://wss-primary.slack.com/link/?ticket=" + OPAQUE + "&app_id=A0SYNTH":
                "wss://wss-primary.slack.com/link/?ticket=" + URL_MASK + "&app_id=A0SYNTH",
            "otpauth://totp/Example:me?secret=" + totp_seed + "&issuer=Example":
                "otpauth://totp/Example:me?secret=" + URL_MASK + "&issuer=Example",
            # Percent-escapes inside a value (base64 `+` and `=`) are part of it.
            "?sv=2022-11-02&sp=r&sig=" + OPAQUE[:10] + "%2B" + OPAQUE[10:] + "%3D":
                "?sv=2022-11-02&sp=r&sig=" + URL_MASK,
            # A URL nested in another, percent-encoded; the value ends at %26.
            "next=https%3A%2F%2Fh.example%2Fcb%3Faccess_token%3D" + OPAQUE + "%26state%3Dok":
                "next=https%3A%2F%2Fh.example%2Fcb%3Faccess_token%3D" + URL_MASK + "%26state%3Dok",
            # JSON-escaped (Slack's API escapes `/` and some JSON encoders `&`).
            '{"url":"wss:\\/\\/h\\/?a=1\\u0026frt=' + OPAQUE[:12] + "\\/" + OPAQUE[12:] + '\\u0026b=2"}':
                '{"url":"wss:\\/\\/h\\/?a=1\\u0026frt=' + URL_MASK + '\\u0026b=2"}',
            "<a href=\"/x?a=1&amp;frt=" + OPAQUE + "&amp;b=2\">": "<a href=\"/x?a=1&amp;frt=" + URL_MASK + "&amp;b=2\">",
            # A form-encoded body, such as an OAuth token response.
            "access_token=" + OPAQUE + "&token_type=bearer&scope=repo":
                "access_token=" + URL_MASK + "&token_type=bearer&scope=repo",
        }
        codex = codex_hook()
        for text, expected in cases.items():
            self.assertEqual(M.replacement(bash(text))["updatedToolOutput"]["stdout"], expected, text)
            self.assertNotIn(OPAQUE[:12], codex.decision(bash(text))["reason"], text)
        # A value that ends one string stops at the separator the hooks join
        # strings with, instead of swallowing the next string.
        masked = M.replacement(bash("?frt=" + OPAQUE, stderr="warning: x"))["updatedToolOutput"]
        self.assertEqual((masked["stdout"], masked["stderr"]), ("?frt=" + URL_MASK, "warning: x"))

    def test_ordinary_url_parameters_are_left_alone(self):
        codex = codex_hook()
        for text in (
            "https://api.github.com/repos/o/r/issues?page=2&per_page=100&q=is%3Aopen&sort=updated&state=open",
            # Names that merely contain a credential name, and pagination tokens.
            f"https://h.example/?signal={OPAQUE}&tokenizer={OPAQUE}&secretary={OPAQUE}",
            f"https://www.googleapis.com/drive/v3/files?pageToken={OPAQUE}&nextPageToken={OPAQUE}&next_token={OPAQUE}",
            "https://b.s3.amazonaws.com/k?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Expires=900"
            "&X-Amz-Credential=ASIDEXAMPLE%2F20261115%2Fus-east-1%2Fs3%2Faws4_request",
            # Values too short to be credentials.
            "?token=abc123&sig=short&ticket=PROJ-1234",
            # Code that builds such a URL.
            'url = f"https://h/?token={token}"; u = "?token=" + token; v = "?token=${SLACK_TOKEN}"',
            '(concat "wss://h/?token=" (slack-team-token team) "&frt=" frt)',
        ):
            self.assertIsNone(M.replacement(bash(text + "\n")), text)
            self.assertIsNone(codex.decision(bash(text + "\n")), text)

    def test_numeric_uuid_prefix_survives_both_output_hooks(self):
        codex = codex_hook()
        for value in (MBID, MBID.upper()):
            event = bash(json.dumps({"mbid": value}))
            self.assertIsNone(M.replacement(event))
            self.assertIsNone(codex.decision(event))
        # UUIDs cannot hide an adjacent numeric credential or a contextual token.
        for value in (MULLVAD, "-".join(["1234"] * 4), " ".join(["1234"] * 4), "é" + MULLVAD + "é"):
            output = M.replacement(bash(f"{MBID} {value} {MBID}"))["updatedToolOutput"]["stdout"]
            self.assertEqual(output.count(MBID), 2)
            self.assertNotIn(value, output)
            self.assertIn("[16_DIGIT_NUMBER_REDACTED]", output)
        for value in ("Bearer " + MBID, "Authorization: " + MBID):
            output = M.replacement(bash(value))["updatedToolOutput"]["stdout"]
            self.assertNotIn(MBID, output)
            self.assertIn("[REDACTED]", output)

    def test_decimal_fractions_are_left_alone(self):
        fraction = "1234567" * 2 + "89"  # 16 digits after a decimal point
        for value in (f"-3.{fraction}", f"0.{fraction}", f"replaygain: 1.{fraction} dB"):
            self.assertIsNone(M.replacement(bash(value + "\n")))
        # A bare 16-digit run is still masked, even next to a fraction.
        output = M.replacement(bash(f"0.5 {MULLVAD}"))["updatedToolOutput"]["stdout"]
        self.assertNotIn(MULLVAD, output)

    def test_malformed_uuid_does_not_exempt_numeric_prefix(self):
        for value in (MBID[:-1], MBID + "0", MBID[:-1] + "z", MBID + "-suffix"):
            output = M.replacement(bash(value))["updatedToolOutput"]["stdout"]
            self.assertIn("[16_DIGIT_NUMBER_REDACTED]", output)

    def test_known_uuid_secret_is_still_masked(self):
        import os
        import secret_index
        with tempfile.TemporaryDirectory() as directory:
            key = os.urandom(32)
            path = Path(directory, "index.json")
            secret_index.write(secret_index.build_entries(key, [(MBID, "synthetic:uuid")]), {}, path)
            with patch.dict(os.environ, {"SECRET_INDEX_PATH": str(path), "SECRET_INDEX_KEY_HEX": key.hex()}):
                output = M.replacement(bash(MBID))["updatedToolOutput"]["stdout"]
                self.assertEqual(output, secret_index.REDACTION)

    def test_images_pass_through(self):
        # An ordinary page: images stay (credential pages are tested in test_credential_pages).
        context = '\n\nTab Context:\n- Executed on tabId: 1\n  • tabId 1: "t" ("https://example.org/")'
        ev = {"tool_name": "mcp__claude-in-chrome__computer",
              "tool_response": [{"type": "image", "data": "AAAA"}, {"type": "text", "text": CLOUDFLARE + context}]}
        blocks = M.replacement(ev)["updatedMCPToolOutput"]
        self.assertEqual(blocks[0], {"type": "image", "data": "AAAA"})
        self.assertNotIn(CLOUDFLARE, blocks[1]["text"])

    def test_known_values_are_masked_in_any_tool_result(self):
        # An unprefixed hex secret (the 2026-07-21 Slack app secret shape) is
        # invisible to the shape patterns; the fingerprint index catches it.
        import os
        import tempfile
        sys.path.insert(0, str(HOOK.parents[2] / "lib/python"))
        import secret_index
        hex_secret = "".join("0123456789abcdef"[(i * 7) % 16] for i in range(32))
        with tempfile.TemporaryDirectory() as d:
            key = os.urandom(32)
            path = Path(d, "index.json")
            secret_index.write(secret_index.build_entries(key, [(hex_secret, "test:1")]), {}, path)
            env = {"SECRET_INDEX_PATH": str(path), "SECRET_INDEX_KEY_HEX": key.hex()}
            old = {k: os.environ.get(k) for k in env}
            os.environ.update(env)
            try:
                read = {"tool_name": "Read", "tool_response": {"type": "text", "file": {
                    "filePath": "/tmp/x.json", "content": f'{{"signing_secret": "{hex_secret}"}}', "numLines": 1}}}
                masked = M.replacement(read)["updatedToolOutput"]
                self.assertNotIn(hex_secret, json.dumps(masked))
                self.assertEqual(masked["file"]["numLines"], 1)  # structure kept
                self.assertEqual(masked["file"]["filePath"], "/tmp/x.json")
                grep = {"tool_name": "Grep", "tool_response": {"filenames": [], "content": f"a.env:3:K={hex_secret}"}}
                self.assertNotIn(hex_secret, json.dumps(M.replacement(grep)["updatedToolOutput"]))
                self.assertIsNone(M.replacement({"tool_name": "Read", "tool_response": {"content": "clean"}}))
            finally:
                for k, v in old.items():
                    if v is None:
                        os.environ.pop(k, None)
                    else:
                        os.environ[k] = v

    def test_redactor_failure_withholds_output(self):
        original = M.mask_strings
        M.mask_strings = lambda strings: (_ for _ in ()).throw(RuntimeError("boom"))
        try:
            out = M.replacement(bash(f"value {CLAUDE}\n"))["updatedToolOutput"]
            self.assertEqual(out["stdout"], M.WITHHELD)
            self.assertFalse(out["interrupted"])
            blocks = M.replacement({"tool_name": "mcp__x__y", "tool_response": [
                {"type": "image", "data": "AAAA"}, {"type": "text", "text": "anything"}]})["updatedMCPToolOutput"]
            # No image survives a failed check, and block types stay valid.
            self.assertEqual(blocks[0], {"type": "text", "text": M.WITHHELD})
            self.assertEqual(blocks[1], {"type": "text", "text": M.WITHHELD})
        finally:
            M.mask_strings = original

    def test_hook_protocol(self):
        r = subprocess.run([sys.executable, str(HOOK)], input=json.dumps(bash(CLAUDE)), capture_output=True,
                           text=True, timeout=30)
        out = json.loads(r.stdout)["hookSpecificOutput"]
        self.assertEqual(out["hookEventName"], "PostToolUse")
        self.assertNotIn(CLAUDE, json.dumps(out))
        r = subprocess.run([sys.executable, str(HOOK)], input="not json", capture_output=True, text=True, timeout=30)
        self.assertEqual((r.returncode, r.stdout), (0, ""))


if __name__ == "__main__":
    unittest.main()
