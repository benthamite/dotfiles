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
import unittest
from pathlib import Path

HOOK = Path(__file__).resolve().parents[1] / "claude/hooks/redact-tool-output.py"
spec = importlib.util.spec_from_file_location("redact_tool_output", HOOK)
M = importlib.util.module_from_spec(spec)
spec.loader.exec_module(M)

CLOUDFLARE = "cf" + "ut_" + "Ab3" * 15
MULLVAD = "5" + "1234567" * 2 + "8"  # 16 digits
CLAUDE = "sk-" + "ant-" + "oat01-" + "Zx9_" * 20
SUBSTACK = "s%" + "3A" + "Qw8." * 12


def bash(stdout, stderr=""):
    return {"tool_name": "Bash", "tool_response": {"stdout": stdout, "stderr": stderr, "interrupted": False}}


def mcp(text):
    return {"tool_name": "mcp__claude-in-chrome__find", "tool_response": [{"type": "text", "text": text}]}


class RedactToolOutputTest(unittest.TestCase):
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
        # Longer digit runs (tweet ids, timestamps) are not 16-digit numbers.
        self.assertIsNone(M.replacement(bash("x.com/a/status/1790549176458123456 at 1790549176458\n")))

    def test_images_pass_through(self):
        ev = {"tool_name": "mcp__claude-in-chrome__computer",
              "tool_response": [{"type": "image", "data": "AAAA"}, {"type": "text", "text": CLOUDFLARE}]}
        blocks = M.replacement(ev)["updatedMCPToolOutput"]
        self.assertEqual(blocks[0], {"type": "image", "data": "AAAA"})
        self.assertNotIn(CLOUDFLARE, blocks[1]["text"])

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
