"""Screenshots of credential pages never reach the model (both runtimes)."""
import importlib.util
import json
import os
import subprocess
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.dont_write_bytecode = True
sys.path.insert(0, str(ROOT / "lib/python"))
import credential_pages as cp  # noqa: E402

spec = importlib.util.spec_from_file_location("claude_redact", ROOT / "claude/hooks/redact-tool-output.py")
CLAUDE = importlib.util.module_from_spec(spec)
spec.loader.exec_module(CLAUDE)

IMAGE = {"type": "image", "source": {"type": "base64", "data": "AAAA"}}


def chrome(url, tool_input=None):
    context = (f'\n\nTab Context:\n- Executed on tabId: 1\n- Available tabs:\n  • tabId 1: "t" ("{url}")'
               if url else "")
    return {"tool_name": "mcp__claude-in-chrome__computer",
            "tool_input": tool_input or {"action": "screenshot", "tabId": 1},
            "tool_response": [{"type": "text", "text": "Successfully captured screenshot" + context}, IMAGE]}


class CredentialPageTests(unittest.TestCase):
    def setUp(self):
        os.environ["SECRET_INDEX_PATH"] = "/nonexistent/index.json"

    def test_listed_pages_match(self):
        pages = cp.patterns()
        for url in ("https://github.com/settings/tokens", "https://my.1password.com/vaults/x",
                    "https://dash.cloudflare.com/profile/api-tokens", "https://platform.openai.com/api-keys",
                    "https://console.cloud.google.com/auth/clients/abc?project=p",
                    "https://api.slack.com/apps/A123/oauth"):
            with self.subTest(url=url):
                self.assertTrue(any(p.search(url) for p in pages))
        for url in ("https://github.com/anthropics/claude-code", "https://example.org/settings/tokens"):
            with self.subTest(url=url):
                self.assertFalse(any(p.search(url) for p in pages))

    def claude_blocks(self, event):
        out = CLAUDE.replacement(event)
        return out["updatedMCPToolOutput"] if out else event["tool_response"]

    def test_claude_strips_images_on_credential_or_unknown_pages(self):
        for event in (chrome("https://github.com/settings/tokens"), chrome(None),
                      chrome("https://example.org/", {"actions": [
                          {"name": "navigate", "input": {"url": "https://my.1password.com/"}}]})):
            blocks = self.claude_blocks(event)
            self.assertFalse(any(b.get("type") == "image" for b in blocks))
            self.assertTrue(any(cp.NOTICE == b.get("text") for b in blocks))

    def test_claude_keeps_images_on_ordinary_pages(self):
        blocks = self.claude_blocks(chrome("https://example.org/"))
        self.assertIn(IMAGE, blocks)

    def test_codex_strips_images_on_credential_pages(self):
        hook = ROOT / "codex/hooks/redact-tool-output.py"
        env = {**os.environ, "SECRET_INDEX_PATH": "/nonexistent/index.json"}
        for url, stripped in (("https://github.com/settings/tokens", True), ("https://example.org/", False)):
            event = chrome(url)
            event["tool_name"] = "mcp__node_repl__js"
            r = subprocess.run([sys.executable, str(hook)], input=json.dumps(event), capture_output=True,
                               text=True, env=env)
            with self.subTest(url=url):
                if stripped:
                    out = json.loads(r.stdout)
                    self.assertEqual(out["decision"], "block")
                    self.assertIn(cp.NOTICE, out["reason"])
                    self.assertNotIn("AAAA", out["reason"])
                else:
                    self.assertEqual(r.stdout.strip(), "")


if __name__ == "__main__":
    unittest.main()
