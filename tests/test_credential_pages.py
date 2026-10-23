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


    # Fake values shaped like the 2026-10-23 exposure (R2 S3 credentials: bare hex, no prefix).
    FAKE_ID = "0123456789abcdef0123456789abcdef"
    FAKE_SECRET = "fedcba9876543210" * 4
    R2 = "https://dash.cloudflare.com/0123456789abcdef0123456789abcdef/r2/api-tokens/success"

    def page_text(self, url, tool="mcp__claude-in-chrome__get_page_text"):
        text = (f"Access Key ID\n\n{self.FAKE_ID}\nClick to copy\n\nSecret Access Key\n\n{self.FAKE_SECRET}\n"
                f"Click to copy\n\nTab Context:\n- Available tabs:\n  • tabId 1: \"R2\" (\"{url}\")")
        return {"tool_name": tool, "tool_input": {"tabId": 1}, "tool_response": [{"type": "text", "text": text}]}

    def test_claude_masks_opaque_text_on_credential_pages(self):
        out = json.dumps(self.claude_blocks(self.page_text(self.R2)))
        self.assertNotIn(self.FAKE_ID, out.replace(self.R2, ""))
        self.assertNotIn(self.FAKE_SECRET, out)
        self.assertIn(cp.OPAQUE_MASK, out)
        self.assertIn(self.R2, out)  # URLs are kept, so the tab context stays usable

    def test_claude_leaves_text_on_ordinary_pages(self):
        event = self.page_text("https://example.org/commit")
        self.assertIsNone(CLAUDE.replacement(event))

    def test_codex_masks_opaque_text_on_credential_pages(self):
        hook = ROOT / "codex/hooks/redact-tool-output.py"
        env = {**os.environ, "SECRET_INDEX_PATH": "/nonexistent/index.json"}
        event = self.page_text(self.R2, tool="mcp__node_repl__js")
        r = subprocess.run([sys.executable, str(hook)], input=json.dumps(event), capture_output=True, text=True, env=env)
        out = json.loads(r.stdout)["reason"]
        self.assertNotIn(self.FAKE_SECRET, out)
        self.assertIn(cp.OPAQUE_MASK, out)

    def test_mask_opaque_spares_words_and_short_ids(self):
        self.assertEqual(cp.mask_opaque("ref_123 tabId 453647500 Infrastructure"), "ref_123 tabId 453647500 Infrastructure")


if __name__ == "__main__":
    unittest.main()
