"""chrome-bridge-recover: detect a disconnected Claude in Chrome bridge and recover."""
import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

HOOK = Path(__file__).resolve().parents[1] / "claude/hooks/chrome-bridge-recover.py"
spec = importlib.util.spec_from_file_location("chrome_bridge_recover", HOOK)
M = importlib.util.module_from_spec(spec)
spec.loader.exec_module(M)

NOT_CONNECTED = ("Browser extension is not connected. Please ensure the Claude browser extension is "
                 "installed and running")


def event(tool="mcp__claude-in-chrome__tabs_context_mcp", response=NOT_CONNECTED, name="PostToolUse"):
    return {"hook_event_name": name, "tool_name": tool, "tool_response": response}


class RecoverTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.calls = self.root / "calls"
        launcher = self.root / "chrome-profile-open"
        launcher.write_text(f'#!/bin/sh\necho "$@" >> "{self.calls}"\n')
        launcher.chmod(0o700)
        claude = self.root / "claude"
        claude.write_text("#!/bin/sh\n")
        claude.chmod(0o700)
        wrapper = self.root / "chrome-native-host"
        wrapper.write_text(f"#!/bin/sh\nexec {claude} --chrome-native-host\n")
        manifest = self.root / "manifest.json"
        manifest.write_text(json.dumps({"path": str(wrapper)}))
        profiles = self.root / "profiles.json"
        profiles.write_text(json.dumps({"config_dirs": {".claude-tlon": "tlon"}}))
        for name, value in (("LAUNCHER", launcher), ("MANIFEST", manifest), ("PROFILE_MAP", profiles)):
            old = getattr(M, name)
            setattr(M, name, value)
            self.addCleanup(setattr, M, name, old)
        self.wrapper, self.claude, self.manifest = wrapper, claude, manifest
        self.env = {"CLAUDE_CONFIG_DIR": "/Users/x/.claude-tlon", "TMPDIR": str(self.root)}

    def opened(self):
        return self.calls.read_text().splitlines() if self.calls.exists() else []

    def test_connected_results_are_left_alone(self):
        for ev in (event(response='{"availableTabs":[{"tabId":1}]}'),
                   event(tool="mcp__claude-in-chrome__list_connected_browsers",
                         response='[{"deviceId":"b92","inUse":true}]')):
            self.assertIsNone(M.advice(ev, self.env))
        # With one browser connected and in use, switch_browser says there is
        # nothing to switch to; that is not a disconnection.
        self.assertIsNone(M.advice(event(tool="mcp__claude-in-chrome__switch_browser",
                                         response="No other browsers available to switch to."), self.env))
        self.assertEqual(self.opened(), [])

    def test_not_connected_opens_the_mapped_profile_and_says_retry(self):
        text = M.advice(event(), self.env)
        self.assertEqual(self.opened(), ["tlon about:blank"])
        self.assertIn("opened Chrome profile 'tlon'", text)
        self.assertIn("Retry the browser call now", text)
        self.assertNotIn("Native host:", text)

    def test_other_disconnection_forms_are_detected(self):
        for ev in (event(response="The hidden tabs_context_mcp lookup did not respond within 8s.",
                         name="PostToolUseFailure"),
                   event(tool="mcp__claude-in-chrome__list_connected_browsers", response="[]"),
                   event(tool="mcp__claude-in-chrome__list_connected_browsers",
                         response=[{"type": "text", "text": "[]"}]),
                   ):
            with self.subTest(ev=ev):
                self.assertTrue(M.disconnected(ev))

    def test_profile_is_not_reopened_within_the_interval(self):
        M.advice(event(), self.env)
        text = M.advice(event(), self.env)
        self.assertEqual(self.opened(), ["tlon about:blank"])
        self.assertIn("not reopening", text)

    def test_unmapped_account_asks_once_instead_of_guessing(self):
        text = M.advice(event(), dict(self.env, CLAUDE_CONFIG_DIR="/Users/x/.claude-epoch"))
        self.assertEqual(self.opened(), [])
        self.assertIn("No Chrome profile is mapped", text)
        self.assertIn(".claude-epoch", text)

    def test_stale_native_host_binary_is_named(self):
        self.claude.unlink()
        text = M.advice(event(), self.env)
        self.assertIn("no longer exists", text)
        self.assertIn(str(self.claude), text)

    def test_missing_wrapper_and_manifest_are_named(self):
        self.wrapper.unlink()
        self.assertIn("does not exist", M.manifest_problem(self.manifest))
        self.manifest.unlink()
        self.assertIn("missing or unreadable", M.manifest_problem(self.manifest))

    def test_hook_output_shape_on_both_events(self):
        for name in ("PostToolUse", "PostToolUseFailure"):
            with self.subTest(name=name):
                env = dict(os.environ, CLAUDE_CONFIG_DIR="/Users/x/.claude-unmapped")
                r = subprocess.run([sys.executable, str(HOOK)], input=json.dumps(event(name=name)),
                                   capture_output=True, text=True, env=env, timeout=30)
                out = json.loads(r.stdout)["hookSpecificOutput"]
                self.assertEqual(out["hookEventName"], name)
                self.assertIn("No Chrome profile is mapped", out["additionalContext"])

    def test_bad_stdin_is_ignored(self):
        r = subprocess.run([sys.executable, str(HOOK)], input="not json", capture_output=True, text=True, timeout=30)
        self.assertEqual((r.returncode, r.stdout), (0, ""))


if __name__ == "__main__":
    unittest.main()
