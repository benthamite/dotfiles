"""The security-controls policy as each runtime's registered chain runs it.

tests/agent_guard/test_policy_security_controls.py covers the classifier; these
cases prove that Claude's Bash chain and Codex's shell chain both reach it,
in-process and through the real agent-guard.sh, and that no other policy in
the chain turns its allowed reads into denials.
"""

import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from guard_harness import run_e2e, run_engine  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
MARK = "changes the Mac's security controls, and agents may not run it"

DENY = [
    """osascript -e 'tell application "System Events" to tell process "LuLu" to click button "Allow" of window 1'""",
    """osascript -e 'tell application "System Events" to key code 36'""",
    "false && tccutil reset Accessibility com.example.none",
    """sqlite3 "$HOME/Library/Application Support/com.apple.TCC/TCC.db" 'delete from access'""",
    "sudo fdesetup disable",
    "sudo spctl --master-disable",
    "sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate off",
    "killall LuLu",
    "xattr -rd com.apple.quarantine /Applications/Example.app",
]
ALLOW = [
    """osascript -e 'tell application "System Events" to get name of first application process whose frontmost is true'""",
    """osascript -e 'tell application "System Events" to tell process "LuLu" to get name of windows'""",
    """osascript -e 'tell application "Finder" to get name of front window'""",
    "grep -rn tccutil docs/",
    "fdesetup status",
    "spctl --status",
    "/usr/libexec/ApplicationFirewall/socketfilterfw --getglobalstate",
    "pgrep -x LuLu",
    "xattr -l notes.txt",
    "bin/lulu-directory-rules check",
    "~/repos/dotfiles/bin/opentopage file.pdf 3",
]


def payload(runtime: str, command: str) -> dict:
    if runtime == "codex":
        return {"tool_name": "exec_command", "tool_input": {"cmd": command}, "cwd": str(ROOT)}
    return {"tool_name": "Bash", "tool_input": {"command": command}, "cwd": str(ROOT)}


def decision(result) -> tuple[str, str]:
    assert result.returncode == 0, result.stderr
    output = result.decision() or {}
    return output.get("permissionDecision", "allow"), output.get("permissionDecisionReason", "")


class SecurityControlsChainTests(unittest.TestCase):
    def test_each_class_is_denied_by_both_chains(self):
        for runtime in ("claude", "codex"):
            for command in DENY:
                with self.subTest(runtime=runtime, command=command):
                    verdict, reason = decision(run_engine(runtime, payload(runtime, command), cwd=ROOT))
                    self.assertEqual(verdict, "deny")
                    self.assertIn(MARK, reason)

    def test_reads_helpers_and_mentions_pass_both_chains(self):
        for runtime in ("claude", "codex"):
            for command in ALLOW:
                with self.subTest(runtime=runtime, command=command):
                    verdict, reason = decision(run_engine(runtime, payload(runtime, command), cwd=ROOT))
                    self.assertEqual(verdict, "allow", reason)

    def test_real_hook_scripts(self):
        for runtime in ("claude", "codex"):
            with self.subTest(runtime=runtime):
                verdict, reason = decision(run_e2e(runtime, payload(runtime, DENY[1]), cwd=ROOT))
                self.assertEqual(verdict, "deny")
                self.assertIn(MARK, reason)
                self.assertEqual(decision(run_e2e(runtime, payload(runtime, "spctl --status"), cwd=ROOT))[0], "allow")


if __name__ == "__main__":
    unittest.main()
