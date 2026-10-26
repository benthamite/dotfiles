"""Tests for the paired LaunchAgent registry guards.

The guards exist because every launchd job on this machine was installed
straight into ~/Library/LaunchAgents and only discovered later by the drift
audit in ~/repos/launchd. They deny the hand-rolled paths and name
bin/launchd-install.py instead.
"""

import json
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from guard_harness import run_guard as run_hook  # noqa: E402

from agent_guard import engine  # noqa: E402
from agent_guard.core import CLAUDE, CODEX  # noqa: E402
from agent_guard.policies import launchagent  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
CLAUDE_HOOK = "claude/hooks/block-unregistered-launchagent.sh"
CODEX_HOOK = "codex/hooks/block-unregistered-launchagent.sh"
LIVE_DIR = Path.home() / "Library/LaunchAgents"


def bash(path, command):
    return run_hook(path, {"tool_name": "Bash", "tool_input": {"command": command}})


def reason(result):
    return json.loads(result.stdout)["hookSpecificOutput"]["permissionDecisionReason"]


class DenialContractTest(unittest.TestCase):
    """What the guard must stop, on the Claude side."""

    def assert_denied(self, command):
        result = bash(CLAUDE_HOOK, command)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotEqual(result.stdout, "", f"expected a denial for: {command}")
        payload = json.loads(result.stdout)["hookSpecificOutput"]
        self.assertEqual(payload["permissionDecision"], "deny")
        return payload["permissionDecisionReason"]

    def test_copying_a_plist_into_the_live_directory_is_denied(self):
        self.assert_denied("cp /tmp/com.stafforini.x.plist ~/Library/LaunchAgents/com.stafforini.x.plist")

    def test_symlinking_into_the_live_directory_is_denied(self):
        # Even the correct end state skips the registry entry when done by hand.
        self.assert_denied(
            "ln -s ~/repos/launchd/agents/com.stafforini.x.plist "
            "~/Library/LaunchAgents/com.stafforini.x.plist"
        )

    def test_heredoc_write_into_the_live_directory_is_denied(self):
        self.assert_denied(
            "cat > ~/Library/LaunchAgents/com.stafforini.x.plist <<'EOF'\n<plist/>\nEOF"
        )

    def test_expanded_home_path_is_denied(self):
        self.assert_denied("mv /tmp/com.stafforini.x.plist $HOME/Library/LaunchAgents/")

    def test_absolute_live_path_is_denied(self):
        self.assert_denied(f"cp /tmp/com.stafforini.x.plist {LIVE_DIR}/com.stafforini.x.plist")

    def test_copy_after_a_cd_is_denied(self):
        self.assert_denied("cd /tmp && cp com.stafforini.x.plist ~/Library/LaunchAgents/")

    def test_bootstrap_is_denied(self):
        text = self.assert_denied("launchctl bootstrap gui/501 ~/Library/LaunchAgents/com.stafforini.x.plist")
        self.assertIn("launchd-install.py", text)

    def test_legacy_load_is_denied(self):
        self.assert_denied("launchctl load -w ~/Library/LaunchAgents/com.stafforini.x.plist")

    def test_bootout_is_denied_with_retirement_guidance(self):
        text = self.assert_denied("launchctl bootout gui/501/com.stafforini.x")
        self.assertIn("registry/jobs.json", text)

    def test_write_tool_into_the_live_directory_is_denied(self):
        result = run_hook(
            CLAUDE_HOOK,
            {
                "tool_name": "Write",
                "tool_input": {"file_path": str(LIVE_DIR / "com.stafforini.x.plist")},
            },
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("launchd-install.py", reason(result))

    def test_every_denial_names_the_installer_or_the_registry(self):
        for command in (
            "cp /tmp/com.stafforini.x.plist ~/Library/LaunchAgents/",
            "launchctl bootstrap gui/501 ~/Library/LaunchAgents/com.stafforini.x.plist",
            "launchctl bootout gui/501/com.stafforini.x",
        ):
            with self.subTest(command=command):
                text = self.assert_denied(command)
                self.assertTrue(
                    "launchd-install.py" in text or "registry/jobs.json" in text,
                    "a denial must point somewhere actionable",
                )


class AllowanceContractTest(unittest.TestCase):
    """What the guard must not touch. A guard that fires on inspection gets ignored."""

    def assert_allowed(self, command, hook=CLAUDE_HOOK):
        result = bash(hook, command)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "", f"expected no denial for: {command}")

    def test_the_sanctioned_installer_is_allowed(self):
        self.assert_allowed(
            "python3 ~/repos/launchd/bin/launchd-install.py /tmp/com.stafforini.x.plist "
            "--purpose 'Do the thing.' --load"
        )

    def test_installer_relative_invocation_is_allowed(self):
        self.assert_allowed("bin/launchd-install.py /tmp/com.stafforini.x.plist --purpose x --adopt")

    def test_listing_the_live_directory_is_allowed(self):
        self.assert_allowed("ls -la ~/Library/LaunchAgents/")

    def test_reading_a_live_plist_is_allowed(self):
        self.assert_allowed("cat ~/Library/LaunchAgents/com.stafforini.x.plist")

    def test_plutil_inspection_is_allowed(self):
        self.assert_allowed("plutil -p ~/Library/LaunchAgents/com.stafforini.x.plist")

    def test_readlink_and_diff_are_allowed(self):
        self.assert_allowed("readlink ~/Library/LaunchAgents/com.stafforini.x.plist")
        self.assert_allowed(
            "diff ~/Library/LaunchAgents/com.stafforini.x.plist "
            "~/repos/launchd/agents/com.stafforini.x.plist"
        )

    def test_launchctl_inspection_is_allowed(self):
        self.assert_allowed("launchctl print gui/501/com.stafforini.x")
        self.assert_allowed("launchctl list | grep stafforini")

    def test_the_drift_audit_is_allowed(self):
        self.assert_allowed(
            "python3 ~/repos/launchd/bin/launchd-audit.py "
            "--registry ~/repos/launchd/registry/jobs.json"
        )

    def test_unrelated_commands_are_allowed(self):
        self.assert_allowed("git status")

    def test_a_quoted_path_in_a_heredoc_body_is_not_an_invocation(self):
        # A commit message describing a past install must not trip the guard.
        self.assert_allowed(
            "git commit -F - <<'EOF'\nfix: cp plist to ~/Library/LaunchAgents\nEOF"
        )

    def test_writing_the_canonical_plist_in_the_repo_is_allowed(self):
        result = run_hook(
            CLAUDE_HOOK,
            {
                "tool_name": "Write",
                "tool_input": {
                    "file_path": str(Path.home() / "repos/launchd/agents/com.stafforini.x.plist")
                },
            },
        )
        self.assertEqual(result.stdout, "")

    def test_the_documented_override_is_honoured(self):
        self.assert_allowed("ALLOW_LAUNCHAGENT_CHANGE=1 launchctl bootout gui/501/com.stafforini.x")


PROBE = "com.stafforini.deco-connectivity-probe.plist"

# Read-only inspection that the guard once denied (launchd plan 2026-10-26, item 10).
PLUTIL_READS = [
    f"p=~/Library/LaunchAgents/{PROBE}; plutil -extract StandardOutPath raw $p",
    f"o=$(plutil -extract StandardOutPath raw {LIVE_DIR}/{PROBE})",
    f"plutil -extract StandardOutPath raw -n ~/Library/LaunchAgents/{PROBE}",
    f"plutil -extract Label xml1 -o - ~/Library/LaunchAgents/{PROBE}",
    f"plutil -convert json -o - ~/Library/LaunchAgents/{PROBE} | jq .",
    f"plutil -convert xml1 -o /tmp/x.plist ~/Library/LaunchAgents/{PROBE}",
    f"plutil -lint ~/Library/LaunchAgents/{PROBE}",
    f"plutil ~/Library/LaunchAgents/{PROBE}",
    f"plutil -type Label ~/Library/LaunchAgents/{PROBE}",
    f"p=\"$HOME/Library/LaunchAgents/{PROBE}\"; less \"$p\"",
]

# Writes into the live directory that must still be denied.
PLUTIL_WRITES = [
    f"plutil -convert xml1 ~/Library/LaunchAgents/{PROBE}",
    f"plutil -replace Label -string x ~/Library/LaunchAgents/{PROBE}",
    f"plutil -insert Foo -bool YES ~/Library/LaunchAgents/{PROBE}",
    f"plutil -remove StandardOutPath {LIVE_DIR}/{PROBE}",
    # Without -o, a non-raw -extract overwrites the input file in place.
    f"plutil -extract StartInterval xml1 ~/Library/LaunchAgents/{PROBE}",
    f"plutil -extract Label raw -o ~/Library/LaunchAgents/y.plist /tmp/x.plist",
    f"plutil -convert xml1 -o ~/Library/LaunchAgents/y.plist /tmp/x.plist",
    f"p=~/Library/LaunchAgents/{PROBE}; plutil -convert binary1 \"$p\"",
    f"p=~/Library/LaunchAgents/{PROBE}; cp /tmp/x.plist $p",
    f"p=~/Library/LaunchAgents/{PROBE}; echo x > ${{p}}",
    f"plutil -p /tmp/x.plist > ~/Library/LaunchAgents/{PROBE}",
    f"tee ~/Library/LaunchAgents/{PROBE} < /tmp/x.plist",
]


class PlutilContractTest(unittest.TestCase):
    """Both shell hooks pass plist inspection and still deny plist writes."""

    def test_inspection_is_allowed_by_both_hooks(self):
        for hook in (CLAUDE_HOOK, CODEX_HOOK):
            for command in PLUTIL_READS:
                with self.subTest(hook=hook, command=command):
                    result = bash(hook, command)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout, "")

    def test_writes_are_denied_by_both_hooks(self):
        for hook in (CLAUDE_HOOK, CODEX_HOOK):
            for command in PLUTIL_WRITES:
                with self.subTest(hook=hook, command=command):
                    self.assertIn("launchd-install.py", reason(bash(hook, command)))


class CodexParityTest(unittest.TestCase):
    """The Codex guard must reach the same verdicts through its own payloads."""

    def test_codex_denies_a_hand_installed_plist(self):
        result = bash(CODEX_HOOK, "cp /tmp/com.stafforini.x.plist ~/Library/LaunchAgents/")
        self.assertIn("launchd-install.py", reason(result))

    def test_codex_denies_bootstrap(self):
        result = bash(CODEX_HOOK, "launchctl bootstrap gui/501 ~/Library/LaunchAgents/com.stafforini.x.plist")
        self.assertIn("launchd-install.py", reason(result))

    def test_codex_denies_an_apply_patch_into_the_live_directory(self):
        patch = (
            "*** Begin Patch\n"
            f"*** Add File: {LIVE_DIR}/com.stafforini.x.plist\n"
            "+<plist/>\n"
            "*** End Patch"
        )
        result = run_hook(
            CODEX_HOOK,
            {"tool_name": "apply_patch", "tool_input": {"patch": patch}},
        )
        self.assertIn("launchd-install.py", reason(result))

    def test_codex_allows_the_installer_and_inspection(self):
        for command in (
            "python3 ~/repos/launchd/bin/launchd-install.py /tmp/x.plist --purpose x",
            "ls ~/Library/LaunchAgents/",
            "launchctl print gui/501/com.stafforini.x",
            "git status",
        ):
            with self.subTest(command=command):
                self.assertEqual(bash(CODEX_HOOK, command).stdout, "")

    def test_the_two_guards_agree_on_every_case(self):
        cases = [
            "cp /tmp/com.stafforini.x.plist ~/Library/LaunchAgents/com.stafforini.x.plist",
            "ln -s ~/repos/launchd/agents/x.plist ~/Library/LaunchAgents/x.plist",
            "launchctl bootstrap gui/501 ~/Library/LaunchAgents/com.stafforini.x.plist",
            "launchctl bootout gui/501/com.stafforini.x",
            "ls -la ~/Library/LaunchAgents/",
            "cat ~/Library/LaunchAgents/com.stafforini.x.plist",
            "launchctl print gui/501/com.stafforini.x",
            "python3 ~/repos/launchd/bin/launchd-install.py /tmp/x.plist --purpose x",
            "git status",
        ]
        for command in cases:
            with self.subTest(command=command):
                claude = bash(CLAUDE_HOOK, command).stdout
                codex = bash(CODEX_HOOK, command).stdout
                self.assertEqual(bool(claude), bool(codex))


class RegistrationTest(unittest.TestCase):
    def test_both_runtimes_register_the_guard_for_shell_and_write_tools(self):
        for runtime, tools in ((CLAUDE, ("Bash", "Write", "Edit")),
                               (CODEX, ("functions.exec_command", "functions.exec", "apply_patch"))):
            for tool in tools:
                with self.subTest(runtime=runtime, tool=tool):
                    self.assertIn(launchagent, list(engine.policies_for(runtime, tool)))


if __name__ == "__main__":
    unittest.main()
