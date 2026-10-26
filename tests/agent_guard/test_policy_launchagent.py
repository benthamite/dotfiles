import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import CLAUDE, CODEX, Call  # noqa: E402
from agent_guard.policies import launchagent  # noqa: E402

ENV = {"HOME": "/h"}


def call(runtime, tool, tool_input):
    return Call.from_payload(runtime, {"tool_name": tool, "tool_input": tool_input}, env=ENV)


def first_line(result):
    return result.reason.split("\n", 1)[0] if result.decision == "deny" else "allow"


class FileTests(unittest.TestCase):
    def test_write_into_live_dir(self):
        result = launchagent.decide(call(CLAUDE, "Write", {"file_path": "/h/Library/LaunchAgents/x.plist"}))
        self.assertEqual(first_line(result), "BLOCKED: writing a plist directly into ~/Library/LaunchAgents.")
        self.assertIn("\n\n/h/Library/LaunchAgents/x.plist\n\n", result.reason)

    def test_other_paths_and_the_directory_itself(self):
        for path in ("/h/Library/LaunchAgents", "/h/Library/LaunchAgentsX/a", "/other/Library/LaunchAgents/a"):
            self.assertEqual(launchagent.decide(call(CLAUDE, "Write", {"file_path": path})).decision, "allow")

    def test_claude_file_path_skips_the_command(self):
        tool_input = {"file_path": "/tmp/a", "command": "launchctl load x"}
        self.assertEqual(launchagent.decide(call(CLAUDE, "Edit", tool_input)).decision, "allow")
        self.assertEqual(launchagent.decide(call(CODEX, "Edit", tool_input)).decision, "deny")

    def test_codex_patch_paths(self):
        patch = "*** Begin Patch\n*** Add File: /h/Library/LaunchAgents/x.plist\n+x\n*** End Patch"
        self.assertEqual(launchagent.decide(call(CODEX, "apply_patch", {"patch": patch})).decision, "deny")


class CommandTests(unittest.TestCase):
    def label(self, command, runtime=CLAUDE):
        return first_line(launchagent.decide(call(runtime, "Bash", {"command": command})))

    def test_kinds(self):
        self.assertEqual(self.label("cp a.plist ~/Library/LaunchAgents/"),
                         "BLOCKED: installing a plist into ~/Library/LaunchAgents by hand.")
        self.assertEqual(self.label("echo x > $HOME/Library/LaunchAgents/a.plist"),
                         "BLOCKED: installing a plist into ~/Library/LaunchAgents by hand.")
        self.assertEqual(self.label("launchctl bootstrap gui/501 x"),
                         "BLOCKED: loading a LaunchAgent outside the launchd registry.")
        self.assertEqual(self.label("launchctl bootout gui/501/x"),
                         "BLOCKED: unloading or removing a LaunchAgent outside the launchd registry.")

    def test_allowed(self):
        for command in ("launchctl list", "ls ~/Library/LaunchAgents", "plutil -p ~/Library/LaunchAgents/a.plist",
                        "defaults read ~/Library/LaunchAgents/a", "python3 ~/repos/launchd/bin/launchd-install.py a",
                        "ALLOW_LAUNCHAGENT_CHANGE=1 launchctl load x", "cat <<EOF\nlaunchctl load x\nEOF"):
            self.assertEqual(self.label(command), "allow", command)

    def test_removing_a_live_plist_is_denied_even_with_the_override(self):
        removed = "BLOCKED: removing a plist from ~/Library/LaunchAgents by hand."
        for command in ("trash ~/Library/LaunchAgents/a.plist", "rm -f /h/Library/LaunchAgents/a.plist",
                        "ALLOW_LAUNCHAGENT_CHANGE=1 launchctl bootout gui/501/a && trash ~/Library/LaunchAgents/a.plist",
                        "unlink $HOME/Library/LaunchAgents/a.plist"):
            self.assertEqual(self.label(command), removed, command)
            self.assertIn("launchd-retire.py", launchagent.decide(
                call(CLAUDE, "Bash", {"command": command})).reason)

    def test_the_reloader_is_allowed_and_named_for_reloads(self):
        self.assertEqual(self.label("python3 ~/repos/launchd/bin/launchd-reload.py a b"), "allow")
        result = launchagent.decide(call(CLAUDE, "Bash", {"command": "launchctl bootstrap gui/501 x"}))
        self.assertIn("launchd-reload.py", result.reason)

    def test_bootout_names_the_retirer(self):
        result = launchagent.decide(call(CLAUDE, "Bash", {"command": "launchctl bootout gui/501/x"}))
        self.assertIn("launchd-retire.py", result.reason)

    def test_the_retirer_and_unrelated_removals_are_allowed(self):
        for command in ("python3 ~/repos/launchd/bin/launchd-retire.py a --reason x",
                        "trash /tmp/a.plist", "rm ~/repos/launchd/agents/a.plist"):
            self.assertEqual(self.label(command), "allow", command)

    def test_fragment_keeps_leading_operator(self):
        result = launchagent.decide(call(CODEX, "exec_command", {"cmd": "x;\n\ncp a ~/Library/LaunchAgents/"}))
        self.assertIn("\n\n  ;\n\ncp a ~/Library/LaunchAgents/\n\n", result.reason)

    def test_long_command_with_early_match_is_denied(self):
        # Intended difference 15: the hook's pre-filter lost this to SIGPIPE and exited 0.
        self.assertEqual(self.label("launchctl bootstrap gui/501 x\n" + "x\n" * 200_001),
                         "BLOCKED: loading a LaunchAgent outside the launchd registry.")

    def test_plutil_reads_are_allowed_in_both_runtimes(self):
        for command in ("p=~/Library/LaunchAgents/a.plist; plutil -extract StandardOutPath raw $p",
                        "o=$(plutil -extract StandardOutPath raw /h/Library/LaunchAgents/a.plist)",
                        "plutil -extract Label json -o - ~/Library/LaunchAgents/a.plist",
                        "plutil -lint ~/Library/LaunchAgents/a.plist",
                        "plutil -extract Label raw ~/Library/LaunchAgents/a.plist 2>/dev/null"):
            for runtime in (CLAUDE, CODEX):
                self.assertEqual(self.label(command, runtime), "allow", (runtime, command))

    def test_plutil_writes_are_denied_in_both_runtimes(self):
        written = "BLOCKED: installing a plist into ~/Library/LaunchAgents by hand."
        for command in ("plutil -convert xml1 ~/Library/LaunchAgents/a.plist",
                        "plutil -replace Label -string x $HOME/Library/LaunchAgents/a.plist",
                        "plutil -extract K xml1 ~/Library/LaunchAgents/a.plist",
                        "plutil -extract K raw -o ~/Library/LaunchAgents/b /tmp/a.plist",
                        "p=~/Library/LaunchAgents/a.plist; plutil -remove K \"$p\"",
                        "p=/h/Library/LaunchAgents/a.plist && mv /tmp/a.plist $p"):
            for runtime in (CLAUDE, CODEX):
                self.assertEqual(self.label(command, runtime), written, (runtime, command))

    def test_override_needs_whitespace_boundaries(self):
        self.assertEqual(self.label("echo ALLOW_LAUNCHAGENT_CHANGE=1x; launchctl load y"),
                         "BLOCKED: loading a LaunchAgent outside the launchd registry.")


if __name__ == "__main__":
    unittest.main()
