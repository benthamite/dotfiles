import sys
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard import engine  # noqa: E402
from agent_guard.core import CLAUDE, CODEX, Call  # noqa: E402
from agent_guard.policies import security_controls as policy  # noqa: E402

TCC_SYSTEM = "/Library/Application Support/com.apple.TCC/TCC.db"
FIREWALL = "/usr/libexec/ApplicationFirewall/socketfilterfw"
EVENTS = 'tell application "System Events" to key code 36'
LOOK = 'tell application "System Events" to get name of every process'


def claude(command):
    return policy.decide(Call.from_payload(CLAUDE, {"tool_name": "Bash", "tool_input": {"command": command}}))


def codex(command, tool="exec_command"):
    return policy.decide(Call.from_payload(CODEX, {"tool_name": tool, "tool_input": {"cmd": command}}))


DENIED = {
    policy.APPLESCRIPT: [
        f"osascript -e '{EVENTS}'",
        'osascript -e "tell application \\"System Events\\" to key code 36"',
        """/usr/bin/osascript -l JavaScript -e 'Application("System Events").keystroke("a")'""",
        """osascript -e 'tell application "System Preferences" to quit'""",
        """osascript -e 'tell application "System Settings" to reveal pane id "com.apple.Lock-Settings.extension"'""",
        """osascript -e 'tell application id "com.apple.systempreferences" to set current pane to pane 1'""",
        """osascript -e 'tell process "SecurityAgent" to click button "Allow" of window 1'""",
        """osascript -e 'tell application "LuLu" to quit'""",
        """osascript -e 'quit app "LuLu"'""",
        f"""osascript -e 'tell application "Preview" to activate' -e '{EVENTS}'""",
        f"osascript <<'EOF'\n{EVENTS}\nEOF",
        f"osascript <<EOF\n{EVENTS}\nEOF",
        """osascript -l JavaScript <<'JXA'\nconst events = Application("System Events");\nevents.keyCode(36);\nJXA""",
        f"echo '{EVENTS}' | osascript",
        f"printf '%s' '{EVENTS}' | osascript -",
        f"osascript <<< '{EVENTS}'",
        f"sudo osascript -e '{EVENTS}'",
        f"env LANG=C command osascript -e '{EVENTS}'",
        f"cd /tmp && osascript -e '{EVENTS}'",
        """bash -c "osascript -e 'tell application \\"System Events\\" to key code 36'" """,
        f"S='{EVENTS}'; osascript -e \"$S\"",
        """osascript -e 'tell application "System'' Events" to key code 36'""",
        # An action after a read, inside a tell block, in a heredoc, split across -e arguments.
        f"""osascript -e '{LOOK}' -e 'tell application "System Events" to keystroke "q" using command down'""",
        """osascript -e 'tell application "System Events" to get name of processes
tell application "System Events" to click button 1 of window 1 of process "LuLu"'""",
        """osascript -e 'tell application "System Events"' -e 'tell process "LuLu"' -e 'get name of windows' """
        """-e 'click button "Allow" of window 1' -e 'end tell' -e 'end tell'""",
        """osascript <<'EOF'\ntell application "System Events"\n  tell process "LuLu"\n    set n to count of windows\n"""
        """    click button "Allow" of window 1\n  end tell\nend tell\nEOF""",
        """osascript <<'EOF' | head -c 400\ntell application "System Events" to tell process "LuLu"\n"""
        """  get name of windows\n  set value of text field 1 of window 1 to "x"\nend tell\nEOF""",
        """osascript -e 'tell application "System Events" to tell process "LuLu" to set frontmost to true'""",
        """osascript -e 'tell application "System Events" to tell appearance preferences to set dark mode to true'""",
        """osascript -e 'tell application "System Events" to perform action "AXPress" of button 1 of window 1 of process "LuLu"'""",
        """osascript -e 'tell application "System Events" to get (click button 1 of window 1 of process "LuLu")'""",
        """osascript -e 'tell application "System Events" to do shell script "id"'""",
        """osascript -e 'tell application "System Events" to make new login item at end'""",
        """osascript -e 'set out to ""' -e 'tell application "System Events" to log out'""",
        # Reads the guard cannot follow stay denied: JXA, SecurityAgent, a pipe, a here-string, a variable.
        """osascript -l JavaScript -e 'Application("System Events").processes.name()'""",
        """osascript -e 'tell application "System Events" to get name of windows of process "SecurityAgent"'""",
        f"echo '{LOOK}' | osascript",
        f"osascript <<< '{LOOK}'",
        'osascript -e "tell application \\"System Events\\" to get name of process \\"$1\\""',
        f"osascript <<EOF\ntell application \"System Events\" to get name of process \"$P\"\nEOF",
        """cat > /tmp/x.scpt <<'EOF'\ntell application "System Events" to key code 36\nEOF\nosascript -e 'run script POSIX file "/tmp/x.scpt"'""",
        # A script file written and run by the same command.
        f"cat > /tmp/s.applescript <<'EOF'\n{EVENTS}\nEOF\nosascript /tmp/s.applescript",
        """S=/tmp/x; cat > $S/a.js <<'EOF'\nconst p = Application("System Events");\nEOF\ntimeout 60 osascript -l JavaScript $S/a.js""",
    ],
    policy.TCCUTIL: [
        "tccutil", "tccutil reset All", "tccutil reset Accessibility com.example.app", "sudo tccutil reset All",
        "/usr/bin/tccutil reset Camera", "false && tccutil reset Accessibility com.example.none",
        "echo x; tccutil reset All", "echo x\ntccutil reset All", "bash -c 'tccutil reset All'",
        'zsh -lc "tccutil reset All"', 'eval "tccutil reset All"', "echo All | xargs tccutil reset",
        "x=$(tccutil reset All)", "echo `tccutil reset All`", "T=tccutil; $T reset All", "export T=tccutil; $T reset",
        "tcc'util' reset All", "tcc\\util reset All", "sh <<EOF\ntccutil reset All\nEOF", "echo 'tccutil reset All' | sh",
        "bash <<< 'tccutil reset All'", "nohup tccutil reset All &", "if true; then tccutil reset All; fi",
        "for s in Camera Microphone; do tccutil reset $s; done", "find . -name x -exec tccutil reset All \\;",
        "(tccutil reset All)", "{ tccutil reset All; }", "timeout 5 tccutil reset All", "TCCUTIL reset All",
        "sudo \\\n  tccutil reset All", "echo 'unclosed; tccutil reset All",
    ],
    policy.TCC_DB: [
        "sqlite3 ~/Library/Application\\ Support/com.apple.TCC/TCC.db 'delete from access'",
        f'sqlite3 "{TCC_SYSTEM}" "select client from access"',
        f"sudo sqlite3 '{TCC_SYSTEM}' 'update access set auth_value=2'",
        'cd "/Library/Application Support/com.apple.TCC" && sqlite3 TCC.db .dump',
        'DB="$HOME/Library/Application Support/com.apple.TCC/TCC.db"; sqlite3 "$DB" "delete from access"',
        f"sqlite3 '{TCC_SYSTEM}' <<'EOF'\ndelete from access;\nEOF",
        f'echo x > "{TCC_SYSTEM}"', "echo x >> ~/Library/Application\\ Support/com.apple.TCC/TCC.db",
        f'cp /tmp/other.db "{TCC_SYSTEM}"', f'rm "{TCC_SYSTEM}"', f"trash '{TCC_SYSTEM}'",
        f'mv "{TCC_SYSTEM}" /tmp/moved.db', f'echo x | sudo tee "{TCC_SYSTEM}"', f'dd if=/dev/zero of="{TCC_SYSTEM}"',
        f"""python3 -c "import sqlite3; sqlite3.connect('{TCC_SYSTEM}').execute('delete from access')" """,
        f"python3 - <<'EOF'\nimport sqlite3\ndb = sqlite3.connect('{TCC_SYSTEM}')\ndb.execute('delete from access')\nEOF",
    ],
    policy.FDESETUP: [
        "fdesetup disable", "sudo fdesetup disable", "fdesetup removeuser -user x",
        "sudo fdesetup changerecovery -personal", "fdesetup enable", "fdesetup remove -uuid X", "fdesetup authrestart",
        "/usr/bin/fdesetup add -usertoadd x", "bash -c 'fdesetup disable'", "sudo fdesetup removerecovery -personal",
        "fdesetup -verbose disable",
    ],
    policy.SPCTL: [
        "spctl --master-disable", "sudo spctl --global-disable", "spctl --disable", "spctl --add /Applications/X.app",
        "spctl --remove --label X", 'spctl --add --label "ok" /x', "spctl --master-enable",
        "sudo /usr/sbin/spctl --master-disable", "spctl developer-mode enable-terminal", "spctl --assess --add /x",
        "spctl --disable --label X", "sudo -u root spctl --global-disable",
    ],
    policy.FIREWALL: [
        f"{FIREWALL} --setglobalstate off", f"sudo {FIREWALL} --setstealthmode off", f"{FIREWALL} --setblockall off",
        f"{FIREWALL} --setallowsigned on", f"{FIREWALL} --setallowsignedapp on", f"{FIREWALL} --add /usr/bin/python3",
        f"{FIREWALL} --remove /x", f"{FIREWALL} --unblockapp /x", f"{FIREWALL} --blockapp /x",
        f"cd /usr/libexec/ApplicationFirewall && ./socketfilterfw --setglobalstate off",
        f"{FIREWALL} --getglobalstate --setglobalstate off",
    ],
    policy.LULU: [
        "killall LuLu", "pkill -x LuLu", "pkill -f LuLu.app", "sudo killall -9 LuLu", "kill $(pgrep LuLu)",
        "kill -9 $(pgrep -x LuLu)", "pgrep LuLu | xargs kill", "pkill -f com.objective-see.lulu.extension",
        "PID=$(pgrep LuLu); kill $PID", "launchctl bootout system/com.objective-see.lulu",
        "sudo launchctl unload /Library/LaunchDaemons/com.objective-see.lulu.plist",
        "launchctl kill TERM system/com.objective-see.lulu", "launchctl disable system/com.objective-see.lulu",
        "ALLOW_LAUNCHAGENT_CHANGE=1 launchctl bootout gui/501/com.objective-see.lulu.app",
        "systemextensionsctl uninstall VBG97UB4TA com.objective-see.lulu.extension",
        "defaults write com.objective-see.lulu disabled -bool true", "defaults delete com.objective-see.lulu",
        "defaults -currentHost delete com.objective-see.lulu passiveMode",
        "sudo defaults write /Library/Objective-See/LuLu/preferences disabled -bool true",
        "sudo rm /Library/Objective-See/LuLu/rules.plist", "sudo trash /Library/Objective-See/LuLu",
        "echo x | sudo tee /Library/Objective-See/LuLu/rules.plist",
        "sudo cp /tmp/rules.plist /Library/Objective-See/LuLu/rules.plist",
        "echo x > /Library/Objective-See/LuLu/preferences.plist",
        "sudo plutil -remove key /Library/Objective-See/LuLu/rules.plist",
        '/usr/libexec/PlistBuddy -c "Delete :x" /Library/Objective-See/LuLu/rules.plist',
        "sudo sed -i '' s/a/b/ /Library/Objective-See/LuLu/preferences.plist",
        "sudo find /Library/Objective-See/LuLu -name '*.plist' -delete",
    ],
    policy.QUARANTINE: [
        "xattr -d com.apple.quarantine /Applications/X.app", "xattr -rd com.apple.quarantine ~/Downloads/x",
        "xattr -dr com.apple.quarantine x", "xattr -r -d com.apple.quarantine x",
        "sudo xattr -d -r com.apple.quarantine x", "xattr -c x", "xattr -cr x", "xattr -rc x", "xattr -r -c x",
        "xattr -d 'com.apple.quarantine' x", "find . -name '*.app' -exec xattr -d com.apple.quarantine {} \\;",
        "find ~/Downloads -print0 | xargs -0 xattr -d com.apple.quarantine",
        "/usr/bin/xattr -d com.apple.quarantine x", "brew install x && xattr -cr /Applications/X.app",
    ],
    policy.ANGER_LOCK: [
        "trash ~/.local/state/anger-lock/locked-until",
        "rm -f $HOME/.local/state/anger-lock/locked-until",
        "echo 0 > ~/.local/state/anger-lock/locked-until",
        "rm -rf ~/.local/state/anger-lock",
        "mv ~/.local/state/anger-lock /tmp/x",
        "touch ~/.local/state/anger-lock/locked-until",
        "bash -c 'rm ~/.local/state/anger-lock/locked-until'",
    ],
}

ALLOWED = [
    # AppleScript addressed to other applications, and script files.
    """osascript -e 'tell application "Google Chrome" to get URL of active tab of front window'""",
    """osascript -e 'tell application "Finder" to activate'""", """osascript -e 'tell application "Preview" to quit'""",
    """osascript -e 'tell application "Emacs" to activate'""", """osascript -e 'tell application "Spotify" to pause'""",
    """osascript -l JavaScript -e 'Application("Finder").name()'""", "osascript /tmp/script.scpt",
    """osascript <<'EOF'\ntell application "Finder" to get name of front window\nEOF""",
    # Looking at System Events, System Settings and LuLu: reads and plain control flow.
    f"osascript -e '{LOOK}'",
    """osascript -e 'tell application "System Events" to get name of first application process whose frontmost is true'""",
    """osascript -e 'tell application "System Events" to tell process "LuLu" to get {name, title} of every window'""",
    """timeout 20 osascript -e 'tell application "System Events" to tell process "LuLu" to get name of windows' 2>&1""",
    """osascript -e 'tell application "System Events" to count (windows of process "LuLu" whose name is "LuLu Alert")'""",
    """osascript -e 'tell application "System Events" to get UI elements enabled'""",
    """osascript -e 'tell application "System Events"' -e 'tell process "LuLu"' -e 'get name of windows' """
    """-e 'end tell' -e 'end tell'""",
    """osascript -e 'tell application "System Events" to get name of processes
tell application "System Events" to get title of window 1 of process "Google Chrome"'""",
    """for i in 1 2 3; do osascript -e 'tell application "System Events" to get bundle identifier of first """
    """application process whose frontmost is true' 2>&1; done""",
    """osascript <<'EOF'\ntell application "System Events" to tell process "LuLu"\n  set out to ""\n"""
    """  repeat with w in windows\n    set out to out & (name of w) & linefeed\n  end repeat\n  return out\nend tell\nEOF""",
    """osascript <<'EOF' | head -c 4000\ntell application "System Events" to tell process "LuLu"\n"""
    """  set w to (first window whose name starts with "LuLu Rules")\n  return {count rows of outline 1 of scroll area 1 of w}\n"""
    """end tell\nEOF""",
    """osascript <<EOF\ntell application "System Events" to get title of every window of process "Google Chrome"\nEOF""",
    """osascript -e 'tell application "System Settings" to activate'""",
    """osascript -e 'tell application id "com.apple.systempreferences" to activate'""",
    """osascript -e 'tell application "System Settings" to get name of current pane'""",
    """osascript -e 'tell application "LuLu" to activate'""",
    """osascript -e 'tell application "Google Chrome" to activate' -e 'delay 0.5' """
    f"""-e '{LOOK}'""",
    # Each osascript is judged on its own script.
    """osascript -e 'tell application "Google Chrome" to close (first window whose id is 4)'; """
    f"""osascript -e '{LOOK}'""",
    """osascript -e 'tell application "Finder" to activate'; grep -c "System Events" notes.md""",
    # The sanctioned helpers, run by path.
    "~/repos/dotfiles/bin/drive-errors list", "bin/lulu-directory-rules check",
    "~/repos/dotfiles/bin/lulu-directory-rules add", "python3 bin/lulu-directory-rules add /opt/homebrew/Cellar/git",
    "bin/opentopage file.pdf 3", "~/repos/dotfiles/bin/dictation-language", "bash bin/opentopage file.pdf 3",
    # Inert mentions.
    "grep -rn tccutil docs/", 'rg -n "System Events" bin/', "grep -c TCC.db notes.md", "man tccutil", "which tccutil",
    "command -v tccutil", "type spctl", 'echo "tccutil reset All"', "git log --grep tccutil --oneline",
    'git commit -m "Deny tccutil reset and spctl --master-disable" -- lib/python/agent_guard/policies/x.py',
    """git commit -m 'Deny osascript addressed to "System Events"' -- a.py""",
    'gh pr create --title "Guard tccutil" --body "tccutil reset All and xattr -cr are now denied"',
    'gh issue comment 3 --body "run sudo spctl --master-disable by hand"',
    "cat <<'EOF' > notes.md\ntccutil reset All\nosascript -e 'tell application \"System Events\" to key code 36'\nEOF",
    "printf '%s' 'sudo spctl --master-disable' | ~/repos/dotfiles/claude/bin/kill-ring-put",
    "~/repos/dotfiles/claude/bin/kill-ring-put <<'EOF'\ntccutil reset Accessibility com.example.app\nEOF",
    "grep -rn 'xattr -d com.apple.quarantine' ~/repos/dotfiles/shell", "rg -n 'xattr -cr' bin shell",
    "cat bin/dictation-language", "sed -n 1,30p bin/opentopage", "grep -n osascript bin/opentopage",
    # Read-only forms.
    "fdesetup status", "fdesetup list", "sudo fdesetup haspersonalrecoverykey", "fdesetup hasinstitutionalrecoverykey",
    "fdesetup isactive", "fdesetup supportsauthrestart", "fdesetup status -extended", "fdesetup",
    "spctl --status", "spctl --assess --verbose /Applications/X.app", "spctl -a -vv -t install /x", "spctl",
    f"{FIREWALL} --getglobalstate", f"{FIREWALL} --getstealthmode", f"{FIREWALL} --listapps",
    f"{FIREWALL} --getappblocked /x", f"{FIREWALL} --getblockall; {FIREWALL} --getallowsigned",
    "pgrep LuLu", "pgrep -x LuLu || open -a LuLu", "open -a LuLu", "defaults read com.objective-see.lulu",
    "python3 macos/.claude/skills/security-audit/scripts/lulu-rules.py",
    "python3 macos/.claude/skills/security-audit/scripts/tcc-grants.py",
    "launchctl print system/com.objective-see.lulu", "launchctl list | grep -i lulu",
    "systemextensionsctl list | grep -i objective-see", "plutil -p /Library/Objective-See/LuLu/rules.plist",
    "plutil -convert json -o - /Library/Objective-See/LuLu/rules.plist", "ls -la /Library/Objective-See/LuLu",
    "cp /Library/Objective-See/LuLu/rules.plist /tmp/rules.plist", "pkill -f lulu-directory-rules",
    "ps aux | grep -i lulu | grep -v grep", '/usr/libexec/PlistBuddy -c "Print" /Library/Objective-See/LuLu/rules.plist',
    "xattr -l x", "xattr -p com.apple.quarantine x", "xattr x", "xattr -d com.apple.metadata:kMDItemWhereFroms x",
    "xattr -w com.example.tag 1 x", "xattr -lr ~/Downloads",
    'sqlite3 -readonly "$HOME/Library/Application Support/com.apple.TCC/TCC.db" "select client from access"',
    f'sqlite3 "file:{TCC_SYSTEM}?mode=ro" "select 1"', f'ls -l "{TCC_SYSTEM}"', f'stat "{TCC_SYSTEM}"',
    f'cp "{TCC_SYSTEM}" /tmp/tcc-copy.db',
    f"""python3 -c "import sqlite3; sqlite3.connect('file:{TCC_SYSTEM}?mode=ro', uri=True)" """,
    f"python3 - <<'EOF'\nimport sqlite3\ndb = sqlite3.connect('file:{TCC_SYSTEM}?mode=ro', uri=True)\nEOF",
    # Commands no rule is about.
    "ls", "git status", "kill 1234", "killall Dock", "defaults write com.apple.dock autohide -bool true",
    "launchctl list", "sqlite3 app.db 'delete from t'", "rm notes.txt", "",
    # The anger lock: reading it, and editing the hook that owns it.
    "cat ~/.local/state/anger-lock/locked-until", "tail ~/.local/state/anger-lock/log.jsonl",
    "sed -i '' 's/LOCK_SECONDS = 5/LOCK_SECONDS = 300/' lib/python/anger_lock.py",
]


class DeniedTests(unittest.TestCase):
    def test_each_class_is_denied_in_both_runtimes(self):
        for label, commands in DENIED.items():
            for command in commands:
                for run in (claude, codex):
                    with self.subTest(label=label, runtime=run.__name__, command=command):
                        result = run(command)
                        self.assertEqual(result.decision, "deny")
                        if label == policy.APPLESCRIPT:
                            # Its headline also says why the script did not pass as a read.
                            self.assertTrue(result.reason.startswith(f"BLOCKED: {label} ("), result.reason)
                            self.assertTrue(result.reason.endswith(policy.reason(label).split(".\n\n", 1)[1]))
                        else:
                            self.assertEqual(result.reason, policy.reason(label))

    def test_reason_names_the_class_and_the_handoff(self):
        text = policy.reason(policy.TCCUTIL)
        self.assertTrue(text.startswith("BLOCKED: tccutil, which resets privacy grants.\n\n"))
        self.assertIn("changes the Mac's security controls, and agents may not run it; there is no override", text)
        self.assertIn("hand Pablo the exact command, staged with ~/repos/dotfiles/claude/bin/kill-ring-put", text)

    def test_applescript_denial_says_what_was_not_a_read(self):
        cases = {
            f"osascript -e '{EVENTS}'": "(not recognized as a read: `key code`)",
            """osascript -e 'tell application "LuLu" to quit'""": "(not recognized as a read: `quit`)",
            f"echo '{LOOK}' | osascript": "(the guard cannot see the script; pass it with -e or in a quoted heredoc)",
            """osascript -l JavaScript -e 'Application("System Events").name()'""":
                "(JXA naming these targets is denied; write the read in AppleScript)",
            """osascript -e 'tell application "System Events" to get name of process "SecurityAgent"'""":
                "(SecurityAgent is closed, reads included)",
        }
        for command, detail in cases.items():
            self.assertEqual(claude(command).reason.split(".\n\n")[0], f"BLOCKED: {policy.APPLESCRIPT} {detail}", command)

    def test_no_override_variable(self):
        for prefix in ("ALLOW_SECURITY_CONTROL_CHANGE=1 ", "ALLOW_LAUNCHAGENT_CHANGE=1 ", "ALLOW_CLONE=1 "):
            self.assertEqual(claude(prefix + "tccutil reset All").decision, "deny")

    def test_codex_program_nested_commands(self):
        program = 'const r = await tools.exec_command({cmd: "tccutil reset All"});\ntext(r);'
        self.assertEqual(codex(program, "functions.exec").reason, policy.reason(policy.TCCUTIL))
        program = 'const r = await tools.exec_command({cmd: "spctl --status"});\ntext(r);'
        self.assertEqual(codex(program, "functions.exec").decision, "allow")

    def test_argv_array_commands(self):
        call = Call.from_payload(CODEX, {"tool_name": "Bash", "tool_input": {"command": ["bash", "-lc", "tccutil reset All"]}})
        self.assertEqual(policy.decide(call).reason, policy.reason(policy.TCCUTIL))
        call = Call.from_payload(CODEX, {"tool_name": "Bash", "tool_input": {"command": ["bash", "-lc", "spctl --status"]}})
        self.assertEqual(policy.decide(call).decision, "allow")

    def test_nesting_too_deep_fails_closed(self):
        command = "tccutil reset All"
        for _ in range(8):
            command = "bash -c " + policy.shlex.quote(command)
        self.assertEqual(claude(command).reason, policy.reason(policy.UNCLASSIFIED))


class AllowedTests(unittest.TestCase):
    def test_reads_helpers_and_inert_mentions_are_allowed_in_both_runtimes(self):
        for command in ALLOWED:
            for run in (claude, codex):
                with self.subTest(runtime=run.__name__, command=command):
                    self.assertEqual(run(command).decision, "allow")

    def test_other_tools_are_not_judged(self):
        call = Call.from_payload(CLAUDE, {"tool_name": "Write", "tool_input": {"file_path": "/tmp/a.sh",
                                                                              "content": "tccutil reset All"}})
        self.assertEqual(policy.decide(call).decision, "allow")


class AngerLockFileToolTests(unittest.TestCase):
    def test_edit_and_write_tools_are_denied_on_the_lock(self):
        lock = "/Users/someone/.local/state/anger-lock/locked-until"
        for tool, extra in (("Write", {"content": "0"}), ("Edit", {"old_string": "1", "new_string": "0"})):
            call = Call.from_payload(CLAUDE, {"tool_name": tool, "tool_input": {"file_path": lock, **extra}})
            self.assertEqual(policy.decide(call).reason, policy.reason(policy.ANGER_LOCK), tool)

    def test_lock_reason_offers_no_command(self):
        text = policy.reason(policy.ANGER_LOCK)
        self.assertNotIn("kill-ring-put", text)
        self.assertIn("no agent may lift", text)

    def test_codex_patch_on_the_lock_is_denied(self):
        patch = ("*** Begin Patch\n*** Delete File: /Users/someone/.local/state/anger-lock/locked-until\n"
                 "*** End Patch\n")
        call = Call.from_payload(CODEX, {"tool_name": "apply_patch", "tool_input": {"command": patch}})
        self.assertEqual(policy.decide(call).decision, "deny")

    def test_edit_tools_run_the_policy(self):
        for runtime, tool in ((CLAUDE, "Edit"), (CLAUDE, "Write"), (CODEX, "apply_patch")):
            self.assertIn(policy, list(engine.policies_for(runtime, tool)), (runtime, tool))


class RegistrationTests(unittest.TestCase):
    def test_both_runtimes_run_the_policy_on_their_shell_tools(self):
        for runtime, tool in ((CLAUDE, "Bash"), (CODEX, "Bash"), (CODEX, "exec_command"), (CODEX, "functions.exec"),
                              (CODEX, "functions.exec_command")):
            self.assertIn(policy, list(engine.policies_for(runtime, tool)), (runtime, tool))


if __name__ == "__main__":
    unittest.main()
