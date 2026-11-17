import sys
import unittest
from unittest import mock
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import Call, HookFailure  # noqa: E402
from agent_guard.policies import sensitive_read  # noqa: E402

HOME = "/Users/tester"
ENV = {"HOME": HOME, "PWD": "/"}
CONTEXT = ("Sensitive environment file loading was allowed for this {} command. Do not print, inspect, summarize, "
           "or quote loaded environment values; use them only as process environment for the requested command.")


def call(runtime, tool, tool_input, **extra):
    return Call.from_payload(runtime, {"tool_name": tool, "tool_input": tool_input, "cwd": "/tmp", **extra}, env=ENV)


def decide(runtime, tool, tool_input, **extra):
    return sensitive_read.decide(call(runtime, tool, tool_input, **extra))


def bash(runtime, command, tool="Bash"):
    return decide(runtime, tool, {"command": command})


class LabelTests(unittest.TestCase):
    def test_label_order_differs_by_runtime(self):
        text = "cat .env .mcp.json .zshenv-secrets"
        self.assertEqual(sensitive_read.label_for_text("claude", text), "shell secrets file")
        self.assertEqual(sensitive_read.label_for_text("codex", text), "MCP credential config")
        self.assertEqual(sensitive_read.label_for_text("claude", "cat .env tokens.json"), "environment secrets file")
        self.assertEqual(sensitive_read.label_for_text("codex", "cat .env tokens.json"), "credential JSON")

    def test_templates_and_commit_messages_are_not_secrets(self):
        for text in ("cat .env.op", "cat app/.env.example", "cat .env.op.dev.example; ls", "git commit -m 'drop .env'"):
            self.assertEqual(sensitive_read.label_for_text("claude", text), "", text)
        self.assertEqual(sensitive_read.label_for_text("claude", "cat .env.op.bak"), "environment secrets file")

    def test_public_ssh_key_is_not_private(self):
        self.assertEqual(sensitive_read.label_for_text("claude", "cat ~/.ssh/id_rsa.pub"), "")
        self.assertEqual(sensitive_read.label_for_text("claude", "cat ~/.ssh/id_rsa"), "SSH private key")

    def test_word_boundary_follows_grep_not_python(self):
        # `²` is a word character for Python's \w but not for BSD grep's \b.
        self.assertEqual(sensitive_read.label_for_text("claude", "cat .zshenv-secrets²"), "shell secrets file")
        self.assertEqual(sensitive_read.label_for_text("claude", "cat .zshenv-secretsé"), "")
        self.assertEqual(sensitive_read.label_for_text("claude", "cat .env"), "environment secrets file")


class BashTests(unittest.TestCase):
    def test_content_read_denied_with_runtime_wording(self):
        claude = bash("claude", "cat .env")
        self.assertEqual(claude.decision, "deny")
        self.assertTrue(claude.reason.startswith(
            "BLOCKED: sensitive read: environment secrets file — Bash command appears to read or process the "
            "contents of environment secrets file.\n\nThis could expose"))
        codex = bash("codex", "cat .env", "exec_command")
        self.assertIn("— command appears to read", codex.reason)
        self.assertIn("git-crypt/security for auth-managed secrets", codex.reason)

    def test_metadata_commands_allowed_only_alone(self):
        for command in ("ls -la .env", "wc -l .env", "grep -l foo .env", "test -f .env", "git -C r ls-files .env",
                        "op read op://x/.env"):
            self.assertEqual(bash("claude", command).decision, "allow", command)
        result = bash("claude", "ls .env; cat .env")
        self.assertIn("compound Bash command cannot use a metadata-command allowance safely", result.reason)

    def test_env_loader_allowed_with_context(self):
        result = bash("claude", "source .env && npm test")
        self.assertEqual((result.decision, result.context), ("allow", CONTEXT.format("Bash")))
        self.assertEqual(bash("claude", "source .env && printenv").decision, "deny")
        result = bash("codex", "op-automations run --env-file .env -- node app.js > out.log")
        self.assertEqual(result.context, CONTEXT.format("shell"))
        # An environment dumper loses the loader allowance but still starts with an auth-aware tool.
        self.assertEqual(bash("codex", "op-automations run --env-file .env -- env").context, "")
        for command in ("OP_X=1 op-automations run --env-file .env -- x",
                        "op-automations run --env-file .env -- node $(x)", "op-automations run --env-file .env -- x >&2"):
            self.assertEqual(bash("codex", command).decision, "deny", command)

    def test_heredoc_program_naming_file_denied(self):
        result = bash("claude", "python3 - <<'EOF'\nopen('.env')\nEOF")
        self.assertIn("a program fed to an interpreter through a heredoc", result.reason)
        self.assertEqual(bash("claude", "cat <<'EOF' > notes.md\n.env\nEOF").decision, "allow")

    def test_shell_export_classifier_exact_invocation(self):
        helper = f"{HOME}/repos/dotfiles/macos/.claude/skills/security-audit/scripts/classify-shell-exports.py"
        target = f"{HOME}/repos/dotfiles/shell/.zshenv-secrets"
        self.assertEqual(bash("claude", f'python3 "{helper}" "{target}"').decision, "allow")
        self.assertEqual(bash("claude", f'python3 "{helper}" "{target}" -v').decision, "deny")


class ReadAndGrepTests(unittest.TestCase):
    def test_claude_read_globs(self):
        self.assertEqual(decide("claude", "Read", {"file_path": "~/.ssh/id_ed25519"}).reason.split(".\n")[0],
                         "BLOCKED: sensitive read: SSH private key — ~/.ssh/id_ed25519")
        self.assertEqual(decide("claude", "Read", {"file_path": "/r/.env.op"}).decision, "allow")
        self.assertEqual(decide("claude", "Read", {"file_path": "/r/.env.local"}).decision, "deny")
        self.assertEqual(decide("claude", "Read", {"file_path": f"{HOME}/.config/x/client_secret_1.json"}).reason
                         .split(" — ")[0], "BLOCKED: sensitive read: OAuth client secret")

    def test_codex_read_uses_command_regexes(self):
        self.assertEqual(decide("codex", "Read", {"path": "notes/.env"}).decision, "deny")
        self.assertEqual(decide("codex", "Read", {"file_path": "x/tokens.json"}).reason.split(" — ")[0],
                         "BLOCKED: sensitive read: credential JSON")

    def test_claude_grep_scope_and_glob(self):
        self.assertEqual(decide("claude", "Grep", {"path": "~/.ssh"}).decision, "allow")
        result = decide("claude", "Grep", {"path": "~/.ssh", "output_mode": "content"})
        self.assertIn("SSH directory — Grep content output could return matching lines from ~/.ssh", result.reason)
        result = decide("claude", "Grep", {"output_mode": "content"}, cwd=HOME)
        self.assertIn("broad directory scope containing user secrets", result.reason)
        result = decide("claude", "Grep", {"path": "/r", "output_mode": "content", "glob": "**/.env*"})
        self.assertIn("sensitive file glob — Grep content output with glob **/.env* could", result.reason)

    def test_non_object_tool_input_fails_closed_for_claude_only(self):
        with self.assertRaises(HookFailure):
            decide("claude", "Read", "x")
        self.assertEqual(decide("codex", "Bash", "cat .env").decision, "deny")


CHROME = "Library/Application Support/Google/Chrome"
STORE_READS = {
    "Codex credentials": [
        "cat ~/.codex/auth.json", "cat ~/.codex-epoch/auth.json", f"jq . {HOME}/.codex/auth.json",
        'head -c 100 "$HOME/.codex-personal/auth.json"', "rg token ~/.codex/auth.json",
        f"""python3 -c "print(open('{HOME}/.codex/auth.json').read())" """, "false && cat ~/.codex/auth.json",
        "cp ~/.codex/auth.json /tmp/a.json",
    ],
    "Slack session cache": [
        "cat ~/.cache/slack-chrome/epoch.slack.com.json", "jq -r .token ~/.cache/slack-chrome/x.json",
        "grep -r xoxc ~/.cache/slack-chrome", "ls ~/.cache/slack-chrome | xargs cat",
        f"python3 -m json.tool {HOME}/.cache/slack-chrome/x.json",
    ],
    "GitHub CLI credentials": [
        "cat ~/.config/gh/hosts.yml", "grep oauth_token ~/.config/gh/hosts.yml", "cd ~/.config && cat gh/hosts.yml",
        'yq . "$HOME/.config/gh/hosts.yml"', "sed -n 1,5p ~/.config/gh/hosts.yml",
    ],
    "Chrome cookie database": [
        f'sqlite3 "$HOME/{CHROME}/Default/Cookies" "select * from cookies"',
        "sqlite3 ~/Library/Application\\ Support/Google/Chrome/Profile\\ 2/Network/Cookies .dump",
        f'cp "$HOME/{CHROME}/Default/Network/Cookies" /tmp/c.db', f'strings "{HOME}/{CHROME}/Default/Cookies-journal"',
        f'cd "$HOME/{CHROME}/Default" && sqlite3 Cookies .dump',
        f'sqlite3 "file:{HOME}/{CHROME}/Profile 3/Network/Cookies?mode=ro" "select name from cookies"',
    ],
    "shell history": [
        "cat ~/.zsh_history", "tail -50 ~/.zsh_history", "grep -n curl ~/.bash_history", "cat ~/.python_history",
        "less ~/.psql_history", "cat ~/.node_repl_history", "cat ~/.zsh_sessions/ABC.history",
        'rg -n token "$HOME/.zsh_history"', "gitleaks detect --no-git --source ~/.zsh_history",
        "cd ~ && head .zsh_history", "sort < ~/.zsh_history | uniq -c",
    ],
}
STORE_ALLOWED = [
    # Metadata commands, alone, as for every other sensitive file.
    "ls -la ~/.codex/auth.json", "stat ~/.zsh_history", "wc -l ~/.zsh_history", "grep -c curl ~/.zsh_history",
    "test -f ~/.config/gh/hosts.yml", "ls ~/.cache/slack-chrome", f'ls -l "$HOME/{CHROME}/Default/Cookies"',
    # Programs that open a store themselves: the guard reads the command, not what the program opens.
    "python3 claude/bin/slack.py -w epoch channels | head -3",
    "python3 ~/repos/dotfiles/claude/bin/slack_chrome.py epoch.slack.com", "bin/secret-index refresh",
    "~/repos/dotfiles/bin/secret-index status", "gh auth status", "gh auth status --hostname github.com",
    "gh api user --jq .login", "codex login status",
    """python3 -c 'import slack_chrome; slack_chrome.refresh("epoch.slack.com")'""",
    # Neighbours of the stores, and inert mentions.
    "cat ~/.codex/config.toml", "ls ~/.codex-epoch/sessions", "cat ~/.config/gh/config.yml", "cat project/auth.json",
    "cat hosts.yml", "grep -rn slack_chrome claude/bin", f'ls "$HOME/{CHROME}/Default/Extensions"',
    "cat docs/zsh_history.md", "cat ~/.zshrc", "git commit -m 'guard .zsh_history and ~/.codex/auth.json' -- x.py",
    "cat <<'EOF' > notes.md\n~/.codex/auth.json and ~/.zsh_history are denied\nEOF",
]


class CredentialStoreTests(unittest.TestCase):
    def test_shell_reads_denied_in_both_runtimes(self):
        for label, commands in STORE_READS.items():
            for command in commands:
                for runtime, tool in (("claude", "Bash"), ("codex", "exec_command")):
                    with self.subTest(label=label, runtime=runtime, command=command):
                        result = bash(runtime, command, tool)
                        self.assertEqual(result.decision, "deny")
                        self.assertTrue(result.reason.startswith(f"BLOCKED: sensitive read: {label} — "), result.reason)

    def test_metadata_helpers_and_neighbours_allowed_in_both_runtimes(self):
        for command in STORE_ALLOWED:
            for runtime, tool in (("claude", "Bash"), ("codex", "exec_command")):
                with self.subTest(runtime=runtime, command=command):
                    self.assertEqual(bash(runtime, command, tool).decision, "allow")

    def test_dispatcher_copy_agrees(self):
        for command in ("cat ~/.zsh_history", "cat ~/.codex/auth.json", "ls ~/.cache/slack-chrome", "gh auth status"):
            c = call("claude", "Bash", {"command": command})
            hook = sensitive_read.decide(c)
            self.assertEqual(sensitive_read.dispatcher_check(c), ([hook.reason] if hook.reason else [], hook.context))

    def test_read_tool(self):
        denied = {
            "Codex credentials": ("~/.codex/auth.json", f"{HOME}/.codex-epoch/auth.json"),
            "Slack session cache": (f"{HOME}/.cache/slack-chrome/epoch.slack.com.json",),
            "GitHub CLI credentials": (f"{HOME}/.config/gh/hosts.yml",),
            "Chrome cookie database": (f"{HOME}/{CHROME}/Default/Cookies", f"{HOME}/{CHROME}/Profile 2/Network/Cookies",
                                       f"{HOME}/{CHROME}/Default/Network/Cookies-journal"),
            "shell history": ("~/.zsh_history", f"{HOME}/.bash_history", f"{HOME}/.python_history",
                              f"{HOME}/.psql_history", f"{HOME}/.node_repl_history", f"{HOME}/.zsh_sessions/A.history"),
        }
        for label, paths in denied.items():
            for path in paths:
                with self.subTest(path=path):
                    self.assertEqual(decide("claude", "Read", {"file_path": path}).reason.split(".\n\n")[0],
                                     f"BLOCKED: sensitive read: {label} — {path}")
                    self.assertTrue(decide("codex", "Read", {"path": path}).reason.startswith(
                        f"BLOCKED: sensitive read: {label} — "), path)
        for path in (f"{HOME}/.codex/config.toml", f"{HOME}/.config/gh/config.yml", "/r/project/auth.json",
                     f"{HOME}/{CHROME}/Default/Preferences", "/r/docs/zsh_history.md", f"{HOME}/.zshrc",
                     f"{HOME}/.cache/slack-chrome-notes.md", "/r/dotfiles/claude/bin/slack_chrome.py"):
            with self.subTest(path=path):
                self.assertEqual(decide("claude", "Read", {"file_path": path}).decision, "allow")
                self.assertEqual(decide("codex", "Read", {"path": path}).decision, "allow")

    def test_grep_tool_scopes(self):
        denied = {
            "~/.codex": "Codex credentials", "~/.codex-epoch": "Codex credentials",
            f"{HOME}/.codex/auth.json": "Codex credentials", "~/.cache/slack-chrome": "Slack session cache",
            "~/.cache": "Slack session cache", "~/.config/gh": "GitHub CLI credentials",
            f"{HOME}/.config/gh/hosts.yml": "GitHub CLI credentials",
            f"{HOME}/{CHROME}": "Chrome cookie database", f"{HOME}/{CHROME}/Default": "Chrome cookie database",
            f"{HOME}/{CHROME}/Default/Network/": "Chrome cookie database", "~/.bash_history": "shell history",
            "~/.zsh_history": "shell history", f"{HOME}/.node_repl_history": "shell history",
        }
        for path, label in denied.items():
            with self.subTest(path=path):
                result = decide("claude", "Grep", {"path": path, "output_mode": "content"})
                self.assertIn(f"{label} — Grep content output could return matching lines from {path}", result.reason)
                # Name-only and count modes return no file contents.
                self.assertEqual(decide("claude", "Grep", {"path": path}).decision, "allow")
                self.assertEqual(decide("claude", "Grep", {"path": path, "output_mode": "count"}).decision, "allow")
        for path in ("~/.codex/sessions", "~/.config", f"{HOME}/{CHROME}/Default/Extensions", "/r/dotfiles/macos/.codex",
                     "/r/project/.cache", "~/repos/dotfiles/claude/bin"):
            with self.subTest(path=path):
                self.assertEqual(decide("claude", "Grep", {"path": path, "output_mode": "content"}).decision, "allow")

    def test_grep_tool_globs(self):
        for path, glob in (("/r", "**/.bash_history"), ("/r", ".python_history"), ("~/.codex/sessions/..", "auth.json"),
                           ("~/.config", "**/hosts.yml"), ("~/Library/Application Support", "**/Cookies"),
                           ("/r", "slack-chrome/*.json")):
            with self.subTest(path=path, glob=glob):
                result = decide("claude", "Grep", {"path": path, "output_mode": "content", "glob": glob})
                self.assertIn(f"sensitive file glob — Grep content output with glob {glob} could", result.reason)
        for path, glob in (("/r/project", "auth.json"), ("/r/ansible", "hosts.yml"), ("/r/web", "**/useCookies.ts")):
            with self.subTest(path=path, glob=glob):
                self.assertEqual(decide("claude", "Grep", {"path": path, "output_mode": "content", "glob": glob}).decision,
                                 "allow")


HISTORY_SCANS = [
    "gitleaks dir ~/.zsh_history --redact",
    "/opt/homebrew/bin/gitleaks dir ~/.zsh_history ~/.bash_history ~/.python_history --redact --no-banner",
    'gitleaks dir "$HOME/.zsh_history" --redact=100 --report-path /tmp/history-scan.json --report-format json',
    "gitleaks file ~/.zsh_sessions --redact -r /tmp/sessions.json --exit-code 0",
    "gitleaks directory ~/.psql_history ~/.node_repl_history --redact --no-color --log-level warn",
    "cd /tmp/audit && gitleaks dir ~/.zsh_history --redact --no-banner -r history.json > scan.log 2> scan.err",
    "cd ~ && gitleaks dir .zsh_history .bash_history --redact",
    "gitleaks detect --no-git --source ~/.zsh_history --redact --no-banner",
    f"gitleaks dir {HOME}/.zsh_history --redact --report-format=sarif --report-path=/tmp/h.sarif",
]
HISTORY_SCAN_NEAR_MISSES = [
    # no redaction, partial redaction, or output that can show a finding
    "gitleaks dir ~/.zsh_history", "gitleaks dir ~/.zsh_history --no-banner -r /tmp/h.json",
    "gitleaks dir ~/.zsh_history --redact=20", "gitleaks dir ~/.zsh_history --redact=0",
    "gitleaks dir ~/.zsh_history --redact 50", "gitleaks dir ~/.zsh_history --redact --verbose",
    "gitleaks dir ~/.zsh_history --redact -v", "gitleaks dir ~/.zsh_history --redact --log-level debug",
    "gitleaks dir ~/.zsh_history --redact --report-format template --report-template t.tmpl",
    "gitleaks dir ~/.zsh_history --redact -r -", "gitleaks dir ~/.zsh_history --redact --report-path /dev/stdout",
    "gitleaks dir ~/.zsh_history --redact --report-path ~/.bash_history",
    "gitleaks dir ~/.zsh_history --redact > /dev/stdout", "gitleaks dir ~/.zsh_history --redact 2>&1",
    # a second path, sensitive or not
    "gitleaks dir ~/.zsh_history ~/.ssh/id_ed25519 --redact", "gitleaks dir ~/.zsh_history .env --redact",
    "gitleaks dir ~/.zsh_history ~/notes.txt --redact", "gitleaks dir ~/.codex/auth.json --redact",
    "gitleaks dir ~/.zsh_history --redact -r ~/.config/gh/hosts.yml",
    "gitleaks dir ~ --redact --report-path ~/.zsh_history", "gitleaks dir ~/.zsh_sessions/../notes --redact",
    # composition
    "gitleaks dir ~/.zsh_history --redact | cat", "gitleaks dir ~/.zsh_history --redact; cat ~/.zsh_history",
    "gitleaks dir ~/.zsh_history --redact && tail ~/.zsh_history", "cat ~/.zsh_history | gitleaks stdin --redact",
    "gitleaks stdin --redact < ~/.zsh_history", "gitleaks dir $(echo ~/.zsh_history) --redact",
    'gitleaks dir ~/.zsh_history --redact -r "$(mktemp)"', 'gitleaks dir "$H/.zsh_history" --redact',
    "false && gitleaks dir ~/.zsh_history --redact", "cd ~ && cd / && gitleaks dir ~/.zsh_history --redact",
    "cd /tmp&env && gitleaks dir ~/.zsh_history --redact", "cd -P && gitleaks dir ~/.zsh_history --redact",
    "gitleaks dir ~/.zsh_history --redact &", "gitleaks dir ~/.zsh_history --redact\ncat ~/.zsh_history",
    # another program, configuration or subcommand
    "./gitleaks dir ~/.zsh_history --redact", "/tmp/gitleaks dir ~/.zsh_history --redact",
    "sudo gitleaks dir ~/.zsh_history --redact", "GITLEAKS_CONFIG=x gitleaks dir ~/.zsh_history --redact",
    "gitleaks dir ~/.zsh_history --redact --config /tmp/rules.toml", "gitleaks git ~/.zsh_history --redact",
    "gitleaks detect --source ~/.zsh_history --redact", "gitleaks detect --no-git --redact ~/.zsh_history",
    "trufflehog filesystem ~/.zsh_history",
]


class HistoryScanTests(unittest.TestCase):
    """The one way to scan history: the security-audit skill's redacting gitleaks run."""

    def test_redacting_scan_allowed_in_both_runtimes(self):
        for command in HISTORY_SCANS:
            for runtime, tool in (("claude", "Bash"), ("codex", "exec_command")):
                with self.subTest(runtime=runtime, command=command):
                    self.assertEqual(bash(runtime, command, tool).decision, "allow")
                    c = call(runtime, tool, {"command": command})
                    if runtime == "claude":
                        self.assertEqual(sensitive_read.dispatcher_check(c), ([], ""))

    def test_near_misses_stay_denied_in_both_runtimes(self):
        for command in HISTORY_SCAN_NEAR_MISSES:
            for runtime, tool in (("claude", "Bash"), ("codex", "exec_command")):
                with self.subTest(runtime=runtime, command=command):
                    result = bash(runtime, command, tool)
                    self.assertEqual(result.decision, "deny")
                    self.assertTrue(result.reason.startswith("BLOCKED: sensitive read: "), result.reason)

    def test_other_reads_of_history_are_unchanged(self):
        for command in ("cat ~/.zsh_history", "grep -n gitleaks ~/.zsh_history", "tail -5 ~/.bash_history"):
            self.assertEqual(bash("claude", command).decision, "deny", command)
        self.assertEqual(decide("claude", "Read", {"file_path": "~/.zsh_history"}).decision, "deny")


class FailureTests(unittest.TestCase):
    """A helper failure propagates and the engine denies, as the hook's EXIT
    trap did (or, where the shell swallowed the failure and allowed, stricter)."""

    def test_helper_failures_propagate(self):
        for helper in ("mask_heredoc_bodies", "mask_git_commit_messages", "sed", "grep"):
            with self.subTest(helper=helper), mock.patch.object(sensitive_read, helper, side_effect=RuntimeError):
                with self.assertRaises(RuntimeError):
                    bash("claude", "cat notes.txt")
                with self.assertRaises(RuntimeError):
                    sensitive_read.dispatcher_check(call("claude", "Bash", {"command": "cat notes.txt"}))

    def test_unusable_pwd_falls_back_to_getcwd_as_bash_did(self):
        c = Call.from_payload("claude", {"tool_name": "Grep", "tool_input": {"output_mode": "content"}},
                              env={"HOME": HOME, "PWD": "/nonexistent/dir"})
        with mock.patch.object(sensitive_read.os, "getcwd", return_value=HOME):
            self.assertIn("broad directory scope", sensitive_read.decide(c).reason)


class DispatcherTests(unittest.TestCase):
    def test_heredoc_reason_has_no_advice(self):
        reasons, context = sensitive_read.dispatcher_check(call("claude", "Bash", {"command": "python3 <<EOF\n.env\nEOF"}))
        self.assertEqual(reasons, ["BLOCKED: sensitive read: environment secrets file — a program fed to an "
                                   "interpreter through a heredoc names this file and the guard cannot prove the "
                                   "program does not read it; write the program to a file and run that, or drop the "
                                   "mention."])

    def test_other_reasons_and_context_match_hook(self):
        for command in ("cat .env", "ls .env | x", "source .env && make", "ls"):
            c = call("claude", "Bash", {"command": command})
            hook = sensitive_read.decide(c)
            self.assertEqual(sensitive_read.dispatcher_check(c), ([hook.reason] if hook.reason else [], hook.context))


if __name__ == "__main__":
    unittest.main()
