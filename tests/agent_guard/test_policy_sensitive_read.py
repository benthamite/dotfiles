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
        helper = f"{HOME}/My Drive/dotfiles/macos/.claude/skills/security-audit/scripts/classify-shell-exports.py"
        target = f"{HOME}/My Drive/dotfiles/shell/.zshenv-secrets"
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
