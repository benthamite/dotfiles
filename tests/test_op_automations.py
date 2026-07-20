import json
import os
import stat
import subprocess
import tempfile
import textwrap
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WRAPPER = ROOT / "bin" / "op-automations"


def write_executable(path, content):
    path.write_text(textwrap.dedent(content))
    path.chmod(path.stat().st_mode | stat.S_IXUSR)


def run_hook(path, payload):
    return subprocess.run(
        [str(ROOT / path)],
        input=json.dumps(payload),
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )


class OpAutomationsTest(unittest.TestCase):
    def run_wrapper(self, *, inherited_token=None, pass_output="service-token\n"):
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            log_path = tmp_path / "calls.log"
            write_executable(
                tmp_path / "pass",
                f"""
                #!/usr/bin/env bash
                printf 'pass:%s\\n' "$*" >> {str(log_path)!r}
                printf %s {pass_output!r}
                """,
            )
            write_executable(
                tmp_path / "op",
                f"""
                #!/usr/bin/env bash
                printf 'op-token:%s\\n' "${{OP_SERVICE_ACCOUNT_TOKEN:-}}" >> {str(log_path)!r}
                printf 'op-args:%s\\n' "$*" >> {str(log_path)!r}
                """,
            )
            env = os.environ.copy()
            env["PATH"] = f"{tmp_path}:{env['PATH']}"
            if inherited_token is None:
                env.pop("OP_SERVICE_ACCOUNT_TOKEN", None)
            else:
                env["OP_SERVICE_ACCOUNT_TOKEN"] = inherited_token

            result = subprocess.run(
                [str(WRAPPER), "item", "list", "--vault", "Automations"],
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                env=env,
                check=False,
            )
            calls = log_path.read_text() if log_path.exists() else ""
            return result, calls

    def test_loads_service_account_once_and_passes_arguments(self):
        result, calls = self.run_wrapper()

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(calls.count("pass:"), 1)
        self.assertIn("pass:show epoch/1password-service-account-token", calls)
        self.assertIn("op-token:service-token", calls)
        self.assertIn("op-args:item list --vault Automations", calls)

    def test_reuses_inherited_service_account_without_reading_pass(self):
        result, calls = self.run_wrapper(inherited_token="already-loaded")

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn("pass:", calls)
        self.assertIn("op-token:already-loaded", calls)

    def test_refuses_to_run_op_when_bootstrap_entry_is_empty(self):
        result, calls = self.run_wrapper(pass_output="")

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("is empty", result.stderr)
        self.assertNotIn("op-args:", calls)


class RawOpGuardTest(unittest.TestCase):
    def assert_denied(self, path, payload):
        result = run_hook(path, payload)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("direct 1Password CLI command", result.stdout)

    def assert_allowed(self, path, payload):
        result = run_hook(path, payload)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn("direct 1Password CLI command", result.stdout)

    def test_claude_dispatcher_denies_raw_op(self):
        self.assert_denied(
            "claude/hooks/pretooluse-bash.sh",
            {"tool_name": "Bash", "tool_input": {"command": "op item list --vault Automations"}},
        )

    def test_claude_standalone_guard_denies_raw_op(self):
        self.assert_denied(
            "claude/hooks/block-secret-leak.sh",
            {"tool_name": "Bash", "tool_input": {"command": "op item get abc --vault Automations"}},
        )

    def test_codex_guard_denies_raw_op(self):
        self.assert_denied(
            "codex/hooks/block-secret-leak.sh",
            {"tool_name": "functions.exec_command", "tool_input": {"cmd": "op read op://Automations/Item/credential"}},
        )

    def test_codex_guard_denies_raw_op_nested_in_exec_script(self):
        self.assert_denied(
            "codex/hooks/block-secret-leak.sh",
            {
                "tool_name": "functions.exec",
                "tool_input": {
                    "input": 'await tools.exec_command({cmd: "op item list --vault Automations"});'
                },
            },
        )

    def test_guards_deny_common_raw_op_spellings(self):
        commands = (
            "  op item list --vault Automations",
            "(op item list --vault Automations)",
            "/opt/homebrew/bin/op item list --vault Automations",
            "command op item list --vault Automations",
            "env op item list --vault Automations",
            "env -u UNRELATED op item list --vault Automations",
            "FOO=bar op item list --vault Automations",
            "OP_SERVICE_ACCOUNT_TOKEN= op item list --vault Automations",
            "xargs op",
            "sudo op item list --vault Automations",
            "timeout 5 op item list --vault Automations",
            "nice op item list --vault Automations",
            "exec op item list --vault Automations",
            "eval 'op item list --vault Automations'",
            "/usr/bin/env -u OTHER op item list --vault Automations",
            "env -i op item list --vault Automations",
            "env -- op item list --vault Automations",
            "nohup op item list --vault Automations",
            "time op item list --vault Automations",
            "! op item list --vault Automations",
            "if op item list --vault Automations; then true; fi",
            "find . -exec op item list --vault Automations ;",
            "/bin/bash -c 'op item list --vault Automations'",
            "bash -lc 'op item list --vault Automations'",
            "bash -c 'op item list --vault Automations'",
            "sh -c 'op item list --vault Automations'",
            "zsh -c 'op item list --vault Automations'",
            "$(command -v op) item list --vault Automations",
        )
        paths = (
            ("claude/hooks/pretooluse-bash.sh", "Bash", "command"),
            ("codex/hooks/block-secret-leak.sh", "functions.exec_command", "cmd"),
        )
        for path, tool_name, field in paths:
            for command in commands:
                with self.subTest(path=path, command=command):
                    self.assert_denied(
                        path,
                        {"tool_name": tool_name, "tool_input": {field: command}},
                    )

    def test_codex_guard_denies_single_quoted_nested_exec_command(self):
        command = "o" + "p item list --vault Automations"
        self.assert_denied(
            "codex/hooks/block-secret-leak.sh",
            {
                "tool_name": "functions.exec",
                "tool_input": {
                    "input": "await tools.exec_command({cmd: '" + command + "'});"
                },
            },
        )

    def test_codex_guard_classifies_every_nested_exec_command(self):
        op_word = "o" + "p"
        commands = (
            f"env -u OTHER {op_word} item list --vault Automations",
            f"FOO=bar {op_word} item list --vault Automations",
            f"OP_SERVICE_ACCOUNT_TOKEN= {op_word} item list --vault Automations",
            f"xargs -n 1 {op_word}",
            f"timeout 5 {op_word} item list --vault Automations",
            f"find . -exec {op_word} item list --vault Automations ;",
            f"/bin/bash -c '{op_word} item list --vault Automations'",
            f"exec {op_word} item list --vault Automations",
            f"nice {op_word} item list --vault Automations",
        )
        quote_builders = (
            lambda command: json.dumps(command),
            lambda command: "'" + command.replace("'", "\\'") + "'",
            lambda command: "`" + command.replace("`", "\\`") + "`",
        )
        for command in commands:
            for quote_command in quote_builders:
                source = f"await tools.exec_command({{cmd: {quote_command(command)}}});"
                with self.subTest(command=command, source=source):
                    self.assert_denied(
                        "codex/hooks/block-secret-leak.sh",
                        {"tool_name": "functions.exec", "tool_input": {"input": source}},
                    )

    def test_guards_allow_promptless_wrapper(self):
        payloads = (
            ("claude/hooks/pretooluse-bash.sh", {"tool_name": "Bash", "tool_input": {"command": "op-automations item list --vault Automations"}}),
            ("codex/hooks/block-secret-leak.sh", {"tool_name": "functions.exec", "tool_input": {"input": 'await tools.exec_command({cmd: "op-automations item list --vault Automations"});'}}),
        )
        for path, payload in payloads:
            with self.subTest(path=path):
                self.assert_allowed(path, payload)

    def test_guards_allow_quoted_documentation(self):
        commands = (
            "printf '(op item)'",
            "git commit -m 'docs; op item'",
            "printf '| op item'",
            "rg '; op item' file",
        )
        paths = (
            ("claude/hooks/pretooluse-bash.sh", "Bash", "command"),
            ("codex/hooks/block-secret-leak.sh", "functions.exec_command", "cmd"),
        )
        for path, tool_name, field in paths:
            for command in commands:
                with self.subTest(path=path, command=command):
                    self.assert_allowed(
                        path,
                        {"tool_name": tool_name, "tool_input": {field: command}},
                    )

    def test_guards_allow_explicit_desktop_auth(self):
        command = "env -u OP_SERVICE_ACCOUNT_TOKEN op item get abc --vault Employee"
        payloads = (
            ("claude/hooks/pretooluse-bash.sh", {"tool_name": "Bash", "tool_input": {"command": command}}),
            ("codex/hooks/block-secret-leak.sh", {"tool_name": "functions.exec_command", "tool_input": {"cmd": command}}),
        )
        for path, payload in payloads:
            with self.subTest(path=path):
                self.assert_allowed(path, payload)

    def test_guards_allow_one_explicit_desktop_batch(self):
        op_word = "o" + "p"
        command = (
            "env -u OP_SERVICE_ACCOUNT_TOKEN bash -lc '"
            f"{op_word} whoami >/dev/null 2>&1 || {op_word} signin >/dev/null; "
            f"{op_word} item edit abc --vault=Automations --title=Example'"
        )
        payloads = (
            ("claude/hooks/pretooluse-bash.sh", {"tool_name": "Bash", "tool_input": {"command": command}}),
            ("claude/hooks/block-secret-leak.sh", {"tool_name": "Bash", "tool_input": {"command": command}}),
            ("codex/hooks/block-secret-leak.sh", {"tool_name": "functions.exec_command", "tool_input": {"cmd": command}}),
        )
        for path, payload in payloads:
            with self.subTest(path=path):
                self.assert_allowed(path, payload)

    def test_guards_deny_raw_op_after_explicit_desktop_batch(self):
        op_word = "o" + "p"
        command = (
            "env -u OP_SERVICE_ACCOUNT_TOKEN bash -lc '"
            f"{op_word} whoami >/dev/null || {op_word} item edit abc'"
            f"; {op_word} item list --vault Automations"
        )
        paths = (
            ("claude/hooks/pretooluse-bash.sh", "Bash", "command"),
            ("claude/hooks/block-secret-leak.sh", "Bash", "command"),
            ("codex/hooks/block-secret-leak.sh", "functions.exec_command", "cmd"),
        )
        for path, tool_name, field in paths:
            with self.subTest(path=path):
                self.assert_denied(path, {"tool_name": tool_name, "tool_input": {field: command}})

    def test_guards_deny_reveal_through_sanctioned_forms(self):
        commands = (
            "op-automations item get abc --reveal",
            "op-automations item get abc --reveal 2>/dev/null",
            "env -u OP_SERVICE_ACCOUNT_TOKEN op item get abc --reveal",
            "env -u OP_SERVICE_ACCOUNT_TOKEN bash -lc 'op whoami || op item get abc --reveal'",
        )
        paths = (
            ("claude/hooks/pretooluse-bash.sh", "Bash", "command"),
            ("codex/hooks/block-secret-leak.sh", "functions.exec_command", "cmd"),
        )
        for path, tool_name, field in paths:
            for command in commands:
                with self.subTest(path=path, command=command):
                    result = run_hook(
                        path,
                        {"tool_name": tool_name, "tool_input": {field: command}},
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertIn("revealed 1Password field", result.stdout)

    def test_codex_guard_denies_nested_reveal_through_sanctioned_forms(self):
        op_word = "o" + "p"
        commands = (
            f"{op_word}-automations item get abc --reveal",
            f"{op_word}-automations item get abc --reveal 2>/dev/null",
            f"env -u OP_SERVICE_ACCOUNT_TOKEN {op_word} item get abc --reveal",
        )
        for command in commands:
            source = f"await tools.exec_command({{cmd: {json.dumps(command)}}});"
            with self.subTest(command=command):
                result = run_hook(
                    "codex/hooks/block-secret-leak.sh",
                    {"tool_name": "functions.exec", "tool_input": {"input": source}},
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn("revealed 1Password field", result.stdout)


if __name__ == "__main__":
    unittest.main()
