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
    def run_wrapper(self, *, inherited_token=None,
                    prefix=(), keychain_output="keychain-token", keychain_status=0):
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            log_path = tmp_path / "calls.log"
            write_executable(
                tmp_path / "security",
                f"""
                #!/usr/bin/env bash
                printf 'keychain:%s\\n' "$*" >> {str(log_path)!r}
                printf %s {keychain_output!r}
                exit {keychain_status}
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
                [str(WRAPPER), *prefix, "item", "list", "--vault", "Automations"],
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                env=env,
                check=False,
            )
            calls = log_path.read_text() if log_path.exists() else ""
            return result, calls

    def test_loads_epoch_token_from_keychain_once_and_passes_arguments(self):
        result, calls = self.run_wrapper()

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(calls.count("keychain:"), 1)
        self.assertIn("-s op-service-account/epoch-automation -w", calls)
        self.assertIn("op-token:keychain-token", calls)
        self.assertIn("op-args:item list --vault Automations", calls)

    def test_reuses_inherited_service_account_without_reading_keychain(self):
        result, calls = self.run_wrapper(inherited_token="already-loaded")

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn("keychain:", calls)
        self.assertIn("op-token:already-loaded", calls)

    def test_refuses_to_run_op_when_epoch_keychain_token_is_missing(self):
        result, calls = self.run_wrapper(keychain_output="", keychain_status=44)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("op-service-account/epoch-automation", result.stderr)
        self.assertNotIn("op-args:", calls)

    def test_account_selector_reads_keychain_and_ignores_inherited_token(self):
        result, calls = self.run_wrapper(prefix=("@personal",), inherited_token="epoch-token")

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("-s op-service-account/personal-automation -w", calls)
        self.assertIn("op-token:keychain-token", calls)
        self.assertIn("op-args:item list --vault Automations", calls)

    def test_account_selector_refuses_to_run_op_without_keychain_token(self):
        result, calls = self.run_wrapper(prefix=("@tlon",), keychain_output="", keychain_status=44)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("op-service-account/tlon-automation", result.stderr)
        self.assertNotIn("op-args:", calls)

    def test_unknown_account_selector_is_rejected(self):
        result, calls = self.run_wrapper(prefix=("@nosuch",))

        self.assertEqual(result.returncode, 2)
        self.assertNotIn("op-args:", calls)


class OpAutomationsCacheTest(unittest.TestCase):
    REF = "op://Automation/item/credential"

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        tmp_path = Path(self.tmp.name)
        self.log_path = tmp_path / "calls.log"
        self.status_path = tmp_path / "op-status"
        self.status_path.write_text("0")
        self.state = tmp_path / "state"
        bin_dir = tmp_path / "bin"
        bin_dir.mkdir()
        write_executable(bin_dir / "security", "#!/usr/bin/env bash\nprintf keychain-token\n")
        write_executable(bin_dir / "tmutil", f"#!/usr/bin/env bash\nprintf 'tmutil:%s\\n' \"$*\" >> {str(self.log_path)!r}\n")
        write_executable(
            bin_dir / "op",
            f"""
            #!/usr/bin/env bash
            printf 'op-args:%s\\n' "$*" >> {str(self.log_path)!r}
            status=$(cat {str(self.status_path)!r})
            if [[ "$status" != 0 ]]; then
              printf '[ERROR] Too many requests\\n' >&2
              exit "$status"
            fi
            printf 'value-for-%s\\n' "${{OP_SERVICE_ACCOUNT_TOKEN:-}}"
            """,
        )
        self.env = os.environ.copy()
        self.env["PATH"] = f"{bin_dir}:{self.env['PATH']}"
        self.env["XDG_STATE_HOME"] = str(self.state)
        self.env.pop("OP_SERVICE_ACCOUNT_TOKEN", None)

    def broker(self, *args):
        return subprocess.run(
            [str(WRAPPER), *args], text=True, capture_output=True, env=self.env, check=False
        )

    def op_calls(self):
        text = self.log_path.read_text() if self.log_path.exists() else ""
        return [line for line in text.splitlines() if line.startswith("op-args:")]

    def cache_dir(self):
        return self.state / "op-automations" / "cache"

    def entries(self):
        return sorted(p for p in self.cache_dir().iterdir() if not p.name.endswith(".failed"))

    def test_miss_reads_once_and_hit_serves_private_file_without_op(self):
        first = self.broker("@personal", "cache", "read", self.REF)
        second = self.broker("@personal", "cache", "read", self.REF)

        self.assertEqual(first.returncode, 0, first.stderr)
        self.assertEqual(first.stdout, "value-for-keychain-token\n")
        self.assertEqual(second.stdout, first.stdout)
        self.assertEqual(self.op_calls(), [f"op-args:read {self.REF}"])
        self.assertEqual(stat.S_IMODE(self.cache_dir().stat().st_mode), 0o700)
        [entry] = self.entries()
        self.assertEqual(stat.S_IMODE(entry.stat().st_mode), 0o600)
        self.assertNotIn(self.REF, entry.name)
        self.assertEqual([p.name for p in self.cache_dir().iterdir() if p.name.startswith(".")], [])
        self.assertIn("tmutil:addexclusion", self.log_path.read_text())

    def test_expired_entry_is_replaced_by_one_fresh_read(self):
        self.broker("cache", "read", self.REF)
        [entry] = self.entries()
        old = entry.stat().st_mtime - 7200
        os.utime(entry, (old, old))

        result = self.broker("cache", "read", "--ttl", "1h", self.REF)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(self.op_calls()), 2)
        self.assertGreater(entry.stat().st_mtime, old)

    def test_ttl_is_honoured_for_younger_entries(self):
        self.broker("cache", "read", self.REF)
        [entry] = self.entries()
        old = entry.stat().st_mtime - 1800
        os.utime(entry, (old, old))

        self.broker("cache", "read", "--ttl=1h", self.REF)

        self.assertEqual(len(self.op_calls()), 1)

    def test_forget_forces_exactly_one_fresh_read(self):
        self.broker("cache", "read", self.REF)
        forget = self.broker("cache", "forget", self.REF)
        self.assertEqual((forget.returncode, forget.stdout), (0, ""))
        self.assertEqual(self.entries(), [])

        self.broker("cache", "read", self.REF)
        self.broker("cache", "read", self.REF)

        self.assertEqual(len(self.op_calls()), 2)

    def test_failed_read_is_not_cached_and_backs_off_until_forgotten(self):
        self.status_path.write_text("1")
        failed = self.broker("@personal", "cache", "read", self.REF)
        self.assertEqual(failed.returncode, 1)
        self.assertEqual(failed.stdout, "")
        self.assertIn("Too many requests", failed.stderr)
        self.assertEqual(self.entries(), [])

        self.status_path.write_text("0")
        backing_off = self.broker("@personal", "cache", "read", self.REF)
        self.assertEqual(backing_off.returncode, 1)
        self.assertIn("not retrying", backing_off.stderr)
        self.assertEqual(len(self.op_calls()), 1)

        self.broker("@personal", "cache", "forget", self.REF)
        recovered = self.broker("@personal", "cache", "read", self.REF)
        self.assertEqual(recovered.returncode, 0, recovered.stderr)
        self.assertEqual(len(self.op_calls()), 2)

    def test_backoff_expires(self):
        self.status_path.write_text("1")
        self.broker("cache", "read", self.REF)
        [marker] = self.cache_dir().glob("*.failed")
        old = marker.stat().st_mtime - 601
        os.utime(marker, (old, old))
        self.status_path.write_text("0")

        result = self.broker("cache", "read", self.REF)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(list(self.cache_dir().glob("*.failed")), [])

    def test_accounts_have_separate_entries(self):
        self.broker("@personal", "cache", "read", self.REF)
        self.broker("@tlon", "cache", "read", self.REF)
        self.broker("@personal", "cache", "forget", self.REF)

        self.assertEqual(len(self.entries()), 1)
        self.assertEqual(len(self.op_calls()), 2)

    def test_malformed_requests_are_rejected_without_op(self):
        for args in (
            ("cache", "read", "Automation/item/credential"),
            ("cache", "read", "--ttl", "soon", self.REF),
            ("cache", "read", self.REF, "extra"),
            ("cache", "forget", "--ttl", "1h", self.REF),
            ("cache", "purge", self.REF),
            ("cache",),
        ):
            with self.subTest(args=args):
                result = self.broker(*args)
                self.assertEqual(result.returncode, 2)
                self.assertEqual(result.stdout, "")
        self.assertEqual(self.op_calls(), [])


class RawOpGuardTest(unittest.TestCase):
    def assert_denied(self, path, payload):
        result = run_hook(path, payload)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('"permissionDecision": "deny"', result.stdout)

    def assert_allowed(self, path, payload):
        result = run_hook(path, payload)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn('"permissionDecision": "deny"', result.stdout)

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

    def test_guards_deny_promptless_wrapper_output(self):
        payloads = (
            ("claude/hooks/pretooluse-bash.sh", {"tool_name": "Bash", "tool_input": {"command": "op-automations item list --vault Automations"}}),
            ("codex/hooks/block-secret-leak.sh", {"tool_name": "functions.exec", "tool_input": {"input": 'await tools.exec_command({cmd: "op-automations item list --vault Automations"});'}}),
        )
        for path, payload in payloads:
            with self.subTest(path=path):
                result = run_hook(path, payload)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn('"permissionDecision": "deny"', result.stdout)
                self.assertIn("1Password secret", result.stdout)

    def test_guards_allow_masked_op_run(self):
        payloads = (
            ("claude/hooks/pretooluse-bash.sh", {"tool_name": "Bash", "tool_input": {"command": "op-automations run --env-file=.env.op -- true"}}),
            ("claude/hooks/block-secret-leak.sh", {"tool_name": "Bash", "tool_input": {"command": "op-automations run --env-file .env.op -- python3 sync.py --flag 2> /tmp/err.log"}}),
            ("codex/hooks/block-secret-leak.sh", {"tool_name": "functions.exec", "tool_input": {"input": 'await tools.exec_command({cmd: "op-automations run --env-file=.env.op -- true"});'}}),
            ("codex/hooks/block-secret-leak.sh", {"tool_name": "functions.exec_command", "tool_input": {"cmd": "op-automations run --env-file=.env.op -- python3 sync.py"}}),
        )
        for path, payload in payloads:
            with self.subTest(path=path):
                self.assert_allowed(path, payload)

    def test_codex_guard_denies_broker_named_outside_literal_exec_commands(self):
        source = 'const tool = "op-automations"; await tools.exec_command({cmd: tool + " read op://Automations/X/credential"});'
        self.assert_denied(
            "codex/hooks/block-secret-leak.sh",
            {"tool_name": "functions.exec", "tool_input": {"input": source}},
        )

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

    def test_guards_deny_unbatched_desktop_auth(self):
        # Raw desktop authentication is denied everywhere; desktop-gated work
        # must use the persistent op-desktop broker.
        command = "env -u OP_SERVICE_ACCOUNT_TOKEN op item get abc --vault Employee"
        payloads = (
            ("claude/hooks/pretooluse-bash.sh", {"tool_name": "Bash", "tool_input": {"command": command}}),
            ("claude/hooks/block-secret-leak.sh", {"tool_name": "Bash", "tool_input": {"command": command}}),
            ("codex/hooks/block-secret-leak.sh", {"tool_name": "functions.exec_command", "tool_input": {"cmd": command}}),
        )
        for path, payload in payloads:
            with self.subTest(path=path):
                self.assert_denied(path, payload)

    def test_guards_deny_old_explicit_desktop_batch(self):
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
                self.assert_denied(path, payload)

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
                    self.assertIn('"permissionDecision": "deny"', result.stdout)

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
                self.assertIn('"permissionDecision": "deny"', result.stdout)

    def test_codex_guard_denies_nested_secret_output_commands(self):
        commands = (
            "pbpaste",
            "op-automations read op://Automations/Example/credential",
            "op-automations run --env-file=.env.op -- printenv SECRET",
            "op-automations inject --in-file=.env.op",
            "op-desktop item list --format=json | jq .",
            "op-desktop document get abc",
            "op-desktop item share abc",
        )
        for command in commands:
            source = f"await tools.exec_command({{cmd: {json.dumps(command)}}});"
            with self.subTest(command=command):
                result = run_hook(
                    "codex/hooks/block-secret-leak.sh",
                    {"tool_name": "functions.exec", "tool_input": {"input": source}},
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn('"permissionDecision": "deny"', result.stdout)


if __name__ == "__main__":
    unittest.main()
