import json
import os
import shlex
import subprocess
import unittest
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DISPATCHER = ROOT / "claude/hooks/pretooluse-bash.sh"

DIFFERENTIAL_COMMANDS = (
    "pwd",
    "git status --short",
    "op item get Example --reveal",
    "cat ~/.zshenv-secrets",
    "cat mcp.json",
    "cat .env.local",
    "cat ~/.ssh/id_ed25519",
    "cat ~/.gnupg/private-keys-v1.d/key",
    "cat ~/.config/tool/tokens.json",
    "cat ~/.gmail-mcp-epoch/credentials/key.json",
    "cat ~/.config/tool/secret.json",
    "cat credentials.json",
    "git reset --hard",
    "git push --force origin main",
    "git clone https://example.com/example/repo.git",
    "git clean -fd",
    "git checkout -- .",
    "git branch -D topic",
    "gh repo delete owner/repo",
    "dropdb example",
    "bq rm dataset.table",
    "aws s3 rm s3://example --recursive",
    "op item delete Example",
    "printenv PATH",
    "printf '%s\\n' 'git reset --hard'",
    "printf 'first\\nsecond\\n'; cat .env.local",
)

# Each tuple names one planned prefilter call site, its exact producer and input
# variable, the unchanged BSD grep regex, valid necessary literals, and a
# matching witness.
PREFILTER_SITES = (
    ("op find exec", "printf", "OP_SCAN", r"find[[:space:]].*-exec[[:space:]]+(/opt/homebrew/bin/|/usr/local/bin/|/usr/bin/)?op([[:space:]]+|$)", ("op",), "find . -exec op "),
    ("op command lookup", "printf", "OP_SCAN", r"\$\([[:space:]]*command[[:space:]]+-v[[:space:]]+op[[:space:]]*\)", ("op",), "$(command -v op)"),
    ("shell secrets", "echo", "COMMAND", r"\.zshenv-secrets\b", (".zshenv-secrets",), "cat ~/.zshenv-secrets"),
    ("password store", "echo", "COMMAND", r"\.password-store/", (".password-store/",), "cat ~/.password-store/example"),
    ("mcp config", "echo", "COMMAND", r"(^|[[:space:]/])\.mcp\.json\b|(^|[[:space:]/])mcp\.json\b", (".mcp.json", "mcp.json"), "cat ~/.mcp.json"),
    ("environment file", "echo", "COMMAND", r"""(^|[[:space:]/])\.env([.[:space:]"';&|)]|$)|(^|[[:space:]/])\.envrc\b""", (".env",), "cat .env.local"),
    ("ssh private key", "echo", "COMMAND", r"(^|[ /=])\.ssh/id_[A-Za-z0-9_]+\b", (".ssh/id_",), "cat ~/.ssh/id_ed25519"),
    ("gpg keyring", "echo", "COMMAND", r"\.gnupg/", (".gnupg/",), "cat ~/.gnupg/private-keys-v1.d/key"),
    ("oauth tokens", "echo", "COMMAND", r"(^|[[:space:]])?/?[^[:space:]]*\.config/[^/[:space:]]+/tokens\.json\b", ("tokens.json",), "cat ~/.config/tool/tokens.json"),
    ("gmail credentials", "echo", "COMMAND", r"(^|[[:space:]])?/?[^[:space:]]*\.gmail-mcp-epoch/credentials/", (".gmail-mcp-epoch/credentials/",), "cat ~/.gmail-mcp-epoch/credentials/key.json"),
    ("oauth client secret", "echo", "COMMAND", r"""(^|[[:space:]])?/?[^[:space:]]*\.config/[^/[:space:]]+/(secret\.json|client_secret[^"[:space:]]*\.json)\b""", ("secret.json", "client_secret"), "cat ~/.config/tool/secret.json"),
    ("credential json", "echo", "COMMAND", r"(^|[[:space:]/])(credentials\.json|service-account[^/[:space:]]*\.json|tokens\.json)\b", ("credentials.json", "service-account", "tokens.json"), "cat credentials.json"),
    ("recursive rm", "echo", "SCAN", r"\brm\s+(-[a-zA-Z]*r[a-zA-Z]*f|(-[a-zA-Z]*f[a-zA-Z]*r)|-rf|-fr)\b", ("rm",), "rm -rf example"),
    ("force push", "echo", "SCAN", r"\bgit\s+push[^|;&]*(\s-f\b|\s--force\b)", ("push",), "git push --force origin main"),
    ("git clone", "echo", "SCAN", r"\b(git\s+clone|gh\s+repo\s+clone)\b", ("clone",), "git clone https://example.com/example/repo.git"),
    ("hard reset", "echo", "SCAN", r"\bgit\s+reset\s+--hard\b", ("reset",), "git reset --hard"),
    ("git clean", "echo", "SCAN", r"\bgit\s+clean\s+.*-[a-zA-Z]*f", ("clean",), "git clean -fd"),
    ("checkout all", "echo", "SCAN", r"\bgit\s+checkout\s+--\s+\.", ("checkout",), "git checkout -- ."),
    ("force branch delete", "echo", "SCAN", r"\bgit\s+branch\s+-D\b", ("branch",), "git branch -D topic"),
    ("visibility change", "echo", "SCAN", r"\bgh\s+api\s+.*repos/.*visibility|gh\s+repo\s+edit\s+.*--visibility", ("visibility",), "gh repo edit owner/repo --visibility private"),
    ("repo delete", "echo", "SCAN", r"\bgh\s+repo\s+delete\b", ("delete",), "gh repo delete owner/repo"),
    ("dropdb", "echo", "SCAN", r"\bdropdb\b", ("dropdb",), "dropdb example"),
    ("bigquery rm", "echo", "SCAN", r"\bbq\s+rm\b", ("bq",), "bq rm dataset.table"),
    ("recursive s3 rm", "echo", "SCAN", r"\baws\s+s3\s+rm[^|;&]*(\s--recursive\b|\s-r\b)", ("aws",), "aws s3 rm s3://example --recursive"),
    ("1password item delete", "echo", "SCAN", r"\bop\s+item\s+(delete|remove)\b", ("op",), "op item delete Example"),
)

PREFILTER_BRANCH_WITNESSES = {
    "mcp config": (
        ("cat ~/.mcp.json", (".mcp.json",)),
        ("cat mcp.json", ("mcp.json",)),
    ),
    "oauth client secret": (
        ("cat ~/.config/tool/secret.json", ("secret.json",)),
        ("cat ~/.config/tool/client_secret_example.json", ("client_secret",)),
    ),
    "credential json": (
        ("cat credentials.json", ("credentials.json",)),
        ("cat service-account-example.json", ("service-account",)),
        ("cat tokens.json", ("tokens.json",)),
    ),
}


def run_hook(script, payload, *, env=None, trace=False):
    command = [str(ROOT / "claude" / "hooks" / script)]
    if trace:
        command = ["bash", "-x", *command]
    return subprocess.run(
        command,
        input=json.dumps(payload),
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
        env={**os.environ, "CLAUDE_BASH_PREFILTERS": "1", **(env or {})},
    )


class ClaudeBashPreToolUseTest(unittest.TestCase):
    def test_prefilters_skip_at_least_twenty_grep_processes_for_pwd(self):
        payload = {"tool_name": "Bash", "tool_input": {"command": "pwd"}}
        enabled = run_hook("pretooluse-bash.sh", payload, trace=True)
        disabled = run_hook(
            "pretooluse-bash.sh",
            payload,
            env={"CLAUDE_BASH_PREFILTERS": "0"},
            trace=True,
        )
        self.assertEqual(enabled.returncode, 0, enabled.stderr)
        self.assertEqual(disabled.returncode, 0, disabled.stderr)

        enabled_greps = sum(
            line.startswith("+ grep -qE") for line in enabled.stderr.splitlines()
        )
        disabled_greps = sum(
            line.startswith("+ grep -qE") for line in disabled.stderr.splitlines()
        )
        self.assertGreaterEqual(disabled_greps - enabled_greps, 20)

    def test_prefilters_preserve_exact_dispatcher_results(self):
        for command in DIFFERENTIAL_COMMANDS:
            with self.subTest(command=command):
                payload = {"tool_name": "Bash", "tool_input": {"command": command}}
                enabled = run_hook("pretooluse-bash.sh", payload)
                disabled = run_hook(
                    "pretooluse-bash.sh",
                    payload,
                    env={"CLAUDE_BASH_PREFILTERS": "0"},
                )
                self.assertEqual(enabled.returncode, disabled.returncode)
                self.assertEqual(enabled.stdout, disabled.stdout)
                self.assertEqual(enabled.stderr, disabled.stderr)

    def test_prefilter_calls_keep_regex_and_literals_adjacent(self):
        source = DISPATCHER.read_text()
        self.assertTrue(
            'grep -qE -- "$regex"' in source,
            "prefilter_grep must keep the original grep -qE call authoritative",
        )
        call_lines = [
            line
            for line in source.splitlines()
            if "prefilter_grep " in line and not line.lstrip().startswith("#")
        ]
        self.assertEqual(source.count("prefilter_grep "), len(call_lines))

        actual_calls = []
        for line in call_lines:
            lexer = shlex.shlex(line, posix=True, punctuation_chars=";&|")
            lexer.whitespace_split = True
            lexer.commenters = ""
            words = list(lexer)
            start = words.index("prefilter_grep")
            end = next(
                (
                    index
                    for index in range(start + 1, len(words))
                    if words[index] in {";", "&&", "||", "|"}
                ),
                len(words),
            )
            actual_calls.append(tuple(words[start:end]))

        expected_calls = [
            ("prefilter_grep", producer, f"${variable}", regex, *literals)
            for _name, producer, variable, regex, literals, _witness in PREFILTER_SITES
        ]
        self.assertCountEqual(actual_calls, expected_calls)

    def test_prefilter_site_witnesses_and_necessary_literals(self):
        cases = []
        for name, _producer, _variable, regex, literals, witness in PREFILTER_SITES:
            branch_witnesses = PREFILTER_BRANCH_WITNESSES.get(
                name, ((witness, literals),)
            )
            for branch_witness, required_literals in branch_witnesses:
                variants = (
                    branch_witness,
                    f"  {branch_witness}",
                    f"{branch_witness} trailing",
                    f"prefix\n{branch_witness}",
                    f"true; {branch_witness}",
                    f"true | {branch_witness}",
                    f"true && {branch_witness}",
                )
                for variant in variants:
                    with self.subTest(site=name, variant=variant):
                        match = subprocess.run(
                            ["/usr/bin/grep", "-qE", regex],
                            input=variant,
                            text=True,
                            check=False,
                        )
                        self.assertEqual(match.returncode, 0)
                        self.assertTrue(
                            any(literal in variant for literal in required_literals)
                        )
                        cases.append((name, variant))

        def compare_dispatcher_modes(case):
            _name, command = case
            payload = {"tool_name": "Bash", "tool_input": {"command": command}}
            enabled = run_hook("pretooluse-bash.sh", payload)
            disabled = run_hook(
                "pretooluse-bash.sh",
                payload,
                env={"CLAUDE_BASH_PREFILTERS": "0"},
            )
            return enabled, disabled

        with ThreadPoolExecutor(max_workers=8) as executor:
            results = executor.map(compare_dispatcher_modes, cases)
            for (name, variant), (enabled, disabled) in zip(cases, results):
                with self.subTest(site=name, variant=variant):
                    self.assertEqual(enabled.returncode, disabled.returncode)
                    self.assertEqual(enabled.stdout, disabled.stdout)
                    self.assertEqual(enabled.stderr, disabled.stderr)

    def assert_rewrite_preserves_bash_input_fields(self, script):
        payload = {
            "tool_name": "Bash",
            "tool_input": {
                "command": "printenv",
                "timeout": 600000,
                "run_in_background": True,
                "description": "long local eval",
            },
        }
        result = run_hook(script, payload)

        self.assertEqual(result.returncode, 0, result.stderr)
        output = json.loads(result.stdout)
        updated_input = output["hookSpecificOutput"]["updatedInput"]

        self.assertIn("redact-secrets.sh", updated_input["command"])
        self.assertEqual(updated_input["timeout"], 600000)
        self.assertIs(updated_input["run_in_background"], True)
        self.assertEqual(updated_input["description"], "long local eval")

    def test_command_rewrite_preserves_bash_input_fields(self):
        self.assert_rewrite_preserves_bash_input_fields("pretooluse-bash.sh")

    def test_standalone_wrap_rewrite_preserves_bash_input_fields(self):
        self.assert_rewrite_preserves_bash_input_fields("wrap-bash-output.sh")

    def assert_benign_command_is_not_rewritten(self, script):
        payload = {
            "tool_name": "Bash",
            "tool_input": {"command": "git status --short"},
        }
        result = run_hook(script, payload)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")

    def test_dispatcher_leaves_benign_command_unchanged(self):
        self.assert_benign_command_is_not_rewritten("pretooluse-bash.sh")

    def test_standalone_wrap_leaves_benign_command_unchanged(self):
        self.assert_benign_command_is_not_rewritten("wrap-bash-output.sh")


if __name__ == "__main__":
    unittest.main()
