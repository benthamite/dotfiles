"""Shell function bodies and Google OAuth credentials never reach agent output.

On 2026-09-30 `which mbsync-passcmd gmail-maildir-sync` printed a zsh function
whose body held a literal Google OAuth client secret and refresh token, and the
output redactor recognized neither shape. This covers the three layers added
in response: lib-function-body-policy.py (deny printing a known function's
body, both runtimes), the Google OAuth shapes in the input guards and in
redact-secrets.sh, and wrapping of shell introspection for Claude.

Credential-shaped values are built at runtime so this file holds no
secret-shaped literal. A throwaway ZDOTDIR defines the fixture function, so
the classifier's zsh probe never sees the real shell configuration.
"""

import contextlib
import importlib.machinery
import importlib.util
import io
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parent))
from guard_harness import run_guard as run_hook  # noqa: E402

from agent_guard import classifiers  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
POLICIES = [ROOT / "claude/hooks/lib-function-body-policy.py"]  # the engine loads this copy
REDACTOR = ROOT / "claude" / "hooks" / "redact-secrets.sh"

CLIENT_SECRET = "GOCSPX" + "-" + "Fx7_" * 7
REFRESH_TOKEN = "1/" + "/0g" + "Rt9-" * 20
ACCESS_TOKEN = "ya29" + "." + "Ac5_" * 10
FUNCTION = "synthfn"


def fixture_env(directory):
    zdotdir = Path(directory)
    (zdotdir / ".zshenv").write_text(f"{FUNCTION}() {{ print fixture; }}\n")
    return dict(os.environ, ZDOTDIR=str(zdotdir))


def load_policy(path):
    """One copy of the classifier as its own module, so both copies are exercised."""
    name = f"function_body_policy_{path.parent.parent.name}"
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    module = importlib.util.module_from_spec(importlib.util.spec_from_loader(name, loader))
    flag = sys.dont_write_bytecode
    loader.exec_module(module)
    sys.dont_write_bytecode = flag
    return module


MODULES = {policy: load_policy(policy) for policy in POLICIES}


def classify(policy, stdin, env):
    """The decision the classifier's stdin/stdout entry point prints for STDIN."""
    out = io.StringIO()
    with mock.patch.dict(os.environ, env, clear=True), mock.patch.object(sys, "stdin", io.StringIO(stdin)), \
            contextlib.redirect_stdout(out):
        MODULES[policy].main()
    return json.loads(out.getvalue())["decision"]


def run_guard(path, tool, command, env):
    if tool == "functions.exec":
        tool_input = {"input": "text(await tools.exec_command(" + json.dumps({"cmd": command}) + "));"}
    else:
        tool_input = {"command" if tool == "Bash" else "cmd": command}
    payload = {"tool_name": tool, "tool_input": tool_input, "cwd": str(ROOT)}
    result = run_hook(path, payload, cwd=ROOT, env=env)
    if result.returncode != 0:
        raise AssertionError(result.stderr)
    return json.loads(result.stdout) if result.stdout.strip() else {}


def decision(output):
    return output.get("hookSpecificOutput", {}).get("permissionDecision", "allow")


GUARD_ROUTES = [
    (ROOT / "claude/hooks/block-secret-leak.sh", "Bash"),
    (ROOT / "claude/hooks/pretooluse-bash.sh", "Bash"),
    (ROOT / "codex/hooks/block-secret-leak.sh", "functions.exec_command"),
    (ROOT / "codex/hooks/block-secret-leak.sh", "functions.exec"),
]


class FunctionBodyPolicyTest(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory(prefix="function-body-")
        self.addCleanup(temp.cleanup)
        self.env = fixture_env(temp.name)

    def test_cases(self):
        cases = {
            # The incident shape, and every way to reach it.
            f"which mbsync-passcmd {FUNCTION}": "deny",
            f"which -a ls {FUNCTION}": "deny",
            f"where {FUNCTION}": "deny",
            f"whence -c {FUNCTION}": "deny",
            f"whence -f {FUNCTION}": "deny",
            f"type {FUNCTION}": "deny",
            f"sudo which {FUNCTION}": "deny",
            f"env FOO=1 command which {FUNCTION}": "deny",
            f"ls; builtin which {FUNCTION}": "deny",
            f"x=$(which {FUNCTION})": "deny",
            f"zsh -c 'which {FUNCTION}'": "deny",
            f"eval which {FUNCTION}": "deny",
            f"cat <(which {FUNCTION})": "deny",
            f"zsh <<'EOF'\nwhich {FUNCTION}\nEOF": "deny",
            # Dumpers that print bodies by design.
            "functions": "deny",
            "functions -m 'g*'": "deny",
            f"typeset -f {FUNCTION}": "deny",
            "declare -f": "deny",
            f"print -r -- ${{functions[{FUNCTION}]}}": "deny",
            "print -l ${(kv)functions}": "deny",
            'git commit -m "list $functions"': "deny",
            "zsh -c 'print $functions'": "deny",
            # Name-only and path lookups stay available.
            "which -a python3 ls": "allow",
            f"whence -w {FUNCTION}": "allow",
            f"whence {FUNCTION}": "allow",
            f"type -w {FUNCTION}": "allow",
            f"command -v {FUNCTION}": "allow",
            "functions +": "allow",
            "typeset +f": "allow",
            "print -l ${(k)functions}": "allow",
            "export FOO=1": "allow",
            # Mentions are data.
            f"echo which {FUNCTION}": "allow",
            f"git commit -m 'which {FUNCTION}'": "allow",
            'git commit -m "the \\$functions parameter"': "allow",
            "echo 'the $functions parameter'": "allow",
            f"rg -n 'which|functions|declare -f' README.org": "allow",
            f"python3 - <<'EOF'\nprint('which {FUNCTION} | functions')\nEOF": "allow",
        }
        for policy in POLICIES:
            for command, expected in cases.items():
                with self.subTest(policy=policy.parent.parent.name, command=command):
                    self.assertEqual(classify(policy, json.dumps(command), self.env), expected)

    def test_malformed_input_denies(self):
        for policy in POLICIES:
            self.assertEqual(classify(policy, "not json", self.env), "deny")


class GuardEntryPointTest(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory(prefix="function-body-")
        self.addCleanup(temp.cleanup)
        self.env = fixture_env(temp.name)

    def test_function_bodies_are_denied_at_every_entry(self):
        cases = {
            f"which mbsync-passcmd {FUNCTION}": "deny",
            "typeset -f": "deny",
            "which -a python3": "allow",
            f"whence -w {FUNCTION}": "allow",
        }
        for path, tool in GUARD_ROUTES:
            for command, expected in cases.items():
                with self.subTest(path=str(path.relative_to(ROOT)), tool=tool, command=command):
                    self.assertEqual(decision(run_guard(path, tool, command, self.env)), expected)

    def test_classifier_failure_denies(self):
        module = classifiers.load("function-body-policy")
        for runtime in ("claude", "codex"):
            for failure in (RuntimeError("crashed"), ValueError("malformed")):
                with self.subTest(runtime=runtime, failure=failure), \
                        mock.patch.object(module, "classify", side_effect=failure):
                    output = run_guard(ROOT / runtime / "hooks/block-secret-leak.sh", "Bash", "which ls", self.env)
                    self.assertEqual(decision(output), "deny")

    def test_google_oauth_literals_are_denied_in_commands(self):
        commands = [
            f"GOOGLE_WORKSPACE_CLIENT_SECRET={CLIENT_SECRET} gmail-maildir-sync",
            f"printf '%s' '{REFRESH_TOKEN}' > /dev/null",
            f"curl -H 'Authorization: Bearer {ACCESS_TOKEN}' https://example.org",
        ]
        for path, tool in GUARD_ROUTES:
            for command in commands:
                with self.subTest(path=str(path.relative_to(ROOT)), tool=tool, command=command[:30]):
                    self.assertEqual(decision(run_guard(path, tool, command, self.env)), "deny")

    def test_urls_are_not_refresh_tokens(self):
        for path, tool in GUARD_ROUTES:
            with self.subTest(path=str(path.relative_to(ROOT)), tool=tool):
                command = "echo https://oauth2.googleapis.com/token file:///Users/x/a"
                self.assertEqual(decision(run_guard(path, tool, command, self.env)), "allow")


class RedactorTest(unittest.TestCase):
    def redact(self, text):
        return subprocess.run([str(REDACTOR)], input=text, capture_output=True, text=True,
                              timeout=30, check=True).stdout

    def test_function_body_credentials_are_masked(self):
        body = (f"{FUNCTION} () {{\n"
                f"\tGOOGLE_WORKSPACE_CLIENT_SECRET={CLIENT_SECRET} "
                f"GOOGLE_WORKSPACE_REFRESH_TOKEN='{REFRESH_TOKEN}' command {FUNCTION} \"$@\"\n"
                f"\tcurl -H 'Authorization: Bearer {ACCESS_TOKEN}'\n}}\n")
        masked = self.redact(body)
        for secret in (CLIENT_SECRET, REFRESH_TOKEN, ACCESS_TOKEN):
            self.assertNotIn(secret, masked)
        self.assertIn("GOOGLE_WORKSPACE_CLIENT_SECRET=[GOOGLE_OAUTH_CLIENT_SECRET_REDACTED]", masked)
        self.assertIn("GOOGLE_WORKSPACE_REFRESH_TOKEN='[GOOGLE_REFRESH_TOKEN_REDACTED]'", masked)

    def test_urls_and_paths_are_left_alone(self):
        text = ("https://oauth2.googleapis.com/token\nfile:///Users/x/My Drive/a\n"
                "s3://bucket-name/prefix/object-name-abcdefghijklmnopqrstuvwxyz\n")
        self.assertEqual(self.redact(text), text)


class IntrospectionWrapTest(unittest.TestCase):
    def test_introspection_output_goes_through_the_redactor(self):
        for command in ("which -a python3", "type ls", "whence -v ls", "print -l ${(k)functions}"):
            with self.subTest(command=command):
                payload = {"tool_name": "Bash", "tool_input": {"command": command}}
                result = run_hook(ROOT / "claude/hooks/pretooluse-bash.sh", payload)
                self.assertEqual(result.returncode, 0, result.stderr)
                updated = result.decision()["updatedInput"]["command"]
                self.assertIn("redact-secrets.sh", updated)


if __name__ == "__main__":
    unittest.main()
