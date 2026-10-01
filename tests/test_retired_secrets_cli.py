"""Tests for the shared guard against the retired password-store CLI."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "lib/python"))
sys.path.insert(0, str(Path(__file__).resolve().parent))

import retired_secrets_cli as guard  # noqa: E402
from guard_harness import GuardResult, run_guard  # noqa: E402

HOOKS = (
    ROOT / "claude/hooks/block-retired-secrets-cli.py",
    ROOT / "codex/hooks/block-retired-secrets-cli.py",
)

REFERENCES = (
    "token = $(pass show env/home-assistant-token)",
    'subprocess.run(["pass", "show", "env/x"])',
    "subprocess.check_output(['pass','show','env/x'])",
    "Credentials live in `pass`.",
    "read the token from =pass= first",
    "stored in ~/.password-store/env",
    "export PASSWORD_STORE_DIR=/tmp/x",
    "(require 'pass-extras)",
    "(auth-source-pass-enable)",
    "the full pass entry path",
    "/opt/homebrew/bin/pass",
    "brew install pass",
    "Read the key from pass.",
    "the token comes via `pass` on each run",
    "the pass CLI was removed",
    "pass insert -m env/new",
)

INNOCENT = (
    "def f():\n    pass\n",
    "class E(Exception):\n    pass",
    "All tests passed; the run passes in passing.",
    "Use a passphrase or a password manager.",
    "bypass the compass",
    "the 1Password field label is pass path",
    "in pass 2 the compiler folds constants",
    "a pass rate of 90%",
    "from passlib import hash",
    "op-automations @personal read op://Automation/home-assistant-token/credential",
    "except ValueError:\n    pass  # ignore",
    "~/My Drive/dotfiles/claude/hooks/block-retired-secrets-cli.py",
)


def payload(tool: str, **tool_input: object) -> dict:
    return {"tool_name": tool, "tool_input": tool_input}


class ReferenceTests(unittest.TestCase):
    def test_references_are_found(self) -> None:
        for text in REFERENCES:
            with self.subTest(text=text):
                self.assertIsNotNone(guard.find_reference(text))

    def test_innocent_text_is_allowed(self) -> None:
        for text in INNOCENT:
            with self.subTest(text=text):
                self.assertIsNone(guard.find_reference(text))


class NativeWriteTests(unittest.TestCase):
    def test_write_and_edit_new_text_are_checked(self) -> None:
        for tool, data in (
            ("Write", {"file_path": "/tmp/a.md", "content": REFERENCES[0]}),
            ("Edit", {"file_path": "/tmp/a.md", "old_string": "x", "new_string": REFERENCES[3]}),
            ("MultiEdit", {"file_path": "/tmp/a.md", "edits": [{"old_string": "x", "new_string": REFERENCES[1]}]}),
            ("NotebookEdit", {"notebook_path": "/tmp/a.ipynb", "new_source": REFERENCES[2]}),
        ):
            with self.subTest(tool=tool):
                self.assertIn("1Password", guard.decide(payload(tool, **data)) or "")

    def test_removing_a_reference_is_allowed(self) -> None:
        decision = guard.decide(payload("Edit", file_path="/tmp/a.md", old_string=REFERENCES[0], new_string="op-automations read"))
        self.assertIsNone(decision)

    def test_python_pass_statement_is_allowed(self) -> None:
        self.assertIsNone(guard.decide(payload("Write", file_path="/tmp/a.py", content=INNOCENT[0])))

    def test_only_the_policy_and_its_tests_are_exempt(self) -> None:
        for path in guard.EXEMPT_PATHS:
            with self.subTest(path=path):
                self.assertIsNone(guard.decide(payload("Write", file_path=str(path), content=REFERENCES[0])))
        hook = str(HOOKS[0])
        self.assertIsNotNone(guard.decide(payload("Write", file_path=hook, content=REFERENCES[0])))


class PatchTests(unittest.TestCase):
    def test_added_lines_are_checked_and_removed_lines_are_not(self) -> None:
        added = "*** Begin Patch\n*** Update File: a.md\n@@\n-old\n+" + REFERENCES[4] + "\n*** End Patch\n"
        removed = "*** Begin Patch\n*** Update File: a.md\n@@\n-" + REFERENCES[4] + "\n+new\n*** End Patch\n"
        self.assertIsNotNone(guard.decide(payload("apply_patch", patch=added)))
        self.assertIsNone(guard.decide(payload("apply_patch", patch=removed)))
        self.assertIsNotNone(guard.decide({"tool_name": "apply_patch", "tool_input": added}))


class ShellTests(unittest.TestCase):
    def test_invocations_are_denied(self) -> None:
        for command in (
            "pass show env/x",
            "TOKEN=$(pass show env/x) curl -H x",
            "cd /tmp && pass ls",
            "/opt/homebrew/bin/pass show env/x",
            "which pass",
            "true; pass",
        ):
            with self.subTest(command=command):
                self.assertIsNotNone(guard.decide(payload("Bash", command=command)))

    def test_writes_containing_references_are_denied(self) -> None:
        for command in (
            "echo 'pass show env/x' > notes.md",
            "printf '%s' '`pass`' >> README.md",
            "sed -i '' 's/op-automations read/pass show/' f.py",
            "cat > f.md <<'EOF'\nUse `pass` here.\nEOF",
            "git commit -m 'Read the key from pass'",
            "brew install pass",
        ):
            with self.subTest(command=command):
                self.assertIsNotNone(guard.decide(payload("Bash", command=command)))

    def test_read_only_searches_and_unrelated_commands_are_allowed(self) -> None:
        for command in (
            "git grep -n 'pass show' -- docs",
            "grep -rn 'password-store' . 2>/dev/null | head",
            "rg -l '`pass`' ~/repos",
            "python3 -c 'pass'",
            "cat > f.py <<'EOF'\ndef f():\n    pass\nEOF",
            "pytest -q tests && echo passed",
            "X=$(op-automations @personal read op://Automation/t/credential) && curl -s -H \"a: $X\" http://h",
        ):
            with self.subTest(command=command):
                self.assertIsNone(guard.decide(payload("Bash", command=command)))

    def test_codex_shell_shapes(self) -> None:
        self.assertIsNotNone(guard.decide(payload("exec_command", cmd="pass show env/x")))
        self.assertIsNotNone(guard.decide({"tool_name": "Bash", "tool_input": "pass show env/x"}))
        self.assertIsNone(guard.decide(payload("exec_command", cmd="ls")))


class HookProcessTests(unittest.TestCase):
    def run_hook(self, hook: Path, data: object) -> GuardResult:
        return run_guard(hook, data)

    def test_both_entry_points_deny_and_allow(self) -> None:
        for hook in HOOKS:
            with self.subTest(hook=hook.parent.parent.name):
                denied = self.run_hook(hook, payload("Bash", command="pass show env/x"))
                self.assertEqual(denied.returncode, 0)
                out = denied.decision()
                self.assertEqual(out["permissionDecision"], "deny")
                self.assertIn("secrets.md", out["permissionDecisionReason"])
                allowed = self.run_hook(hook, payload("Write", file_path="/tmp/a.py", content=INNOCENT[0]))
                self.assertEqual((allowed.returncode, allowed.stdout), (0, ""))

    def test_malformed_input_fails_closed(self) -> None:
        for hook in HOOKS:
            with self.subTest(hook=hook.parent.parent.name):
                result = self.run_hook(hook, "not json")
                self.assertEqual(result.returncode, 2)


if __name__ == "__main__":
    unittest.main()
