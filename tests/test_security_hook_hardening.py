from __future__ import annotations

import json
import subprocess
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
EXTERNAL_ACTION_GUARDS = (
    DOTFILES / "claude/hooks/guard-external-actions.sh",
    DOTFILES / "codex/hooks/guard-external-actions.sh",
)
SENSITIVE_READ_GUARDS = (
    DOTFILES / "claude/hooks/block-sensitive-read.sh",
    DOTFILES / "codex/hooks/block-sensitive-read.sh",
)


def run_hook(script: Path, payload: dict) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["bash", str(script)],
        input=json.dumps(payload),
        text=True,
        capture_output=True,
        check=False,
    )


def permission_decision(result: subprocess.CompletedProcess[str]) -> str:
    if not result.stdout.strip():
        return "allow"
    output = json.loads(result.stdout)
    return output["hookSpecificOutput"]["permissionDecision"]


class ExternalActionGuardTests(unittest.TestCase):
    def assert_guarded_by_both(self, bare_tool: str) -> None:
        payload = {"tool_name": f"mcp__example__{bare_tool}", "tool_input": {}}
        for guard in EXTERNAL_ACTION_GUARDS:
            with self.subTest(guard=guard, tool=bare_tool):
                result = run_hook(guard, payload)
                self.assertEqual(result.returncode, 2, result.stderr)

    def test_plain_read_only_get_remains_allowed(self):
        payload = {"tool_name": "mcp__example__get_item", "tool_input": {}}
        for guard in EXTERNAL_ACTION_GUARDS:
            with self.subTest(guard=guard):
                result = run_hook(guard, payload)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout, "")

    def test_get_or_create_requires_confirmation(self):
        self.assert_guarded_by_both("get_or_create_item")

    def test_check_and_update_requires_confirmation(self):
        self.assert_guarded_by_both("check_and_update_item")


class SensitiveReadGuardTests(unittest.TestCase):
    def test_safe_prefix_does_not_hide_later_content_read(self):
        command = "ls ~/.ssh/id_test_guard; cat ~/.ssh/id_test_guard"
        payload = {"tool_name": "Bash", "tool_input": {"command": command}}
        for guard in SENSITIVE_READ_GUARDS:
            with self.subTest(guard=guard):
                result = run_hook(guard, payload)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "deny")

    def test_single_metadata_command_remains_allowed(self):
        payload = {
            "tool_name": "Bash",
            "tool_input": {"command": "ls -l ~/.ssh/id_test_guard"},
        }
        for guard in SENSITIVE_READ_GUARDS:
            with self.subTest(guard=guard):
                result = run_hook(guard, payload)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "allow")

    def test_git_ignore_metadata_commands_are_allowed(self):
        commands = (
            "git check-ignore --no-index .env.local",
            "git ls-files --error-unmatch .env.example",
        )
        for guard in SENSITIVE_READ_GUARDS:
            for command in commands:
                with self.subTest(guard=guard, command=command):
                    payload = {
                        "tool_name": "Bash",
                        "tool_input": {"command": command},
                    }
                    result = run_hook(guard, payload)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(permission_decision(result), "allow")

    def test_git_metadata_prefix_does_not_allow_compound_read(self):
        command = "git check-ignore .env.local; cat .env.local"
        payload = {"tool_name": "Bash", "tool_input": {"command": command}}
        for guard in SENSITIVE_READ_GUARDS:
            with self.subTest(guard=guard):
                result = run_hook(guard, payload)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "deny")


class ClaudeGrepSensitiveReadTests(unittest.TestCase):
    guard = DOTFILES / "claude/hooks/block-sensitive-read.sh"

    def assert_decision(self, tool_input: dict, expected: str, **payload_fields) -> None:
        payload = {"tool_name": "Grep", "tool_input": tool_input, **payload_fields}
        result = run_hook(self.guard, payload)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(permission_decision(result), expected)

    def test_content_mode_blocks_direct_sensitive_path(self):
        self.assert_decision(
            {"pattern": "TOKEN", "path": "~/.env", "output_mode": "content"},
            "deny",
        )

    def test_non_content_mode_allows_direct_sensitive_path(self):
        self.assert_decision(
            {
                "pattern": "TOKEN",
                "path": "~/.env",
                "output_mode": "files_with_matches",
            },
            "allow",
        )

    def test_content_mode_blocks_sensitive_glob(self):
        self.assert_decision(
            {
                "pattern": "TOKEN",
                "path": str(DOTFILES),
                "glob": "**/.env.*",
                "output_mode": "content",
            },
            "deny",
        )

    def test_content_mode_blocks_home_directory_scope(self):
        self.assert_decision(
            {"pattern": "TOKEN", "path": "~", "output_mode": "content"},
            "deny",
        )

    def test_content_mode_blocks_omitted_path_from_sensitive_cwd(self):
        self.assert_decision(
            {"pattern": "TOKEN", "output_mode": "content"},
            "deny",
            cwd=str(Path.home() / ".gnupg"),
        )

    def test_content_mode_allows_ordinary_repo_scope(self):
        self.assert_decision(
            {
                "pattern": "TOKEN",
                "path": str(DOTFILES / "tests"),
                "output_mode": "content",
            },
            "allow",
        )


if __name__ == "__main__":
    unittest.main()
