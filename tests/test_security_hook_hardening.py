from __future__ import annotations

import json
import subprocess
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
SENSITIVE_READ_GUARDS = (
    DOTFILES / "claude/hooks/block-sensitive-read.sh",
    DOTFILES / "codex/hooks/block-sensitive-read.sh",
    DOTFILES / "claude/hooks/pretooluse-bash.sh",
)
SENSITIVE_READ_HELPERS = {
    DOTFILES / "claude/hooks/block-sensitive-read.sh": DOTFILES
    / "macos/.claude/skills/security-audit/scripts/classify-shell-exports.py",
    DOTFILES / "codex/hooks/block-sensitive-read.sh": DOTFILES
    / "macos/.codex/skills/security-audit/scripts/classify-shell-exports.py",
    DOTFILES / "claude/hooks/pretooluse-bash.sh": DOTFILES
    / "macos/.claude/skills/security-audit/scripts/classify-shell-exports.py",
}


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
    return output["hookSpecificOutput"].get("permissionDecision", "allow")


def hook_matchers(config: dict, command_fragment: str) -> list[str]:
    return [
        registration["matcher"]
        for registration in config["hooks"]["PreToolUse"]
        if any(
            command_fragment in hook.get("command", "")
            for hook in registration.get("hooks", [])
        )
    ]


class ProtectedHookRegistrationTests(unittest.TestCase):
    def test_live_claude_sensitive_read_guard_covers_read_and_grep(self):
        """The live registration must enforce both implemented read branches.

        Grep coverage was removed on 2026-07-31 after broad searches interrupted
        ordinary work. The guard now has regression coverage for the intended
        boundary: ordinary repository scopes pass, while sensitive paths, globs,
        and broad home-directory content searches are denied.
        """
        config = json.loads((Path.home() / ".claude/settings.json").read_text())
        matchers = hook_matchers(config, "block-sensitive-read.sh")
        covered_tools = {
            tool
            for matcher in matchers
            for tool in matcher.split("|")
        }
        self.assertGreaterEqual(covered_tools, {"Read", "Grep"})

    def test_secret_store_and_clipboard_reads_are_not_preapproved(self):
        config = json.loads((Path.home() / ".claude/settings.json").read_text())
        permission_allows = set(config["permissions"]["allow"])
        self.assertTrue(
            {
                "Bash(op-automations:*)",
                "Bash(pbpaste)",
                "Bash(pbpaste:*)",
            }.isdisjoint(permission_allows)
        )
        auto_allows = config.get("autoMode", {}).get("allow", [])
        self.assertFalse(any("op-automations" in rule for rule in auto_allows))


class SensitiveReadGuardTests(unittest.TestCase):
    def test_value_free_shell_export_classifier_is_allowed(self):
        secrets = DOTFILES / "shell/.zshenv-secrets"
        for guard, helper in SENSITIVE_READ_HELPERS.items():
            with self.subTest(guard=guard):
                command = f'python3 "{helper}" "{secrets}"'
                payload = {"tool_name": "Bash", "tool_input": {"command": command}}
                result = run_hook(guard, payload)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "allow")

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


class HeredocSensitiveReadTests(unittest.TestCase):
    """Heredoc bodies fed to a data sink are data; bodies fed to interpreters are not."""

    def run_all(self, command: str) -> list[tuple[Path, subprocess.CompletedProcess[str]]]:
        payload = {"tool_name": "Bash", "tool_input": {"command": command}}
        return [(guard, run_hook(guard, payload)) for guard in SENSITIVE_READ_GUARDS]

    def test_secrets_path_inside_a_sink_heredoc_is_allowed(self):
        command = "cat <<'EOF' > notes.md\nNever print .zshenv-secrets in a report.\nEOF"
        for guard, result in self.run_all(command):
            with self.subTest(guard=guard):
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "allow")

    def test_secrets_path_inside_an_interpreter_heredoc_is_denied_with_the_reason(self):
        command = "python3 - <<'PY'\nprint(open('.zshenv-secrets').read())\nPY"
        for guard, result in self.run_all(command):
            with self.subTest(guard=guard):
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "deny")
                self.assertIn("heredoc", result.stdout)

    def test_sink_heredoc_piped_to_a_shell_is_denied(self):
        command = "cat <<'EOF' | bash\ncat ~/.zshenv-secrets\nEOF"
        for guard, result in self.run_all(command):
            with self.subTest(guard=guard):
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "deny")


if __name__ == "__main__":
    unittest.main()
