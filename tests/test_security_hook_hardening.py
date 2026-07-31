from __future__ import annotations

import json
import subprocess
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
CLAUDE_EXTERNAL_GUARD = DOTFILES / "claude/hooks/guard-external-actions.sh"
CODEX_EXTERNAL_GUARD = DOTFILES / "codex/hooks/guard-external-actions.sh"
EXTERNAL_ACTION_GUARDS = (CLAUDE_EXTERNAL_GUARD, CODEX_EXTERNAL_GUARD)
SENSITIVE_READ_GUARDS = (
    DOTFILES / "claude/hooks/block-sensitive-read.sh",
    DOTFILES / "codex/hooks/block-sensitive-read.sh",
    DOTFILES / "claude/hooks/pretooluse-bash.sh",
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
    def test_external_action_guards_use_prefix_regex_matchers(self):
        configs = (
            (
                "codex",
                json.loads((DOTFILES / "codex/hooks.json").read_text()),
            ),
            (
                "claude",
                json.loads((Path.home() / ".claude/settings.json").read_text()),
            ),
        )
        for name, config in configs:
            with self.subTest(config=name):
                self.assertEqual(
                    hook_matchers(config, "guard-external-actions.sh"),
                    ["mcp__.*"],
                )

    def test_external_action_guards_never_use_a_bare_exit_2(self):
        """Exit 2 is unexplained in Claude and fail-open in Codex.

        Codex 0.146.0 logs "hook exited with code 2 but did not write a
        blocking reason to stderr" and allows the call. Both copies must emit a
        structured permissionDecision instead.
        """
        for guard in EXTERNAL_ACTION_GUARDS:
            with self.subTest(guard=guard.parent.parent.name):
                source = guard.read_text()
                offenders = [
                    line.strip()
                    for line in source.splitlines()
                    if line.strip() == "exit 2"
                ]
                self.assertEqual(offenders, [])
                self.assertIn("permissionDecision", source)

    def test_codex_external_guard_cannot_emit_an_ask_decision(self):
        source = CODEX_EXTERNAL_GUARD.read_text()
        self.assertNotIn('"permissionDecision": "ask"', source)

    def test_live_claude_sensitive_read_guard_covers_read_and_grep(self):
        config = json.loads((Path.home() / ".claude/settings.json").read_text())
        matchers = hook_matchers(config, "block-sensitive-read.sh")
        covered_tools = {
            tool
            for matcher in matchers
            for tool in matcher.split("|")
        }
        self.assertGreaterEqual(covered_tools, {"Read", "Grep"})


class ExternalActionGuardTests(unittest.TestCase):
    """Assert what each *host* does with the guard's output, not the exit code.

    The previous version of these tests asserted ``returncode == 2`` under names
    that said "requires confirmation". That is not what exit 2 means, and the
    mismatch hid two real defects for as long as the tests existed:

    * Claude Code treats exit 2 as a blocking error and shows stderr as the
      reason. The guard wrote nothing, so every block read as
      "hook error: No stderr output".
    * Codex 0.146.0 treats exit 2 with an empty stderr as a *failed* hook and
      lets the call proceed, so the guard blocked nothing at all under Codex.

    Both copies must therefore exit 0 and emit a structured decision with a
    reason. Codex has no "ask" decision and rejects it as unsupported (failing
    open), so needs_approval is enforced there as a deny.
    """

    def decision_for(self, guard: Path, tool_name: str, tool_input: dict | str | None):
        payload = {"tool_name": tool_name, "tool_input": {} if tool_input is None else tool_input}
        result = run_hook(guard, payload)
        # Never exit 2 from either copy: it is unexplained in Claude and
        # fail-open in Codex.
        self.assertEqual(result.returncode, 0, f"{guard.name} stderr={result.stderr!r}")
        self.assertEqual(result.stderr, "", f"{guard.name} wrote to stderr")
        if not result.stdout.strip():
            return "safe", ""
        output = json.loads(result.stdout)
        specific = output["hookSpecificOutput"]
        self.assertEqual(specific["hookEventName"], "PreToolUse")
        reason = specific.get("permissionDecisionReason", "")
        self.assertTrue(reason.strip(), f"{guard.name} emitted a decision with no reason")
        return specific["permissionDecision"], reason

    def assert_safe(self, tool_name: str, tool_input: dict | str | None = None) -> None:
        for guard in EXTERNAL_ACTION_GUARDS:
            with self.subTest(guard=guard.parent.parent.name, tool=tool_name):
                decision, _ = self.decision_for(guard, tool_name, tool_input)
                self.assertEqual(decision, "safe")

    def assert_needs_approval(self, tool_name: str, tool_input: dict | str | None = None) -> None:
        """Claude escalates to the user; Codex denies because it cannot escalate."""
        claude, _ = self.decision_for(CLAUDE_EXTERNAL_GUARD, tool_name, tool_input)
        self.assertEqual(claude, "ask", f"Claude copy should escalate {tool_name}")
        codex, reason = self.decision_for(CODEX_EXTERNAL_GUARD, tool_name, tool_input)
        self.assertEqual(codex, "deny", f"Codex copy should deny {tool_name}")
        self.assertIn("ask", reason.lower(), "Codex deny should explain the missing ask semantics")

    def assert_denied(self, tool_name: str, tool_input: dict | str | None = None) -> None:
        for guard in EXTERNAL_ACTION_GUARDS:
            with self.subTest(guard=guard.parent.parent.name, tool=tool_name):
                decision, _ = self.decision_for(guard, tool_name, tool_input)
                self.assertEqual(decision, "deny")

    # --- non-MCP and generic name classification ---

    def test_non_mcp_tool_is_untouched(self):
        self.assert_safe("Bash", {"command": "ls"})

    def test_plain_read_only_get_remains_allowed(self):
        self.assert_safe("mcp__example__get_item")

    def test_get_or_create_requires_confirmation(self):
        self.assert_needs_approval("mcp__example__get_or_create_item")

    def test_check_and_update_requires_confirmation(self):
        self.assert_needs_approval("mcp__example__check_and_update_item")

    def test_unknown_tool_on_new_server_requires_confirmation(self):
        self.assert_needs_approval("mcp__brand_new_server__do_something")

    def test_codex_copy_never_emits_ask(self):
        """"ask" is an unsupported decision in Codex and fails open if emitted."""
        for tool_name, tool_input in (
            ("mcp__example__get_or_create_item", {}),
            ("mcp__claude-in-chrome__computer", {"action": "left_click", "tabId": 1}),
            ("mcp__claude-in-chrome__javascript_tool", {"text": "1"}),
        ):
            with self.subTest(tool=tool_name):
                decision, _ = self.decision_for(CODEX_EXTERNAL_GUARD, tool_name, tool_input)
                self.assertNotEqual(decision, "ask")

    # --- browser reachability ---

    def test_reaching_and_reading_a_page_needs_no_approval(self):
        """The whole path to a page must be usable, or the read tools are dead.

        The old allowlist permitted every page-reading tool and denied
        navigation, so an agent could read a page it had no way to open.
        """
        for tool_name, tool_input in (
            ("mcp__claude-in-chrome__list_connected_browsers", {}),
            ("mcp__claude-in-chrome__select_browser", {"deviceId": "device-1"}),
            ("mcp__claude-in-chrome__tabs_context_mcp", {"createIfEmpty": True}),
            ("mcp__claude-in-chrome__tabs_create_mcp", {}),
            ("mcp__claude-in-chrome__navigate", {"url": "https://example.test/page", "tabId": 1}),
            ("mcp__claude-in-chrome__read_page", {"tabId": 1}),
            ("mcp__claude-in-chrome__get_page_text", {"tabId": 1}),
        ):
            with self.subTest(tool=tool_name):
                self.assert_safe(tool_name, tool_input)

    # --- input-aware browser classification ---

    def test_observing_with_the_computer_tool_is_allowed(self):
        for action in ("screenshot", "wait", "scroll", "zoom", "hover", "scroll_to"):
            with self.subTest(action=action):
                self.assert_safe(
                    "mcp__claude-in-chrome__computer", {"action": action, "tabId": 1}
                )

    def test_acting_with_the_computer_tool_requires_approval(self):
        for action in (
            "left_click",
            "right_click",
            "double_click",
            "triple_click",
            "type",
            "key",
            "left_click_drag",
        ):
            with self.subTest(action=action):
                self.assert_needs_approval(
                    "mcp__claude-in-chrome__computer", {"action": action, "tabId": 1}
                )

    def test_forms_uploads_and_arbitrary_javascript_require_approval(self):
        for tool in ("form_input", "file_upload", "upload_image", "javascript_tool"):
            with self.subTest(tool=tool):
                self.assert_needs_approval(f"mcp__claude-in-chrome__{tool}", {"tabId": 1})

    def test_http_navigation_is_allowed_and_local_schemes_are_denied(self):
        for url in ("https://example.test/x", "http://example.test/x", "example.test:8080/x"):
            with self.subTest(url=url):
                self.assert_safe("mcp__claude-in-chrome__navigate", {"url": url, "tabId": 1})
        for url in (
            "javascript:alert(1)",
            "file:///etc/passwd",
            "chrome://settings",
            "data:text/html,<b>x",
            "view-source:https://example.test",
        ):
            with self.subTest(url=url):
                self.assert_denied("mcp__claude-in-chrome__navigate", {"url": url, "tabId": 1})

    # --- browser_batch is inspected item by item ---

    def test_batch_of_safe_actions_is_allowed(self):
        self.assert_safe(
            "mcp__claude-in-chrome__browser_batch",
            {
                "actions": [
                    {"name": "navigate", "input": {"url": "https://example.test", "tabId": 1}},
                    {"name": "computer", "input": {"action": "screenshot", "tabId": 1}},
                ]
            },
        )

    def test_batch_takes_the_most_restrictive_verdict_of_its_items(self):
        """A batch must not smuggle an action past a check of the wrapper alone."""
        self.assert_needs_approval(
            "mcp__claude-in-chrome__browser_batch",
            {
                "actions": [
                    {"name": "navigate", "input": {"url": "https://example.test", "tabId": 1}},
                    {"name": "computer", "input": {"action": "left_click", "tabId": 1}},
                ]
            },
        )
        self.assert_denied(
            "mcp__claude-in-chrome__browser_batch",
            {"actions": [{"name": "navigate", "input": {"url": "javascript:alert(1)"}}]},
        )

    def test_empty_or_malformed_batch_requires_approval(self):
        self.assert_needs_approval("mcp__claude-in-chrome__browser_batch", {"actions": []})
        self.assert_needs_approval(
            "mcp__claude-in-chrome__browser_batch", {"actions": [{"input": {}}]}
        )

    def test_string_wrapped_tool_input_is_still_classified(self):
        """Codex sends tool_input as a JSON string on some events."""
        self.assert_needs_approval(
            "mcp__claude-in-chrome__computer",
            json.dumps({"action": "left_click", "tabId": 1}),
        )
        self.assert_safe(
            "mcp__claude-in-chrome__computer",
            json.dumps({"action": "screenshot", "tabId": 1}),
        )


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
