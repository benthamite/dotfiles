"""The 1Password reference template is not a secrets file, and may not become one.

A `.env.op` holds `NAME=op://vault/item/field` lines that `op run --env-file`
resolves at runtime; it is tracked in git and contains no secret values by
construction. Two guards share that assumption and this file pins both sides:

- the sensitive-read guards (three copies) let it be read, edited, named in a
  commit message or mentioned in an interpreter-fed heredoc, while `.env`,
  `.env.local` and `.env.op.bak` stay denied;
- the secret-leak guards (two copies plus the dispatcher) deny writing a
  literal secret into it, because they no longer exempt the name from the
  pattern scan.
"""

from __future__ import annotations

import json
import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SENSITIVE_READ = {
    "claude": ROOT / "claude" / "hooks" / "block-sensitive-read.sh",
    "codex": ROOT / "codex" / "hooks" / "block-sensitive-read.sh",
    "dispatcher": ROOT / "claude" / "hooks" / "pretooluse-bash.sh",
}
SECRET_LEAK = {
    "claude": ROOT / "claude" / "hooks" / "block-secret-leak.sh",
    "codex": ROOT / "codex" / "hooks" / "block-secret-leak.sh",
    "dispatcher": ROOT / "claude" / "hooks" / "pretooluse-bash.sh",
}
FAKE_GITHUB_TOKEN = "ghp_" + "A1b2C3d4E5f6G7h8I9j0K1l2M3n4O5p6Q7r8S9t0"


def run(guard: Path, payload: dict) -> str:
    result = subprocess.run(["bash", str(guard)], input=json.dumps(payload), text=True,
                            capture_output=True, check=False)
    assert result.returncode == 0, result.stderr
    if not result.stdout.strip():
        return "allow"
    out = json.loads(result.stdout)["hookSpecificOutput"]
    return out.get("permissionDecision") or "allow"


def bash(guard: Path, command: str) -> str:
    return run(guard, {"tool_name": "Bash", "tool_input": {"command": command}})


class TemplateIsReadable(unittest.TestCase):
    ALLOW = [
        "cat .env.op",
        "cat /Users/x/repos/epoch/staff-data/.env.op",
        "cd ~/repos/x && cat .env.op",
        "git show HEAD:.env.op",
        "git -C /Users/x/repo show HEAD:.env.op",
        "git -C /Users/x/repo ls-files .env.op | xargs cat",
        "python3 dump.py --env-file=.env.op",
        "sed -n 1,20p .env.op",
        "grep -n 'op://' .env.op",
        "diff .env.op ../other/.env.op",
        "git commit -m 'staff-data: add the Slack token to .env.op'",
        "perl -0pi -e 's/switch the vault segment in each =.env.op=/x/' notes.org",
        "python3 - <<'EOF'\nprint('edit each .env.op')\nEOF",
        "cat <<'EOF' > notes.md\nThe .env.op template resolves at runtime.\nEOF",
    ]
    DENY = [
        "cat .env",
        "cat /Users/x/repo/.env",
        "cat .env.local",
        "cat .env.production",
        "cat .env.op.bak",
        "git show HEAD:.env",
        "git show HEAD:.env.local",
        "cat .env.op .env",
        "cat .env.op; cat .env.local",
        "python3 - <<'EOF'\nprint(open('.env').read())\nEOF",
        "sed -n 1p .envrc",
    ]

    def test_template_reads_are_allowed_everywhere(self):
        for name, guard in SENSITIVE_READ.items():
            for command in self.ALLOW:
                with self.subTest(guard=name, command=command):
                    self.assertEqual(bash(guard, command), "allow")

    def test_real_env_files_stay_denied_everywhere(self):
        for name, guard in SENSITIVE_READ.items():
            for command in self.DENY:
                with self.subTest(guard=name, command=command):
                    self.assertEqual(bash(guard, command), "deny")

    def test_read_tool_distinguishes_template_from_env_file(self):
        for name in ("claude", "codex"):
            guard = SENSITIVE_READ[name]
            for path, expected in (
                ("/Users/x/repos/epoch/staff-data/.env.op", "allow"),
                ("~/repos/epoch/staff-data/.env.op", "allow"),
                ("/Users/x/repos/epoch/staff-data/.env", "deny"),
                ("/Users/x/repos/epoch/staff-data/.env.local", "deny"),
                ("/Users/x/repos/epoch/staff-data/.env.op.bak", "deny"),
            ):
                with self.subTest(guard=name, path=path):
                    payload = {"tool_name": "Read", "tool_input": {"file_path": path}}
                    self.assertEqual(run(guard, payload), expected)

    def test_grep_tool_content_mode_over_template_is_allowed(self):
        # Codex has no Grep tool, so only the Claude copy carries this branch.
        for name in ("claude",):
            guard = SENSITIVE_READ[name]
            for path, expected in (
                ("/Users/x/repos/epoch/staff-data/.env.op", "allow"),
                ("/Users/x/repos/epoch/staff-data/.env", "deny"),
            ):
                with self.subTest(guard=name, path=path):
                    payload = {"tool_name": "Grep",
                               "tool_input": {"pattern": "op://", "path": path, "output_mode": "content"}}
                    self.assertEqual(run(guard, payload), expected)


class InertMentionsAndExampleTemplates(unittest.TestCase):
    """A commit message is stored data; a tracked *.example file is a template."""

    ALLOW = [
        "git commit -m 'staff-data: stop reading .env at import time'",
        'git commit -q -m "note: .env.local is gitignored"',
        "git -C /Users/x/repo commit -m 'drop the stale .env.local'",
        "git add README.md && git commit -m 'docs: explain .env and .env.local'",
        "git commit -m 'guards: real dotenv files\n\n.env, .env.local and .envrc stay covered.'",
        "git commit -m 'first' -m 'second names .env.local'",
        "cat .env.example",
        "cat /Users/x/repos/epoch/paid-service-savings/.env.op.finance.example",
        "sed -n 1,20p .env.op.example",
        "diff .env.example .env.op.example",
    ]
    DENY = [
        'git commit -m "$(cat .env)"',
        "git commit -m 'x' && cat .env",
        "git commit -m 'x'; cat .env.local",
        "git commit -F .env",
        "less -m '.env'",
        "python3 -m '.env'",
        "cat .env.example.bak",
        "cat .env.op.finance",
    ]

    def test_inert_mentions_and_templates_are_allowed(self):
        for name, guard in SENSITIVE_READ.items():
            for command in self.ALLOW:
                with self.subTest(guard=name, command=command):
                    self.assertEqual(bash(guard, command), "allow")

    def test_real_reads_beside_them_stay_denied(self):
        for name, guard in SENSITIVE_READ.items():
            for command in self.DENY:
                with self.subTest(guard=name, command=command):
                    self.assertEqual(bash(guard, command), "deny")

    def test_read_tool_allows_example_templates(self):
        for name in ("claude", "codex"):
            guard = SENSITIVE_READ[name]
            for path, expected in (
                ("/Users/x/repo/.env.example", "allow"),
                ("/Users/x/repo/.env.op.finance.example", "allow"),
                ("/Users/x/repo/.env.op.finance", "deny"),
            ):
                with self.subTest(guard=name, path=path):
                    payload = {"tool_name": "Read", "tool_input": {"file_path": path}}
                    self.assertEqual(run(guard, payload), expected)


class TemplateMayNotReceiveSecrets(unittest.TestCase):
    def test_bash_write_of_a_token_into_the_template_is_denied(self):
        for name, guard in SECRET_LEAK.items():
            command = f"echo 'GITHUB_TOKEN={FAKE_GITHUB_TOKEN}' >> .env.op"
            with self.subTest(guard=name):
                self.assertEqual(bash(guard, command), "deny")

    def test_bash_write_of_a_reference_into_the_template_is_allowed(self):
        for name, guard in SECRET_LEAK.items():
            command = "echo 'GITHUB_TOKEN=op://Automations/GitHub - Epoch/credential' >> .env.op"
            with self.subTest(guard=name):
                self.assertEqual(bash(guard, command), "allow")

    def test_write_tool_token_into_the_template_is_denied(self):
        guard = SECRET_LEAK["claude"]
        payload = {"tool_name": "Write",
                   "tool_input": {"file_path": "/Users/x/repo/.env.op",
                                  "content": f"GITHUB_TOKEN={FAKE_GITHUB_TOKEN}\n"}}
        self.assertEqual(run(guard, payload), "deny")

    def test_write_tool_references_into_the_template_are_allowed(self):
        guard = SECRET_LEAK["claude"]
        payload = {"tool_name": "Write",
                   "tool_input": {"file_path": "/Users/x/repo/.env.op",
                                  "content": "SLACK_BOT_TOKEN=op://Automations/Slack - Epoch Notifier/credential\n"
                                             "AIRTABLE_API_KEY=\"op://Automations/Airtable API - Epoch/credential\"\n"}}
        self.assertEqual(run(guard, payload), "allow")


if __name__ == "__main__":
    unittest.main()
