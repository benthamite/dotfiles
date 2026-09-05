"""Exercise actual guard entry points without executing their proposed tools."""

import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
GUARD_NAMES = (
    "block-secret-leak.sh", "block-sensitive-read.sh",
    "block-destructive-command.sh", "block-unguarded-ahrefs-api.sh",
    "block-dirty-tree-rewrite.sh", "block-unregistered-launchagent.sh",
    "block-walk-list-access.sh",
)
GUARDS = [ROOT / provider / "hooks" / name
          for provider in ("claude", "codex") for name in GUARD_NAMES]
GUARDS += [ROOT / "claude/hooks/pretooluse-bash.sh",
           ROOT / "codex/hooks/block-github-write-command.sh"]


def run_guard(path, payload, env=None):
    return subprocess.run(["/bin/bash", str(path)], input=payload,
                          text=True, capture_output=True, env=env, timeout=15)


class EditingToolSecretCoverageTests(unittest.TestCase):
    def test_actual_edit_fields_are_scanned(self):
        # A header alone is a recognizable pattern, not a private key.
        marker = "-" * 5 + "BEGIN PRIVATE KEY" + "-" * 5
        for provider, tool, field in (
            ("claude", "Write", "content"), ("claude", "Edit", "new_string"),
            ("claude", "NotebookEdit", "new_source"),
            ("codex", "Write", "content"), ("codex", "Edit", "new_string"),
            ("codex", "apply_patch", "input"),
        ):
            for content, expected in (("ordinary replacement", "allow"), (marker, "deny")):
                with self.subTest(provider=provider, tool=tool, expected=expected):
                    values = {"file_path": "/tmp/guard-fixture.txt", field: content}
                    if tool == "NotebookEdit":
                        values["notebook_path"] = values.pop("file_path")
                    payload = json.dumps({"tool_name": tool, "tool_input": values})
                    result = run_guard(ROOT / provider / "hooks/block-secret-leak.sh", payload)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    decision = (json.loads(result.stdout)["hookSpecificOutput"]["permissionDecision"]
                                if result.stdout.strip() else "allow")
                    self.assertEqual(decision, expected)
                    self.assertNotIn(marker, result.stdout + result.stderr)


class GuardFailureSafetyTests(unittest.TestCase):
    def assert_blocks_failure(self, result):
        self.assertEqual(result.returncode, 2,
                         f"failure must block, got {result.returncode}: {result.stdout!r}")
        self.assertIn("Security hook failed", result.stderr)

    def test_malformed_input_blocks_at_every_entry_point(self):
        for guard in GUARDS:
            with self.subTest(guard=guard):
                self.assert_blocks_failure(run_guard(guard, "{broken"))

    def test_missing_jq_blocks_at_every_entry_point(self):
        with tempfile.TemporaryDirectory(prefix="guard-no-jq-") as directory:
            # Retain startup dependencies so this specifically reaches missing jq.
            for name in ("dirname", "cat", "bash"):
                Path(directory, name).symlink_to(shutil.which(name))
            env = {**os.environ, "PATH": directory}
            payload = json.dumps({"tool_name": "Bash", "tool_input": {"command": "true"}})
            for guard in GUARDS:
                with self.subTest(guard=guard):
                    result = run_guard(guard, payload, env)
                    self.assert_blocks_failure(result)
                    self.assertIn("jq", result.stderr)

    def test_missing_startup_source_blocks_at_every_entry_point(self):
        with tempfile.TemporaryDirectory(prefix="guard-no-source-") as directory:
            for index, guard in enumerate(GUARDS):
                if not any(line.startswith("source ") for line in guard.read_text().splitlines()):
                    continue  # This standalone guard has no startup source dependency.
                with self.subTest(guard=guard):
                    copied = Path(directory, str(index), guard.name)
                    copied.parent.mkdir()
                    shutil.copyfile(guard, copied)
                    payload = json.dumps({"tool_name": "Bash", "tool_input": {"command": "true"}})
                    self.assert_blocks_failure(run_guard(copied, payload))


if __name__ == "__main__":
    unittest.main()
