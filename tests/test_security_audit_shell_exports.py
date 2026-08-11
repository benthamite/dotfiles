from __future__ import annotations

import json
import subprocess
import tempfile
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
CLASSIFIERS = {
    "claude": DOTFILES
    / "macos/.claude/skills/security-audit/scripts/classify-shell-exports.py",
    "codex": DOTFILES
    / "macos/.codex/skills/security-audit/scripts/classify-shell-exports.py",
}
SKILLS = {
    "claude": DOTFILES / "macos/.claude/skills/security-audit/SKILL.md",
    "codex": DOTFILES / "macos/.codex/skills/security-audit/SKILL.md",
}


class SecurityAuditShellExportTests(unittest.TestCase):
    def run_classifier(self, script: Path, content: str) -> list[dict[str, object]]:
        with tempfile.TemporaryDirectory() as directory:
            fixture = Path(directory) / "shell-secrets"
            fixture.write_text(content)
            result = subprocess.run(
                ["python3", str(script), str(fixture)],
                capture_output=True,
                text=True,
                check=False,
            )
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads(result.stdout)

    def assert_both(self, content: str, expected: list[dict[str, object]]) -> None:
        outputs = {}
        for tool, script in CLASSIFIERS.items():
            with self.subTest(tool=tool):
                outputs[tool] = self.run_classifier(script, content)
                self.assertEqual(outputs[tool], expected)
        self.assertEqual(outputs["claude"], outputs["codex"])

    def test_classifies_only_real_exports_without_exposing_values(self):
        content = """\
# export COMMENT_TOKEN="not-a-real-export"
run_service() {
  FUNCTION_TOKEN="$(pass env/function-token)" command
  export FUNCTION_SECRET="function-only"
}
export PERSONAL_EMAIL="person@example.com"
export SERVICE_TOKEN="$(pass env/service-token)"
export LEGACY_PASSWORD="literal-value-that-must-never-print"
export FORWARDED_API_KEY="$EXISTING_API_KEY"
export DISPLAY_MODE="compact"
"""
        expected = [
            {
                "classification": "function-local-credential",
                "line": 4,
                "name": "FUNCTION_SECRET",
                "scope": "function",
            },
            {
                "classification": "identity",
                "line": 6,
                "name": "PERSONAL_EMAIL",
                "scope": "global",
            },
            {
                "classification": "credential-store-backed",
                "line": 7,
                "name": "SERVICE_TOKEN",
                "scope": "global",
            },
            {
                "classification": "credential-literal",
                "line": 8,
                "name": "LEGACY_PASSWORD",
                "scope": "global",
            },
            {
                "classification": "credential-indirect",
                "line": 9,
                "name": "FORWARDED_API_KEY",
                "scope": "global",
            },
            {
                "classification": "non-secret",
                "line": 10,
                "name": "DISPLAY_MODE",
                "scope": "global",
            },
        ]
        self.assert_both(content, expected)

        rendered = json.dumps(expected)
        self.assertNotIn("not-a-real-export", rendered)
        self.assertNotIn("literal-value-that-must-never-print", rendered)
        self.assertNotIn("person@example.com", rendered)

    def test_skill_requires_value_free_export_classification(self):
        texts = {tool: path.read_text() for tool, path in SKILLS.items()}
        for tool, text in texts.items():
            with self.subTest(tool=tool):
                self.assertIn("scripts/classify-shell-exports.py", text)
                self.assertIn("absolute path", text)
                self.assertIn("Do not infer ambient secrets from export counts", text)
                self.assertIn("Identity exports", text)
        self.assertEqual(texts["claude"], texts["codex"])


if __name__ == "__main__":
    unittest.main()
