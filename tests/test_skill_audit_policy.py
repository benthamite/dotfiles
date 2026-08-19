from __future__ import annotations

import re
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
CLAUDE_SKILL = DOTFILES / ".claude/skills/skill-audit"
CODEX_SKILL = DOTFILES / ".codex/skills/skill-audit"


def frontmatter(path: Path) -> str:
    text = path.read_text(encoding="utf-8")
    match = re.match(r"^---\n(.*?)\n---\n", text, re.DOTALL)
    if match is None:
        raise AssertionError(f"missing frontmatter: {path}")
    return match.group(1)


class SkillAuditPolicyTests(unittest.TestCase):
    def test_claude_blocks_model_invocation(self):
        metadata = frontmatter(CLAUDE_SKILL / "SKILL.md")

        self.assertRegex(metadata, r"(?m)^disable-model-invocation: true$")

    def test_codex_blocks_implicit_invocation(self):
        metadata = (CODEX_SKILL / "agents/openai.yaml").read_text(
            encoding="utf-8"
        )

        self.assertRegex(
            metadata,
            r"(?m)^policy:\n  allow_implicit_invocation: false$",
        )

    def test_openai_metadata_is_paired(self):
        self.assertEqual(
            (CLAUDE_SKILL / "agents/openai.yaml").read_bytes(),
            (CODEX_SKILL / "agents/openai.yaml").read_bytes(),
        )

    def test_routing_evaluations_cover_positive_and_negative_cases(self):
        scenarios = (CODEX_SKILL / "evals/scenarios.md").read_text(encoding="utf-8")

        self.assertIn("Should load:", scenarios)
        self.assertIn("Must not load automatically:", scenarios)
        self.assertIn("$skill-audit", scenarios)


if __name__ == "__main__":
    unittest.main()
