from __future__ import annotations

import re
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
CLAUDE_SKILLS = DOTFILES / "claude" / "skills"
BARE_HIGH_RISK_TOOLS = {"Bash", "Write", "Edit", "Agent", "AskUserQuestion"}


def frontmatter(path: Path) -> str:
    text = path.read_text(encoding="utf-8")
    match = re.match(r"^---\n(.*?)\n---\n", text, re.DOTALL)
    if match is None:
        raise AssertionError(f"missing frontmatter: {path}")
    return match.group(1)


def frontmatter_value(metadata: str, key: str) -> str | None:
    match = re.search(rf"(?m)^{re.escape(key)}:[ \t]*(.*)$", metadata)
    return match.group(1).strip() if match else None


def allowed_tools(metadata: str) -> list[str]:
    lines = metadata.splitlines()
    for index, line in enumerate(lines):
        match = re.match(r"^allowed-tools:[ \t]*(.*)$", line)
        if match is None:
            continue

        value = match.group(1).strip()
        if value:
            if value.startswith("[") and value.endswith("]"):
                value = value[1:-1]
            return [
                item.strip().strip("'\"")
                for item in value.split(",")
                if item.strip()
            ]

        tools: list[str] = []
        for continuation in lines[index + 1 :]:
            item = re.match(r"^[ \t]+-[ \t]+(.+)$", continuation)
            if item is None:
                break
            tools.append(item.group(1).strip().strip("'\""))
        return tools
    return []


def bare_high_risk_grants(metadata: str) -> list[str]:
    return sorted(BARE_HIGH_RISK_TOOLS & set(allowed_tools(metadata)))


class SkillPermissionFrontmatterTests(unittest.TestCase):
    def test_allowed_tools_parser_handles_inline_and_block_lists(self):
        self.assertEqual(
            allowed_tools("allowed-tools: [Bash, Read, Write]"),
            ["Bash", "Read", "Write"],
        )
        self.assertEqual(
            allowed_tools("allowed-tools:\n  - Bash(git status:*)\n  - Read"),
            ["Bash(git status:*)", "Read"],
        )

    def test_scoped_bash_pattern_is_not_a_bare_grant(self):
        metadata = "allowed-tools: Bash(git status:*), Read"

        self.assertEqual(bare_high_risk_grants(metadata), [])

    def test_model_invocable_skills_do_not_preapprove_bare_high_risk_tools(self):
        violations: list[str] = []

        for skill_file in sorted(CLAUDE_SKILLS.glob("*/SKILL.md")):
            metadata = frontmatter(skill_file)
            if frontmatter_value(metadata, "disable-model-invocation") == "true":
                continue

            bare_grants = bare_high_risk_grants(metadata)
            if bare_grants:
                relative = skill_file.relative_to(DOTFILES)
                violations.append(f"{relative}: {', '.join(bare_grants)}")

        self.assertEqual(
            violations,
            [],
            "Model-invocable Claude skills must not preapprove bare high-risk "
            "tools. Remove the grant or replace bare Bash with a command-scoped "
            "Bash(...) pattern:\n" + "\n".join(violations),
        )


if __name__ == "__main__":
    unittest.main()
