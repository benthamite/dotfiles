from __future__ import annotations

import os
import re
import subprocess
import unittest
from pathlib import Path


DOTFILES = Path("/Users/pablostafforini/My Drive/dotfiles")
NPM_MINIMUM = (11, 10, 0)


def command_output(binary: Path, *args: str) -> str:
    env = os.environ.copy()
    env["PATH"] = f"{binary.parent}:{env['PATH']}"
    result = subprocess.run(
        [str(binary), *args],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        env=env,
        check=False,
    )
    if result.returncode:
        raise AssertionError(result.stderr)
    return result.stdout


def version_tuple(version: str) -> tuple[int, int, int]:
    match = re.fullmatch(r"(\d+)\.(\d+)\.(\d+)", version.strip())
    if not match:
        raise AssertionError(f"unexpected npm version: {version!r}")
    return tuple(map(int, match.groups()))


class NodeSupplyChainPolicyTests(unittest.TestCase):
    def npm_binaries(self) -> list[Path]:
        binaries = sorted((Path.home() / ".nvm/versions/node").glob("v*/bin/npm"))
        homebrew = Path("/opt/homebrew/bin/npm")
        if homebrew.exists():
            binaries.append(homebrew)
        return binaries

    def test_every_installed_npm_enforces_three_day_release_age(self):
        binaries = self.npm_binaries()
        self.assertTrue(binaries, "no npm installation found")

        for npm in binaries:
            with self.subTest(npm=npm):
                version = version_tuple(command_output(npm, "--version"))
                self.assertGreaterEqual(
                    version,
                    NPM_MINIMUM,
                    f"{npm} predates min-release-age support",
                )
                config = command_output(npm, "config", "ls", "-l")
                self.assertRegex(config, r"(?m)^min-release-age = 3$")
                self.assertNotRegex(config, r"(?m)^minimum-release-age =")

    def test_security_audit_skills_prescribe_the_npm_setting(self):
        skill_paths = (
            DOTFILES / "macos/.claude/skills/security-audit/SKILL.md",
            DOTFILES / "macos/.codex/skills/security-audit/SKILL.md",
        )
        for skill_path in skill_paths:
            with self.subTest(skill=skill_path):
                text = skill_path.read_text(encoding="utf-8")
                self.assertTrue(
                    "`min-release-age=3`" in text,
                    f"{skill_path} lacks the canonical npm release-age setting",
                )
                self.assertFalse(
                    "`minimum-release-age=3d`" in text,
                    f"{skill_path} still prescribes an unsupported npm setting",
                )


if __name__ == "__main__":
    unittest.main()
