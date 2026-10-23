from __future__ import annotations

import os
import re
import subprocess
import tomllib
import unittest
from pathlib import Path


DOTFILES = Path("/Users/pablostafforini/My Drive/dotfiles")
NPM_MINIMUM = (11, 10, 0)
BUN_MINIMUM = (1, 3, 0)
PIP_MINIMUM = (26, 1, 0)
# pips that cannot take the gate, with the reason.
PIP_EXEMPT = {
    Path("/usr/bin/pip3"): "Apple's system pip; not upgradable in place",
    Path.home() / ".pyenv/versions/3.9.18/bin/pip": "pip 26.1+ needs Python 3.10+",
    Path.home() / ".pyenv/versions/3.10.10/bin/pip": "Python built without ssl; cannot reach PyPI",
}


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
    match = re.match(r"(\d+)\.(\d+)(?:\.(\d+))?", version.strip())
    if not match:
        raise AssertionError(f"unexpected version: {version!r}")
    return tuple(int(part or 0) for part in match.groups())


def read_toml(path: Path) -> dict:
    with path.open("rb") as handle:
        return tomllib.load(handle)


class ReleaseAgePolicyTests(unittest.TestCase):
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

    def test_bun_enforces_three_day_release_age(self):
        bun = Path("/opt/homebrew/bin/bun")
        if not bun.exists():
            self.skipTest("bun not installed")
        self.assertGreaterEqual(version_tuple(command_output(bun, "--version")), BUN_MINIMUM)
        config = read_toml(Path.home() / ".bunfig.toml")
        self.assertEqual(config["install"]["minimumReleaseAge"], 3 * 24 * 60 * 60)

    def test_uv_enforces_three_day_release_age(self):
        config = read_toml(Path.home() / ".config/uv/uv.toml")
        self.assertEqual(config["exclude-newer"], "3 days")

    def pip_binaries(self) -> list[Path]:
        binaries = sorted((Path.home() / ".pyenv/versions").glob("*/bin/pip"))
        binaries += [Path("/opt/homebrew/bin/pip3"), Path("/usr/bin/pip3")]
        return [pip for pip in binaries if pip.exists()]

    def test_every_installed_pip_enforces_three_day_release_age(self):
        for pip in self.pip_binaries():
            if pip in PIP_EXEMPT:
                continue
            with self.subTest(pip=pip):
                version = command_output(pip, "--version").split()[1]
                self.assertGreaterEqual(
                    version_tuple(version),
                    PIP_MINIMUM,
                    f"{pip} predates duration support in uploaded-prior-to",
                )
                config = command_output(pip, "config", "list")
                self.assertRegex(config, r"(?m)^install\.uploaded-prior-to='P3D'$")

    def test_security_audit_skills_prescribe_the_npm_setting(self):
        skill_paths = (
            DOTFILES / "macos/.claude/skills/security-audit/references/supply-chain.md",
            DOTFILES / "macos/.codex/skills/security-audit/references/supply-chain.md",
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
