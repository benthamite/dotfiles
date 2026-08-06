#!/usr/bin/env python3
"""End-to-end checks for proofread's Drive-safe dependency runtime."""

from __future__ import annotations

import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from urllib.parse import quote


ROOT = Path(__file__).resolve().parents[1]
CLAUDE_SKILL = ROOT / "claude" / "skills" / "proofread"
CODEX_SKILL = ROOT / "codex" / "skills" / "proofread"
INSTALL_COMMAND = "yarn -s setup-runtime"
PRECEDENCE_TEXT = "PROOFREAD_RUNTIME_DIR, then XDG_DATA_HOME, then ~/.local/share/proofread"


class ProofreadRuntimeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.temp_dir = tempfile.TemporaryDirectory()
        cls.root = Path(cls.temp_dir.name)
        cls.skill_dir = cls.root / "My Drive" / "proofread"
        cls.runtime_dir = cls.root / "external-data" / "proofread"
        shutil.copytree(CLAUDE_SKILL, cls.skill_dir)

        env = os.environ.copy()
        env["XDG_DATA_HOME"] = str(cls.root / "external-data")
        subprocess.run(
            [
                "yarn",
                "install",
                "--frozen-lockfile",
                "--modules-folder",
                str(cls.runtime_dir / "node_modules"),
            ],
            cwd=cls.skill_dir,
            env=env,
            check=True,
            capture_output=True,
            text=True,
        )
        cls.env = env

    @classmethod
    def tearDownClass(cls) -> None:
        cls.temp_dir.cleanup()

    def run_yarn(self, *args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["yarn", "-s", *args],
            cwd=self.skill_dir,
            env=self.env,
            check=False,
            capture_output=True,
            text=True,
        )

    def test_spellcheck_runs_from_external_runtime_without_drive_node_modules(self) -> None:
        source = self.root / "sample.md"
        source.write_text("This sentnce has a typo.\n", encoding="utf-8")

        result = self.run_yarn("proofread", str(source), "--engine", "spellcheck")

        self.assertEqual(result.returncode, 0, result.stderr)
        payload = json.loads(result.stdout)
        self.assertEqual(payload["file"], "sample.md")
        self.assertTrue((self.root / "sample.proofread.md").is_file())
        self.assertFalse((self.skill_dir / "node_modules").exists())
        self.assertTrue((self.runtime_dir / "node_modules" / "tsx").is_dir())

    def test_apply_runs_from_external_runtime(self) -> None:
        payload = quote(
            json.dumps(
                {"id": "S1", "text": "Fix typo", "from": "sentnce", "to": "sentence"},
                separators=(",", ":"),
            ),
            safe="",
        )
        source = self.root / "apply.proofread.md"
        source.write_text(
            f"This sentnce. <!-- proofread:{payload} -->\n",
            encoding="utf-8",
        )

        result = self.run_yarn("apply", str(source), "S1")

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            (self.root / "apply.final.md").read_text(encoding="utf-8"),
            "This sentence.\n",
        )
        self.assertFalse((self.skill_dir / "node_modules").exists())

    def test_paired_docs_use_external_runtime_setup_command(self) -> None:
        for skill_dir in (CLAUDE_SKILL, CODEX_SKILL):
            with self.subTest(skill_dir=skill_dir):
                skill = (skill_dir / "SKILL.md").read_text(encoding="utf-8")
                readme = (skill_dir / "README.md").read_text(encoding="utf-8")
                self.assertIn(INSTALL_COMMAND, skill)
                self.assertIn(INSTALL_COMMAND, readme)
                self.assertNotIn("```bash\nyarn install\n```", skill)
                self.assertNotIn("```bash\nyarn install\n```", readme)


class ProofreadRuntimeLocationTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp_dir = tempfile.TemporaryDirectory()
        self.root = Path(self.temp_dir.name)
        self.home = self.root / "home"
        self.drive = self.home / "My Drive"
        self.skill_dir = self.drive / "proofread"
        self.drive.mkdir(parents=True)
        shutil.copytree(CLAUDE_SKILL, self.skill_dir)

    def tearDown(self) -> None:
        self.temp_dir.cleanup()

    def env(self, **overrides: str) -> dict[str, str]:
        env = os.environ.copy()
        env["HOME"] = str(self.home)
        env.update(overrides)
        return env

    def run_yarn(
        self, *args: str, env: dict[str, str]
    ) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["yarn", "-s", *args],
            cwd=self.skill_dir,
            env=env,
            check=False,
            capture_output=True,
            text=True,
        )

    def test_setup_and_execution_share_proofread_override_precedence(self) -> None:
        xdg_runtime = self.root / "xdg" / "proofread"
        override_runtime = self.root / "override"
        env = self.env(
            XDG_DATA_HOME=str(self.root / "xdg"),
            PROOFREAD_RUNTIME_DIR=str(override_runtime),
        )

        setup = self.run_yarn("setup-runtime", env=env)

        self.assertEqual(setup.returncode, 0, setup.stderr)
        self.assertTrue((override_runtime / "node_modules" / "tsx").is_dir())
        self.assertFalse(xdg_runtime.exists())

        source = self.root / "precedence.md"
        source.write_text("This sentnce has a typo.\n", encoding="utf-8")
        proofread = self.run_yarn(
            "proofread", str(source), "--engine", "spellcheck", env=env
        )
        self.assertEqual(proofread.returncode, 0, proofread.stderr)

    def test_execution_rejects_xdg_runtime_inside_drive(self) -> None:
        rejected = self.drive / "runtime-data"
        env = self.env(XDG_DATA_HOME=str(rejected))
        source = self.root / "rejected.md"
        source.write_text("Text.\n", encoding="utf-8")

        result = self.run_yarn(
            "proofread", str(source), "--engine", "spellcheck", env=env
        )

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Refusing proofread runtime inside Google Drive", result.stderr)
        self.assertFalse(rejected.exists())

    def test_setup_rejects_external_symlink_resolving_into_drive(self) -> None:
        target = self.drive / "hidden-runtime"
        target.mkdir()
        alias = self.root / "external-looking-runtime"
        alias.symlink_to(target, target_is_directory=True)
        env = self.env(PROOFREAD_RUNTIME_DIR=str(alias))

        result = self.run_yarn("setup-runtime", env=env)

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Refusing proofread runtime inside Google Drive", result.stderr)
        self.assertFalse((target / "node_modules").exists())

    def test_paired_docs_define_one_runtime_precedence_model(self) -> None:
        for skill_dir in (CLAUDE_SKILL, CODEX_SKILL):
            with self.subTest(skill_dir=skill_dir):
                skill = (skill_dir / "SKILL.md").read_text(encoding="utf-8")
                readme = (skill_dir / "README.md").read_text(encoding="utf-8")
                self.assertIn(INSTALL_COMMAND, skill)
                self.assertIn(INSTALL_COMMAND, readme)
                self.assertIn(PRECEDENCE_TEXT, skill)
                self.assertIn(PRECEDENCE_TEXT, readme)


if __name__ == "__main__":
    unittest.main()
