from __future__ import annotations

import importlib.machinery
import importlib.util
import sys
import tempfile
import tomllib
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]


def load_script(name: str, path: Path):
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


class SkillPruneTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.module = load_script("skill_prune_script", DOTFILES / "bin" / "skill-prune")

    def test_strip_codex_block_removes_a_complete_generated_region(self):
        managed = "/repo/.codex/skills/example/SKILL.md"
        text = (
            "before = true\n\n"
            f"{self.module.CODEX_BLOCK_BEGIN}\n\n"
            "[[skills.config]]\n"
            f'path = "{managed}"\n'
            "enabled = false\n"
            f"{self.module.CODEX_BLOCK_END}\n\n"
            "after = true\n"
        )

        result = self.module.strip_codex_block(text, {managed})

        self.assertEqual("before = true\n\nafter = true\n", result)

    def test_strip_codex_block_repairs_a_missing_begin_marker(self):
        managed = "/repo/.codex/skills/example/SKILL.md"
        text = (
            "before = true\n\n"
            "[[skills.config]]\n"
            f'path = "{managed}"\n'
            "enabled = false\n\n"
            "unrelated = \"preserve me\"\n\n"
            f"{self.module.CODEX_BLOCK_END}\n"
        )

        result = self.module.strip_codex_block(text, {managed})

        self.assertNotIn("skills.config", result)
        self.assertNotIn(self.module.CODEX_BLOCK_END, result)
        self.assertIn('unrelated = "preserve me"', result)

    def test_strip_codex_block_preserves_unmanaged_disabled_tables(self):
        managed = "/repo/.codex/skills/example/SKILL.md"
        unmanaged = "/repo/.codex/skills/manual/SKILL.md"
        text = (
            "[[skills.config]]\n"
            f'path = "{managed}"\n'
            "enabled = false\n\n"
            "[[skills.config]]\n"
            f'path = "{unmanaged}"\n'
            "enabled = false\n"
        )

        result = self.module.strip_codex_block(text, {managed})

        self.assertNotIn(managed, result)
        self.assertIn(unmanaged, result)

    def test_strip_codex_block_uses_exact_path_boundaries(self):
        managed = "/repo/.codex/skills/example/SKILL.md"
        neighboring = "/repo/.codex/skills/example-extra/SKILL.md"
        text = (
            "[[skills.config]]\n"
            f'path = "{neighboring}"\n'
            "enabled = false\n"
        )

        result = self.module.strip_codex_block(text, {managed})

        self.assertIn(neighboring, result)

    def test_strip_codex_block_collapses_duplicate_managed_tables(self):
        managed = "/repo/.codex/skills/example/SKILL.md"
        table = (
            "[[skills.config]]\n"
            f'path = "{managed}"\n'
            "enabled = false\n"
        )

        result = self.module.strip_codex_block(f"{table}\n{table}", {managed})

        self.assertNotIn(managed, result)
        tomllib.loads(result)

    def test_strip_codex_block_is_idempotent(self):
        managed = "/repo/.codex/skills/example/SKILL.md"
        text = (
            "before = true\n\n"
            "[[skills.config]]\n"
            f'path = "{managed}"\n'
            "enabled = false\n\n"
            f"{self.module.CODEX_BLOCK_END}\n"
        )

        first = self.module.strip_codex_block(text, {managed})

        self.assertEqual(first, self.module.strip_codex_block(first, {managed}))

    def test_strip_codex_block_rejects_malformed_toml(self):
        with self.assertRaises(tomllib.TOMLDecodeError):
            self.module.strip_codex_block("broken = [\n", set())

    def test_update_codex_config_enables_one_skill_and_keeps_another_disabled(self):
        workspace = tempfile.TemporaryDirectory()
        self.addCleanup(workspace.cleanup)
        root = Path(workspace.name)
        config = root / "codex/config.toml"
        config.parent.mkdir()
        first = root / ".codex/skills/first/SKILL.md"
        second = root / ".codex/skills/second/SKILL.md"
        config.write_text(
            "[[skills.config]]\n"
            f'path = "{first}"\n'
            "enabled = false\n\n"
            "[[skills.config]]\n"
            f'path = "{second}"\n'
            "enabled = false\n\n"
            f"{self.module.CODEX_BLOCK_END}\n",
            encoding="utf-8",
        )
        skills = [
            self.module.Skill("first", first.as_posix(), first.as_posix(), "paired-project-local"),
            self.module.Skill("second", second.as_posix(), second.as_posix(), "paired-project-local"),
        ]

        original_root = self.module.ROOT
        self.addCleanup(setattr, self.module, "ROOT", original_root)
        self.module.ROOT = root
        self.module.update_codex_config({"second"}, skills)

        result = config.read_text(encoding="utf-8")
        self.assertNotIn(first.as_posix(), result)
        self.assertEqual(1, result.count(second.as_posix()))
        self.assertEqual(1, result.count(self.module.CODEX_BLOCK_BEGIN))
        self.assertEqual(1, result.count(self.module.CODEX_BLOCK_END))
        tomllib.loads(result)

    def test_update_codex_config_disables_every_path_for_a_shared_name(self):
        workspace = tempfile.TemporaryDirectory()
        self.addCleanup(workspace.cleanup)
        root = Path(workspace.name)
        config = root / "codex/config.toml"
        config.parent.mkdir()
        config.write_text("setting = true\n", encoding="utf-8")
        first = root / "one/.codex/skills/shared/SKILL.md"
        second = root / "two/.codex/skills/shared/SKILL.md"
        skills = [
            self.module.Skill("shared", first.as_posix(), first.as_posix(), "paired-project-local"),
            self.module.Skill("shared", second.as_posix(), second.as_posix(), "paired-project-local"),
        ]

        original_root = self.module.ROOT
        self.addCleanup(setattr, self.module, "ROOT", original_root)
        self.module.ROOT = root
        self.module.update_codex_config({"shared"}, skills)

        result = config.read_text(encoding="utf-8")
        self.assertEqual(1, result.count(first.as_posix()))
        self.assertEqual(1, result.count(second.as_posix()))
        tomllib.loads(result)

    def test_update_codex_config_does_not_write_malformed_input(self):
        workspace = tempfile.TemporaryDirectory()
        self.addCleanup(workspace.cleanup)
        root = Path(workspace.name)
        config = root / "codex/config.toml"
        config.parent.mkdir()
        malformed = "broken = [\n"
        config.write_text(malformed, encoding="utf-8")

        original_root = self.module.ROOT
        self.addCleanup(setattr, self.module, "ROOT", original_root)
        self.module.ROOT = root
        with self.assertRaises(tomllib.TOMLDecodeError):
            self.module.update_codex_config(set(), [])

        self.assertEqual(malformed, config.read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
