from __future__ import annotations

import importlib.machinery
import importlib.util
import json
import os
import sqlite3
import sys
import tempfile
import tomllib
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest import mock


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

    def test_account_registry_and_active_home_are_deduplicated(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "emacs").mkdir()
            account = root / "account"
            account.mkdir()
            alias = root / "alias"
            alias.symlink_to(account, target_is_directory=True)
            (root / "emacs/config.org").write_text(
                f'(agent-claude-accounts \'(("first" . "{account}") ("alias" . "{alias}")))\n'
                f'(agent-codex-accounts \'(("work" :home "{account}" :pool "pool")))\n'
            )
            with mock.patch.object(self.module, "ROOT", root), \
                 mock.patch.dict(os.environ, {"CLAUDE_CONFIG_DIR": str(alias), "CODEX_HOME": str(account)}, clear=True):
                for tool in ("claude", "codex"):
                    homes = self.module.account_homes(tool)
                    self.assertEqual(homes.count(account.resolve()), 1)
                    self.assertEqual(len(homes), 2)

    def test_all_collectors_count_alternate_account_usage(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            claude = root / "claude-work"
            codex = root / "codex-work"
            (claude / "projects").mkdir(parents=True)
            (codex / "sessions").mkdir(parents=True)
            now = datetime.now(timezone.utc)
            (claude / "projects/session.jsonl").write_text(json.dumps({
                "timestamp": now.isoformat(), "type": "assistant",
                "message": {"content": [{"type": "tool_use", "name": "Skill", "input": {"skill": "claude-example"}}]},
            }) + "\n")
            (codex / "sessions/session.jsonl").write_text(json.dumps({
                "timestamp": now.isoformat(), "payload": {"role": "assistant",
                    "content": "Using rollout-example skill from /codex/skills/rollout-example/SKILL.md"},
            }) + "\n")
            with sqlite3.connect(codex / "logs_2.sqlite") as connection:
                connection.execute("CREATE TABLE logs (ts INTEGER, feedback_log_body TEXT)")
                connection.execute("INSERT INTO logs VALUES (?, ?)", (int(now.timestamp()),
                    "Using sqlite-example skill from /codex/skills/sqlite-example/SKILL.md"))
            skills = [self.module.Skill(name, None, None, "paired")
                      for name in ("claude-example", "rollout-example", "sqlite-example")]
            with mock.patch.object(self.module, "account_homes", side_effect=lambda tool: [claude if tool == "claude" else codex]):
                result = self.module.scan_usage(skills, 60)
            self.assertIn("claude-example", result["claude"])
            self.assertIn("rollout-example", result["codex"])
            self.assertIn("sqlite-example", result["codex"])

    def test_missing_account_history_prevents_apply(self):
        with tempfile.TemporaryDirectory() as temporary:
            with mock.patch.object(self.module, "load_skills", return_value=[]), \
                 mock.patch.object(self.module, "load_disabled", return_value={}), \
                 mock.patch.object(self.module, "account_homes", return_value=[Path(temporary)]), \
                 mock.patch.object(self.module, "save_disabled") as save, \
                 mock.patch.object(self.module, "install") as install:
                with self.assertRaisesRegex(self.module.UsageEvidenceError, "unavailable"):
                    self.module.apply(type("Args", (), {"days": 60})())
                save.assert_not_called()
                install.assert_not_called()

    def test_unrecognized_registry_is_not_silently_omitted(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "emacs").mkdir()
            (root / "emacs/config.org").write_text('(agent-claude-accounts \'(("work" . (getenv "ACCOUNT"))))')
            with mock.patch.object(self.module, "ROOT", root):
                with self.assertRaises(self.module.UsageEvidenceError):
                    self.module.account_homes("claude")

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

    def test_update_claude_settings_writes_where_claude_reads_overrides(self):
        """User skills go to <config>/settings.json, project-local skills to the
        project's .claude/settings.local.json, and entries left in the user-level
        settings.local.json (which Claude Code never reads) are retired."""
        workspace = tempfile.TemporaryDirectory()
        self.addCleanup(workspace.cleanup)
        root = Path(workspace.name)
        # The active config dir links its settings into a shared tree, as the
        # multi-account layout does; writes must land in the shared file and
        # leave the links in place.
        shared = root / "shared"
        shared.mkdir()
        (shared / "settings.json").write_text(
            '{"model": "opus", "skillOverrides": {"auto-mode-setup": "off", "old-user": "off"}}\n'
        )
        (shared / "settings.local.json").write_text(
            '{"permissions": {"allow": ["Bash(ls)"]}, "skillOverrides": {"proj-off": "off", "user-off": "off"}}\n'
        )
        config_dir = root / "config"
        config_dir.mkdir()
        (config_dir / "settings.json").symlink_to(shared / "settings.json")
        (config_dir / "settings.local.json").symlink_to(shared / "settings.local.json")
        project = root / "project"
        proj_off = project / ".claude/skills/proj-off/SKILL.md"
        proj_on = project / ".claude/skills/proj-on/SKILL.md"
        skills = [
            self.module.Skill("user-off", "claude/skills/user-off/SKILL.md", "codex/skills/user-off/SKILL.md", "paired"),
            self.module.Skill("old-user", "claude/skills/old-user/SKILL.md", "codex/skills/old-user/SKILL.md", "paired"),
            self.module.Skill("proj-off", proj_off.as_posix(), proj_off.as_posix(), "paired-project-local", "project-local"),
            self.module.Skill("proj-on", proj_on.as_posix(), proj_on.as_posix(), "paired-project-local", "project-local"),
        ]

        original_resolve = self.module.resolve_home
        self.addCleanup(setattr, self.module, "resolve_home", original_resolve)
        self.module.resolve_home = lambda tool: config_dir
        self.module.update_claude_settings({"user-off", "proj-off"}, skills)

        self.assertTrue((config_dir / "settings.json").is_symlink())
        self.assertTrue((config_dir / "settings.local.json").is_symlink())
        user = json.loads((shared / "settings.json").read_text())
        self.assertEqual("opus", user["model"])
        self.assertEqual({"auto-mode-setup": "off", "user-off": "off"}, user["skillOverrides"])
        local = json.loads((project / ".claude/settings.local.json").read_text())
        self.assertEqual({"skillOverrides": {"proj-off": "off"}}, local)
        legacy = json.loads((shared / "settings.local.json").read_text())
        self.assertEqual({"permissions": {"allow": ["Bash(ls)"]}}, legacy)


if __name__ == "__main__":
    unittest.main()
