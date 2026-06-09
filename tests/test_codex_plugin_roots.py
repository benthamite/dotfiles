from __future__ import annotations

import importlib.machinery
import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path


DOTFILES = Path("/Users/pablostafforini/My Drive/dotfiles")


def load_script(name: str, path: Path):
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


class CodexPluginRootTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.modules = [
            load_script("agent_skill_script", DOTFILES / "bin" / "agent-skill"),
            load_script("ai_config_sync_script", DOTFILES / "bin" / "ai-config-sync"),
        ]

    def write_skill(self, skills_root: Path, name: str):
        skill_dir = skills_root / name
        skill_dir.mkdir(parents=True)
        (skill_dir / "SKILL.md").write_text(
            f"---\nname: {name}\ndescription: test skill\n---\n",
            encoding="utf-8",
        )

    def test_plugin_roots_use_enabled_current_versions_only(self):
        for module in self.modules:
            with self.subTest(module=module.__name__):
                with tempfile.TemporaryDirectory() as temp:
                    codex_home = Path(temp) / ".codex"
                    old_root = (
                        codex_home
                        / "plugins/cache/openai-curated/superpowers/oldhash/skills"
                    )
                    new_root = (
                        codex_home
                        / "plugins/cache/openai-curated/superpowers/newhash/skills"
                    )
                    disabled_root = (
                        codex_home
                        / "plugins/cache/openai-curated/browser/newhash/skills"
                    )
                    self.write_skill(old_root, "old-skill")
                    self.write_skill(new_root, "new-skill")
                    self.write_skill(disabled_root, "disabled-skill")

                    plugin_list = [
                        {
                            "name": "superpowers",
                            "marketplaceName": "openai-curated",
                            "version": "newhash",
                            "installed": True,
                            "enabled": True,
                        },
                        {
                            "name": "browser",
                            "marketplaceName": "openai-curated",
                            "version": "newhash",
                            "installed": True,
                            "enabled": False,
                        },
                    ]

                    roots = module.codex_plugin_skill_roots(codex_home, plugin_list)
                    self.assertEqual([new_root], roots)

    def test_stale_prompt_roots_report_only_inactive_plugin_paths(self):
        for module in self.modules:
            with self.subTest(module=module.__name__):
                with tempfile.TemporaryDirectory() as temp:
                    codex_home = Path(temp) / ".codex"
                    old_root = (
                        codex_home
                        / "plugins/cache/openai-curated/superpowers/oldhash/skills"
                    )
                    new_root = (
                        codex_home
                        / "plugins/cache/openai-curated/superpowers/newhash/skills"
                    )
                    self.write_skill(old_root, "old-skill")
                    self.write_skill(new_root, "new-skill")
                    prompt = (
                        f"### Skill roots\n- r5 = `{old_root}`\n"
                        f"- r6 = `{new_root}`\n"
                        "- r7 = `/tmp/not-a-plugin-root`\n"
                    )

                    stale = module.stale_codex_prompt_plugin_roots(
                        prompt,
                        active_roots=[new_root],
                    )

                    self.assertEqual([old_root], stale)


if __name__ == "__main__":
    unittest.main()
