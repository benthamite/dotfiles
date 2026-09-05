"""Resolver contracts tested exclusively against disposable catalogs and metadata."""

from __future__ import annotations

from contextlib import redirect_stderr, redirect_stdout
import importlib.machinery
import importlib.util
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


DOTFILES = Path(__file__).resolve().parents[1]


def load_resolver():
    loader = importlib.machinery.SourceFileLoader("isolated_agent_skill", str(DOTFILES / "bin/agent-skill"))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[loader.name] = module
    loader.exec_module(module)
    return module


class AgentSkillTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.resolver = load_resolver()
        cls.plugin_loader = staticmethod(cls.resolver.load_codex_plugin_list)

    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="agent-skill-test-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name).resolve()
        self.home = self.root / "home"
        self.project = self.root / "workspace/project"
        self.cwd = self.project / "subdir"
        self.cwd.mkdir(parents=True)
        (self.project / ".git").mkdir()
        self.codex_home = self.root / "codex-account"
        self.claude_home = self.root / "claude-account"
        self.admin = self.root / "admin/skills"
        self.inventory = mock.Mock(return_value=[])
        patches = (
            mock.patch.object(self.resolver, "HOME", self.home),
            mock.patch.object(self.resolver, "REPO_ROOT", self.root / "dotfiles"),
            mock.patch.object(self.resolver, "CODEX_ADMIN_SKILLS", self.admin),
            mock.patch.object(self.resolver, "load_codex_plugin_list", self.inventory),
            mock.patch.object(
                self.resolver, "existing_dir",
                side_effect=lambda path: self.root in path.parents and path.is_dir(),
            ),
            mock.patch.dict(os.environ, {
                "PATH": os.defpath,
                "CODEX_HOME": str(self.codex_home),
                "CLAUDE_CONFIG_DIR": str(self.claude_home),
            }, clear=True),
        )
        for patch in patches:
            patch.start()
            self.addCleanup(patch.stop)

    def skill(self, root, folder, name=None):
        path = root / folder / "SKILL.md"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(f"---\nname: {name or folder}\ndescription: fixture\n---\n", encoding="utf-8")
        return path

    def registry(self, entries, enabled=None):
        self.claude_home.mkdir(parents=True, exist_ok=True)
        (self.claude_home / "settings.json").write_text(json.dumps({
            "enabledPlugins": enabled if enabled is not None else {key: True for key in entries},
        }))
        plugins = self.claude_home / "plugins"
        plugins.mkdir(exist_ok=True)
        (plugins / "installed_plugins.json").write_text(json.dumps({"version": 2, "plugins": entries}))

    def roots(self, tool):
        return self.resolver.discover_roots(self.cwd, tool)

    def test_standard_codex_roots_and_legacy_roots_have_bounded_scope(self):
        for path in (
            self.cwd / ".agents/skills", self.project / ".agents/skills",
            self.project.parent / ".agents/skills", self.home / ".agents/skills",
            self.cwd / ".codex/skills", self.project.parent / ".codex/skills",
            self.codex_home / "skills", self.admin,
        ):
            self.skill(path, "fixture")

        roots = self.roots("codex")
        priorities = {root.path: root.priority for root in roots}

        self.assertEqual(10, priorities[self.cwd / ".agents/skills"])
        self.assertEqual(11, priorities[self.project / ".agents/skills"])
        self.assertNotIn(self.project.parent / ".agents/skills", priorities)
        self.assertIn(self.project.parent / ".codex/skills", priorities)
        self.assertEqual(100, priorities[self.home / ".agents/skills"])
        self.assertEqual(100, priorities[self.codex_home / "skills"])
        self.assertEqual(300, priorities[self.admin])
        self.inventory.assert_called_once_with(self.codex_home)

    def test_standard_roots_stop_at_worktree_file_or_cwd_outside_git(self):
        (self.project / ".git").rmdir()
        (self.project / ".git").write_text("gitdir: /fixture-only/not-read\n")
        self.assertEqual([self.cwd, self.project], self.resolver.codex_project_ancestors(self.cwd))
        outside = self.root / "outside"
        outside.mkdir()
        self.assertEqual([outside], self.resolver.codex_project_ancestors(outside))

    def test_precedence_is_deterministic_and_same_priority_conflicts_refuse(self):
        preferred = self.skill(self.cwd / ".agents/skills", "shared")
        self.skill(self.home / ".agents/skills", "shared")
        skills = self.resolver.discover_skills(self.roots("codex"))
        self.assertEqual(preferred, self.resolver.resolve_skill(list(reversed(skills)), "shared").path)
        self.skill(self.cwd / ".codex/skills", "other-folder", "shared")
        skills = self.resolver.discover_skills(self.roots("codex"))
        with self.assertRaisesRegex(SystemExit, "Ambiguous"):
            self.resolver.resolve_skill(skills, "shared")

    def test_claude_native_account_variable_wins_without_default_fallback(self):
        native = self.skill(self.claude_home / "skills", "native")
        legacy = self.root / "legacy-account"
        self.skill(legacy / "skills", "legacy")
        self.skill(self.home / ".claude/skills", "default")
        os.environ["CLAUDE_HOME"] = str(legacy)
        skills = self.resolver.discover_skills(self.roots("claude"))
        self.assertEqual([native], [skill.path for skill in skills])
        os.environ["CLAUDE_CONFIG_DIR"] = str(self.root / "empty-account")
        self.assertEqual([], self.roots("claude"))

    def test_legacy_claude_home_is_used_only_when_native_variable_is_unset(self):
        legacy = self.root / "legacy-account"
        expected = self.skill(legacy / "skills", "legacy")
        os.environ.pop("CLAUDE_CONFIG_DIR")
        os.environ["CLAUDE_HOME"] = str(legacy)
        skills = self.resolver.discover_skills(self.roots("claude"))
        self.assertEqual([expected], [skill.path for skill in skills])

    def test_claude_registry_selects_current_version_not_stale_cache(self):
        cache = self.claude_home / "plugins/cache/market/sample"
        self.skill(cache / "old/skills", "shared")
        expected = self.skill(cache / "current/skills", "shared")
        self.registry({"sample@market": [{
            "scope": "user", "installPath": str(cache / "current"), "version": "current",
        }]})

        skills = self.resolver.discover_skills(self.roots("claude"))

        self.assertEqual([expected], [skill.path for skill in skills])
        self.assertEqual(expected, self.resolver.resolve_skill(skills, "sample:shared").path)

    def test_external_claude_install_paths_keep_name_and_folder_qualified_aliases(self):
        install = self.root / "external-install"
        expected = self.skill(install / "skills", "folder-alias", "declared-name")
        self.registry({"sample@market": [{
            "scope": "user", "installPath": str(install), "version": "current",
        }]})
        skills = self.resolver.discover_skills(self.roots("claude"))
        for name in ("sample:declared-name", "sample:folder-alias", "sample@market:declared-name"):
            with self.subTest(name=name):
                self.assertEqual(expected, self.resolver.resolve_skill(skills, name).path)

    def test_codex_plugin_aliases_ignore_earlier_cache_directory_components(self):
        codex_home = self.root / "cache/accounts/codex"
        os.environ["CODEX_HOME"] = str(codex_home)
        expected = self.skill(codex_home / "plugins/cache/market/sample/current/skills", "shared")
        self.inventory.return_value = [{
            "name": "sample", "marketplaceName": "market", "version": "current",
            "installed": True, "enabled": True,
        }]
        skills = self.resolver.discover_skills(self.roots("codex"))
        self.assertEqual(expected, self.resolver.resolve_skill(skills, "sample@market:shared").path)
        self.assertEqual(expected, self.resolver.resolve_skill(skills, "sample:shared").path)

    def test_claude_project_and_local_installs_are_filtered_by_project_path(self):
        applicable = self.root / "applicable-install"
        unrelated = self.root / "unrelated-install"
        expected = self.skill(applicable / "skills", "shared")
        self.skill(unrelated / "skills", "shared")
        for scope in ("project", "local"):
            with self.subTest(scope=scope):
                self.registry({"sample@market": [
                    {"scope": scope, "projectPath": str(self.project), "installPath": str(applicable)},
                    {"scope": scope, "projectPath": str(self.root / "other-project"), "installPath": str(unrelated)},
                ]})
                skills = self.resolver.discover_skills(self.roots("claude"))
                self.assertEqual([expected], [skill.path for skill in skills])

    def test_enabled_claude_plugin_without_registry_reports_incomplete_discovery(self):
        self.claude_home.mkdir()
        (self.claude_home / "settings.json").write_text(json.dumps({"enabledPlugins": {"sample@market": True}}))
        with self.assertRaisesRegex(RuntimeError, "registry.*incomplete"):
            self.roots("claude")

    def test_absent_or_disabled_claude_plugins_do_not_require_registry(self):
        self.assertEqual([], self.roots("claude"))
        self.claude_home.mkdir()
        (self.claude_home / "settings.json").write_text(json.dumps({"enabledPlugins": {"sample@market": False}}))
        self.assertEqual([], self.roots("claude"))

    def test_malformed_claude_enabled_plugins_never_become_empty_inventory(self):
        self.claude_home.mkdir()
        settings = self.claude_home / "settings.json"
        for value in ([], "", False, 0, None, {"sample@market": "false"}, {"sample@market": 1}):
            with self.subTest(value=value):
                settings.write_text(json.dumps({"enabledPlugins": value}))
                with self.assertRaisesRegex(RuntimeError, "enabledPlugins is malformed.*incomplete"):
                    self.roots("claude")

    def test_missing_or_empty_claude_enabled_plugins_are_valid_empty_inventory(self):
        self.claude_home.mkdir()
        settings = self.claude_home / "settings.json"
        for payload in ({}, {"enabledPlugins": {}}):
            with self.subTest(payload=payload):
                settings.write_text(json.dumps(payload))
                self.assertEqual([], self.roots("claude"))

    def test_codex_inventory_failures_are_explicit_and_do_not_disclose_output(self):
        canary = "fixture-private-output"
        results = (
            FileNotFoundError(canary),
            subprocess.TimeoutExpired(canary, 30),
            subprocess.CompletedProcess([], 1, stdout=canary, stderr=canary),
            subprocess.CompletedProcess([], 0, stdout=canary, stderr=""),
            subprocess.CompletedProcess([], 0, stdout=json.dumps({"unexpected": canary}), stderr=""),
            subprocess.CompletedProcess([], 0, stdout=json.dumps([canary]), stderr=""),
        )
        transport = self.plugin_loader.__globals__["subprocess"]
        for index, result in enumerate(results):
            with self.subTest(result=index):
                arguments = {"side_effect": result} if isinstance(result, Exception) else {"return_value": result}
                with mock.patch.object(transport, "run", **arguments):
                    with self.assertRaisesRegex(RuntimeError, "incomplete") as raised:
                        self.plugin_loader(self.codex_home)
                self.assertNotIn(canary, str(raised.exception))

    def test_valid_empty_codex_inventory_remains_valid_and_account_bound(self):
        transport = self.plugin_loader.__globals__["subprocess"]
        for payload in ([], {"installed": []}):
            with self.subTest(payload=payload):
                result = subprocess.CompletedProcess([], 0, stdout=json.dumps(payload), stderr="")
                with mock.patch.object(transport, "run", return_value=result) as invoked:
                    self.assertEqual([], self.plugin_loader(self.codex_home))
                self.assertEqual(str(self.codex_home), invoked.call_args.kwargs["env"]["CODEX_HOME"])
                self.assertEqual(30, invoked.call_args.kwargs["timeout"])

    def test_resolver_cli_reports_incomplete_discovery_without_traceback(self):
        self.inventory.side_effect = RuntimeError("Codex plugin inventory is unavailable; discovery is incomplete")
        stdout, stderr = io.StringIO(), io.StringIO()
        with mock.patch.object(sys, "argv", ["agent-skill", "path", "fixture", "--cwd", str(self.cwd)]):
            with redirect_stdout(stdout), redirect_stderr(stderr):
                status = self.resolver.main()
        self.assertEqual(2, status)
        self.assertEqual("", stdout.getvalue())
        self.assertIn("discovery is incomplete", stderr.getvalue())
        self.assertNotIn("Traceback", stderr.getvalue())


if __name__ == "__main__":
    unittest.main()
