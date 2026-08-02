from __future__ import annotations

import copy
import importlib.machinery
import importlib.util
import json
import os
import stat
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]

EXPECTED_DISPOSITIONS = [
    ("80000hours.global", "frontend/node_modules",
     "materialize_generated_directory", "disposable", "directory"),
    ("80000hours.global", "frontend/.next",
     "materialize_generated_directory", "disposable", "directory"),
    ("80000hours.global", ".pytest_cache", "preserve_link", "retain", "link"),
    ("80000hours.global", ".venv", "preserve_link", "retain", "link"),
    ("80000hours.global", "src/eighty_k_global/__pycache__",
     "preserve_link", "retain", "link"),
    ("80000hours.global", "src/eighty_k_global/api/__pycache__",
     "preserve_link", "retain", "link"),
    ("agent-skills", ".opencode/skills", "preserve_link", "retain", "link"),
    ("consensus-trader", ".pytest_cache", "preserve_link", "retain", "link"),
    ("consensus-trader", "consensus_trader/__pycache__",
     "preserve_link", "retain", "link"),
    ("consensus-trader", "tests/__pycache__", "preserve_link", "retain", "link"),
    ("consensus-trader", "consensus_trader.egg-info",
     "preserve_link", "retain", "link"),
    ("consensus-trader", "results", "preserve_link", "retain", "link"),
    ("consensus-trader", ".venv", "preserve_link", "retain", "link"),
    ("consensus-trader", "scripts/__pycache__", "preserve_link", "retain", "link"),
    ("consensus-trader", "data", "preserve_link", "retain", "link"),
    ("ea.news", "ea.news-front/node_modules",
     "materialize_generated_directory", "disposable", "directory"),
    ("ea.news", "ea.news-front/.next",
     "materialize_generated_directory", "disposable", "directory"),
    ("stafforini.com", ".pytest_cache", "preserve_link", "retain", "link"),
    ("stafforini.com", "node_modules",
     "materialize_generated_directory", "disposable", "directory"),
    ("stafforini.com", "tests/__pycache__", "preserve_link", "retain", "link"),
    ("stafforini.com", "public", "preserve_link", "retain", "link"),
    ("stafforini.com", "static/pdfs", "preserve_link", "retain", "link"),
    ("stafforini.com", "static/pdf-thumbnails", "preserve_link", "retain", "link"),
    ("stafforini.com", "scripts/__pycache__", "preserve_link", "retain", "link"),
    ("tangodb", "web/node_modules",
     "materialize_generated_directory", "disposable", "directory"),
    ("tangodb", "web/.svelte-kit",
     "materialize_generated_directory", "disposable", "directory"),
    ("tangodb", "admin/node_modules",
     "materialize_generated_directory", "disposable", "directory"),
    ("tangodb", "admin/.svelte-kit",
     "materialize_generated_directory", "disposable", "directory"),
    ("tangodb", "backend/app/models/__pycache__",
     "preserve_link", "retain", "link"),
    ("tangodb", "backend/app/__pycache__", "preserve_link", "retain", "link"),
    ("tangodb", "backend/app/schemas/__pycache__",
     "preserve_link", "retain", "link"),
    ("tangodb", "backend/app/api/__pycache__", "preserve_link", "retain", "link"),
    ("tangodb", "backend/.pytest_cache", "preserve_link", "retain", "link"),
    ("tangodb", "backend/tests/__pycache__", "preserve_link", "retain", "link"),
    ("tangodb", "backend/scripts/reconciliation/__pycache__",
     "preserve_link", "retain", "link"),
    ("tangodb", "backend/scripts/reconciliation/source_loaders/__pycache__",
     "preserve_link", "retain", "link"),
    ("tangodb", "backend/scripts/integration/__pycache__",
     "preserve_link", "retain", "link"),
    ("tangodb", "backend/scripts/musicbrainz_dump/__pycache__",
     "preserve_link", "retain", "link"),
    ("tangodb", "backend/scripts/utils/__pycache__",
     "preserve_link", "retain", "link"),
    ("tangodb", "backend/scripts/__pycache__", "preserve_link", "retain", "link"),
    ("tangodb", "backend/scripts/archived/__pycache__",
     "preserve_link", "retain", "link"),
    ("tangodb", "backend/.browser-data-todotango",
     "preserve_link", "retain", "link"),
    ("tangodb", "scripts/__pycache__", "preserve_link", "retain", "link"),
]


def load_script(name: str, path: Path):
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


class ManifestTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.module = load_script("drive_workspace", ROOT / "bin/drive-workspace")
        cls.manifest = cls.module.load_manifest(ROOT / "drive-workspaces.json")

    def test_roots_are_exact_and_external(self):
        self.assertEqual("~/My Drive", self.manifest.drive_root)
        self.assertEqual("~/repos", self.manifest.repos_root)
        self.assertEqual("~/repos/epoch", self.manifest.epoch_repos_root)
        self.assertEqual("~/repos/.worktrees", self.manifest.worktrees_root)

    def test_personal_scope_is_six_workspace_transactions(self):
        personal = [w for w in self.manifest.workspaces if w.group == "personal"]
        self.assertEqual(
            {
                "80000hours.global", "agent-skills", "consensus-trader",
                "ea.news", "stafforini.com", "tangodb",
            },
            {w.name for w in personal},
        )
        self.assertEqual(43, sum(w.expected_symlinks for w in personal))

    def test_ea_news_is_one_aggregate_transaction(self):
        item = self.manifest.by_name("ea.news")
        self.assertEqual("~/My Drive/repos/ea.news", item.source)
        self.assertEqual("~/repos/ea.news", item.destination)
        self.assertEqual((".", "ea.news-api", "ea.news-front"), item.git_roots)

    def test_manifest_loading_is_structural_not_live_git_validation(self):
        item = self.manifest.by_name("ea.news")
        self.assertEqual("workspace", item.kind)
        self.assertIn("ea.news-api", item.git_roots)

    def test_smokes_have_only_cwd_argv_and_env(self):
        for workspace in self.manifest.workspaces:
            for smoke in workspace.smoke:
                self.assertEqual({"cwd", "argv", "env"}, set(smoke.raw))
                self.assertIsInstance(smoke.argv, tuple)

    def test_agent_skills_has_exact_trust_consumer(self):
        item = self.manifest.by_name("agent-skills")
        self.assertIn(
            "codex-agent-skills-trust",
            {consumer.name for consumer in item.consumers},
        )

    def test_wuzapi_is_never_a_manifest_target(self):
        serialized = json.dumps(self.manifest.raw)
        self.assertNotIn("wuzapi", serialized)

    def test_script_is_executable_before_direct_invocation(self):
        path = ROOT / "bin/drive-workspace"
        self.assertEqual(0o755, stat.S_IMODE(path.stat().st_mode))

    def test_state_root_is_exact_and_external(self):
        self.assertEqual(
            "~/.local/state/drive-workspace-migration",
            self.manifest.state_root,
        )

    def test_disposition_list_is_the_exact_ordered_43_record_set(self):
        actual = [
            (
                workspace.name,
                link.path,
                link.disposition,
                link.target_policy,
                link.post_smoke_type,
            )
            for workspace in self.manifest.workspaces
            if workspace.group == "personal"
            for link in workspace.links
        ]
        self.assertEqual(EXPECTED_DISPOSITIONS, actual)


class SmokeRejectionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.module = load_script("drive_workspace", ROOT / "bin/drive-workspace")
        cls.base = json.loads(
            (ROOT / "drive-workspaces.json").read_text(encoding="utf-8")
        )

    def load_with_extra_smoke(self, smoke: dict):
        document = copy.deepcopy(self.base)
        document["workspaces"][0]["smoke"].append(smoke)
        with tempfile.NamedTemporaryFile(
            "w", suffix=".json", delete=False
        ) as handle:
            json.dump(document, handle)
        self.addCleanup(os.unlink, handle.name)
        return self.module.load_manifest(Path(handle.name))

    def test_env_wrapper_with_assignment_is_rejected(self):
        with self.assertRaises(self.module.ManifestError):
            self.load_with_extra_smoke(
                {
                    "cwd": ".",
                    "argv": ["env", "UV_CACHE_DIR=/tmp/uv-cache", "uv", "run", "pytest"],
                    "env": {},
                }
            )

    def test_env_wrapper_without_assignment_is_rejected(self):
        with self.assertRaises(self.module.ManifestError):
            self.load_with_extra_smoke(
                {"cwd": ".", "argv": ["/usr/bin/env", "python3", "-V"], "env": {}}
            )

    def test_environment_assignment_in_any_argv_position_is_rejected(self):
        with self.assertRaises(self.module.ManifestError):
            self.load_with_extra_smoke(
                {"cwd": ".", "argv": ["uv", "UV_CACHE_DIR=/somewhere"], "env": {}}
            )

    def test_shared_tmp_uv_cache_in_argv_is_rejected(self):
        with self.assertRaises(self.module.ManifestError):
            self.load_with_extra_smoke(
                {"cwd": ".", "argv": ["ls", "/tmp/uv-cache"], "env": {}}
            )

    def test_shared_tmp_uv_cache_in_env_is_rejected(self):
        with self.assertRaises(self.module.ManifestError):
            self.load_with_extra_smoke(
                {
                    "cwd": ".",
                    "argv": ["uv", "run", "pytest", "-q"],
                    "env": {"UV_CACHE_DIR": "/tmp/uv-cache"},
                }
            )


if __name__ == "__main__":
    unittest.main()
