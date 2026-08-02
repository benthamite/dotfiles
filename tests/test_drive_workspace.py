from __future__ import annotations

import contextlib
import copy
import hashlib
import importlib.machinery
import importlib.util
import io
import json
import os
import shutil
import stat
import subprocess
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


class AuditTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.module = load_script("drive_workspace", ROOT / "bin/drive-workspace")

    def setUp(self):
        # The audit subcommand test needs a drive root the manifest schema
        # accepts, and that schema requires raw "~/"-prefixed paths, so the
        # sandbox lives in a hidden temporary directory under the home
        # directory (outside "~/My Drive"; nothing here touches Drive sync).
        self.root = Path(
            tempfile.mkdtemp(prefix=".drive-workspace-audit-", dir=Path.home())
        )
        self.addCleanup(shutil.rmtree, self.root)
        self.drive = self.root / "drive"
        self.drive.mkdir()
        self.repo = self.make_repo(self.root / "repo")

    def make_repo(self, path: Path) -> Path:
        """Create a commit-bearing repository with dirty, untracked, and ignored files."""
        path.mkdir(parents=True)
        env = dict(
            os.environ, GIT_CONFIG_GLOBAL=os.devnull, GIT_CONFIG_SYSTEM=os.devnull
        )

        def git(*args: str) -> None:
            subprocess.run(
                ["git", "-C", str(path), *args],
                check=True,
                capture_output=True,
                env=env,
            )

        git("init", "--quiet")
        git("config", "user.email", "audit-test@example.invalid")
        git("config", "user.name", "Audit Test")
        (path / ".gitignore").write_text("ignored.txt\n", encoding="utf-8")
        (path / "tracked.txt").write_text("original\n", encoding="utf-8")
        git("add", ".gitignore", "tracked.txt")
        git("commit", "--quiet", "-m", "initial")
        (path / "tracked.txt").write_text("dirty\n", encoding="utf-8")
        (path / "untracked.txt").write_text("untracked\n", encoding="utf-8")
        (path / "ignored.txt").write_text("ignored\n", encoding="utf-8")
        return path

    def tilde(self, path: Path) -> str:
        return "~/" + str(path.relative_to(Path.home()))

    def write_manifest(self) -> Path:
        document = {
            "version": 1,
            "drive_root": self.tilde(self.drive),
            "repos_root": "~/repos",
            "epoch_repos_root": "~/repos/epoch",
            "worktrees_root": "~/repos/.worktrees",
            "state_root": "~/.local/state/drive-workspace-migration",
            "workspaces": [
                {
                    "name": "sample",
                    "group": "personal",
                    "kind": "workspace",
                    "source": self.tilde(self.drive / "repo"),
                    "destination": "~/repos/drive-workspace-audit-sample",
                    "git_roots": ["."],
                    "expected_symlinks": 0,
                    "links": [],
                    "consumers": [],
                    "smoke": [{"cwd": ".", "argv": ["true"], "env": {}}],
                }
            ],
        }
        path = self.root / "manifest.json"
        path.write_text(json.dumps(document), encoding="utf-8")
        return path

    def run_audit_main(self, manifest_path: Path):
        stdout = io.StringIO()
        stderr = io.StringIO()
        with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
            code = self.module.main(["audit", "--manifest", str(manifest_path)])
        return stdout.getvalue(), stderr.getvalue(), code

    def test_scan_symlinks_reports_every_link_without_following_it(self):
        target = self.root / "outside"; target.mkdir()
        link = self.drive / "repo" / "node_modules"
        link.parent.mkdir(parents=True); link.symlink_to(target)
        self.assertEqual([str(link)], self.module.scan_symlinks(self.drive))

    def test_snapshot_records_dirty_untracked_ignored_modes_and_links(self):
        snapshot = self.module.snapshot_git(self.repo)
        self.assertIn("head", snapshot)
        self.assertIn("refs", snapshot)
        self.assertIn("status_porcelain_v2", snapshot)
        self.assertIn("untracked", snapshot)
        self.assertIn("ignored", snapshot)
        self.assertIn("symlinks", snapshot)
        self.assertIn("worktrees", snapshot)
        self.assertIn("submodules", snapshot)

    def test_unreadable_directory_is_an_audit_error_not_empty_success(self):
        blocked = self.drive / "blocked"
        blocked.mkdir()
        os.chmod(blocked, 0)
        self.addCleanup(os.chmod, blocked, 0o755)
        with self.assertRaises(self.module.AuditError):
            self.module.scan_symlinks(self.drive)

    def test_unreadable_file_makes_hashing_a_blocker(self):
        secret = self.repo / "secret.txt"
        secret.write_text("secret\n", encoding="utf-8")
        os.chmod(secret, 0)
        self.addCleanup(os.chmod, secret, 0o644)
        with self.assertRaises(self.module.AuditError):
            self.module.audit_filesystem(self.repo)

    def test_non_utf8_git_output_is_an_audit_error(self):
        # macOS APFS enforces UTF-8 filenames, so a repository with a
        # non-UTF-8 path cannot be created on this filesystem; exercise
        # the decode boundary through Git output we control instead: a
        # config value containing invalid UTF-8 bytes.
        config = self.repo / ".git" / "config"
        with open(config, "ab") as handle:
            handle.write(b"[audit]\n\tblob = caf\xe9\n")
        with self.assertRaises(self.module.AuditError):
            self.module._git_read(self.repo, "config", "audit.blob")

    def test_audit_subcommand_exits_nonzero_for_any_drive_symlink(self):
        self.make_repo(self.drive / "repo")
        manifest_path = self.write_manifest()
        stray_target = self.root / "outside"
        stray_target.mkdir()
        (self.drive / "stray-link").symlink_to(stray_target)
        stdout, stderr, code = self.run_audit_main(manifest_path)
        self.assertNotEqual(0, code)
        self.assertIn("symlink", stderr)
        self.assertIn(str(self.drive / "stray-link"), stdout)

    def test_unchanged_audit_twice_is_byte_identical_canonical_json(self):
        self.make_repo(self.drive / "repo")
        manifest_path = self.write_manifest()
        first_out, first_err, first_code = self.run_audit_main(manifest_path)
        second_out, second_err, second_code = self.run_audit_main(manifest_path)
        self.assertEqual(0, first_code, first_err)
        self.assertEqual(0, second_code, second_err)
        self.assertEqual(
            first_out.encode("utf-8"), second_out.encode("utf-8")
        )
        payload = json.loads(first_out)
        self.assertEqual(
            first_out,
            json.dumps(payload, sort_keys=True, separators=(",", ":")) + "\n",
        )
        records = {
            record["path"]: record
            for record in payload["workspaces"]["sample"]["filesystem"]
        }
        self.assertEqual(sorted(records), list(records))
        tracked = records["tracked.txt"]
        self.assertEqual(
            hashlib.sha256(b"dirty\n").hexdigest(), tracked["sha256"]
        )
        self.assertIn("dev", tracked)
        self.assertRegex(tracked["mode"], r"^0o[0-7]{3,4}$")
        self.assertEqual([], payload["symlinks"])


if __name__ == "__main__":
    unittest.main()
