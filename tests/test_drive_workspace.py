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
import threading
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


class RemoteCoverageTests(unittest.TestCase):
    """remote_coverage() proves every commit that matters is on some remote.

    Every remote is a local bare repository created with `git init --bare`;
    nothing in this class touches the network.  Each test snapshots every
    pre-existing ref before the audit and asserts afterwards that the refs
    are byte-identical and that the run-scoped audit namespace is absent,
    on success and failure paths alike.
    """

    @classmethod
    def setUpClass(cls):
        cls.module = load_script("drive_workspace", ROOT / "bin/drive-workspace")

    def setUp(self):
        self.root = Path(
            tempfile.mkdtemp(prefix=".drive-workspace-remote-", dir=Path.home())
        )
        self.addCleanup(shutil.rmtree, self.root)
        # Isolate every git call -- the fixtures' and the module's own --
        # from user and system configuration.
        self._saved_env = {
            name: os.environ.get(name)
            for name in ("GIT_CONFIG_GLOBAL", "GIT_CONFIG_SYSTEM")
        }
        os.environ["GIT_CONFIG_GLOBAL"] = os.devnull
        os.environ["GIT_CONFIG_SYSTEM"] = os.devnull
        self.addCleanup(self._restore_env)
        self.env = dict(os.environ)

    def _restore_env(self):
        for name, value in self._saved_env.items():
            if value is None:
                os.environ.pop(name, None)
            else:
                os.environ[name] = value

    def git(self, repo: Path, *args: str) -> str:
        completed = subprocess.run(
            ["git", "-C", str(repo), *args],
            check=True,
            capture_output=True,
            encoding="utf-8",
            env=self.env,
        )
        return completed.stdout

    def make_repo(self, path: Path) -> Path:
        path.mkdir(parents=True)
        self.git(path, "init", "--quiet", "-b", "main")
        self.git(path, "config", "user.email", "coverage-test@example.invalid")
        self.git(path, "config", "user.name", "Coverage Test")
        return path

    def make_bare(self, name: str) -> Path:
        path = self.root / name
        subprocess.run(
            ["git", "init", "--quiet", "--bare", "-b", "main", str(path)],
            check=True,
            capture_output=True,
            env=self.env,
        )
        return path

    def commit(self, repo: Path, name: str, content: str) -> str:
        (repo / name).write_text(content, encoding="utf-8")
        self.git(repo, "add", name)
        self.git(repo, "commit", "--quiet", "-m", f"add {name}")
        return self.git(repo, "rev-parse", "HEAD").strip()

    def make_published_repo(self) -> Path:
        """One repository with a single commit pushed to a bare origin."""
        repo = self.make_repo(self.root / "repo")
        self.commit(repo, "a.txt", "a\n")
        origin = self.make_bare("origin.git")
        self.git(repo, "remote", "add", "origin", str(origin))
        self.git(repo, "push", "--quiet", "origin", "main")
        return repo

    def run_coverage(self, repo: Path, *extra_repos: Path) -> dict:
        """Run remote_coverage and assert it left every repository untouched."""
        repos = (repo, *extra_repos)
        ref_format = "--format=%(refname)%00%(objectname)"
        before = {r: self.git(r, "for-each-ref", ref_format) for r in repos}
        result = self.module.remote_coverage(repo)
        for r in repos:
            self.assertEqual(before[r], self.git(r, "for-each-ref", ref_format))
            self.assertEqual(
                "", self.git(r, "for-each-ref", "refs/drive-workspace-audit")
            )
        return result

    def assert_blocked_by(self, result: dict, oid: str) -> None:
        self.assertFalse(result["ok"])
        self.assertTrue(
            any(oid in blocker for blocker in result["blockers"]),
            f"{oid} not named in blockers: {result['blockers']}",
        )

    def test_published_branch_has_no_blockers(self):
        repo = self.make_published_repo()
        result = self.run_coverage(repo)
        self.assertEqual([], result["blockers"])
        self.assertTrue(result["ok"])

    def test_unpublished_branch_commit_oid_is_a_blocker(self):
        repo = self.make_published_repo()
        self.git(repo, "switch", "--quiet", "-c", "topic")
        oid = self.commit(repo, "b.txt", "b\n")
        self.assert_blocked_by(self.run_coverage(repo), oid)

    def test_stash_commit_oid_is_a_blocker(self):
        repo = self.make_published_repo()
        (repo / "a.txt").write_text("stashed\n", encoding="utf-8")
        self.git(repo, "stash", "push", "--quiet")
        oid = self.git(repo, "rev-parse", "refs/stash").strip()
        self.assert_blocked_by(self.run_coverage(repo), oid)

    def test_detached_linked_worktree_head_is_a_blocker(self):
        repo = self.make_published_repo()
        worktree = self.root / "wt"
        self.git(repo, "worktree", "add", "--quiet", "--detach", str(worktree))
        oid = self.commit(worktree, "w.txt", "w\n")
        self.assert_blocked_by(self.run_coverage(repo), oid)

    def test_checked_out_submodule_head_is_a_blocker(self):
        sub_origin = self.make_bare("sub-origin.git")
        seed = self.make_repo(self.root / "sub-seed")
        self.commit(seed, "s.txt", "s\n")
        self.git(seed, "remote", "add", "origin", str(sub_origin))
        self.git(seed, "push", "--quiet", "origin", "main")

        superproject = self.make_repo(self.root / "super")
        self.commit(superproject, "base.txt", "base\n")
        # protocol.file.allow is a test-fixture concession: modern git
        # blocks local-path submodule clones by default.  The tool under
        # test never sets it.
        self.git(
            superproject, "-c", "protocol.file.allow=always",
            "submodule", "add", "--quiet", str(sub_origin), "sub",
        )
        self.git(superproject, "commit", "--quiet", "-m", "add submodule")
        origin = self.make_bare("super-origin.git")
        self.git(superproject, "remote", "add", "origin", str(origin))
        self.git(superproject, "push", "--quiet", "origin", "main")

        sub = superproject / "sub"
        self.git(sub, "config", "user.email", "coverage-test@example.invalid")
        self.git(sub, "config", "user.name", "Coverage Test")
        oid = self.commit(sub, "extra.txt", "x\n")
        self.assert_blocked_by(self.run_coverage(superproject, sub), oid)

    def test_advertised_non_head_ref_provides_coverage(self):
        repo = self.make_published_repo()
        self.git(repo, "switch", "--quiet", "-c", "topic")
        self.commit(repo, "b.txt", "b\n")
        self.git(repo, "push", "--quiet", "origin", "topic:refs/odd/keep")
        result = self.run_coverage(repo)
        self.assertEqual([], result["blockers"])

    def test_annotated_tag_target_is_required_until_the_tag_is_pushed(self):
        repo = self.make_published_repo()
        first = self.git(repo, "rev-parse", "HEAD").strip()
        tagged = self.commit(repo, "b.txt", "b\n")
        self.git(repo, "tag", "-a", "v1", "-m", "v1")
        self.git(repo, "reset", "--hard", "--quiet", first)
        self.assert_blocked_by(self.run_coverage(repo), tagged)
        self.git(repo, "push", "--quiet", "origin", "v1")
        covered = self.run_coverage(repo)
        self.assertEqual([], covered["blockers"])

    def test_unreachable_remote_is_a_blocker_not_a_crash(self):
        repo = self.make_repo(self.root / "repo")
        self.commit(repo, "a.txt", "a\n")
        self.git(repo, "remote", "add", "origin", str(self.root / "missing.git"))
        result = self.run_coverage(repo)
        self.assertFalse(result["ok"])
        self.assertTrue(
            any("origin" in blocker for blocker in result["blockers"]),
            result["blockers"],
        )


class TransactionTests(unittest.TestCase):
    """Journaled transaction layer: hash-chained JSONL plus atomic moves.

    Everything runs against disposable temporary trees and repositories
    under a hidden directory in the home directory; the state root is
    pointed at a per-test directory through DRIVE_WORKSPACE_STATE_ROOT so
    the real ~/.local/state is never written.  Native-Drive and cloud
    evidence comes from monkeypatched provider seams; the fail-closed
    default (no provider) must be a blocker, never a silent success.
    """

    @classmethod
    def setUpClass(cls):
        cls.module = load_script("drive_workspace", ROOT / "bin/drive-workspace")

    def setUp(self):
        self.root = Path(
            tempfile.mkdtemp(prefix=".drive-workspace-txn-", dir=Path.home())
        )
        self.addCleanup(shutil.rmtree, self.root, ignore_errors=True)
        self._saved_env = {
            name: os.environ.get(name)
            for name in (
                "GIT_CONFIG_GLOBAL",
                "GIT_CONFIG_SYSTEM",
                "DRIVE_WORKSPACE_STATE_ROOT",
            )
        }
        os.environ["GIT_CONFIG_GLOBAL"] = os.devnull
        os.environ["GIT_CONFIG_SYSTEM"] = os.devnull
        self.state = self.root / "state"
        os.environ["DRIVE_WORKSPACE_STATE_ROOT"] = str(self.state)
        self.addCleanup(self._restore_env)
        self.env = dict(os.environ)

        self.module.DRIVE_STATE_PROVIDER = None
        self.module.DRIVE_PAUSE_PROVIDER = None
        self.module.SESSION_ADAPTERS = {}
        self.adapter_calls = []

        self.drive = self.root / "drive"
        self.drive.mkdir()
        self.repos = self.root / "repos"
        self.repos.mkdir()
        (self.root / "origins").mkdir()
        self.targets = self.root / "targets"
        (self.targets / "node_modules").mkdir(parents=True)
        (self.targets / "node_modules" / "dep.js").write_text(
            "dep\n", encoding="utf-8"
        )
        (self.targets / "venv").mkdir()
        (self.targets / "venv" / "bin.txt").write_text("bin\n", encoding="utf-8")

        self.old = str(self.drive / "repo")
        self.new = str(self.repos / "repo")
        self.source = self.make_pushed_repo(self.drive / "repo")
        (self.source / "node_modules").symlink_to(self.targets / "node_modules")
        (self.source / ".venv").symlink_to(self.targets / "venv")
        self.destination = self.repos / "repo"
        self.consumer_repo = self.make_consumer_repo()
        self.journal = self.state / "txn-1" / "journal.jsonl"
        self.manifest_path = self.write_manifest()

    def _restore_env(self):
        for name, value in self._saved_env.items():
            if value is None:
                os.environ.pop(name, None)
            else:
                os.environ[name] = value

    def git(self, repo: Path, *args: str) -> str:
        completed = subprocess.run(
            ["git", "-C", str(repo), *args],
            check=True,
            capture_output=True,
            encoding="utf-8",
            env=self.env,
        )
        return completed.stdout

    def make_pushed_repo(self, path: Path) -> Path:
        path.mkdir(parents=True)
        self.git(path, "init", "--quiet", "-b", "main")
        self.git(path, "config", "user.email", "txn-test@example.invalid")
        self.git(path, "config", "user.name", "Txn Test")
        (path / ".gitignore").write_text(
            "node_modules\n.venv\ncache\nsmoke-out.json\nnever-ran.marker\n",
            encoding="utf-8",
        )
        (path / "tracked.txt").write_text("original\n", encoding="utf-8")
        (path / "smoke_check.py").write_text(
            "import json, os\n"
            "with open('smoke-out.json', 'w') as handle:\n"
            "    json.dump({'cwd': os.getcwd(),"
            " 'tmp': os.environ.get('SMOKE_TMP', '')}, handle)\n",
            encoding="utf-8",
        )
        (path / "smoke_fail.py").write_text(
            "import sys\nsys.exit(3)\n", encoding="utf-8"
        )
        (path / "smoke_never.py").write_text(
            "open('never-ran.marker', 'w').close()\n", encoding="utf-8"
        )
        self.git(path, "add", "-A")
        self.git(path, "commit", "--quiet", "-m", "initial")
        bare = self.root / "origins" / f"{path.name}.git"
        subprocess.run(
            ["git", "init", "--quiet", "--bare", "-b", "main", str(bare)],
            check=True,
            capture_output=True,
            env=self.env,
        )
        self.git(path, "remote", "add", "origin", str(bare))
        self.git(path, "push", "--quiet", "origin", "main")
        return path

    def make_consumer_repo(self) -> Path:
        repo = self.root / "consumers"
        repo.mkdir()
        self.git(repo, "init", "--quiet", "-b", "main")
        self.git(repo, "config", "user.email", "txn-test@example.invalid")
        self.git(repo, "config", "user.name", "Txn Test")
        self.config_path = repo / "config.txt"
        self.committed_config = f"alpha {self.old}\nbeta {self.old}\n"
        self.config_path.write_text(self.committed_config, encoding="utf-8")
        self.git(repo, "add", "config.txt")
        self.git(repo, "commit", "--quiet", "-m", "config")
        self.dirty_line = "gamma untouched\n"
        self.config_path.write_text(
            self.committed_config + self.dirty_line, encoding="utf-8"
        )
        self.preimage_config = self.committed_config + self.dirty_line
        return repo

    def tilde(self, path: Path) -> str:
        return "~/" + str(path.relative_to(Path.home()))

    def link_record(self, name: str) -> dict:
        link = self.source / name
        generated = name == "node_modules"
        disposition = (
            "materialize_generated_directory" if generated else "preserve_link"
        )
        return {
            "path": name,
            "lstat_type": "symlink",
            "lstat_mode": f"0o{stat.S_IMODE(os.lstat(link).st_mode):o}",
            "link_text": os.readlink(link),
            "disposition": disposition,
            "target_policy": "disposable" if generated else "retain",
            "post_smoke_type": "directory" if generated else "link",
        }

    def write_manifest(
        self,
        smoke: list | None = None,
        consumer_count: int = 2,
        include_worktree: bool = False,
        link_names: list | None = None,
        extra_consumers: list | None = None,
    ) -> Path:
        names = link_names or ["node_modules", ".venv"]
        workspace = {
            "name": "sample",
            "group": "personal",
            "kind": "workspace",
            "source": self.tilde(self.source),
            "destination": self.tilde(self.destination),
            "git_roots": ["."],
            "expected_symlinks": len(names),
            "links": [self.link_record(name) for name in names],
            "consumers": [
                {
                    "name": "config",
                    "kind": "text",
                    "path": self.tilde(self.config_path),
                    "old": self.old,
                    "new": self.new,
                    "count": consumer_count,
                    "session_adapter": "test-adapter",
                }
            ] + (extra_consumers or []),
            "smoke": smoke
            or [
                {
                    "cwd": ".",
                    "argv": ["python3", "smoke_check.py"],
                    "env": {"SMOKE_TMP": "{transaction_tmp}/scratch"},
                }
            ],
        }
        workspaces = [workspace]
        if include_worktree:
            workspaces.append(
                {
                    "name": "wt",
                    "group": "worktree",
                    "kind": "linked_worktree",
                    "source": self.tilde(self.worktree_source),
                    "destination": self.tilde(self.repos / "wt"),
                    "owner": self.tilde(self.owner),
                    "branch": "topic",
                    "expected_symlinks": 0,
                    "consumers": [],
                    "smoke": [{"cwd": ".", "argv": ["true"], "env": {}}],
                }
            )
        document = {
            "version": 1,
            "drive_root": self.tilde(self.drive),
            "repos_root": self.tilde(self.repos),
            "epoch_repos_root": self.tilde(self.repos / "epoch"),
            "worktrees_root": self.tilde(self.repos / ".worktrees"),
            "state_root": "~/.local/state/drive-workspace-migration",
            "workspaces": workspaces,
        }
        path = self.root / "manifest.json"
        path.write_text(json.dumps(document), encoding="utf-8")
        return path

    def add_worktree_fixture(self):
        self.owner = self.make_pushed_repo(self.drive / "owner")
        self.worktree_source = self.drive / "wt"
        self.git(
            self.owner, "worktree", "add", "--quiet", "-b", "topic",
            str(self.worktree_source),
        )
        self.git(self.owner, "push", "--quiet", "origin", "topic")
        self.wt_journal = self.state / "txn-wt" / "journal.jsonl"
        self.manifest_path = self.write_manifest(include_worktree=True)

    def run_main(self, *argv):
        stdout = io.StringIO()
        stderr = io.StringIO()
        with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
            code = self.module.main([str(item) for item in argv])
        return stdout.getvalue(), stderr.getvalue(), code

    def ok(self, *argv):
        stdout, stderr, code = self.run_main(*argv)
        self.assertEqual(0, code, f"argv={argv!r} stderr={stderr!r}")
        return stdout, stderr

    def expect_fail(self, *argv):
        stdout, stderr, code = self.run_main(*argv)
        self.assertNotEqual(0, code, f"argv={argv!r} stdout={stdout!r}")
        return stdout, stderr

    def install_providers(self, **overrides):
        evidence = {
            "rows": [{"path": "repo", "id": "obj-1"}],
            "queue_cursors": {"outbound": 7},
            "cloud": {
                "object_id": "obj-1",
                "parent_id": "parent-1",
                "duplicates": [],
            },
        }
        evidence.update(overrides)
        self.module.DRIVE_STATE_PROVIDER = (
            lambda name: copy.deepcopy(evidence)
        )
        self.module.DRIVE_PAUSE_PROVIDER = (
            lambda name: {"paused": True, "queue_advanced": False}
        )
        self.module.SESSION_ADAPTERS = {
            "test-adapter": lambda consumer: self.adapter_calls.append(
                consumer["name"]
            )
        }
        return evidence

    def capture(self, name="sample", journal=None):
        self.ok(
            "capture", name,
            "--journal", journal or self.journal,
            "--manifest", self.manifest_path,
        )

    def prepare_move(self, name="sample", journal=None):
        journal = journal or self.journal
        self.install_providers()
        self.capture(name, journal)
        self.ok("record-drive-state", name, "--journal", journal)
        self.ok("confirm-drive-paused", name, "--journal", journal)

    def do_move(self, name="sample", journal=None):
        journal = journal or self.journal
        self.prepare_move(name, journal)
        self.ok(
            "move", name, "--journal", journal,
            "--manifest", self.manifest_path,
        )

    def events(self, journal=None):
        journal = journal or self.journal
        return [
            json.loads(line)
            for line in journal.read_text(encoding="utf-8").splitlines()
        ]

    def event_types(self, journal=None):
        return [event["event"] for event in self.events(journal)]

    def compute_event_hash(self, event: dict) -> str:
        body = {
            key: value for key, value in event.items() if key != "event_hash"
        }
        canonical = (
            json.dumps(body, sort_keys=True, separators=(",", ":")) + "\n"
        )
        return hashlib.sha256(canonical.encode("utf-8")).hexdigest()

    def craft_line(
        self,
        journal: Path,
        event_type: str,
        payload: dict | None = None,
        sequence: int | None = None,
        previous_hash: str | None = None,
        event_hash: str | None = None,
    ) -> None:
        lines = journal.read_bytes().splitlines(keepends=True)
        last = lines[-1]
        last_event = json.loads(last)
        event = {
            "event": event_type,
            "transaction_id": last_event["transaction_id"],
            "sequence": (
                last_event["sequence"] + 1 if sequence is None else sequence
            ),
            "previous_hash": (
                hashlib.sha256(last).hexdigest()
                if previous_hash is None
                else previous_hash
            ),
            "time_utc": "2026-01-01T00:00:00+00:00",
            "payload": payload or {},
        }
        event["event_hash"] = (
            self.compute_event_hash(event) if event_hash is None else event_hash
        )
        with open(journal, "ab") as handle:
            handle.write(
                json.dumps(event, sort_keys=True, separators=(",", ":")).encode(
                    "utf-8"
                )
                + b"\n"
            )

    def make_baseline(self) -> Path:
        baseline = self.state / "baseline" / "journal.jsonl"
        self.ok(
            "init-journal", "--journal", baseline,
            "--kind", "observation", "--label", "baseline",
        )
        self.ok("record-drive-state", "sample", "--journal", baseline)
        return baseline

    def success_stdout(self, journal: Path) -> str:
        return json.dumps(
            {"journal": str(journal)}, separators=(",", ":")
        ) + "\n"

    # ------------------------------------------------------------------
    # Journal creation and hash chain
    # ------------------------------------------------------------------

    def test_capture_creates_secure_journal_and_prints_exact_path(self):
        stdout, _stderr = self.ok(
            "capture", "sample",
            "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertEqual(self.success_stdout(self.journal), stdout)
        self.assertEqual(
            0o700, stat.S_IMODE(os.stat(self.journal.parent).st_mode)
        )
        self.assertEqual(0o600, stat.S_IMODE(os.stat(self.journal).st_mode))
        events = self.events()
        header = events[0]
        self.assertEqual("header", header["event"])
        self.assertEqual(0, header["sequence"])
        self.assertIsNone(header["previous_hash"])
        self.assertEqual(1, header["schema_version"])
        self.assertEqual("sample", header["workspace"])
        self.assertEqual(str(self.source), header["source"])
        self.assertEqual(str(self.destination), header["destination"])
        self.assertTrue(header["transaction_id"])
        self.assertIn("time_utc", header)
        self.assertEqual("capture", events[1]["event"])
        capture_payload = events[1]["payload"]
        self.assertIn("filesystem", capture_payload)
        self.assertIn("git", capture_payload)
        consumers = capture_payload["consumers"]
        self.assertEqual(1, len(consumers))
        self.assertEqual(self.preimage_config, consumers[0]["content"])
        self.assertEqual(2, consumers[0]["count"])

    def test_capture_rejects_journal_outside_state_root(self):
        outside = self.root / "elsewhere" / "journal.jsonl"
        _stdout, stderr = self.expect_fail(
            "capture", "sample",
            "--journal", outside,
            "--manifest", self.manifest_path,
        )
        self.assertIn("state root", stderr)
        self.assertFalse(outside.exists())

    def test_capture_rejects_existing_journal(self):
        self.capture()
        _stdout, stderr = self.expect_fail(
            "capture", "sample",
            "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("exist", stderr)
        self.assertEqual(2, len(self.events()))

    def test_journal_chain_matches_specified_hash_construction(self):
        self.install_providers()
        self.capture()
        self.ok("record-drive-state", "sample", "--journal", self.journal)
        lines = self.journal.read_bytes().splitlines(keepends=True)
        self.assertGreaterEqual(len(lines), 3)
        previous_line = None
        transaction_id = None
        for index, line in enumerate(lines):
            event = json.loads(line)
            self.assertEqual(index, event["sequence"])
            if transaction_id is None:
                transaction_id = event["transaction_id"]
            self.assertEqual(transaction_id, event["transaction_id"])
            if previous_line is None:
                self.assertIsNone(event["previous_hash"])
            else:
                self.assertEqual(
                    hashlib.sha256(previous_line).hexdigest(),
                    event["previous_hash"],
                )
            self.assertEqual(
                self.compute_event_hash(event), event["event_hash"]
            )
            previous_line = line

    def test_verify_journal_accepts_valid_chain(self):
        self.capture()
        stdout, _stderr = self.ok("verify-journal", "--journal", self.journal)
        self.assertEqual(self.success_stdout(self.journal), stdout)

    def test_verify_journal_rejects_unknown_event(self):
        self.capture()
        self.craft_line(self.journal, "mystery-event")
        self.expect_fail("verify-journal", "--journal", self.journal)

    def test_init_journal_creates_kinds_and_rejects_existing(self):
        observation = self.state / "obs" / "journal.jsonl"
        stdout, _stderr = self.ok(
            "init-journal", "--journal", observation,
            "--kind", "observation", "--label", "baseline",
        )
        self.assertEqual(self.success_stdout(observation), stdout)
        header = self.events(observation)[0]
        self.assertEqual("observation", header["journal_kind"])
        self.assertIsNone(header["workspace"])
        self.assertIsNone(header["source"])
        self.assertIsNone(header["destination"])
        self.expect_fail(
            "init-journal", "--journal", observation,
            "--kind", "observation", "--label", "again",
        )
        outside = self.root / "outside.jsonl"
        self.expect_fail(
            "init-journal", "--journal", outside,
            "--kind", "program", "--label", "gates",
        )
        self.assertFalse(outside.exists())

    def corrupted_variants(self):
        """Yield (label, journal_path) for every corruption case."""
        valid = self.journal.read_bytes()

        def fresh(label: str, mutate) -> tuple[str, Path]:
            directory = self.state / f"corrupt-{label}"
            directory.mkdir(mode=0o700, parents=True)
            journal = directory / "journal.jsonl"
            journal.write_bytes(valid)
            os.chmod(journal, 0o600)
            mutate(journal)
            return label, journal

        yield fresh(
            "truncated",
            lambda journal: journal.write_bytes(valid[:-10]),
        )
        yield fresh(
            "duplicate-sequence",
            lambda journal: self.craft_line(
                journal, "drive_state",
                sequence=json.loads(
                    journal.read_bytes().splitlines()[-1]
                )["sequence"],
            ),
        )
        yield fresh(
            "skipped-sequence",
            lambda journal: self.craft_line(
                journal, "drive_state",
                sequence=json.loads(
                    journal.read_bytes().splitlines()[-1]
                )["sequence"] + 2,
            ),
        )
        yield fresh(
            "wrong-previous-hash",
            lambda journal: self.craft_line(
                journal, "drive_state", previous_hash="0" * 64
            ),
        )
        yield fresh(
            "wrong-event-hash",
            lambda journal: self.craft_line(
                journal, "drive_state", event_hash="f" * 64
            ),
        )
        yield fresh(
            "broad-journal-permissions",
            lambda journal: os.chmod(journal, 0o640),
        )
        yield fresh(
            "broad-parent-permissions",
            lambda journal: os.chmod(journal.parent, 0o750),
        )

    def test_mutating_commands_reject_every_corruption_before_source(self):
        self.install_providers()
        self.capture()
        baseline = self.make_baseline()
        sentinel = (self.source / "tracked.txt").read_bytes()
        manifest = self.manifest_path
        for label, journal in self.corrupted_variants():
            commands = [
                ["record-drive-state", "sample", "--journal", journal],
                ["confirm-drive-paused", "sample", "--journal", journal],
                ["move", "sample", "--journal", journal,
                 "--manifest", manifest],
                ["apply-consumers", "sample", "--journal", journal,
                 "--manifest", manifest],
                ["verify-consumers", "sample", "--journal", journal,
                 "--manifest", manifest],
                ["stage-consumers", "sample", "--journal", journal,
                 "--manifest", manifest],
                ["verify-local", "sample", "--journal", journal,
                 "--manifest", manifest],
                ["materialize-generated-paths", "sample",
                 "--journal", journal, "--manifest", manifest],
                ["run-smoke", "sample", "--journal", journal,
                 "--manifest", manifest],
                ["rollback-generated-paths", "sample", "--journal", journal,
                 "--manifest", manifest],
                ["finalize-generated-paths", "sample", "--journal", journal,
                 "--manifest", manifest],
                ["rollback-consumers", "sample", "--journal", journal,
                 "--manifest", manifest],
                ["rollback-local", "sample", "--journal", journal,
                 "--manifest", manifest],
                ["verify-rollback", "sample", "--journal", journal,
                 "--baseline-journal", baseline, "--manifest", manifest],
                ["request-rollback-drill", "sample", "--journal", journal,
                 "--manifest", manifest],
                ["verify-journal", "--journal", journal],
            ]
            corrupted_bytes = journal.read_bytes()
            for argv in commands:
                _stdout, stderr = self.expect_fail(*argv)
                self.assertIn(
                    "journal", stderr.lower(),
                    f"{label}/{argv[0]}: stderr={stderr!r}",
                )
                self.assertEqual(
                    corrupted_bytes, journal.read_bytes(),
                    f"{label}/{argv[0]} repaired or truncated the journal",
                )
            self.assertEqual(
                sentinel, (self.source / "tracked.txt").read_bytes(),
                f"{label} touched the source tree",
            )
            self.assertFalse(
                self.destination.exists(), f"{label} created the destination"
            )

    def test_concurrent_appends_never_corrupt_the_chain(self):
        self.install_providers()
        self.capture()
        failures = []

        def worker():
            for _ in range(5):
                _stdout, stderr, code = self.run_main(
                    "record-drive-state", "sample", "--journal", self.journal
                )
                if code != 0:
                    failures.append(stderr)

        threads = [threading.Thread(target=worker) for _ in range(4)]
        # redirect_stdout swaps a process-global stream, so racing threads
        # can leak a success line past their own redirect; hold an outer
        # redirect across the whole threaded section to keep the runner's
        # output clean.
        outer_out, outer_err = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(outer_out), \
                contextlib.redirect_stderr(outer_err):
            for thread in threads:
                thread.start()
            for thread in threads:
                thread.join()
        self.assertEqual([], failures)
        self.ok("verify-journal", "--journal", self.journal)
        sequences = [event["sequence"] for event in self.events()]
        self.assertEqual(list(range(len(sequences))), sequences)
        self.assertEqual(
            20, self.event_types().count("drive_state")
        )

    # ------------------------------------------------------------------
    # Move preflight and execution
    # ------------------------------------------------------------------

    def test_move_rejects_destination_collision(self):
        self.prepare_move()
        self.destination.mkdir()
        _stdout, stderr = self.expect_fail(
            "move", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("destination", stderr)
        self.assertTrue(self.source.exists())

    def test_move_rejects_cross_device_destination(self):
        self.prepare_move()
        original = self.module._device_of

        def fake(path):
            if os.path.abspath(str(path)) == str(self.repos):
                return 1 << 40
            return original(path)

        self.module._device_of = fake
        self.addCleanup(setattr, self.module, "_device_of", original)
        _stdout, stderr = self.expect_fail(
            "move", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("device", stderr)
        self.assertTrue(self.source.exists())
        self.assertFalse(self.destination.exists())

    def test_move_rejects_changed_source_snapshot(self):
        self.prepare_move()
        (self.source / "tracked.txt").write_text("drifted\n", encoding="utf-8")
        _stdout, stderr = self.expect_fail(
            "move", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("snapshot", stderr)
        self.assertTrue(self.source.exists())
        self.assertFalse(self.destination.exists())

    def test_move_rejects_uncovered_commit(self):
        (self.source / "unpushed.txt").write_text("local\n", encoding="utf-8")
        self.git(self.source, "add", "unpushed.txt")
        self.git(self.source, "commit", "--quiet", "-m", "unpushed")
        oid = self.git(self.source, "rev-parse", "HEAD").strip()
        self.prepare_move()
        _stdout, stderr = self.expect_fail(
            "move", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn(oid, stderr)
        self.assertTrue(self.source.exists())
        self.assertFalse(self.destination.exists())

    def test_move_rejects_missing_fresh_pause_evidence(self):
        self.install_providers()
        self.capture()
        self.ok("record-drive-state", "sample", "--journal", self.journal)
        _stdout, stderr = self.expect_fail(
            "move", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("pause", stderr)
        self.assertTrue(self.source.exists())
        self.assertFalse(self.destination.exists())

    def test_record_drive_state_fails_closed_without_provider(self):
        self.capture()
        _stdout, stderr = self.expect_fail(
            "record-drive-state", "sample", "--journal", self.journal
        )
        self.assertIn("provider", stderr)
        self.assertNotIn("drive_state", self.event_types())

    def test_confirm_drive_paused_requires_provider_evidence(self):
        self.capture()
        _stdout, stderr = self.expect_fail(
            "confirm-drive-paused", "sample", "--journal", self.journal
        )
        self.assertIn("provider", stderr)
        self.module.DRIVE_PAUSE_PROVIDER = lambda name: {
            "paused": False, "queue_advanced": False,
        }
        self.expect_fail("confirm-drive-paused", "sample", "--journal", self.journal)
        self.module.DRIVE_PAUSE_PROVIDER = lambda name: {
            "paused": True, "queue_advanced": True,
        }
        self.expect_fail("confirm-drive-paused", "sample", "--journal", self.journal)
        self.assertNotIn("drive_paused", self.event_types())
        self.module.DRIVE_PAUSE_PROVIDER = lambda name: {
            "paused": True, "queue_advanced": False,
        }
        self.ok("confirm-drive-paused", "sample", "--journal", self.journal)
        self.assertEqual("drive_paused", self.event_types()[-1])

    def test_move_renames_workspace_and_journals_event(self):
        self.prepare_move()
        source_inode = os.lstat(self.source).st_ino
        stdout, _stderr = self.ok(
            "move", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertEqual(self.success_stdout(self.journal), stdout)
        self.assertFalse(self.source.exists())
        self.assertTrue(self.destination.is_dir())
        self.assertEqual(source_inode, os.lstat(self.destination).st_ino)
        self.assertEqual("moved", self.event_types()[-1])
        moved = self.events()[-1]["payload"]
        self.assertEqual(str(self.source), moved["source"])
        self.assertEqual(str(self.destination), moved["destination"])

    def test_rollback_local_restores_original_path_and_exact_snapshot(self):
        self.do_move()
        destination_inode = os.lstat(self.destination).st_ino
        self.ok(
            "rollback-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertTrue(self.source.is_dir())
        self.assertFalse(self.destination.exists())
        self.assertEqual(destination_inode, os.lstat(self.source).st_ino)
        self.assertEqual(
            "original\n",
            (self.source / "tracked.txt").read_text(encoding="utf-8"),
        )
        self.assertEqual(
            str(self.targets / "node_modules"),
            os.readlink(self.source / "node_modules"),
        )
        self.assertEqual("local_rolled_back", self.event_types()[-1])
        self.expect_fail(
            "rollback-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )

    def test_rollback_local_refuses_until_consumers_rolled_back(self):
        self.do_move()
        self.ok(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        _stdout, stderr = self.expect_fail(
            "rollback-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("consumer", stderr)
        self.assertTrue(self.destination.exists())
        self.ok(
            "rollback-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertEqual(
            self.preimage_config,
            self.config_path.read_text(encoding="utf-8"),
        )
        self.ok(
            "rollback-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertTrue(self.source.is_dir())

    def test_linked_worktree_moves_with_git_worktree_move_and_back(self):
        self.add_worktree_fixture()
        calls = []
        original = self.module._run_git

        def spy(repo, args, stdin_text=None):
            calls.append((str(repo), tuple(args)))
            return original(repo, args, stdin_text=stdin_text)

        self.module._run_git = spy
        self.addCleanup(setattr, self.module, "_run_git", original)
        self.do_move("wt", self.wt_journal)
        destination = self.repos / "wt"
        self.assertFalse(self.worktree_source.exists())
        self.assertTrue(destination.is_dir())
        self.assertTrue(
            any(call[1][:2] == ("worktree", "move") for call in calls),
            calls,
        )
        listing = self.git(self.owner, "worktree", "list", "--porcelain")
        self.assertIn(str(destination), listing)
        self.assertEqual(
            "topic",
            self.git(destination, "branch", "--show-current").strip(),
        )
        self.ok(
            "rollback-local", "wt", "--journal", self.wt_journal,
            "--manifest", self.manifest_path,
        )
        self.assertTrue(self.worktree_source.is_dir())
        self.assertFalse(destination.exists())
        listing = self.git(self.owner, "worktree", "list", "--porcelain")
        self.assertIn(str(self.worktree_source), listing)

    # ------------------------------------------------------------------
    # Consumers
    # ------------------------------------------------------------------

    def test_apply_consumers_substitutes_and_invokes_adapter(self):
        self.do_move()
        self.ok(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        content = self.config_path.read_text(encoding="utf-8")
        self.assertNotIn(self.old, content)
        self.assertEqual(2, content.count(self.new))
        self.assertIn(self.dirty_line, content)
        self.assertEqual(["config"], self.adapter_calls)
        self.assertIn("consumers_applied", self.event_types())
        self.ok(
            "verify-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertEqual("consumers_verified", self.event_types()[-1])

    def test_apply_consumers_refuses_preimage_drift(self):
        self.do_move()
        drifted = "unexpected edit\n" + self.preimage_config
        self.config_path.write_text(drifted, encoding="utf-8")
        _stdout, stderr = self.expect_fail(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("consumer", stderr)
        self.assertEqual(
            drifted, self.config_path.read_text(encoding="utf-8")
        )
        self.assertEqual([], self.adapter_calls)

    def test_apply_consumers_without_registered_adapter_fails_closed(self):
        self.do_move()
        self.module.SESSION_ADAPTERS = {}
        _stdout, stderr = self.expect_fail(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("adapter", stderr)
        self.assertEqual(
            self.preimage_config,
            self.config_path.read_text(encoding="utf-8"),
        )

    def test_apply_consumers_rolls_back_on_adapter_failure(self):
        self.do_move()

        def broken(consumer):
            raise RuntimeError("session adapter exploded")

        self.module.SESSION_ADAPTERS = {"test-adapter": broken}
        self.expect_fail(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertEqual(
            self.preimage_config,
            self.config_path.read_text(encoding="utf-8"),
        )

    def test_apply_consumers_restores_preimages_when_a_write_fails(self):
        second_dir = self.root / "consumers2"
        second_dir.mkdir()
        second = second_dir / "other.txt"
        second_preimage = f"delta {self.old}\n"
        second.write_text(second_preimage, encoding="utf-8")
        self.manifest_path = self.write_manifest(
            extra_consumers=[
                {
                    "name": "other",
                    "kind": "text",
                    "path": self.tilde(second),
                    "old": self.old,
                    "new": self.new,
                    "count": 1,
                }
            ]
        )
        self.do_move()
        # Make the second consumer's write fail at the filesystem level:
        # its parent directory becomes read-only, so the atomic temp file
        # cannot be created.  The first consumer was already substituted
        # and must be restored before the command returns nonzero.
        os.chmod(second_dir, 0o500)
        self.addCleanup(os.chmod, second_dir, 0o755)
        _stdout, stderr = self.expect_fail(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("cannot write", stderr)
        self.assertEqual(
            self.preimage_config,
            self.config_path.read_text(encoding="utf-8"),
            "first consumer's preimage must be restored",
        )
        self.assertEqual(
            second_preimage, second.read_text(encoding="utf-8")
        )
        self.assertNotIn("consumers_applied", self.event_types())
        for directory in (self.consumer_repo, second_dir):
            leaked = [
                entry.name
                for entry in directory.iterdir()
                if entry.name.startswith(".drive-workspace-")
            ]
            self.assertEqual([], leaked, f"temp files leaked in {directory}")

    def test_capture_rejects_preimage_containing_new_value(self):
        self.config_path.write_text(
            self.preimage_config + f"note {self.new}\n", encoding="utf-8"
        )
        _stdout, stderr = self.expect_fail(
            "capture", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("new value", stderr)
        self.assertFalse(self.journal.exists())

    def test_verify_consumers_rejects_remaining_old_value(self):
        self.do_move()
        self.ok(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        applied = self.config_path.read_text(encoding="utf-8")
        self.config_path.write_text(
            applied + f"stray {self.old}\n", encoding="utf-8"
        )
        self.expect_fail(
            "verify-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )

    def test_stage_consumers_stages_only_manifest_hunks(self):
        self.do_move()
        self.ok(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "stage-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        staged = self.git(self.consumer_repo, "show", ":config.txt")
        self.assertEqual(
            self.committed_config.replace(self.old, self.new), staged
        )
        self.assertEqual(
            "config.txt",
            self.git(
                self.consumer_repo, "diff", "--cached", "--name-only"
            ).strip(),
        )
        unstaged = self.git(self.consumer_repo, "diff")
        self.assertIn("gamma untouched", unstaged)
        self.assertIn("consumers_staged", self.event_types())

    def test_stage_consumers_rejects_overlapping_dirty_hunk(self):
        self.dirty_line = f"gamma {self.old}\n"
        self.preimage_config = self.committed_config + self.dirty_line
        self.config_path.write_text(self.preimage_config, encoding="utf-8")
        self.manifest_path = self.write_manifest(consumer_count=3)
        self.do_move()
        self.ok(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        _stdout, stderr = self.expect_fail(
            "stage-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("hunk", stderr)
        self.assertEqual(
            "",
            self.git(
                self.consumer_repo, "diff", "--cached", "--name-only"
            ).strip(),
        )

    def test_stage_consumers_rejects_additional_staged_path(self):
        self.do_move()
        self.ok(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        extra = self.consumer_repo / "extra.txt"
        extra.write_text("extra\n", encoding="utf-8")
        self.git(self.consumer_repo, "add", "extra.txt")
        _stdout, stderr = self.expect_fail(
            "stage-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("staged", stderr)
        self.assertEqual(
            "extra.txt",
            self.git(
                self.consumer_repo, "diff", "--cached", "--name-only"
            ).strip(),
        )

    def test_rollback_consumers_restores_preimages_and_refuses_drift(self):
        self.do_move()
        self.ok(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "stage-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        applied = self.config_path.read_text(encoding="utf-8")
        self.config_path.write_text(applied + "drift\n", encoding="utf-8")
        _stdout, stderr = self.expect_fail(
            "rollback-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("drift", stderr)
        self.config_path.write_text(applied, encoding="utf-8")
        self.ok(
            "rollback-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertEqual(
            self.preimage_config,
            self.config_path.read_text(encoding="utf-8"),
        )
        self.assertEqual(
            "",
            self.git(
                self.consumer_repo, "diff", "--cached", "--name-only"
            ).strip(),
        )

    # ------------------------------------------------------------------
    # verify-local, generated paths, smoke
    # ------------------------------------------------------------------

    def test_verify_local_requires_every_captured_link_intact(self):
        self.do_move()
        self.ok(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertEqual("local_verified", self.event_types()[-1])
        os.unlink(self.destination / ".venv")
        self.expect_fail(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )

    def test_verify_local_rejects_unclassified_symlink(self):
        self.do_move()
        (self.destination / "stray").symlink_to(self.targets / "venv")
        self.expect_fail(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )

    def test_materialize_moves_only_the_link_and_retains_target(self):
        self.do_move()
        self.ok(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        stdout, _stderr = self.ok(
            "materialize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertEqual(self.success_stdout(self.journal), stdout)
        self.assertFalse(os.path.lexists(self.destination / "node_modules"))
        self.assertTrue(
            (self.destination / ".venv").is_symlink(),
            "preserve_link entry must not be touched",
        )
        self.assertTrue(
            (self.targets / "node_modules" / "dep.js").is_file(),
            "external target must be retained",
        )
        event = [
            item for item in self.events()
            if item["event"] == "generated_paths_materialized"
        ][-1]
        records = event["payload"]["links"]
        self.assertEqual(1, len(records))
        record = records[0]
        self.assertEqual("node_modules", record["path"])
        self.assertEqual(
            str(self.targets / "node_modules"), record["link_text"]
        )
        trash_path = Path(record["trash_path"])
        self.assertTrue(trash_path.is_symlink())
        self.assertEqual(
            str(self.targets / "node_modules"), os.readlink(trash_path)
        )
        self.assertTrue(
            str(trash_path).startswith(str(self.journal.parent)),
            "trash must live under the transaction state directory",
        )

    def test_materialize_rejects_concurrent_drift(self):
        self.do_move()
        self.ok(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        link = self.destination / "node_modules"
        os.unlink(link)
        link.symlink_to(self.targets / "venv")
        _stdout, stderr = self.expect_fail(
            "materialize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("drift", stderr)
        self.assertTrue(link.is_symlink())

    def test_rollback_generated_paths_restores_exact_link(self):
        self.do_move()
        self.ok(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "materialize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        replacement = self.destination / "node_modules"
        replacement.mkdir()
        (replacement / "generated.js").write_text("gen\n", encoding="utf-8")
        self.ok(
            "rollback-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertTrue(replacement.is_symlink())
        self.assertEqual(
            str(self.targets / "node_modules"), os.readlink(replacement)
        )
        self.assertEqual(
            "generated_paths_rolled_back", self.event_types()[-1]
        )
        trashed = [
            path
            for path in (self.journal.parent / "trash").rglob("generated.js")
        ]
        self.assertEqual(1, len(trashed))

    def test_rollback_generated_paths_refuses_symlink_drift(self):
        self.do_move()
        self.ok(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "materialize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        (self.destination / "node_modules").symlink_to(self.targets / "venv")
        _stdout, stderr = self.expect_fail(
            "rollback-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("drift", stderr)

    def test_run_smoke_runs_exact_records_and_cleans_tmp(self):
        self.do_move()
        self.ok(
            "run-smoke", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        output = json.loads(
            (self.destination / "smoke-out.json").read_text(encoding="utf-8")
        )
        self.assertEqual(str(self.destination), output["cwd"])
        self.assertTrue(output["tmp"].endswith("/scratch"))
        self.assertIn("drive-workspace-", output["tmp"])
        self.assertFalse(output["tmp"].startswith(str(self.drive)))
        smoke_event = [
            item for item in self.events() if item["event"] == "smoke"
        ][-1]
        payload = smoke_event["payload"]
        self.assertEqual([0], [
            record["status"] for record in payload["results"]
        ])
        self.assertFalse(Path(payload["transaction_tmp"]).exists())

    def test_run_smoke_stops_on_first_failure_and_still_cleans(self):
        self.manifest_path = self.write_manifest(
            smoke=[
                {"cwd": ".", "argv": ["python3", "smoke_check.py"],
                 "env": {"SMOKE_TMP": "{transaction_tmp}/scratch"}},
                {"cwd": ".", "argv": ["python3", "smoke_fail.py"], "env": {}},
                {"cwd": ".", "argv": ["python3", "smoke_never.py"], "env": {}},
            ]
        )
        self.do_move()
        self.expect_fail(
            "run-smoke", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertFalse((self.destination / "never-ran.marker").exists())
        smoke_event = [
            item for item in self.events() if item["event"] == "smoke"
        ][-1]
        payload = smoke_event["payload"]
        self.assertEqual(
            [0, 3], [record["status"] for record in payload["results"]]
        )
        self.assertFalse(Path(payload["transaction_tmp"]).exists())

    def test_run_smoke_rejects_concurrent_path_drift(self):
        self.do_move()
        os.unlink(self.destination / ".venv")
        _stdout, stderr = self.expect_fail(
            "run-smoke", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("drift", stderr)
        self.assertFalse((self.destination / "smoke-out.json").exists())

    def test_verify_local_after_smoke_accepts_declared_transition(self):
        self.do_move()
        self.ok(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "materialize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "run-smoke", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        _stdout, _stderr = self.expect_fail(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        (self.destination / "node_modules").mkdir()
        self.ok(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )

    def test_verify_local_allows_interior_symlinks_after_smoke(self):
        self.do_move()
        self.ok(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "materialize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "run-smoke", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        # A regenerated node_modules legitimately contains its own
        # symlinks (npm .bin shims, venv interpreters); only the declared
        # transition at the record's own path is required.
        regenerated = self.destination / "node_modules"
        regenerated.mkdir()
        (regenerated / ".bin").mkdir()
        (regenerated / ".bin" / "tool").symlink_to(self.targets / "venv")
        self.ok(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        # Full strictness still applies outside the regenerated interior.
        (self.destination / "rogue").symlink_to(self.targets / "venv")
        _stdout, stderr = self.expect_fail(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("drift", stderr)

    # ------------------------------------------------------------------
    # finalize-generated-paths
    # ------------------------------------------------------------------

    def materialized_journal(self):
        self.do_move()
        self.ok(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "materialize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )

    def test_finalize_requires_rollback_window_closed(self):
        self.materialized_journal()
        _stdout, stderr = self.expect_fail(
            "finalize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("rollback_window_closed", stderr)
        self.assertTrue((self.targets / "node_modules").is_dir())

    def test_finalize_moves_journaled_target_and_noops_after(self):
        self.materialized_journal()
        self.module._append_event(
            self.journal, "rollback_window_closed", {}
        )
        self.ok(
            "finalize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertFalse((self.targets / "node_modules").exists())
        trashed = list((self.journal.parent / "trash").rglob("dep.js"))
        self.assertEqual(1, len(trashed))
        self.assertTrue(
            (self.targets / "venv").is_dir(),
            "retained target must never be touched",
        )
        finalized = self.event_types().count("generated_target_finalized")
        self.assertEqual(1, finalized)
        self.ok(
            "finalize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertEqual(
            1, self.event_types().count("generated_target_finalized")
        )

    def test_finalize_refuses_changed_target(self):
        self.materialized_journal()
        self.module._append_event(
            self.journal, "rollback_window_closed", {}
        )
        shutil.rmtree(self.targets / "node_modules")
        (self.targets / "node_modules").mkdir()
        _stdout, stderr = self.expect_fail(
            "finalize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("changed", stderr)
        self.assertTrue((self.targets / "node_modules").is_dir())

    def test_finalize_refuses_shared_target(self):
        (self.source / "cache").symlink_to(self.targets / "node_modules")
        self.manifest_path = self.write_manifest(
            link_names=["node_modules", ".venv", "cache"]
        )
        self.do_move()
        self.ok(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "materialize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.module._append_event(
            self.journal, "rollback_window_closed", {}
        )
        _stdout, stderr = self.expect_fail(
            "finalize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("shared", stderr)
        self.assertTrue((self.targets / "node_modules").is_dir())

    def test_finalize_refuses_recreated_materialized_link(self):
        self.materialized_journal()
        self.module._append_event(
            self.journal, "rollback_window_closed", {}
        )
        # Someone recreated the materialized link behind the journal's
        # back; the target is live-referenced and must not be trashed.
        (self.destination / "node_modules").symlink_to(
            self.targets / "node_modules"
        )
        _stdout, stderr = self.expect_fail(
            "finalize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("referenced", stderr)
        self.assertTrue((self.targets / "node_modules").is_dir())

    # ------------------------------------------------------------------
    # verify-rollback, drill, program gates, --force
    # ------------------------------------------------------------------

    def rolled_back_journal(self) -> Path:
        self.do_move()
        self.ok(
            "rollback-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        return self.make_baseline()

    def verify_rollback_argv(self, baseline: Path) -> list:
        return [
            "verify-rollback", "sample", "--journal", self.journal,
            "--baseline-journal", baseline, "--manifest", self.manifest_path,
        ]

    def test_verify_rollback_refuses_each_mismatch(self):
        baseline = self.rolled_back_journal()
        argv = self.verify_rollback_argv(baseline)

        stray = self.source / "stray.txt"
        stray.write_text("stray\n", encoding="utf-8")
        self.expect_fail(*argv)
        stray.unlink()

        self.config_path.write_text(
            self.preimage_config + "drift\n", encoding="utf-8"
        )
        self.expect_fail(*argv)
        self.config_path.write_text(self.preimage_config, encoding="utf-8")

        link = self.source / "node_modules"
        os.unlink(link)
        link.symlink_to(self.targets / "venv")
        self.expect_fail(*argv)
        os.unlink(link)
        link.symlink_to(self.targets / "node_modules")

        base = {
            "rows": [{"path": "repo", "id": "obj-1"}],
            "queue_cursors": {"outbound": 7},
            "cloud": {
                "object_id": "obj-1", "parent_id": "parent-1",
                "duplicates": [],
            },
        }
        for label, mutate in [
            ("cloud object id", lambda e: e["cloud"].update(object_id="obj-2")),
            ("cloud parent", lambda e: e["cloud"].update(parent_id="parent-2")),
            ("duplicate", lambda e: e["cloud"].update(duplicates=["obj-9"])),
            ("queue", lambda e: e.update(queue_cursors={"outbound": 9})),
            ("native rows", lambda e: e.update(
                rows=[{"path": "repo", "id": "obj-1", "extra": 1}]
            )),
        ]:
            evidence = copy.deepcopy(base)
            mutate(evidence)
            self.module.DRIVE_STATE_PROVIDER = (
                lambda name, evidence=evidence: copy.deepcopy(evidence)
            )
            self.expect_fail(*argv)
            self.assertNotIn(
                "rollback_verified", self.event_types(), label
            )

        self.module.DRIVE_STATE_PROVIDER = (
            lambda name: copy.deepcopy(base)
        )
        self.ok(*argv)
        self.assertEqual("rollback_verified", self.event_types()[-1])
        self.assertEqual(
            1, self.event_types().count("rollback_verified")
        )

    def test_request_rollback_drill_validates_convergence(self):
        self.install_providers()
        self.capture()
        self.ok("record-drive-state", "sample", "--journal", self.journal)
        self.module.DRIVE_STATE_PROVIDER = None
        _stdout, stderr = self.expect_fail(
            "request-rollback-drill", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertIn("provider", stderr)
        self.install_providers(
            cloud={
                "object_id": "obj-1", "parent_id": "parent-1",
                "duplicates": ["obj-9"],
            }
        )
        self.expect_fail(
            "request-rollback-drill", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.install_providers()
        (self.source / "drifted.txt").write_text("x\n", encoding="utf-8")
        self.expect_fail(
            "request-rollback-drill", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        (self.source / "drifted.txt").unlink()
        self.assertNotIn("rollback_drill_requested", self.event_types())
        self.ok(
            "request-rollback-drill", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertEqual(
            "rollback_drill_requested", self.event_types()[-1]
        )

    def test_record_program_gate_validates_evidence(self):
        self.install_providers()
        self.capture()
        self.ok("record-drive-state", "sample", "--journal", self.journal)
        program = self.state / "program" / "journal.jsonl"
        self.ok(
            "init-journal", "--journal", program,
            "--kind", "program", "--label", "gates",
        )
        self.ok(
            "record-program-gate", "--journal", program,
            "--gate", "native_record_convergence_passed",
            "--evidence-journal", self.journal,
        )
        gate_event = self.events(program)[-1]
        self.assertEqual("program_gate", gate_event["event"])
        self.assertEqual(
            "native_record_convergence_passed",
            gate_event["payload"]["gate"],
        )
        _stdout, stderr = self.expect_fail(
            "record-program-gate", "--journal", program,
            "--gate", "after_resume_rollback_passed",
            "--evidence-journal", self.journal,
        )
        self.assertIn("rollback_verified", stderr)
        bare_journal = self.state / "txn-bare" / "journal.jsonl"
        self.ok(
            "capture", "sample", "--journal", bare_journal,
            "--manifest", self.manifest_path,
        )
        _stdout, stderr = self.expect_fail(
            "record-program-gate", "--journal", program,
            "--gate", "native_record_convergence_passed",
            "--evidence-journal", bare_journal,
        )
        self.assertIn("drive_state", stderr)
        observation = self.state / "obs2" / "journal.jsonl"
        self.ok(
            "init-journal", "--journal", observation,
            "--kind", "observation", "--label", "not-a-program",
        )
        self.expect_fail(
            "record-program-gate", "--journal", observation,
            "--gate", "native_record_convergence_passed",
            "--evidence-journal", self.journal,
        )
        with self.assertRaises(SystemExit):
            with contextlib.redirect_stderr(io.StringIO()):
                self.module.main([
                    "record-program-gate", "--journal", str(program),
                    "--gate", "made_up_gate",
                    "--evidence-journal", str(self.journal),
                ])

    def test_no_subcommand_accepts_force(self):
        argvs = [
            ["capture", "sample", "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["init-journal", "--journal", str(self.journal),
             "--kind", "observation", "--label", "x"],
            ["verify-journal", "--journal", str(self.journal)],
            ["record-drive-state", "sample", "--journal", str(self.journal)],
            ["confirm-drive-paused", "sample", "--journal", str(self.journal)],
            ["move", "sample", "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["apply-consumers", "sample", "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["verify-consumers", "sample", "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["stage-consumers", "sample", "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["verify-local", "sample", "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["materialize-generated-paths", "sample",
             "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["run-smoke", "sample", "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["rollback-generated-paths", "sample",
             "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["finalize-generated-paths", "sample",
             "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["rollback-consumers", "sample", "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["rollback-local", "sample", "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["verify-rollback", "sample", "--journal", str(self.journal),
             "--baseline-journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["request-rollback-drill", "sample",
             "--journal", str(self.journal),
             "--manifest", str(self.manifest_path)],
            ["record-program-gate", "--journal", str(self.journal),
             "--gate", "native_record_convergence_passed",
             "--evidence-journal", str(self.journal)],
            ["audit", "--manifest", str(self.manifest_path)],
        ]
        for argv in argvs:
            stderr = io.StringIO()
            with self.assertRaises(SystemExit, msg=argv[0]):
                with contextlib.redirect_stderr(stderr):
                    self.module.main(argv + ["--force"])
            self.assertIn("--force", stderr.getvalue(), argv[0])


if __name__ == "__main__":
    unittest.main()
