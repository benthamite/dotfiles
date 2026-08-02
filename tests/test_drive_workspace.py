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
import signal
import socket
import stat
import subprocess
import sys
import tempfile
import threading
import time
import unittest
import unittest.mock
import urllib.error
import urllib.parse
import urllib.request
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


class JournalFixture(unittest.TestCase):
    """Shared journaled-transaction fixture for transaction and live-gate tests.

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

        # move's fail-closed process/Emacs gates would otherwise run the
        # real lsof and emacsclient (slow, and emacsclient fails without
        # a live Emacs server).  Default every test to explicit "no live
        # users" evidence; gate-specific tests override these seams or
        # restore the real functions over a mocked _run_tool.
        self.real_process_cwds = self.module._process_cwds
        self.real_emacs_visited_files = self.module._emacs_visited_files
        self.module._process_cwds = lambda: []
        self.module._emacs_visited_files = lambda: []
        self.addCleanup(
            setattr, self.module, "_process_cwds", self.real_process_cwds
        )
        self.addCleanup(
            setattr, self.module, "_emacs_visited_files",
            self.real_emacs_visited_files,
        )

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
        runtime_services: list | None = None,
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
        if runtime_services is not None:
            workspace["runtime_services"] = runtime_services
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


class TransactionTests(JournalFixture):
    """Journaled transaction layer: hash-chained JSONL plus atomic moves."""

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


class _FakeCloudResponse:
    def __init__(self, payload: dict):
        self._data = json.dumps(payload).encode("utf-8")

    def read(self) -> bytes:
        return self._data

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        return False


class FakeCloud:
    """In-memory Google Drive v3 plus OAuth token endpoint behind urlopen.

    Installed by patching urllib.request.urlopen, so no test ever opens a
    network connection for cloud work.  Every request is recorded as
    (method, url, body) for exactness assertions; the Authorization
    header is verified on every API call.
    """

    def __init__(self, test, files, root_id="root-1", list_hits=None,
                 incomplete=False, list_pages=None):
        self.test = test
        self.files = files
        self.root_id = root_id
        self.list_hits = list(list_hits or [])
        self.incomplete = incomplete
        # Multi-page listings: each page dict is served verbatim; the
        # pageToken parameter indexes the list ("" or absent -> page 0,
        # "N" -> page N), mirroring the nextPageToken values fixtures use.
        self.list_pages = (
            None if list_pages is None else copy.deepcopy(list_pages)
        )
        self.requests = []

    def install(self):
        patcher = unittest.mock.patch("urllib.request.urlopen", new=self)
        patcher.start()
        self.test.addCleanup(patcher.stop)
        return self

    def mutations(self):
        return [
            (method, url, body)
            for method, url, body in self.requests
            if method != "GET" and "oauth2" not in url
        ]

    def __call__(self, request, timeout=None):
        method = request.get_method()
        url = request.full_url
        body = request.data
        self.requests.append((method, url, body))
        parsed = urllib.parse.urlsplit(url)
        if parsed.netloc == "oauth2.googleapis.com":
            self.test.assertEqual("POST", method)
            params = urllib.parse.parse_qs((body or b"").decode("utf-8"))
            self.test.assertEqual(
                [self.test.refresh_token], params.get("refresh_token")
            )
            self.test.assertEqual(
                ["refresh_token"], params.get("grant_type")
            )
            return _FakeCloudResponse(
                {
                    "access_token": self.test.access_token,
                    "expires_in": 3599,
                    "token_type": "Bearer",
                }
            )
        self.test.assertEqual("www.googleapis.com", parsed.netloc)
        self.test.assertEqual(
            f"Bearer {self.test.access_token}",
            request.get_header("Authorization"),
        )
        if parsed.path == "/drive/v3/files":
            self.test.assertEqual("GET", method)
            params = urllib.parse.parse_qs(parsed.query)
            self.test.assertIn(
                "nextPageToken", params["fields"][0],
                "the listing must request nextPageToken so truncation "
                "is detectable",
            )
            if self.list_pages is not None:
                token = params.get("pageToken", [""])[0]
                index = 0 if token == "" else int(token)
                return _FakeCloudResponse(
                    copy.deepcopy(self.list_pages[index])
                )
            return _FakeCloudResponse(
                {
                    "incompleteSearch": self.incomplete,
                    "files": copy.deepcopy(self.list_hits),
                }
            )
        file_id = urllib.parse.unquote(parsed.path.rsplit("/", 1)[1])
        if file_id == "root":
            file_id = self.root_id
        if method == "GET":
            if file_id not in self.files:
                raise urllib.error.HTTPError(
                    url, 404, "Not Found", None, io.BytesIO(b"{}")
                )
            return _FakeCloudResponse(copy.deepcopy(self.files[file_id]))
        if method == "PATCH":
            payload = json.loads(body.decode("utf-8"))
            self.files[file_id].update(payload)
            return _FakeCloudResponse(copy.deepcopy(self.files[file_id]))
        raise AssertionError(f"unexpected cloud request: {method} {url}")


NATIVE_PAGE = {
    "rows": [
        {
            "row_key": "err-1",
            "path": "/drive/legacy/file.txt",
            "category": "Sync error",
            "text": "Cannot sync file",
            "row_index": 0,
        }
    ],
    "next_page_token": None,
    "screenshot_sha256": "a" * 64,
    "captured_utc": "2026-08-02T00:00:00+00:00",
}

PREFLIGHT_FIXTURE = {
    "window": {
        "AXRole": "AXWindow",
        "AXIdentifier": "GDFSMenuWindow",
        "AXTitle": "Google Drive",
        "AXDescription": "Google Drive status window",
        "AXValue": "",
        "AXPosition": [1204.0, 25.0],
        "AXSize": [420.0, 640.0],
        "AXWindowNumber": 5521,
    },
    "screenshot_sha256": "c" * 64,
    "captured_utc": "2026-08-02T00:00:00+00:00",
}

SMOKE_SERVER_SOURCE = """\
import http.server
import json
import signal
import subprocess
import sys
import time

port = int(sys.argv[1])
mode = sys.argv[2] if len(sys.argv) > 2 else "marker"
if "ignore-term" in mode:
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
child_body = "import time\\ntime.sleep(600)\\n"
if "ignore-term" in mode:
    child_body = (
        "import signal, time\\n"
        "signal.signal(signal.SIGTERM, signal.SIG_IGN)\\n"
        "time.sleep(600)\\n"
    )
child = subprocess.Popen([sys.executable, "-c", child_body])


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if "json" in mode:
            body = json.dumps({"status": "ok", "workspace": "sample"}).encode()
            ctype = "application/json"
        else:
            body = b"drive-smoke-ok"
            ctype = "text/plain"
        self.send_response(200)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, *args):
        pass


http.server.HTTPServer(("127.0.0.1", port), Handler).serve_forever()
"""


class FakeTools:
    """Recorded stand-in for the module's live-observer subprocess seam.

    Serves lsof cwd listings, emacsclient buffer queries, the JXA
    list-errors provider, and both ps forms from fixture data, asserting
    the exact argv contract for each tool.
    """

    def __init__(self, test, cwds=(), emacs_files=(), emacs_rc=0, lsof_rc=0,
                 native_pages=None, native_rc=0, native_stderr="",
                 ps_groups=(), drive_pids=(4242,)):
        self.test = test
        self.cwds = list(cwds)
        self.emacs_files = list(emacs_files)
        self.emacs_rc = emacs_rc
        self.lsof_rc = lsof_rc
        self.native_pages = (
            [copy.deepcopy(NATIVE_PAGE)]
            if native_pages is None
            else copy.deepcopy(native_pages)
        )
        self.native_rc = native_rc
        self.native_stderr = native_stderr
        self.ps_groups = list(ps_groups)
        self.drive_pids = list(drive_pids)
        self.calls = []

    def __call__(self, argv):
        argv = [str(item) for item in argv]
        self.calls.append(argv)
        base = os.path.basename(argv[0])
        if base == "lsof":
            self.test.assertEqual(["-Fn", "-a", "-d", "cwd"], argv[1:])
            out = "".join(
                f"p{pid}\nfcwd\nn{path}\n" for pid, path in self.cwds
            )
            return subprocess.CompletedProcess(argv, self.lsof_rc, out, "")
        if base == "emacsclient":
            self.test.assertEqual(3, len(argv), argv)
            self.test.assertEqual("-e", argv[1])
            self.test.assertIn("buffer-file-name", argv[2])
            if self.emacs_rc != 0:
                return subprocess.CompletedProcess(
                    argv, self.emacs_rc, "", "emacsclient: can't find socket\n"
                )
            payload = json.dumps(json.dumps(self.emacs_files))
            return subprocess.CompletedProcess(argv, 0, payload + "\n", "")
        if base == "osascript":
            self.test.assertEqual("/usr/bin/osascript", argv[0])
            self.test.assertEqual(["-l", "JavaScript"], argv[1:3])
            self.test.assertEqual(self.test.module.NATIVE_JXA_PATH, argv[3])
            self.test.assertEqual("list-errors", argv[4])
            self.test.assertEqual("--page-token", argv[5])
            token = argv[6]
            if self.native_rc:
                return subprocess.CompletedProcess(
                    argv, self.native_rc, "", self.native_stderr
                )
            index = 0 if token == "" else int(token)
            return subprocess.CompletedProcess(
                argv, 0, json.dumps(self.native_pages[index]), ""
            )
        if base == "ps":
            if argv[1:] == ["-Ao", "pgid=,pid="]:
                out = "".join(
                    f"{pgid} {pid}\n" for pgid, pid in self.ps_groups
                )
                return subprocess.CompletedProcess(argv, 0, out, "")
            if argv[1:] == ["-Axo", "pid=,comm="]:
                out = "1 /sbin/launchd\n" + "".join(
                    f"{pid} /Applications/Google Drive.app/Contents/"
                    f"MacOS/Google Drive\n"
                    for pid in self.drive_pids
                )
                return subprocess.CompletedProcess(argv, 0, out, "")
        raise AssertionError(f"unexpected tool invocation: {argv}")


class CloudJournalFixture(JournalFixture):
    """JournalFixture plus fixture OAuth credentials behind the env seam.

    Every cloud-touching test (workspace or path repair) runs with these
    fixture credentials so the real personal token file is never read;
    FakeCloud asserts the fixture refresh token on every OAuth call.
    """

    def setUp(self):
        super().setUp()
        self.refresh_token = "fixture-refresh-token-1849"
        self.access_token = "fixture-access-token-7203"
        self.client_secret = "fixture-client-secret-5561"
        oauth_dir = self.root / "oauth"
        oauth_dir.mkdir(mode=0o700)
        self.oauth_file = oauth_dir / "token.json"
        self.oauth_file.write_text(
            json.dumps(
                {
                    "token": "stale-access-token",
                    "refresh_token": self.refresh_token,
                    "token_uri": "https://oauth2.googleapis.com/token",
                    "client_id": "fixture-client-id",
                    "client_secret": self.client_secret,
                    "scopes": ["https://www.googleapis.com/auth/drive"],
                }
            ),
            encoding="utf-8",
        )
        os.chmod(self.oauth_file, 0o600)
        saved = os.environ.get(self.module.OAUTH_FILE_ENV)
        os.environ[self.module.OAUTH_FILE_ENV] = str(self.oauth_file)

        def restore():
            if saved is None:
                os.environ.pop(self.module.OAUTH_FILE_ENV, None)
            else:
                os.environ[self.module.OAUTH_FILE_ENV] = saved

        self.addCleanup(restore)

    def install_tools(self, **kwargs):
        tools = FakeTools(self, **kwargs)
        original = self.module._run_tool
        self.module._run_tool = tools
        self.addCleanup(setattr, self.module, "_run_tool", original)
        # Route the process/Emacs gates through the mocked observer seam
        # instead of the fixture's benign no-users default.
        self.module._process_cwds = self.real_process_cwds
        self.module._emacs_visited_files = self.real_emacs_visited_files
        return tools


class LiveGateTests(CloudJournalFixture):
    """Process, Emacs-buffer, consumer, cloud, native, and smoke-service gates.

    All cloud traffic is served by FakeCloud through a patched
    urllib.request.urlopen; lsof, emacsclient, osascript, and ps are
    served by FakeTools through the module's _run_tool seam.  The OAuth
    credential path is overridden with fixture credentials so the real
    personal token file is never read, and no fixture token may ever
    appear in a journal, stdout, or stderr.
    """

    # ------------------------------------------------------------------
    # Shared fixtures
    # ------------------------------------------------------------------

    def cloud_files(self, trashed=True):
        return {
            "obj-1": {
                "id": "obj-1",
                "name": "repo",
                "parents": ["parent-1"],
                "trashed": trashed,
                "explicitlyTrashed": trashed,
                "ownedByMe": True,
                "mimeType": "application/vnd.google-apps.folder",
            },
            "parent-1": {
                "id": "parent-1",
                "name": "repos",
                "parents": ["root-1"],
                "trashed": False,
                "ownedByMe": True,
            },
            "root-1": {"id": "root-1", "name": "My Drive", "trashed": False},
        }

    def install_cloud(self, trashed=True, files=None, **kwargs):
        cloud = FakeCloud(
            self,
            self.cloud_files(trashed=trashed) if files is None else files,
            **kwargs,
        )
        return cloud.install()

    def assert_no_secret_leak(self, *streams):
        journal_bytes = b""
        if self.journal.exists():
            journal_bytes = self.journal.read_bytes()
        for secret in (
            self.refresh_token, self.access_token, self.client_secret
        ):
            self.assertNotIn(secret.encode("utf-8"), journal_bytes)
            for stream in streams:
                self.assertNotIn(secret, stream)

    def cloud_journal(self):
        """A captured journal carrying drive_state cloud identity."""
        self.install_providers()
        self.capture()
        self.ok("record-drive-state", "sample", "--journal", self.journal)

    def recorded_cloud_journal(self, do_move=False):
        if do_move:
            self.do_move()
        else:
            self.cloud_journal()
        cloud = self.install_cloud(trashed=False)
        self.ok("record-cloud", "sample", "--journal", self.journal)
        return cloud

    def trash_old_object(self, cloud):
        cloud.files["obj-1"]["trashed"] = True
        cloud.files["obj-1"]["explicitlyTrashed"] = True

    # ------------------------------------------------------------------
    # Cloud record / verify
    # ------------------------------------------------------------------

    def test_default_oauth_path_is_the_personal_gdoc_token(self):
        self.assertEqual(
            "~/.config/gdoc/accounts/personal/token.json",
            self.module.DEFAULT_OAUTH_FILE,
        )

    def test_record_cloud_stores_object_id_and_parent_chain(self):
        self.cloud_journal()
        cloud = self.install_cloud(trashed=False)
        stdout, stderr = self.ok(
            "record-cloud", "sample", "--journal", self.journal
        )
        self.assertEqual(self.success_stdout(self.journal), stdout)
        event = [
            item for item in self.events()
            if item["event"] == "cloud_recorded"
        ][-1]
        payload = event["payload"]
        self.assertEqual("obj-1", payload["object"]["id"])
        self.assertEqual("repo", payload["object"]["name"])
        self.assertEqual("parent-1", payload["parent_id"])
        self.assertEqual(
            ["parent-1", "root-1"],
            [entry["id"] for entry in payload["parent_chain"]],
        )
        self.assertEqual("root-1", payload["root_id"])
        self.assertEqual(1, payload["is_my_drive"])
        token_calls = [
            url for _method, url, _body in cloud.requests if "oauth2" in url
        ]
        self.assertEqual(1, len(token_calls))
        self.assertEqual([], cloud.mutations())
        self.assert_no_secret_leak(stdout, stderr)

    def test_record_cloud_fails_closed_without_drive_state(self):
        self.capture()
        cloud = self.install_cloud(trashed=False)
        _stdout, stderr = self.expect_fail(
            "record-cloud", "sample", "--journal", self.journal
        )
        self.assertIn("drive_state", stderr)
        self.assertEqual([], cloud.requests)
        self.assertNotIn("cloud_recorded", self.event_types())

    def test_record_cloud_rejects_missing_oauth_file_without_requests(self):
        self.cloud_journal()
        cloud = self.install_cloud(trashed=False)
        os.environ[self.module.OAUTH_FILE_ENV] = str(
            self.root / "missing-token.json"
        )
        _stdout, stderr = self.expect_fail(
            "record-cloud", "sample", "--journal", self.journal
        )
        self.assertIn("OAuth", stderr)
        self.assertEqual([], cloud.requests)

    def test_verify_cloud_accepts_trashed_original_and_journals_event(self):
        cloud = self.recorded_cloud_journal()
        self.trash_old_object(cloud)
        stdout, stderr = self.ok(
            "verify-cloud", "sample", "--journal", self.journal
        )
        self.assertEqual(self.success_stdout(self.journal), stdout)
        event = [
            item for item in self.events()
            if item["event"] == "cloud_verified"
        ][-1]
        payload = event["payload"]
        self.assertEqual("obj-1", payload["object_id"])
        self.assertEqual("parent-1", payload["parent_id"])
        self.assertEqual("root-1", payload["root_id"])
        self.assertEqual(0, payload["machine_root_count"])
        self.assertEqual([], cloud.mutations())
        self.assert_no_secret_leak(stdout, stderr)

    def test_verify_cloud_rejects_untrashed_old_tree_after_resume(self):
        self.recorded_cloud_journal()
        self.install_cloud(trashed=False)
        _stdout, stderr = self.expect_fail(
            "verify-cloud", "sample", "--journal", self.journal
        )
        self.assertIn("trashed", stderr)
        self.assertNotIn("cloud_verified", self.event_types())

    def test_verify_cloud_rejects_old_folder_with_wrong_parent(self):
        self.recorded_cloud_journal()
        files = self.cloud_files(trashed=True)
        files["obj-1"]["parents"] = ["parent-2"]
        files["parent-2"] = {
            "id": "parent-2", "name": "elsewhere",
            "parents": ["root-1"], "trashed": False,
        }
        self.install_cloud(files=files)
        _stdout, stderr = self.expect_fail(
            "verify-cloud", "sample", "--journal", self.journal
        )
        self.assertIn("parent", stderr)
        self.assertNotIn("cloud_verified", self.event_types())

    def test_verify_cloud_rejects_duplicates_ambiguity_and_foreign_roots(self):
        self.recorded_cloud_journal()
        live_at_old_path = {
            "id": "dup-1", "name": "repo", "parents": ["parent-1"],
            "trashed": False,
        }
        my_drive_duplicate = {
            "id": "dup-2", "name": "repo", "parents": ["other-1"],
            "trashed": False,
        }
        machine_duplicate = {
            "id": "dup-3", "name": "repo", "parents": ["machine-1"],
            "trashed": False,
        }
        extra_files = {
            "other-1": {
                "id": "other-1", "name": "archive",
                "parents": ["root-1"], "trashed": False,
            },
            "machine-1": {"id": "machine-1", "name": "MacBook Pro",
                          "trashed": False},
        }
        cases = [
            ("occupies", dict(list_hits=[live_at_old_path])),
            ("duplicate", dict(list_hits=[my_drive_duplicate])),
            ("computer", dict(list_hits=[machine_duplicate])),
            ("incomplete", dict(incomplete=True)),
        ]
        for marker, kwargs in cases:
            with self.subTest(marker):
                files = self.cloud_files(trashed=True)
                files.update(copy.deepcopy(extra_files))
                self.install_cloud(files=files, **kwargs)
                _stdout, stderr = self.expect_fail(
                    "verify-cloud", "sample", "--journal", self.journal
                )
                self.assertIn(marker, stderr)
        with self.subTest("not-my-drive"):
            files = self.cloud_files(trashed=True)
            files["obj-1"]["ownedByMe"] = False
            self.install_cloud(files=files)
            _stdout, stderr = self.expect_fail(
                "verify-cloud", "sample", "--journal", self.journal
            )
            self.assertIn("My Drive", stderr)
        with self.subTest("root-mismatch"):
            files = self.cloud_files(trashed=True)
            files["root-2"] = {"id": "root-2", "name": "My Drive",
                               "trashed": False}
            self.install_cloud(files=files, root_id="root-2")
            _stdout, stderr = self.expect_fail(
                "verify-cloud", "sample", "--journal", self.journal
            )
            self.assertIn("root", stderr)
        self.assertNotIn("cloud_verified", self.event_types())

    def test_verify_cloud_paginates_the_duplicate_sweep(self):
        self.recorded_cloud_journal()
        base_files = self.cloud_files(trashed=True)
        base_files["other-1"] = {
            "id": "other-1", "name": "archive",
            "parents": ["root-1"], "trashed": False,
        }
        duplicate = {
            "id": "dup-2", "name": "repo", "parents": ["other-1"],
            "trashed": False,
        }
        with self.subTest("clean-multi-page-listing-passes"):
            self.install_cloud(
                files=copy.deepcopy(base_files),
                list_pages=[
                    {"incompleteSearch": False, "files": [],
                     "nextPageToken": "1"},
                    {"incompleteSearch": False, "files": []},
                ],
            )
            self.ok("verify-cloud", "sample", "--journal", self.journal)
        with self.subTest("duplicate-on-page-two-blocks"):
            self.install_cloud(
                files=copy.deepcopy(base_files),
                list_pages=[
                    {"incompleteSearch": False, "files": [],
                     "nextPageToken": "1"},
                    {"incompleteSearch": False,
                     "files": [copy.deepcopy(duplicate)]},
                ],
            )
            _stdout, stderr = self.expect_fail(
                "verify-cloud", "sample", "--journal", self.journal
            )
            self.assertIn("duplicate", stderr)
        with self.subTest("pagination-loop-blocks"):
            self.install_cloud(
                files=copy.deepcopy(base_files),
                list_pages=[
                    {"incompleteSearch": False, "files": [],
                     "nextPageToken": "1"},
                    {"incompleteSearch": False, "files": [],
                     "nextPageToken": "1"},
                ],
            )
            _stdout, stderr = self.expect_fail(
                "verify-cloud", "sample", "--journal", self.journal
            )
            self.assertIn("loop", stderr)
        with self.subTest("incomplete-later-page-blocks"):
            self.install_cloud(
                files=copy.deepcopy(base_files),
                list_pages=[
                    {"incompleteSearch": False, "files": [],
                     "nextPageToken": "1"},
                    {"files": []},
                ],
            )
            _stdout, stderr = self.expect_fail(
                "verify-cloud", "sample", "--journal", self.journal
            )
            self.assertIn("incomplete", stderr)

    # ------------------------------------------------------------------
    # Move preflight gates
    # ------------------------------------------------------------------

    def move_argv(self):
        return [
            "move", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        ]

    def test_move_blocks_on_process_cwd_below_source(self):
        self.prepare_move()
        self.module._process_cwds = lambda: [
            (4321, str(self.source / "subdir"))
        ]
        _stdout, stderr = self.expect_fail(*self.move_argv())
        self.assertIn("process", stderr)
        self.assertIn("4321", stderr)
        self.assertTrue(self.source.exists())
        self.assertFalse(self.destination.exists())
        self.assertNotIn("moved", self.event_types())

    def test_move_blocks_on_emacs_buffer_below_source(self):
        self.prepare_move()
        self.module._emacs_visited_files = lambda: [
            str(self.source / "tracked.txt")
        ]
        _stdout, stderr = self.expect_fail(*self.move_argv())
        self.assertIn("Emacs", stderr)
        self.assertTrue(self.source.exists())
        self.assertFalse(self.destination.exists())
        self.assertNotIn("moved", self.event_types())

    # ------------------------------------------------------------------
    # Cloud restore
    # ------------------------------------------------------------------

    def test_restore_cloud_issues_only_files_update_and_precedes_rollback(self):
        cloud = self.recorded_cloud_journal(do_move=True)
        self.trash_old_object(cloud)
        stdout, stderr = self.ok(
            "restore-cloud", "sample", "--journal", self.journal
        )
        self.assertEqual(self.success_stdout(self.journal), stdout)
        mutations = cloud.mutations()
        self.assertEqual(1, len(mutations))
        method, url, body = mutations[0]
        self.assertEqual("PATCH", method)
        self.assertIn("/drive/v3/files/obj-1", url)
        self.assertEqual({"trashed": False}, json.loads(body.decode("utf-8")))
        for req_method, req_url, _body in cloud.requests:
            self.assertNotIn("upload", req_url)
            if "oauth2" in req_url:
                continue
            self.assertNotIn(req_method, {"POST", "PUT", "DELETE"})
        self.assertFalse(cloud.files["obj-1"]["trashed"])
        event = [
            item for item in self.events()
            if item["event"] == "cloud_restored"
        ][-1]
        self.assertEqual("obj-1", event["payload"]["object_id"])
        self.assertEqual(["parent-1"], event["payload"]["parents"])
        self.assert_no_secret_leak(stdout, stderr)
        self.ok(
            "rollback-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        types = self.event_types()
        self.assertLess(
            types.index("cloud_restored"), types.index("local_rolled_back")
        )

    def test_restore_cloud_refuses_changed_parent_and_live_duplicate(self):
        with self.subTest("changed-parent"):
            cloud = self.recorded_cloud_journal(do_move=True)
            files = self.cloud_files(trashed=True)
            files["obj-1"]["parents"] = ["parent-2"]
            files["parent-2"] = {
                "id": "parent-2", "name": "elsewhere",
                "parents": ["root-1"], "trashed": False,
            }
            moved_cloud = self.install_cloud(files=files)
            _stdout, stderr = self.expect_fail(
                "restore-cloud", "sample", "--journal", self.journal
            )
            self.assertIn("parent", stderr)
            self.assertEqual([], moved_cloud.mutations())
        with self.subTest("live-duplicate"):
            duplicate = {
                "id": "dup-1", "name": "repo", "parents": ["parent-1"],
                "trashed": False,
            }
            dup_cloud = self.install_cloud(
                trashed=True, list_hits=[duplicate]
            )
            _stdout, stderr = self.expect_fail(
                "restore-cloud", "sample", "--journal", self.journal
            )
            self.assertIn("duplicate", stderr)
            self.assertEqual([], dup_cloud.mutations())
        self.assertNotIn("cloud_restored", self.event_types())

    def test_restore_cloud_characterizes_the_pause_provider(self):
        cloud = self.recorded_cloud_journal(do_move=True)
        self.trash_old_object(cloud)
        cases = [
            ("no-provider", None, "provider"),
            (
                "running",
                lambda name: {"paused": False, "queue_advanced": False},
                "paused",
            ),
            (
                "ambiguous-ui",
                lambda name: {"paused": "maybe", "queue_advanced": False},
                "ambiguous",
            ),
            (
                "queue-advances-while-paused",
                lambda name: {"paused": True, "queue_advanced": True},
                "queue",
            ),
        ]
        for label, provider, marker in cases:
            with self.subTest(label):
                requests_before = len(cloud.requests)
                self.module.DRIVE_PAUSE_PROVIDER = provider
                _stdout, stderr = self.expect_fail(
                    "restore-cloud", "sample", "--journal", self.journal
                )
                self.assertIn(marker, stderr)
                self.assertEqual(
                    requests_before, len(cloud.requests),
                    "a refused pause must not reach the cloud API",
                )
        self.assertNotIn("cloud_restored", self.event_types())

    def test_restore_cloud_requires_destination_present_outside_drive(self):
        cloud = self.recorded_cloud_journal(do_move=True)
        self.trash_old_object(cloud)
        os.rename(self.destination, self.root / "stashed-destination")
        _stdout, stderr = self.expect_fail(
            "restore-cloud", "sample", "--journal", self.journal
        )
        self.assertIn("destination", stderr)
        self.assertEqual([], cloud.mutations())
        self.assertNotIn("cloud_restored", self.event_types())

    # ------------------------------------------------------------------
    # Native provider: preflight and error recording
    # ------------------------------------------------------------------

    def install_preflight(self, returncode=0, stdout="", stderr=""):
        calls = []

        def fake(argv):
            calls.append([str(item) for item in argv])
            return subprocess.CompletedProcess(
                calls[-1], returncode, stdout, stderr
            )

        original = self.module._run_tool
        self.module._run_tool = fake
        self.addCleanup(setattr, self.module, "_run_tool", original)
        return calls

    def test_native_preflight_invokes_the_exact_jxa_provider(self):
        calls = self.install_preflight(
            returncode=0, stdout=json.dumps(PREFLIGHT_FIXTURE)
        )
        stdout, _stderr = self.ok("native-preflight")
        self.assertEqual(
            [
                [
                    "/usr/bin/osascript", "-l", "JavaScript",
                    self.module.NATIVE_JXA_PATH, "preflight",
                ]
            ],
            calls,
        )
        payload = json.loads(stdout)
        self.assertEqual("AXWindow", payload["window"]["AXRole"])
        self.assertEqual(5521, payload["window"]["AXWindowNumber"])
        self.assertEqual("c" * 64, payload["screenshot_sha256"])

    def test_native_preflight_rejects_denied_or_ambiguous_fixtures(self):
        cases = [
            (
                "drive-absent", 1, "",
                "Google Drive process is not running", "Google Drive",
            ),
            ("accessibility-denied", 77, "", "-25211", "Accessibility"),
            (
                "screen-recording-denied", 78, "",
                "capture is blank; Screen Recording permission is missing",
                "Screen Recording",
            ),
            (
                "ambiguous-window", 1, "",
                "ambiguous: found 2 visible Google Drive windows",
                "ambiguous",
            ),
            (
                "missing-attribute", 0,
                json.dumps(
                    {
                        "window": {"AXRole": "AXWindow"},
                        "screenshot_sha256": "c" * 64,
                        "captured_utc": "2026-08-02T00:00:00+00:00",
                    }
                ),
                "", "AX",
            ),
        ]
        for label, code, out, err, marker in cases:
            with self.subTest(label):
                self.install_preflight(
                    returncode=code, stdout=out, stderr=err
                )
                stdout, stderr, status = self.run_main("native-preflight")
                self.assertNotEqual(0, status)
                self.assertIn(marker, stderr)

    def test_jxa_provider_is_committed_and_matches_the_contract(self):
        path = ROOT / "bin" / "drive-workspace-native.jxa"
        self.assertTrue(
            self.module.NATIVE_JXA_PATH.endswith(
                "bin/drive-workspace-native.jxa"
            )
        )
        text = path.read_text(encoding="utf-8")
        for required in (
            "System Events", "Google Drive", "UI elements enabled",
            "-25211", "77", "AXRole", "AXIdentifier", "AXTitle",
            "AXDescription", "AXValue", "AXPosition", "AXSize",
            "AXWindowNumber", "/usr/sbin/screencapture", "-x", "-l",
            "list-errors", "--page-token", "0o600",
        ):
            self.assertIn(required, text)

    def test_record_native_errors_follows_pagination_and_stores_schema(self):
        self.capture()
        page_one = {
            "rows": [
                {
                    "row_key": "err-1",
                    "path": "/drive/a.txt",
                    "category": "Sync error",
                    "text": "Cannot sync a.txt",
                    "row_index": 0,
                }
            ],
            "next_page_token": "1",
            "screenshot_sha256": "a" * 64,
            "captured_utc": "2026-08-02T00:00:01+00:00",
        }
        page_two = {
            "rows": [
                {
                    "row_key": "err-2",
                    "path": "/drive/b.txt",
                    "category": "Sync error",
                    "text": "Cannot sync b.txt",
                    "row_index": 0,
                    "annotation": "matches drivefs log line 88",
                }
            ],
            "next_page_token": None,
            "screenshot_sha256": "b" * 64,
            "captured_utc": "2026-08-02T00:00:02+00:00",
        }
        tools = self.install_tools(native_pages=[page_one, page_two])
        stdout, _stderr = self.ok(
            "record-native-errors", "--journal", self.journal
        )
        self.assertEqual(self.success_stdout(self.journal), stdout)
        tokens = [
            call[6] for call in tools.calls
            if os.path.basename(call[0]) == "osascript"
        ]
        self.assertEqual(["", "1"], tokens)
        event = [
            item for item in self.events()
            if item["event"] == "native_errors_recorded"
        ][-1]
        rows = event["payload"]["rows"]
        self.assertEqual(2, len(rows))
        first, second = rows
        self.assertEqual(
            {
                "row_key": "err-1",
                "path": "/drive/a.txt",
                "category": "Sync error",
                "text": "Cannot sync a.txt",
                "page_index": 0,
                "row_index": 0,
                "captured_utc": "2026-08-02T00:00:01+00:00",
                "screenshot_sha256": "a" * 64,
            },
            first,
        )
        self.assertEqual("err-2", second["row_key"])
        self.assertEqual(1, second["page_index"])
        self.assertEqual(
            "matches drivefs log line 88", second["annotation"],
            "log matches are optional annotations on a native row",
        )

    def test_record_native_errors_rejects_bad_rows_loops_and_duplicates(self):
        self.capture()
        unreadable = copy.deepcopy(NATIVE_PAGE)
        unreadable["rows"][0]["path"] = None
        looping = copy.deepcopy(NATIVE_PAGE)
        looping["next_page_token"] = "0"
        duplicate_a = copy.deepcopy(NATIVE_PAGE)
        duplicate_a["next_page_token"] = "1"
        duplicate_b = copy.deepcopy(NATIVE_PAGE)
        duplicate_b["rows"][0]["row_index"] = 1
        cases = [
            ("unreadable-row", [unreadable], "row"),
            ("pagination-loop", [looping], "loop"),
            ("duplicate-key", [duplicate_a, duplicate_b], "duplicate"),
        ]
        for label, pages, marker in cases:
            with self.subTest(label):
                self.install_tools(native_pages=pages)
                _stdout, stderr = self.expect_fail(
                    "record-native-errors", "--journal", self.journal
                )
                self.assertIn(marker, stderr)
        with self.subTest("provider-failure"):
            self.install_tools(native_rc=77, native_stderr="-25211")
            _stdout, stderr = self.expect_fail(
                "record-native-errors", "--journal", self.journal
            )
            self.assertIn("Accessibility", stderr)
        with self.subTest("program-journal"):
            program = self.state / "program" / "journal.jsonl"
            self.ok(
                "init-journal", "--journal", program,
                "--kind", "program", "--label", "gates",
            )
            self.install_tools()
            _stdout, stderr = self.expect_fail(
                "record-native-errors", "--journal", program
            )
            self.assertIn("program", stderr)
        self.assertNotIn("native_errors_recorded", self.event_types())

    # ------------------------------------------------------------------
    # close-rollback-window
    # ------------------------------------------------------------------

    def close_argv(self, baseline, pid=1111):
        return [
            "close-rollback-window", "--journal", self.journal,
            "--baseline-journal", baseline,
            "--restart-before-pid", str(pid),
        ]

    def verify_local_ok(self):
        self.ok(
            "verify-local", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )

    def closed_window_prereqs(self):
        """Drive the full happy path up to a closable rollback window."""
        self.do_move()
        self.verify_local_ok()
        self.ok(
            "materialize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "run-smoke", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        (self.destination / "node_modules").mkdir()
        self.verify_local_ok()
        self.ok(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "verify-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.cloud = self.install_cloud(trashed=False)
        self.ok("record-cloud", "sample", "--journal", self.journal)
        self.trash_old_object(self.cloud)
        self.ok("verify-cloud", "sample", "--journal", self.journal)
        baseline = self.make_baseline()
        tools = self.install_tools()
        self.ok("record-native-errors", "--journal", baseline)
        return baseline, tools

    def test_close_rollback_window_requires_ordered_evidence(self):
        self.do_move()
        baseline = self.make_baseline()
        self.verify_local_ok()
        self.ok(
            "materialize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
        self.assertIn("run-smoke", stderr)
        self.ok(
            "run-smoke", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
        self.assertIn("verify-local", stderr)
        (self.destination / "node_modules").mkdir()
        self.verify_local_ok()
        _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
        self.assertIn(
            "verify-consumers", stderr,
            "a workspace still needs the explicit verify-consumers event",
        )
        self.ok(
            "apply-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "verify-consumers", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
        self.assertIn("verify-cloud", stderr)
        self.cloud = self.install_cloud(trashed=False)
        self.ok("record-cloud", "sample", "--journal", self.journal)
        self.trash_old_object(self.cloud)
        self.ok("verify-cloud", "sample", "--journal", self.journal)
        tools = self.install_tools()
        _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
        self.assertIn("native", stderr)
        self.ok("record-native-errors", "--journal", baseline)
        self.assertNotIn("rollback_window_closed", self.event_types())
        stdout, _stderr = self.ok(*self.close_argv(baseline))
        self.assertEqual(self.success_stdout(self.journal), stdout)
        closed = [
            item for item in self.events()
            if item["event"] == "rollback_window_closed"
        ]
        self.assertEqual(1, len(closed))
        payload = closed[0]["payload"]
        self.assertEqual("sample", payload["workspace"])
        self.assertEqual({"outbound": 7}, payload["queue_cursors"])
        self.assertEqual(4242, payload["drive_pid"])
        self.assertEqual(1111, payload["restart_before_pid"])
        _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
        self.assertIn("closed", stderr)
        self.ok(
            "finalize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.assertFalse(
            (self.targets / "node_modules").exists(),
            "the producer event must unlock Task 4's finalize consumer",
        )

    def test_close_rollback_window_requires_exactly_one_smoke(self):
        self.do_move()
        baseline = self.make_baseline()
        self.verify_local_ok()
        self.ok(
            "materialize-generated-paths", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        self.ok(
            "run-smoke", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        (self.destination / "node_modules").mkdir()
        self.ok(
            "run-smoke", "sample", "--journal", self.journal,
            "--manifest", self.manifest_path,
        )
        _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
        self.assertIn("run-smoke", stderr)
        self.assertNotIn("rollback_window_closed", self.event_types())

    def test_close_rollback_window_blocks_on_process_cwd_below_source(self):
        baseline, _tools = self.closed_window_prereqs()
        tools = self.install_tools(
            cwds=[(4321, str(self.source / "subdir"))]
        )
        _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
        self.assertIn("process", stderr)
        self.assertIn("4321", stderr)
        lsof_calls = [
            call for call in tools.calls
            if os.path.basename(call[0]) == "lsof"
        ]
        self.assertTrue(lsof_calls)
        for call in lsof_calls:
            self.assertEqual(["-Fn", "-a", "-d", "cwd"], call[1:])
        self.assertNotIn("rollback_window_closed", self.event_types())

    def test_close_rollback_window_blocks_on_emacs_buffer_below_source(self):
        baseline, _tools = self.closed_window_prereqs()
        tools = self.install_tools(
            emacs_files=[str(self.source / "tracked.txt")]
        )
        _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
        self.assertIn("Emacs", stderr)
        emacs_calls = [
            call for call in tools.calls
            if os.path.basename(call[0]) == "emacsclient"
        ]
        self.assertTrue(emacs_calls)
        for call in emacs_calls:
            self.assertEqual("-e", call[1])
            self.assertEqual(3, len(call))
        self.install_tools(emacs_rc=1)
        _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
        self.assertIn("emacsclient", stderr)
        self.assertNotIn("rollback_window_closed", self.event_types())

    def test_close_rollback_window_blocks_on_consumer_with_old_path(self):
        baseline, _tools = self.closed_window_prereqs()
        content = self.config_path.read_text(encoding="utf-8")
        self.config_path.write_text(
            content + f"regressed {self.old}\n", encoding="utf-8"
        )
        _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
        self.assertIn("consumer", stderr)
        self.assertNotIn("rollback_window_closed", self.event_types())

    def test_close_rollback_window_blocks_on_live_state_regressions(self):
        baseline, _tools = self.closed_window_prereqs()
        with self.subTest("new-native-error"):
            regressed = copy.deepcopy(NATIVE_PAGE)
            regressed["rows"].append(
                {
                    "row_key": "err-new",
                    "path": str(self.destination / "tracked.txt"),
                    "category": "Sync error",
                    "text": "Cannot sync tracked.txt",
                    "row_index": 1,
                }
            )
            self.install_tools(native_pages=[regressed])
            _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
            self.assertIn("native", stderr)
        self.install_tools()
        with self.subTest("unstable-queue-cursors"):
            counter = {"value": 0}

            def advancing(name):
                counter["value"] += 1
                return {
                    "rows": [{"path": "repo", "id": "obj-1"}],
                    "queue_cursors": {"outbound": counter["value"]},
                    "cloud": {
                        "object_id": "obj-1", "parent_id": "parent-1",
                        "duplicates": [],
                    },
                }

            self.module.DRIVE_STATE_PROVIDER = advancing
            _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
            self.assertIn("stable", stderr)
            self.install_providers()
        with self.subTest("drive-pid-unchanged"):
            _stdout, stderr = self.expect_fail(
                *self.close_argv(baseline, pid=4242)
            )
            self.assertIn("restart", stderr)
        with self.subTest("live-smoke-process-group"):
            self.module._append_event(
                self.journal, "smoke_started",
                {
                    "service": "web", "pid": 7778, "pgid": 7777,
                    "port": 4567, "log": str(self.journal.parent / "l.log"),
                },
            )
            self.install_tools(ps_groups=[(7777, 7778)])
            _stdout, stderr = self.expect_fail(*self.close_argv(baseline))
            self.assertIn("group", stderr)
        self.assertNotIn("rollback_window_closed", self.event_types())

    # ------------------------------------------------------------------
    # Smoke services
    # ------------------------------------------------------------------

    def free_port(self) -> int:
        with socket.socket() as probe:
            probe.bind(("127.0.0.1", 0))
            return probe.getsockname()[1]

    def services_manifest(self):
        (self.source / "smoke_server.py").write_text(
            SMOKE_SERVER_SOURCE, encoding="utf-8"
        )
        self.web_port = self.free_port()
        self.api_port = self.free_port()
        self.badjson_port = self.free_port()
        self.stubborn_port = self.free_port()
        services = [
            {
                "name": "web", "cwd": ".",
                "argv": ["python3", "smoke_server.py", str(self.web_port)],
                "port": self.web_port,
                "http": {"status": 200, "content_marker": "drive-smoke-ok"},
            },
            {
                "name": "api", "cwd": ".",
                "argv": [
                    "python3", "smoke_server.py", str(self.api_port), "json",
                ],
                "port": self.api_port,
                "http": {"json_keys": ["status", "workspace"]},
            },
            {
                "name": "badjson", "cwd": ".",
                "argv": [
                    "python3", "smoke_server.py",
                    str(self.badjson_port), "json",
                ],
                "port": self.badjson_port,
                "http": {"json_keys": ["status", "workspace", "extra"]},
            },
            {
                "name": "stubborn", "cwd": ".",
                "argv": [
                    "python3", "smoke_server.py",
                    str(self.stubborn_port), "ignore-term",
                ],
                "port": self.stubborn_port,
                "http": {"status": 200, "content_marker": "drive-smoke-ok"},
            },
        ]
        self.manifest_path = self.write_manifest(runtime_services=services)

    def kill_group(self, pgid):
        try:
            os.killpg(pgid, signal.SIGKILL)
        except (ProcessLookupError, PermissionError):
            # ESRCH: already gone; EPERM: only unreaped zombies remain.
            pass

    def wait_port(self, port, timeout=10.0):
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            try:
                with socket.create_connection(("127.0.0.1", port), 0.2):
                    return
            except OSError:
                time.sleep(0.05)
        self.fail(f"port {port} never started listening")

    def start_service(self, service):
        self.ok(
            "smoke-start", "sample", "--service", service,
            "--journal", self.journal,
        )
        event = [
            item for item in self.events()
            if item["event"] == "smoke_started"
        ][-1]
        payload = event["payload"]
        self.addCleanup(self.kill_group, payload["pgid"])
        self.wait_port(payload["port"])
        return payload

    def group_members(self, pgid):
        # Reap any of this process's own exited children first so a
        # zombie is not misread as a live group member.
        self.module._reap_exited_children(pgid)
        listing = subprocess.run(
            ["/bin/ps", "-Ao", "pgid=,pid="],
            capture_output=True, encoding="utf-8", check=True,
        ).stdout
        members = []
        for line in listing.splitlines():
            parts = line.split()
            if len(parts) == 2 and parts[0] == str(pgid):
                members.append(int(parts[1]))
        return members

    def assert_port_bindable(self, port):
        with socket.socket() as probe:
            # SO_REUSEADDR: a lingering TIME_WAIT connection is fine;
            # only a live listener must fail this probe.
            probe.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            probe.bind(("127.0.0.1", port))

    def test_smoke_service_normal_lifecycle(self):
        self.services_manifest()
        self.do_move()
        payload = self.start_service("web")
        self.assertEqual(self.web_port, payload["port"])
        log = Path(payload["log"])
        self.assertTrue(log.is_file())
        self.assertEqual(0o600, stat.S_IMODE(os.stat(log).st_mode))
        self.assertTrue(str(log).startswith(str(self.state)))
        self.assertGreaterEqual(
            len(self.group_members(payload["pgid"])), 2,
            "the fixture server must have spawned a child in its group",
        )
        _stdout, stderr = self.expect_fail(
            "smoke-start", "sample", "--service", "web",
            "--journal", self.journal,
        )
        self.assertIn("already", stderr)
        self.ok(
            "smoke-check", "sample", "--service", "web",
            "--journal", self.journal,
        )
        self.assertEqual("smoke_checked", self.event_types()[-1])
        self.ok(
            "smoke-stop", "sample", "--service", "web",
            "--journal", self.journal,
        )
        stopped = [
            item for item in self.events()
            if item["event"] == "smoke_stopped"
        ][-1]
        self.assertFalse(stopped["payload"]["forced"])
        self.assertEqual([], self.group_members(payload["pgid"]))
        self.assert_port_bindable(self.web_port)

    def test_smoke_check_enforces_the_exact_json_key_set(self):
        self.services_manifest()
        self.do_move()
        self.start_service("api")
        self.ok(
            "smoke-check", "sample", "--service", "api",
            "--journal", self.journal,
        )
        self.start_service("badjson")
        _stdout, stderr = self.expect_fail(
            "smoke-check", "sample", "--service", "badjson",
            "--journal", self.journal,
        )
        self.assertIn("json", stderr.lower())
        for service in ("api", "badjson"):
            self.ok(
                "smoke-stop", "sample", "--service", service,
                "--journal", self.journal,
            )

    def test_smoke_start_rejects_a_preexisting_listener(self):
        self.services_manifest()
        self.do_move()
        squatter = socket.socket()
        self.addCleanup(squatter.close)
        squatter.bind(("127.0.0.1", self.web_port))
        squatter.listen(1)
        _stdout, stderr = self.expect_fail(
            "smoke-start", "sample", "--service", "web",
            "--journal", self.journal,
        )
        self.assertIn("listener", stderr)
        self.assertNotIn("smoke_started", self.event_types())

    def test_smoke_start_rejects_a_wildcard_listener(self):
        # A 0.0.0.0 listener with SO_REUSEADDR would pass a plain
        # 127.0.0.1 bind probe; the lsof listener sweep must still block.
        self.services_manifest()
        self.do_move()
        squatter = socket.socket()
        self.addCleanup(squatter.close)
        squatter.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        squatter.bind(("0.0.0.0", self.web_port))
        squatter.listen(1)
        _stdout, stderr = self.expect_fail(
            "smoke-start", "sample", "--service", "web",
            "--journal", self.journal,
        )
        self.assertIn("listener", stderr)
        self.assertNotIn("smoke_started", self.event_types())

    def test_preexisting_listener_never_satisfies_a_smoke_check(self):
        self.services_manifest()
        self.do_move()
        _stdout, stderr = self.expect_fail(
            "smoke-check", "sample", "--service", "web",
            "--journal", self.journal,
        )
        self.assertIn("smoke-start", stderr)
        payload = self.start_service("web")
        self.kill_group(payload["pgid"])
        deadline = time.monotonic() + 10
        while self.group_members(payload["pgid"]):
            if time.monotonic() > deadline:
                self.fail("fixture group did not die")
            time.sleep(0.05)
        impostor = subprocess.Popen(
            [
                sys.executable, "-c",
                "import socket, time\n"
                "server = socket.socket()\n"
                "server.setsockopt("
                "socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)\n"
                f"server.bind(('127.0.0.1', {self.web_port}))\n"
                "server.listen(1)\n"
                "time.sleep(300)\n",
            ],
            start_new_session=True,
        )
        self.addCleanup(impostor.wait)
        self.addCleanup(impostor.kill)
        self.wait_port(self.web_port)
        _stdout, stderr = self.expect_fail(
            "smoke-check", "sample", "--service", "web",
            "--journal", self.journal,
        )
        self.assertIn("group", stderr)
        self.assertNotIn("smoke_checked", self.event_types())

    def test_smoke_stop_forces_sigkill_on_a_stubborn_group(self):
        self.services_manifest()
        self.do_move()
        payload = self.start_service("stubborn")
        self.ok(
            "smoke-stop", "sample", "--service", "stubborn",
            "--journal", self.journal,
        )
        stopped = [
            item for item in self.events()
            if item["event"] == "smoke_stopped"
        ][-1]
        self.assertTrue(stopped["payload"]["forced"])
        self.assertEqual([], self.group_members(payload["pgid"]))
        self.assert_port_bindable(self.stubborn_port)

    # ------------------------------------------------------------------
    # Command-surface hygiene
    # ------------------------------------------------------------------

    def test_new_commands_reject_force_and_corrupt_journals(self):
        self.capture()
        journal_argvs = [
            ["record-cloud", "sample", "--journal", str(self.journal)],
            ["verify-cloud", "sample", "--journal", str(self.journal)],
            ["restore-cloud", "sample", "--journal", str(self.journal)],
            ["record-native-errors", "--journal", str(self.journal)],
            ["close-rollback-window", "--journal", str(self.journal),
             "--baseline-journal", str(self.journal),
             "--restart-before-pid", "1"],
            ["smoke-start", "sample", "--service", "web",
             "--journal", str(self.journal)],
            ["smoke-check", "sample", "--service", "web",
             "--journal", str(self.journal)],
            ["smoke-stop", "sample", "--service", "web",
             "--journal", str(self.journal)],
        ]
        for argv in journal_argvs + [["native-preflight"]]:
            stderr = io.StringIO()
            with self.assertRaises(SystemExit, msg=argv[0]):
                with contextlib.redirect_stderr(stderr):
                    self.module.main(argv + ["--force"])
            self.assertIn("--force", stderr.getvalue(), argv[0])
        self.craft_line(self.journal, "drive_state", event_hash="f" * 64)
        corrupted = self.journal.read_bytes()
        for argv in journal_argvs:
            _stdout, stderr = self.expect_fail(*argv)
            self.assertIn("journal", stderr.lower(), argv[0])
            self.assertEqual(corrupted, self.journal.read_bytes(), argv[0])


class PathRepairTests(CloudJournalFixture):
    """Journaled single-path repair transactions: capture through finalize.

    Every repair operates on one in-Drive path (symlink, regular file,
    or directory) inside disposable temporary Drive roots.  Cloud
    identity is served by FakeCloud, native pause/state evidence by the
    provider seams, and live-observer tools by FakeTools; no test ever
    touches the network, the real Drive client, or a real repository.
    """

    # ------------------------------------------------------------------
    # Helpers and fixtures
    # ------------------------------------------------------------------

    def capture_argv(
        self,
        label,
        path,
        *,
        replacements=(),
        final_type="absent",
        policy="disposable",
        journal=None,
    ):
        argv = [
            "capture-path", label, "--journal", journal or self.journal,
            "--path", path, "--final-type", final_type,
            "--target-policy", policy,
        ]
        for replacement in replacements:
            argv.extend(["--replacement", replacement])
        return argv

    def capture_path(self, label, path, **kwargs):
        return self.ok(*self.capture_argv(label, path, **kwargs))

    def payload(self, event_type, journal=None):
        matches = [
            event for event in self.events(journal)
            if event["event"] == event_type
        ]
        self.assertTrue(matches, f"no {event_type} event")
        return matches[-1]["payload"]

    def confirm_paused(self, journal=None):
        self.install_providers()
        self.ok(
            "confirm-path-paused", "--journal", journal or self.journal
        )

    def capture_link(self):
        """Link whose target is a disposable generated directory."""
        self.link_path = self.source / "node_modules"
        self.capture_path("nm", self.link_path)
        return self.link_path

    def capture_dirty_tracked_file(self):
        """Tracked regular file with a same-path replacement."""
        path = self.source / "tracked.txt"
        path.write_bytes(b"corrupted\n")
        os.chmod(path, 0o644)
        self.capture_path(
            "fix-tracked", path, replacements=[path],
            final_type="file", policy="retain",
        )
        return path

    def capture_sibling_file(self):
        """Regular-file preimage with a journaled sibling replacement."""
        path = self.source / "notes.txt"
        path.write_bytes(b"alpha\nbeta\n")
        os.chmod(path, 0o640)
        self.sibling = self.source / "notes-fixed.txt"
        self.capture_path(
            "notes", path, replacements=[self.sibling],
            final_type="absent", policy="retain",
        )
        return path

    def make_preimage_directory(self):
        path = self.source / "cachedir"
        (path / "sub").mkdir(parents=True)
        (path / "a.txt").write_bytes(b"alpha\n")
        os.chmod(path / "a.txt", 0o600)
        (path / "sub" / "b.bin").write_bytes(b"beta\n")
        os.chmod(path / "sub" / "b.bin", 0o755)
        return path

    def capture_directory(self, journal=None):
        """Regular-directory preimage replaced in place."""
        path = self.make_preimage_directory()
        self.capture_path(
            "cache", path, replacements=[path],
            final_type="directory", policy="retain", journal=journal,
        )
        return path

    def capture_dist_link(self):
        """Link replaced by a directory of journaled interior files."""
        target = self.targets / "dist-target"
        target.mkdir()
        (target / "old.js").write_bytes(b"old\n")
        self.dist_target = target
        path = self.source / "dist"
        path.symlink_to(target)
        self.capture_path(
            "dist", path, replacements=[path / "index.js"],
            final_type="directory", policy="disposable",
        )
        return path

    def path_cloud_files(self, name, **overrides):
        obj = {
            "id": "pobj-1",
            "name": name,
            "parents": ["parent-1"],
            "trashed": False,
            "explicitlyTrashed": False,
            "ownedByMe": True,
            "mimeType": "application/vnd.google-apps.folder",
        }
        obj.update(overrides)
        return {
            "pobj-1": obj,
            "parent-1": {
                "id": "parent-1", "name": "repos", "parents": ["root-1"],
                "trashed": False, "ownedByMe": True,
            },
            "root-1": {"id": "root-1", "name": "My Drive", "trashed": False},
        }

    def hit(self, name, file_id="pobj-1", **overrides):
        record = {
            "id": file_id, "name": name, "parents": ["parent-1"],
            "trashed": False, "ownedByMe": True,
        }
        record.update(overrides)
        return record

    # ------------------------------------------------------------------
    # capture-path
    # ------------------------------------------------------------------

    def test_capture_path_link_records_lstat_and_target_manifest(self):
        path = self.source / "node_modules"
        link_mode = f"0o{stat.S_IMODE(os.lstat(path).st_mode):o}"
        stdout, _stderr = self.capture_path("nm", path)
        self.assertEqual(self.success_stdout(self.journal), stdout)
        self.assertEqual(
            0o700, stat.S_IMODE(os.lstat(self.journal.parent).st_mode)
        )
        self.assertEqual(0o600, stat.S_IMODE(os.lstat(self.journal).st_mode))
        header = self.events()[0]
        self.assertEqual("path_repair", header["journal_kind"])
        self.assertEqual("nm", header["label"])
        payload = self.payload("path_captured")
        self.assertEqual(str(path), payload["path"])
        self.assertEqual(os.path.realpath(str(self.source)), payload["root"])
        source = payload["source"]
        self.assertEqual("symlink", source["lstat_type"])
        self.assertEqual(os.readlink(path), source["link_text"])
        self.assertEqual(link_mode, source["lstat_mode"])
        target = source["target"]
        self.assertEqual(str(self.targets / "node_modules"), target["path"])
        dep = [
            record for record in target["manifest"]
            if record["path"] == "dep.js"
        ]
        self.assertEqual(1, len(dep))
        self.assertEqual(
            hashlib.sha256(b"dep\n").hexdigest(), dep[0]["sha256"]
        )
        self.assertEqual("absent", payload["final_type"])
        self.assertEqual("disposable", payload["target_policy"])
        self.assertEqual([], payload["replacements"])
        self.assertIsNone(payload["cloud_preimage"])
        self.assertFalse(payload["tracked"])
        self.assertIsNotNone(payload["git"])

    def test_capture_path_retains_user_data_target(self):
        path = self.source / ".venv"
        self.capture_path("venv", path, policy="retain")
        payload = self.payload("path_captured")
        self.assertEqual("retain", payload["target_policy"])
        target = payload["source"]["target"]
        self.assertEqual(str(self.targets / "venv"), target["path"])
        names = {record["path"] for record in target["manifest"]}
        self.assertIn("bin.txt", names)

    def test_capture_path_tracked_file_records_git_and_private_asset(self):
        path = self.capture_dirty_tracked_file()
        payload = self.payload("path_captured")
        self.assertTrue(payload["tracked"])
        source = payload["source"]
        self.assertEqual("file", source["lstat_type"])
        self.assertEqual("0o644", source["lstat_mode"])
        sha = hashlib.sha256(b"corrupted\n").hexdigest()
        self.assertEqual(sha, source["sha256"])
        asset = self.journal.parent / "preimage" / "blobs" / sha
        self.assertEqual(b"corrupted\n", asset.read_bytes())
        self.assertEqual(0o600, stat.S_IMODE(os.lstat(asset).st_mode))
        self.assertEqual(
            0o700,
            stat.S_IMODE(os.lstat(self.journal.parent / "preimage").st_mode),
        )
        git = payload["git"]
        self.assertIn("100644", git["ls_files"])
        entries = [entry for entry in git["status"].split("\0") if entry]
        self.assertTrue(
            any(entry.startswith("1 ") for entry in entries),
            f"the dirty preimage must show in the exact status: {entries!r}",
        )
        self.assertEqual([str(path)], payload["replacements"])

    def test_capture_path_directory_preimage_is_recursive_and_nul_safe(self):
        self.capture_directory()
        payload = self.payload("path_captured")
        source = payload["source"]
        self.assertEqual("directory", source["lstat_type"])
        by_path = {record["path"]: record for record in source["entries"]}
        self.assertEqual({"a.txt", "sub", "sub/b.bin"}, set(by_path))
        self.assertEqual("0o600", by_path["a.txt"]["mode"])
        self.assertEqual("0o755", by_path["sub/b.bin"]["mode"])
        self.assertEqual("directory", by_path["sub"]["type"])
        sha = hashlib.sha256(b"beta\n").hexdigest()
        self.assertEqual(sha, by_path["sub/b.bin"]["sha256"])
        blob = self.journal.parent / "preimage" / "blobs" / sha
        self.assertEqual(b"beta\n", blob.read_bytes())

    def test_capture_path_rejects_unknown_or_unreadable_objects(self):
        missing = self.source / "not-there"
        _stdout, stderr = self.expect_fail(*self.capture_argv("x", missing))
        self.assertIn("unknown", stderr.lower())
        self.assertFalse(
            self.journal.parent.exists(), "no journal may outlive a blocker"
        )
        fifo = self.source / "fifo"
        os.mkfifo(fifo)
        _stdout, stderr = self.expect_fail(
            *self.capture_argv("x", fifo, policy="retain")
        )
        self.assertIn("unknown", stderr.lower())
        hard = self.source / "hard.txt"
        os.link(self.source / "tracked.txt", hard)
        _stdout, stderr = self.expect_fail(
            *self.capture_argv(
                "x", self.source / "tracked.txt",
                final_type="file", policy="retain",
            )
        )
        self.assertIn("hard link", stderr)
        os.remove(hard)
        unreadable = self.source / "secret.txt"
        unreadable.write_bytes(b"data")
        os.chmod(unreadable, 0)
        _stdout, stderr = self.expect_fail(
            *self.capture_argv(
                "x", unreadable, final_type="file", policy="retain",
            )
        )
        self.assertIn("read", stderr.lower())
        os.chmod(unreadable, 0o600)
        bad_dir = self.make_preimage_directory()
        os.mkfifo(bad_dir / "pipe")
        _stdout, stderr = self.expect_fail(
            *self.capture_argv(
                "x", bad_dir, final_type="directory", policy="retain",
            )
        )
        self.assertIn("not supported", stderr)
        os.remove(bad_dir / "pipe")
        os.link(bad_dir / "a.txt", bad_dir / "hard.txt")
        _stdout, stderr = self.expect_fail(
            *self.capture_argv(
                "x", bad_dir, final_type="directory", policy="retain",
            )
        )
        self.assertIn("hard link", stderr)
        os.remove(bad_dir / "hard.txt")
        (bad_dir / "inner-link").symlink_to(self.targets / "venv")
        _stdout, stderr = self.expect_fail(
            *self.capture_argv(
                "x", bad_dir, final_type="directory", policy="retain",
            )
        )
        self.assertIn("not supported", stderr)
        self.assertFalse(self.journal.parent.exists())

    def test_capture_path_rejects_bad_replacements_and_roots(self):
        path = self.source / "notes.txt"
        path.write_bytes(b"n\n")
        outside = self.targets / "elsewhere.txt"
        _stdout, stderr = self.expect_fail(
            *self.capture_argv(
                "x", path, replacements=[outside], policy="retain",
            )
        )
        self.assertIn("journaled root", stderr)
        sibling = self.source / "notes-new.txt"
        sibling.write_bytes(b"occupied\n")
        _stdout, stderr = self.expect_fail(
            *self.capture_argv(
                "x", path, replacements=[sibling], policy="retain",
            )
        )
        self.assertIn("replacement", stderr)
        os.remove(sibling)
        sibling.touch()
        self.ok(
            *self.capture_argv(
                "x", path, replacements=[sibling], policy="retain",
            )
        )
        outside_journal = self.root / "outside.jsonl"
        _stdout, stderr = self.expect_fail(
            *self.capture_argv(
                "y", path, policy="retain", journal=outside_journal,
            )
        )
        self.assertIn("state root", stderr)
        journal3 = self.state / "r3" / "journal.jsonl"
        _stdout, stderr = self.expect_fail(
            *self.capture_argv("z", path, journal=journal3)
        )
        self.assertIn("disposable", stderr)
        dangling = self.source / "dangle"
        dangling.symlink_to(self.source / "gone")
        _stdout, stderr = self.expect_fail(
            *self.capture_argv("d", dangling, journal=journal3)
        )
        self.assertIn("target", stderr)
        self.ok(
            *self.capture_argv(
                "d", dangling, policy="retain", journal=journal3,
            )
        )
        self.assertIsNone(
            self.payload("path_captured", journal3)["source"]["target"]
        )
        alias = self.drive / "alias"
        alias.symlink_to(self.source)
        journal4 = self.state / "r4" / "journal.jsonl"
        _stdout, stderr = self.expect_fail(
            *self.capture_argv(
                "a", alias / "notes.txt", policy="retain", journal=journal4,
            )
        )
        self.assertIn("root", stderr)

    # ------------------------------------------------------------------
    # confirm-path-paused and verify-path-local
    # ------------------------------------------------------------------

    def test_confirm_path_paused_requires_characterized_evidence(self):
        self.capture_link()
        _stdout, stderr = self.expect_fail(
            "confirm-path-paused", "--journal", self.journal
        )
        self.assertIn("pause", stderr)
        self.module.DRIVE_PAUSE_PROVIDER = (
            lambda name: {"paused": False, "queue_advanced": False}
        )
        _stdout, stderr = self.expect_fail(
            "confirm-path-paused", "--journal", self.journal
        )
        self.assertIn("paused", stderr)
        self.module.DRIVE_PAUSE_PROVIDER = (
            lambda name: {"paused": True, "queue_advanced": True}
        )
        _stdout, stderr = self.expect_fail(
            "confirm-path-paused", "--journal", self.journal
        )
        self.assertIn("queue", stderr)
        self.install_providers()
        self.ok("confirm-path-paused", "--journal", self.journal)
        payload = self.payload("drive_paused")
        self.assertEqual(
            {"paused": True, "queue_advanced": False}, payload["evidence"]
        )

    def test_verify_path_local_requires_declared_absent(self):
        path = self.capture_link()
        _stdout, stderr = self.expect_fail(
            "verify-path-local", "--journal", self.journal
        )
        self.assertIn("absent", stderr)
        os.remove(path)
        stdout, _stderr = self.ok(
            "verify-path-local", "--journal", self.journal
        )
        self.assertEqual(self.success_stdout(self.journal), stdout)
        self.assertEqual(
            "absent", self.payload("path_local_verified")["final_type"]
        )

    def test_verify_path_local_never_accepts_a_symlink(self):
        path = self.capture_dirty_tracked_file()
        os.remove(path)
        path.symlink_to(self.targets / "venv" / "bin.txt")
        _stdout, stderr = self.expect_fail(
            "verify-path-local", "--journal", self.journal
        )
        self.assertIn("symlink", stderr)
        os.remove(path)
        journal2 = self.state / "r2" / "journal.jsonl"
        directory = self.capture_directory(journal=journal2)
        shutil.rmtree(directory)
        directory.symlink_to(self.targets / "venv")
        _stdout, stderr = self.expect_fail(
            "verify-path-local", "--journal", journal2
        )
        self.assertIn("symlink", stderr)

    def test_verify_path_local_requires_clean_git_status(self):
        path = self.capture_dirty_tracked_file()
        path.write_bytes(b"still-wrong\n")
        _stdout, stderr = self.expect_fail(
            "verify-path-local", "--journal", self.journal
        )
        self.assertIn("git", stderr.lower())
        path.write_bytes(b"original\n")
        self.ok("verify-path-local", "--journal", self.journal)
        git = self.payload("path_local_verified")["git"]
        self.assertIn("100644", git["ls_files"])

    def test_verify_path_local_checks_nul_safe_sha256_manifest(self):
        path = self.capture_dirty_tracked_file()
        path.write_bytes(b"original\n")
        good = hashlib.sha256(b"original\n").hexdigest()
        manifest = self.root / "final.manifest"
        manifest.write_bytes(f"{good}  .\0".encode("utf-8"))
        self.ok(
            "verify-path-local", "--journal", self.journal,
            "--manifest", manifest,
        )
        manifest.write_bytes(f"{'0' * 64}  .\0".encode("utf-8"))
        _stdout, stderr = self.expect_fail(
            "verify-path-local", "--journal", self.journal,
            "--manifest", manifest,
        )
        self.assertIn("manifest", stderr)
        journal2 = self.state / "r2" / "journal.jsonl"
        nm = self.source / "node_modules"
        self.capture_path("nm", nm, journal=journal2)
        os.remove(nm)
        _stdout, stderr = self.expect_fail(
            "verify-path-local", "--journal", journal2,
            "--manifest", manifest,
        )
        self.assertIn("absent", stderr)
        journal3 = self.state / "r3" / "journal.jsonl"
        directory = self.capture_directory(journal=journal3)
        shutil.rmtree(directory)
        (directory / "sub").mkdir(parents=True)
        (directory / "a.txt").write_bytes(b"new-a\n")
        (directory / "sub" / "b.bin").write_bytes(b"new-b\n")
        sha_a = hashlib.sha256(b"new-a\n").hexdigest()
        sha_b = hashlib.sha256(b"new-b\n").hexdigest()
        manifest.write_bytes(
            f"{sha_a}  a.txt\0{sha_b}  sub/b.bin\0".encode("utf-8")
        )
        self.ok(
            "verify-path-local", "--journal", journal3,
            "--manifest", manifest,
        )
        manifest.write_bytes(f"{sha_a}  a.txt\0".encode("utf-8"))
        _stdout, stderr = self.expect_fail(
            "verify-path-local", "--journal", journal3,
            "--manifest", manifest,
        )
        self.assertIn("manifest", stderr)

    def test_verify_path_local_rejects_a_path_escaping_the_root(self):
        path = self.capture_dirty_tracked_file()
        path.write_bytes(b"original\n")
        moved = self.drive / "repo-moved"
        os.rename(self.source, moved)
        (self.drive / "repo").symlink_to(moved)
        _stdout, stderr = self.expect_fail(
            "verify-path-local", "--journal", self.journal
        )
        self.assertIn("root", stderr)

    def test_verify_path_local_requires_journaled_sibling_replacements(self):
        path = self.capture_sibling_file()
        os.remove(path)
        _stdout, stderr = self.expect_fail(
            "verify-path-local", "--journal", self.journal
        )
        self.assertIn("replacement", stderr)
        self.sibling.write_bytes(b"fixed\n")
        self.ok("verify-path-local", "--journal", self.journal)

    # ------------------------------------------------------------------
    # rollback-path-local
    # ------------------------------------------------------------------

    def test_rollback_path_local_requires_fresh_pause_evidence(self):
        path = self.capture_link()
        os.remove(path)
        _stdout, stderr = self.expect_fail(
            "rollback-path-local", "--journal", self.journal
        )
        self.assertIn("pause", stderr)
        self.assertFalse(os.path.lexists(path))
        self.confirm_paused()
        self.module.DRIVE_PAUSE_PROVIDER = None
        _stdout, stderr = self.expect_fail(
            "rollback-path-local", "--journal", self.journal
        )
        self.assertIn("pause", stderr)
        self.install_providers()
        self.ok("rollback-path-local", "--journal", self.journal)
        self.assertTrue(os.path.islink(path))

    def test_rollback_path_local_restores_exact_link_and_leaves_target(self):
        path = self.capture_link()
        text = os.readlink(path)
        self.confirm_paused()
        os.remove(path)
        stdout, _stderr = self.ok(
            "rollback-path-local", "--journal", self.journal
        )
        self.assertEqual(self.success_stdout(self.journal), stdout)
        self.assertTrue(os.path.islink(path))
        self.assertEqual(text, os.readlink(path))
        self.assertEqual(
            b"dep\n",
            (self.targets / "node_modules" / "dep.js").read_bytes(),
            "the external target is never touched",
        )
        payload = self.payload("path_local_rolled_back")
        self.assertEqual([], payload["trash"])

    def test_rollback_path_local_trashes_same_path_replacement(self):
        path = self.capture_dirty_tracked_file()
        self.confirm_paused()
        path.write_bytes(b"original\n")
        self.ok("rollback-path-local", "--journal", self.journal)
        self.assertEqual(b"corrupted\n", path.read_bytes())
        self.assertEqual(0o644, stat.S_IMODE(os.lstat(path).st_mode))
        payload = self.payload("path_local_rolled_back")
        self.assertEqual(1, len(payload["trash"]))
        record = payload["trash"][0]
        self.assertEqual(str(path), record["path"])
        self.assertEqual(
            b"original\n", Path(record["trash_path"]).read_bytes()
        )
        self.assertTrue(
            record["trash_path"].startswith(str(self.journal.parent)),
            "the trash must be tool-owned under the transaction directory",
        )

    def test_rollback_path_local_trashes_sibling_replacement(self):
        path = self.capture_sibling_file()
        self.confirm_paused()
        os.rename(path, self.sibling)
        self.ok("rollback-path-local", "--journal", self.journal)
        self.assertEqual(b"alpha\nbeta\n", path.read_bytes())
        self.assertEqual(0o640, stat.S_IMODE(os.lstat(path).st_mode))
        self.assertFalse(os.path.lexists(self.sibling))
        payload = self.payload("path_local_rolled_back")
        self.assertEqual(
            [str(self.sibling)],
            [record["path"] for record in payload["trash"]],
        )

    def test_rollback_path_local_restores_directory_preimage(self):
        path = self.capture_directory()
        self.confirm_paused()
        shutil.rmtree(path)
        path.mkdir()
        (path / "replacement.txt").write_bytes(b"new\n")
        self.ok("rollback-path-local", "--journal", self.journal)
        self.assertEqual(b"alpha\n", (path / "a.txt").read_bytes())
        self.assertEqual(
            0o600, stat.S_IMODE(os.lstat(path / "a.txt").st_mode)
        )
        self.assertEqual(b"beta\n", (path / "sub" / "b.bin").read_bytes())
        self.assertEqual(
            0o755, stat.S_IMODE(os.lstat(path / "sub" / "b.bin").st_mode)
        )
        trashed = self.payload("path_local_rolled_back")["trash"]
        self.assertEqual(1, len(trashed))
        self.assertEqual(
            b"new\n",
            (Path(trashed[0]["trash_path"]) / "replacement.txt").read_bytes(),
        )

    def test_rollback_path_local_refuses_unjournaled_replacements(self):
        path = self.capture_link()
        self.confirm_paused()
        os.remove(path)
        path.mkdir()
        (path / "user-data.txt").write_bytes(b"precious\n")
        _stdout, stderr = self.expect_fail(
            "rollback-path-local", "--journal", self.journal
        )
        self.assertIn("unjournaled", stderr)
        self.assertEqual(
            b"precious\n", (path / "user-data.txt").read_bytes()
        )
        shutil.rmtree(path)
        path.mkdir()
        self.ok("rollback-path-local", "--journal", self.journal)
        self.assertTrue(os.path.islink(path))

    def test_rollback_path_local_refuses_changed_interior_replacement(self):
        path = self.capture_dist_link()
        self.confirm_paused()
        os.remove(path)
        path.mkdir()
        (path / "index.js").write_bytes(b"built\n")
        (path / "extra.txt").write_bytes(b"unjournaled\n")
        _stdout, stderr = self.expect_fail(
            "rollback-path-local", "--journal", self.journal
        )
        self.assertIn("unjournaled", stderr)
        os.remove(path / "extra.txt")
        self.ok("rollback-path-local", "--journal", self.journal)
        self.assertTrue(os.path.islink(path))
        self.assertEqual(b"old\n", (self.dist_target / "old.js").read_bytes())

    def test_rollback_path_local_is_unavailable_after_the_window_closes(self):
        path = self.capture_link()
        self.confirm_paused()
        os.remove(path)
        self.module._append_event(
            self.journal, "rollback_window_closed", {}
        )
        _stdout, stderr = self.expect_fail(
            "rollback-path-local", "--journal", self.journal
        )
        self.assertIn("closed", stderr)
        self.assertFalse(os.path.lexists(path))

    # ------------------------------------------------------------------
    # record-path-cloud / verify-path-cloud / restore-path-cloud
    # ------------------------------------------------------------------

    def test_record_path_cloud_snapshots_preimage_identity(self):
        self.capture_link()
        files = self.path_cloud_files("node_modules")
        cloud = FakeCloud(
            self, files, list_hits=[self.hit("node_modules")]
        ).install()
        stdout, _stderr = self.ok(
            "record-path-cloud", "--journal", self.journal
        )
        self.assertEqual(self.success_stdout(self.journal), stdout)
        payload = self.payload("path_cloud_recorded")
        self.assertEqual("pobj-1", payload["cloud_preimage"]["id"])
        self.assertEqual("parent-1", payload["parent_id"])
        self.assertEqual(
            ["parent-1", "root-1"],
            [entry["id"] for entry in payload["parent_chain"]],
        )
        self.assertEqual("root-1", payload["root_id"])
        self.assertEqual([], cloud.mutations())
        _stdout, stderr = self.expect_fail(
            "record-path-cloud", "--journal", self.journal
        )
        self.assertIn("already", stderr)

    def test_record_path_cloud_null_ambiguous_and_backup_blockers(self):
        self.capture_link()
        files = self.path_cloud_files("node_modules")
        cloud = FakeCloud(self, files).install()
        self.ok("record-path-cloud", "--journal", self.journal)
        payload = self.payload("path_cloud_recorded")
        self.assertIsNone(payload["cloud_preimage"])
        self.assertIsNone(payload["parent_id"])
        line = [
            line for line in
            self.journal.read_text(encoding="utf-8").splitlines()
            if '"path_cloud_recorded"' in line
        ][-1]
        self.assertIn(
            '"cloud_preimage":null', line,
            "an absent preimage must be the literal null, never an ID",
        )
        journal2 = self.state / "r2" / "journal.jsonl"
        self.capture_path(
            "venv", self.source / ".venv", policy="retain", journal=journal2,
        )
        files["pobj-2"] = dict(files["pobj-1"], id="pobj-2", name=".venv")
        files["pobj-1"] = dict(files["pobj-1"], name=".venv")
        cloud.list_hits[:] = [
            self.hit(".venv"), self.hit(".venv", file_id="pobj-2"),
        ]
        _stdout, stderr = self.expect_fail(
            "record-path-cloud", "--journal", journal2
        )
        self.assertIn("ambiguous", stderr)
        files["mobj-1"] = {
            "id": "mobj-1", "name": ".venv", "parents": ["machine-1"],
            "trashed": False, "ownedByMe": True,
        }
        files["machine-1"] = {"id": "machine-1", "name": "My MacBook"}
        cloud.list_hits[:] = [
            self.hit(".venv", file_id="mobj-1", parents=["machine-1"]),
        ]
        _stdout, stderr = self.expect_fail(
            "record-path-cloud", "--journal", journal2
        )
        self.assertIn("computer-backup", stderr)

    def test_verify_path_cloud_absent_accepts_trashed_preimage(self):
        path = self.capture_link()
        files = self.path_cloud_files("node_modules")
        cloud = FakeCloud(
            self, files, list_hits=[self.hit("node_modules")]
        ).install()
        self.ok("record-path-cloud", "--journal", self.journal)
        os.remove(path)
        files["pobj-1"]["trashed"] = True
        cloud.list_hits[:] = []
        self.ok("verify-path-cloud", "--journal", self.journal)
        payload = self.payload("path_cloud_verified")
        self.assertEqual("absent", payload["final_type"])
        self.assertEqual(0, payload["machine_root_count"])
        self.assertEqual([], cloud.mutations())

    def test_verify_path_cloud_rejects_each_blocker(self):
        path = self.capture_link()
        files = self.path_cloud_files("node_modules")
        cloud = FakeCloud(
            self, files, list_hits=[self.hit("node_modules")]
        ).install()
        self.ok("record-path-cloud", "--journal", self.journal)
        os.remove(path)
        cloud.list_hits[:] = []
        _stdout, stderr = self.expect_fail(
            "verify-path-cloud", "--journal", self.journal
        )
        self.assertIn("not trashed", stderr)
        files["pobj-1"]["trashed"] = True
        files["pobj-1"]["parents"] = ["elsewhere-1"]
        _stdout, stderr = self.expect_fail(
            "verify-path-cloud", "--journal", self.journal
        )
        self.assertIn("parent", stderr)
        files["pobj-1"]["parents"] = ["parent-1"]
        files["dup-1"] = {
            "id": "dup-1", "name": "node_modules", "parents": ["parent-1"],
            "trashed": False, "ownedByMe": True,
        }
        cloud.list_hits[:] = [self.hit("node_modules", file_id="dup-1")]
        _stdout, stderr = self.expect_fail(
            "verify-path-cloud", "--journal", self.journal
        )
        self.assertIn("occupies", stderr)
        files["mobj-1"] = {
            "id": "mobj-1", "name": "node_modules", "parents": ["machine-1"],
            "trashed": False, "ownedByMe": True,
        }
        files["machine-1"] = {"id": "machine-1", "name": "My MacBook"}
        cloud.list_hits[:] = [
            self.hit("node_modules", file_id="mobj-1", parents=["machine-1"]),
        ]
        _stdout, stderr = self.expect_fail(
            "verify-path-cloud", "--journal", self.journal
        )
        self.assertIn("computer-backup", stderr)
        self.assertEqual([], cloud.mutations())

    def test_verify_path_cloud_null_preimage_requires_absence(self):
        path = self.capture_link()
        files = self.path_cloud_files("node_modules")
        cloud = FakeCloud(self, files).install()
        self.ok("record-path-cloud", "--journal", self.journal)
        os.remove(path)
        self.ok("verify-path-cloud", "--journal", self.journal)
        self.assertIsNone(self.payload("path_cloud_verified")["object_id"])
        files["new-1"] = {
            "id": "new-1", "name": "node_modules", "parents": ["parent-1"],
            "trashed": False, "ownedByMe": True,
        }
        cloud.list_hits[:] = [self.hit("node_modules", file_id="new-1")]
        _stdout, stderr = self.expect_fail(
            "verify-path-cloud", "--journal", self.journal
        )
        self.assertIn("unexpected", stderr)

    def test_verify_path_cloud_checks_final_file_hashes(self):
        path = self.capture_dirty_tracked_file()
        final = b"original\n"
        md5 = hashlib.md5(final).hexdigest()
        files = self.path_cloud_files(
            "tracked.txt", mimeType="text/plain", md5Checksum=md5,
        )
        cloud = FakeCloud(
            self, files, list_hits=[self.hit("tracked.txt")]
        ).install()
        self.ok("record-path-cloud", "--journal", self.journal)
        path.write_bytes(final)
        self.ok("verify-path-cloud", "--journal", self.journal)
        self.assertEqual(
            "pobj-1", self.payload("path_cloud_verified")["object_id"]
        )
        manifest = self.root / "cloud.manifest"
        manifest.write_bytes(
            f"{hashlib.sha256(final).hexdigest()}  .\0".encode("utf-8")
        )
        self.ok(
            "verify-path-cloud", "--journal", self.journal,
            "--manifest", manifest,
        )
        manifest.write_bytes(f"{'0' * 64}  .\0".encode("utf-8"))
        _stdout, stderr = self.expect_fail(
            "verify-path-cloud", "--journal", self.journal,
            "--manifest", manifest,
        )
        self.assertIn("manifest", stderr)
        files["pobj-1"]["md5Checksum"] = "0" * 32
        _stdout, stderr = self.expect_fail(
            "verify-path-cloud", "--journal", self.journal
        )
        self.assertIn("hash", stderr)
        del files["pobj-1"]["md5Checksum"]
        _stdout, stderr = self.expect_fail(
            "verify-path-cloud", "--journal", self.journal
        )
        self.assertIn("hash", stderr)
        files["pobj-1"]["md5Checksum"] = md5
        files["gnew-1"] = dict(
            files["pobj-1"], id="gnew-1",
            mimeType="application/vnd.google-apps.document",
        )
        cloud.list_hits[:] = [self.hit("tracked.txt", file_id="gnew-1")]
        _stdout, stderr = self.expect_fail(
            "verify-path-cloud", "--journal", self.journal
        )
        self.assertIn("Google-native", stderr)
        self.assertEqual([], cloud.mutations())

    def test_restore_path_cloud_updates_only_journaled_ids(self):
        self.capture_link()
        files = self.path_cloud_files("node_modules")
        cloud = FakeCloud(
            self, files, list_hits=[self.hit("node_modules")]
        ).install()
        self.ok("record-path-cloud", "--journal", self.journal)
        files["pobj-1"]["trashed"] = True
        cloud.list_hits[:] = []
        _stdout, stderr = self.expect_fail(
            "restore-path-cloud", "--journal", self.journal
        )
        self.assertIn("pause", stderr)
        self.assertEqual([], cloud.mutations())
        self.install_providers()
        self.ok("restore-path-cloud", "--journal", self.journal)
        mutations = cloud.mutations()
        self.assertEqual(1, len(mutations))
        method, url, body = mutations[0]
        self.assertEqual("PATCH", method)
        self.assertIn("/files/pobj-1", url)
        self.assertEqual({"trashed": False}, json.loads(body))
        self.assertFalse(files["pobj-1"]["trashed"])
        payload = self.payload("path_cloud_restored")
        self.assertEqual("pobj-1", payload["object_id"])
        self.assertEqual(["parent-1"], payload["parents"])

    def test_restore_path_cloud_refusals(self):
        self.capture_link()
        files = self.path_cloud_files("node_modules")
        cloud = FakeCloud(self, files).install()
        self.ok("record-path-cloud", "--journal", self.journal)
        self.install_providers()
        _stdout, stderr = self.expect_fail(
            "restore-path-cloud", "--journal", self.journal
        )
        self.assertIn("cloud_preimage", stderr)
        journal2 = self.state / "r2" / "journal.jsonl"
        self.capture_path(
            "venv", self.source / ".venv", policy="retain", journal=journal2,
        )
        files["pobj-1"]["name"] = ".venv"
        cloud.list_hits[:] = [self.hit(".venv")]
        self.ok("record-path-cloud", "--journal", journal2)
        cloud.list_hits[:] = []
        _stdout, stderr = self.expect_fail(
            "restore-path-cloud", "--journal", journal2
        )
        self.assertIn("Trash", stderr)
        files["pobj-1"]["trashed"] = True
        files["pobj-1"]["parents"] = ["elsewhere-1"]
        _stdout, stderr = self.expect_fail(
            "restore-path-cloud", "--journal", journal2
        )
        self.assertIn("parent", stderr)
        files["pobj-1"]["parents"] = ["parent-1"]
        files["dup-1"] = {
            "id": "dup-1", "name": ".venv", "parents": ["parent-1"],
            "trashed": False, "ownedByMe": True,
        }
        cloud.list_hits[:] = [self.hit(".venv", file_id="dup-1")]
        _stdout, stderr = self.expect_fail(
            "restore-path-cloud", "--journal", journal2
        )
        self.assertIn("duplicate", stderr)
        self.assertEqual([], cloud.mutations())

    # ------------------------------------------------------------------
    # close-rollback-window and finalize-path
    # ------------------------------------------------------------------

    def test_close_rollback_window_for_path_repair(self):
        path = self.capture_link()
        files = self.path_cloud_files("node_modules")
        cloud = FakeCloud(
            self, files, list_hits=[self.hit("node_modules")]
        ).install()
        self.ok("record-path-cloud", "--journal", self.journal)
        os.remove(path)
        self.install_providers()
        baseline = self.make_baseline()
        tools = self.install_tools()
        self.ok("record-native-errors", "--journal", baseline)
        files["pobj-1"]["trashed"] = True
        cloud.list_hits[:] = []
        argv = [
            "close-rollback-window", "--journal", self.journal,
            "--baseline-journal", baseline,
            "--restart-before-pid", "1111",
        ]
        _stdout, stderr = self.expect_fail(*argv)
        self.assertIn("verify-path-local", stderr)
        self.ok("verify-path-local", "--journal", self.journal)
        _stdout, stderr = self.expect_fail(*argv)
        self.assertIn("verify-path-cloud", stderr)
        self.ok("verify-path-cloud", "--journal", self.journal)
        stdout, _stderr = self.ok(*argv)
        self.assertEqual(self.success_stdout(self.journal), stdout)
        closed = self.payload("rollback_window_closed")
        self.assertEqual("nm", closed["label"])
        self.assertEqual({"outbound": 7}, closed["queue_cursors"])
        self.assertEqual(4242, closed["drive_pid"])
        self.assertEqual(1111, closed["restart_before_pid"])
        _stdout, stderr = self.expect_fail(*argv)
        self.assertIn("closed", stderr)
        self.ok("finalize-path", "--journal", self.journal)
        self.assertFalse((self.targets / "node_modules").exists())

    def test_finalize_path_requires_rollback_window_closed(self):
        path = self.capture_link()
        os.remove(path)
        _stdout, stderr = self.expect_fail(
            "finalize-path", "--journal", self.journal
        )
        self.assertIn("rollback_window_closed", stderr)
        self.assertTrue((self.targets / "node_modules").is_dir())

    def test_finalize_path_disposable_rechecks_target(self):
        path = self.capture_link()
        os.remove(path)
        self.module._append_event(
            self.journal, "rollback_window_closed", {}
        )
        (self.targets / "node_modules" / "new.txt").write_bytes(b"x")
        _stdout, stderr = self.expect_fail(
            "finalize-path", "--journal", self.journal
        )
        self.assertIn("changed", stderr)
        self.assertTrue((self.targets / "node_modules").is_dir())
        os.remove(self.targets / "node_modules" / "new.txt")
        path.symlink_to(self.targets / "node_modules")
        _stdout, stderr = self.expect_fail(
            "finalize-path", "--journal", self.journal
        )
        self.assertIn("referenc", stderr)
        self.assertTrue((self.targets / "node_modules").is_dir())
        os.remove(path)
        self.ok("finalize-path", "--journal", self.journal)
        self.assertFalse((self.targets / "node_modules").exists())
        trashed = list((self.journal.parent / "trash").rglob("dep.js"))
        self.assertEqual(1, len(trashed))
        self.assertEqual(
            1, self.event_types().count("path_target_finalized")
        )
        self.assertEqual(
            1, self.event_types().count("private_preimage_finalized")
        )
        self.ok("verify-journal", "--journal", self.journal)
        _stdout, stderr = self.expect_fail(
            "finalize-path", "--journal", self.journal
        )
        self.assertIn("already", stderr)

    def test_finalize_path_retains_user_data_target(self):
        path = self.source / ".venv"
        self.capture_path("venv", path, policy="retain")
        os.remove(path)
        self.module._append_event(
            self.journal, "rollback_window_closed", {}
        )
        self.ok("finalize-path", "--journal", self.journal)
        self.assertTrue(
            (self.targets / "venv").is_dir(),
            "a retained target must never be touched",
        )
        self.assertEqual(
            0, self.event_types().count("path_target_finalized")
        )
        self.assertEqual([], self.payload("private_preimage_finalized")["assets"])

    def test_finalize_path_removes_only_private_preimage_assets(self):
        path = self.capture_dirty_tracked_file()
        sha = hashlib.sha256(b"corrupted\n").hexdigest()
        asset = self.journal.parent / "preimage" / "blobs" / sha
        self.assertTrue(asset.is_file())
        path.write_bytes(b"original\n")
        self.module._append_event(
            self.journal, "rollback_window_closed", {}
        )
        self.ok("finalize-path", "--journal", self.journal)
        self.assertFalse(asset.exists())
        self.assertFalse((self.journal.parent / "preimage").exists())
        payload = self.payload("private_preimage_finalized")
        self.assertEqual(
            [{"path": f"blobs/{sha}", "sha256": sha}], payload["assets"]
        )
        self.assertEqual(
            b"original\n", path.read_bytes(),
            "finalize never touches the repaired path",
        )
        self.install_providers()
        _stdout, stderr = self.expect_fail(
            "rollback-path-local", "--journal", self.journal
        )
        self.assertIn("closed", stderr)

    # ------------------------------------------------------------------
    # Symlink-escape containment, mutation gates, and hash guards
    # ------------------------------------------------------------------

    def test_capture_path_rejects_symlink_escaping_replacement(self):
        # Reviewer probe: root/sub -> ../../outside makes a textually
        # contained replacement physically escape the journaled root.
        outside = self.root / "outside"
        outside.mkdir()
        (self.source / "sub").symlink_to(Path("../../outside"))
        path = self.source / "notes.txt"
        path.write_bytes(b"n\n")
        _stdout, stderr = self.expect_fail(
            *self.capture_argv(
                "x", path,
                replacements=[self.source / "sub" / "new.txt"],
                policy="retain",
            )
        )
        self.assertIn("symlink", stderr)
        self.assertFalse(
            self.journal.parent.exists(), "no journal may outlive a blocker"
        )

    def test_rollback_path_local_blocks_symlinked_replacement_component(self):
        outside = self.root / "outside"
        outside.mkdir()
        subdir = self.source / "subdir"
        subdir.mkdir()
        path = self.source / "notes.txt"
        path.write_bytes(b"alpha\n")
        replacement = subdir / "new.txt"
        self.capture_path(
            "notes", path, replacements=[replacement],
            final_type="absent", policy="retain",
        )
        self.confirm_paused()
        os.remove(path)
        os.rmdir(subdir)
        subdir.symlink_to(outside)
        (outside / "new.txt").write_bytes(b"escaped\n")
        _stdout, stderr = self.expect_fail(
            "rollback-path-local", "--journal", self.journal
        )
        self.assertIn("symlink", stderr)
        self.assertEqual(
            b"escaped\n", (outside / "new.txt").read_bytes(),
            "nothing outside the journaled root may ever move to trash",
        )
        _stdout, stderr = self.expect_fail(
            "verify-path-local", "--journal", self.journal
        )
        self.assertIn("symlink", stderr)

    def test_finalize_path_blocks_interior_target_reference(self):
        path = self.capture_link()
        os.remove(path)
        self.module._append_event(
            self.journal, "rollback_window_closed", {}
        )
        alias = self.source / "dep-alias"
        alias.symlink_to(self.targets / "node_modules" / "dep.js")
        _stdout, stderr = self.expect_fail(
            "finalize-path", "--journal", self.journal
        )
        self.assertIn("referenc", stderr)
        self.assertTrue(
            (self.targets / "node_modules" / "dep.js").is_file(),
            "an interior-referenced target must never be trashed",
        )
        os.remove(alias)
        self.ok("finalize-path", "--journal", self.journal)
        self.assertFalse((self.targets / "node_modules").exists())

    def test_record_path_cloud_refuses_after_local_mutation(self):
        path = self.capture_link()
        cloud = FakeCloud(
            self, self.path_cloud_files("node_modules")
        ).install()
        os.remove(path)
        _stdout, stderr = self.expect_fail(
            "record-path-cloud", "--journal", self.journal
        )
        self.assertIn("preimage", stderr)
        self.assertEqual(
            [], cloud.requests,
            "the local pre-mutation gate must fire before any cloud call",
        )
        self.assertNotIn("path_cloud_recorded", self.event_types())

    def test_verify_path_cloud_never_hashes_through_a_symlink(self):
        path = self.capture_dirty_tracked_file()
        final = b"original\n"
        files = self.path_cloud_files(
            "tracked.txt", mimeType="text/plain",
            md5Checksum=hashlib.md5(final).hexdigest(),
        )
        FakeCloud(
            self, files, list_hits=[self.hit("tracked.txt")]
        ).install()
        self.ok("record-path-cloud", "--journal", self.journal)
        other = self.source / "other.txt"
        other.write_bytes(final)
        os.remove(path)
        path.symlink_to(other)
        _stdout, stderr = self.expect_fail(
            "verify-path-cloud", "--journal", self.journal
        )
        self.assertIn("regular file", stderr)

    def test_rollback_path_local_restores_exact_symlink_mode(self):
        path = self.source / "node_modules"
        recorded_mode = stat.S_IMODE(os.lstat(path).st_mode)
        self.capture_path("nm", path)
        self.confirm_paused()
        drifted = 0o700 if recorded_mode != 0o700 else 0o755
        os.chmod(path, drifted, follow_symlinks=False)
        self.ok("rollback-path-local", "--journal", self.journal)
        self.assertTrue(os.path.islink(path))
        self.assertEqual(
            recorded_mode, stat.S_IMODE(os.lstat(path).st_mode),
            "rollback must restore the exact journaled symlink mode",
        )
        trash = self.payload("path_local_rolled_back")["trash"]
        self.assertEqual(1, len(trash))
        self.assertEqual(str(path), trash[0]["path"])

    # ------------------------------------------------------------------
    # --force and corrupted journals
    # ------------------------------------------------------------------

    def test_path_commands_reject_force_and_corrupt_journals(self):
        path = self.capture_link()
        journal_argvs = [
            ["confirm-path-paused", "--journal", str(self.journal)],
            ["verify-path-local", "--journal", str(self.journal)],
            ["rollback-path-local", "--journal", str(self.journal)],
            ["record-path-cloud", "--journal", str(self.journal)],
            ["verify-path-cloud", "--journal", str(self.journal)],
            ["restore-path-cloud", "--journal", str(self.journal)],
            ["finalize-path", "--journal", str(self.journal)],
        ]
        capture_argv = [
            str(item) for item in self.capture_argv(
                "f", path, journal=self.state / "force" / "journal.jsonl",
            )
        ]
        for argv in journal_argvs + [capture_argv]:
            stderr = io.StringIO()
            with self.assertRaises(SystemExit, msg=argv[0]):
                with contextlib.redirect_stderr(stderr):
                    self.module.main(argv + ["--force"])
            self.assertIn("--force", stderr.getvalue(), argv[0])
        self.craft_line(self.journal, "drive_paused", event_hash="f" * 64)
        corrupted = self.journal.read_bytes()
        sentinel = os.readlink(path)
        for argv in journal_argvs:
            _stdout, stderr = self.expect_fail(*argv)
            self.assertIn("journal", stderr.lower(), argv[0])
            self.assertEqual(
                corrupted, self.journal.read_bytes(),
                f"{argv[0]} repaired or truncated the journal",
            )
        self.assertEqual(
            sentinel, os.readlink(path),
            "no command may touch the path behind a corrupt journal",
        )


if __name__ == "__main__":
    unittest.main()
