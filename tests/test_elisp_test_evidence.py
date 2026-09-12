from __future__ import annotations

import base64
from contextlib import contextmanager
import json
import os
from pathlib import Path
import runpy
import subprocess
import tempfile
import time
import unittest
from unittest import mock


DOTFILES = Path(__file__).resolve().parents[1]
REVISION_HELPER = DOTFILES / "claude/bin/elisp-source-revision"
BATCH_TEST = DOTFILES / "claude/bin/batch-test.sh"
REBUILD_WAIT = DOTFILES / "claude/bin/elpaca-rebuild-wait"
CHECK_EVIDENCE = DOTFILES / "claude/bin/elisp-check-evidence"
LIVE_VERIFY = DOTFILES / "claude/bin/elisp-live-verify"
EVIDENCE_LIB = DOTFILES / "claude/hooks/lib-elisp-evidence.sh"


def write_rebuild_emacsclient(path):
    """Disposable fake transport shared by the rebuild and live-verify tests."""
    path.write_text('''#!/usr/bin/env python3
import base64, json, os, pathlib, subprocess, sys
args = " ".join(sys.argv[1:])
for key in ("EMACSCLIENT_CALLED", "FAKE_EMACSCLIENT_LOG"):
    if os.environ.get(key):
        with open(os.environ[key], "a") as stream:
            stream.write(args + "\\034")
source = os.environ["FAKE_PACKAGE_SOURCE"]
package = os.environ["FAKE_PACKAGE_ID"]
def emit(value):
    print(json.dumps(base64.b64encode(json.dumps(value).encode()).decode()))
if "elpaca-extras-resolve-package" in args:
    if os.environ.get("FAKE_RESOLVE_ERROR"):
        print(os.environ["FAKE_RESOLVE_ERROR"], file=sys.stderr)
        sys.exit(1)
    enc = lambda value: base64.b64encode(value.encode()).decode()
    print(json.dumps(package + ":" + enc(source + "/") + ":" + enc(pathlib.Path(source).name)))
elif "elpaca-rebuild-context-v1" in args:
    runtime = dict(package=package, pid=int(os.environ.get("FAKE_PID", "123")),
                   start=os.environ.get("FAKE_START", "fixture-start"),
                   profile=os.environ.get("FAKE_PROFILE", str(pathlib.Path(source).parent)),
                   source=os.environ.get("FAKE_RUNTIME_SOURCE", source))
    runtime_file = os.environ.get("FAKE_RUNTIME_FILE")
    if runtime_file and pathlib.Path(runtime_file).exists():
        runtime.update(json.loads(pathlib.Path(runtime_file).read_text()))
    if "elpaca-extras-rebuild-and-reload" in args:
        if os.environ.get("FAKE_REQUEST_MARKER"):
            pathlib.Path(os.environ["FAKE_REQUEST_MARKER"]).write_text("requested")
        if os.environ.get("FAKE_REQUEST_ERROR"):
            print(os.environ["FAKE_REQUEST_ERROR"], file=sys.stderr)
            sys.exit(1)
        if os.environ.get("FAKE_AFTER_REQUEST"):
            pathlib.Path(runtime_file).write_text(os.environ["FAKE_AFTER_REQUEST"])
        if os.environ.get("FAKE_DIRTY_AFTER_REQUEST"):
            pathlib.Path(os.environ["FAKE_DIRTY_AFTER_REQUEST"]).write_text("changed")
        if os.environ.get("FAKE_STATUS_REPLACE_AFTER_REQUEST"):
            status = pathlib.Path(os.environ["FAKE_STATUS_REPLACE_AFTER_REQUEST"])
            replacement = status.with_name("producer-temporary")
            replacement.write_text("pending:queued by post-commit\\n")
            replacement.replace(status)
        emit(dict(token=os.environ.get("FAKE_TOKEN", "token-1")))
    elif "elpaca-extras-build-reload-status" in args:
        emit(dict(runtime=runtime, package=os.environ.get("FAKE_TOKEN_PACKAGE", package),
                  state=os.environ.get("FAKE_TOKEN_STATE", "finished")))
    else:
        emit(runtime)
elif "unload-feature" in args:
    print("t")
else:
    if os.environ.get("FAKE_LIVE_EDIT_SOURCE"):
        pathlib.Path(os.environ["FAKE_LIVE_EDIT_SOURCE"]).write_text("(provide 'live-changed)\\n")
    if os.environ.get("FAKE_LIVE_COMMIT_REPO"):
        subprocess.run(["git", "-C", os.environ["FAKE_LIVE_COMMIT_REPO"],
                        "commit", "--allow-empty", "-qm", "fixture concurrent commit"], check=True)
    print(os.environ.get("FAKE_LIVE_RESULT", "t"))
''')
    path.chmod(0o755)


def run(command: list[str], **kwargs) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, text=True, capture_output=True, check=False, **kwargs)


def evidence_environment(receipt_dir: Path) -> dict[str, str]:
    env = os.environ.copy()
    env["ELISP_EVIDENCE_RECEIPT_DIR"] = str(receipt_dir)
    return env


def issue_evidence(
    kind: str, repo: Path, label: str, identity: str, receipt_dir: Path
) -> str:
    canonical_repo = run(
        ["git", "-C", str(repo), "rev-parse", "--show-toplevel"]
    ).stdout.strip()
    repo_b64 = base64.b64encode(canonical_repo.encode()).decode()
    label_b64 = base64.b64encode(label.encode()).decode()
    result = run(
        [
            "bash",
            "-c",
            'source "$1"; elisp_evidence_emit "$2" "$3" "$4" "$5"',
            "issue-evidence",
            str(EVIDENCE_LIB),
            kind,
            repo_b64,
            label_b64,
            identity,
        ],
        env=evidence_environment(receipt_dir),
    )
    if result.returncode != 0:
        raise RuntimeError(result.stderr or "failed to issue evidence receipt")
    return result.stdout.strip()


def init_repo(path: Path, filename: str = "example.el") -> Path:
    path.mkdir(parents=True)
    subprocess.run(["git", "init", "-q", str(path)], check=True)
    subprocess.run(["git", "-C", str(path), "config", "user.email", "test@example.com"], check=True)
    subprocess.run(["git", "-C", str(path), "config", "user.name", "Hook Test"], check=True)
    source = path / filename
    source.parent.mkdir(parents=True, exist_ok=True)
    source.write_text("(provide 'example)\n")
    subprocess.run(["git", "-C", str(path), "add", filename], check=True)
    subprocess.run(["git", "-C", str(path), "commit", "-qm", "baseline"], check=True)
    return path


class ElispSourceRevisionTests(unittest.TestCase):
    def test_revision_tracks_source_content(self):
        with tempfile.TemporaryDirectory() as directory:
            repo = init_repo(Path(directory) / "example")
            before = run([str(REVISION_HELPER), str(repo)])
            self.assertEqual(before.returncode, 0, before.stderr)
            (repo / "example.el").write_text("(provide 'changed)\n")
            after = run([str(REVISION_HELPER), str(repo)])
            self.assertEqual(after.returncode, 0, after.stderr)
            self.assertNotEqual(before.stdout, after.stdout)

    def test_index_revision_matches_only_staged_bytes(self):
        with tempfile.TemporaryDirectory() as directory:
            repo = init_repo(Path(directory) / "example")
            (repo / "example.el").write_text("(provide 'staged)\n")
            subprocess.run(["git", "-C", str(repo), "add", "example.el"], check=True)
            staged = run([str(REVISION_HELPER), "--index", str(repo)]).stdout.strip()
            working_before = run([str(REVISION_HELPER), str(repo)]).stdout.strip()
            self.assertEqual(staged, working_before)
            (repo / "example.el").write_text("(provide 'unstaged)\n")
            working_after = run([str(REVISION_HELPER), str(repo)]).stdout.strip()
            self.assertNotEqual(staged, working_after)
            self.assertEqual(
                staged,
                run([str(REVISION_HELPER), "--index", str(repo)]).stdout.strip(),
            )


class BatchTestTests(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        self.root = Path(self.temp_dir.name)
        self.receipt_dir = self.root / "receipts"
        self.home = self.root / "home"
        self.source = init_repo(
            self.home / ".config/emacs-profiles/test/elpaca/sources/example"
        )
        self.fake_bin = self.root / "bin"
        self.fake_bin.mkdir()
        emacsclient = self.fake_bin / "emacsclient"
        emacsclient.write_text(
            "#!/usr/bin/env python3\n"
            "import base64, json, os, sys\n"
            "source = os.environ['FAKE_BATCH_SOURCE']\n"
            "encode = lambda value: base64.b64encode(value.encode()).decode()\n"
            "if 'elpaca-extras-resolve-package' in ' '.join(sys.argv):\n"
            "    print(json.dumps('example:' + encode(source) + ':' + encode('example')))\n"
            "else:\n"
            "    print(json.dumps(encode(json.dumps({'id': 'example', 'source': source, 'builds': [], 'package_build': source + '/build'}))))\n"
        )
        emacsclient.chmod(0o755)
        emacs = self.fake_bin / "emacs"
        emacs.write_text(
            "#!/bin/sh\n"
            "printf '%s\\n' \"$@\"\n"
            "if [ -n \"${FAKE_EMACS_EDIT_SOURCE:-}\" ]; then\n"
            "  printf '%s\\n' \"(provide 'edited-during-check)\" > \"$FAKE_EMACS_EDIT_SOURCE\"\n"
            "fi\n"
            "if [ -n \"${FAKE_EMACS_STALE:-}\" ]; then\n"
            "  printf '%s\\n' 'Source file newer than byte-compiled file; using older file'\n"
            "fi\n"
        )
        emacs.chmod(0o755)

    def environment(self, stale: bool = False) -> dict[str, str]:
        env = os.environ.copy()
        env["HOME"] = str(self.home)
        env["PATH"] = f"{self.fake_bin}:{env['PATH']}"
        env["ELISP_EVIDENCE_RECEIPT_DIR"] = str(self.receipt_dir)
        env["FAKE_BATCH_SOURCE"] = str(self.source)
        if stale:
            env["FAKE_EMACS_STALE"] = "1"
        return env

    def test_loads_canonical_standalone_source_and_emits_evidence(self):
        result = run([str(BATCH_TEST), "example"], env=self.environment())
        self.assertEqual(result.returncode, 0, result.stderr)
        encoded_source = base64.b64encode(str((self.source / "example.el").resolve()).encode()).decode()
        encoded_source_dir = base64.b64encode(str(self.source.resolve()).encode()).decode()
        self.assertTrue(result.stdout.startswith("-Q\n--batch\n"))
        self.assertIn(encoded_source, result.stdout)
        self.assertIn(
            f"add-to-list 'load-path (decode-coding-string (base64-decode-string \"{encoded_source_dir}\")",
            result.stdout,
        )
        self.assertRegex(
            result.stdout,
            r"(?m)^ELISP_TEST_EVIDENCE_V2:[^:]+:[^:]+:[0-9a-f]{64}:receipt\.[A-Za-z0-9]+$",
        )

    def test_stale_load_warning_fails_without_evidence(self):
        result = run([str(BATCH_TEST), "example"], env=self.environment(stale=True))
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_source_change_during_batch_check_emits_no_evidence(self):
        env = self.environment()
        env["FAKE_EMACS_EDIT_SOURCE"] = str(self.source / "example.el")
        result = run([str(BATCH_TEST), "example"], env=env)
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)


class ElispCheckEvidenceTests(unittest.TestCase):
    def project_check_fixture(self, root, body="exit 0\n"):
        repo = init_repo(root / "project", ".dir-locals.el")
        check = repo / "check.sh"
        check.write_text("#!/bin/sh\n" + body)
        check.chmod(0o755)
        subprocess.run(["git", "-C", str(repo), "add", "check.sh"], check=True)
        subprocess.run(["git", "-C", str(repo), "commit", "-qm", "add check"], check=True)
        return repo, check

    def snapshot_fixture_environment(self, root, repo, action):
        fake_bin = root / "bin"
        fake_bin.mkdir()
        real_git = run(["which", "git"]).stdout.strip()
        git = fake_bin / "git"
        git.write_text('''#!/usr/bin/env python3
import os, pathlib, subprocess, sys
real = os.environ["ELISP_FIXTURE_REAL_GIT"]
args = sys.argv[1:]
if "cat-file" in args and "--batch" in args:
    if os.environ["ELISP_FIXTURE_CHECKOUT_ACTION"] == "fail":
        sys.exit(42)
    result = subprocess.run([real, *args], check=False)
    if result.returncode:
        sys.exit(result.returncode)
    repo = pathlib.Path(os.environ["ELISP_FIXTURE_REPO"])
    (repo / ".dir-locals.el").write_text("((nil . ((fill-column . 81))))\\n")
    original_env = os.environ.copy()
    original_env.pop("GIT_INDEX_FILE", None)
    subprocess.run([real, "-C", str(repo), "add", ".dir-locals.el"],
                   env=original_env, check=True)
    sys.exit(0)
os.execv(real, [real, *args])
''')
        git.chmod(0o755)
        real_mktemp = run(["which", "mktemp"]).stdout.strip()
        mktemp = fake_bin / "mktemp"
        mktemp.write_text('''#!/usr/bin/env python3
import os, subprocess, sys
result = subprocess.run([os.environ["ELISP_FIXTURE_REAL_MKTEMP"], *sys.argv[1:]],
                        text=True, capture_output=True)
if result.returncode == 0:
    with open(os.environ["ELISP_FIXTURE_ALLOCATION_LOG"], "a") as log:
        log.write(result.stdout)
print(result.stdout, end="")
print(result.stderr, end="", file=sys.stderr)
sys.exit(result.returncode)
''')
        mktemp.chmod(0o755)
        scratch = root / "scratch"
        scratch.mkdir()
        env = evidence_environment(root / "receipts")
        env.update(ELISP_FIXTURE_REAL_GIT=real_git,
                   ELISP_FIXTURE_REPO=str(repo),
                   ELISP_FIXTURE_CHECKOUT_ACTION=action,
                   ELISP_FIXTURE_REAL_MKTEMP=real_mktemp,
                   ELISP_FIXTURE_ALLOCATION_LOG=str(root / "allocations"),
                   TMPDIR=str(scratch), PATH=f"{fake_bin}:{env['PATH']}")
        return env

    def test_index_change_during_materialization_cannot_certify_older_snapshot(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo, check = self.project_check_fixture(
                root, "touch \"$ELISP_FIXTURE_CHECK_RAN\"\ngrep -q 'fill-column . 80' .dir-locals.el\n")
            (repo / ".dir-locals.el").write_text("((nil . ((fill-column . 80))))\n")
            subprocess.run(["git", "-C", str(repo), "add", ".dir-locals.el"], check=True)
            env = self.snapshot_fixture_environment(root, repo, "advance")
            marker = root / "check-ran"
            env["ELISP_FIXTURE_CHECK_RAN"] = str(marker)
            result = run([str(CHECK_EVIDENCE), "--staged", "file:.dir-locals.el",
                          "--", str(check)], cwd=repo, env=env)
            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)
            self.assertFalse(marker.exists(), "do not run a snapshot already known to be stale")
            self.assertEqual(list((root / "scratch").iterdir()), [])
            self.assertTrue((root / "allocations").read_text().strip())
            self.assertTrue(all(not Path(path).exists() for path in (root / "allocations").read_text().splitlines()))

    def test_executable_symlink_cannot_escape_owner_or_staged_snapshot(self):
        for staged in (False, True):
            with self.subTest(staged=staged), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                repo, check = self.project_check_fixture(root)
                outside = root / "outside.sh"
                outside.write_text("#!/bin/sh\ntouch \"$ELISP_FIXTURE_CHECK_RAN\"\n")
                outside.chmod(0o755)
                check.unlink()
                check.symlink_to(outside)
                subprocess.run(["git", "-C", str(repo), "add", "check.sh"], check=True)
                marker = root / "escaped"
                env = evidence_environment(root / "receipts")
                env["ELISP_FIXTURE_CHECK_RAN"] = str(marker)
                args = [str(CHECK_EVIDENCE)] + (["--staged"] if staged else [])
                result = run(args + ["file:.dir-locals.el", "--", str(check)], cwd=repo, env=env)
                self.assertNotEqual(result.returncode, 0, result.stdout)
                self.assertFalse(marker.exists())
                self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_staged_executable_symlink_is_rejected_when_worktree_file_is_regular(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo, check = self.project_check_fixture(root)
            outside = root / "outside.sh"
            outside.write_text("#!/bin/sh\ntouch \"$ELISP_FIXTURE_CHECK_RAN\"\n")
            outside.chmod(0o755)
            check.unlink()
            check.symlink_to(outside)
            subprocess.run(["git", "-C", str(repo), "add", "check.sh"], check=True)
            check.unlink()
            check.write_text("#!/bin/sh\nexit 0\n")
            check.chmod(0o755)
            marker = root / "escaped"
            env = evidence_environment(root / "receipts")
            env["ELISP_FIXTURE_CHECK_RAN"] = str(marker)
            result = run([str(CHECK_EVIDENCE), "--staged", "file:.dir-locals.el",
                          "--", str(check)], cwd=repo, env=env)
            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertFalse(marker.exists())
            self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_failed_snapshot_materialization_cleans_temporary_artifacts(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo, check = self.project_check_fixture(root)
            env = self.snapshot_fixture_environment(root, repo, "fail")
            result = run([str(CHECK_EVIDENCE), "--staged", "file:.dir-locals.el",
                          "--", str(check)], cwd=repo, env=env)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(list((root / "scratch").iterdir()), [])
            self.assertTrue((root / "allocations").read_text().strip())
            self.assertTrue(all(not Path(path).exists() for path in (root / "allocations").read_text().splitlines()))

    def test_staged_source_symlink_cannot_read_outside_snapshot(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo, check = self.project_check_fixture(root, "grep -q 'fill-column . 99' .dir-locals.el\n")
            outside = root / "outside.el"
            outside.write_text("((nil . ((fill-column . 99))))\n")
            source = repo / ".dir-locals.el"
            source.unlink()
            source.symlink_to(outside)
            subprocess.run(["git", "-C", str(repo), "add", ".dir-locals.el"], check=True)
            result = run([str(CHECK_EVIDENCE), "--staged", "file:.dir-locals.el",
                          "--", str(check)], cwd=repo,
                         env=evidence_environment(root / "receipts"))
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("symlink escapes", result.stderr)
            self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_staged_internal_source_symlink_reads_indexed_target(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo, check = self.project_check_fixture(root, "grep -q 'fill-column . 80' .dir-locals.el\n")
            source = repo / ".dir-locals.el"
            source.unlink()
            source.symlink_to("options.el")
            options = repo / "options.el"
            options.write_text("((nil . ((fill-column . 80))))\n")
            subprocess.run(["git", "-C", str(repo), "add", ".dir-locals.el", "options.el"], check=True)
            options.write_text("((nil . ((fill-column . 99))))\n")
            result = run([str(CHECK_EVIDENCE), "--staged", "file:.dir-locals.el",
                          "--", str(check)], cwd=repo,
                         env=evidence_environment(root / "receipts"))
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_staged_snapshot_supports_split_index(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo, check = self.project_check_fixture(root, "grep -q 'fill-column . 80' .dir-locals.el\n")
            source = repo / ".dir-locals.el"
            source.write_text("((nil . ((fill-column . 80))))\n")
            subprocess.run(["git", "-C", str(repo), "add", ".dir-locals.el"], check=True)
            subprocess.run(["git", "-C", str(repo), "update-index", "--split-index"], check=True)
            source.write_text("((nil . ((fill-column . 99))))\n")
            result = run([str(CHECK_EVIDENCE), "--staged", "file:.dir-locals.el",
                          "--", str(check)], cwd=repo,
                         env=evidence_environment(root / "receipts"))
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn(run([str(REVISION_HELPER), "--index", str(repo)]).stdout.strip(), result.stdout)

    def test_output_allocation_failure_cleans_materialized_snapshot(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo, check = self.project_check_fixture(root)
            fake_bin = root / "bin"
            fake_bin.mkdir()
            real_mktemp = run(["which", "mktemp"]).stdout.strip()
            mktemp = fake_bin / "mktemp"
            mktemp.write_text('''#!/usr/bin/env python3
import os, subprocess, sys
if any("/elisp-check." in arg for arg in sys.argv[1:]):
    sys.exit(42)
real = os.environ["ELISP_FIXTURE_REAL_MKTEMP"]
result = subprocess.run([real, *sys.argv[1:]], text=True, capture_output=True)
if result.returncode == 0:
    with open(os.environ["ELISP_FIXTURE_ALLOCATION_LOG"], "a") as log:
        log.write(result.stdout)
print(result.stdout, end="")
print(result.stderr, end="", file=sys.stderr)
sys.exit(result.returncode)
''')
            mktemp.chmod(0o755)
            scratch = root / "scratch"
            scratch.mkdir()
            env = evidence_environment(root / "receipts")
            env.update(ELISP_FIXTURE_REAL_MKTEMP=real_mktemp,
                       ELISP_FIXTURE_ALLOCATION_LOG=str(root / "allocations"),
                       TMPDIR=str(scratch), PATH=f"{fake_bin}:{env['PATH']}")
            result = run([str(CHECK_EVIDENCE), "--staged", "file:.dir-locals.el",
                          "--", str(check)], cwd=repo, env=env)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(list(scratch.iterdir()), [])
            self.assertTrue((root / "allocations").read_text().strip())
            self.assertTrue(all(not Path(path).exists() for path in (root / "allocations").read_text().splitlines()))

    def test_staged_materialization_uses_raw_blobs_without_smudge_filters(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo, check = self.project_check_fixture(root, "grep -q 'fill-column . 80' .dir-locals.el\n")
            (repo / ".dir-locals.el").write_text("((nil . ((fill-column . 80))))\n")
            (repo / ".gitattributes").write_text(".dir-locals.el filter=fixture\n")
            subprocess.run(["git", "-C", str(repo), "add", ".dir-locals.el", ".gitattributes"], check=True)
            smudge = root / "smudge.sh"
            smudge.write_text("#!/bin/sh\ntouch \"$ELISP_FIXTURE_SMUDGE_RAN\"\nsed 's/80/99/g'\n")
            smudge.chmod(0o755)
            subprocess.run(["git", "-C", str(repo), "config", "filter.fixture.smudge", str(smudge)], check=True)
            marker = root / "smudge-ran"
            env = evidence_environment(root / "receipts")
            env["ELISP_FIXTURE_SMUDGE_RAN"] = str(marker)
            result = run([str(CHECK_EVIDENCE), "--staged", "file:.dir-locals.el",
                          "--", str(check)], cwd=repo, env=env)
            self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
            self.assertFalse(marker.exists(), "snapshot creation must not execute configured smudge filters")
            self.assertIn(run([str(REVISION_HELPER), "--index", str(repo)]).stdout.strip(), result.stdout)

    def test_staged_snapshot_uses_private_tmp_even_when_TMPDIR_is_inside_repo(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo, check = self.project_check_fixture(root, "pwd > \"$ELISP_FIXTURE_RUN_ROOT\"\n")
            unsafe = repo / "scratch"
            unsafe.mkdir()
            recorded = root / "run-root"
            env = evidence_environment(root / "receipts")
            env.update(TMPDIR=str(unsafe), ELISP_FIXTURE_RUN_ROOT=str(recorded))
            result = run([str(CHECK_EVIDENCE), "--staged", "file:.dir-locals.el",
                          "--", str(check)], cwd=repo, env=env)
            self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
            snapshot = Path(recorded.read_text().strip())
            self.assertEqual(snapshot.parent.parent.resolve(), Path("/tmp").resolve())
            self.assertFalse(snapshot.exists())
            self.assertEqual(list(unsafe.iterdir()), [])

    def test_project_check_emits_file_labeled_evidence(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo = init_repo(root / "project", ".dir-locals.el")
            check = repo / "check.sh"
            check.write_text("#!/bin/sh\nexit 0\n")
            check.chmod(0o755)
            subprocess.run(["git", "-C", str(repo), "add", "check.sh"], check=True)
            subprocess.run(["git", "-C", str(repo), "commit", "-qm", "add check"], check=True)
            result = run(
                [str(CHECK_EVIDENCE), "file:.dir-locals.el", "--", str(check)],
                cwd=repo,
                env=evidence_environment(root / "receipts"),
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            label = base64.b64encode(b"file:.dir-locals.el").decode()
            self.assertRegex(
                result.stdout,
                rf"(?m)^ELISP_TEST_EVIDENCE_V2:[^:]+:{label}:[0-9a-f]{{64}}:receipt\.[A-Za-z0-9]+$",
            )

    def test_failed_project_check_emits_no_evidence(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo = init_repo(root / "project", ".dir-locals.el")
            check = repo / "check.sh"
            check.write_text("#!/bin/sh\nexit 1\n")
            check.chmod(0o755)
            subprocess.run(["git", "-C", str(repo), "add", "check.sh"], check=True)
            subprocess.run(["git", "-C", str(repo), "commit", "-qm", "add check"], check=True)
            result = run(
                [str(CHECK_EVIDENCE), "file:.dir-locals.el", "--", str(check)],
                cwd=repo,
                env=evidence_environment(root / "receipts"),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_trivial_true_command_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo = init_repo(root / "project", ".dir-locals.el")
            result = run(
                [str(CHECK_EVIDENCE), "file:.dir-locals.el", "--", "true"],
                cwd=repo,
                env=evidence_environment(root / "receipts"),
            )
            self.assertEqual(result.returncode, 2)
            self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_source_change_during_project_check_emits_no_evidence(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo = init_repo(root / "project", ".dir-locals.el")
            check = repo / "check.sh"
            check.write_text(
                "#!/bin/sh\nprintf '%s\\n' '((nil . ((fill-column . 81))))' > .dir-locals.el\n"
            )
            check.chmod(0o755)
            subprocess.run(["git", "-C", str(repo), "add", "check.sh"], check=True)
            subprocess.run(["git", "-C", str(repo), "commit", "-qm", "add check"], check=True)
            result = run(
                [str(CHECK_EVIDENCE), "file:.dir-locals.el", "--", str(check)],
                cwd=repo,
                env=evidence_environment(root / "receipts"),
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertNotIn("ELISP_TEST_EVIDENCE_", result.stdout)

    def test_staged_check_runs_against_index_snapshot(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            repo = init_repo(root / "project", ".dir-locals.el")
            check = repo / "check.sh"
            check.write_text("#!/bin/sh\ngrep -q 'fill-column . 80' .dir-locals.el\n")
            check.chmod(0o755)
            subprocess.run(["git", "-C", str(repo), "add", "check.sh"], check=True)
            subprocess.run(["git", "-C", str(repo), "commit", "-qm", "add check"], check=True)
            (repo / ".dir-locals.el").write_text("((nil . ((fill-column . 80))))\n")
            subprocess.run(["git", "-C", str(repo), "add", ".dir-locals.el"], check=True)
            (repo / ".dir-locals.el").write_text("((nil . ((fill-column . 99))))\n")
            result = run(
                [
                    str(CHECK_EVIDENCE),
                    "--staged",
                    "file:.dir-locals.el",
                    "--",
                    str(check),
                ],
                cwd=repo,
                env=evidence_environment(root / "receipts"),
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn(
                run([str(REVISION_HELPER), "--index", str(repo)]).stdout.strip(),
                result.stdout,
            )


class TestEvidenceHookTests(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        self.root = Path(self.temp_dir.name)
        self.receipt_dir = self.root / "receipts"
        self.env = evidence_environment(self.receipt_dir)
        self.repo = init_repo(self.root / "example")
        (self.repo / "example.el").write_text("(provide 'changed)\n")
        subprocess.run(["git", "-C", str(self.repo), "add", "example.el"], check=True)

    def evidence(self, package: str = "example") -> str:
        revision = run([str(REVISION_HELPER), str(self.repo)]).stdout.strip()
        return issue_evidence("test", self.repo, package, revision, self.receipt_dir)

    def payload(self, tool: str, command: str, session: str, output: str = "", exit_code: int = 0):
        if tool == "codex":
            return {
                "tool_name": "exec_command",
                "session_id": session,
                "tool_input": {"cmd": command, "workdir": str(self.repo)},
                "tool_response": {"output": output, "exit_code": exit_code},
            }
        return {
            "session_id": session,
            "tool_input": {"command": command, "workdir": str(self.repo)},
            "tool_output": {"stdout": output, "exitCode": exit_code},
        }

    def run_hooks(self, tool: str, evidence: str, session: str):
        track = DOTFILES / f"{tool}/hooks/track-elisp-test.sh"
        require = DOTFILES / f"{tool}/hooks/require-elisp-test-before-commit.sh"
        marker = Path(f"/tmp/claude-elisp-tested-{session}")
        marker.unlink(missing_ok=True)
        self.addCleanup(marker.unlink, missing_ok=True)
        tracked = run(
            ["bash", str(track)],
            input=json.dumps(self.payload(tool, "batch-test.sh example", session, evidence)),
            cwd=self.repo,
            env=self.env,
        )
        required = run(
            ["bash", str(require)],
            input=json.dumps(self.payload(tool, "git commit -m fixture", session)),
            cwd=self.repo,
            env=self.env,
        )
        return tracked, required, marker

    def test_matching_repository_package_and_revision_allows_commit(self):
        for tool in ("claude", "codex"):
            with self.subTest(tool=tool):
                tracked, required, marker = self.run_hooks(
                    tool, self.evidence(), f"evidence-{tool}-{os.getpid()}"
                )
                self.assertEqual(tracked.returncode, 0, tracked.stderr)
                self.assertTrue(marker.exists())
                self.assertEqual(required.returncode, 0, required.stderr)
                self.assertEqual(required.stdout, "")

    def test_package_mismatch_blocks_commit(self):
        tracked, required, marker = self.run_hooks(
            "codex", self.evidence("other"), f"mismatch-{os.getpid()}"
        )
        self.assertEqual(tracked.returncode, 0, tracked.stderr)
        self.assertFalse(marker.exists())
        self.assertIn("does not match the wrapper command", tracked.stdout)
        self.assertIn("additionalContext", tracked.stdout)
        self.assertIn("permissionDecision", required.stdout)

    def test_source_change_after_test_blocks_commit(self):
        session = f"changed-{os.getpid()}"
        track = DOTFILES / "codex/hooks/track-elisp-test.sh"
        require = DOTFILES / "codex/hooks/require-elisp-test-before-commit.sh"
        marker = Path(f"/tmp/claude-elisp-tested-{session}")
        marker.unlink(missing_ok=True)
        self.addCleanup(marker.unlink, missing_ok=True)
        tracked = run(
            ["bash", str(track)],
            input=json.dumps(self.payload("codex", "batch-test.sh example", session, self.evidence())),
            cwd=self.repo,
            env=self.env,
        )
        self.assertEqual(tracked.returncode, 0, tracked.stderr)
        (self.repo / "example.el").write_text("(provide 'changed-again)\n")
        required = run(
            ["bash", str(require)],
            input=json.dumps(self.payload("codex", "git commit -m fixture", session)),
            cwd=self.repo,
            env=self.env,
        )
        self.assertIn("permissionDecision", required.stdout)

    def test_tested_worktree_cannot_authorize_different_staged_bytes(self):
        session = f"index-divergence-{os.getpid()}"
        (self.repo / "example.el").write_text("(provide 'working-tree-version)\n")
        tracked, required, _marker = self.run_hooks(
            "codex", self.evidence(), session
        )
        self.assertEqual(tracked.returncode, 0, tracked.stderr)
        self.assertIn("staged and working-tree versions differ", required.stdout)

    def test_combined_elisp_add_and_commit_is_blocked(self):
        session = f"compound-add-{os.getpid()}"
        require = DOTFILES / "codex/hooks/require-elisp-test-before-commit.sh"
        result = run(
            ["bash", str(require)],
            input=json.dumps(
                self.payload(
                    "codex",
                    "git add example.el && git commit -m fixture",
                    session,
                )
            ),
            cwd=self.repo,
        )
        self.assertIn("Stage Elisp source in a separate command", result.stdout)

    def test_failed_batch_command_creates_no_marker(self):
        session = f"failed-{os.getpid()}"
        track = DOTFILES / "codex/hooks/track-elisp-test.sh"
        marker = Path(f"/tmp/claude-elisp-tested-{session}")
        marker.unlink(missing_ok=True)
        result = run(
            ["bash", str(track)],
            input=json.dumps(
                self.payload("codex", "batch-test.sh example", session, self.evidence(), 1)
            ),
            cwd=self.repo,
            env=self.env,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(marker.exists())

    def test_file_labeled_project_evidence_allows_nonpackage_commit(self):
        repo = init_repo(self.root / "dotfiles", ".dir-locals.el")
        (repo / ".dir-locals.el").write_text("((nil . ((fill-column . 80))))\n")
        subprocess.run(["git", "-C", str(repo), "add", ".dir-locals.el"], check=True)
        revision = run([str(REVISION_HELPER), str(repo)]).stdout.strip()
        evidence = issue_evidence(
            "test", repo, "file:.dir-locals.el", revision, self.receipt_dir
        )
        session = f"nonpackage-{os.getpid()}"
        marker = Path(f"/tmp/claude-elisp-tested-{session}")
        marker.unlink(missing_ok=True)
        self.addCleanup(marker.unlink, missing_ok=True)
        payload = {
            "tool_name": "exec_command",
            "session_id": session,
            "tool_input": {"cmd": "elisp-check-evidence file:.dir-locals.el -- true", "workdir": str(repo)},
            "tool_response": {"output": evidence, "exit_code": 0},
        }
        tracked = run(
            ["bash", str(DOTFILES / "codex/hooks/track-elisp-test.sh")],
            input=json.dumps(payload),
            cwd=repo,
            env=self.env,
        )
        self.assertEqual(tracked.returncode, 0, tracked.stderr)
        payload["tool_input"]["cmd"] = "git commit -m fixture"
        payload["tool_response"]["output"] = ""
        required = run(
            ["bash", str(DOTFILES / "codex/hooks/require-elisp-test-before-commit.sh")],
            input=json.dumps(payload),
            cwd=repo,
            env=self.env,
        )
        self.assertEqual(required.stdout, "")

    def test_config_org_requires_its_exact_file_label(self):
        repo = init_repo(self.root / "config-dotfiles", "emacs/config.org")
        (repo / "emacs/config.org").write_text("#+title: changed\n")
        subprocess.run(["git", "-C", str(repo), "add", "emacs/config.org"], check=True)
        revision = run([str(REVISION_HELPER), str(repo)]).stdout.strip()
        evidence = issue_evidence(
            "test", repo, "some-package", revision, self.receipt_dir
        )
        session = f"config-label-{os.getpid()}"
        payload = {
            "tool_name": "exec_command",
            "session_id": session,
            "tool_input": {"cmd": "batch-test.sh some-package", "workdir": str(repo)},
            "tool_response": {"output": evidence, "exit_code": 0},
        }
        marker = Path(f"/tmp/claude-elisp-tested-{session}")
        marker.unlink(missing_ok=True)
        self.addCleanup(marker.unlink, missing_ok=True)
        run(
            ["bash", str(DOTFILES / "codex/hooks/track-elisp-test.sh")],
            input=json.dumps(payload),
            cwd=repo,
            env=self.env,
        )
        payload["tool_input"]["cmd"] = "git commit -m fixture"
        required = run(
            ["bash", str(DOTFILES / "codex/hooks/require-elisp-test-before-commit.sh")],
            input=json.dumps(payload),
            cwd=repo,
            env=self.env,
        )
        self.assertIn("file:emacs/config.org", required.stdout)

    def test_deleted_extra_requires_file_labeled_project_evidence(self):
        repo = init_repo(self.root / "deleted-extra", "emacs/extras/old-package.el")
        (repo / "emacs/extras/old-package.el").unlink()
        subprocess.run(
            ["git", "-C", str(repo), "add", "emacs/extras/old-package.el"],
            check=True,
        )
        session = f"deleted-extra-{os.getpid()}"
        payload = {
            "tool_name": "exec_command",
            "session_id": session,
            "tool_input": {"cmd": "git commit -m delete", "workdir": str(repo)},
            "tool_response": {"output": "", "exit_code": 0},
        }
        required = run(
            ["bash", str(DOTFILES / "codex/hooks/require-elisp-test-before-commit.sh")],
            input=json.dumps(payload),
            cwd=repo,
            env=self.env,
        )
        self.assertIn("elisp-check-evidence", required.stdout)
        self.assertIn("file:emacs/extras/old-package.el", required.stdout)
        self.assertNotIn("batch-test.sh\" old-package", required.stdout)

    def test_renamed_extra_requires_old_file_and_new_package_evidence(self):
        repo = init_repo(self.root / "renamed-extra", "emacs/extras/old-package.el")
        subprocess.run(
            [
                "git",
                "-C",
                str(repo),
                "mv",
                "emacs/extras/old-package.el",
                "emacs/extras/new-package.el",
            ],
            check=True,
        )
        revision = run([str(REVISION_HELPER), str(repo)]).stdout.strip()
        session = f"renamed-extra-{os.getpid()}"
        marker = Path(f"/tmp/claude-elisp-tested-{session}")
        marker.unlink(missing_ok=True)
        self.addCleanup(marker.unlink, missing_ok=True)

        def payload(command: str, output: str = "") -> dict[str, object]:
            return {
                "tool_name": "exec_command",
                "session_id": session,
                "tool_input": {"cmd": command, "workdir": str(repo)},
                "tool_response": {"output": output, "exit_code": 0},
            }

        old_evidence = issue_evidence(
            "test",
            repo,
            "file:emacs/extras/old-package.el",
            revision,
            self.receipt_dir,
        )
        tracked = run(
            ["bash", str(DOTFILES / "codex/hooks/track-elisp-test.sh")],
            input=json.dumps(
                payload(
                    "elisp-check-evidence file:emacs/extras/old-package.el -- check",
                    old_evidence,
                )
            ),
            cwd=repo,
            env=self.env,
        )
        self.assertEqual(tracked.returncode, 0, tracked.stderr)
        required = run(
            ["bash", str(DOTFILES / "codex/hooks/require-elisp-test-before-commit.sh")],
            input=json.dumps(payload("git commit -m rename")),
            cwd=repo,
            env=self.env,
        )
        self.assertIn('batch-test.sh\\\" new-package', required.stdout)

        new_evidence = issue_evidence(
            "test", repo, "new-package", revision, self.receipt_dir
        )
        tracked = run(
            ["bash", str(DOTFILES / "codex/hooks/track-elisp-test.sh")],
            input=json.dumps(payload("batch-test.sh new-package", new_evidence)),
            cwd=repo,
            env=self.env,
        )
        self.assertEqual(tracked.returncode, 0, tracked.stderr)
        required = run(
            ["bash", str(DOTFILES / "codex/hooks/require-elisp-test-before-commit.sh")],
            input=json.dumps(payload("git commit -m rename")),
            cwd=repo,
            env=self.env,
        )
        self.assertEqual(required.stdout, "")

    def test_staged_file_evidence_allows_partial_file_commit(self):
        repo = init_repo(self.root / "partial-file", ".dir-locals.el")
        check = repo / "check.sh"
        check.write_text("#!/bin/sh\ngrep -q 'fill-column . 80' .dir-locals.el\n")
        check.chmod(0o755)
        subprocess.run(["git", "-C", str(repo), "add", "check.sh"], check=True)
        subprocess.run(["git", "-C", str(repo), "commit", "-qm", "add check"], check=True)
        (repo / ".dir-locals.el").write_text("((nil . ((fill-column . 80))))\n")
        subprocess.run(["git", "-C", str(repo), "add", ".dir-locals.el"], check=True)
        (repo / ".dir-locals.el").write_text("((nil . ((fill-column . 99))))\n")
        checked = run(
            [
                str(CHECK_EVIDENCE), "--staged", "file:.dir-locals.el", "--", str(check)
            ],
            cwd=repo,
            env=self.env,
        )
        self.assertEqual(checked.returncode, 0, checked.stderr)
        session = f"partial-file-{os.getpid()}"
        marker = Path(f"/tmp/claude-elisp-tested-{session}")
        marker.unlink(missing_ok=True)
        self.addCleanup(marker.unlink, missing_ok=True)
        payload = {
            "tool_name": "exec_command",
            "session_id": session,
            "tool_input": {"cmd": "elisp-check-evidence --staged file:.dir-locals.el -- ./check.sh", "workdir": str(repo)},
            "tool_response": {"output": checked.stdout, "exit_code": 0},
        }
        run(
            ["bash", str(DOTFILES / "codex/hooks/track-elisp-test.sh")],
            input=json.dumps(payload), cwd=repo, env=self.env,
        )
        payload["tool_input"]["cmd"] = "git commit -m fixture"
        required = run(
            ["bash", str(DOTFILES / "codex/hooks/require-elisp-test-before-commit.sh")],
            input=json.dumps(payload), cwd=repo, env=self.env,
        )
        self.assertEqual(required.stdout, "")

    def test_forged_evidence_without_receipt_creates_no_marker(self):
        revision = run([str(REVISION_HELPER), str(self.repo)]).stdout.strip()
        encoded_repo = base64.b64encode(str(self.repo).encode()).decode()
        encoded_package = base64.b64encode(b"example").decode()
        evidence = (
            f"ELISP_TEST_EVIDENCE_V2:{encoded_repo}:{encoded_package}:"
            f"{revision}:receipt.forged"
        )
        session = f"forged-{os.getpid()}"
        marker = Path(f"/tmp/claude-elisp-tested-{session}")
        marker.unlink(missing_ok=True)
        self.addCleanup(marker.unlink, missing_ok=True)
        payload = self.payload(
            "codex", "printf forged # batch-test.sh", session, evidence
        )
        tracked = run(
            ["bash", str(DOTFILES / "codex/hooks/track-elisp-test.sh")],
            input=json.dumps(payload),
            cwd=self.repo,
            env=self.env,
        )
        self.assertEqual(tracked.returncode, 0, tracked.stderr)
        self.assertFalse(marker.exists())
        self.assertEqual(tracked.stderr, "")

    def test_valid_receipt_with_textual_wrapper_mention_creates_no_marker(self):
        for tool in ("claude", "codex"):
            with self.subTest(tool=tool):
                session = f"{tool}-textual-wrapper-{os.getpid()}"
                marker = Path(f"/tmp/claude-elisp-tested-{session}")
                marker.unlink(missing_ok=True)
                self.addCleanup(marker.unlink, missing_ok=True)
                payload = self.payload(
                    tool,
                    "printf '%s' 'batch-test.sh example'",
                    session,
                    self.evidence(),
                )
                tracked = run(
                    ["bash", str(DOTFILES / f"{tool}/hooks/track-elisp-test.sh")],
                    input=json.dumps(payload),
                    cwd=self.repo,
                    env=self.env,
                )
                self.assertEqual(tracked.returncode, 0, tracked.stderr)
                self.assertFalse(marker.exists())

    def test_multiple_test_wrappers_cannot_claim_one_receipt(self):
        for tool in ("claude", "codex"):
            with self.subTest(tool=tool):
                session = f"{tool}-multiple-wrappers-{os.getpid()}"
                marker = Path(f"/tmp/claude-elisp-tested-{session}")
                marker.unlink(missing_ok=True)
                self.addCleanup(marker.unlink, missing_ok=True)
                payload = self.payload(
                    tool,
                    "batch-test.sh example; batch-test.sh other",
                    session,
                    self.evidence(),
                )
                tracked = run(
                    ["bash", str(DOTFILES / f"{tool}/hooks/track-elisp-test.sh")],
                    input=json.dumps(payload),
                    cwd=self.repo,
                    env=self.env,
                )
                self.assertEqual(tracked.returncode, 0, tracked.stderr)
                self.assertFalse(marker.exists())

    def test_wrapper_plus_output_command_cannot_claim_one_receipt(self):
        for tool in ("claude", "codex"):
            with self.subTest(tool=tool):
                session = f"{tool}-wrapper-output-{os.getpid()}"
                marker = Path(f"/tmp/claude-elisp-tested-{session}")
                marker.unlink(missing_ok=True)
                self.addCleanup(marker.unlink, missing_ok=True)
                payload = self.payload(
                    tool,
                    "batch-test.sh example; printf forged",
                    session,
                    self.evidence(),
                )
                tracked = run(
                    ["bash", str(DOTFILES / f"{tool}/hooks/track-elisp-test.sh")],
                    input=json.dumps(payload),
                    cwd=self.repo,
                    env=self.env,
                )
                self.assertEqual(tracked.returncode, 0, tracked.stderr)
                self.assertFalse(marker.exists())

    def test_evidence_receipt_cannot_be_replayed(self):
        evidence = self.evidence()
        first, _required, first_marker = self.run_hooks(
            "codex", evidence, f"receipt-first-{os.getpid()}"
        )
        self.assertEqual(first.returncode, 0, first.stderr)
        self.assertTrue(first_marker.exists())
        replay, _required, replay_marker = self.run_hooks(
            "codex", evidence, f"receipt-replay-{os.getpid()}"
        )
        self.assertEqual(replay.returncode, 0, replay.stderr)
        self.assertFalse(replay_marker.exists())

    def test_abandoned_receipts_older_than_one_day_are_removed(self):
        self.receipt_dir.mkdir(mode=0o700)
        stale_receipt = self.receipt_dir / "receipt.abandoned"
        stale_receipt.write_text("abandoned\n")
        os.utime(stale_receipt, (1, 1))
        self.evidence()
        self.assertFalse(stale_receipt.exists())


class ElpacaRebuildWaitTests(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        self.root = Path(self.temp_dir.name)
        self.dotfiles = init_repo(self.root / "dotfiles", "emacs/extras/example.el")
        self.home = self.root / "home"
        mirror_parent = self.home / ".config/emacs-profiles/test/elpaca/sources"
        mirror_parent.mkdir(parents=True)
        self.mirror = mirror_parent / "dotfiles"
        subprocess.run(
            ["git", "clone", "-q", str(self.dotfiles), str(self.mirror)], check=True
        )
        profile_cache = self.home / ".config/emacs-profiles/.current-profile"
        profile_cache.parent.mkdir(parents=True, exist_ok=True)
        profile_cache.write_text("test\n")
        self.state = self.root / "state"
        self.fake_bin = self.root / "bin"
        self.fake_bin.mkdir()
        self.called = self.root / "emacsclient-called"

    def environment(self) -> dict[str, str]:
        env = os.environ.copy()
        env["HOME"] = str(self.home)
        env["DOTFILES_ROOT"] = str(self.dotfiles)
        env["ELPACA_RELOAD_STATE_DIR"] = str(self.state)
        env["ELPACA_RELOAD_TIMEOUT_SECONDS"] = "3"
        env["ELPACA_RELOAD_POLL_INTERVAL_SECONDS"] = "0"
        env["EMACSCLIENT_CALLED"] = str(self.called)
        env["FAKE_PACKAGE_ID"] = "example"
        env["FAKE_PACKAGE_SOURCE"] = str(self.dotfiles)
        env["FAKE_RUNTIME_SOURCE"] = str(self.mirror)
        env["PATH"] = f"{self.fake_bin}:{env['PATH']}"
        return env

    def write_emacsclient(self):
        write_rebuild_emacsclient(self.fake_bin / "emacsclient")

    def status_path(self):
        commit = run(["git", "-C", str(self.dotfiles), "rev-parse", "HEAD"]).stdout.strip()
        return self.state / commit / "example.status"

    def rebuild(self, **changes):
        self.write_emacsclient()
        env = self.environment()
        env.update(changes)
        return run([str(REBUILD_WAIT), "example"], env=env, cwd=self.dotfiles)

    def requests(self):
        if not self.called.exists():
            return 0
        return self.called.read_text().count("elpaca-extras-rebuild-and-reload")

    def test_legacy_finished_is_not_runtime_evidence(self):
        status = self.status_path()
        status.parent.mkdir(parents=True)
        status.write_text("finished:private old message\n")
        result = self.rebuild()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("ELPACA_RELOAD_OWNER=1", result.stderr)
        self.assertNotIn("private old", result.stderr)
        self.assertEqual(self.requests(), 0)
        self.assertEqual(status.read_text(), "finished:private old message\n")

    def test_pending_producer_startup_gap_is_wait_only(self):
        status = self.status_path()
        status.parent.mkdir(parents=True)
        status.write_text("pending:queued by post-commit\n")
        result = self.rebuild(ELPACA_RELOAD_TIMEOUT_SECONDS="0.1")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("existing owner", result.stderr)
        self.assertEqual(self.requests(), 0)
        self.assertEqual(status.read_text(), "pending:queued by post-commit\n")

    def test_pending_never_reuses_previous_finished_receipt(self):
        self.assertEqual(self.rebuild(ELPACA_RELOAD_OWNER="1").returncode, 0)
        self.status_path().write_text("pending:queued by post-commit\n")
        result = self.rebuild(ELPACA_RELOAD_TIMEOUT_SECONDS="0.1")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.requests(), 1)

    def test_runtime_restart_or_profile_change_invalidates_cached_success(self):
        self.assertEqual(self.rebuild(ELPACA_RELOAD_OWNER="1").returncode, 0)
        other_profile = self.root / "other-profile"
        other_profile.mkdir()
        for changes in ({"FAKE_PID": "456"}, {"FAKE_START": "new-start"},
                        {"FAKE_PROFILE": str(other_profile)}):
            with self.subTest(changes=changes):
                result = self.rebuild(**changes)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("unverified completion", result.stderr)
        self.assertEqual(self.requests(), 1)

    def test_actual_registry_mirror_not_cached_profile_is_checked(self):
        cache = self.home / ".config/emacs-profiles/.current-profile"
        cache.write_text("nonexistent-stale-profile\n")
        result = self.rebuild(ELPACA_RELOAD_OWNER="1")
        self.assertEqual(result.returncode, 0, result.stderr)
        receipt = json.loads(self.status_path().with_suffix(".status.receipt.json").read_text())
        self.assertEqual(receipt["context"]["runtime"]["source"], str(self.mirror.resolve()))

    def test_dirty_same_head_mirror_and_target_source_refused(self):
        for target in (self.mirror / "emacs/extras/example.el", self.dotfiles / "emacs/extras/example.el"):
            with self.subTest(target=target):
                original = target.read_bytes()
                target.write_text("dirty source\n")
                result = self.rebuild(ELPACA_RELOAD_OWNER="1")
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("uncommitted", result.stderr)
                self.assertEqual(self.requests(), 0)
                target.write_bytes(original)

    def test_unrelated_primary_changes_do_not_block_package_rebuild(self):
        (self.dotfiles / "unrelated.md").write_text("unrelated staged user work\n")
        subprocess.run(["git", "-C", str(self.dotfiles), "add", "unrelated.md"], check=True)
        (self.dotfiles / "another.md").write_text("unrelated untracked user work\n")
        result = self.rebuild(ELPACA_RELOAD_OWNER="1")
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_runtime_or_source_change_during_request_cannot_finish(self):
        runtime_file = self.root / "runtime.json"
        result = self.rebuild(ELPACA_RELOAD_OWNER="1", FAKE_RUNTIME_FILE=str(runtime_file),
                              FAKE_AFTER_REQUEST=json.dumps({"pid": 456}))
        self.assertNotEqual(result.returncode, 0)
        self.assertNotEqual(self.status_path().read_text().split(":")[0], "finished")
        self.assertFalse(self.status_path().with_suffix(".status.receipt.json").exists())

    def test_source_changes_during_build_cannot_finish(self):
        target = self.mirror / "emacs/extras/example.el"
        original = target.read_bytes()
        result = self.rebuild(ELPACA_RELOAD_OWNER="1",
                              FAKE_DIRTY_AFTER_REQUEST=str(target))
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("uncommitted", result.stderr)
        self.assertFalse(self.status_path().with_suffix(".status.receipt.json").exists())
        self.assertEqual(self.status_path().read_text().split(":")[0], "failed")
        target.write_bytes(original)
        retried = self.rebuild(ELPACA_RELOAD_OWNER="1")
        self.assertEqual(retried.returncode, 0, retried.stderr)
        self.assertEqual(self.requests(), 2, "rejected token must not be reused after restoring source")

    def test_owner_preflight_failure_terminates_producer_pending_status(self):
        status = self.status_path()
        status.parent.mkdir(parents=True)
        status.write_text("pending:queued by post-commit\n")
        (self.mirror / "emacs/extras/example.el").write_text("dirty mirror\n")
        result = self.rebuild(ELPACA_RELOAD_OWNER="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(status.read_text(), "failed:owner preflight failed; no rebuild requested\n")
        self.assertEqual(self.requests(), 0)

    def test_new_producer_pending_is_not_overwritten_by_prior_completion(self):
        result = self.rebuild(ELPACA_RELOAD_OWNER="1",
                              FAKE_STATUS_REPLACE_AFTER_REQUEST=str(self.status_path()))
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("status replaced", result.stderr)
        self.assertEqual(self.status_path().read_text(), "pending:queued by post-commit\n")
        self.assertFalse(self.status_path().with_suffix(".status.receipt.json").exists())

    def test_old_commit_producer_cannot_label_current_source_rebuild(self):
        status = self.state / ("a" * 40) / "example.status"
        status.parent.mkdir(parents=True)
        status.write_text("pending:queued by post-commit\n")
        result = self.rebuild(ELPACA_RELOAD_OWNER="1", ELPACA_RELOAD_STATUS_FILE=str(status))
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("status revision", result.stderr)
        self.assertEqual(self.requests(), 0)
        self.assertEqual(status.read_text(), "failed:owner preflight failed; no rebuild requested\n")

    def test_wrong_package_or_missing_token_never_certifies_completion(self):
        self.assertEqual(self.rebuild(ELPACA_RELOAD_OWNER="1").returncode, 0)
        for changes in ({"FAKE_TOKEN_PACKAGE": "other"}, {"FAKE_TOKEN_STATE": "missing"}):
            with self.subTest(changes=changes):
                result = self.rebuild(**changes)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("another package", result.stderr)
        self.assertEqual(self.requests(), 1)

    def test_malformed_token_is_not_evaluated_and_request_remains_uncertain(self):
        result = self.rebuild(ELPACA_RELOAD_OWNER="1", FAKE_TOKEN='bad-token\") (error "sentinel")')
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("sentinel", result.stderr)
        self.assertNotIn("elpaca-extras-build-reload-status", self.called.read_text())
        second = self.rebuild(ELPACA_RELOAD_OWNER="1")
        self.assertNotEqual(second.returncode, 0)
        self.assertIn("unresolved rebuild", second.stderr)
        self.assertEqual(self.requests(), 1)

    def test_request_transport_error_is_sanitized_and_blocks_duplicate(self):
        result = self.rebuild(ELPACA_RELOAD_OWNER="1", FAKE_REQUEST_ERROR="private-sentinel")
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("private-sentinel", result.stdout + result.stderr)
        second = self.rebuild(ELPACA_RELOAD_OWNER="1")
        self.assertNotEqual(second.returncode, 0)
        self.assertEqual(self.requests(), 1)

    def test_timeout_owner_can_resume_same_token_without_new_request(self):
        first = self.rebuild(ELPACA_RELOAD_OWNER="1", FAKE_TOKEN_STATE="queued",
                             ELPACA_RELOAD_TIMEOUT_SECONDS="0.3")
        self.assertNotEqual(first.returncode, 0)
        second = self.rebuild(ELPACA_RELOAD_OWNER="1")
        self.assertEqual(second.returncode, 0, second.stderr)
        self.assertEqual(self.requests(), 1)

    def test_observer_certifies_completed_token_after_owner_timeout(self):
        first = self.rebuild(ELPACA_RELOAD_OWNER="1", FAKE_TOKEN_STATE="queued",
                             ELPACA_RELOAD_TIMEOUT_SECONDS="0.3")
        self.assertNotEqual(first.returncode, 0)
        operation_file = self.state / "owners/example.json"
        before = json.loads(operation_file.read_text())
        second = self.rebuild(ELPACA_RELOAD_TIMEOUT_SECONDS="0.5")
        self.assertEqual(second.returncode, 0, second.stderr)
        after = json.loads(operation_file.read_text())
        self.assertEqual(after["token"], before["token"])
        self.assertEqual(after["context"], before["context"])
        self.assertEqual(after["state"], "finished")
        self.assertEqual(self.status_path().read_text(), "finished:loaded\n")
        self.assertEqual(self.requests(), 1)

    def test_observer_does_not_certify_pending_token_for_changed_runtime(self):
        first = self.rebuild(ELPACA_RELOAD_OWNER="1", FAKE_TOKEN_STATE="queued",
                             ELPACA_RELOAD_TIMEOUT_SECONDS="0.3")
        self.assertNotEqual(first.returncode, 0)
        operation_file = self.state / "owners/example.json"
        before = operation_file.read_bytes()
        runtime_file = self.root / "changed-runtime.json"
        runtime = json.loads(before)["context"]["runtime"]
        runtime["pid"] += 1
        runtime_file.write_text(json.dumps(runtime))
        result = self.rebuild(FAKE_RUNTIME_FILE=str(runtime_file),
                              ELPACA_RELOAD_TIMEOUT_SECONDS="0.5")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(operation_file.read_bytes(), before)
        self.assertEqual(self.requests(), 1)

    def test_observer_does_not_take_over_live_owner_lock(self):
        first = self.rebuild(ELPACA_RELOAD_OWNER="1", FAKE_TOKEN_STATE="queued",
                             ELPACA_RELOAD_TIMEOUT_SECONDS="0.3")
        self.assertNotEqual(first.returncode, 0)
        loaded = runpy.run_path(str(REBUILD_WAIT), run_name="fixture_rebuild")
        operation_file = self.state / "owners/example.json"
        before = operation_file.read_bytes()
        with loaded["owner_lock"](self.state, "example"):
            result = self.rebuild(ELPACA_RELOAD_TIMEOUT_SECONDS="0.2")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(operation_file.read_bytes(), before)
        self.assertEqual(self.requests(), 1)

    def test_observer_refuses_replaced_pending_status(self):
        first = self.rebuild(ELPACA_RELOAD_OWNER="1", FAKE_TOKEN_STATE="queued",
                             ELPACA_RELOAD_TIMEOUT_SECONDS="0.3")
        self.assertNotEqual(first.returncode, 0)
        operation_file = self.state / "owners/example.json"
        before = operation_file.read_bytes()
        status = self.status_path()
        replacement = status.with_name("replacement.status")
        replacement.write_bytes(status.read_bytes())
        replacement.replace(status)
        result = self.rebuild(ELPACA_RELOAD_TIMEOUT_SECONDS="0.5")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("unresolved rebuild", result.stderr)
        self.assertEqual(operation_file.read_bytes(), before)
        self.assertEqual(self.requests(), 1)

    def test_observer_records_failure_of_abandoned_token(self):
        first = self.rebuild(ELPACA_RELOAD_OWNER="1", FAKE_TOKEN_STATE="queued",
                             ELPACA_RELOAD_TIMEOUT_SECONDS="0.3")
        self.assertNotEqual(first.returncode, 0)
        result = self.rebuild(FAKE_TOKEN_STATE="failed")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.status_path().read_text(), "failed:build or reload failed\n")
        self.assertEqual(self.requests(), 1)

    def test_concurrent_owner_is_refused_before_another_request(self):
        self.write_emacsclient()
        env = self.environment()
        marker = self.root / "request-started"
        env.update(ELPACA_RELOAD_OWNER="1", FAKE_TOKEN_STATE="queued",
                   ELPACA_RELOAD_TIMEOUT_SECONDS="2", FAKE_REQUEST_MARKER=str(marker))
        process = subprocess.Popen([str(REBUILD_WAIT), "example"], env=env,
                                   cwd=self.dotfiles, text=True, stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE)
        try:
            deadline = time.monotonic() + 3
            while not marker.exists() and time.monotonic() < deadline:
                time.sleep(0.01)
            self.assertTrue(marker.exists())
            second = self.rebuild(ELPACA_RELOAD_OWNER="1")
            self.assertNotEqual(second.returncode, 0)
            self.assertIn("another rebuild owner", second.stderr)
        finally:
            process.communicate(timeout=5)
        self.assertEqual(self.requests(), 1)

    def test_nonowner_rechecks_missing_status_after_acquiring_owner_lock(self):
        self.write_emacsclient()
        loaded = runpy.run_path(str(REBUILD_WAIT), run_name="fixture_rebuild")
        namespace = loaded["main"].__globals__
        original_lock = namespace["owner_lock"]
        @contextmanager
        def interleaving(root, package):
            completed = self.rebuild(ELPACA_RELOAD_OWNER="1")
            self.assertEqual(completed.returncode, 0, completed.stderr)
            with original_lock(root, package) as directory:
                yield directory
        with mock.patch.dict(namespace, owner_lock=interleaving), \
                mock.patch.dict(os.environ, self.environment(), clear=True), \
                mock.patch.object(loaded["sys"], "argv", [str(REBUILD_WAIT), "example"]):
            with self.assertRaisesRegex(loaded["RebuildError"], "another owner published status"):
                loaded["main"]()
        self.assertEqual(self.requests(), 1)

    def test_large_poll_interval_cannot_exceed_observer_or_owner_deadline(self):
        status = self.status_path()
        status.parent.mkdir(parents=True)
        status.write_text("pending:queued by post-commit\n")
        for owner in ("0", "1"):
            with self.subTest(owner=owner):
                start = time.monotonic()
                result = self.rebuild(ELPACA_RELOAD_OWNER=owner, FAKE_TOKEN_STATE="queued",
                                      ELPACA_RELOAD_TIMEOUT_SECONDS="0.4",
                                      ELPACA_RELOAD_POLL_INTERVAL_SECONDS="10000")
                self.assertNotEqual(result.returncode, 0)
                self.assertLess(time.monotonic() - start, 3)

    def test_malformed_owner_state_cannot_start_another_request(self):
        self.assertEqual(self.rebuild(ELPACA_RELOAD_OWNER="1").returncode, 0)
        operation = self.state / "owners/example.json"
        value = json.loads(operation.read_text())
        value["state"] = "unknown"
        operation.write_text(json.dumps(value))
        result = self.rebuild(ELPACA_RELOAD_OWNER="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("invalid owner record", result.stderr)
        self.assertEqual(self.requests(), 1)

    def test_invalid_timeout_values_refuse_before_runtime_calls(self):
        for value in ("NaN", "Inf", "-1"):
            with self.subTest(value=value):
                result = self.rebuild(ELPACA_RELOAD_TIMEOUT_SECONDS=value)
                self.assertNotEqual(result.returncode, 0)
                self.assertFalse(self.called.exists())

    def test_atomic_receipt_replace_failure_preserves_prior_json(self):
        loaded = runpy.run_path(str(REBUILD_WAIT), run_name="fixture_rebuild")
        target = self.root / "receipt.json"
        target.write_text('{"old": true}\n')
        with mock.patch.object(loaded["os"], "replace", side_effect=OSError("fixture")):
            with self.assertRaises(OSError):
                loaded["write_json"](target, {"new": True})
        self.assertEqual(json.loads(target.read_text()), {"old": True})
        self.assertEqual(list(self.root.glob(".reload-*")), [])

    def test_observes_finished_post_commit_state_without_new_request(self):
        commit = run(["git", "-C", str(self.dotfiles), "rev-parse", "HEAD"]).stdout.strip()
        status = self.state / commit / "example.status"
        self.write_emacsclient()
        owner_env = self.environment()
        owner_env["ELPACA_RELOAD_OWNER"] = "1"
        first = run([str(REBUILD_WAIT), "example"], env=owner_env, cwd=self.dotfiles)
        self.assertEqual(first.returncode, 0, first.stderr)
        self.called.write_text("")
        result = run(
            [str(REBUILD_WAIT), "example"], env=self.environment(), cwd=self.dotfiles
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn("elpaca-extras-rebuild-and-reload", self.called.read_text())
        self.assertIn("elpaca-extras-build-reload-status", self.called.read_text())

    def test_owner_request_waits_for_finished_and_persists_status(self):
        self.write_emacsclient()
        status = self.root / "explicit.status"
        env = self.environment()
        env["ELPACA_RELOAD_OWNER"] = "1"
        env["ELPACA_RELOAD_STATUS_FILE"] = str(status)
        result = run([str(REBUILD_WAIT), "example"], env=env, cwd=self.dotfiles)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(status.read_text(), "finished:loaded\n")
        self.assertTrue(self.called.exists())

    def test_standalone_package_never_reuses_dotfiles_commit_state(self):
        self.write_emacsclient()
        standalone = init_repo(self.root / "standalone", "standalone.el")
        commit = run(["git", "-C", str(standalone), "rev-parse", "HEAD"]).stdout.strip()
        status = self.state / commit / "standalone.status"
        status.parent.mkdir(parents=True)
        status.write_text("finished:stale\n")
        env = self.environment()
        env["FAKE_PACKAGE_ID"] = "standalone"
        env["FAKE_PACKAGE_SOURCE"] = str(standalone)
        env["FAKE_RUNTIME_SOURCE"] = str(standalone)
        result = run(
            [str(REBUILD_WAIT), "standalone"],
            env=env,
            cwd=standalone,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(self.called.exists())
        self.assertEqual(status.read_text(), "finished:loaded\n")

    def test_repository_label_rebuilds_resolved_package_id(self):
        self.write_emacsclient()
        standalone = init_repo(self.root / "emacs-slack", "slack.el")
        log = self.root / "emacsclient.log"
        env = self.environment()
        env["FAKE_PACKAGE_ID"] = "slack"
        env["FAKE_PACKAGE_SOURCE"] = str(standalone)
        env["FAKE_RUNTIME_SOURCE"] = str(standalone)
        env["FAKE_EMACSCLIENT_LOG"] = str(log)
        result = run(
            [str(REBUILD_WAIT), "emacs-slack"], env=env, cwd=standalone
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        calls = log.read_text() if log.exists() else ""
        self.assertIn("elpaca-extras-rebuild-and-reload 'slack", calls)
        self.assertNotIn("elpaca-extras-rebuild-and-reload 'emacs-slack", calls)

    def test_rejects_finished_state_for_a_different_mirror_head(self):
        (self.dotfiles / "emacs/extras/example.el").write_text("(provide 'changed)\n")
        subprocess.run(
            ["git", "-C", str(self.dotfiles), "add", "emacs/extras/example.el"],
            check=True,
        )
        subprocess.run(
            ["git", "-C", str(self.dotfiles), "commit", "-qm", "change"], check=True
        )
        commit = run(["git", "-C", str(self.dotfiles), "rev-parse", "HEAD"]).stdout.strip()
        status = self.state / commit / "example.status"
        status.parent.mkdir(parents=True)
        status.write_text("finished:stale mirror\n")
        self.write_emacsclient()
        result = run(
            [str(REBUILD_WAIT), "example"], env=self.environment(), cwd=self.dotfiles
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("does not match source HEAD", result.stderr)
        self.assertNotIn("elpaca-extras-rebuild-and-reload", self.called.read_text())


class ElispLiveVerifyTests(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        self.root = Path(self.temp_dir.name)
        self.repo = init_repo(self.root / "example", "example.el")
        self.fake_bin = self.root / "bin"
        self.fake_bin.mkdir()
        emacsclient = self.fake_bin / "emacsclient"
        write_rebuild_emacsclient(emacsclient)

    def environment(self) -> dict[str, str]:
        env = os.environ.copy()
        env["PATH"] = f"{self.fake_bin}:{env['PATH']}"
        env["ELPACA_RELOAD_STATE_DIR"] = str(self.root / "state")
        env["ELPACA_RELOAD_POLL_INTERVAL_SECONDS"] = "0"
        env["ELISP_EVIDENCE_RECEIPT_DIR"] = str(self.root / "receipts")
        env["FAKE_PACKAGE_ID"] = "example"
        env["FAKE_PACKAGE_SOURCE"] = str(self.repo)
        return env

    def test_emits_repository_label_and_commit_bound_evidence(self):
        result = run(
            [str(LIVE_VERIFY), "example", "--", "(example-status)"],
            cwd=self.repo,
            env=self.environment(),
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertRegex(
            result.stdout,
            r"(?m)^ELISP_LIVE_EVIDENCE_V2:[^:]+:[^:]+:[0-9a-f]{40,64}:receipt\.[A-Za-z0-9]+$",
        )

    def test_normal_package_resolves_from_a_nonrepository_working_directory(self):
        result = run([str(LIVE_VERIFY), "example", "--", "(example-status)"],
                     cwd=self.root, env=self.environment())
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("ELISP_LIVE_EVIDENCE_V2:", result.stdout)

    def test_source_edit_during_live_expression_emits_no_commit_evidence(self):
        env = self.environment()
        env["FAKE_LIVE_EDIT_SOURCE"] = str(self.repo / "example.el")
        result = run([str(LIVE_VERIFY), "example", "--", "(example-status)"],
                     cwd=self.repo, env=env)
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("ELISP_LIVE_EVIDENCE_", result.stdout)

    def test_commit_change_during_live_expression_emits_no_stale_evidence(self):
        env = self.environment()
        env["FAKE_LIVE_COMMIT_REPO"] = str(self.repo)
        result = run([str(LIVE_VERIFY), "example", "--", "(example-status)"],
                     cwd=self.repo, env=env)
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("ELISP_LIVE_EVIDENCE_", result.stdout)

    def test_new_untracked_source_during_live_expression_is_not_committed_evidence(self):
        env = self.environment()
        env["FAKE_LIVE_EDIT_SOURCE"] = str(self.repo / "new-library.el")
        result = run([str(LIVE_VERIFY), "example", "--", "(example-status)"],
                     cwd=self.repo, env=env)
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("ELISP_LIVE_EVIDENCE_", result.stdout)

    def test_unrelated_non_source_edit_during_live_expression_is_preserved(self):
        env = self.environment()
        unrelated = self.repo / "unrelated.md"
        env["FAKE_LIVE_EDIT_SOURCE"] = str(unrelated)
        result = run([str(LIVE_VERIFY), "example", "--", "(example-status)"],
                     cwd=self.repo, env=env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(unrelated.exists())
        self.assertIn("ELISP_LIVE_EVIDENCE_", result.stdout)

    def test_repository_label_uses_package_id_but_remains_evidence_label(self):
        repo = init_repo(self.root / "emacs-slack", "slack.el")
        log = self.root / "emacsclient.log"
        env = self.environment()
        env["FAKE_PACKAGE_ID"] = "slack"
        env["FAKE_PACKAGE_SOURCE"] = str(repo)
        env["FAKE_EMACSCLIENT_LOG"] = str(log)
        result = run(
            [str(LIVE_VERIFY), "emacs-slack", "--", "(featurep 'slack)"],
            cwd=DOTFILES,
            env=env,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        evidence = next(
            line for line in result.stdout.splitlines()
            if line.startswith("ELISP_LIVE_EVIDENCE_V2:")
        )
        fields = evidence.split(":")
        self.assertEqual(base64.b64decode(fields[1]).decode(), str(repo.resolve()))
        self.assertEqual(base64.b64decode(fields[2]).decode(), "emacs-slack")
        calls = log.read_text()
        self.assertIn("elpaca-extras-rebuild-and-reload 'slack", calls)
        self.assertNotIn("elpaca-extras-rebuild-and-reload 'emacs-slack", calls)

    def test_unrelated_expression_is_rejected(self):
        result = run(
            [str(LIVE_VERIFY), "example", "--", '(message "fixture")'],
            cwd=self.repo,
            env=self.environment(),
        )
        self.assertEqual(result.returncode, 2)
        self.assertNotIn("ELISP_LIVE_EVIDENCE_", result.stdout)

    def test_dirty_package_source_is_rejected(self):
        (self.repo / "example.el").write_text("(provide 'dirty)\n")
        result = run(
            [str(LIVE_VERIFY), "example", "--", "(example-status)"],
            cwd=self.repo,
            env=self.environment(),
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("ELISP_LIVE_EVIDENCE_", result.stdout)

    def test_nil_live_result_emits_no_evidence(self):
        env = self.environment()
        env["FAKE_LIVE_RESULT"] = "nil"
        result = run(
            [str(LIVE_VERIFY), "example", "--", "(example-status)"],
            cwd=self.repo,
            env=env,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("ELISP_LIVE_EVIDENCE_", result.stdout)

    def test_deleted_package_mode_unloads_and_verifies_absence(self):
        repo = init_repo(self.root / "deleted", "lisp/old-package.el")
        (repo / "lisp/old-package.el").unlink()
        subprocess.run(
            ["git", "-C", str(repo), "add", "lisp/old-package.el"], check=True
        )
        subprocess.run(
            ["git", "-C", str(repo), "commit", "-qm", "delete old package"],
            check=True,
        )
        log = self.root / "emacsclient.log"
        env = self.environment()
        env["FAKE_EMACSCLIENT_LOG"] = str(log)
        result = run(
            [
                str(LIVE_VERIFY),
                "deleted:old-package",
                "--",
                "(not (featurep 'old-package))",
            ],
            cwd=repo,
            env=env,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        calls = [call.decode() for call in log.read_bytes().split(b"\x1c") if call]
        cleanup_call = next(call for call in calls if "unload-feature id" in call)
        cleanup_expression = cleanup_call.removeprefix("--eval ")
        self.assertIn("elpaca<-build-dir", cleanup_expression)
        self.assertIn("elpaca-builds-directory", cleanup_expression)
        self.assertIn("derived-build", cleanup_expression)
        self.assertIn("delete-directory", cleanup_expression)
        self.assertIn("unload-feature id", cleanup_expression)
        self.assertIn("file-exists-p build-directory", cleanup_expression)
        self.assertIn("load-path", cleanup_expression)
        self.assertIn("ELISP_LIVE_EVIDENCE_", result.stdout)

        builds_root = self.root / "builds"
        build_directory = builds_root / "old-package"
        build_directory.mkdir(parents=True)
        library = build_directory / "old-package.el"
        library.write_text("(provide 'old-package)\n")
        checked = run(
            [
                "emacs",
                "-Q",
                "--batch",
                "--eval",
                "(require 'cl-lib)",
                "--eval",
                f"(setq elpaca-builds-directory {json.dumps(str(builds_root))})",
                "--eval",
                "(defun elpaca-get (_id) t)",
                "--eval",
                f"(defun elpaca<-build-dir (_e) {json.dumps(str(build_directory))})",
                "--eval",
                '(defun elpaca<-package (_e) "old-package")',
                "--eval",
                f"(add-to-list 'load-path {json.dumps(str(build_directory))})",
                "--eval",
                f"(load {json.dumps(str(library))} nil nil t)",
                "--eval",
                f"(unless {cleanup_expression} (kill-emacs 1))",
            ]
        )
        self.assertEqual(checked.returncode, 0, checked.stderr)
        self.assertFalse(build_directory.exists())

        retried = run(
            [
                "emacs",
                "-Q",
                "--batch",
                "--eval",
                "(require 'cl-lib)",
                "--eval",
                f"(setq elpaca-builds-directory {json.dumps(str(builds_root))})",
                "--eval",
                "(defun elpaca-get (_id) nil)",
                "--eval",
                f"(add-to-list 'load-path {json.dumps(str(build_directory))})",
                "--eval",
                f"(unless {cleanup_expression} (kill-emacs 1))",
            ]
        )
        self.assertEqual(retried.returncode, 0, retried.stderr)
        self.assertFalse(build_directory.exists())

    def test_deleted_cleanup_can_retry_after_nil_user_result(self):
        repo = init_repo(self.root / "deleted-retry", "old-package.el")
        (repo / "old-package.el").unlink()
        subprocess.run(["git", "-C", str(repo), "add", "old-package.el"], check=True)
        subprocess.run(
            ["git", "-C", str(repo), "commit", "-qm", "delete old package"],
            check=True,
        )
        command = [
            str(LIVE_VERIFY),
            "deleted:old-package",
            "--",
            "(not (featurep 'old-package))",
        ]
        first_env = self.environment()
        first_env["FAKE_LIVE_RESULT"] = "nil"
        first = run(command, cwd=repo, env=first_env)
        self.assertNotEqual(first.returncode, 0)
        self.assertNotIn("ELISP_LIVE_EVIDENCE_", first.stdout)

        retried = run(command, cwd=repo, env=self.environment())
        self.assertEqual(retried.returncode, 0, retried.stderr)
        self.assertIn("ELISP_LIVE_EVIDENCE_", retried.stdout)

    def test_deleted_package_mode_rejects_nested_vendor_file(self):
        repo = init_repo(self.root / "vendor-deletion", "vendor/foo.el")
        (repo / "vendor/foo.el").unlink()
        subprocess.run(["git", "-C", str(repo), "add", "vendor/foo.el"], check=True)
        subprocess.run(
            ["git", "-C", str(repo), "commit", "-qm", "delete vendor file"],
            check=True,
        )
        result = run(
            [str(LIVE_VERIFY), "deleted:foo", "--", "(not (featurep 'foo))"],
            cwd=repo,
            env=self.environment(),
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("one matching deleted path", result.stderr)
        self.assertNotIn("ELISP_LIVE_EVIDENCE_", result.stdout)

    def test_deleted_package_mode_rejects_remaining_canonical_main(self):
        repo = init_repo(self.root / "remaining-main", "foo.el")
        (repo / "lisp").mkdir()
        (repo / "lisp/foo.el").write_text("(provide 'foo)\n")
        subprocess.run(["git", "-C", str(repo), "add", "lisp/foo.el"], check=True)
        subprocess.run(["git", "-C", str(repo), "commit", "-qm", "add second main"], check=True)
        (repo / "foo.el").unlink()
        subprocess.run(["git", "-C", str(repo), "add", "foo.el"], check=True)
        subprocess.run(["git", "-C", str(repo), "commit", "-qm", "delete root main"], check=True)
        result = run(
            [str(LIVE_VERIFY), "deleted:foo", "--", "(not (featurep 'foo))"],
            cwd=repo,
            env=self.environment(),
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("no canonical main file", result.stderr)
        self.assertNotIn("ELISP_LIVE_EVIDENCE_", result.stdout)


if __name__ == "__main__":
    unittest.main()
