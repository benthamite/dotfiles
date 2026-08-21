from __future__ import annotations

import base64
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


DOTFILES = Path(__file__).resolve().parents[1]
REVISION_HELPER = DOTFILES / "claude/bin/elisp-source-revision"
BATCH_TEST = DOTFILES / "claude/bin/batch-test.sh"
REBUILD_WAIT = DOTFILES / "claude/bin/elpaca-rebuild-wait"
CHECK_EVIDENCE = DOTFILES / "claude/bin/elisp-check-evidence"
LIVE_VERIFY = DOTFILES / "claude/bin/elisp-live-verify"
EVIDENCE_LIB = DOTFILES / "claude/hooks/lib-elisp-evidence.sh"


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
        emacsclient.write_text("#!/bin/sh\nprintf '\"test\"\\n'\n")
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
        if stale:
            env["FAKE_EMACS_STALE"] = "1"
        return env

    def test_loads_canonical_standalone_source_and_emits_evidence(self):
        result = run([str(BATCH_TEST), "example"], env=self.environment())
        self.assertEqual(result.returncode, 0, result.stderr)
        encoded_source = base64.b64encode(str(self.source / "example.el").encode()).decode()
        encoded_source_dir = base64.b64encode(str(self.source).encode()).decode()
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
        self.assertIn("does not match the wrapper command", tracked.stderr)
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
        env["PATH"] = f"{self.fake_bin}:{env['PATH']}"
        return env

    def write_emacsclient(self):
        counter = self.root / "counter"
        script = self.fake_bin / "emacsclient"
        script.write_text(
            "#!/bin/sh\n"
            "printf '%s\\034' \"$*\" >> \"$EMACSCLIENT_CALLED\"\n"
            "if [ -n \"${FAKE_EMACSCLIENT_LOG:-}\" ]; then printf '%s\\034' \"$*\" >> \"$FAKE_EMACSCLIENT_LOG\"; fi\n"
            f"counter={counter!s}\n"
            "case \"$*\" in\n"
            "  *elpaca-extras-resolve-package*)\n"
            "    source=$(printf '%s' \"$FAKE_PACKAGE_SOURCE/\" | base64 | tr -d '\\n')\n"
            "    label=$(basename \"$FAKE_PACKAGE_SOURCE\" | base64 | tr -d '\\n')\n"
            "    printf '\"%s:%s:%s\"\\n' \"$FAKE_PACKAGE_ID\" \"$source\" \"$label\" ;;\n"
            "  *format-build-reload-status*)\n"
            "    count=$(sed -n '1p' \"$counter\" 2>/dev/null || printf '0')\n"
            "    count=$((count + 1)); printf '%s\\n' \"$count\" > \"$counter\"\n"
            "    if [ \"$count\" -ge 2 ]; then printf '\"finished:loaded\"\\n'; else printf '\"queued:building\"\\n'; fi ;;\n"
            "  *) printf '\"token-1\"\\n' ;;\n"
            "esac\n"
        )
        script.chmod(0o755)

    def test_observes_finished_post_commit_state_without_new_request(self):
        commit = run(["git", "-C", str(self.dotfiles), "rev-parse", "HEAD"]).stdout.strip()
        status = self.state / commit / "example.status"
        status.parent.mkdir(parents=True)
        status.write_text("finished:already loaded\n")
        self.write_emacsclient()
        result = run(
            [str(REBUILD_WAIT), "example"], env=self.environment(), cwd=self.dotfiles
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(self.called.exists())

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
        self.assertFalse(self.called.exists())


class ElispLiveVerifyTests(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        self.root = Path(self.temp_dir.name)
        self.repo = init_repo(self.root / "example", "example.el")
        self.fake_bin = self.root / "bin"
        self.fake_bin.mkdir()
        emacsclient = self.fake_bin / "emacsclient"
        emacsclient.write_text(
            "#!/bin/sh\n"
            "if [ -n \"${FAKE_EMACSCLIENT_LOG:-}\" ]; then printf '%s\\034' \"$*\" >> \"$FAKE_EMACSCLIENT_LOG\"; fi\n"
            "case \"$*\" in\n"
            "  *elpaca-extras-resolve-package*)\n"
            "    source=$(printf '%s' \"$FAKE_PACKAGE_SOURCE/\" | base64 | tr -d '\\n')\n"
            "    label=$(basename \"$FAKE_PACKAGE_SOURCE\" | base64 | tr -d '\\n')\n"
            "    printf '\"%s:%s:%s\"\\n' \"$FAKE_PACKAGE_ID\" \"$source\" \"$label\" ;;\n"
            "  *format-build-reload-status*) printf '%s\\n' '\"finished:loaded\"' ;;\n"
            "  *elpaca-extras-rebuild-and-reload*) printf '%s\\n' '\"token-1\"' ;;\n"
            "  *unload-feature*) printf '%s\\n' 't' ;;\n"
            "  *) printf '%s\\n' \"${FAKE_LIVE_RESULT:-t}\" ;;\n"
            "esac\n"
        )
        emacsclient.chmod(0o755)

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
