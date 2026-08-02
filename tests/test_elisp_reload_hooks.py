from __future__ import annotations

import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
HOOKS = (
    DOTFILES / "claude/hooks/load-elisp-after-edit.sh",
    DOTFILES / "codex/hooks/load-elisp-after-edit.sh",
)
VERIFY_TRACKER = DOTFILES / "codex/hooks/track-elisp-verify.sh"


class ElispReloadHookTests(unittest.TestCase):
    def run_hook(self, hook: Path, file_path: Path, marker: Path):
        payload = {"tool_input": {"file_path": str(file_path)}}
        env = os.environ.copy()
        env["EMACSCLIENT_CALLED"] = str(marker)
        env["PATH"] = f"{self.fake_bin}:{env['PATH']}"
        return subprocess.run(
            ["bash", str(hook)],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
            env=env,
        )

    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        root = Path(self.temp_dir.name)
        self.repo = root / "repo"
        self.repo.mkdir()
        subprocess.run(
            ["git", "init", "-q", str(self.repo)],
            check=True,
        )
        self.elisp_file = self.repo / "elpaca/sources/example/example.el"
        self.elisp_file.parent.mkdir(parents=True)
        self.elisp_file.write_text("(provide 'example)\n")
        self.fake_bin = root / "bin"
        self.fake_bin.mkdir()
        emacsclient = self.fake_bin / "emacsclient"
        emacsclient.write_text(
            "#!/bin/sh\n"
            ": > \"$EMACSCLIENT_CALLED\"\n"
            "printf 'nil\\n'\n"
        )
        emacsclient.chmod(0o755)

    def test_skips_reload_during_git_operation(self):
        operations = (
            ("rebase-merge", True),
            ("rebase-apply", True),
            ("MERGE_HEAD", False),
            ("CHERRY_PICK_HEAD", False),
            ("REVERT_HEAD", False),
        )
        for operation, is_directory in operations:
            operation_path = self.repo / ".git" / operation
            operation_path.mkdir() if is_directory else operation_path.touch()
            try:
                for hook in HOOKS:
                    with self.subTest(operation=operation, hook=hook):
                        marker = (
                            Path(self.temp_dir.name)
                            / f"{operation}-{hook.parents[1].name}-called"
                        )
                        result = self.run_hook(hook, self.elisp_file, marker)
                        self.assertEqual(result.returncode, 0, result.stderr)
                        self.assertFalse(marker.exists())
                        self.assertIn("Git operation is in progress", result.stdout)
            finally:
                operation_path.rmdir() if is_directory else operation_path.unlink()

    def test_skips_reload_for_markerless_unmerged_index(self):
        relative_path = self.elisp_file.relative_to(self.repo)

        def write_blob(contents: str) -> str:
            result = subprocess.run(
                ["git", "-C", str(self.repo), "hash-object", "-w", "--stdin"],
                input=contents,
                text=True,
                capture_output=True,
                check=True,
            )
            return result.stdout.strip()

        base = write_blob("(provide 'base)\n")
        ours = write_blob("(provide 'ours)\n")
        theirs = write_blob("(provide 'theirs)\n")
        index_entries = "".join(
            f"100644 {blob} {stage}\t{relative_path}\n"
            for stage, blob in ((1, base), (2, ours), (3, theirs))
        )
        subprocess.run(
            ["git", "-C", str(self.repo), "update-index", "--index-info"],
            input=index_entries,
            text=True,
            check=True,
        )

        for hook in HOOKS:
            with self.subTest(hook=hook):
                marker = (
                    Path(self.temp_dir.name)
                    / f"unmerged-{hook.parents[1].name}-called"
                )
                result = self.run_hook(hook, self.elisp_file, marker)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertFalse(marker.exists())
                self.assertIn("Git operation is in progress", result.stdout)

    def test_reload_still_runs_outside_git_operation(self):
        for hook in HOOKS:
            with self.subTest(hook=hook):
                marker = Path(self.temp_dir.name) / f"{hook.parents[1].name}-called"
                result = self.run_hook(hook, self.elisp_file, marker)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertTrue(marker.exists())

    def test_skips_elisp_files_in_test_directories(self):
        for directory in ("test", "tests"):
            test_file = self.elisp_file.parent / directory / "helpers.el"
            test_file.parent.mkdir()
            test_file.write_text("(provide 'helpers)\n")
            for hook in HOOKS:
                with self.subTest(directory=directory, hook=hook):
                    marker = (
                        Path(self.temp_dir.name)
                        / f"{directory}-{hook.parents[1].name}-called"
                    )
                    result = self.run_hook(hook, test_file, marker)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertFalse(marker.exists())

    def test_skips_all_supported_test_filename_patterns(self):
        for filename in ("example-test.el", "example-tests.el", "test-example.el"):
            test_file = self.elisp_file.parent / filename
            test_file.write_text("(provide 'example-test)\n")
            for hook in HOOKS:
                with self.subTest(filename=filename, hook=hook):
                    marker = (
                        Path(self.temp_dir.name)
                        / f"{filename}-{hook.parents[1].name}-called"
                    )
                    result = self.run_hook(hook, test_file, marker)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertFalse(marker.exists())

    def test_test_named_ancestor_does_not_suppress_production_reload(self):
        repo = Path(self.temp_dir.name) / "test" / "production-repo"
        source = repo / "elpaca/sources/example/example.el"
        source.parent.mkdir(parents=True)
        source.write_text("(provide 'example)\n")
        subprocess.run(["git", "init", "-q", str(repo)], check=True)
        for hook in HOOKS:
            with self.subTest(hook=hook):
                marker = (
                    Path(self.temp_dir.name)
                    / f"ancestor-{hook.parents[1].name}-called"
                )
                result = self.run_hook(hook, source, marker)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertTrue(marker.exists())

    def test_parent_components_do_not_disguise_production_source_as_test(self):
        test_directory = self.elisp_file.parent / "test"
        test_directory.mkdir()
        disguised_path = test_directory / ".." / self.elisp_file.name
        for hook in HOOKS:
            with self.subTest(hook=hook):
                marker = (
                    Path(self.temp_dir.name)
                    / f"parent-component-{hook.parents[1].name}-called"
                )
                result = self.run_hook(hook, disguised_path, marker)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertTrue(marker.exists())


class ElispVerifyTrackingHookTests(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        self.root = Path(self.temp_dir.name)

    def make_repo(self, name: str, filename: str) -> Path:
        repo = self.root / name
        repo.mkdir()
        subprocess.run(["git", "init", "-q", str(repo)], check=True)
        subprocess.run(
            ["git", "-C", str(repo), "config", "user.email", "test@example.com"],
            check=True,
        )
        subprocess.run(
            ["git", "-C", str(repo), "config", "user.name", "Hook Test"],
            check=True,
        )
        (repo / "README.md").write_text("fixture\n")
        subprocess.run(["git", "-C", str(repo), "add", "README.md"], check=True)
        subprocess.run(["git", "-C", str(repo), "commit", "-qm", "baseline"], check=True)
        (repo / filename).write_text("(provide 'fixture)\n" if filename.endswith(".el") else "#!/bin/sh\n")
        subprocess.run(["git", "-C", str(repo), "add", filename], check=True)
        subprocess.run(["git", "-C", str(repo), "commit", "-qm", "fixture"], check=True)
        return repo

    def run_source(
        self,
        fallback_repo: Path,
        source: str,
        session: str,
        exit_code: int = 0,
        initial_marker: bool = False,
    ):
        payload = {
            "tool_name": "functions.exec",
            "session_id": session,
            "tool_input": source,
            "tool_response": {"exit_code": exit_code},
        }
        marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
        marker.unlink(missing_ok=True)
        if initial_marker:
            marker.touch()
        self.addCleanup(marker.unlink, missing_ok=True)
        result = subprocess.run(
            ["bash", str(VERIFY_TRACKER)],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
            cwd=fallback_repo,
        )
        return result, marker

    def run_tracker(self, fallback_repo: Path, target_repo: Path, session: str):
        source = (
            "const r = await tools.exec_command("
            + json.dumps({"cmd": 'git commit -m "fixture"', "workdir": str(target_repo)})
            + "); text(r.output);"
        )
        return self.run_source(fallback_repo, source, session)

    def run_direct(
        self,
        repo: Path,
        command: str,
        session: str,
        exit_code: int,
        initial_marker: bool,
    ):
        marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
        marker.unlink(missing_ok=True)
        if initial_marker:
            marker.touch()
        self.addCleanup(marker.unlink, missing_ok=True)
        payload = {
            "tool_name": "exec_command",
            "session_id": session,
            "tool_input": {"cmd": command, "workdir": str(repo)},
            "tool_response": {"exit_code": exit_code},
        }
        result = subprocess.run(
            ["bash", str(VERIFY_TRACKER)],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
            cwd=self.root,
        )
        return result, marker

    def test_nested_non_elisp_commit_does_not_inspect_fallback_repo(self):
        fallback_repo = self.make_repo("fallback-elisp", "fixture.el")
        target_repo = self.make_repo("target-shell", "fixture.sh")
        result, marker = self.run_tracker(
            fallback_repo,
            target_repo,
            f"codex-non-elisp-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(marker.exists())

    def test_nested_elisp_commit_uses_its_explicit_workdir(self):
        fallback_repo = self.make_repo("fallback-shell", "fixture.sh")
        target_repo = self.make_repo("target-elisp", "fixture.el")
        result, marker = self.run_tracker(
            fallback_repo,
            target_repo,
            f"codex-elisp-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_nested_workdir_before_command_stays_paired(self):
        fallback_repo = self.make_repo("fallback-elisp-order", "fixture.el")
        target_repo = self.make_repo("target-shell-order", "fixture.sh")
        source = (
            "await tools.exec_command({workdir: "
            + json.dumps(str(target_repo))
            + ", cmd: 'git commit -m fixture'});"
        )
        result, marker = self.run_source(
            fallback_repo,
            source,
            f"codex-order-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(marker.exists())

    def test_decoy_command_object_is_not_treated_as_nested_call(self):
        fallback_repo = self.make_repo("fallback-elisp-decoy", "fixture.el")
        target_repo = self.make_repo("target-shell-decoy", "fixture.sh")
        source = (
            "const decoy = "
            + json.dumps({"cmd": "git commit -m decoy", "workdir": str(fallback_repo)})
            + "; await tools.exec_command("
            + json.dumps({"cmd": "true", "workdir": str(target_repo)})
            + ");"
        )
        result, marker = self.run_source(
            fallback_repo,
            source,
            f"codex-decoy-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(marker.exists())

    def test_quoted_commit_text_is_not_treated_as_a_commit(self):
        fallback_repo = self.make_repo("fallback-shell-quoted", "fixture.sh")
        target_repo = self.make_repo("target-elisp-quoted", "fixture.el")
        source = (
            "await tools.exec_command("
            + json.dumps(
                {
                    "cmd": "printf '%s\\n' 'git commit' # git commit in a comment",
                    "workdir": str(target_repo),
                }
            )
            + ");"
        )
        result, marker = self.run_source(
            fallback_repo,
            source,
            f"codex-quoted-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(marker.exists())

    def test_shell_interpreter_commit_payload_remains_conservative(self):
        fallback_repo = self.make_repo("fallback-shell-interpreter", "fixture.sh")
        target_repo = self.make_repo("target-elisp-interpreter", "fixture.el")
        source = (
            "await tools.exec_command("
            + json.dumps(
                {
                    "cmd": "bash -c 'git commit -m fixture'",
                    "workdir": str(target_repo),
                }
            )
            + ");"
        )
        result, marker = self.run_source(
            fallback_repo,
            source,
            f"codex-interpreter-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_unattributed_nested_commit_requires_verification(self):
        fallback_repo = self.make_repo("fallback-shell-missing", "fixture.sh")
        result, marker = self.run_source(
            fallback_repo,
            "await tools.exec_command({cmd: 'git commit -m fixture'});",
            f"codex-missing-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_nonzero_outer_result_does_not_hide_nested_elisp_commit(self):
        fallback_repo = self.make_repo("fallback-shell-failed-outer", "fixture.sh")
        target_repo = self.make_repo("target-elisp-failed-outer", "fixture.el")
        source = (
            "await tools.exec_command("
            + json.dumps({"cmd": "git commit -m fixture", "workdir": str(target_repo)})
            + "); throw new Error('later failure');"
        )
        result, marker = self.run_source(
            fallback_repo,
            source,
            f"codex-failed-outer-{os.getpid()}",
            exit_code=1,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_repeated_nested_commits_to_one_repo_are_conservative(self):
        fallback_repo = self.make_repo("fallback-shell-repeat", "fixture.sh")
        target_repo = self.make_repo("target-shell-repeat", "fixture.sh")
        call = (
            "await tools.exec_command("
            + json.dumps({"cmd": "git commit -m fixture", "workdir": str(target_repo)})
            + ");"
        )
        result, marker = self.run_source(
            fallback_repo,
            call + call,
            f"codex-repeat-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_two_commits_in_one_nested_command_are_conservative(self):
        fallback_repo = self.make_repo("fallback-shell-one-call", "fixture.sh")
        target_repo = self.make_repo("target-shell-one-call", "fixture.sh")
        source = (
            "await tools.exec_command("
            + json.dumps(
                {
                    "cmd": "git commit -m first && git commit -m second",
                    "workdir": str(target_repo),
                }
            )
            + ");"
        )
        result, marker = self.run_source(
            fallback_repo,
            source,
            f"codex-one-call-repeat-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_dynamic_nested_workdir_requires_verification(self):
        fallback_repo = self.make_repo("fallback-shell-dynamic", "fixture.sh")
        source = (
            "const target = '/tmp/dynamic'; "
            "await tools.exec_command({cmd: 'git commit -m fixture', workdir: target});"
        )
        result, marker = self.run_source(
            fallback_repo,
            source,
            f"codex-dynamic-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_template_nested_workdir_requires_verification(self):
        fallback_repo = self.make_repo("fallback-shell-template", "fixture.sh")
        source = (
            "const target = '/tmp/dynamic'; "
            "await tools.exec_command({cmd: 'git commit -m fixture', "
            "workdir: `${target}`});"
        )
        result, marker = self.run_source(
            fallback_repo,
            source,
            f"codex-template-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_duplicate_dynamic_workdir_override_requires_verification(self):
        fallback_repo = self.make_repo("fallback-shell-duplicate", "fixture.sh")
        target_repo = self.make_repo("target-shell-duplicate", "fixture.sh")
        source = (
            "const target = '/tmp/dynamic'; "
            "await tools.exec_command({cmd: 'git commit -m fixture', workdir: "
            + json.dumps(str(target_repo))
            + ", workdir: target});"
        )
        result, marker = self.run_source(
            fallback_repo,
            source,
            f"codex-duplicate-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_spread_override_requires_verification(self):
        fallback_repo = self.make_repo("fallback-shell-spread", "fixture.sh")
        target_repo = self.make_repo("target-shell-spread", "fixture.sh")
        source = (
            "const actualArgs = {}; "
            "await tools.exec_command({cmd: 'true', workdir: "
            + json.dumps(str(target_repo))
            + ", ...actualArgs});"
        )
        result, marker = self.run_source(
            fallback_repo,
            source,
            f"codex-spread-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_outer_wrapper_cannot_clear_verification_marker(self):
        fallback_repo = self.make_repo("fallback-shell-clear", "fixture.sh")
        source = (
            "await tools.exec_command("
            + json.dumps(
                {
                    "cmd": "emacsclient --eval '(message \"fixture\")'",
                    "workdir": str(fallback_repo),
                }
            )
            + ");"
        )
        result, marker = self.run_source(
            fallback_repo,
            source,
            f"codex-outer-clear-{os.getpid()}",
            initial_marker=True,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_exact_successful_direct_emacsclient_event_clears_marker(self):
        repo = self.make_repo("direct-shell-clear", "fixture.sh")
        result, marker = self.run_direct(
            repo,
            "emacsclient --eval '(message \"fixture\")'",
            f"codex-direct-clear-{os.getpid()}",
            exit_code=0,
            initial_marker=True,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(marker.exists())

    def test_failed_direct_emacsclient_event_keeps_marker(self):
        repo = self.make_repo("direct-shell-failed-clear", "fixture.sh")
        result, marker = self.run_direct(
            repo,
            "emacsclient --eval '(message \"fixture\")'",
            f"codex-direct-failed-clear-{os.getpid()}",
            exit_code=1,
            initial_marker=True,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_emacsclient_text_in_commit_message_cannot_clear_marker(self):
        repo = self.make_repo("direct-elisp-commit-message", "fixture.el")
        result, marker = self.run_direct(
            repo,
            "git commit -m 'emacsclient --eval fake'",
            f"codex-commit-message-{os.getpid()}",
            exit_code=0,
            initial_marker=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

    def test_expanding_commit_message_cannot_clear_marker(self):
        repo = self.make_repo("direct-elisp-expanding-message", "fixture.el")
        result, marker = self.run_direct(
            repo,
            'git commit -m "x; emacsclient --eval $HOME"',
            f"codex-expanding-message-{os.getpid()}",
            exit_code=0,
            initial_marker=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

if __name__ == "__main__":
    unittest.main()
