from __future__ import annotations

import base64
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
VERIFY_TRACKERS = {
    "claude": DOTFILES / "claude/hooks/track-elisp-verify.sh",
    "codex": DOTFILES / "codex/hooks/track-elisp-verify.sh",
}
SYNC_HOOK = Path("/Users/pablostafforini/git-dirs/dotfiles/hooks/sync-elpaca-clone.sh")
CHECK_SYNC_HOOK = DOTFILES / "claude/bin/check-elpaca-sync-hook"


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

    def test_codex_registers_reload_for_direct_and_composed_edits(self):
        config = json.loads((DOTFILES / "codex/hooks.json").read_text())
        registrations = config["hooks"]["PostToolUse"]
        matching = []
        for registration in registrations:
            for hook in registration["hooks"]:
                if hook["command"].endswith("codex/hooks/load-elisp-after-edit.sh"):
                    matching.append((registration["matcher"], hook["timeout"]))
        self.assertIn(("Bash|exec_command|functions.exec|functions.exec_command", 150), matching)
        self.assertIn(("apply_patch|Edit|Write", 150), matching)

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

    def test_codex_reload_recovers_path_from_nested_patch(self):
        marker = Path(self.temp_dir.name) / "nested-patch-called"
        payload = {
            "tool_name": "functions.exec",
            "tool_input": (
                "const result = await tools.apply_patch(`*** Begin Patch\n"
                f"*** Update File: {self.elisp_file}\n"
                "@@\n-(provide 'old)\n+(provide 'example)\n"
                "*** End Patch`);"
            ),
        }
        env = os.environ.copy()
        env["EMACSCLIENT_CALLED"] = str(marker)
        env["PATH"] = f"{self.fake_bin}:{env['PATH']}"
        result = subprocess.run(
            ["bash", str(DOTFILES / "codex/hooks/load-elisp-after-edit.sh")],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
            env=env,
        )
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
        if filename.endswith(".el"):
            repo = self.root / "profile/elpaca/sources" / name
            repo.parent.mkdir(parents=True, exist_ok=True)
        else:
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
        (repo / filename).parent.mkdir(parents=True, exist_ok=True)
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
            ["bash", str(VERIFY_TRACKERS["codex"])],
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
        marker_content: str | None = None,
        reload_state_root: Path | None = None,
        output: str = "",
        tool: str = "codex",
    ):
        marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
        marker.unlink(missing_ok=True)
        if initial_marker:
            marker.write_text(marker_content or "")
        self.addCleanup(marker.unlink, missing_ok=True)
        if tool == "claude":
            payload = {
                "tool_name": "Bash",
                "session_id": session,
                "tool_input": {"command": command, "workdir": str(repo)},
                "tool_output": {"exitCode": exit_code, "stdout": output},
            }
        else:
            payload = {
                "tool_name": "exec_command",
                "session_id": session,
                "tool_input": {"cmd": command, "workdir": str(repo)},
                "tool_response": {"exit_code": exit_code, "output": output},
            }
        env = os.environ.copy()
        if reload_state_root is not None:
            env["ELPACA_RELOAD_STATE_DIR"] = str(reload_state_root)
        result = subprocess.run(
            ["bash", str(VERIFY_TRACKERS[tool])],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
            cwd=self.root,
            env=env,
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

    def test_unattributed_nested_commit_is_not_misattributed(self):
        fallback_repo = self.make_repo("fallback-shell-missing", "fixture.sh")
        result, marker = self.run_source(
            fallback_repo,
            "await tools.exec_command({cmd: 'git commit -m fixture'});",
            f"codex-missing-{os.getpid()}",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(marker.exists())

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

    def test_repeated_nested_non_elisp_commits_do_not_create_marker(self):
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
        self.assertFalse(marker.exists())

    def test_two_non_elisp_commits_in_one_nested_command_do_not_create_marker(self):
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
        self.assertFalse(marker.exists())

    def test_dynamic_nested_workdir_is_not_misattributed(self):
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
        self.assertFalse(marker.exists())

    def test_template_nested_workdir_is_not_misattributed(self):
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
        self.assertFalse(marker.exists())

    def test_duplicate_dynamic_workdir_override_is_not_misattributed(self):
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
        self.assertFalse(marker.exists())

    def test_spread_override_is_not_misattributed(self):
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
        self.assertFalse(marker.exists())

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

    def test_unrelated_direct_emacsclient_event_keeps_marker(self):
        repo = self.make_repo("direct-shell-clear", "fixture.sh")
        result, marker = self.run_direct(
            repo,
            "emacsclient --eval '(message \"fixture\")'",
            f"codex-direct-clear-{os.getpid()}",
            exit_code=0,
            initial_marker=True,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())

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

    def test_matching_live_evidence_clears_only_its_bound_obligation(self):
        repo = self.make_repo("direct-extras-reload", "emacs/extras/fixture.el")
        commit = subprocess.run(
            ["git", "-C", str(repo), "rev-parse", "HEAD"],
            text=True,
            capture_output=True,
            check=True,
        ).stdout.strip()
        encoded_repo = base64.b64encode(str(repo).encode()).decode()
        label = base64.b64encode(b"fixture").decode()
        marker_content = f"{encoded_repo}:{commit}:{label}\n"
        evidence = f"ELISP_LIVE_EVIDENCE_V1:{encoded_repo}:{label}:{commit}\n"

        result, marker = self.run_direct(
            repo,
            "elisp-live-verify fixture -- '(fixture-status)'",
            f"codex-direct-live-{os.getpid()}",
            exit_code=0,
            initial_marker=True,
            marker_content=marker_content,
            output=evidence,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(marker.exists())

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

    def test_matching_live_evidence_works_for_both_trackers(self):
        for tool in ("claude", "codex"):
            with self.subTest(tool=tool):
                repo = self.make_repo(f"{tool}-live", "fixture.el")
                commit = subprocess.run(
                    ["git", "-C", str(repo), "rev-parse", "HEAD"],
                    text=True,
                    capture_output=True,
                    check=True,
                ).stdout.strip()
                encoded_repo = base64.b64encode(str(repo).encode()).decode()
                label = base64.b64encode(repo.name.encode()).decode()
                content = f"{encoded_repo}:{commit}:{label}\n"
                evidence = f"ELISP_LIVE_EVIDENCE_V1:{encoded_repo}:{label}:{commit}\n"
                result, marker = self.run_direct(
                    repo,
                    f"elisp-live-verify {repo.name} -- '({repo.name}-status)'",
                    f"{tool}-live-{os.getpid()}",
                    exit_code=0,
                    initial_marker=True,
                    marker_content=content,
                    output=evidence,
                    tool=tool,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertFalse(marker.exists())

    def test_live_evidence_for_one_label_preserves_other_obligations(self):
        repo = self.make_repo("multi-label", "fixture.el")
        commit = subprocess.run(
            ["git", "-C", str(repo), "rev-parse", "HEAD"],
            text=True,
            capture_output=True,
            check=True,
        ).stdout.strip()
        encoded_repo = base64.b64encode(str(repo).encode()).decode()
        first = base64.b64encode(b"multi-label").decode()
        second = base64.b64encode(b"other").decode()
        content = (
            f"{encoded_repo}:{commit}:{first}\n"
            f"{encoded_repo}:{commit}:{second}\n"
        )
        evidence = f"ELISP_LIVE_EVIDENCE_V1:{encoded_repo}:{first}:{commit}\n"
        result, marker = self.run_direct(
            repo,
            "elisp-live-verify multi-label -- '(multi-label-status)'",
            f"multi-label-{os.getpid()}",
            exit_code=0,
            initial_marker=True,
            marker_content=content,
            output=evidence,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(marker.exists())
        self.assertEqual(marker.read_text(), f"{encoded_repo}:{commit}:{second}\n")

    def test_direct_multi_commit_event_scans_all_new_commits(self):
        for tool in ("claude", "codex"):
            with self.subTest(tool=tool):
                repo = self.make_repo(f"{tool}-multi-commit", "fixture.el")
                (repo / "later.sh").write_text("#!/bin/sh\n")
                subprocess.run(["git", "-C", str(repo), "add", "later.sh"], check=True)
                subprocess.run(
                    ["git", "-C", str(repo), "commit", "-qm", "later"], check=True
                )
                result, marker = self.run_direct(
                    repo,
                    "git commit -m first && git commit -m second",
                    f"{tool}-multi-commit-{os.getpid()}",
                    exit_code=0,
                    initial_marker=False,
                    tool=tool,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertTrue(marker.exists())

    def test_root_elisp_commit_creates_obligation(self):
        for tool in ("claude", "codex"):
            with self.subTest(tool=tool):
                repo = self.root / "root-profile/elpaca/sources" / f"{tool}-root"
                repo.parent.mkdir(parents=True, exist_ok=True)
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
                (repo / "root.el").write_text("(provide 'root)\n")
                subprocess.run(["git", "-C", str(repo), "add", "root.el"], check=True)
                subprocess.run(["git", "-C", str(repo), "commit", "-qm", "root"], check=True)
                result, marker = self.run_direct(
                    repo,
                    "git commit -m root",
                    f"{tool}-root-{os.getpid()}",
                    exit_code=0,
                    initial_marker=False,
                    tool=tool,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertTrue(marker.exists())

    def test_nonpackage_elisp_commit_needs_no_live_package_obligation(self):
        for tool in ("claude", "codex"):
            with self.subTest(tool=tool):
                repo = self.root / f"{tool}-nonpackage"
                repo.mkdir()
                subprocess.run(["git", "init", "-q", str(repo)], check=True)
                subprocess.run(["git", "-C", str(repo), "config", "user.email", "test@example.com"], check=True)
                subprocess.run(["git", "-C", str(repo), "config", "user.name", "Hook Test"], check=True)
                (repo / ".dir-locals.el").write_text("((nil . ((fill-column . 80))))\n")
                subprocess.run(["git", "-C", str(repo), "add", ".dir-locals.el"], check=True)
                subprocess.run(["git", "-C", str(repo), "commit", "-qm", "root"], check=True)
                result, marker = self.run_direct(
                    repo,
                    "git commit -m root",
                    f"{tool}-nonpackage-{os.getpid()}",
                    exit_code=0,
                    initial_marker=False,
                    tool=tool,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertFalse(marker.exists())

    def test_concurrent_commit_events_preserve_all_repositories(self):
        session = f"race-{os.getpid()}"
        marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
        lock = Path(f"{marker}.lock")
        marker.unlink(missing_ok=True)
        if lock.is_dir():
            lock.rmdir()
        self.addCleanup(marker.unlink, missing_ok=True)
        repos = [self.make_repo(f"race-{index}", "fixture.el") for index in range(12)]
        processes = []
        for repo in repos:
            payload = {
                "tool_name": "exec_command",
                "session_id": session,
                "tool_input": {"cmd": "git commit -m fixture", "workdir": str(repo)},
                "tool_response": {"exit_code": 0},
            }
            process = subprocess.Popen(
                ["bash", str(VERIFY_TRACKERS["codex"])],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                cwd=self.root,
            )
            processes.append((process, json.dumps(payload)))
        for process, payload in processes:
            _stdout, stderr = process.communicate(payload, timeout=10)
            self.assertEqual(process.returncode, 0, stderr)
        self.assertEqual(len(marker.read_text().splitlines()), len(repos))

    def test_verify_gates_allow_only_executable_helpers(self):
        repo = self.make_repo("gate", "fixture.el")
        for tool in ("claude", "codex"):
            with self.subTest(tool=tool):
                session = f"{tool}-gate-{os.getpid()}"
                marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
                marker.write_text("malformed:keeps:gate\n")
                self.addCleanup(marker.unlink, missing_ok=True)
                if tool == "claude":
                    payload = lambda command: {
                        "tool_name": "Bash",
                        "session_id": session,
                        "tool_input": {"command": command, "workdir": str(repo)},
                    }
                else:
                    payload = lambda command: {
                        "tool_name": "exec_command",
                        "session_id": session,
                        "tool_input": {"cmd": command, "workdir": str(repo)},
                    }
                gate = DOTFILES / f"{tool}/hooks/require-elisp-verify-after-commit.sh"
                quoted = subprocess.run(
                    ["bash", str(gate)], input=json.dumps(payload("printf '%s' elisp-live-verify")),
                    text=True, capture_output=True, check=False, cwd=repo,
                )
                allowed = subprocess.run(
                    ["bash", str(gate)], input=json.dumps(payload("elisp-live-verify gate -- '(gate-status)'")),
                    text=True, capture_output=True, check=False, cwd=repo,
                )
                self.assertIn("permissionDecision", quoted.stdout)
                self.assertEqual(allowed.stdout, "")

    def test_active_claude_dispatcher_uses_bound_live_gate(self):
        repo = self.make_repo("claude-dispatch", "fixture.el")
        session = f"claude-dispatch-{os.getpid()}"
        marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
        marker.write_text("malformed:keeps:gate\n")
        self.addCleanup(marker.unlink, missing_ok=True)
        payload = {
            "tool_name": "Bash",
            "session_id": session,
            "tool_input": {"command": "printf '%s' emacsclient", "workdir": str(repo)},
        }
        result = subprocess.run(
            ["bash", str(DOTFILES / "claude/hooks/pretooluse-bash.sh")],
            input=json.dumps(payload), text=True, capture_output=True, check=False, cwd=repo,
        )
        self.assertIn("package- and commit-bound live evidence", result.stdout)

    def test_codex_composed_live_helper_is_allowed_and_consumed(self):
        repo = self.make_repo("codex-composed-live", "fixture.el")
        commit = subprocess.run(
            ["git", "-C", str(repo), "rev-parse", "HEAD"],
            text=True, capture_output=True, check=True,
        ).stdout.strip()
        encoded_repo = base64.b64encode(str(repo).encode()).decode()
        label = base64.b64encode(repo.name.encode()).decode()
        content = f"{encoded_repo}:{commit}:{label}\n"
        evidence = f"ELISP_LIVE_EVIDENCE_V1:{encoded_repo}:{label}:{commit}\n"
        session = f"codex-composed-live-{os.getpid()}"
        marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
        marker.write_text(content)
        self.addCleanup(marker.unlink, missing_ok=True)
        shell_command = f"elisp-live-verify {repo.name} -- '({repo.name}-status)'"
        source = (
            "const r = await tools.exec_command("
            + json.dumps({"cmd": shell_command, "workdir": str(repo)})
            + "); text(r.output);"
        )
        payload = {
            "tool_name": "functions.exec",
            "session_id": session,
            "tool_input": source,
            "tool_response": {"exit_code": 0, "output": evidence},
        }
        gate = subprocess.run(
            ["bash", str(DOTFILES / "codex/hooks/require-elisp-verify-after-commit.sh")],
            input=json.dumps(payload), text=True, capture_output=True, check=False, cwd=repo,
        )
        self.assertEqual(gate.stdout, "")
        tracked = subprocess.run(
            ["bash", str(VERIFY_TRACKERS["codex"])],
            input=json.dumps(payload), text=True, capture_output=True, check=False, cwd=repo,
        )
        self.assertEqual(tracked.returncode, 0, tracked.stderr)
        self.assertFalse(marker.exists())

class ElpacaSyncHookTests(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        self.root = Path(self.temp_dir.name)
        self.primary = self.root / "primary"
        self.primary.mkdir()
        subprocess.run(["git", "init", "-q", str(self.primary)], check=True)
        subprocess.run(["git", "-C", str(self.primary), "config", "user.email", "test@example.com"], check=True)
        subprocess.run(["git", "-C", str(self.primary), "config", "user.name", "Hook Test"], check=True)
        for package in ("foo", "bar"):
            source = self.primary / f"emacs/extras/{package}.el"
            source.parent.mkdir(parents=True, exist_ok=True)
            source.write_text(f"(provide '{package})\n")
        helper = self.primary / "claude/bin/elpaca-rebuild-wait"
        helper.parent.mkdir(parents=True)
        helper.write_text("#!/bin/sh\nprintf '%s\\n' 'finished:test' > \"$ELPACA_RELOAD_STATUS_FILE\"\n")
        helper.chmod(0o755)
        subprocess.run(["git", "-C", str(self.primary), "add", "."], check=True)
        subprocess.run(["git", "-C", str(self.primary), "commit", "-qm", "baseline"], check=True)
        self.old = subprocess.run(
            ["git", "-C", str(self.primary), "rev-parse", "HEAD"],
            text=True, capture_output=True, check=True,
        ).stdout.strip()
        self.home = self.root / "home"
        profile_root = self.home / ".config/emacs-profiles/test/elpaca/sources"
        profile_root.mkdir(parents=True)
        self.mirror = profile_root / "dotfiles"
        subprocess.run(["git", "clone", "-q", str(self.primary), str(self.mirror)], check=True)
        profile_cache = self.home / ".config/emacs-profiles/.current-profile"
        profile_cache.parent.mkdir(parents=True, exist_ok=True)
        profile_cache.write_text("test\n")
        for package in ("foo", "bar"):
            (self.primary / f"emacs/extras/{package}.el").write_text(f"(defun {package}-new ())\n")
            subprocess.run(["git", "-C", str(self.primary), "add", f"emacs/extras/{package}.el"], check=True)
            subprocess.run(["git", "-C", str(self.primary), "commit", "-qm", f"change {package}"], check=True)
        self.new = subprocess.run(
            ["git", "-C", str(self.primary), "rev-parse", "HEAD"],
            text=True, capture_output=True, check=True,
        ).stdout.strip()
        self.state = self.root / "state"

    def run_sync(self):
        env = os.environ.copy()
        env.update(
            HOME=str(self.home),
            GIT_DIR=str(self.primary / ".git"),
            GIT_WORK_TREE=str(self.primary),
            ELPACA_RELOAD_STATE_DIR=str(self.state),
        )
        return subprocess.run(
            ["sh", str(SYNC_HOOK), "rebase"], input=f"{self.old} {self.new}\n",
            text=True, capture_output=True, check=False, cwd=self.primary, env=env,
        )

    def test_rewrite_ranges_rebuild_every_changed_package(self):
        result = self.run_sync()
        self.assertEqual(result.returncode, 0, result.stderr)
        for package in ("foo", "bar"):
            status = self.state / self.new / f"{package}.status"
            for _attempt in range(100):
                if status.exists() and status.read_text().startswith("finished:"):
                    break
                import time
                time.sleep(0.01)
            self.assertEqual(status.read_text(), "finished:test\n")
        mirror_head = subprocess.run(
            ["git", "-C", str(self.mirror), "rev-parse", "HEAD"],
            text=True, capture_output=True, check=True,
        ).stdout.strip()
        self.assertEqual(mirror_head, self.new)

    def test_status_retention_preserves_active_obligation(self):
        unreferenced = self.state / ("a" * 40)
        referenced = self.state / self.old
        for directory in (unreferenced, referenced):
            directory.mkdir(parents=True)
            (directory / "fixture.status").write_text("finished:old\n")
            os.utime(directory, (1_600_000_000, 1_600_000_000))
        session = f"retention-{os.getpid()}"
        marker = Path(f"/tmp/claude-elisp-verify-needed-{session}")
        marker.write_text(
            f"{base64.b64encode(str(self.primary).encode()).decode()}:{self.old}:{base64.b64encode(b'foo').decode()}\n"
        )
        self.addCleanup(marker.unlink, missing_ok=True)
        result = self.run_sync()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(unreferenced.exists())
        self.assertTrue(referenced.exists())

    def test_installed_rewrite_wrapper_forwards_hook_arguments(self):
        wrapper = Path("/Users/pablostafforini/git-dirs/dotfiles/hooks/post-rewrite")
        self.assertIn('sync-elpaca-clone.sh" "$@"', wrapper.read_text())

    def test_embedded_and_installed_sync_hooks_are_identical(self):
        result = subprocess.run(
            [str(CHECK_SYNC_HOOK)], text=True, capture_output=True, check=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr + result.stdout)


if __name__ == "__main__":
    unittest.main()
