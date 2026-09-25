from __future__ import annotations

import json
import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
HOOKS = (
    DOTFILES / "claude/hooks/load-elisp-after-edit.sh",
    DOTFILES / "codex/hooks/load-elisp-after-edit.sh",
)
EVIDENCE = DOTFILES / "claude/bin/elisp-evidence"
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

    def test_repository_name_mismatch_uses_shared_package_resolver(self):
        source = self.repo / "elpaca/sources/emacs-slack/slack-feed.el"
        source.parent.mkdir(parents=True)
        source.write_text("(provide 'slack-feed)\n")
        emacsclient = self.fake_bin / "emacsclient"
        log = Path(self.temp_dir.name) / "resolver.log"
        emacsclient.write_text(
            "#!/bin/sh\n"
            "printf '%s\\034' \"$*\" >> \"$FAKE_EMACSCLIENT_LOG\"\n"
            "case \"$*\" in\n"
            "  *format-build-reload-status*) printf '\"finished:loaded\"\\n' ;;\n"
            "  *) printf '\"slack:token-1\"\\n' ;;\n"
            "esac\n"
        )
        emacsclient.chmod(0o755)
        for hook in HOOKS:
            with self.subTest(hook=hook):
                marker = Path(self.temp_dir.name) / f"{hook.parents[1].name}-called"
                payload = {"tool_input": {"file_path": str(source)}}
                env = os.environ.copy()
                env["EMACSCLIENT_CALLED"] = str(marker)
                env["FAKE_EMACSCLIENT_LOG"] = str(log)
                env["ELPACA_RELOAD_POLL_INTERVAL_SECONDS"] = "0"
                env["PATH"] = f"{self.fake_bin}:{env['PATH']}"
                result = subprocess.run(
                    ["bash", str(hook)], input=json.dumps(payload), text=True,
                    capture_output=True, check=False, env=env
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn("slack", result.stdout)
        calls = log.read_text()
        self.assertIn("elpaca-extras-resolve-package file", calls)
        self.assertNotIn("elpaca--queued", calls)

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
        shutil.copy2(EVIDENCE, self.primary / "claude/bin/elisp-evidence")
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
            AGENT_ELISP_EVIDENCE_DIR=str(self.root / "evidence"),
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

    def test_deleted_package_is_removed_without_a_rebuild_request(self):
        (self.primary / "emacs/extras/foo.el").unlink()
        subprocess.run(
            ["git", "-C", str(self.primary), "add", "emacs/extras/foo.el"],
            check=True,
        )
        subprocess.run(
            ["git", "-C", str(self.primary), "commit", "-qm", "delete foo"],
            check=True,
        )
        self.new = subprocess.run(
            ["git", "-C", str(self.primary), "rev-parse", "HEAD"],
            text=True,
            capture_output=True,
            check=True,
        ).stdout.strip()
        result = self.run_sync()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse((self.mirror / "emacs/extras/foo.el").exists())
        self.assertFalse((self.state / self.new / "foo.status").exists())
        bar_status = self.state / self.new / "bar.status"
        for _attempt in range(100):
            if bar_status.exists() and bar_status.read_text().startswith("finished:"):
                break
            import time
            time.sleep(0.01)
        self.assertEqual(bar_status.read_text(), "finished:test\n")

    def test_status_retention_preserves_active_obligation(self):
        unreferenced = self.state / ("a" * 40)
        referenced = self.state / self.old
        for directory in (unreferenced, referenced):
            directory.mkdir(parents=True)
            (directory / "fixture.status").write_text("finished:old\n")
            os.utime(directory, (1_600_000_000, 1_600_000_000))
        debts = self.root / "evidence" / "debts"
        debts.mkdir(parents=True)
        (debts / "session.jsonl").write_text(json.dumps(
            {"kind": "live", "label": "foo", "repo": str(self.primary), "commit": self.old}) + "\n")
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
