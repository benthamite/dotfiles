from __future__ import annotations

import json
import os
import shutil
import subprocess
import tempfile
import time
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
HOOKS = (
    DOTFILES / "claude/hooks/load-elisp-after-edit.sh",
    DOTFILES / "codex/hooks/load-elisp-after-edit.sh",
)
EVIDENCE = DOTFILES / "claude/bin/elisp-evidence"
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


def documented_hook(name: str, directory: Path) -> Path:
    """Write the hook NAME documented in emacs/config.org into DIRECTORY."""
    lines = (DOTFILES / "emacs/config.org").read_text().splitlines()
    start = lines.index(f"=hooks/{name}=:")
    begin = next(i for i in range(start, len(lines)) if lines[i].startswith("#+begin_src shell"))
    end = lines.index("#+end_src", begin)
    path = directory / name
    path.write_text("\n".join(lines[begin + 1:end]) + "\n")
    path.chmod(0o755)
    return path


class ElpacaSyncHookTests(unittest.TestCase):
    """Exercise the documented hooks, which are what --install writes."""

    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        self.root = Path(self.temp_dir.name)
        self.home = self.root / "home"
        self.state = self.root / "state"
        self.hooks = self.root / "hooks"
        self.hooks.mkdir()
        self.no_hooks = self.root / "no-hooks"
        self.no_hooks.mkdir()
        for name in ("sync-elpaca-clone.sh", "post-commit", "post-rewrite", "post-merge"):
            documented_hook(name, self.hooks)
        self.sync_hook = self.hooks / "sync-elpaca-clone.sh"
        self.env = os.environ.copy()
        for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE",
                     "CLAUDE_CODE_SESSION_ID", "CODEX_THREAD_ID"):
            self.env.pop(name, None)
        self.env.update(
            HOME=str(self.home),
            GIT_CONFIG_NOSYSTEM="1",
            ELPACA_RELOAD_STATE_DIR=str(self.state),
            AGENT_ELISP_EVIDENCE_DIR=str(self.root / "evidence"),
        )
        self.primary = self.root / "primary"
        self.primary.mkdir()
        self.git("init", "-q", "-b", "master", str(self.primary), cwd=self.root)
        self.git("config", "user.email", "test@example.com")
        self.git("config", "user.name", "Hook Test")
        for package in ("foo", "bar"):
            source = self.primary / f"emacs/extras/{package}.el"
            source.parent.mkdir(parents=True, exist_ok=True)
            source.write_text(f"(provide '{package})\n")
        helper = self.primary / "claude/bin/elpaca-rebuild-wait"
        helper.parent.mkdir(parents=True)
        helper.write_text("#!/bin/sh\nprintf '%s\\n' 'finished:test' > \"$ELPACA_RELOAD_STATUS_FILE\"\n")
        helper.chmod(0o755)
        shutil.copy2(EVIDENCE, self.primary / "claude/bin/elisp-evidence")
        self.git("add", ".")
        self.git("commit", "-qm", "baseline", hooks=False)
        self.old = self.rev("HEAD")
        profile_root = self.home / ".config/emacs-profiles/test/elpaca/sources"
        profile_root.mkdir(parents=True)
        self.mirror = profile_root / "dotfiles"
        self.git("clone", "-q", str(self.primary), str(self.mirror), cwd=self.root)
        profile_cache = self.home / ".config/emacs-profiles/.current-profile"
        profile_cache.write_text("test\n")
        for package in ("foo", "bar"):
            self.change(package, f"(defun {package}-new ())\n", hooks=False)
        self.new = self.rev("HEAD")

    def git(self, *args, cwd=None, hooks=True):
        hooks_path = self.hooks if hooks else self.no_hooks
        return subprocess.run(
            ["git", "-c", f"core.hooksPath={hooks_path}", *args],
            text=True, capture_output=True, check=True,
            cwd=cwd or self.primary, env=self.env,
        )

    def rev(self, ref, repo=None):
        return self.git("rev-parse", ref, cwd=repo).stdout.strip()

    def change(self, package, text, cwd=None, hooks=True):
        cwd = cwd or self.primary
        (cwd / f"emacs/extras/{package}.el").write_text(text)
        self.git("add", f"emacs/extras/{package}.el", cwd=cwd)
        self.git("commit", "-qm", f"change {package}", cwd=cwd, hooks=hooks)

    def mirror_head(self):
        return self.rev("HEAD", repo=self.mirror)

    def wait_for_status(self, commit, package):
        status = self.state / commit / f"{package}.status"
        for _attempt in range(200):
            if status.exists() and status.read_text().startswith("finished:"):
                break
            time.sleep(0.01)
        return status.read_text() if status.exists() else ""

    def run_sync(self):
        env = dict(self.env, GIT_DIR=str(self.primary / ".git"), GIT_WORK_TREE=str(self.primary))
        return subprocess.run(
            ["sh", str(self.sync_hook), "rebase"], input=f"{self.old} {self.new}\n",
            text=True, capture_output=True, check=False, cwd=self.primary, env=env,
        )

    def test_rewrite_ranges_rebuild_every_changed_package(self):
        result = self.run_sync()
        self.assertEqual(result.returncode, 0, result.stderr)
        for package in ("foo", "bar"):
            self.assertEqual(self.wait_for_status(self.new, package), "finished:test\n")
        self.assertEqual(self.mirror_head(), self.new)

    def test_deleted_package_is_removed_without_a_rebuild_request(self):
        (self.primary / "emacs/extras/foo.el").unlink()
        self.git("add", "emacs/extras/foo.el")
        self.git("commit", "-qm", "delete foo", hooks=False)
        self.new = self.rev("HEAD")
        result = self.run_sync()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse((self.mirror / "emacs/extras/foo.el").exists())
        self.assertFalse((self.state / self.new / "foo.status").exists())
        self.assertEqual(self.wait_for_status(self.new, "bar"), "finished:test\n")

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

    def test_commit_on_master_moves_mirror_and_rebuilds(self):
        self.change("foo", "(defun foo-newer ())\n")
        head = self.rev("HEAD")
        self.assertEqual(self.mirror_head(), head)
        self.assertEqual(self.wait_for_status(head, "foo"), "finished:test\n")

    def test_commit_in_linked_worktree_leaves_mirror_alone(self):
        self.run_sync()
        before = self.mirror_head()
        worktree = self.root / "worktree"
        self.git("worktree", "add", "-q", "-b", "feature", str(worktree), hooks=False)
        self.change("foo", "(defun foo-feature ())\n", cwd=worktree)
        self.git("commit", "-q", "--amend", "-m", "amended", cwd=worktree)
        self.assertNotEqual(self.rev("HEAD", repo=worktree), before)
        self.assertEqual(self.mirror_head(), before)
        self.assertFalse((self.state / self.rev("HEAD", repo=worktree)).exists())

    def test_commit_on_other_branch_in_primary_leaves_mirror_at_master(self):
        self.run_sync()
        self.git("checkout", "-q", "-b", "feature", hooks=False)
        self.change("foo", "(defun foo-feature ())\n")
        self.assertEqual(self.mirror_head(), self.rev("master"))
        self.assertNotEqual(self.rev("HEAD"), self.rev("master"))

    def test_fast_forward_merge_of_worktree_branch_syncs_mirror(self):
        self.run_sync()
        worktree = self.root / "worktree"
        self.git("worktree", "add", "-q", "-b", "feature", str(worktree), hooks=False)
        self.change("foo", "(defun foo-feature ())\n", cwd=worktree)
        self.change("bar", "(defun bar-feature ())\n", cwd=worktree)
        self.assertEqual(self.mirror_head(), self.new)

        self.git("merge", "-q", "--ff-only", "feature")

        head = self.rev("master")
        self.assertEqual(head, self.rev("feature"))
        self.assertEqual(self.mirror_head(), head)
        for package in ("foo", "bar"):
            self.assertEqual(self.wait_for_status(head, package), "finished:test\n")

    def test_manual_run_in_primary_realigns_a_stray_mirror(self):
        stray_tree = self.root / "stray"
        self.git("worktree", "add", "-q", "-b", "stray", str(stray_tree), hooks=False)
        self.change("foo", "(defun foo-stray ())\n", cwd=stray_tree, hooks=False)
        self.git("fetch", "-q", str(self.primary), "stray", cwd=self.mirror)
        self.git("reset", "-q", "--hard", "FETCH_HEAD", cwd=self.mirror)
        self.assertEqual(self.mirror_head(), self.rev("stray"))

        result = subprocess.run(["sh", str(self.sync_hook)], text=True, capture_output=True,
                                check=False, cwd=self.primary, env=self.env)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.mirror_head(), self.rev("master"))
        self.assertEqual(self.wait_for_status(self.rev("master"), "foo"), "finished:test\n")

    def test_install_is_refused_from_a_linked_worktree(self):
        worktree = self.root / "worktree"
        self.git("worktree", "add", "-q", "-b", "feature", str(worktree), hooks=False)
        checker = worktree / "claude/bin/check-elpaca-sync-hook"
        checker.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(CHECK_SYNC_HOOK, checker)
        (worktree / "emacs").mkdir(exist_ok=True)
        shutil.copy2(DOTFILES / "emacs/config.org", worktree / "emacs/config.org")
        result = subprocess.run([str(checker), "--install"], text=True, capture_output=True,
                                check=False, env=self.env)
        self.assertEqual(result.returncode, 1)
        self.assertIn("primary checkout", result.stderr)
        self.assertFalse((self.primary / ".git/hooks/sync-elpaca-clone.sh").exists())

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
