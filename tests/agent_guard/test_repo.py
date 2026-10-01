import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard import repo  # noqa: E402
from agent_guard.core import CLAUDE, CODEX  # noqa: E402

GIT_ENV = {"GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t", "GIT_COMMITTER_NAME": "t",
           "GIT_COMMITTER_EMAIL": "t@t", "GIT_CONFIG_GLOBAL": os.devnull, "GIT_CONFIG_NOSYSTEM": "1"}


class CdTargetTests(unittest.TestCase):
    def test_last_cd_head_wins(self):
        self.assertEqual(repo.cd_target("cd a && x; cd b", "/h"), ("b", False))
        self.assertEqual(repo.cd_target("{ cd a; git x; } 2>&1", "/h"), ("a", False))
        self.assertEqual(repo.cd_target("cd a\n( cd 'b c' && y )", "/h"), ("b c", False))
        self.assertEqual(repo.cd_target("cd a && cd ''", "/h"), ("", False))
        self.assertEqual(repo.cd_target("echo cd a", "/h"), ("", False))

    def test_unquoted_target_stops_at_space_and_operators(self):
        self.assertEqual(repo.cd_target("cd a&&b", "/h"), ("a", False))
        self.assertEqual(repo.cd_target("cd a\u00a0b", "/h"), ("a", False))
        self.assertEqual(repo.cd_target("cd\u3000a", "/h"), ("a", False))
        self.assertEqual(repo.cd_target("cdr1", "/h"), ("", False))

    def test_home_expansion(self):
        self.assertEqual(repo.cd_target("cd ~/x", "/h"), ("/h/x", False))
        self.assertEqual(repo.cd_target('cd "$HOME/x"', "/h"), ("/h/x", False))
        self.assertEqual(repo.cd_target("cd ${HOME}/x", "/h"), ("/h/x", False))
        self.assertEqual(repo.cd_target("cd '~/x'", "/h"), ("~/x", False))

    def test_substitution_discarded_other_variables_unresolvable(self):
        self.assertEqual(repo.cd_target('cd "$(pwd)"', "/h"), ("", False))
        self.assertEqual(repo.cd_target("cd `pwd`", "/h"), ("", False))
        self.assertEqual(repo.cd_target("cd $REPO", "/h"), ("", True))

    def test_large_commands_cost_linear_time(self):
        # The first port sliced the remainder at every head: 1.4 s CPU here.
        import time
        command = "cd /tmp\n" + "x\n" * 200_000
        start = time.process_time()
        self.assertEqual(repo.cd_target(command, "/h"), ("/tmp", False))
        self.assertLess(time.process_time() - start, 0.5)


class RepoTestCase(unittest.TestCase):
    def setUp(self):
        self.old_cwd = os.getcwd()
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(os.path.realpath(self.tmp.name))
        self.env = dict(os.environ, **GIT_ENV, PWD=str(self.dir), HOME=str(self.dir))
        for name in ("one", "two"):
            self.git("init", "-q", name)
        (self.dir / "one/sub").mkdir()
        (self.dir / "plain").mkdir()
        os.chdir(self.dir)

    def tearDown(self):
        os.chdir(self.old_cwd)
        self.tmp.cleanup()

    def git(self, *args, cwd=None):
        subprocess.run(["git", *args], cwd=cwd or self.dir, env=self.env, check=True, capture_output=True)


class RepoRootTests(RepoTestCase):
    def test_cd_target_decides(self):
        ctx = repo.repo_root("cd one/sub && git status", {"cwd": "two"}, CLAUDE, self.env)
        self.assertEqual((ctx.root, ctx.command_context, ctx.cwd), (str(self.dir / "one"), "one/sub", str(self.dir / "one")))

    def test_unresolvable_or_non_repository_target_gives_no_repository(self):
        for command in ("cd $X && git commit", "cd plain && git commit", "cd missing && git commit"):
            with self.subTest(command=command):
                ctx = repo.repo_root(command, {"cwd": "two"}, CLAUDE, self.env)
                self.assertEqual((ctx.root, ctx.cwd), ("", str(self.dir)))

    def test_structured_workdir_then_process_directory(self):
        payload = {"tool_input": {"command": "x", "workdir": False, "cwd": "two"}, "cwd": "one"}
        self.assertEqual(repo.repo_root("x", payload, CLAUDE, self.env).root, str(self.dir / "two"))
        payload = {"tool_input": '{"working_dir": "one"}'}
        self.assertEqual(repo.repo_root("x", payload, CLAUDE, self.env).root, str(self.dir / "one"))
        ctx = repo.repo_root("x", {"tool_input": "not json"}, CLAUDE, self.env)
        self.assertEqual((ctx.root, ctx.command_context), ("", str(self.dir)))

    def test_repo_context_dir_overrides_payload(self):
        env = dict(self.env, REPO_CONTEXT_DIR="one")
        self.assertEqual(repo.repo_root("x", {"cwd": "two"}, CLAUDE, env).root, str(self.dir / "one"))

    def test_codex_follows_git_global_options_of_the_commit(self):
        command = "cd one && git -C ../two commit -m x"
        self.assertEqual(repo.repo_root(command, {}, CODEX, self.env).root, str(self.dir / "two"))
        self.assertEqual(repo.repo_root(command, {}, CLAUDE, self.env).root, str(self.dir / "one"))


class StagedFilesTests(RepoTestCase):
    def setUp(self):
        super().setUp()
        self.repo = self.dir / "one"
        (self.repo / "a.txt").write_text("a\n")
        self.git("add", "a.txt", cwd=self.repo)
        self.git("commit", "-q", "-m", "first", cwd=self.repo)
        (self.repo / "b.txt").write_text("b\n")
        (self.repo / "c.el").write_text("(c)\n")
        self.git("add", "b.txt", "c.el", cwd=self.repo)

    def staged(self, command, runtime=CLAUDE, target=None):
        command = f"cd '{target or self.repo}' && {command}"
        ctx = repo.repo_root(command, {}, runtime, self.env)
        return repo.staged_files(command, ctx, runtime, self.env)

    def test_index_commit_lists_staged_files(self):
        staged = self.staged("git commit -m x")
        self.assertEqual((staged.files, staged.status, staged.base, staged.selection, staged.deny_reason),
                         ("b.txt\nc.el", "A\tb.txt\nA\tc.el", "", False, ""))

    def test_amend_compares_against_head_parent_when_it_exists(self):
        self.assertEqual(self.staged("git commit --amend --no-edit").base, "")
        self.git("commit", "-q", "-m", "second", cwd=self.repo)
        (self.repo / "a.txt").write_text("changed\n")
        self.git("add", "a.txt", cwd=self.repo)
        staged = self.staged("git commit --amend --no-edit")
        self.assertEqual((staged.files, staged.base), ("a.txt\nb.txt\nc.el", "HEAD~1"))

    def test_explicit_paths_use_the_selection_helper(self):
        staged = self.staged("git commit -m x c.el", CODEX)
        self.assertTrue(staged.selection)
        self.assertEqual((staged.files, staged.status, staged.base), ("c.el", "A\tc.el", ""))
        self.assertEqual(list(staged.elisp_diffs), ["c.el"])
        self.assertEqual(staged.manual_contents, {})

    def test_relative_cd_target_is_resolved_from_the_hook_directory(self):
        # Intended difference: the shell library resolved it from the
        # repository it had cd'd into and denied every such commit.
        staged = self.staged("git commit -m x c.el", target="one")
        self.assertEqual((staged.deny_reason, staged.selection, staged.files), ("", True, "c.el"))
        ctx = repo.repo_root("cd one/sub && git commit -m x ../c.el", {}, CLAUDE, self.env)
        staged = repo.staged_files("cd one/sub && git commit -m x ../c.el", ctx, CLAUDE, self.env)
        self.assertEqual((staged.deny_reason, staged.files), ("", "c.el"))

    def test_selection_error_becomes_the_denial(self):
        staged = self.staged("git commit -m x missing.txt")
        self.assertTrue(staged.deny_reason.startswith("BLOCKED: cannot determine proposed commit files"))


if __name__ == "__main__":
    unittest.main()
