import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import CLAUDE, CODEX, Call, HookFailure  # noqa: E402
from agent_guard.policies import github_write  # noqa: E402

GIT_ENV = {"GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t", "GIT_COMMITTER_NAME": "t",
           "GIT_COMMITTER_EMAIL": "t@t", "GIT_CONFIG_GLOBAL": os.devnull, "GIT_CONFIG_NOSYSTEM": "1"}
FOREIGN = "no-such-owner-zz/repo"


def call(runtime, command, cwd, tool=None, **fields):
    key = "command" if runtime == CLAUDE else "cmd"
    payload = {"tool_name": tool or ("Bash" if runtime == CLAUDE else "exec_command"),
               "tool_input": {key: command, **fields}, "cwd": cwd}
    return Call.from_payload(runtime, payload, env=dict(os.environ, **GIT_ENV, PWD=cwd))


class PureHelperTests(unittest.TestCase):
    def test_normalize_repo(self):
        self.assertEqual(github_write.normalize_repo("https://github.com/Foo/Bar.git"), "foo/bar")
        self.assertEqual(github_write.normalize_repo("git@github.com:a/b/"), "a/b")
        self.assertEqual(github_write.normalize_repo("a/b.git.git"), "a/b.git")

    def test_repo_from_urlish(self):
        self.assertEqual(github_write.repo_from_urlish("push ssh://git@github.com/O/R.git x"), "o/r")
        self.assertEqual(github_write.repo_from_urlish("gitlab.com/o/r"), "")

    def test_compound(self):
        compound = github_write.compound_shell_command
        self.assertTrue(compound("gh pr create && echo"))
        self.assertTrue(compound("git push\nls"))
        self.assertTrue(compound("git push | sed x"))
        self.assertFalse(compound("gh pr create 2>&1 | tail -1"))
        self.assertFalse(compound("echo 'a; b' \"c | d\""))
        self.assertFalse(compound("git push >&2"))
        self.assertFalse(compound("git push\rls"))

    def test_broker_pipeline(self):
        repo = github_write.broker_secret_pipeline_repo
        self.assertEqual(repo("op-desktop read op://v/i/f | gh secret set NAME -R Foo/Bar"), "foo/bar")
        self.assertEqual(repo("op-automations @epoch read -n op://v/i/f | gh secret set N --repo=a/b"), "a/b")
        self.assertEqual(repo("op-automations read --account x op://v/i/f | gh secret set N -R a/b"), "")
        self.assertEqual(repo("op-desktop read op://v/i/f | gh secret set N -R a/b --env x"), "")
        self.assertEqual(repo("op-desktop read op://v/i/f | gh secret set N M -R a/b"), "")
        self.assertEqual(repo("op-desktop read op://v/i/f; gh secret set N -R a/b"), "")


class DecideTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tmp = tempfile.TemporaryDirectory()
        base = Path(cls.tmp.name).resolve()
        cls.foreign = str(base / "foreign")
        cls.bare = str(base / "bare")
        cls.plain = str(base / "plain")
        os.makedirs(cls.plain)
        for path, remote in ((cls.foreign, f"https://github.com/{FOREIGN}.git"), (cls.bare, None)):
            subprocess.run(["git", "init", "-q", path], check=True, env=dict(os.environ, **GIT_ENV))
            if remote:
                subprocess.run(["git", "-C", path, "remote", "add", "origin", remote], check=True)

    @classmethod
    def tearDownClass(cls):
        cls.tmp.cleanup()

    def decide(self, runtime, command, cwd=None, **kwargs):
        cwd = cwd or self.plain
        previous = os.getcwd()
        os.chdir(cwd)
        try:
            return github_write.decide(call(runtime, command, cwd, **kwargs))
        finally:
            os.chdir(previous)

    def assertDenied(self, result, label):
        self.assertEqual(result.decision, "deny")
        self.assertTrue(result.reason.startswith(f"BLOCKED: {label}.\n\n"), result.reason)
        self.assertIn("agents/github-operation-authorizations.md", result.reason)

    def test_reads_and_non_shell_tools_allowed(self):
        for runtime in (CLAUDE, CODEX):
            for command in ("gh pr list", "git status", "gh api user", "echo 'gh pr create'",
                            "gh api graphql -f query='query{viewer{login}}'", "git commit -m 'git push'"):
                self.assertEqual(self.decide(runtime, command, self.foreign).decision, "allow", command)
            self.assertEqual(self.decide(runtime, "git push", self.foreign, tool="Read").decision, "allow")

    def test_push_to_foreign_remote_denied(self):
        for runtime in (CLAUDE, CODEX):
            result = self.decide(runtime, "git push", self.foreign)
            self.assertDenied(result, f"git push targets non-allowlisted repo {FOREIGN}")
            self.assertDenied(self.decide(runtime, f"git -C {self.foreign} push"),
                              f"git push targets non-allowlisted repo {FOREIGN}")
            self.assertDenied(self.decide(runtime, f"sh -c ' cd {self.foreign} && /usr/bin/git push'"),
                              f"git push targets non-allowlisted repo {FOREIGN}")
            self.assertDenied(self.decide(runtime, "sh -c 'git push'", self.foreign),
                              f"git push targets non-allowlisted repo {FOREIGN}")

    def test_dry_run_allowed_unless_compound(self):
        self.assertEqual(self.decide(CLAUDE, "git push --dry-run", self.foreign).decision, "allow")
        self.assertDenied(self.decide(CLAUDE, "git push --dry-run && git push", self.foreign),
                          "compound GitHub command has no segment-local repository target")

    def test_unresolvable_directory_change(self):
        # A quoted `sh -c` body is not compound to the guard, so its `cd` is resolved.
        self.assertDenied(self.decide(CODEX, "sh -c ' cd a; cd b; git push'"), "git push target directory cannot be resolved")
        self.assertDenied(self.decide(CODEX, "sh -c ' cd $(pwd) && git push'"), "git push target directory cannot be resolved")
        self.assertDenied(self.decide(CODEX, "cd a && git push"), "compound GitHub command has no segment-local repository target")

    def test_ambiguous_target_wording_differs_by_runtime(self):
        claude = self.decide(CLAUDE, "gh pr comment 1", self.bare)
        codex = self.decide(CODEX, "gh pr comment 1", self.bare)
        self.assertDenied(claude, "gh pr write operation has no unambiguous repository target")
        self.assertIn("only if Pablo personally created it", claude.reason)
        self.assertIn("only after Pablo explicitly authorizes", codex.reason)

    def test_explicit_targets(self):
        label = f"gh issue write operation targets non-allowlisted repo {FOREIGN}"
        self.assertDenied(self.decide(CLAUDE, f"gh issue close 3 -R {FOREIGN}"), label)
        self.assertDenied(self.decide(CLAUDE, f'GH_REPO="{FOREIGN}" gh issue close 3'), label)
        self.assertDenied(self.decide(CLAUDE, f"gh api repos/{FOREIGN}/issues -f title=x"),
                          f"gh api write operation targets non-allowlisted repo {FOREIGN}")
        self.assertEqual(self.decide(CLAUDE, f"gh api repos/{FOREIGN} --method GET -f x=1").decision, "allow")

    def test_operations_no_allowlist_can_authorize(self):
        self.assertDenied(self.decide(CLAUDE, "gh gist create f"), "gh gist write operation")
        self.assertDenied(self.decide(CLAUDE, "gh repo fork a/b"), "non-repository-scoped gh write operation")
        self.assertDenied(self.decide(CLAUDE, "gh secret set X --org o"),
                          "organization/environment/app GitHub secret or variable mutation")
        self.assertDenied(self.decide(CLAUDE, "gh repo create", self.foreign),
                          "gh repo create without an explicit OWNER/REPO target")

    def test_set_e_exits_keep_their_status(self):
        # `[ -n "$repo" ] || repo=$(target_repo_for_gh)` with no target; Codex's EXIT trap made it 2.
        for runtime, status in ((CLAUDE, 1), (CODEX, 2)):
            with self.assertRaises(HookFailure) as raised:
                self.decide(runtime, "gh repo edit", self.plain)
            self.assertEqual(raised.exception.status, status)

    def test_unset_home_fails_with_the_shell_status(self):
        for runtime, status in ((CLAUDE, 1), (CODEX, 2)):
            env = {key: value for key, value in os.environ.items() if key != "HOME"}
            with self.assertRaises(HookFailure) as raised:
                github_write.decide(Call.from_payload(runtime, {"tool_name": "Bash" if runtime == CLAUDE else "exec_command",
                                                                "tool_input": {"command": "gh pr comment 1", "cmd": "gh pr comment 1"},
                                                                "cwd": self.plain}, env=env))
            self.assertEqual(raised.exception.status, status)
        # An empty command exits before $HOME is read.
        env = {key: value for key, value in os.environ.items() if key != "HOME"}
        payload = {"tool_name": "Bash", "tool_input": {"command": ""}, "cwd": self.plain}
        self.assertEqual(github_write.decide(Call.from_payload(CLAUDE, payload, env=env)).decision, "allow")

    def test_functions_exec_nested_calls(self):
        def exec_call(code, cwd=self.plain):
            payload = {"tool_name": "functions.exec", "tool_input": {"input": code}, "cwd": cwd}
            return github_write.decide(Call.from_payload(CODEX, payload, env=dict(os.environ, **GIT_ENV, PWD=cwd)))

        before = os.getcwd()
        self.assertDenied(exec_call(f'await tools.exec_command({{cmd: "git push", workdir: {json.dumps(self.foreign)}}})'),
                          f"git push targets non-allowlisted repo {FOREIGN}")
        self.assertEqual(os.getcwd(), before)
        self.assertDenied(exec_call('const c = "x"; await tools.exec_command({cmd: c})'),
                          "functions.exec contains an ambiguous nested exec_command")
        self.assertDenied(exec_call('await tools.exec_command({cmd: "ls", workdir: "/nonexistent/zz"})'),
                          "functions.exec nested workdir cannot be resolved")
        self.assertEqual(exec_call('await tools.exec_command({cmd: "git status"})').decision, "allow")


if __name__ == "__main__":
    unittest.main()
