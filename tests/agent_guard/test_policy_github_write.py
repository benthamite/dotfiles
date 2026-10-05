import json
import os
import subprocess
import sys
import tempfile
import unittest
from unittest import mock
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
        # Each element of a compound command is judged alone (CompoundCommandTests).
        self.assertDenied(self.decide(CLAUDE, "git push --dry-run && git push", self.foreign),
                          f"git push targets non-allowlisted repo {FOREIGN}")

    def test_unresolvable_directory_change(self):
        # A quoted `sh -c` body is not compound to the guard, so its `cd` is resolved.
        self.assertDenied(self.decide(CODEX, "sh -c ' cd a; cd b; git push'"), "git push target directory cannot be resolved")
        self.assertDenied(self.decide(CODEX, "sh -c ' cd $(pwd) && git push'"), "git push target directory cannot be resolved")
        self.assertDenied(self.decide(CODEX, "cd a && git push"), "GitHub write target directory cannot be resolved")

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


class RepositoryFixtures(unittest.TestCase):
    """Repositories whose origin is allowlisted (benthamite/* is in the
    committed allowlist), another allowlisted one, and a foreign one."""

    ALLOWED, OTHER = "benthamite/guard-test-scratch", "benthamite/guard-test-other"

    @classmethod
    def setUpClass(cls):
        cls.tmp = tempfile.TemporaryDirectory()
        base = Path(cls.tmp.name).resolve()
        cls.plain = str(base / "plain")
        os.makedirs(cls.plain)
        cls.allowed = str(base / "allowed")
        cls.spaced = str(base / "dir with spaces")
        cls.other = str(base / "other")
        cls.foreign = str(base / "foreign")
        for path, remote in ((cls.allowed, cls.ALLOWED), (cls.spaced, cls.ALLOWED), (cls.other, cls.OTHER),
                             (cls.foreign, FOREIGN)):
            subprocess.run(["git", "init", "-q", path], check=True, env=dict(os.environ, **GIT_ENV))
            subprocess.run(["git", "-C", path, "remote", "add", "origin", f"https://github.com/{remote}.git"], check=True)

    @classmethod
    def tearDownClass(cls):
        cls.tmp.cleanup()

    def decide(self, command, cwd=None, runtime=CLAUDE):
        cwd = cwd or self.plain
        previous = os.getcwd()
        os.chdir(cwd)
        try:
            return github_write.decide(call(runtime, command, cwd))
        finally:
            os.chdir(previous)

    def assertAllowed(self, command, cwd=None):
        for runtime in (CLAUDE, CODEX):
            with self.subTest(command=command, runtime=runtime):
                result = self.decide(command, cwd, runtime)
                self.assertEqual(result.decision, "allow", result.reason)

    def assertDenied(self, command, label, cwd=None):
        for runtime in (CLAUDE, CODEX):
            with self.subTest(command=command, runtime=runtime):
                result = self.decide(command, cwd, runtime)
                self.assertEqual(result.decision, "deny", command)
                self.assertTrue(result.reason.startswith(f"BLOCKED: {label}.\n\n"), result.reason)

    UNRESOLVED = "GitHub write target directory cannot be resolved"
    WHOLE = "compound GitHub command has no segment-local repository target"


class CompoundCommandTests(RepositoryFixtures):
    """A write in a compound command is bound to the nearest preceding literal
    `cd DIR` of its `&&` chain (2026-10-05). Before, `cd REPO && git push` and
    even `printf '… git push' | kill-ring-put` were denied as having no
    segment-local target."""

    def test_cd_and_push_binds_to_the_cd_target(self):
        self.assertAllowed(f"cd {self.allowed} && git fetch -q && git push")
        self.assertAllowed(f"cd {self.allowed} && git push 2>&1 | tail -3")
        self.assertDenied(f"cd {self.foreign} && git fetch -q && git push",
                          f"git push targets non-allowlisted repo {FOREIGN}")
        # The cd target decides, not the directory the command starts in.
        self.assertDenied(f"cd {self.foreign} && git push", f"git push targets non-allowlisted repo {FOREIGN}",
                          cwd=self.allowed)
        self.assertDenied(f"cd {self.foreign} && gh pr create --title T --body B",
                          f"gh pr write operation targets non-allowlisted repo {FOREIGN}", cwd=self.allowed)
        # Relative to the starting directory, and through an unquoted ~.
        self.assertAllowed("cd ../allowed && git push", cwd=self.foreign)
        base = str(Path(self.allowed).parent)
        with mock.patch.dict(os.environ, HOME=base):
            self.assertAllowed("cd ~/allowed && git push")
            # A quoted ~ is a literal directory name, not HOME.
            self.assertDenied("cd '~/allowed' && git push", self.UNRESOLVED)

    def test_quoted_directory_with_spaces(self):
        self.assertAllowed(f'cd "{self.spaced}" && git push')
        self.assertAllowed(f"cd '{self.spaced}' && git push")
        self.assertAllowed("cd " + self.spaced.replace(" ", "\\ ") + " && git push")

    def test_successive_cds_bind_to_the_last(self):
        self.assertDenied(f"cd {self.allowed} && cd {self.foreign} && git push",
                          f"git push targets non-allowlisted repo {FOREIGN}")
        self.assertAllowed(f"cd {self.foreign} && cd {self.allowed} && git push")
        self.assertAllowed(f"cd {self.foreign} && cd ../allowed && git push")
        # An unresolvable later cd leaves the directory unknown.
        self.assertDenied(f"cd {self.allowed} && cd $X && git push", self.UNRESOLVED)
        self.assertDenied(f"cd {self.allowed} && cd /nonexistent/zz && git push", self.UNRESOLVED)
        self.assertDenied(f"cd {self.allowed} && cd - && git push", self.UNRESOLVED)
        self.assertDenied(f"cd {self.allowed} && CDPATH=/x cd sub && git push", self.UNRESOLVED)

    def test_scope_ends_outside_the_and_chain(self):
        for connector in (";", "\n", " || ", " & "):
            command = f"cd {self.allowed}{connector}git push"
            self.assertDenied(command, self.UNRESOLVED)
        self.assertDenied(f"cd {self.allowed} && git fetch; git push", self.UNRESOLVED)
        # A command that may change directory (here a shell function) ends it too.
        self.assertDenied(f"cd {self.allowed} && z foreign && git push", self.UNRESOLVED)
        # Without any cd, the command's own directory stays known across `;`.
        self.assertAllowed("git fetch; git push", cwd=self.allowed)

    def test_subshells_and_stateful_commands_keep_the_whole_command_rule(self):
        self.assertDenied(f"(cd {self.allowed} && git push)", self.WHOLE)
        self.assertDenied(f"(cd {self.allowed}) && git push", self.WHOLE, cwd=self.foreign)
        self.assertDenied(f"{{ cd {self.allowed} && git push; }}", self.WHOLE)
        self.assertDenied(f"cd {self.allowed} && export GIT_DIR={self.foreign}/.git && git push", self.WHOLE)
        self.assertDenied(f"pushd {self.allowed} && git push", self.WHOLE)
        self.assertDenied(f"cd {self.allowed} | git push", self.WHOLE, cwd=self.foreign)

    def test_writes_to_different_repositories_are_denied(self):
        label = "compound GitHub command writes to more than one repository"
        self.assertDenied(f"cd {self.allowed} && git push && cd {self.other} && git push", label)
        self.assertDenied(f"cd {self.allowed} && git push && gh pr create -R {self.OTHER} --title T --body B", label)
        self.assertAllowed(f"cd {self.allowed} && git push && gh pr create -R {self.ALLOWED} --title T --body B")

    def test_explicit_targets_stay_authoritative(self):
        self.assertDenied(f"cd {self.allowed} && git -C {self.foreign} push",
                          f"git push targets non-allowlisted repo {FOREIGN}")
        self.assertAllowed(f"cd {self.foreign} && git -C {self.allowed} push")
        self.assertAllowed(f"cd {self.foreign} && git -C ../allowed push")
        self.assertDenied(f"cd {self.allowed} && gh pr create -R {FOREIGN} --title T --body B",
                          f"gh pr write operation targets non-allowlisted repo {FOREIGN}")
        self.assertDenied(f"cd {self.allowed} && git push https://github.com/{FOREIGN}.git HEAD:x",
                          f"git push targets non-allowlisted repo {FOREIGN}")

    def test_text_only_stages(self):
        # A quoted mention of a write, piped to the kill ring, runs nothing.
        self.assertAllowed(f"printf '%s\\n' 'cd {self.foreign} && git push' | kill-ring-put", cwd=self.foreign)
        self.assertAllowed('echo "gh pr create --repo no-such-owner-zz/repo" | pbcopy && echo done', cwd=self.foreign)
        self.assertAllowed(f"cd {self.allowed} && git push && printf 'pushed\\n'")
        # A substitution, an interpreter downstream, or a file written is not
        # text-only (HiddenCommandTests covers writes with no space before them).
        self.assertDenied("printf '%s' \"$( git push)\" | kill-ring-put", self.WHOLE, cwd=self.foreign)
        self.assertDenied("printf '%s' \"$(git push)\" | kill-ring-put", self.WHOLE, cwd=self.allowed)
        self.assertDenied("printf '%s' $( git push) | kill-ring-put", self.WHOLE, cwd=self.foreign)
        self.assertDenied("printf '%s' ` git push` | kill-ring-put", self.WHOLE, cwd=self.foreign)
        self.assertDenied("printf '%s' <( git push) | kill-ring-put", self.WHOLE, cwd=self.foreign)
        for interpreter in ("sh", "bash", "zsh", "python3", "eval", "xargs sh -c"):
            with self.subTest(interpreter=interpreter):
                self.assertDenied(f"printf 'x && git push\\n' | {interpreter}", self.WHOLE, cwd=self.allowed)
        self.assertFalse(github_write._text_only(github_write.Segment(["printf 'git push' > run.sh"], None)))
        self.assertTrue(github_write._text_only(github_write.Segment(["printf 'git push' 2>&1", "kill-ring-put"], None)))
        self.assertFalse(github_write._text_only(github_write.Segment(["printf -v X 'git push'"], None)))

    def test_shell_segments(self):
        split = github_write.shell_segments
        self.assertEqual([(s.stages, s.connector) for s in split('cd "a b" && git push 2>&1 | tail -1; ls\nx &')],
                         [(['cd "a b"'], "&&"), (["git push 2>&1", "tail -1"], ";"), (["ls"], ";"), (["x"], "&")])
        self.assertEqual([s.stages for s in split("echo 'a && b | c'")], [["echo 'a && b | c'"]])
        for command in ("a $(b)", 'a "$(b)"', "a `b`", "(a)", "{ a; }", "a <(b)", "cat <<EOF", "a # c", "a |",
                        "| a", "a && && b", "$'a'", "'a", "a \\\nb", "case x in a) ;; esac"):
            with self.subTest(command=command):
                self.assertIsNone(split(command))


class HiddenCommandTests(RepositoryFixtures):
    """A write hidden in a substitution, an `sh -c` string, eval, xargs or text
    fed to a shell is judged like a plain write (2026-10-05). The write
    patterns need a separator before `git` or `gh`, so before, `"$(git push)"`,
    `eval "git push"` or `printf 'git push' | sh` passed unchecked."""

    NOT_ALLOWED = f"git push targets non-allowlisted repo {FOREIGN}"
    PR_NOT_ALLOWED = f"gh pr write operation targets non-allowlisted repo {FOREIGN}"

    def assertJudgedLikeAPlainWrite(self, command, label=None):
        """Denied where the directory's repository is not allowlisted, and
        allowed where it is, as a plain `git push` is."""
        self.assertDenied(command, label or self.NOT_ALLOWED, cwd=self.foreign)
        self.assertAllowed(command, cwd=self.allowed)

    def test_command_and_process_substitutions(self):
        for command in ('"$(git push)"', 'echo "$(git push)"', "echo $(git push)", "`git push`", "echo `git push`",
                        "echo $(echo $(git push))", "cat <(git push)", "tee >(git push) </dev/null",
                        'echo "${X:-$(git push)}"'):
            self.assertJudgedLikeAPlainWrite(command)
        self.assertJudgedLikeAPlainWrite("x=$(gh pr create --title T --body B)", self.PR_NOT_ALLOWED)

    def test_shell_strings_eval_and_xargs(self):
        for command in ("bash -c 'git push'", 'sh -c "git push"', "bash -lc 'git push'", "nohup bash -c 'git push'",
                        "env FOO=1 bash -c 'git push'", 'eval "git push"', "eval git push", 'command eval "git push"',
                        "eval $'git\\x20push'", "xargs git push", 'bash -c "$(printf \'git push\')"',
                        "bash <<< 'git push'", "source <(echo 'git push')"):
            self.assertJudgedLikeAPlainWrite(command)
        self.assertJudgedLikeAPlainWrite("zsh -c 'gh pr create --title T --body B'", self.PR_NOT_ALLOWED)

    def test_shell_string_binds_its_own_cd(self):
        # Before, a `cd` right after the quote was missed and the start directory decided.
        self.assertDenied(f'sh -c "cd {self.foreign} && git push"', self.NOT_ALLOWED, cwd=self.allowed)
        self.assertDenied(f"bash -c 'cd {self.foreign} && git push'", self.NOT_ALLOWED, cwd=self.allowed)
        self.assertAllowed(f'sh -c "cd {self.allowed} && git push"', cwd=self.allowed)
        self.assertDenied('sh -c "cd nonexistent-zz && git push"', self.UNRESOLVED, cwd=self.allowed)

    def test_text_fed_to_an_interpreter(self):
        # A pipeline into an interpreter has no segment-local target, wherever it runs.
        for command in ("printf 'git push' | sh", 'echo "gh pr create --title T --body B" | bash',
                        "printf 'git push' | sh -s", "echo 'git push' | zsh", "echo 'git push' | source /dev/stdin",
                        "printf 'push' | xargs git", "echo x | xargs git push", "sh <<EOF\ngit push\nEOF",
                        "cat <<EOF\n$(git push)\nEOF"):
            self.assertDenied(command, self.WHOLE, cwd=self.allowed)

    def test_writes_to_two_repositories(self):
        self.assertDenied(f'echo "$(git -C {self.allowed} push) $(git -C {self.other} push)"',
                          "GitHub command writes to more than one repository", cwd=self.allowed)

    def test_wrappers_the_guard_cannot_follow_fail_closed(self):
        for command in ("env -C /tmp bash -c 'git push'", "env -S 'git push'", "sudo -D /tmp sh -c 'git push'"):
            self.assertDenied(command, "GitHub write in shell text the guard cannot parse", cwd=self.allowed)

    def test_mentions_used_as_data_stay_allowed(self):
        for command in ("printf '%s' \"git push\" | kill-ring-put", 'grep -rn "git push" .',
                        "grep -n '$(git push)' file", 'rg "\\`git push\\`" docs', 'git commit -m "git push later"',
                        'git commit -m "Explain why \\`git push\\` is gated"', 'echo "it\'s \\$(git push)"',
                        "cat <<'EOF'\n$(git push)\nEOF", 'echo "a $(date) b" # $(git push) in a comment',
                        "echo 'eval \"git push\"' | pbcopy", "echo $((1<<2)) && git status", "bash -c 'git status'",
                        "echo 'gh pr list' | sh"):
            self.assertAllowed(command, cwd=self.foreign)

    def test_hidden_commands(self):
        hidden = github_write.hidden_commands
        self.assertEqual(hidden('a "$(b c)" `d` <(e) "${X:-$(f)}"'), ["b c", "d", "e", "f"])
        self.assertEqual(hidden("sh -c 'a; b' x"), ["a; b", "x"])
        self.assertEqual(hidden("printf 'a\\nb' | bash"), ["a\nb"])
        self.assertEqual(hidden("echo y | xargs -I{} git {} -q"), ["git y -q"])
        self.assertEqual(hidden("bash <<'EOF'\nx $(y)\nEOF"), ["x $(y)\n"])
        self.assertEqual(hidden("grep '$(a)' \"\\$(b)\" # $(c)"), [])
        for command in ("a 'b", 'a "$(b', "a `b", "env -C /x b", "cat <<EOF"):
            with self.subTest(command=command), self.assertRaises(github_write._Unparsed):
                hidden(command)


if __name__ == "__main__":
    unittest.main()
