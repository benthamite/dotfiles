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

from agent_guard import codex_payload as cp  # noqa: E402
from agent_guard.core import HookFailure  # noqa: E402


class PayloadFieldTests(unittest.TestCase):
    def test_jq_raw_renders_non_strings_as_pretty_json(self):
        self.assertEqual(cp.tool_name({"tool_name": 5}), "5")
        self.assertEqual(cp.tool_name({"tool_name": True}), "true")
        self.assertEqual(cp.tool_name({"tool_name": {"a": [1, "x\x7f"]}}),
                         '{\n  "a": [\n    1,\n    "x\\u007f"\n  ]\n}')
        self.assertEqual(cp.tool_name({"tool_name": False}), "")
        self.assertEqual(cp.session_id({}), "")

    def test_shell_command_uses_jq_alternative_semantics(self):
        self.assertEqual(cp.shell_command({"tool_input": {"command": "", "cmd": "ls"}}), "")
        self.assertEqual(cp.shell_command({"tool_input": {"command": False, "cmd": "ls"}}), "ls")
        self.assertEqual(cp.shell_command({"tool_input": {"command": 0, "cmd": "ls"}}), "0")
        self.assertEqual(cp.shell_command({"tool_input": {"input": "x"}}), "x")
        self.assertEqual(cp.shell_command({"tool_input": '{"cmd": "decoded"}'}), "decoded")
        self.assertEqual(cp.shell_command({"tool_input": "raw text"}), "raw text")
        self.assertEqual(cp.shell_command({"tool_input": "Bash\n\n"}), "Bash\n\n")

    def test_tool_input_field(self):
        self.assertEqual(cp.tool_input_field({"tool_input": {"path": "/x"}}, "path"), "/x")
        self.assertEqual(cp.tool_input_field({"tool_input": {"path": None}}, "path"), "")

    def test_tool_response_joins_text_chunks(self):
        payload = {"tool_response": [{"text": "a"}, {"text": False, "output": 7}, "b", 5, None, {"text": True}]}
        self.assertEqual(cp.tool_response(payload), {"output": "a7btrue"})
        self.assertEqual(cp.tool_response({"tool_response": '{"output": "j"}'}), {"output": "j"})
        self.assertEqual(cp.tool_response({"tool_response": "[1]"}), {"output": "[1]"})
        self.assertEqual(cp.tool_response({"tool_response": 3}), {})
        with self.assertRaises(HookFailure):
            cp.tool_response({"tool_response": [{"output": {"x": 1}}]})

    def test_shell_tool_names(self):
        for name in ("Bash", "exec_command", "functions.exec", "functions.exec_command"):
            self.assertTrue(cp.is_shell_tool(name))
        self.assertFalse(cp.is_shell_tool("apply_patch"))

    def test_command_substitution(self):
        self.assertEqual(cp.command_substitution("a\0b\n\n"), "ab")
        self.assertEqual(cp.command_substitution("a\n \n"), "a\n ")


class NestedExecTests(unittest.TestCase):
    def test_literal_call_context(self):
        source = 'await tools.exec_command({cmd: "ls", workdir: "/tmp"})'
        self.assertEqual(cp.nested_exec_contexts(source), [{"cmd": "ls", "workdir": "/tmp", "ambiguous": False}])
        self.assertEqual(cp.nested_exec_commands(source), ["ls"])

    def test_bound_object_and_unresolvable_references(self):
        source = 'const o = {cmd: "x", cwd: "/w"}; await tools.exec_command(o); tools["exec_command"]({})'
        self.assertEqual(cp.nested_exec_contexts(source), [
            {"cmd": "x", "workdir": "/w", "ambiguous": False},
            {"cmd": None, "workdir": None, "ambiguous": True},
        ])
        self.assertEqual(cp.nested_exec_commands(source), ["x"])

    def test_dynamic_values_are_ambiguous(self):
        for source in ('tools.exec_command({cmd: "a" + b})', "tools.exec_command({cmd: `ls ${d}`})",
                       'tools.exec_command({...base, cmd: "x"})', 'tools.exec_command({cmd: "x", cwd: "/a", workdir: "/b"})'):
            self.assertTrue(cp.nested_exec_contexts(source)[0]["ambiguous"], source)

    def test_escapes_and_raw_carriage_returns(self):
        source = 'tools.exec_command({cmd: "\\x41\\u0042\\ud83d\\ude00\r\nraw\rcr"})'
        self.assertEqual(cp.nested_exec_commands(source), ["AB\U0001F600\r\nraw\rcr"])

    def test_failure_keeps_values_printed_before_it(self):
        source = 'tools.exec_command({cmd: "ok"}); tools.exec_command({cmd: "a\\u0000b"})'
        with self.assertRaises(cp.PartialOutput) as caught:
            cp.nested_exec_contexts(source)
        self.assertEqual(caught.exception.values, [{"cmd": "ok", "workdir": None, "ambiguous": False}])
        with self.assertRaises(cp.PartialOutput):
            cp.nested_exec_commands('tools.exec_command({cmd: "\\ud800"})')

    def test_literal_wrapper(self):
        self.assertTrue(cp.literal_exec_wrapper('text(await tools.exec_command({"cmd": "ls", "max_output_tokens": 5}));'))
        self.assertTrue(cp.literal_exec_wrapper('await tools.exec_command({cmd: "ls", workdir: "/t"}) \r\n'))
        for source in ('await tools.exec_command({"cmd": "ls", "max_output_tokens": true})',
                       'await tools.exec_command({"cmd": "a", "cmd": "b"})',
                       'await tools.exec_command({"cmd": "ls"}); more()',
                       'await tools.exec_command({"workdir": "/t"})'):
            self.assertFalse(cp.literal_exec_wrapper(source), source)


class ParentExecWorkdirTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.dirs = [os.path.join(self.tmp.name, name) for name in ("d1", "d2")]
        for directory in self.dirs:
            os.mkdir(directory)

    def transcript(self, *programs, trailer=()):
        path = os.path.join(self.tmp.name, "t.jsonl")
        lines = [json.dumps({"payload": {"type": "custom_tool_call", "name": "exec", "input": p}}) for p in programs]
        with open(path, "w") as f:
            f.write("\n".join(lines + list(trailer)) + "\n")
        return {"transcript_path": path}

    def call(self, workdir, cmd="ls", extra=""):
        return f'tools.exec_command({{cmd: "{cmd}", workdir: "{workdir}"{extra}}});'

    def test_newest_program_naming_the_command_wins(self):
        payload = self.transcript(self.call(self.dirs[0]), self.call(self.dirs[1]))
        self.assertEqual(cp.parent_exec_workdir(payload, "ls"), self.dirs[1])
        self.assertIsNone(cp.parent_exec_workdir(payload, "other"))

    def test_conflicting_or_missing_workdirs_resolve_nothing(self):
        payload = self.transcript(self.call(self.dirs[0]) + self.call(self.dirs[1]))
        self.assertIsNone(cp.parent_exec_workdir(payload, "ls"))
        payload = self.transcript(self.call(self.dirs[0]), self.call("/nonexistent/x"))
        self.assertIsNone(cp.parent_exec_workdir(payload, "ls"))

    def test_trailing_newlines_compare_as_command_substitution_saw_them(self):
        payload = self.transcript(self.call(self.dirs[0] + "\\n", cmd="ls\\n"))
        self.assertEqual(cp.parent_exec_workdir(payload, "ls"), self.dirs[0])

    def test_only_last_64_lines_and_unhandled_events_stop_the_search(self):
        self.assertIsNone(cp.parent_exec_workdir(self.transcript(self.call(self.dirs[0]), trailer=["{}"] * 64), "ls"))
        self.assertEqual(cp.parent_exec_workdir(self.transcript(self.call(self.dirs[0]), trailer=["{}"] * 63), "ls"),
                         self.dirs[0])
        self.assertIsNone(cp.parent_exec_workdir(self.transcript(self.call(self.dirs[0]), trailer=["[1]"]), "ls"))
        self.assertEqual(cp.parent_exec_workdir(self.transcript(self.call(self.dirs[0]), trailer=["junk"]), "ls"),
                         self.dirs[0])

    def test_no_transcript(self):
        self.assertIsNone(cp.parent_exec_workdir({}, "ls"))
        self.assertIsNone(cp.parent_exec_workdir({"transcript_path": self.tmp.name}, "ls"))


class GitRepoTestCase(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.repo = os.path.realpath(os.path.join(self.tmp.name, "repo"))
        env = {**os.environ, "GIT_CONFIG_GLOBAL": "/dev/null", "GIT_CONFIG_NOSYSTEM": "1"}
        self.env = env
        git = ["git", "-C", self.repo, "-c", "user.email=a@b", "-c", "user.name=a"]
        subprocess.run(["git", "init", "-q", self.repo], check=True, env=env)
        os.mkdir(os.path.join(self.repo, "emacs"))
        for name in ("b.txt", "tracked.el", "emacs/config.org"):
            Path(self.repo, name).write_text("x\n")
        subprocess.run(git + ["add", "."], check=True, env=env)
        subprocess.run(git + ["commit", "-qm", "init"], check=True, env=env)
        subprocess.run(git + ["config", "alias.ci", "commit"], check=True, env=env)


class GitInvocationTests(GitRepoTestCase):
    def test_global_options_and_status_decoupling(self):
        records = cp.git_invocations("git -C sub commit -m x | cat", self.repo, self.env)
        self.assertEqual(records, [{
            "subcommand": "commit", "args": ["-m", "x"], "global_args": ["-C", "sub"], "assignments": [],
            "ambiguous": True, "context_dir": self.repo, "ambiguity": "shell-status-decoupled",
            "status_operator": "|", "sequence": 0,
        }])

    def test_cd_static_substitution_and_aliases(self):
        records = cp.git_invocations("cd emacs && git $(printf commit); git ci -m y", self.repo, self.env)
        self.assertEqual([(r["subcommand"], r["context_dir"], r.get("invoked_subcommand")) for r in records],
                         [("commit", self.repo + "/emacs", None), ("commit", self.repo + "/emacs", "ci")])

    def test_literal_commit_message_heredoc_is_masked(self):
        source = "git commit -F - <<'EOF'\n$(git push) `git rm x`\nEOF\n"
        self.assertEqual([r["subcommand"] for r in cp.git_invocations(source, self.repo, self.env)], ["commit"])

    def test_dynamic_forms_are_ambiguous_commits(self):
        for source in ("git `echo x`", "eval git status", "$GIT commit", "git $(echo 'x"):
            records = cp.git_invocations(source, self.repo, self.env)
            self.assertTrue(records and all(r["ambiguous"] for r in records if r["subcommand"] == "commit"), source)

    def test_counts(self):
        source = "git status; git \"commit\n\" -m x && git commit"
        self.assertEqual(cp.git_commit_count(source, self.repo, self.env), 2)
        self.assertEqual(cp.git_subcommand_count(source, "status", self.repo, self.env), 1)

    def test_record_command_quotes_like_printf_q(self):
        record = {"assignments": ["A=1"], "global_args": ["-C", "a b"], "subcommand": "commit",
                  "args": ["-m", "", "~x", "x=~", "a~", "#c", "d#", "é", "t\tab", "\x01é", "\x1b", "\u00a0", "\u200b"]}
        # NBSP is printable to libc iswprint (unlike str.isprintable); U+200B is not.
        self.assertEqual(cp.git_record_command(record),
                         "A=1 git -C a\\ b commit -m '' \\~x x=\\~ a~ \\#c d# é $'t\\tab' $'\\001é' $'\\E' "
                         "\u00a0 $'\\342\\200\\213' ")

    def test_commit_inspection(self):
        for args, expected in ((["--dry-run"], True), (["-m", "--dry-run"], False), (["--", "--dry-run"], False),
                               (["-qv", "--short"], True), (["-h"], True), (["--dry"], False), (["-x"], False),
                               (["-Shello", "--porcelain"], True), (["-mfoo", "--dry-run"], True)):
            self.assertEqual(cp.git_commit_inspection_p({"args": args}), expected, args)

    def test_add_selects_elisp(self):
        Path(self.repo, "b.txt").write_text("y\n")
        def selects(*args):
            return cp.git_add_selects_elisp(self.repo, {"args": list(args)}, self.env)
        self.assertFalse(selects("b.txt"))
        self.assertFalse(selects())
        self.assertFalse(selects("-A"))
        Path(self.repo, "new.el").write_text("y\n")
        self.assertTrue(selects("-A"))
        self.assertFalse(selects("-u"))
        self.assertTrue(selects("--", "new.el"))
        self.assertTrue(selects("--pathspec-from-file=list"))
        Path(self.repo, "emacs/config.org").write_text("y\n")
        self.assertTrue(selects("-u"))

    def test_invocation_repo(self):
        record = {"assignments": [], "global_args": ["-C", "emacs"]}
        self.assertEqual(cp.git_invocation_repo(record, self.repo, self.env), self.repo + "\n")
        self.assertEqual(cp.git_invocation_repo(record, "", self.env), "")
        self.assertEqual(cp.git_invocation_repo(record, "/nonexistent", self.env), "")
        env = {**self.env, "OLDPWD": self.repo}
        self.assertEqual(cp.git_invocation_repo({"assignments": [], "global_args": []}, "-", env),
                         self.repo + "\n" + self.repo + "\n")


class ShellSyntaxTests(unittest.TestCase):
    def test_git_commit_syntax_count(self):
        self.assertEqual(cp._shell_syntax_count("echo 'git commit'; git commit", "git"), 1)
        self.assertEqual(cp._shell_syntax_count("bash -c 'git commit'", "git"), 1)
        self.assertEqual(cp._shell_syntax_count('echo "$(git commit)"', "git"), 1)

    def test_emacsclient_eval_count(self):
        self.assertEqual(cp.emacsclient_eval_count("emacsclient -e x; command emacsclient --eval y"), 2)
        self.assertEqual(cp.emacsclient_eval_count("echo 'emacsclient -e x'"), 0)
        self.assertEqual(cp.emacsclient_eval_count("elpaca-extras-format-build-reload-status; emacsclient -e x"), 0)

    def test_executables(self):
        source = "A=1 command git status | exec exec x; ./a=b y; { batch-test.sh l; }"
        self.assertEqual(cp.shell_executables(source), ["git", "exec", "a=b", "batch-test.sh"])
        self.assertEqual(cp.executable_count(source, "git"), 1)
        with self.assertRaises(HookFailure):
            cp.executable_count("echo 'unterminated", "git")

    def test_elisp_evidence_labels(self):
        source = ("batch-test.sh one; elisp-check-evidence --staged two -- x; elisp-live-verify three -- x; "
                  "elisp-live-verify four -- x y; batch-test.sh '$five'")
        self.assertEqual(cp.elisp_evidence_labels(source, "test"), ["one", "two"])
        self.assertEqual(cp.elisp_evidence_labels(source, "live"), ["three"])
        self.assertEqual(cp.elisp_evidence_labels("batch-test.sh 'x", "test"), [])


if __name__ == "__main__":
    unittest.main()
