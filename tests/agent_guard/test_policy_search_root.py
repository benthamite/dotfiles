import os
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard import engine  # noqa: E402
from agent_guard.core import Call  # noqa: E402
from agent_guard.policies import search_root  # noqa: E402


class SearchRootCase(unittest.TestCase):
    """A temporary HOME holding `My Drive/dotfiles`, `repos/x` and symlinks to them."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        base = Path(self.tmp.name)
        self.home = str(base / "home")
        for sub in ("My Drive/dotfiles", "repos/x/src", "Library/CloudStorage/Drive"):
            os.makedirs(os.path.join(self.home, sub))
        os.symlink(self.home, base / "home-link")
        os.symlink(os.path.join(self.home, "repos/x"), base / "x-link")
        os.symlink(os.path.join(self.home, "My Drive"), os.path.join(self.home, "repos/drive-link"))
        self.base = str(base)
        self.repo = os.path.join(self.home, "repos/x")

    def decide(self, runtime, tool, tool_input, cwd=None):
        payload = {"tool_name": tool, "tool_input": tool_input, "cwd": cwd or self.repo}
        return search_root.decide(Call.from_payload(runtime, payload, env={"HOME": self.home}))

    def bash(self, command, cwd=None, runtime="claude"):
        return self.decide(runtime, "Bash", {"command": command}, cwd).decision

    def assertDenied(self, command, cwd=None, runtime="claude"):
        self.assertEqual(self.bash(command, cwd, runtime), "deny", command)

    def assertAllowed(self, command, cwd=None, runtime="claude"):
        self.assertEqual(self.bash(command, cwd, runtime), "allow", command)


class ShellRootTests(SearchRootCase):
    def test_each_search_tool_at_a_huge_root(self):
        for command in ("find / -name x", "bfs / -name x", "gfind / -name x", "grep -r foo /", "grep -R foo /",
                        "egrep -r foo /", "ugrep -r foo /", "ug -r foo /", "grep --recursive foo /",
                        "grep -d recurse foo /", "grep --directories=recurse foo /", "rg foo /", "fd x /",
                        "fdfind x /", "find /Users -name x", "rg foo /Volumes", "find /System/Volumes/Data -name x"):
            self.assertDenied(command)

    def test_home_and_drive_spellings(self):
        for command in ("find ~ -name x", "find ~/ -name x", "find $HOME -name x", "find ${HOME} -name x",
                        'find "$HOME" -name x', "find ~/My\\ Drive -name x", 'rg foo "$HOME/My Drive/"',
                        "rg foo '" + self.home + "/My Drive'", "rg foo ~/Library/CloudStorage"):
            self.assertDenied(command)

    def test_narrower_roots_allowed(self):
        for command in ("find ~/My\\ Drive/dotfiles -name x", "rg foo ~/repos/x", "grep -rn foo ~/repos/x/src",
                        "fd x ~/My\\ Drive/dotfiles", "find /tmp -name x", "rg foo .", "find . -name x",
                        "rg foo ~/Library/CloudStorage/Drive"):
            self.assertAllowed(command)

    def test_ancestor_of_a_huge_root_is_huge(self):
        # ~/Library contains ~/Library/CloudStorage.
        self.assertDenied("find ~/Library -name x")
        self.assertDenied("rg foo " + os.path.dirname(self.home))

    def test_non_recursive_and_non_search_commands_allowed(self):
        for command in ("grep foo ~/.zshrc", "grep foo /etc/hosts", "ugrep foo ~", "echo find / -name x",
                        "git grep foo", "ls ~", "rg --version", "rg --type-list", "find --help", "cat <<'EOF'\n"
                        "find / -name x\nEOF", "printf '%s' 'rg foo /'"):
            self.assertAllowed(command)

    def test_shallow_depth_allowed(self):
        for command in ("find ~ -maxdepth 1 -name x", "find / -maxdepth 2 -name x", "fd -d 1 x ~",
                        "fd --max-depth=2 x ~", "rg --max-depth 1 foo ~", "rg -d2 foo ~", "ugrep -r -1 foo ~",
                        "ugrep -r --depth=2 foo ~", "ugrep -r --depth=1,2 foo ~"):
            self.assertAllowed(command)
        for command in ("find ~ -maxdepth 3 -name x", "fd -d 5 x ~", "rg --max-depth 10 foo ~",
                        "ugrep -r --depth=3 foo ~", "find ~ -mindepth 1 -name x"):
            self.assertDenied(command)


class PathNormalizationTests(SearchRootCase):
    def test_trailing_slashes_and_dots(self):
        for path in ("/", "//", "/./", "~/", "~//", "~/.", "~/repos/..", "~/My\\ Drive/", "~/My\\ Drive/./",
                     "~/My\\ Drive/dotfiles/.."):
            self.assertDenied(f"find {path} -name x")
        self.assertAllowed("find ~/My\\ Drive/dotfiles/ -name x")

    def test_relative_paths_resolve_against_cwd(self):
        self.assertDenied("find .. -name x", cwd=os.path.join(self.home, "repos"))
        self.assertDenied("find ../.. -name x")
        self.assertDenied("rg foo ../../My\\ Drive")
        self.assertAllowed("rg foo ../../My\\ Drive/dotfiles")
        self.assertAllowed("find src -name x")
        self.assertDenied("find . -name x", cwd=self.home)
        self.assertDenied("find . -name x", cwd=self.home + "/")

    def test_symlinks_resolve(self):
        self.assertDenied(f"find {self.base}/home-link -name x")
        self.assertDenied("rg foo drive-link", cwd=os.path.join(self.home, "repos"))
        self.assertAllowed(f"rg foo {self.base}/x-link")
        # A cwd reached through a symlink resolves too.
        self.assertDenied("rg foo", cwd=f"{self.base}/home-link")
        self.assertAllowed("rg foo", cwd=f"{self.base}/x-link")

    def test_no_path_searches_the_cwd(self):
        for command in ("rg foo", "fd x", "find", "find -name x", "bfs -name x", "grep -r foo", "rg --files"):
            self.assertDenied(command, cwd=self.home)
            self.assertAllowed(command)
        self.assertDenied("rg foo", cwd="/")

    def test_glob_in_last_component_searches_the_parent(self):
        self.assertDenied("find ~/* -name x")
        self.assertDenied("grep -r foo /*")
        self.assertAllowed("find ~/repos/* -name x")
        # A glob in a middle component names specific subdirectories.
        self.assertAllowed("find ~/*/dotfiles -name x")

    def test_unresolvable_roots_allowed(self):
        for command in ("find $OTHER -name x", "rg foo $(pwd)", "find ~other -name x", "rg foo \"$DIR\""):
            self.assertAllowed(command)

    def test_cd_changes_the_cwd(self):
        self.assertDenied("cd ~ && rg foo")
        self.assertDenied("cd; rg foo")
        self.assertDenied("cd -- ~/My\\ Drive && find . -name x")
        self.assertDenied("cd ~/repos && rg foo ..")
        self.assertAllowed("cd ~/repos/x && rg foo")
        self.assertAllowed("cd ~ && cd repos/x && rg foo")
        self.assertDenied("pushd ~ >/dev/null && fd x")
        # `cd -` goes somewhere unknown: relative roots after it are not judged.
        self.assertAllowed("cd - && rg foo")
        self.assertDenied("cd - && rg foo /")


class BashParsingTests(SearchRootCase):
    def test_compound_commands_and_pipelines(self):
        for command in ("echo hi; find / -name x", "true && rg foo ~", "false || fd x /", "rg foo ~ | head",
                        "ls & find ~ -name x", "(find ~ -name x)", "{ rg foo ~; }", "if true; then rg foo ~; fi",
                        "x=$(find ~ -name '*.pdf')", 'echo "$(rg -l foo ~)"', "echo `find ~ -name x`",
                        "for f in $(fd pdf ~); do echo $f; done", "find ~ \\\n  -name x"):
            self.assertDenied(command)

    def test_piped_grep_and_rg_read_stdin(self):
        self.assertAllowed("cat file | grep -r foo", cwd=self.home)
        self.assertAllowed("ls | rg foo", cwd=self.home)
        self.assertDenied("ls | rg foo ~")
        # find and fd ignore stdin.
        self.assertDenied("echo | find -name x", cwd=self.home)

    def test_wrappers_and_assignments(self):
        for command in ("timeout 30 find / -name x", "timeout -s KILL 5 rg foo /", "nice -n 10 rg foo ~",
                        "env LC_ALL=C grep -r foo ~", "LC_ALL=C rg foo ~", "command find / -name x",
                        "exec rg foo ~", "nohup find / -name x &", "time rg foo ~", "sudo find / -name x",
                        "\\find / -name x", "/usr/bin/find / -name x", "/opt/homebrew/bin/rg foo ~",
                        "noglob find ~ -name *.pdf"):
            self.assertDenied(command)

    def test_nested_shells(self):
        for command in ("bash -c 'find / -name x'", "sh -c \"rg foo ~\"", "zsh -lc 'cd ~ && rg foo'",
                        "bash -c 'echo $(find ~ -name x)'"):
            self.assertDenied(command)
        self.assertAllowed("bash -c 'rg foo ~/repos/x'")

    def test_option_parsing(self):
        # The first operand is the pattern unless -e/-f supplies it.
        self.assertAllowed("rg / ~/repos/x")
        self.assertAllowed("grep -r / ~/repos/x")
        self.assertDenied("rg -e foo ~")
        self.assertDenied("rg -efoo ~")
        self.assertDenied("grep -rne foo ~")
        self.assertDenied("grep -rf patterns.txt ~")
        self.assertDenied("rg --regexp foo ~")
        self.assertDenied("rg --regexp=foo ~")
        # Option values are not operands.
        self.assertAllowed("rg -g '*.py' foo ~/repos/x")
        self.assertAllowed("rg --glob / foo ~/repos/x")
        self.assertAllowed("grep -r --include / foo ~/repos/x")
        self.assertAllowed("rg -m 1 foo ~/repos/x")
        self.assertDenied("rg -m 1 foo ~")
        # Options after operands (GNU permutation) and `--`.
        self.assertDenied("grep foo ~ -r")
        self.assertDenied("rg foo -- ~")
        self.assertAllowed("rg -- -r ~/repos/x")
        # Multiple paths: one huge root is enough.
        self.assertDenied("rg foo ~/repos/x ~/My\\ Drive")

    def test_find_leading_options(self):
        for command in ("find -L / -name x", "find -H -L ~ -name x", "find -f / -name x", "bfs -S dfs ~ -name x",
                        "find -x / -name x", "find -E ~ -regex x", "find -O3 / -name x", "find / ~ -name x"):
            self.assertDenied(command)
        self.assertAllowed("find ~/repos/x -name x -o -name y")

    def test_fd_paths(self):
        self.assertDenied("fd --search-path ~ x")
        self.assertDenied("fd x --search-path=/")
        self.assertDenied("fd --base-directory ~ x")
        self.assertAllowed("fd --base-directory ~/repos/x x")
        self.assertAllowed("fd --base-directory ~ x repos/x")
        self.assertAllowed("fd -e py x ~/repos/x")
        self.assertAllowed("fd x ~/repos/x -x rg foo /")    # the -x command's words are not fd operands

    def test_unparseable_command_allowed(self):
        self.assertAllowed("find / -name 'x")


class ClaudeToolTests(SearchRootCase):
    def test_grep_path(self):
        self.assertEqual(self.decide("claude", "Grep", {"pattern": "x", "path": "~"}).decision, "deny")
        self.assertEqual(self.decide("claude", "Grep", {"pattern": "x", "path": "/"}).decision, "deny")
        self.assertEqual(self.decide("claude", "Grep", {"pattern": "x", "path": self.home + "/My Drive/"}).decision,
                         "deny")
        self.assertEqual(self.decide("claude", "Grep", {"pattern": "x", "path": "~/My Drive/dotfiles"}).decision,
                         "allow")
        self.assertEqual(self.decide("claude", "Grep", {"pattern": "x", "path": "../.."}).decision, "deny")
        self.assertEqual(self.decide("claude", "Grep", {"pattern": "x", "path": "src"}).decision, "allow")

    def test_grep_without_path_uses_cwd(self):
        self.assertEqual(self.decide("claude", "Grep", {"pattern": "x"}, cwd=self.home).decision, "deny")
        self.assertEqual(self.decide("claude", "Grep", {"pattern": "x"}, cwd=self.home + "/My Drive").decision,
                         "deny")
        self.assertEqual(self.decide("claude", "Grep", {"pattern": "x"}).decision, "allow")

    def test_glob(self):
        self.assertEqual(self.decide("claude", "Glob", {"pattern": "**/*.pdf"}, cwd=self.home).decision, "deny")
        self.assertEqual(self.decide("claude", "Glob", {"pattern": "**/*.pdf", "path": "~/My Drive"}).decision,
                         "deny")
        self.assertEqual(self.decide("claude", "Glob", {"pattern": "~/My Drive/**/*.pdf"}).decision, "deny")
        self.assertEqual(self.decide("claude", "Glob", {"pattern": self.home + "/**"}).decision, "deny")
        self.assertEqual(self.decide("claude", "Glob", {"pattern": "~/My Drive/dotfiles/**/*.el"}).decision,
                         "allow")
        self.assertEqual(self.decide("claude", "Glob", {"pattern": "**/*.py"}).decision, "allow")
        # Without ** the depth is fixed.
        self.assertEqual(self.decide("claude", "Glob", {"pattern": "*.md"}, cwd=self.home).decision, "allow")

    def test_denial_message(self):
        reason = self.decide("claude", "Grep", {"pattern": "x", "path": "~/My Drive"}).reason
        self.assertTrue(reason.startswith("BLOCKED: search rooted at a huge tree."), reason)
        self.assertIn("~/My Drive", reason)
        self.assertIn("Name a narrower directory", reason)
        reason = self.decide("claude", "Bash", {"command": "rg foo"}, cwd=self.home).reason
        self.assertIn("its working directory", reason)

    def test_other_tools_allowed(self):
        self.assertEqual(self.decide("claude", "Read", {"file_path": "/"}).decision, "allow")


class CodexTests(SearchRootCase):
    def test_shell_tools(self):
        for tool in ("Bash", "exec_command", "functions.exec_command"):
            self.assertEqual(self.decide("codex", tool, {"cmd": "rg foo ~"}).decision, "deny", tool)
            self.assertEqual(self.decide("codex", tool, {"command": "find / -name x"}).decision, "deny", tool)
            self.assertEqual(self.decide("codex", tool, {"cmd": "rg foo ~/repos/x"}).decision, "allow", tool)

    def test_workdir(self):
        self.assertEqual(self.decide("codex", "exec_command", {"cmd": "rg foo", "workdir": self.home}).decision,
                         "deny")
        self.assertEqual(self.decide("codex", "exec_command", {"cmd": "rg foo", "workdir": self.repo}).decision,
                         "allow")
        self.assertEqual(self.decide("codex", "exec_command", {"cmd": "rg foo"}, cwd=self.home).decision, "deny")

    def test_functions_exec_nested_commands(self):
        program = ("const r = await tools.exec_command({cmd: 'rg foo', workdir: '" + self.home + "'});\n"
                   "text(r.output);")
        self.assertEqual(self.decide("codex", "functions.exec", {"input": program}).decision, "deny")
        program = "await tools.exec_command({cmd: 'rg foo', workdir: '" + self.repo + "'});"
        self.assertEqual(self.decide("codex", "functions.exec", {"input": program}).decision, "allow")


class EngineTests(unittest.TestCase):
    def test_registered_in_every_search_chain(self):
        for runtime, tool in (("claude", "Bash"), ("claude", "Grep"), ("claude", "Glob"), ("codex", "Bash"),
                              ("codex", "exec_command"), ("codex", "functions.exec")):
            self.assertIn(search_root, list(engine.policies_for(runtime, tool)), (runtime, tool))
        self.assertNotIn(search_root, list(engine.policies_for("claude", "Read")))

    def test_huge_roots_configured_once(self):
        self.assertIn("/", search_root.HUGE_ROOTS)
        self.assertIn("~", search_root.HUGE_ROOTS)
        self.assertIn("~/My Drive", search_root.HUGE_ROOTS)
        self.assertIn("/Users", search_root.HUGE_ROOTS)
        self.assertIn("/Volumes", search_root.HUGE_ROOTS)


if __name__ == "__main__":
    unittest.main()
