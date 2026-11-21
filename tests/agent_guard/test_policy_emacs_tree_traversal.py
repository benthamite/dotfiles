import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "lib/python"))

from agent_guard.core import CLAUDE, CODEX, Call  # noqa: E402
from agent_guard.engine import decide as engine_decide  # noqa: E402
from agent_guard.policies import emacs_tree_traversal as policy  # noqa: E402

N = policy.MAX_FILES


class Base(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = os.path.realpath(self.tmp.name)
        with open(os.path.join(self.dir, "big.txt"), "w") as f:
            f.write("".join(f"/x/{i}.org\n" for i in range(N + 1)))
        with open(os.path.join(self.dir, "small.txt"), "w") as f:
            f.write("a.org\nb.org\n")
        os.makedirs(os.path.join(self.dir, "tree"))
        for i in range(N + 1):
            open(os.path.join(self.dir, "tree", f"{i}.org"), "w").close()
        os.makedirs(os.path.join(self.dir, "few"))
        open(os.path.join(self.dir, "few", "a.org"), "w").close()

    def tearDown(self):
        self.tmp.cleanup()

    def call(self, command, env=None, runtime=CLAUDE):
        env = {"HOME": self.dir, "CLAUDE_BUFFER_NAME": "*claude:x*"} if env is None else env
        return Call(runtime=runtime, tool_name="Bash", tool_input={"command": command}, cwd=self.dir, env=env)

    def decide(self, command, **kw):
        return policy.decide(self.call(command, **kw)).decision


class InsideEmacsTests(Base):
    def test_2026_11_17_shape_denied(self):
        self.assertEqual(self.decide("tr '\\n' '\\0' < big.txt | xargs -0 rg -l foo"), "deny")

    def test_cat_list_denied(self):
        self.assertEqual(self.decide("cat big.txt | xargs grep -l foo"), "deny")

    def test_xargs_arg_file_denied(self):
        self.assertEqual(self.decide("xargs -a big.txt rg foo"), "deny")

    def test_find_over_large_tree_denied(self):
        self.assertEqual(self.decide("find tree -name '*.org' | xargs grep -l foo"), "deny")

    def test_for_loop_over_find_denied(self):
        self.assertEqual(self.decide('for f in $(find tree -name "*.org"); do grep x "$f"; done'), "deny")

    def test_cd_then_list_denied(self):
        self.assertEqual(self.decide(f"cd {self.dir} && cat big.txt | xargs rg foo", ), "deny")

    def test_git_ls_files_counted(self):
        subprocess.run(["git", "init", "-q", os.path.join(self.dir, "tree")], check=True)
        subprocess.run(["git", "-C", os.path.join(self.dir, "tree"), "add", "."], check=True)
        self.assertEqual(self.decide("cd tree && git ls-files | xargs grep -l foo"), "deny")

    def test_small_list_allowed(self):
        self.assertEqual(self.decide("cat small.txt | xargs grep -l foo"), "allow")

    def test_small_tree_allowed(self):
        self.assertEqual(self.decide("find few -name '*.org' | xargs grep -l foo"), "allow")

    def test_shallow_find_allowed(self):
        self.assertEqual(self.decide("find tree -maxdepth 1 | xargs grep foo"), "allow")

    def test_harmless_xargs_allowed(self):
        self.assertEqual(self.decide("cat big.txt | xargs echo"), "allow")

    def test_single_search_process_allowed(self):
        self.assertEqual(self.decide("rg -l foo tree"), "allow")

    def test_unresolvable_list_allowed(self):
        self.assertEqual(self.decide('cat "$LIST" | xargs rg foo'), "allow")

    def test_codex_runtime_denied(self):
        env = {"HOME": self.dir, "CODEX_BUFFER_NAME": "*codex:x*"}
        call = Call.from_payload(CODEX, {"tool_name": "exec_command", "cwd": self.dir,
                                         "tool_input": {"cmd": "cat big.txt | xargs rg foo", "workdir": self.dir}},
                                 env=env)
        self.assertEqual(policy.decide(call).decision, "deny")

    def test_engine_chain_denies(self):
        self.assertEqual(engine_decide(self.call("cat big.txt | xargs rg foo")).decision, "deny")


class OutsideEmacsTests(Base):
    def test_terminal_session_allowed(self):
        self.assertEqual(self.decide("cat big.txt | xargs rg foo", env={"HOME": self.dir}), "allow")


if __name__ == "__main__":
    unittest.main()
