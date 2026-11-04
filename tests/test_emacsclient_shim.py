"""Argument handling of the emacsclient PATH shim, against a recording fake."""

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import tempfile
import unittest


DOTFILES = Path(__file__).resolve().parents[1]
SHIM = DOTFILES / "shell/shims/emacsclient"
LIMIT_FORM = ('(let ((inhibit-interaction t) (inhibit-debugger t) (inhibit-debug-on-entry t)) '
              '(let ((agent-eval--value (with-timeout (120 (error "Agent evaluation exceeded '
              'EMACSCLIENT_EVAL_LIMIT (%ss)" 120)) {}))) (if (or (stringp agent-eval--value) '
              '(numberp agent-eval--value) (symbolp agent-eval--value)) agent-eval--value '
              '(let ((agent-eval--size (length (prin1-to-string agent-eval--value)))) '
              '(if (> agent-eval--size 4000) (error "Agent evaluation result too large to print '
              'safely (%d chars, limit EMACSCLIENT_EVAL_MAX_PRINT); return a string or a narrower '
              'value" agent-eval--size) agent-eval--value)))))')
EMACS = "/opt/homebrew/bin/emacs"
REAL_CLIENT = "/opt/homebrew/bin/emacsclient"


class ShimTest(unittest.TestCase):
    def setUp(self):
        self.dir = Path(tempfile.mkdtemp())
        self.fake = self.dir / "emacsclient"
        self.fake.write_text('#!/bin/sh\nfor a in "$@"; do printf "%s\\0" "$a"; done\n')
        self.fake.chmod(0o755)

    def run_shim(self, args, **env):
        base = {"PATH": os.environ["PATH"], "EMACSCLIENT_SHIM_REAL": str(self.fake),
                "EMACS_SOCKET_NAME": "/sock"}
        result = subprocess.run([str(SHIM), *args], env={**base, **env},
                                capture_output=True, check=True)
        return result.stdout.decode().split("\0")[:-1]

    def test_agent_eval_is_time_limited(self):
        self.assertEqual(self.run_shim(["--eval", "(foo)"], CLAUDECODE="1"),
                         ["--timeout=130", "--eval", LIMIT_FORM.format("(foo)")])

    def test_codex_session_is_an_agent(self):
        self.assertEqual(self.run_shim(["-e", "(foo)"], CODEX_BUFFER_NAME="*codex*")[-1],
                         LIMIT_FORM.format("(foo)"))

    def test_only_the_first_expression_is_wrapped(self):
        # Hooks pass a JSON payload after the form, which the form reads.
        self.assertEqual(self.run_shim(["--eval", "(handle)", "{\"a\": 1}"], CLAUDECODE="1"),
                         ["--timeout=130", "--eval", LIMIT_FORM.format("(handle)"),
                          '{"a": 1}'])

    def test_option_values_are_not_mistaken_for_the_expression(self):
        out = self.run_shim(["-s", "server", "-a", "false", "--eval", "(foo)"],
                            CLAUDECODE="1")
        self.assertEqual(out, ["--timeout=130", "-s", "server", "-a", "false", "--eval",
                               LIMIT_FORM.format("(foo)")])

    def test_caller_timeout_is_kept(self):
        out = self.run_shim(["--timeout=5", "--eval", "(foo)"], CLAUDECODE="1")
        self.assertEqual(out, ["--timeout=5", "--eval", LIMIT_FORM.format("(foo)")])

    def test_bundled_eval_flag(self):
        out = self.run_shim(["-ne", "(foo)"], CLAUDECODE="1")
        self.assertEqual(out[-1], LIMIT_FORM.format("(foo)"))

    def test_limit_is_configurable_and_can_be_disabled(self):
        out = self.run_shim(["--eval", "(foo)"], CLAUDECODE="1", EMACSCLIENT_EVAL_LIMIT="30")
        self.assertEqual(out[0], "--timeout=40")
        self.assertIn("(with-timeout (30 ", out[-1])
        self.assertEqual(self.run_shim(["--eval", "(foo)"], CLAUDECODE="1",
                                       EMACSCLIENT_EVAL_LIMIT="0"),
                         ["--eval", "(foo)"])

    def test_print_limit_is_configurable(self):
        out = self.run_shim(["--eval", "(foo)"], CLAUDECODE="1", EMACSCLIENT_EVAL_MAX_PRINT="50")
        self.assertIn("(> agent-eval--size 50)", out[-1])

    def test_outside_agents_nothing_changes(self):
        self.assertEqual(self.run_shim(["--eval", "(foo)"]), ["--eval", "(foo)"])

    def test_file_visits_are_untouched(self):
        self.assertEqual(self.run_shim(["-n", "file.txt"], CLAUDECODE="1"),
                         ["-n", "file.txt"])

    def test_socket_is_injected_without_one(self):
        out = self.run_shim(["--eval", "(foo)"], EMACS_SOCKET_NAME="")
        tmp = subprocess.run(["getconf", "DARWIN_USER_TEMP_DIR"], capture_output=True,
                             text=True).stdout.strip()
        if Path(f"{tmp}emacs{os.getuid()}/server").is_socket():
            self.assertTrue(out[0].startswith("--socket-name="))



@unittest.skipUnless(os.access(EMACS, os.X_OK) and os.access(REAL_CLIENT, os.X_OK),
                     "Homebrew Emacs not installed")
class LiveServerTest(unittest.TestCase):
    """The wrapped form against a throwaway `emacs -Q' daemon, never the user's."""

    @classmethod
    def setUpClass(cls):
        cls.dir = tempfile.mkdtemp(dir="/tmp")  # short path: sockets have a length limit
        cls.sock = f"{cls.dir}/s"
        subprocess.run([EMACS, "-Q", f"--daemon={cls.sock}"], check=True,
                       capture_output=True, timeout=60)

    @classmethod
    def tearDownClass(cls):
        subprocess.run([REAL_CLIENT, "-s", cls.sock, "--eval", "(kill-emacs)"],
                       capture_output=True, timeout=30)
        subprocess.run(["trash", cls.dir], capture_output=True)

    def ask(self, form):
        env = {**os.environ, "CLAUDECODE": "1", "EMACSCLIENT_SHIM_REAL": REAL_CLIENT}
        return subprocess.run([str(SHIM), "-s", self.sock, "--eval", form], env=env,
                              capture_output=True, text=True, timeout=20)

    def test_small_values_print_as_before(self):
        self.assertEqual(self.ask('(list 1 "a" (quote b))').stdout.strip(), '(1 "a" b)')
        self.assertEqual(self.ask('"str"').stdout.strip(), '"str"')

    def test_oversized_result_is_an_error_that_returns(self):
        # 2026-11-04: pretty-printing a large result hung the server, and an
        # error text containing a percent sign never reached the client.
        result = self.ask("(make-list 3000 (quote abcdef))")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("too large to print safely (21001 chars", result.stderr)


if __name__ == "__main__":
    unittest.main()
