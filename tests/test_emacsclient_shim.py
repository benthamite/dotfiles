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
              '(with-timeout (120 (error "Agent evaluation exceeded EMACSCLIENT_EVAL_LIMIT (%ss)" 120)) {}))')


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


if __name__ == "__main__":
    unittest.main()
