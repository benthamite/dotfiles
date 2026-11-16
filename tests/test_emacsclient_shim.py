#!/usr/bin/env python3
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
              '(let* ((agent-eval--source "{}") (agent-eval--read (read-from-string '
              'agent-eval--source))) (condition-case nil (progn (read-from-string '
              'agent-eval--source (cdr agent-eval--read)) (error "Agent evaluation must be '
              'exactly one Lisp expression, but text follows it")) (end-of-file nil) '
              '(invalid-read-syntax (error "Agent evaluation must be exactly one balanced Lisp '
              'expression, but unbalanced text follows it"))) (let ((agent-eval--value '
              '(with-timeout (120 (error "Agent evaluation exceeded EMACSCLIENT_EVAL_LIMIT (%ss)" '
              '120)) (eval (car agent-eval--read) t)))) (if (or (stringp agent-eval--value) '
              '(numberp agent-eval--value) (symbolp agent-eval--value)) agent-eval--value '
              '(let ((agent-eval--size (length (prin1-to-string agent-eval--value)))) '
              '(if (> agent-eval--size 4000) (error "Agent evaluation result too large to print '
              'safely (%d chars, limit EMACSCLIENT_EVAL_MAX_PRINT); return a string or a narrower '
              'value" agent-eval--size) agent-eval--value))))))')
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

    def run_slow(self, sleep, code=0, **env):
        slow = self.dir / "slow"
        slow.write_text(f"#!/bin/sh\nsleep {sleep}\nexit {code}\n")
        slow.chmod(0o755)
        base = {"PATH": os.environ["PATH"], "EMACSCLIENT_SHIM_REAL": str(slow),
                "EMACS_SOCKET_NAME": "/sock"}
        return subprocess.run([str(SHIM), "--eval", "(foo)"], env={**base, **env},
                              capture_output=True)

    def test_slow_agent_eval_warns_and_keeps_exit_status(self):
        result = self.run_slow(2, code=3, CLAUDECODE="1", EMACSCLIENT_EVAL_WARN="1")
        self.assertEqual(result.returncode, 3)
        self.assertIn(b"blocked Pablo's live Emacs for about", result.stderr)

    def test_fast_or_unwatched_evals_do_not_warn(self):
        self.assertEqual(self.run_slow(0, CLAUDECODE="1").stderr, b"")
        self.assertEqual(self.run_slow(2, EMACSCLIENT_EVAL_WARN="1").stderr, b"")
        self.assertEqual(self.run_slow(2, CLAUDECODE="1", EMACSCLIENT_EVAL_WARN="0").stderr, b"")

    def test_expression_is_quoted_as_a_lisp_string(self):
        out = self.run_shim(["--eval", '(message "a\\\\b")'], CLAUDECODE="1")
        self.assertIn('(agent-eval--source "(message \\"a\\\\\\\\b\\")")', out[-1])

    def test_agent_eval_loading_an_init_file_is_refused(self):
        # 2026-11-16: an agent loaded the profile's init.el into the live session.
        for form in ['(load "~/.config/emacs-profiles/9.0.2-dev/init.el")',
                     '(load (expand-file-name "~/x/init.el") nil)',
                     "(load-file user-init-file)",
                     '(load "early-init.el")',
                     '(load-file "/p/late-init.elc")']:
            with self.subTest(form=form):
                result = subprocess.run(
                    [str(SHIM), "--eval", form],
                    env={"PATH": os.environ["PATH"], "EMACSCLIENT_SHIM_REAL": str(self.fake),
                         "EMACS_SOCKET_NAME": "/sock", "CLAUDECODE": "1"},
                    capture_output=True)
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stdout, b"")
                self.assertIn(b"refused", result.stderr)

    def test_other_loads_and_init_mentions_pass(self):
        for form in ['(load "simple-extras")',
                     "(init-build-profile (file-name-directory user-init-file))",
                     '(load "my-init.el.bak-notes")',
                     '(find-file "init.el")']:
            with self.subTest(form=form):
                self.assertEqual(self.run_shim(["--eval", form], CLAUDECODE="1")[-1],
                                 LIMIT_FORM.format(form.replace("\\", "\\\\")
                                                   .replace('"', '\\"')))

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

    def test_quotes_backslashes_and_newlines_survive(self):
        self.assertEqual(self.ask('(concat "a\\"b" "\\\\"\n "c")').stdout.strip(),
                         '"a\\"b\\\\c"')

    def test_extra_closing_paren_is_an_error_and_nothing_runs(self):
        # 2026-11-16: one paren too many closed the spliced wrapper early, so
        # the client saw "agent-eval--value is void" and the code escaped it.
        result = self.ask("(setq shim-test-ran t))")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("unbalanced text follows it", result.stderr)
        self.assertEqual(self.ask("(boundp (quote shim-test-ran))").stdout.strip(), "nil")

    def test_second_expression_is_an_error(self):
        result = self.ask("(+ 1 2) (+ 3 4)")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("text follows it", result.stderr)

    def test_trailing_comment_is_allowed(self):
        self.assertEqual(self.ask("(+ 1 2) ; note").stdout.strip(), "3")

    def test_oversized_result_is_an_error_that_returns(self):
        # 2026-11-04: pretty-printing a large result hung the server, and an
        # error text containing a percent sign never reached the client.
        result = self.ask("(make-list 3000 (quote abcdef))")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("too large to print safely (21001 chars", result.stderr)


class DebuggerInteractionTest(unittest.TestCase):
    """The debug advice in config.org, so a debugger entered mid-eval can prompt.

    On 2026-11-13 SIGUSR2 broke a stuck agent eval into the debugger, which
    inherited the shim's `inhibit-interaction' binding: every command that read
    input failed until the debugger exited.  A probe advice innermost on `debug'
    reports the binding the real debugger would see, without entering it.
    """

    START = "(defun debug-allow-interaction"
    END = "(advice-add 'debug :around #'debug-allow-interaction)"

    def debugger_sees(self, with_fix):
        text = (DOTFILES / "emacs/config.org").read_text()
        start = text.index(self.START)
        fix = text[start:text.index(self.END, start) + len(self.END)]
        form = ("(progn %s (advice-add 'debug :around (lambda (_fn &rest _) inhibit-interaction)"
                " '((depth . 100))) (prin1 (list (let ((inhibit-interaction t)) (debug))"
                " (let ((inhibit-interaction t)) (debug) inhibit-interaction))))"
                % (fix if with_fix else ""))
        result = subprocess.run([EMACS, "-Q", "--batch", "--eval", form],
                                capture_output=True, text=True, check=True, timeout=60)
        return result.stdout.strip()

    def test_debugger_entered_under_the_shim_binding_can_prompt(self):
        # Inside the debugger: nil.  After it returns: the caller's t again.
        self.assertEqual(self.debugger_sees(with_fix=True), "(nil t)")

    def test_without_the_advice_the_binding_leaks(self):
        self.assertEqual(self.debugger_sees(with_fix=False), "(t t)")


if __name__ == "__main__":
    unittest.main()
