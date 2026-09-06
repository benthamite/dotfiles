"""Native client-runner checks using only newly created Python processes."""
import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
HELPERS = [ROOT / "emacs" / runtime / "skills/emacs-freeze/scripts/run-client.py"
           for runtime in (".codex", ".claude")]


def load_runner():
    spec = importlib.util.spec_from_file_location("freeze_client_runner", HELPERS[0])
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class EmacsFreezeClientTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="freeze-client-tests-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name).resolve()
        self.env = {"PATH": "/usr/bin:/bin", "PYTHONDONTWRITEBYTECODE": "1"}

    def invoke(self, *arguments, helper=None):
        return subprocess.run([sys.executable, "-B", str(helper or HELPERS[0]),
                               *map(str, arguments)], env=self.env,
                              stdin=subprocess.DEVNULL, capture_output=True,
                              text=True, timeout=8)

    def child(self, source):
        return [sys.executable, "-B", "-c", source]

    def ready_child(self, ready, *, ignore_alarm=False):
        return self.child(
            "import os,signal,time; from pathlib import Path; "
            + ("signal.signal(signal.SIGALRM,signal.SIG_IGN); " if ignore_alarm else "")
            + f"Path({str(ready)!r}).write_text(str(os.getpid())); time.sleep(2)")

    def assert_gone(self, pid):
        with self.assertRaises(ProcessLookupError):
            os.kill(pid, 0)

    def test_both_helpers_identical(self):
        self.assertEqual(HELPERS[0].read_bytes(), HELPERS[1].read_bytes())

    def test_normal_streams_and_native_status_are_preserved_in_both_copies(self):
        command = self.child("import sys; print('owned stdout'); "
                             "print('owned stderr',file=sys.stderr); raise SystemExit(7)")
        for helper in HELPERS:
            with self.subTest(helper=helper):
                result = self.invoke("3", *command, helper=helper)
                self.assertEqual(result.returncode, 7)
                self.assertEqual(result.stdout, "owned stdout\n")
                self.assertEqual(result.stderr, "owned stderr\n")

    def test_literal_arguments_do_not_become_shell_code(self):
        arguments = ["spaces and $dollar; punctuation", "--leading-dash", "a'b\nsecond line"]
        result = self.invoke("3", *self.child("import json,sys; print(json.dumps(sys.argv[1:]))"),
                             *arguments)
        self.assertEqual(result.returncode, 0)
        self.assertEqual(json.loads(result.stdout), arguments)

    def test_stdin_is_closed_and_output_is_not_captured_by_runner(self):
        result = self.invoke("3", *self.child("import sys; print(repr(sys.stdin.read()))"))
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "''\n")
        runner = load_runner()
        child = mock.Mock()
        child.wait.return_value = 0
        with mock.patch.object(runner.subprocess, "Popen", return_value=child) as launch:
            self.assertEqual(runner.main(["1", "owned-client", "literal arg"]), 0)
        launch.assert_called_once_with(["owned-client", "literal arg"], stdin=subprocess.DEVNULL)

    def test_missing_executable_is_nonzero_without_echoing_argv(self):
        missing = self.root / "private-name-that-must-not-be-echoed"
        result = self.invoke("1", missing)
        self.assertEqual(result.returncode, 127)
        self.assertEqual(result.stdout, "")
        self.assertEqual(result.stderr, "Cannot launch client.\n")

    def test_invalid_deadlines_never_launch_a_client(self):
        marker = self.root / "must-not-exist"
        command = self.child(f"from pathlib import Path; Path({str(marker)!r}).touch()")
        for seconds in ("", "nan", "inf", "-inf", "0", "-1", "301", "1e308", "invalid"):
            with self.subTest(seconds=seconds):
                result = self.invoke(seconds, *command)
                self.assertEqual(result.returncode, 2)
                self.assertFalse(marker.exists())
                self.assertNotIn("Traceback", result.stderr)
        for arguments in ((), ("1",)):
            with self.subTest(arguments=arguments):
                self.assertEqual(self.invoke(*arguments).returncode, 2)

    def test_timeout_reaps_owned_child_even_when_it_ignores_alarm(self):
        for ignore_alarm in (False, True):
            with self.subTest(ignore_alarm=ignore_alarm):
                ready = self.root / f"ready-{ignore_alarm}"
                started = time.monotonic()
                result = self.invoke("0.5", *self.ready_child(ready, ignore_alarm=ignore_alarm))
                self.assertEqual(result.returncode, 124)
                self.assertIn("server outcome remains unknown", result.stderr)
                self.assertLess(time.monotonic() - started, 3)
                self.assertTrue(ready.is_file(), "child did not reach the owned fixture boundary")
                self.assert_gone(int(ready.read_text()))

    def test_interrupt_reaps_only_the_owned_child(self):
        ready = self.root / "interrupt-child-ready"
        process = subprocess.Popen([sys.executable, "-B", str(HELPERS[0]), "10",
                                    *self.ready_child(ready)], env=self.env,
                                   stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE, text=True)
        try:
            deadline = time.monotonic() + 3
            while not ready.exists() and process.poll() is None and time.monotonic() < deadline:
                time.sleep(0.01)
            self.assertTrue(ready.is_file(), "owned child never became ready")
            process.send_signal(signal.SIGINT)
            stdout, stderr = process.communicate(timeout=7)
            self.assertEqual(process.returncode, 130)
            self.assertEqual(stdout, "")
            self.assertIn("Client wait interrupted", stderr)
            self.assert_gone(int(ready.read_text()))
        finally:
            if process.poll() is None:
                process.kill()
            process.communicate(timeout=7)

    def test_unconfirmed_cleanup_reports_owned_pid_without_raw_errors(self):
        runner = load_runner()
        for cleanup_error in (subprocess.TimeoutExpired("sensitive-command", 5),
                              OSError("sensitive-error"), KeyboardInterrupt()):
            with self.subTest(error=type(cleanup_error).__name__):
                child = mock.Mock(pid=424242)
                child.wait.side_effect = [subprocess.TimeoutExpired("sensitive-command", 1),
                                          cleanup_error]
                output = io.StringIO()
                with mock.patch.object(runner.subprocess, "Popen", return_value=child), \
                        contextlib.redirect_stderr(output):
                    self.assertEqual(runner.main(["1", "owned-client"]), 125)
                child.kill.assert_called_once_with()
                self.assertIn("owned PID 424242", output.getvalue())
                self.assertNotIn("sensitive", output.getvalue())
                self.assertEqual(child.wait.call_args_list,
                                 [mock.call(timeout=1.0), mock.call(timeout=5.0)])

    def test_supported_maximum_and_child_signal_status(self):
        runner = load_runner()
        child = mock.Mock()
        child.wait.return_value = -15
        with mock.patch.object(runner.subprocess, "Popen", return_value=child):
            self.assertEqual(runner.main(["300", "owned-client"]), 143)
        child.wait.assert_called_once_with(timeout=300.0)


if __name__ == "__main__":
    unittest.main()
