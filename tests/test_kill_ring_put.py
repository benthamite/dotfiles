"""Checks of the kill-ring-put helper and its staging form.

The Lisp form is exercised in a standalone ``emacs -Q --batch`` with every
clipboard callback replaced at the interprogram boundary; no server, OS
clipboard, credentials, user buffers or saved kill history are touched. The
command-line paths are tested with the Emacs client and ``pbcopy`` mocked.
"""

import importlib.machinery
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import stat
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "claude" / "bin" / "kill-ring-put"
EMACS = shutil.which("emacs")


def load_module():
    loader = importlib.machinery.SourceFileLoader("kill_ring_put", str(SCRIPT))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[loader.name] = module
    loader.exec_module(module)
    return module


@unittest.skipUnless(EMACS, "Native batch Emacs is required")
class StagingFormTests(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()
        temporary = tempfile.TemporaryDirectory(prefix="kill-ring-put-fixture-", dir="/tmp")
        self.addCleanup(temporary.cleanup)
        self.directory = Path(temporary.name).resolve()
        self.payload = self.directory / 'payload with ñ and "quotes".txt'
        form = self.mod.STAGING_FORM
        self.assertEqual(form.count('"/ABSOLUTE/PRIVATE/PAYLOAD"'), 1)
        self.form = form.replace('"/ABSOLUTE/PRIVATE/PAYLOAD"', json.dumps(str(self.payload), ensure_ascii=False))

    def write_payload(self, text):
        with self.payload.open("xb") as stream:
            os.fchmod(stream.fileno(), 0o600)
            stream.write(text.encode("utf-8"))

    def native(self, body, *, transform="nil"):
        expression = f"""(let ((kill-ring (list "fixture-prior-kill"))
                              (kill-ring-yank-pointer nil)
                              (kill-transform-function {transform})
                              (kill-ring-max 60)
                              (kill-do-not-save-duplicates nil)
                              (save-interprogram-paste-before-kill t)
                              (interprogram-cut-function
                               (lambda (_) (error "Unexpected clipboard export")))
                              (interprogram-paste-function
                               (lambda () (error "Unexpected clipboard import"))))
                         (setq kill-ring-yank-pointer kill-ring)
                         {body})"""
        result = subprocess.run([EMACS, "-Q", "--batch", "--eval", expression],
                                cwd=self.directory, capture_output=True, text=True,
                                timeout=10, check=False)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, "")
        return result.stdout

    def test_form_preserves_text_point_and_pointer_without_clipboard_calls(self):
        payloads = ['Quotes " and slash \\ with ñ and λ', "first\r\nsecond\r\n", "  first\nsecond\t "]
        for payload in payloads:
            with self.subTest(payload=repr(payload)):
                self.write_payload(payload)
                expected = json.dumps(payload, ensure_ascii=False)
                result = self.native(f"""(with-temp-buffer
                                           (insert "fixture-buffer")
                                           (goto-char 4)
                                           (let* ((original (current-buffer))
                                                  (value {self.form}))
                                             (prin1 (list (eq value 'paste-staged)
                                                          (equal-including-properties (car kill-ring) {expected})
                                                          (eq kill-ring-yank-pointer kill-ring)
                                                          (eq original (current-buffer))
                                                          (= (point) 4)
                                                          (equal (buffer-string) "fixture-buffer")
                                                          (equal (cdr kill-ring) '("fixture-prior-kill"))))))""")
                self.assertEqual(result, "(t t t t t t t)")
                self.payload.unlink()

    def test_form_returns_fixed_acknowledgement_not_text(self):
        payload = "fixture-private-content-must-not-be-returned"
        self.write_payload(payload)
        result = self.native(f"(prin1 {self.form})")
        self.assertEqual(result, self.mod.ACK)
        self.assertNotIn(payload, result)

    def test_transform_rejection_and_missing_file_are_not_silent_success(self):
        payload = "fixture-input-not-for-diagnostics"
        cases = [('(lambda (_) "fixture-transformed")', True), ('(lambda (_) nil)', True),
                 ('(lambda (text) (error "Fixture transform saw %s" text))', True), ("nil", False)]
        for transform, write in cases:
            with self.subTest(transform=transform, written=write):
                if write:
                    self.write_payload(payload)
                result = self.native(f"""(condition-case failure
                                           (prin1 {self.form})
                                         (error (princ (error-message-string failure))))""",
                                     transform=transform)
                self.assertEqual(result, "Paste staging failed")
                self.assertNotIn(payload, result)
                self.assertNotIn(str(self.payload), result)
                if write:
                    self.payload.unlink()


class CommandTests(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()

    def run_main(self, argv, stdin=b"", env_socket=True):
        calls = []

        def fake_run(command, **kwargs):
            calls.append((command, kwargs))
            if command[0] == "emacsclient":
                payload_path = Path(json.loads(command[-1].split('(insert-file-contents ')[1].split(")")[0]))
                self.assertTrue(payload_path.exists())
                self.assertEqual(stat.S_IMODE(payload_path.stat().st_mode), 0o600)
                self.assertEqual(stat.S_IMODE(payload_path.parent.stat().st_mode), 0o700)
                self.seen_payload = payload_path.read_bytes()
                return subprocess.CompletedProcess(command, 0, stdout=self.mod.ACK + "\n", stderr="")
            return subprocess.CompletedProcess(command, 0, stdout=b"", stderr=b"")

        out, err = io.StringIO(), io.StringIO()
        stdin_buffer = mock.Mock()
        stdin_buffer.buffer = io.BytesIO(stdin)
        patches = [
            mock.patch.object(self.mod.subprocess, "run", side_effect=fake_run),
            mock.patch.object(self.mod.sys, "argv", ["kill-ring-put", *argv]),
            mock.patch.object(self.mod.sys, "stdin", stdin_buffer),
            mock.patch.object(self.mod.sys, "stdout", out),
            mock.patch.object(self.mod.sys, "stderr", err),
            mock.patch.object(self.mod.shutil, "which", return_value="/usr/bin/pbcopy"),
        ]
        if env_socket:
            patches.append(mock.patch.object(self.mod, "default_socket", return_value=Path("/tmp/fixture-socket")))
        else:
            patches.append(mock.patch.object(self.mod, "default_socket", return_value=None))
        with mock.patch.multiple(self.mod, **{}):
            for patch in patches:
                patch.start()
                self.addCleanup(patch.stop)
            code = self.mod.main()
        return code, out.getvalue(), err.getvalue(), calls

    def test_stdin_strips_one_trailing_newline_and_reports_without_payload(self):
        code, out, err, calls = self.run_main([], stdin="fixture text ñ\n".encode())
        self.assertEqual((code, out, err), (0, "staged: kill-ring\n", ""))
        self.assertEqual(self.seen_payload, "fixture text ñ".encode())
        self.assertEqual([c[0][0] for c in calls], ["emacsclient"])
        self.assertIn("--socket-name", calls[0][0])
        self.assertEqual(calls[0][0][calls[0][0].index("--socket-name") + 1], "/tmp/fixture-socket")

    def test_keep_newline_and_clipboard(self):
        code, out, _, calls = self.run_main(["--keep-newline", "--clipboard"], stdin=b"line\n")
        self.assertEqual((code, out), (0, "staged: kill-ring, clipboard\n"))
        self.assertEqual(self.seen_payload, b"line\n")
        self.assertEqual([c[0][0] for c in calls], ["emacsclient", "pbcopy"])

    def test_missing_server_and_empty_payload_fail_cleanly(self):
        code, out, err, calls = self.run_main([], stdin=b"text", env_socket=False)
        self.assertEqual((code, out, calls), (1, "", []))
        self.assertIn("No running Emacs server", err)
        code, out, err, calls = self.run_main([], stdin=b"  \n")
        self.assertEqual((code, out, calls), (1, "", []))
        self.assertIn("nonempty", err)

    def test_unacknowledged_or_timed_out_client_is_not_success(self):
        for effect, expected_code, marker in [
            (lambda *a, **k: subprocess.CompletedProcess(a[0], 0, stdout="fixture-secret\n", stderr=""), 1, "failed"),
            (subprocess.TimeoutExpired("emacsclient", 30), 2, "uncertain"),
        ]:
            with self.subTest(marker=marker):
                out, err = io.StringIO(), io.StringIO()
                stdin_buffer = mock.Mock()
                stdin_buffer.buffer = io.BytesIO(b"fixture-secret")
                with mock.patch.object(self.mod.subprocess, "run", side_effect=effect), \
                        mock.patch.object(self.mod, "default_socket", return_value=Path("/tmp/fixture-socket")), \
                        mock.patch.object(self.mod.sys, "argv", ["kill-ring-put"]), \
                        mock.patch.object(self.mod.sys, "stdin", stdin_buffer), \
                        mock.patch.object(self.mod.sys, "stdout", out), \
                        mock.patch.object(self.mod.sys, "stderr", err):
                    code = self.mod.main()
                self.assertEqual(code, expected_code)
                self.assertEqual(out.getvalue(), "")
                self.assertTrue(err.getvalue().startswith(marker))
                self.assertNotIn("fixture-secret", err.getvalue())

    def test_default_socket_prefers_env_then_tmpdir_and_requires_owned_socket(self):
        with tempfile.TemporaryDirectory(dir="/tmp") as directory:
            root = Path(directory)
            uid = os.getuid()
            regular = root / "regular"
            regular.write_text("not a socket")
            server_dir = root / f"emacs{uid}"
            server_dir.mkdir()
            sock_path = server_dir / "server"
            import socket as socket_module
            listener = socket_module.socket(socket_module.AF_UNIX)
            self.addCleanup(listener.close)
            listener.bind(str(sock_path))
            with mock.patch.dict(os.environ, {"TMPDIR": str(root), "EMACS_SOCKET_NAME": str(regular)}):
                self.assertEqual(self.mod.default_socket(), sock_path)
            with mock.patch.dict(os.environ, {"TMPDIR": str(root), "EMACS_SOCKET_NAME": str(sock_path)}):
                self.assertEqual(self.mod.default_socket(), sock_path)
            with mock.patch.dict(os.environ, {"TMPDIR": str(root / "missing")}, clear=False):
                os.environ.pop("EMACS_SOCKET_NAME", None)
                with mock.patch.object(Path, "stat", side_effect=OSError):
                    self.assertIsNone(self.mod.default_socket())


if __name__ == "__main__":
    unittest.main()
