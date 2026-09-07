"""Offline broker-client transport checks; no socket, process or credential access."""

import base64
import importlib.machinery
import importlib.util
import io
import sys
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch


SCRIPT = Path(__file__).resolve().parents[1] / "bin" / "op-desktop"


class DesktopStdinTest(unittest.TestCase):
    def setUp(self):
        loader = importlib.machinery.SourceFileLoader("desktop_stdin_test", str(SCRIPT))
        spec = importlib.util.spec_from_loader(loader.name, loader)
        self.mod = importlib.util.module_from_spec(spec)
        loader.exec_module(self.mod)

    def invoke(self, argv, body=b'{"id":"synthetic-id","fields":[]}', tty=False, limit=None):
        incoming = MagicMock()
        incoming.isatty.return_value = tty
        incoming.buffer = io.BytesIO(body)
        conn = MagicMock()
        with patch.object(self.mod, "ensure_runtime_dir"), \
             patch.object(self.mod, "connect_or_start", return_value=conn), \
             patch.object(self.mod, "send_json") as send, \
             patch.object(self.mod, "recv_json", return_value={"rc": 0}), \
             patch.object(self.mod.socket, "socket", side_effect=AssertionError("no sockets")), \
             patch.object(self.mod.subprocess, "run", side_effect=AssertionError("no subprocess")), \
             patch.object(self.mod, "MAX_STDIN_BYTES", limit or self.mod.MAX_STDIN_BYTES), \
             patch.object(sys, "stdin", incoming), \
             patch.object(sys, "stdout", io.StringIO()), \
             patch.object(sys, "stderr", io.StringIO()):
            result = self.mod.main(argv)
        request = send.call_args.args[1] if send.called else None
        return result, request, incoming

    def test_documented_edit_by_id_forwards_template_without_dummy_dash(self):
        args = ["item", "edit", "synthetic-id", "--vault", "Automations"]
        body = b'{"id":"synthetic-id","fields":[{"value":"SYNTHETIC"}]}'
        rc, request, _ = self.invoke(args, body)
        self.assertEqual(rc, 0)
        self.assertEqual(request["argv"], args)
        self.assertEqual(base64.b64decode(request["stdin_b64"]), body)
        self.assertNotIn("SYNTHETIC", repr(request["argv"]))

    def test_create_dash_retains_existing_stdin_contract(self):
        rc, request, _ = self.invoke(["item", "create", "-", "--vault", "Automations"])
        self.assertEqual(rc, 0)
        self.assertIn("stdin_b64", request)

    def test_explicit_account_preserves_create_and_edit_stdin(self):
        body = b'{"fields":[{"id":"credential","value":"SYNTHETIC"}]}'
        for operation, target in (("create", "-"), ("edit", "a" * 26)):
            with self.subTest(operation=operation):
                argv = ["item", operation, target, "--vault", "v" * 26,
                        "--format", "json", "--account", "epoch-team.1password.com"]
                rc, request, _ = self.invoke(argv, body)
                self.assertEqual(rc, 0)
                self.assertEqual(request["argv"], argv)
                self.assertEqual(base64.b64decode(request["stdin_b64"]), body)
                self.assertNotIn("SYNTHETIC", repr(request["argv"]))

    def test_other_commands_and_interactive_input_do_not_consume_stdin(self):
        for argv, tty in [(["whoami"], False), (["item", "get", "synthetic-id"], False),
                          (["item", "edit", "--help"], False),
                          (["item", "edit", "synthetic-id"], True)]:
            with self.subTest(argv=argv, tty=tty):
                rc, request, incoming = self.invoke(argv, tty=tty)
                self.assertEqual(rc, 0)
                self.assertNotIn("stdin_b64", request)
                self.assertEqual(incoming.buffer.tell(), 0)

    def test_oversized_edit_stdin_refuses_before_connect(self):
        rc, request, _ = self.invoke(["item", "edit", "synthetic-id"], b"12345", limit=4)
        self.assertEqual(rc, 2)
        self.assertIsNone(request)


if __name__ == "__main__":
    unittest.main()
