"""Offline broker checks using socketpairs and mocked CLI children; no accounts."""

import base64
import fcntl
import importlib.machinery
import importlib.util
import io
import os
import socket
import struct
import subprocess
import sys
import tempfile
import threading
import unittest
from pathlib import Path
from unittest.mock import patch


SCRIPT = Path(__file__).resolve().parents[1] / "bin" / "op-desktop"


class DesktopConcurrencyTest(unittest.TestCase):
    def setUp(self):
        loader = importlib.machinery.SourceFileLoader("desktop_concurrency_test", str(SCRIPT))
        spec = importlib.util.spec_from_loader(loader.name, loader)
        self.mod = importlib.util.module_from_spec(spec)
        loader.exec_module(self.mod)
        for replacement in (
            patch.object(self.mod, "log"),
            patch.object(self.mod.subprocess, "run", side_effect=AssertionError("no real CLI")),
        ):
            replacement.start()
            self.addCleanup(replacement.stop)
        self.threads = []
        self.addCleanup(self.join_workers)

    def join_workers(self):
        for worker in self.threads:
            worker.join(2)
            self.assertFalse(worker.is_alive(), "test worker did not finish")

    def pair(self):
        client, server = socket.socketpair()
        client.settimeout(2)
        self.addCleanup(client.close)
        self.addCleanup(server.close)
        return client, server

    def submit(self, requests, payload):
        client, server = self.pair()
        worker = requests.start(server)
        if worker is not None:
            self.threads.append(worker)
            self.assertEqual(self.mod.recv_json(client), {"ready": True})
            self.mod.send_json(client, payload)
        return client, worker

    def test_short_request_completes_while_long_child_runs_with_isolated_inputs(self):
        entered = threading.Event()
        release = threading.Event()
        calls = {}

        def child(argv, **kwargs):
            calls[argv[1]] = kwargs
            if argv[1] == "run":
                entered.set()
                if not release.wait(5):
                    raise AssertionError("long child was never released")
                return subprocess.CompletedProcess(argv, 0, b"slow", b"")
            return subprocess.CompletedProcess(argv, 0, b"fast", b"")

        requests = self.mod.BrokerRequests(slave_fd=123)
        with patch.object(self.mod.subprocess, "run", side_effect=child), \
             patch.dict(os.environ, {"OP_SERVICE_ACCOUNT_TOKEN": "SYNTHETIC_AMBIENT"}):
            slow, slow_worker = self.submit(requests, {
                "argv": ["run", "--", "synthetic-program"],
                "env": {"OP_ACCOUNT": "account-a", "OP_SERVICE_ACCOUNT_TOKEN": "SYNTHETIC"},
                "stdin_b64": base64.b64encode(b"SYNTHETIC_INPUT").decode(),
            })
            try:
                self.assertTrue(entered.wait(2))
                fast, fast_worker = self.submit(requests, {
                    "argv": ["item", "get", "synthetic-id"],
                    "env": {"OP_ACCOUNT": "account-b"},
                })
                self.assertEqual(self.mod.recv_json(fast), {"rc": 0, "out": "fast", "err": ""})
                self.assertTrue(slow_worker.is_alive())
                fast_worker.join(2)
                self.assertFalse(fast_worker.is_alive())
                self.assertEqual(calls["run"]["input"], b"SYNTHETIC_INPUT")
                self.assertEqual(calls["item"]["stdin"], 123)
                self.assertNotIn("input", calls["item"])
                self.assertEqual(calls["run"]["env"]["OP_ACCOUNT"], "account-a")
                self.assertEqual(calls["item"]["env"]["OP_ACCOUNT"], "account-b")
                for call in calls.values():
                    self.assertNotIn("OP_SERVICE_ACCOUNT_TOKEN", call["env"])
                self.assertEqual(os.environ["OP_SERVICE_ACCOUNT_TOKEN"], "SYNTHETIC_AMBIENT")
            finally:
                release.set()
                slow_worker.join(2)
            self.assertEqual(self.mod.recv_json(slow)["out"], "slow")

    def test_active_child_prevents_idle_exit_and_completion_restarts_idle_clock(self):
        entered = threading.Event()
        release = threading.Event()
        clock = [100.0]

        def hold(conn, slave_fd):
            with conn:
                self.mod.send_json(conn, {"ready": True})
                entered.set()
                release.wait(5)

        with patch.object(self.mod.time, "monotonic", side_effect=lambda: clock[0]), \
             patch.object(self.mod, "handle_connection", side_effect=hold):
            requests = self.mod.BrokerRequests(123)
            _, worker = self.submit(requests, {"argv": ["run"]})
            try:
                self.assertTrue(entered.wait(2))
                clock[0] += self.mod.IDLE_EXIT_SECONDS + 1
                self.assertFalse(requests.idle_expired())
            finally:
                release.set()
                worker.join(2)
            self.assertFalse(requests.idle_expired())
            clock[0] += self.mod.IDLE_EXIT_SECONDS + 1
            self.assertTrue(requests.idle_expired())

    def test_capacity_rejects_promptly_without_starting_or_cancelling_children(self):
        entered = threading.Event()
        release = threading.Event()

        def child(argv, **kwargs):
            entered.set()
            release.wait(5)
            return subprocess.CompletedProcess(argv, 0, b"", b"")

        requests = self.mod.BrokerRequests(123, limit=1)
        with patch.object(self.mod.subprocess, "run", side_effect=child) as run:
            first, worker = self.submit(requests, {"argv": ["run"]})
            try:
                self.assertTrue(entered.wait(2))
                second, rejected = self.submit(requests, {"argv": ["item", "get", "synthetic-id"]})
                self.assertIsNone(rejected)
                response = self.mod.recv_json(second)
                self.assertEqual(response["rc"], 75)
                self.assertIn("broker busy", response["err"])
                self.assertEqual(run.call_count, 1)
                self.assertTrue(worker.is_alive())
            finally:
                release.set()
                worker.join(2)
            self.assertEqual(self.mod.recv_json(first)["rc"], 0)
            third, new_worker = self.submit(requests, {"argv": ["item", "get", "synthetic-id"]})
            self.assertEqual(self.mod.recv_json(third)["rc"], 0)
            new_worker.join(2)
            self.assertEqual(requests.active, 0)

    def test_bad_request_and_disconnected_client_release_worker_slots(self):
        requests = self.mod.BrokerRequests(123, limit=1)
        client, server = self.pair()
        bad = b"{not JSON"
        client.sendall(struct.pack("!I", len(bad)) + bad)
        worker = requests.start(server)
        self.threads.append(worker)
        self.assertEqual(self.mod.recv_json(client), {"ready": True})
        self.assertEqual(self.mod.recv_json(client)["rc"], 2)
        worker.join(2)
        self.assertEqual(requests.active, 0)
        client, server = self.pair()
        client.close()
        worker = requests.start(server)
        self.threads.append(worker)
        worker.join(2)
        self.assertEqual(requests.active, 0)

    def test_slow_request_sender_times_out_without_retaining_a_slot(self):
        requests = self.mod.BrokerRequests(123, limit=1)
        client, server = self.pair()
        with patch.object(self.mod, "REQUEST_IO_TIMEOUT_SECONDS", 0.05):
            worker = requests.start(server)
            self.threads.append(worker)
            self.assertEqual(self.mod.recv_json(client), {"ready": True})
            self.assertEqual(self.mod.recv_json(client)["rc"], 2)
            worker.join(2)
        self.assertEqual(requests.active, 0)

    def test_busy_client_receives_explicit_error_before_uploading_large_template(self):
        requests = self.mod.BrokerRequests(123, limit=0)
        client, server = self.pair()
        requests.start(server)
        body = b"SYNTHETIC" * (128 * 1024)
        incoming = io.TextIOWrapper(io.BytesIO(body))
        with patch.object(self.mod, "ensure_runtime_dir"), \
             patch.object(self.mod, "connect_or_start", return_value=client), \
             patch.object(self.mod, "send_json", side_effect=AssertionError("no upload before admission")), \
             patch.object(sys, "stdin", incoming), \
             patch.object(sys, "stdout", io.StringIO()), \
             patch.object(sys, "stderr", io.StringIO()) as errors:
            self.assertEqual(self.mod.main(["item", "edit", "synthetic-id"]), 75)
        self.assertIn("broker busy", errors.getvalue())

    def test_runtime_paths_are_versioned_and_startup_lock_protects_existing_broker(self):
        runtime = Path(self.mod.RUNTIME_DIR)
        self.assertEqual(runtime.name, "v2")
        for name in ("SOCKET_PATH", "PID_PATH", "LOG_PATH", "LOCK_PATH"):
            self.assertEqual(Path(getattr(self.mod, name)).parent, runtime)
        with tempfile.TemporaryDirectory(prefix="desktop-broker-test-") as temp:
            lockpath = Path(temp) / "broker.lock"
            with patch.object(self.mod, "ensure_runtime_dir"), \
                 patch.object(self.mod, "LOCK_PATH", str(lockpath)), \
                 patch.object(self.mod, "serve_broker") as serve:
                with lockpath.open("a") as held:
                    fcntl.flock(held, fcntl.LOCK_EX | fcntl.LOCK_NB)
                    self.mod.run_broker()
                    serve.assert_not_called()
                self.mod.run_broker()
                serve.assert_called_once_with()

    def test_starting_client_does_not_unlink_a_broker_socket(self):
        sentinel = object()
        with patch.object(self.mod, "connect", side_effect=[None, sentinel]), \
             patch.object(self.mod, "spawn_broker") as spawn, \
             patch.object(self.mod.os, "unlink", side_effect=AssertionError("no client unlink")):
            self.assertIs(self.mod.connect_or_start(), sentinel)
        spawn.assert_called_once_with()


if __name__ == "__main__":
    unittest.main()
