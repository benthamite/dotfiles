"""Exercise descriptor transport and lifecycle using harmless local fixtures."""

import importlib.util
import json
import os
from pathlib import Path
import pty
import select
import signal
import subprocess
import sys
import tempfile
import time
import tty
import unittest


BROKER = Path(__file__).resolve().parents[1] / "bin/codex_process_broker.py"
SPEC = importlib.util.spec_from_file_location("codex_process_broker", BROKER)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class BrokerTests(unittest.TestCase):
    def setUp(self):
        # Resolve /var on macOS before handing the deliberately strict broker a path.
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name).resolve()
        self.runtime = self.root / "runtime"
        self.fixture = self.root / "fixture"
        self.fixture.write_text(
            f"#!{sys.executable}\n"
            "import json, os, subprocess, sys, time\n"
            "if sys.argv[1:2] == ['wait']:\n"
            "    child = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(60)'])\n"
            "    print(json.dumps([os.getpid(), child.pid]), flush=True)\n"
            "    time.sleep(60)\n"
            "elif sys.argv[1:2] == ['tty']:\n"
            "    os.write(1, os.read(0, 5))\n"
            "    os.write(2, b'tty-stderr')\n"
            "    sys.exit(9)\n"
            "else:\n"
            "    print(json.dumps({'argv': sys.argv[1:], 'cwd': os.getcwd(), 'value': os.getenv('BROKER_TEST_VALUE')}), flush=True)\n"
            "    sys.stdout.buffer.write(sys.stdin.buffer.read())\n"
            "    sys.stderr.buffer.write(b'stderr\\x00bytes')\n"
            "    sys.exit(7)\n")
        self.fixture.chmod(0o700)
        self.daemon = subprocess.Popen(
            [sys.executable, str(BROKER), "--runtime", str(self.runtime),
             "serve", "--binary", str(self.fixture)],
            stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        deadline = time.monotonic() + 5
        while not (self.runtime / "broker.sock").exists():
            if self.daemon.poll() is not None:
                self.fail(self.daemon.communicate()[1].decode())
            if time.monotonic() > deadline:
                self.fail("broker did not start")
            time.sleep(0.02)
        self.clients = []

    def tearDown(self):
        for child in self.clients:
            if child.poll() is None:
                child.terminate()
            child.communicate(timeout=5)
        if self.daemon.poll() is None:
            self.daemon.terminate()
        self.daemon.communicate(timeout=15)
        self.temporary.cleanup()

    def start_client(self, *argv, **kwargs):
        env = dict(os.environ, BROKER_TEST_VALUE="private-test-value")
        process = subprocess.Popen(
            [sys.executable, str(BROKER), "--runtime", str(self.runtime),
             "client", "--", *argv], cwd=self.root, env=env, stdin=subprocess.PIPE,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, **kwargs)
        self.clients.append(process)
        return process

    def assert_gone(self, pid):
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            try:
                os.kill(pid, 0)
            except ProcessLookupError:
                return
            # Linux containers may leave adopted zombies until their init reaps them.
            state = subprocess.run(["ps", "-o", "stat=", "-p", str(pid)],
                                   capture_output=True, text=True).stdout.strip()
            if not state or state.startswith("Z"):
                return
            time.sleep(0.02)
        self.fail(f"worker {pid} still running")

    def test_streams_environment_cwd_exit_and_leading_flags(self):
        process = self.start_client("--ask-for-approval", "never", "exec", "literal ; $(text)")
        stdout, stderr = process.communicate(b"input\x00\xffbytes", timeout=5)
        self.assertEqual(process.returncode, 7, stderr)
        metadata, data = stdout.split(b"\n", 1)
        self.assertEqual(json.loads(metadata), {
            "argv": ["--ask-for-approval", "never", "exec", "literal ; $(text)"],
            "cwd": str(self.root), "value": "private-test-value"})
        self.assertEqual(data, b"input\x00\xffbytes")
        self.assertEqual(stderr, b"stderr\x00bytes")

    def test_disconnect_cleans_own_group_without_affecting_other_client(self):
        first = self.start_client("wait")
        first_pids = json.loads(first.stdout.readline())
        second = self.start_client("wait")
        second_pids = json.loads(second.stdout.readline())
        first.terminate()
        first.wait(timeout=5)
        for pid in first_pids:
            self.assert_gone(pid)
        for pid in second_pids:
            os.kill(pid, 0)
        second.terminate()
        second.wait(timeout=5)
        for pid in second_pids:
            self.assert_gone(pid)

    def test_tty_descriptor_bytes_and_exit_status(self):
        master, slave = pty.openpty()
        tty.setraw(slave)
        try:
            process = subprocess.Popen(
                [sys.executable, str(BROKER), "--runtime", str(self.runtime),
                 "client", "--", "tty"], stdin=slave, stdout=slave, stderr=slave)
            self.clients.append(process)
            os.close(slave)
            slave = None
            os.write(master, b"t\x00\xffty")
            output = bytearray()
            deadline = time.monotonic() + 5
            expected = b"t\x00\xfftytty-stderr"
            while len(output) < len(expected) and time.monotonic() < deadline:
                ready, _, _ = select.select([master], [], [], 0.1)
                if ready:
                    output.extend(os.read(master, 4096))
            self.assertEqual(bytes(output), expected)
            self.assertEqual(process.wait(timeout=5), 9)
        finally:
            os.close(master)
            if slave is not None:
                os.close(slave)

    def test_daemon_termination_cleans_workers(self):
        process = self.start_client("wait")
        pids = json.loads(process.stdout.readline())
        second = self.start_client("wait")
        pids.extend(json.loads(second.stdout.readline()))
        self.daemon.terminate()
        self.daemon.wait(timeout=5)
        for pid in pids:
            self.assert_gone(pid)
        _, stderr = process.communicate(timeout=5)
        self.assertEqual(process.returncode, 125)
        self.assertIn(b"refusing local execution", stderr)
        second.communicate(timeout=5)
        self.assertEqual(second.returncode, 125)

    def test_absent_broker_never_runs_fixture(self):
        self.daemon.terminate()
        self.daemon.wait(timeout=5)
        process = self.start_client("wait")
        stdout, stderr = process.communicate(timeout=5)
        self.assertEqual(stdout, b"")
        self.assertEqual(process.returncode, 125)
        self.assertIn(b"refusing local execution", stderr)

    def test_private_paths_and_symlink_rejection(self):
        self.assertEqual(self.runtime.stat().st_mode & 0o777, 0o700)
        self.assertEqual((self.runtime / "broker.sock").stat().st_mode & 0o777, 0o600)
        link = self.root / "alias"
        link.symlink_to(self.runtime, target_is_directory=True)
        with self.assertRaises(RuntimeError):
            MODULE.private_directory(link)
        self.runtime.chmod(0o755)
        with self.assertRaises(RuntimeError):
            MODULE.private_directory(self.runtime)
        self.runtime.chmod(0o700)

    def test_second_daemon_does_not_replace_socket(self):
        inode = (self.runtime / "broker.sock").stat().st_ino
        second = subprocess.run(
            [sys.executable, str(BROKER), "--runtime", str(self.runtime),
             "serve", "--binary", str(self.fixture)], capture_output=True, timeout=5)
        self.assertEqual(second.returncode, 125)
        self.assertEqual((self.runtime / "broker.sock").stat().st_ino, inode)


if __name__ == "__main__":
    unittest.main()
