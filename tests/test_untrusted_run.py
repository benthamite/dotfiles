"""Live Docker Desktop VM acceptance, using synthetic data and no host secrets."""

import json
import argparse
import importlib.machinery
import importlib.util
import os
from pathlib import Path
import socket
import subprocess
import sys
import tempfile
import time
import unittest
from unittest import mock


RUNNER = Path(__file__).resolve().parents[1] / "bin/untrusted-run"


@unittest.skipUnless(sys.platform == "darwin", "Requires actual Docker Desktop on macOS")
class UntrustedRunTests(unittest.TestCase):
    def test_seccomp_blocks_kernel_socket_escape_families(self):
        with tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
            probe = '''for my $spec ([38,5],[40,1],[2,1]) {
                my ($family,$type)=@$spec; my $ok=socket(my $s,$family,$type,0);
                print "$family ",($ok?1:0)," ",(0+$!),"\\n"; close($s) if $ok;
            }'''
            result = subprocess.run(
                [str(RUNNER), "--workspace", str(Path(temp) / "output"), "--", "/usr/bin/perl", "-e", probe],
                capture_output=True, text=True, timeout=40,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            rows = [list(map(int, row.split())) for row in result.stdout.splitlines()]
            self.assertEqual(rows[:2], [[38, 0, 1], [40, 0, 1]])
            self.assertEqual(rows[2][:2], [2, 1])

    def test_cleanup_errors_are_failures_and_still_reap_client(self):
        loader = importlib.machinery.SourceFileLoader("isolator_under_test", str(RUNNER))
        spec = importlib.util.spec_from_loader(loader.name, loader)
        module = importlib.util.module_from_spec(spec)
        loader.exec_module(module)
        for cleanup in (subprocess.CompletedProcess([], 1), subprocess.TimeoutExpired("docker rm", 30)):
            with self.subTest(cleanup=type(cleanup).__name__), tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
                process = mock.Mock()
                process.wait.return_value = 0
                process.poll.return_value = None
                results = [subprocess.CompletedProcess([], 0, stdout="29.1.3\n"),
                           subprocess.CompletedProcess([], 0), cleanup]
                with mock.patch.object(module.subprocess, "run", side_effect=results), \
                     mock.patch.object(module.subprocess, "Popen", return_value=process):
                    with self.assertRaisesRegex(ValueError, "Cleanup could not confirm"):
                        module.run(argparse.Namespace(command=["node", "--version"],
                            workspace=str(Path(temp) / "output"), read=[], timeout=10))
                process.kill.assert_called_once()
                self.assertEqual(process.wait.call_count, 2)

    def test_live_files_environment_processes_and_network_boundary(self):
        with tempfile.TemporaryDirectory(prefix="isolation-test-", dir="/private/tmp") as temp:
            root = Path(temp)
            inputs = root / "input"
            inputs.mkdir()
            outside = root / "outside"
            outside.write_text("synthetic outside canary")
            (inputs / "dependency").write_text("synthetic read-only dependency")
            (inputs / "escape").symlink_to(outside)
            for name in (".git", ".codex", ".claude", ".ssh"):
                (root / name).mkdir()
                (root / name / "canary").write_text("synthetic guard")
            (root / ".env").write_text("synthetic credential")
            with socket.socket() as tcp, socket.socket(socket.AF_UNIX) as unix:
                tcp.bind(("127.0.0.1", 0))
                tcp.listen()
                socket_path = str(root / "listener.sock")
                unix.bind(socket_path)
                unix.listen()
                target = subprocess.Popen(
                    [sys.executable, "-I", "-c", "import time; time.sleep(60)", "synthetic-host-argument-418"],
                    env={"ISOLATION_TEST_CANARY": "synthetic-host-environment-619"},
                    stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                )
                self.addCleanup(target.wait)
                self.addCleanup(target.terminate)
                probe = inputs / "probe.js"
                probe.write_text('''const fs = require('node:fs'), net = require('node:net');
const [outside, socketPath, port] = process.argv.slice(2);
function denied(operation) {
  try { operation(); return false; }
  catch (error) { return ['EPERM','EACCES','EROFS','ENOENT'].includes(error.code); }
}
async function blocked(address) {
  return await new Promise(resolve => {
    const connection = net.connect(address);
    connection.setTimeout(1000);
    connection.on('connect', () => { connection.destroy(); resolve(false); });
    connection.on('error', () => resolve(true));
    connection.on('timeout', () => { connection.destroy(); resolve(false); });
  });
}
(async () => {
  const results = {};
  results.outside_read = denied(() => fs.readFileSync(outside));
  results.outside_write = denied(() => fs.writeFileSync(outside, 'changed'));
  results.symlink_read = denied(() => fs.readFileSync('/inputs/probe/escape'));
  results.symlink_write = denied(() => fs.writeFileSync('/inputs/probe/escape', 'changed'));
  results.dependency_read = fs.readFileSync('/inputs/probe/dependency','utf8') === 'synthetic read-only dependency';
  results.dependency_write = denied(() => fs.writeFileSync('/inputs/probe/dependency', 'changed'));
  const hostRoot = require('node:path').dirname(outside);
  for (const name of ['.git','.codex','.claude','.ssh','.env']) {
    const path = hostRoot + '/' + name + (name === '.env' ? '' : '/canary');
    results[name + '_read'] = denied(() => fs.readFileSync(path));
    results[name + '_write'] = denied(() => fs.writeFileSync(path, 'changed'));
  }
  fs.writeFileSync('/workspace/output', 'legitimate output');
  results.workspace_rw = fs.readFileSync('/workspace/output','utf8') === 'legitimate output';
  fs.writeFileSync(process.env.HOME + '/home-write', 'temporary');
  fs.writeFileSync(process.env.TMPDIR + '/tmp-write', 'temporary');
  results.clean_environment = ['SYNTHETIC_API_TOKEN','SSH_AUTH_SOCK','DOCKER_HOST','PYTHONPATH','NODE_OPTIONS','BASH_ENV']
    .every(name => !(name in process.env));
  results.nonroot = process.getuid() !== 0;
  results.readonly_root = denied(() => fs.writeFileSync('/etc/isolation-canary', 'changed'));
  results.no_host_socket = denied(() => fs.readFileSync('/var/run/docker.sock'));
  results.private_home = process.env.HOME === '/tmp/home';
  results.stdin_closed = fs.readFileSync(0).length === 0;
  let inherited = false;
  for (const pid of fs.readdirSync('/proc').filter(name => /^[0-9]+$/.test(name))) {
    for (const file of ['cmdline','environ']) {
      try {
        const data = fs.readFileSync('/proc/' + pid + '/' + file).toString();
        inherited ||= data.includes('synthetic-host-argument-418') || data.includes('synthetic-host-environment-619');
      } catch (_) {}
    }
  }
  results.host_process_environment_hidden = !inherited;
  results.tcp = await blocked({host:'127.0.0.1',port:Number(port)});
  results.external_network = await blocked({host:'1.1.1.1',port:443});
  results.unix = await blocked({path:socketPath});
  console.log(JSON.stringify(results));
})();
''')
                environment = os.environ.copy()
                environment.update({
                    "SYNTHETIC_API_TOKEN": "not-a-real-secret", "SSH_AUTH_SOCK": socket_path,
                    "DOCKER_HOST": "unix://" + socket_path, "PYTHONPATH": str(root),
                    "NODE_OPTIONS": "--require=/nonexistent", "BASH_ENV": str(root / "not-loaded"),
                })
                result = subprocess.run(
                    [str(RUNNER), "--workspace", str(root / "output"), "--read", "probe=" + str(inputs),
                     "--", "node", "/inputs/probe/probe.js", str(outside), socket_path, str(tcp.getsockname()[1])],
                    env=environment, capture_output=True, text=True, timeout=45,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                decisions = json.loads(result.stdout)
                self.assertTrue(decisions)
                self.assertTrue(all(decisions.values()), decisions)
                self.assertEqual(outside.read_text(), "synthetic outside canary")
                self.assertEqual((inputs / "dependency").read_text(), "synthetic read-only dependency")

    def test_timeout_removes_daemonized_descendant(self):
        with tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
            workspace = Path(temp) / "output"
            code = '''require('node:child_process').spawn('node', ['-e',
              'setTimeout(() => require("fs").writeFileSync("/workspace/escaped", "bad"), 2500)'],
              {detached:true,stdio:'ignore'}).unref(); setInterval(() => {}, 1000);'''
            result = subprocess.run(
                [str(RUNNER), "--workspace", str(workspace), "--timeout", "1", "--", "node", "-e", code],
                capture_output=True, text=True, timeout=40,
            )
            self.assertEqual(result.returncode, 124, result.stderr)
            time.sleep(3)
            self.assertFalse((workspace / "escaped").exists())

    def test_existing_directory_and_symlink_are_refused(self):
        with tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
            root = Path(temp)
            existing = root / "existing"
            existing.mkdir()
            marker = existing / ".git"
            marker.write_text("unchanged")
            link = root / "link"
            link.symlink_to(existing)
            for destination in (existing, link):
                result = subprocess.run(
                    [str(RUNNER), "--workspace", str(destination), "--", "node", "-e", "process.exit(99)"],
                    capture_output=True, text=True, timeout=10,
                )
                self.assertEqual(result.returncode, 2, result.stderr)
            self.assertEqual(marker.read_text(), "unchanged")


if __name__ == "__main__":
    unittest.main()
