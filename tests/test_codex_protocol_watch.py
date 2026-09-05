"""Exercise the scheduled watcher with isolated state and fake delivery tools.

These checks verify retry and acknowledgement bookkeeping, not live macOS or
Emacs notification receipt. No real delivery command is invoked.
"""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "bin" / "codex-protocol-watch"


class ProtocolWatchTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="protocol-watch-test-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.state = self.root / "state"
        self.commands = self.root / "commands"
        self.commands.mkdir()
        repo = self.root / "repo"
        (repo / "ground-truth").mkdir(parents=True)
        (repo / "ground-truth" / "protocol_coverage.py").write_text(
            "import os\n"
            "print('    thread/newMethod')\n"
            "raise SystemExit(int(os.environ.get('CHECK_STATUS', '1')))\n"
        )
        self.env = {
            **os.environ,
            "PATH": f"{self.commands}:/usr/bin:/bin",
            "CODEX_EL_REPO": str(repo),
            "CODEX_PROTOCOL_WATCH_STATE": str(self.state),
            "TEST_CALLS": str(self.root / "calls"),
            "MAC_STATUS": "1",
            "EMACS_STATUS": "1",
        }
        self.command("codex", "exit 0\n")
        for name, status in (("osascript", "MAC_STATUS"), ("emacsclient", "EMACS_STATUS")):
            self.command(
                name,
                f'echo {name} >> "$TEST_CALLS"\n'
                f'echo "{name} diagnostic" >&2\n'
                f'exit "${status}"\n',
            )

    def command(self, name, body):
        path = self.commands / name
        path.write_text("#!/bin/bash\n" + body)
        path.chmod(0o755)

    def run_watch(self, *args):
        return subprocess.run(
            ["/bin/bash", str(SCRIPT), *args],
            env=self.env,
            capture_output=True,
            text=True,
            timeout=15,
        )

    @property
    def signature(self):
        return self.state / "notified-signature"

    def calls(self):
        path = self.root / "calls"
        return path.read_text().splitlines() if path.exists() else []

    def test_total_failure_is_visible_and_retries_then_suppresses_after_success(self):
        failed = self.run_watch()
        self.assertEqual(failed.returncode, 1)
        self.assertIn("osascript diagnostic", failed.stdout)
        self.assertIn("emacsclient diagnostic", failed.stdout)
        self.assertFalse(self.signature.exists())
        self.assertEqual(len(self.calls()), 2)

        self.env["MAC_STATUS"] = "0"
        retried = self.run_watch()
        self.assertEqual(retried.returncode, 0, retried.stdout)
        self.assertTrue(self.signature.exists())
        self.assertEqual(len(self.calls()), 4)

        suppressed = self.run_watch()
        self.assertEqual(suppressed.returncode, 0)
        self.assertIn("staying quiet", suppressed.stdout)
        self.assertEqual(len(self.calls()), 4)

    def test_emacs_success_alone_acknowledges_delivery(self):
        self.env["EMACS_STATUS"] = "0"
        result = self.run_watch()
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertTrue(self.signature.exists())
        self.assertIn("macOS notification failed", result.stdout)

    def test_failed_new_finding_preserves_previous_signature(self):
        self.state.mkdir()
        self.signature.write_text("previous-finding")
        result = self.run_watch()
        self.assertEqual(result.returncode, 1)
        self.assertEqual(self.signature.read_text(), "previous-finding")

    def test_force_retries_acknowledged_finding(self):
        self.env["MAC_STATUS"] = "0"
        self.assertEqual(self.run_watch().returncode, 0)
        self.assertEqual(self.run_watch("--force").returncode, 0)
        self.assertEqual(len(self.calls()), 4)

    def test_clean_check_rearms_a_reappearing_finding(self):
        self.env["MAC_STATUS"] = "0"
        self.assertEqual(self.run_watch().returncode, 0)
        self.env["CHECK_STATUS"] = "0"
        self.assertEqual(self.run_watch().returncode, 0)
        self.assertFalse(self.signature.exists())
        self.assertEqual(len(self.calls()), 2)
        self.env["CHECK_STATUS"] = "1"
        self.assertEqual(self.run_watch().returncode, 0)
        self.assertEqual(len(self.calls()), 4)


if __name__ == "__main__":
    unittest.main()
