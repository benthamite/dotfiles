"""Account resolution shared by Emacs and unattended scripts."""

import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
COMMAND = ROOT / "bin" / "agent-account-home"
ACCOUNTS = ROOT / "emacs" / "agent-accounts.json"


def run(*args, accounts=None):
    extra = ["--accounts-file", str(accounts)] if accounts else []
    return subprocess.run([sys.executable, str(COMMAND), *args, *extra],
                          capture_output=True, text=True, timeout=30)


class AgentAccountHomeTest(unittest.TestCase):
    def write(self, value):
        handle = tempfile.NamedTemporaryFile("w", suffix=".json", delete=False)
        self.addCleanup(os.unlink, handle.name)
        json.dump(value, handle)
        handle.close()
        return handle.name

    def fixture(self):
        return self.write({
            "claude": [{"name": "solo", "home": "~/.claude-solo"}],
            "codex": [{"name": "plain", "home": "~/.codex-plain"},
                      {"name": "member", "home": "~/.codex-member", "pool": "the-pool"}],
        })

    def test_plain_account_prints_expanded_home(self):
        accounts = self.fixture()
        for backend, name, home in (("codex", "plain", "~/.codex-plain"),
                                    ("codex", "member", "~/.codex-member"),
                                    ("claude", "solo", "~/.claude-solo")):
            completed = run(backend, name, accounts=accounts)
            self.assertEqual(completed.returncode, 0, completed.stderr)
            self.assertEqual(completed.stdout.strip(), os.path.expanduser(home))

    def test_pool_name_is_refused_with_its_own_status(self):
        completed = run("codex", "the-pool", accounts=self.fixture())
        self.assertEqual(completed.returncode, 2)
        self.assertEqual(completed.stdout, "")
        self.assertIn("pool routing needs the running Emacs; name a plain account",
                      completed.stderr)

    def test_unknown_account_and_wrong_backend_fail(self):
        accounts = self.fixture()
        self.assertEqual(run("codex", "absent", accounts=accounts).returncode, 1)
        self.assertEqual(run("claude", "plain", accounts=accounts).returncode, 1)

    def test_invalid_account_files_fail_closed(self):
        for value in ({"codex": []},
                      {"claude": [], "codex": [{"name": "x"}]},
                      {"claude": [], "codex": [{"name": "x", "home": "~/x", "extra": 1}]},
                      {"claude": [], "codex": [{"name": "x", "home": "~/x"},
                                               {"name": "x", "home": "~/y"}]},
                      {"claude": [], "codex": [{"name": "x", "home": "~/x", "pool": "x"}]}):
            with self.subTest(value=value):
                completed = run("codex", "x", accounts=self.write(value))
                self.assertEqual(completed.returncode, 1)
                self.assertEqual(completed.stdout, "")

    def test_repository_account_file_is_valid(self):
        data = json.loads(ACCOUNTS.read_text())
        for backend, entries in data.items():
            for entry in entries:
                completed = run(backend, entry["name"])
                self.assertEqual(completed.returncode, 0, completed.stderr)
            for pool in {entry["pool"] for entry in entries if "pool" in entry}:
                self.assertEqual(run(backend, pool).returncode, 2)


if __name__ == "__main__":
    unittest.main()
