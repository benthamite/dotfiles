"""Exercise the real macOS policy boundary with disposable app data and CLI."""

import importlib.util
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location(
    "op_automations_sandbox", ROOT / "bin/op_automations_sandbox.py"
)
sandbox = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(sandbox)


@unittest.skipUnless(sys.platform == "darwin", "requires the macOS sandbox")
class InheritedSandboxTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.home = Path(self.temporary.name).resolve()
        self.container = self.home / "Library/Group Containers/2BUA8C4S2C.com.1password"
        self.container.mkdir(parents=True)
        (self.container / "private-data").write_text("fixture")
        (self.container / ".com.apple.containermanagerd.metadata.plist").write_text("fixture")
        self.repo = self.home / "repo"
        self.repo.mkdir()
        (self.repo / "source").write_text("unchanged")
        self.cli = self.home / "op"
        self.cli.write_text(
            "#!/usr/bin/env python3\n"
            "import os\n"
            "from pathlib import Path\n"
            "home = Path.home()\n"
            "container = home / 'Library/Group Containers/2BUA8C4S2C.com.1password'\n"
            "try:\n"
            "    (container / 'private-data').read_text()\n"
            "except PermissionError:\n"
            "    print('app-data denied')\n"
            "else:\n"
            "    raise SystemExit('app-data unexpectedly readable')\n"
            "if os.environ.get('TEST_OUTER_SANDBOX'):\n"
            "    try:\n"
            "        (home / 'repo/source').write_text('changed')\n"
            "    except PermissionError:\n"
            "        print('repository write denied')\n"
            "    else:\n"
            "        raise SystemExit('outer sandbox lost')\n"
            "print('CLI ran')\n"
        )
        self.cli.chmod(0o755)
        self.env = dict(os.environ, HOME=str(self.home),
                        PATH=f"{self.home}:{os.environ['PATH']}",
                        OP_SERVICE_ACCOUNT_TOKEN="test-fixture-token")

    def run_wrapper(self, outer=None):
        command = [str(ROOT / "bin/op-automations"), "--version"]
        if outer:
            command = ["/usr/bin/sandbox-exec", "-p", outer, *command]
        return subprocess.run(command, env=self.env, capture_output=True,
                              text=True, timeout=15)

    def test_unsandboxed_caller_gets_app_data_protection(self):
        result = self.run_wrapper()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("app-data denied", result.stdout)
        self.assertIn("CLI ran", result.stdout)

    def test_inherited_protection_keeps_outer_write_denial(self):
        outer = (f'(version 1)(allow default)(deny file-write* (subpath "{self.repo}"))'
                 + sandbox.profile(sandbox.container_paths(self.home)))
        self.env["TEST_OUTER_SANDBOX"] = "1"
        result = self.run_wrapper(outer)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("repository write denied", result.stdout)
        self.assertIn("app-data denied", result.stdout)
        self.assertEqual((self.repo / "source").read_text(), "unchanged")

    def test_unprotected_outer_sandbox_cannot_bypass_protection(self):
        outer = f'(version 1)(allow default)(deny file-write* (subpath "{self.repo}"))'
        result = self.run_wrapper(outer)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Operation not permitted", result.stderr)
        self.assertNotIn("CLI ran", result.stdout)

    def test_literal_directory_denial_does_not_count_as_subtree_protection(self):
        outer = (f'(version 1)(allow default)'
                 f'(deny file-read* file-write* (literal "{self.container}"))')
        result = self.run_wrapper(outer)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Operation not permitted", result.stderr)
        self.assertNotIn("CLI ran", result.stdout)

    def test_broker_child_can_reuse_its_inherited_protection(self):
        outer = '(version 1)(allow default)' + sandbox.profile(sandbox.container_paths(self.home))
        result = self.run_wrapper(outer)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("app-data denied", result.stdout)


if __name__ == "__main__":
    unittest.main()
