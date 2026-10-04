import importlib.machinery
import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
loader = importlib.machinery.SourceFileLoader('playwright_mcp_launch',
                                              str(ROOT / 'bin/playwright-mcp-launch'))
spec = importlib.util.spec_from_loader('playwright_mcp_launch', loader)
launch = importlib.util.module_from_spec(spec)
loader.exec_module(launch)


class ProfileChoiceTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        for name in ('Default', 'Profile 2', 'Profile 9', 'System Profile'):
            (self.root / name).mkdir()

    def install(self, profile):
        (self.root / profile / 'Extensions' / launch.EXTENSION_ID).mkdir(parents=True)

    def last_used(self, profile):
        (self.root / 'Local State').write_text(json.dumps({'profile': {'last_used': profile}}))

    def test_last_used_profile_wins_when_it_has_the_extension(self):
        self.install('Profile 2')
        self.install('Profile 9')
        self.last_used('Profile 9')
        self.assertEqual(launch.chosen_profile(self.root), 'Profile 9')

    def test_falls_back_to_first_profile_with_the_extension(self):
        self.install('Profile 9')
        self.install('Profile 2')
        self.last_used('Default')
        self.assertEqual(launch.chosen_profile(self.root), 'Profile 2')

    def test_no_profile_with_the_extension(self):
        self.last_used('Default')
        self.assertIsNone(launch.chosen_profile(self.root))


class TokenTests(unittest.TestCase):
    def reader(self, stdout, code=0):
        def run(argv, **_):
            self.argv = argv
            return subprocess.CompletedProcess(argv, code, stdout=stdout)
        return run

    def test_reads_the_profile_item_and_strips_the_copy_prefix(self):
        token = launch.read_token('Profile 9', self.reader('PLAYWRIGHT_MCP_EXTENSION_TOKEN=abc\n'))
        self.assertEqual(token, 'abc')
        self.assertEqual(self.argv[1:4], ['@personal', 'cache', 'read'])
        self.assertEqual(self.argv[4], 'op://Automation/playwright-mcp-token-profile-9/credential')

    def test_missing_item_means_no_token(self):
        self.assertIsNone(launch.read_token('Default', self.reader('', code=1)))


if __name__ == '__main__':
    unittest.main()
