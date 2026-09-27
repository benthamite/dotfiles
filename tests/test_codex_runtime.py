"""Exercise immutable runtime publication and survival of upstream replacement."""

import concurrent.futures
import importlib.machinery
import importlib.util
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch


SCRIPT = Path(__file__).resolve().parents[1] / 'bin/codex-runtime'
LOADER = importlib.machinery.SourceFileLoader('codex_runtime', str(SCRIPT))
SPEC = importlib.util.spec_from_loader(LOADER.name, LOADER)
runtime = importlib.util.module_from_spec(SPEC)
LOADER.exec_module(runtime)


class RuntimeTests(unittest.TestCase):
    def test_browser_overrides_win_without_rewriting_literal_prompt(self):
        overrides = ['-c', 'mcp_servers.node_repl.env.browser_version="new"']
        for args in (['app-server', '--listen', 'stdio://', '-c', 'x=true'],
                     ['exec', 'task'], ['resume', 'session']):
            self.assertEqual(runtime.with_browser_overrides(args, overrides), args + overrides)
        self.assertEqual(runtime.with_browser_overrides(['exec', '--', '-literal'], overrides),
                         ['exec', *overrides, '--', '-literal'])

    def test_browser_reconciliation_runs_for_session_entry_points(self):
        for args in ([], ['exec', 'task'], ['resume', '--last'],
                     ['app-server'], ['-m', 'plugin', 'task'],
                     ['--cd', '/tmp/plugin', 'resume'], ['--', 'plugin']):
            with self.subTest(args=args):
                self.assertIsNotNone(runtime.browser_launch_options(args))

    def test_browser_reconciliation_does_not_block_maintenance(self):
        for args in (['--version'], ['--help'], ['plugin', 'list', '--json'],
                     ['-c', 'model="x"', 'mcp', 'get', 'node_repl'],
                     ['app-server', 'generate-json-schema'],
                     ['app-server', 'daemon', 'version'],
                     ['app-server', 'daemon', 'stop'],
                     ['app-server', 'proxy'],
                     ['exec', '--ignore-user-config', 'task'],
                     ['--remote', 'ws://localhost:9999']):
            with self.subTest(args=args):
                self.assertIsNone(runtime.browser_launch_options(args))

    def test_browser_reconciliation_preserves_global_config_overrides(self):
        self.assertEqual(runtime.browser_launch_options(
            ['-c', 'plugins.chrome.enabled=false', '--enable=example', 'resume']),
            ['-c', 'plugins.chrome.enabled=false', '--enable=example'])
        for args in (['resume', 'session', '-c', 'x=true'],
                     ['exec', 'task', '-c', 'x=true'],
                     ['app-server', '--listen', 'stdio://', '-c', 'x=true']):
            self.assertEqual(runtime.browser_launch_options(args), ['-c', 'x=true'])
        self.assertEqual(runtime.browser_launch_options(
            ['--cd', '/tmp', 'exec', 'task', '-cx=true']), ['--cd', '/tmp', '-cx=true'])
        self.assertEqual(runtime.browser_launch_options(
            ['-preview', '-C/tmp', 'exec', 'task']), ['-preview', '-C/tmp'])

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.source = self.root / 'source'
        self.store = self.root / 'retained'
        (self.source / 'bin').mkdir(parents=True)
        (self.source / 'codex-package.json').write_text('{"layoutVersion":1,"entrypoint":"bin/codex"}')
        self.write_program('codex', '#!/bin/sh\nprintf "ready\\n"\nread answer\n"$(dirname "$0")/codex-code-mode-host" "$answer"\n')
        self.write_program('codex-code-mode-host', '#!/bin/sh\nprintf "old:%s\\n" "$1"\n')
        (self.source / 'codex-resources').mkdir()
        (self.source / 'codex-resources/data').write_text('retained resource')

    def tearDown(self):
        for directory in sorted(self.root.rglob('*')):
            if not directory.is_symlink() and directory.is_dir():
                directory.chmod(0o700)
        self.temp.cleanup()

    def write_program(self, name, content):
        path = self.source / 'bin' / name
        path.write_text(content)
        path.chmod(0o755)

    def test_running_process_and_helper_survive_source_deletion(self):
        executable = runtime.snapshot(self.source, self.store)
        child = subprocess.Popen([str(executable)], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
        self.assertEqual(child.stdout.readline(), 'ready\n')
        shutil.rmtree(self.source)
        output, _ = child.communicate('preserved\n', timeout=5)
        self.assertEqual(child.returncode, 0)
        self.assertEqual(output, 'old:preserved\n')
        self.assertEqual((executable.parent.parent / 'codex-resources/data').read_text(), 'retained resource')

    def test_new_install_selects_new_snapshot_without_changing_old(self):
        original = runtime.snapshot(self.source, self.store)
        self.write_program('codex-code-mode-host', '#!/bin/sh\necho new\n')
        updated = runtime.snapshot(self.source, self.store)
        self.assertNotEqual(original, updated)
        self.assertIn('old:', (original.parent / 'codex-code-mode-host').read_text())
        self.assertIn('new', (updated.parent / 'codex-code-mode-host').read_text())

    def test_same_package_reuses_snapshot(self):
        first = runtime.snapshot(self.source, self.store)
        inode = first.stat().st_ino
        second = runtime.snapshot(self.source, self.store)
        self.assertEqual(first, second)
        self.assertEqual(inode, second.stat().st_ino)

    def test_concurrent_publication_has_one_complete_runtime(self):
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            results = list(pool.map(lambda _: runtime.snapshot(self.source, self.store), range(4)))
        self.assertEqual(len(set(results)), 1)
        self.assertEqual(len(list(self.store.glob('[0-9a-f]' * 64))), 1)
        self.assertFalse(list(self.store.glob('.stage-*')))

    def test_missing_helper_fails_explicitly(self):
        (self.source / 'bin/codex-code-mode-host').unlink()
        with self.assertRaisesRegex(runtime.RuntimeErrorDetected, 'missing bin/codex-code-mode-host'):
            runtime.snapshot(self.source, self.store)
        self.assertFalse(self.store.exists())

    def test_upstream_change_during_copy_does_not_publish(self):
        real_copy = shutil.copytree

        def changing_copy(source, target, *args, **kwargs):
            result = real_copy(source, target, *args, **kwargs)
            if Path(source) == self.source:
                self.write_program('codex-code-mode-host', '#!/bin/sh\necho changed\n')
            return result

        with patch.object(runtime.shutil, 'copytree', side_effect=changing_copy):
            with self.assertRaisesRegex(runtime.RuntimeErrorDetected, 'updated during snapshot'):
                runtime.snapshot(self.source, self.store)
        self.assertEqual([path.name for path in self.store.iterdir()], ['.publish.lock'])

    def test_damaged_published_runtime_is_not_overwritten_or_used(self):
        executable = runtime.snapshot(self.source, self.store)
        helper = executable.parent / 'codex-code-mode-host'
        helper.chmod(0o700)
        helper.write_text('damaged')
        with self.assertRaisesRegex(runtime.RuntimeErrorDetected, 'integrity check'):
            runtime.snapshot(self.source, self.store)
        self.assertEqual(helper.read_text(), 'damaged')

    def test_source_symlink_is_rejected(self):
        (self.source / 'unexpected').symlink_to('/tmp')
        with self.assertRaisesRegex(runtime.RuntimeErrorDetected, 'nonregular'):
            runtime.snapshot(self.source, self.store)


if __name__ == '__main__':
    unittest.main()
