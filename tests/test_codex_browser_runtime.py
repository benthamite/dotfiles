"""Retention must survive removal of the installation used by a live session."""
import concurrent.futures
import importlib.util
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('browser_runtime', Path(__file__).resolve().parents[1] / 'bin/codex_browser_runtime.py')
runtime = importlib.util.module_from_spec(spec)
spec.loader.exec_module(runtime)


class RetentionTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.source = self.root / 'source'
        (self.source / 'bin').mkdir(parents=True)
        (self.source / 'lib/node_modules').mkdir(parents=True)
        (self.source / 'lib/node_modules/value').write_text('original\n')
        for name in ('node', 'node_repl'):
            path = self.source / 'bin' / name
            path.write_text('#!/bin/sh\nexec /bin/cat "$(dirname "$0")/../lib/node_modules/value"\n')
            path.chmod(0o755)
        self.store = self.root / 'store'

    def tearDown(self):
        for directory in sorted(self.root.rglob('*'), key=lambda x: len(x.parts)):
            if directory.is_dir() and not directory.is_symlink():
                directory.chmod(0o700)
        self.temp.cleanup()

    def test_executable_survives_app_update_and_removal(self):
        retained = runtime.snapshot_runtime(self.source, self.store)
        (self.source / 'lib/node_modules/value').write_text('updated\n')
        newer = runtime.snapshot_runtime(self.source, self.store)
        self.assertNotEqual(retained, newer)
        shutil.rmtree(self.source)
        for tree, expected in ((retained, 'original\n'), (newer, 'updated\n')):
            self.assertEqual(subprocess.check_output([str(tree / 'bin/node_repl')], text=True), expected)
        self.assertFalse((retained / 'bin/node_repl').stat().st_mode & 0o222)

    def test_internal_relative_link_and_reuse(self):
        (self.source / 'bin/value').symlink_to('../lib/node_modules/value')
        first = runtime.snapshot_runtime(self.source, self.store)
        self.assertEqual((first / 'bin/value').read_text(), 'original\n')
        self.assertTrue((first / 'bin/value').is_symlink())
        self.assertEqual(runtime.snapshot_runtime(self.source, self.store), first)

    def test_escaping_links_rejected(self):
        outside = self.root / 'outside'
        outside.write_text('outside')
        for target in (str(outside), '../../outside'):
            link = self.source / 'bin/escape'
            link.symlink_to(target)
            with self.assertRaises(runtime.RuntimeSnapshotError):
                runtime.snapshot_runtime(self.source, self.store)
            link.unlink()

    def test_link_leaving_tree_then_returning_rejected(self):
        (self.root / 'return').symlink_to(self.source / 'lib/node_modules/value')
        (self.source / 'bin/escape').symlink_to('../../return')
        with self.assertRaises(runtime.RuntimeSnapshotError):
            runtime.snapshot_runtime(self.source, self.store)

    def test_broken_link_rejected(self):
        (self.source / 'bin/broken').symlink_to('missing')
        with self.assertRaises(runtime.RuntimeSnapshotError):
            runtime.snapshot_runtime(self.source, self.store)

    def test_corrupt_retained_file_rejected(self):
        retained = runtime.snapshot_runtime(self.source, self.store)
        path = retained / 'lib/node_modules/value'
        path.chmod(0o600)
        path.write_text('corrupted')
        with self.assertRaises(runtime.RuntimeSnapshotError):
            runtime.snapshot_runtime(self.source, self.store)

    def test_concurrent_publication_returns_one_tree(self):
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            paths = list(pool.map(lambda _: runtime.snapshot_runtime(self.source, self.store), range(8)))
        self.assertEqual(len(set(paths)), 1)
        self.assertFalse(list(self.store.glob('.stage-*')))

    def test_source_mutation_during_copy_rejected_and_stage_removed(self):
        real_copy = shutil.copytree
        def copy_then_mutate(*args, **kwargs):
            result = real_copy(*args, **kwargs)
            (self.source / 'lib/node_modules/value').write_text('changed')
            return result
        with patch.object(runtime.shutil, 'copytree', side_effect=copy_then_mutate):
            with self.assertRaises(runtime.RuntimeSnapshotError):
                runtime.snapshot_runtime(self.source, self.store)
        self.assertFalse(list(self.store.glob('.stage-*')))
        self.assertEqual([p.name for p in self.store.iterdir()], ['.publish.lock'])

    def test_app_cli_sibling_survives_update_and_removal(self):
        resources = self.root / 'Resources'
        resources.mkdir()
        cli = resources / 'codex'
        helper = resources / 'codex-code-mode-host'
        cli.write_text('#!/bin/sh\nexec "$(dirname "$0")/codex-code-mode-host"\n')
        helper.write_text('#!/bin/sh\nprintf original\n')
        cli.chmod(0o755)
        helper.chmod(0o755)
        (resources / 'unrelated').write_text('exclude')
        retained = runtime.snapshot_app_cli(resources, self.store)
        self.assertEqual(sorted(p.name for p in retained.iterdir()), ['codex', 'codex-code-mode-host'])
        helper.write_text('#!/bin/sh\nprintf updated\n')
        newer = runtime.snapshot_app_cli(resources, self.store)
        shutil.rmtree(resources)
        self.assertNotEqual(retained, newer)
        self.assertEqual(subprocess.check_output([str(retained / 'codex')], text=True), 'original')
        self.assertEqual(subprocess.check_output([str(newer / 'codex')], text=True), 'updated')

    def test_app_cli_rejects_symlink_and_missing_helper(self):
        resources = self.root / 'Resources'
        resources.mkdir()
        (resources / 'codex').symlink_to(self.source / 'bin/node')
        with self.assertRaises(runtime.RuntimeSnapshotError):
            runtime.snapshot_app_cli(resources, self.store)
        (resources / 'codex').unlink()
        shutil.copy2(self.source / 'bin/node', resources / 'codex')
        with self.assertRaises(FileNotFoundError):
            runtime.snapshot_app_cli(resources, self.store)

    def test_app_cli_rejects_source_change_during_copy(self):
        resources = self.root / 'Resources'
        resources.mkdir()
        for name in ('codex', 'codex-code-mode-host'):
            shutil.copy2(self.source / 'bin/node', resources / name)
        real_copy = shutil.copy2
        def copy_then_mutate(source, target, **kwargs):
            result = real_copy(source, target, **kwargs)
            Path(source).write_text('changed')
            return result
        with patch.object(runtime.shutil, 'copy2', side_effect=copy_then_mutate):
            with self.assertRaises(runtime.RuntimeSnapshotError):
                runtime.snapshot_app_cli(resources, self.store)
        self.assertFalse(self.store.exists())

    def test_generic_plugin_tree(self):
        plugin = self.root / 'chrome'
        (plugin / 'scripts').mkdir(parents=True)
        (plugin / 'scripts/browser-service.mjs').write_text('export const service = true;')
        retained = runtime.snapshot_tree(plugin, self.store)
        shutil.rmtree(plugin)
        self.assertEqual((retained / 'scripts/browser-service.mjs').read_text(), 'export const service = true;')


if __name__ == '__main__':
    unittest.main()
