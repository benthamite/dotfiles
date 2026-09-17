"""Synthetic input-grant checks that never start Docker or inspect credentials."""

import importlib.machinery
import importlib.util
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest import mock


RUNNER = Path(__file__).resolve().parents[1] / "bin/untrusted-run"


def runner_module():
    loader = importlib.machinery.SourceFileLoader("untrusted_input_checks", str(RUNNER))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


class UntrustedInputTests(unittest.TestCase):
    def test_nested_protected_files_and_directories_are_refused_before_docker(self):
        cases = ((".env", "file"), (".ENV.local", "file"),
                 (".claude.json", "file"), (".git", "directory"),
                 (".codex-epoch5", "directory"), (".claude-personal", "directory"),
                 (".env", "symlink"))
        for name, kind in cases:
            with self.subTest(name=name, kind=kind), tempfile.TemporaryDirectory() as temp:
                root = Path(temp).resolve()
                source = root / "input"
                nested = source / "nested"
                nested.mkdir(parents=True, mode=0o700)
                protected = nested / name
                if kind == "directory":
                    protected.mkdir(mode=0o700)
                elif kind == "symlink":
                    protected.symlink_to("missing-synthetic-target")
                else:
                    protected.write_text("synthetic fixture only")
                    protected.chmod(0o600)
                module = runner_module()
                with mock.patch.object(module.subprocess, "run") as control, \
                     mock.patch.object(module.subprocess, "Popen") as launch:
                    with self.assertRaisesRegex(ValueError, "stage only the required files"):
                        module.safe_host_path(str(source))
                control.assert_not_called()
                launch.assert_not_called()

    def test_profile_and_case_variants_are_refused_as_direct_inputs(self):
        for name in (".codex-epoch5", ".claude-personal", ".claude.json", ".ENV.local"):
            with self.subTest(name=name), tempfile.TemporaryDirectory() as temp:
                source = Path(temp).resolve() / name
                source.mkdir(mode=0o700)
                with self.assertRaisesRegex(ValueError, "not valid inputs"):
                    runner_module().safe_host_path(str(source))

    def test_minimal_staged_tree_is_accepted(self):
        with tempfile.TemporaryDirectory() as temp:
            source = Path(temp).resolve() / "input"
            (source / "src").mkdir(parents=True, mode=0o700)
            (source / "src/probe.js").write_text("console.log('synthetic fixture')")
            (source / "src/probe.js").chmod(0o600)
            self.assertEqual(runner_module().safe_host_path(str(source)), source)

    def test_incomplete_descendant_inventory_is_refused(self):
        if os.geteuid() == 0:
            self.skipTest("Root can read directories regardless of these permission bits")
        with tempfile.TemporaryDirectory() as temp:
            source = Path(temp).resolve() / "input"
            source.mkdir(mode=0o700)
            inaccessible = source / "unreadable"
            inaccessible.mkdir(mode=0o700)
            inaccessible.chmod(0)
            try:
                with self.assertRaisesRegex(ValueError, "completely inspectable"):
                    runner_module().safe_host_path(str(source))
            finally:
                inaccessible.chmod(0o700)

    @unittest.skipUnless(os.uname().sysname == "Darwin", "Runner requires macOS")
    def test_real_cli_refuses_nested_credential_grant_without_creating_output(self):
        with tempfile.TemporaryDirectory(dir="/private/tmp") as temp:
            root = Path(temp)
            source = root / "input"
            source.mkdir(mode=0o700)
            (source / ".env").write_text("synthetic fixture only")
            (source / ".env").chmod(0o600)
            destination = root / "output"
            result = subprocess.run(
                [str(RUNNER), "--workspace", str(destination), "--read", "source=" + str(source),
                 "--", "node", "--version"], capture_output=True, text=True, timeout=5,
            )
            self.assertEqual(result.returncode, 2, result.stderr)
            self.assertIn("stage only the required files", result.stderr)
            self.assertFalse(destination.exists())


if __name__ == "__main__":
    unittest.main()
