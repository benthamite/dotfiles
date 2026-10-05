"""Tests for codex/bin/trust-epoch-workspace against disposable configs."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import io
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path
import sys
import tempfile
import tomllib
import unittest

DOTFILES = Path(__file__).resolve().parents[1]


def load_script():
    loader = importlib.machinery.SourceFileLoader(
        "trust_epoch_workspace", str(DOTFILES / "codex/bin/trust-epoch-workspace"))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[loader.name] = module
    loader.exec_module(module)
    return module


class TrustEpochWorkspaceTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.script = load_script()

    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name).resolve()
        self.workspace = self.root / "ai-enablement"
        for name in ("alpha", "beta", ".hidden"):
            (self.workspace / "projects" / name).mkdir(parents=True)
        (self.workspace / "projects" / "README.md").write_text("x")
        self.config = self.root / "config.toml"
        self.config.write_text(
            'model = "m"\n\n'
            f'[projects."{self.workspace}"]\ntrust_level = "trusted"\n')

    def run_script(self, *extra):
        out, err = io.StringIO(), io.StringIO()
        with redirect_stdout(out), redirect_stderr(err):
            code = self.script.main(["--workspace", str(self.workspace),
                                     "--config", str(self.config), *extra])
        return code, out.getvalue(), err.getvalue()

    def projects(self):
        return tomllib.loads(self.config.read_text())["projects"]

    def test_adds_missing_project_directories_once(self):
        code, out, _ = self.run_script()
        self.assertEqual(0, code)
        self.assertIn("added 2 of 3", out)
        expected = {str(self.workspace), str(self.workspace / "projects/alpha"),
                    str(self.workspace / "projects/beta")}
        self.assertEqual(expected, set(self.projects()))
        self.assertTrue(all(v == {"trust_level": "trusted"} for v in self.projects().values()))
        before = self.config.read_text()
        code, out, _ = self.run_script()
        self.assertIn("added 0 of 3", out)
        self.assertEqual(before, self.config.read_text())

    def test_dry_run_writes_nothing(self):
        before = self.config.read_text()
        code, out, _ = self.run_script("--dry-run")
        self.assertEqual(0, code)
        self.assertIn("would add 2 of 3", out)
        self.assertEqual(before, self.config.read_text())

    def test_refuses_a_workspace_that_does_not_exist(self):
        self.workspace = self.root / "absent"
        before = self.config.read_text()
        code, _, err = self.run_script()
        self.assertEqual(1, code)
        self.assertIn("does not exist", err)
        self.assertEqual(before, self.config.read_text())


if __name__ == "__main__":
    unittest.main()
