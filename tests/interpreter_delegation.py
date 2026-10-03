"""Run a test module under the interpreter that owns its dependencies.

Some tools here run in their own Python environment (gcalcli's Homebrew
virtualenv, gdoc's uv tool environment), so their tests can only import the
code under that interpreter. Loaded by any other Python, such a module skips
its real tests and runs one test instead: the whole module under the owning
interpreter, failing if that interpreter is missing or any test fails. A
missing environment is therefore a failure, never a silent skip.
"""

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[1]


def delegated_case(module: str, python: Path) -> type[unittest.TestCase]:
    """A TestCase that runs tests.MODULE under PYTHON and reports its result."""

    class DelegatedRun(unittest.TestCase):
        def test_module_under_owning_interpreter(self):
            self.assertTrue(os.access(python, os.X_OK), f"{python} is missing; {module} cannot run")
            result = subprocess.run(
                [str(python), "-m", "unittest", f"tests.{module}"],
                cwd=ROOT, capture_output=True, text=True, timeout=600,
                env={**os.environ, "PYTHONDONTWRITEBYTECODE": "1"},
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    DelegatedRun.__qualname__ = DelegatedRun.__name__ = f"DelegatedRun_{module}"
    return DelegatedRun
