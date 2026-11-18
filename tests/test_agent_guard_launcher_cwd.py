"""The guard launchers must not import modules from the working directory.

With `python -c` the working directory is first on sys.path, so a file there
named like a standard-library module (venture-lab's `select.py`, 2026-11-18)
replaced the real one, the engine crashed on import, and every tool was denied.
"""

from __future__ import annotations

import json
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SHADOWED = ("select", "subprocess", "json", "re", "os")
PAYLOAD = json.dumps({"tool_name": "Bash", "tool_input": {"command": "pwd"},
                      "hook_event_name": "PreToolUse", "cwd": "/tmp"})


class LauncherWorkingDirectoryTests(unittest.TestCase):
    def test_launchers_ignore_stdlib_named_modules_in_the_working_directory(self):
        for tool in ("claude", "codex"):
            with self.subTest(tool=tool), tempfile.TemporaryDirectory() as cwd:
                for name in SHADOWED:
                    Path(cwd, f"{name}.py").write_text('raise ImportError("shadowed stdlib module")\n')
                result = subprocess.run(
                    ["bash", str(ROOT / tool / "hooks" / "agent-guard.sh")],
                    input=PAYLOAD, capture_output=True, text=True, cwd=cwd, timeout=60)
                self.assertNotIn("shadowed stdlib module", result.stderr)
                self.assertNotIn("Traceback", result.stderr)
                self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    unittest.main()
