"""Actual wrapper bytes against owned inert helper/clipboard executables only."""

import hashlib
import json
import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WRAPPER = ROOT / "claude/bin/ai-provider-key-from-clipboard"
HELPER = ROOT / "claude/bin/ai-provider-key"
SECRET = "sk-ant-" + "SYNTHETIC" * 5


class ProviderClipboardTest(unittest.TestCase):
    def setUp(self):
        self.root = Path(tempfile.mkdtemp(prefix="provider-clipboard-test-", dir="/private/tmp"))
        self.addCleanup(self.cleanup)
        shutil.copy2(WRAPPER, self.root / WRAPPER.name)
        self.initial = hashlib.sha256(WRAPPER.read_bytes()).hexdigest()
        # Shim validates through the real helper's pure-plan entry point only.
        # Actual store calls are intercepted here before importing any helper.
        helper = self.root / "ai-provider-key"
        helper.write_text("#!/usr/bin/python3 -I\n" + """
import importlib.machinery, importlib.util, json, os, pathlib, socket, sys
sys.dont_write_bytecode = True
def audit(event, args):
    if event.startswith('socket.') or event == 'subprocess.Popen':
        raise RuntimeError('offline shim forbids network/subprocess')
sys.addaudithook(audit)
base = pathlib.Path(__file__).parent
if '--dry-run' in sys.argv or '--help' in sys.argv or '-h' in sys.argv:
    loader = importlib.machinery.SourceFileLoader('provider_pure_fixture', os.environ['SYNTHETIC_HELPER_SOURCE'])
    spec = importlib.util.spec_from_loader(loader.name, loader)
    mod = importlib.util.module_from_spec(spec)
    sys.modules[loader.name] = mod
    loader.exec_module(mod)
    raise SystemExit(mod.main())
payload = sys.stdin.read()
(base / 'record.json').write_text(json.dumps({'argv': sys.argv[1:], 'input': payload}))
print('stored op://Automations/synthetic/credential')
""")
        helper.chmod(0o700)
        clipboard = self.root / "pbpaste"
        clipboard.write_text("#!/usr/bin/python3 -I\n" + """
import pathlib, sys
base = pathlib.Path(__file__).parent
(base / 'clipboard-read').write_text('owned fake was invoked')
sys.stdout.write((base / 'candidate.txt').read_text())
""")
        clipboard.chmod(0o700)
        (self.root / "candidate.txt").write_text(SECRET)

    def cleanup(self):
        self.assertEqual(hashlib.sha256(WRAPPER.read_bytes()).hexdigest(), self.initial)
        result = subprocess.run(["/opt/homebrew/bin/trash", str(self.root)], capture_output=True, text=True, timeout=20)
        self.assertEqual(result.returncode, 0)
        self.assertFalse(os.path.lexists(self.root))

    def invoke(self, *args):
        return subprocess.run(["/bin/bash", "--noprofile", "--norc", str(self.root / WRAPPER.name), *args],
                              cwd=self.root, env={"PATH": f"{self.root}:/usr/bin:/bin", "SYNTHETIC_HELPER_SOURCE": str(HELPER)},
                              capture_output=True, text=True, timeout=15)

    def standard(self):
        return ["--provider", "anthropic", "--project", "synthetic", "--verified-monthly-spend-cap-usd", "1500"]

    def test_valid_anthropic_clipboard_is_stdin_only(self):
        result = self.invoke(*self.standard())
        self.assertEqual(result.returncode, 0, result.stderr)
        record = json.loads((self.root / "record.json").read_text())
        self.assertEqual(record["input"], SECRET)
        self.assertIn("--secret-stdin", record["argv"])
        self.assertNotIn(SECRET, repr(record["argv"]) + result.stdout + result.stderr)
        self.assertTrue((self.root / "clipboard-read").exists())

    def test_help_and_pure_plan_never_read_clipboard_or_store(self):
        for args in (["--help"], [*self.standard(), "--dry-run"]):
            result = self.invoke(*args)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertFalse((self.root / "clipboard-read").exists())
            self.assertFalse((self.root / "record.json").exists())

    def test_openai_invalid_args_and_missing_cap_refuse_before_clipboard(self):
        for args in (["--provider", "openai", "--project", "synthetic"],
                     ["--provider", "anthropic", "--project", "synthetic"],
                     [*self.standard(), "--unknown"], [*self.standard(), "--secret-file", "synthetic"],
                     [*self.standard(), "--provider", "anthropic"],
                     ["--provider", "anthropic", "--project", "synthetic", "--verified-monthly-spend-cap-usd", "nan"]):
            with self.subTest(args=args):
                result = self.invoke(*args)
                self.assertNotEqual(result.returncode, 0)
                self.assertFalse((self.root / "clipboard-read").exists())
                self.assertFalse((self.root / "record.json").exists())

    def test_trailing_newline_controls_and_bad_prefix_never_reach_helper(self):
        for candidate in (SECRET + "\n", SECRET + "\n\n", SECRET + "\r", SECRET + " ", "sk-" + "x" * 50):
            (self.root / "candidate.txt").write_bytes(candidate.encode())
            result = self.invoke(*self.standard())
            self.assertNotEqual(result.returncode, 0)
            self.assertFalse((self.root / "record.json").exists())
            self.assertNotIn(candidate, result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
