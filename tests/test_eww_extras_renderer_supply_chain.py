"""Supply-chain tests for the headless EWW renderer."""

from __future__ import annotations

import json
import os
import stat
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
PROJECT = ROOT / "emacs/extras/scripts/eww-extras-renderer"
RUNNER = PROJECT / "run.sh"


class RendererSupplyChainTests(unittest.TestCase):
    def test_manifest_pins_exact_runtime_dependencies(self) -> None:
        manifest = json.loads((PROJECT / "package.json").read_text())

        self.assertTrue(manifest["private"])
        self.assertEqual(manifest["engines"], {"node": ">=20"})
        self.assertEqual(
            manifest["dependencies"],
            {
                "@duckduckgo/autoconsent": "16.20.1",
                "playwright-core": "1.62.1",
            },
        )

    def test_lockfile_matches_manifest_and_has_integrity_hashes(self) -> None:
        lock = json.loads((PROJECT / "package-lock.json").read_text())

        self.assertEqual(lock["packages"][""]["dependencies"], {
            "@duckduckgo/autoconsent": "16.20.1",
            "playwright-core": "1.62.1",
        })
        self.assertEqual(
            lock["packages"]["node_modules/@duckduckgo/autoconsent"]["version"],
            "16.20.1",
        )
        self.assertEqual(
            lock["packages"]["node_modules/playwright-core"]["version"],
            "1.62.1",
        )
        for name, package in lock["packages"].items():
            if name:
                self.assertIn("integrity", package, name)

    def test_wrapper_uses_locked_scriptless_install_outside_source(self) -> None:
        source = RUNNER.read_text()

        self.assertIn('"$npm_program" ci --ignore-scripts', source)
        self.assertNotIn("npx", source)
        self.assertIn("Library/Caches/eww-extras-renderer", source)
        self.assertIn("package-lock.json", source)
        self.assertNotIn("$script_dir/node_modules", source)
        self.assertTrue(RUNNER.stat().st_mode & stat.S_IXUSR)

    def test_cold_cache_is_published_once_without_partial_state(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            cache = root / "cache"
            fake_npm = root / "npm"
            count_file = root / "npm-count"
            fake_npm.write_text(
                "#!/bin/sh\n"
                "set -eu\n"
                f"printf x >> {count_file!s}\n"
                "test \"$1 $2\" = 'ci --ignore-scripts'\n"
                "mkdir -p node_modules/playwright-core\n"
                "mkdir -p node_modules/@duckduckgo/autoconsent\n"
                "printf '%s\\n' '{\"version\":\"1.62.1\"}' "
                "> node_modules/playwright-core/package.json\n"
                "printf '%s\\n' '{\"version\":\"16.20.1\"}' "
                "> node_modules/@duckduckgo/autoconsent/package.json\n"
            )
            fake_npm.chmod(0o700)
            env = os.environ | {
                "EWW_EXTRAS_RENDERER_CACHE_DIR": str(cache),
                "EWW_EXTRAS_RENDERER_NPM": str(fake_npm),
            }

            first = subprocess.run(
                [RUNNER, "self-check"],
                check=False,
                capture_output=True,
                text=True,
                env=env,
            )
            second = subprocess.run(
                [RUNNER, "self-check"],
                check=False,
                capture_output=True,
                text=True,
                env=env,
            )

            self.assertEqual(first.returncode, 0, first.stderr)
            self.assertEqual(second.returncode, 0, second.stderr)
            self.assertEqual(count_file.read_text(), "x")
            self.assertEqual(list(cache.glob(".lock-*")), [])
            self.assertEqual(list(cache.glob(".staging-*")), [])
            completed = [path for path in cache.iterdir() if not path.name.startswith(".")]
            self.assertEqual(len(completed), 1)


if __name__ == "__main__":
    unittest.main()
