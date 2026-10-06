"""Exercise on-disk package resolution through the real CLI.

emacsclient is replaced by a recording stub so these checks prove the on-disk
mode never consults Emacs; they do not exercise the live-registry mode.
"""
from __future__ import annotations

import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


HELPER = Path(__file__).resolve().parents[1] / "bin/elpaca-package-resolve"


class OnDiskResolveTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="elpaca-package-resolve-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name).resolve()
        self.profiles = self.root / "profiles"
        self.sources = self.profiles / "1.0.0" / "elpaca" / "sources"
        self.sources.mkdir(parents=True)
        (self.profiles / "active").symlink_to(self.profiles / "1.0.0")
        self.checkout = self.sources / "fixture"
        self.checkout.mkdir()
        subprocess.run(["git", "init", "-q", str(self.checkout)], check=True)
        self.dotfiles = self.root / "dotfiles"
        (self.dotfiles / "emacs" / "extras").mkdir(parents=True)
        commands = self.root / "commands"
        commands.mkdir()
        stub = commands / "emacsclient"
        stub.write_text('#!/bin/sh\necho called >> "$EMACSCLIENT_CALLS"\nexit 1\n')
        stub.chmod(0o755)
        self.calls = self.root / "emacsclient-calls"
        self.env = dict(
            os.environ,
            PATH=f"{commands}:{os.environ['PATH']}",
            EMACS_PROFILES_DIR=str(self.profiles),
            DOTFILES_ROOT=str(self.dotfiles),
            EMACSCLIENT_CALLS=str(self.calls),
        )

    def resolve(self, identifier):
        return subprocess.run([str(HELPER), "--on-disk", identifier], env=self.env,
                              text=True, capture_output=True, timeout=10, check=False)

    def assert_refused(self, result):
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")

    def test_resolves_checkout_in_active_profile_without_emacs(self):
        result = self.resolve("fixture")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout), {
            "label": "fixture", "id": "fixture",
            "source": str(self.checkout), "repo": str(self.checkout)})
        self.assertFalse(self.calls.exists())

    def test_follows_active_link_to_the_current_profile(self):
        newer = self.profiles / "2.0.0" / "elpaca" / "sources" / "fixture"
        newer.mkdir(parents=True)
        subprocess.run(["git", "init", "-q", str(newer)], check=True)
        (self.profiles / "active").unlink()
        (self.profiles / "active").symlink_to(self.profiles / "2.0.0")
        result = self.resolve("fixture")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout)["source"], str(newer))

    def test_missing_checkout_fails_loudly(self):
        result = self.resolve("absent")
        self.assert_refused(result)
        self.assertIn("No Elpaca checkout named absent", result.stderr)
        self.assertFalse(self.calls.exists())

    def test_missing_active_profile_fails_loudly(self):
        (self.profiles / "active").unlink()
        result = self.resolve("fixture")
        self.assert_refused(result)
        self.assertIn("No active Emacs profile", result.stderr)

    def test_non_git_directory_is_not_a_checkout(self):
        (self.sources / "plain").mkdir()
        self.assert_refused(self.resolve("plain"))

    def test_dot_segments_are_rejected(self):
        for identifier in [".", ".."]:
            with self.subTest(identifier=identifier):
                self.assert_refused(self.resolve(identifier))


if __name__ == "__main__":
    unittest.main()
