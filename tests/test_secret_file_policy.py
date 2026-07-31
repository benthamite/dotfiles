from __future__ import annotations

import subprocess
import unittest
from pathlib import Path


DOTFILES = Path("/Users/pablostafforini/My Drive/dotfiles")


def ignored(path: str) -> bool:
    result = subprocess.run(
        ["git", "check-ignore", "--no-index", "--quiet", "--", path],
        cwd=DOTFILES,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        check=False,
    )
    if result.returncode not in (0, 1):
        raise AssertionError(result.stderr)
    return result.returncode == 0


class SecretFilePolicyTests(unittest.TestCase):
    def test_environment_secret_variants_are_ignored(self):
        for path in (
            ".env",
            ".env.local",
            ".env.development",
            ".env.production.local",
            "nested/.env",
            "nested/.env.local",
            "nested/.env.development",
            "nested/.env.production.local",
        ):
            with self.subTest(path=path):
                self.assertTrue(ignored(path), f"{path} should be ignored")

    def test_environment_templates_remain_trackable(self):
        for path in (
            ".env.example",
            ".env.op",
            "nested/.env.example",
            "nested/.env.op",
        ):
            with self.subTest(path=path):
                self.assertFalse(ignored(path), f"{path} should remain trackable")


if __name__ == "__main__":
    unittest.main()
