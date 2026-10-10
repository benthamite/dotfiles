"""The CODEX_HOME lookup in shell/.zshenv agrees with bin/agent-account-home.

The lookup parses emacs/agent-accounts.json with zsh builtins only, because
every shell runs it. These tests run just that block of .zshenv (not the
whole file, which sources secrets) in a fake home and compare its answer with
the Python resolver for every Codex account in the real list.
"""

import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
ZSHENV = ROOT / "shell" / ".zshenv"
ACCOUNTS = ROOT / "emacs" / "agent-accounts.json"
RESOLVER = ROOT / "bin" / "agent-account-home"
START = "# Make terminal Codex use the account selected in Emacs."


def codex_block():
    lines = ZSHENV.read_text().splitlines()
    start = lines.index(START)
    end = next(i for i in range(start, len(lines)) if lines[i] == "fi")
    return "\n".join(lines[start:end + 1]) + "\n"


class ZshenvCodexHomeTest(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory(prefix="zshenv-codex-")
        self.addCleanup(temp.cleanup)
        self.home = Path(temp.name) / "home"
        self.home.mkdir()
        self.dotfiles = Path(temp.name) / "dotfiles"
        (self.dotfiles / "emacs").mkdir(parents=True)
        shutil.copy(ACCOUNTS, self.dotfiles / "emacs" / "agent-accounts.json")
        self.block = Path(temp.name) / "block.zsh"
        self.block.write_text(codex_block())

    def lookup(self, marker, codex_home=None):
        if marker is not None:
            (self.home / ".codex-current-account").write_text(marker + "\n")
        env = {"HOME": str(self.home), "PATH": "/usr/bin:/bin", "DOTFILES": str(self.dotfiles)}
        if codex_home:
            env["CODEX_HOME"] = codex_home
        out = subprocess.run(["zsh", "-f", "-c", f'source "{self.block}"; print -r -- "${{CODEX_HOME-unset}}"'],
                             env=env, capture_output=True, text=True, timeout=30)
        self.assertEqual(out.stderr, "")
        return out.stdout.strip()

    def test_every_codex_account_matches_the_python_resolver(self):
        accounts = json.loads(ACCOUNTS.read_text())["codex"]
        self.assertTrue(accounts)
        for entry in accounts:
            with self.subTest(account=entry["name"]):
                expected = subprocess.run(
                    [sys.executable, str(RESOLVER), "codex", entry["name"]],
                    env={**os.environ, "HOME": str(self.home)},
                    capture_output=True, text=True, check=True).stdout.strip()
                Path(expected).mkdir(parents=True, exist_ok=True)
                self.assertEqual(self.lookup(entry["name"]), expected)

    def test_surrounding_whitespace_in_the_marker_is_ignored(self):
        name = json.loads(ACCOUNTS.read_text())["codex"][0]["name"]
        home = json.loads(ACCOUNTS.read_text())["codex"][0]["home"].replace("~", str(self.home), 1)
        Path(home).mkdir(parents=True, exist_ok=True)
        self.assertEqual(self.lookup(f"  {name} "), home)

    def test_pool_unknown_missing_home_and_no_marker_leave_it_unset(self):
        pools = {e["pool"] for e in json.loads(ACCOUNTS.read_text())["codex"] if "pool" in e}
        for marker in [*pools, "no-such-account", None]:
            with self.subTest(marker=marker):
                self.assertEqual(self.lookup(marker), "unset")
        # A listed account whose home does not exist is not exported either.
        name = json.loads(ACCOUNTS.read_text())["codex"][0]["name"]
        self.assertEqual(self.lookup(name), "unset")

    def test_explicit_codex_home_wins(self):
        name = json.loads(ACCOUNTS.read_text())["codex"][0]["name"]
        self.assertEqual(self.lookup(name, codex_home="/pinned"), "/pinned")


if __name__ == "__main__":
    unittest.main()
