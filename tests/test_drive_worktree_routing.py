"""Tests for external worktree routing and the Drive symlink-helper removal.

Every worktree-creating helper must construct destinations under
~/repos/.worktrees/<repository>/<branch> (external to both the repository
checkout and ~/My Drive), the Drive node_modules relocation helper must be
gone from shell/.zshrc, and mkvenv must refuse to create a virtualenv under
~/My Drive.

The Trajectory entry points (`newtask` in .zshrc and `ps/trajectory-*` in
config.org) were retired in df6eb77cc; the helpers below were kept.
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
ZSHRC = ROOT / "shell" / ".zshrc"

WORKTREE_HELPERS = [
    ROOT / "bin" / "cr-review-worktree",
    ROOT / "claude" / "bin" / "cr-worktree-gc.sh",
    ROOT / "bin" / "sync-reasoning-tasks-worktrees",
    ROOT / "claude" / "hooks" / "sync-reasoning-tasks-worktree.sh",
]

EXTERNAL_ROOT_FRAGMENT = "repos/.worktrees/reasoning-tasks"


def zsh_run(snippet: str, home: Path, extra_path: Path | None = None) -> subprocess.CompletedProcess:
    """Run SNIPPET in zsh after sourcing the repo .zshrc under a scratch HOME."""
    env = os.environ.copy()
    env["HOME"] = str(home)
    env.pop("NVM_DIR", None)
    if extra_path is not None:
        env["PATH"] = f"{extra_path}:{env['PATH']}"
    script = f"source {shlex_quote(str(ZSHRC))} >/dev/null 2>&1\n{snippet}\n"
    return subprocess.run(
        ["zsh", "-f", "-c", script],
        capture_output=True,
        text=True,
        env=env,
        cwd=str(home),
    )


def shlex_quote(text: str) -> str:
    return "'" + text.replace("'", "'\\''") + "'"


def make_stub(directory: Path, name: str, log: Path) -> None:
    stub = directory / name
    stub.write_text(
        "#!/bin/sh\n"
        f'printf \'%s\\n\' "{name} $*" >> {shlex_quote(str(log))}\n'
        "exit 0\n"
    )
    stub.chmod(0o755)


class ZshrcRoutingTest(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp(prefix="drive-routing-"))
        self.addCleanup(shutil.rmtree, self.tmp, ignore_errors=True)
        self.stubs = self.tmp / "stub-bin"
        self.stubs.mkdir()
        self.log = self.tmp / "calls.log"
        for name in ("git", "ln", "python3", "mu"):
            make_stub(self.stubs, name, self.log)

    def calls(self) -> str:
        return self.log.read_text() if self.log.exists() else ""

    def test_gdrive_relocate_node_modules_is_gone(self):
        text = ZSHRC.read_text()
        self.assertNotIn("_gdrive_relocate_node_modules", text)
        result = zsh_run(
            "typeset -f _gdrive_relocate_node_modules >/dev/null && echo PRESENT || echo ABSENT",
            self.tmp,
        )
        self.assertIn("ABSENT", result.stdout)

    def test_mkvenv_refuses_drive_root(self):
        project = self.tmp / "My Drive" / "someproj"
        project.mkdir(parents=True)
        result = zsh_run(
            f"cd {shlex_quote(str(project))} && mkvenv; echo rc=$?", self.tmp, self.stubs
        )
        self.assertIn("rc=1", result.stdout)
        self.assertIn("My Drive", result.stderr + result.stdout)
        self.assertNotIn("-m venv", self.calls())

    def test_mkvenv_still_works_outside_drive(self):
        project = self.tmp / "repos" / "someproj"
        project.mkdir(parents=True)
        zsh_run(f"cd {shlex_quote(str(project))} && mkvenv", self.tmp, self.stubs)
        self.assertIn("-m venv .venv", self.calls())


class WorktreeHelperRoutingTest(unittest.TestCase):
    def test_no_helper_contains_a_drive_local_worktree_destination(self):
        drive_local = re.compile(
            r"My Drive[^\n]*\.worktrees|\$DOTFILES[^\n]*\.worktrees|\.cr-tmp/qa-"
        )
        for path in [*WORKTREE_HELPERS, ZSHRC]:
            with self.subTest(path=str(path)):
                self.assertIsNone(
                    drive_local.search(path.read_text()),
                    f"{path} still routes worktrees to a Drive-local destination",
                )

    def test_every_helper_references_the_external_worktrees_root(self):
        for path in WORKTREE_HELPERS:
            with self.subTest(path=str(path)):
                self.assertIn(EXTERNAL_ROOT_FRAGMENT, path.read_text())

    def test_cr_review_worktree_creates_under_external_root(self):
        text = (ROOT / "bin" / "cr-review-worktree").read_text()
        self.assertRegex(
            text, r'WORKTREES="\$HOME/repos/\.worktrees/reasoning-tasks"'
        )
        self.assertRegex(text, r'wt="\$WORKTREES/qa-')


if __name__ == "__main__":
    unittest.main()
