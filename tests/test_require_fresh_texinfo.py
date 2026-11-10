"""Tests for the commit gate that keeps staged Texinfo in step with its Org manual."""

from __future__ import annotations

import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
GATE = ROOT / "claude" / "hooks" / "require-fresh-texinfo.py"
REGENERATE = ROOT / "claude" / "bin" / "regenerate-manual"
MANUAL = """#+title: Fixture manual
#+texinfo_filename: fixture.info
#+export_file_name: fixture.info
#+texinfo_header: @c AUTO-GENERATED from README.org, do not edit manually.

* Overview
First, second, and third.
"""


class RequireFreshTexinfoTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.repo = Path(self.temp.name).resolve()
        self.env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        self.env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        self.git("init", "-q")
        (self.repo / "README.org").write_text(MANUAL)
        self.regenerate()
        self.git("add", "-A")
        self.git("-c", "user.name=T", "-c", "user.email=t@example.com", "commit", "-q", "-m", "init")

    def tearDown(self) -> None:
        self.temp.cleanup()

    def git(self, *args: str) -> None:
        subprocess.run(["git", *args], cwd=self.repo, env=self.env, check=True, capture_output=True)

    def regenerate(self) -> None:
        subprocess.run([str(REGENERATE), "README.org"], cwd=self.repo, env=self.env,
                       check=True, capture_output=True)

    def decision(self) -> str:
        payload = json.dumps({"tool_input": {"command": "git commit"}})
        result = subprocess.run([sys_executable(), str(GATE)], input=payload, cwd=self.repo,
                                env=self.env, capture_output=True, text=True, check=True)
        if not result.stdout.strip():
            return "allow"
        return json.loads(result.stdout)["hookSpecificOutput"]["permissionDecision"]

    def test_regenerated_manual_passes(self) -> None:
        self.assertIn("@comma{}", (self.repo / "fixture.texi").read_text())
        (self.repo / "README.org").write_text(MANUAL + "\nMore text, with a comma.\n")
        self.regenerate()
        self.git("add", "-A")
        self.assertEqual(self.decision(), "allow")

    def test_hand_edited_texi_is_blocked(self) -> None:
        texi = self.repo / "fixture.texi"
        texi.write_text(texi.read_text().replace("@comma{}", ","))
        self.git("add", "fixture.texi")
        self.assertEqual(self.decision(), "deny")

    def test_org_edit_without_regenerating_is_blocked(self) -> None:
        (self.repo / "README.org").write_text(MANUAL + "\nNew paragraph.\n")
        self.git("add", "README.org")
        self.assertEqual(self.decision(), "deny")

    def test_untracked_texi_is_not_checked(self) -> None:
        self.git("rm", "-q", "--cached", "fixture.texi")
        self.git("-c", "user.name=T", "-c", "user.email=t@example.com", "commit", "-q", "-m", "untrack")
        (self.repo / "README.org").write_text(MANUAL + "\nNew paragraph.\n")
        self.git("add", "README.org")
        self.assertEqual(self.decision(), "allow")


def sys_executable() -> str:
    import sys
    return sys.executable


if __name__ == "__main__":
    unittest.main()
