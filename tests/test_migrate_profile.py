"""End-to-end fixture test for bin/migrate-profile.

Two retired profiles hold Claude and Codex sessions for the same package; the
current profile has its checkout. One run must consolidate everything into the
current profile and report what has no counterpart there.
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "migrate-profile"
SID_A = "aaaaaaaa-0000-0000-0000-000000000001"
SID_B = "bbbbbbbb-0000-0000-0000-000000000002"
SID_C = "cccccccc-0000-0000-0000-000000000003"
CODEX_A = "11111111-0000-0000-0000-00000000000a"
CODEX_B = "22222222-0000-0000-0000-00000000000b"


def encode(path: str) -> str:
    import re
    return re.sub(r"[/. ]", "-", path)


def jsonl(*rows) -> str:
    return "".join(json.dumps(row) + "\n" for row in rows)


class MigrateProfileTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="migrate-profile-test-", dir="/tmp")
        self.addCleanup(self.temporary.cleanup)
        self.home = Path(os.path.realpath(self.temporary.name))
        self.profiles = self.home / ".config" / "emacs-profiles"
        for profile in ("1.0", "1.1", "2.0"):
            (self.profiles / profile / "elpaca" / "sources" / "pkg").mkdir(parents=True)
        (self.profiles / "1.0" / "elpaca" / "sources" / "gone").mkdir()
        (self.profiles / "active").symlink_to(self.profiles / "2.0")
        (self.profiles / ".current-profile").write_text("2.0")
        self.old_a = str(self.profiles / "1.0" / "elpaca" / "sources" / "pkg")
        self.old_b = str(self.profiles / "1.1" / "elpaca" / "sources" / "pkg")
        self.gone = str(self.profiles / "1.0" / "elpaca" / "sources" / "gone")
        self.new = str(self.profiles / "2.0" / "elpaca" / "sources" / "pkg")

        self.claude = self.home / ".claude"
        projects = self.claude / "projects"
        for sid, old in ((SID_A, self.old_a), (SID_B, self.old_b), (SID_C, self.gone)):
            bucket = projects / encode(old)
            bucket.mkdir(parents=True)
            (bucket / f"{sid}.jsonl").write_text(jsonl({"sessionId": sid, "type": "user", "cwd": old}))
        self.history = self.claude / "history.jsonl"
        self.history.write_text(jsonl({"sessionId": SID_A, "project": self.old_a, "display": "a"},
                                      {"sessionId": SID_B, "project": self.old_b, "display": "b"},
                                      {"project": self.old_a, "display": "orphan without sessionId"},
                                      {"sessionId": SID_C, "project": self.gone, "display": "c"}))

        self.codex = self.home / ".codex"
        sessions = self.codex / "sessions" / "2026" / "09" / "26"
        sessions.mkdir(parents=True)
        self.rollouts = {}
        for sid, old in ((CODEX_A, self.old_a), (CODEX_B, self.old_b)):
            path = sessions / f"rollout-2026-09-26T00-00-00-{sid}.jsonl"
            path.write_text(jsonl({"type": "session_meta", "payload": {"id": sid, "cwd": old}},
                                  {"type": "turn_context", "payload": {"cwd": old}}))
            self.rollouts[sid] = path
        self.env = {**os.environ, "HOME": str(self.home), "CLAUDE_CONFIG_DIR": str(self.claude),
                    "CODEX_HOME": str(self.codex), "PYTHONDONTWRITEBYTECODE": "1"}

    def run_script(self, *args):
        return subprocess.run([sys.executable, str(SCRIPT), *args], env=self.env,
                              capture_output=True, text=True, timeout=120)

    def test_dry_run_changes_nothing(self):
        before = {p: p.read_bytes() for p in self.home.rglob("*") if p.is_file()}
        result = self.run_script("--dry-run")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual({p: p.read_bytes() for p in self.home.rglob("*") if p.is_file()}, before)
        self.assertFalse((self.home / ".local").exists())

    def test_consolidates_every_retired_profile_into_the_current_one(self):
        result = self.run_script()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        target = self.claude / "projects" / encode(self.new)
        self.assertEqual(sorted(p.name for p in target.glob("*.jsonl")), [f"{SID_A}.jsonl", f"{SID_B}.jsonl"])
        for sid in (SID_A, SID_B):
            self.assertEqual(json.loads((target / f"{sid}.jsonl").read_text())["cwd"], self.new)
        projects = [json.loads(line)["project"] for line in self.history.read_text().splitlines()]
        self.assertEqual(projects, [self.new, self.new, self.new, self.gone])
        for path in self.rollouts.values():
            self.assertEqual({json.loads(line)["payload"]["cwd"] for line in path.read_text().splitlines()},
                             {self.new})
        self.assertIn(f"not moved (no counterpart in 2.0): {self.gone}", result.stdout)
        self.assertTrue((self.claude / "projects" / encode(self.gone) / f"{SID_C}.jsonl").exists())
        again = self.run_script("--dry-run")
        self.assertNotIn("would move", again.stdout)

    def test_renamed_package_follows_to_its_new_name(self):
        renamed = self.profiles / "1.0" / "elpaca" / "sources" / "ai-agent"
        renamed.mkdir()
        agent = self.profiles / "2.0" / "elpaca" / "sources" / "agent"
        agent.mkdir()
        sid = "dddddddd-0000-0000-0000-000000000004"
        bucket = self.claude / "projects" / encode(str(renamed))
        bucket.mkdir()
        (bucket / f"{sid}.jsonl").write_text(jsonl({"sessionId": sid, "type": "user", "cwd": str(renamed)}))
        result = self.run_script()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        moved = self.claude / "projects" / encode(str(agent)) / f"{sid}.jsonl"
        self.assertEqual(json.loads(moved.read_text())["cwd"], str(agent))

    def test_refuses_when_the_startup_cache_disagrees_with_active(self):
        (self.profiles / ".current-profile").write_text("1.1")
        result = self.run_script()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn(".current-profile says 1.1", result.stderr)


if __name__ == "__main__":
    unittest.main()
