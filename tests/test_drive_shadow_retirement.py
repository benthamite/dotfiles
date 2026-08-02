"""Shadow-skill retirement evidence for the Drive migration pilot.

Three surfaces, one contract:

1. ``drive-workspace record-path-hashes`` journals a deterministic recursive
   content-hash manifest of an inspected path into an observation journal,
   without mutating the path.  The pilot uses it to fingerprint the four
   legacy parent-Drive shadow skill directories before retiring them.
2. A disposable resolver test proves that both Claude and Codex global skill
   resolution select the committed tracked ``fix-drive-errors`` skill once
   the parent-Drive project-local shadows are excluded.
3. The ai-config-sync parent-Drive pair audit has a tested expected state
   for both sides of the retirement: it passes while the four shadows are
   present and paired, and it passes after all four are retired together.
   The transitional duplicate-name report that forces the retirement is
   pinned as deliberate behavior.
"""

from __future__ import annotations

import contextlib
import hashlib
import importlib.machinery
import importlib.util
import io
import json
import os
import shutil
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


DOTFILES = Path(__file__).resolve().parents[1]

SHADOW_SKILL = """---
name: {name}
description: Legacy parent-Drive shadow copy used only as a fixture.
---

Legacy shadow body.
"""

TRACKED_FIXTURE_SKILL = """---
name: fix-drive-errors
description: Tracked fixture copy.
---

Tracked body.
"""


def load_script(name: str, path: Path):
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


def write_skill(root: Path, name: str, text: str | None = None) -> Path:
    skill_dir = root / name
    skill_dir.mkdir(parents=True)
    path = skill_dir / "SKILL.md"
    path.write_text(text or SHADOW_SKILL.format(name=name), encoding="utf-8")
    return path


class RecordPathHashesTests(unittest.TestCase):
    """The observation-journal manifest of an inspected path is faithful,
    deterministic, read-only, and restricted to observation journals."""

    @classmethod
    def setUpClass(cls):
        cls.module = load_script(
            "drive_workspace_shadow_tests", DOTFILES / "bin" / "drive-workspace"
        )

    def setUp(self):
        self.root = Path(tempfile.mkdtemp(prefix="drive-shadow-")).resolve()
        self.addCleanup(shutil.rmtree, self.root, ignore_errors=True)
        self.state = self.root / "state"
        patcher = mock.patch.dict(
            os.environ, {"DRIVE_WORKSPACE_STATE_ROOT": str(self.state)}
        )
        patcher.start()
        self.addCleanup(patcher.stop)
        self.journal = self.state / "shadow" / "journal.jsonl"
        self.target = self.root / "shadow-skill"
        (self.target / "nested").mkdir(parents=True)
        (self.target / "SKILL.md").write_text("body one\n", encoding="utf-8")
        (self.target / "nested" / "aux.md").write_text(
            "aux two\n", encoding="utf-8"
        )

    def run_main(self, *argv):
        stdout = io.StringIO()
        stderr = io.StringIO()
        with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(
            stderr
        ):
            code = self.module.main([str(item) for item in argv])
        return stdout.getvalue(), stderr.getvalue(), code

    def ok(self, *argv):
        stdout, stderr, code = self.run_main(*argv)
        self.assertEqual(0, code, f"argv={argv!r} stderr={stderr!r}")
        return stdout, stderr

    def expect_fail(self, *argv):
        stdout, stderr, code = self.run_main(*argv)
        self.assertNotEqual(0, code, f"argv={argv!r} stdout={stdout!r}")
        return stdout, stderr

    def init_observation_journal(self):
        self.ok(
            "init-journal", "--journal", self.journal,
            "--kind", "observation", "--label", "shadow snapshot",
        )

    def events(self):
        return [
            json.loads(line)
            for line in self.journal.read_text(encoding="utf-8").splitlines()
        ]

    def sha256(self, path: Path) -> str:
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def test_records_faithful_deterministic_recursive_hashes(self):
        self.init_observation_journal()
        self.ok(
            "record-path-hashes", "--journal", self.journal,
            "--path", self.target,
        )
        self.ok(
            "record-path-hashes", "--journal", self.journal,
            "--path", self.target,
        )

        events = self.events()
        self.assertEqual(
            ["header", "path_hashes_recorded", "path_hashes_recorded"],
            [event["event"] for event in events],
        )
        first, second = events[1]["payload"], events[2]["payload"]
        self.assertEqual(str(self.target), first["path"])
        self.assertEqual(
            {
                "SKILL.md": {
                    "type": "file",
                    "size": 9,
                    "sha256": self.sha256(self.target / "SKILL.md"),
                },
                "nested": {"type": "directory"},
                "nested/aux.md": {
                    "type": "file",
                    "size": 8,
                    "sha256": self.sha256(self.target / "nested" / "aux.md"),
                },
            },
            first["entries"],
        )
        self.assertEqual(3, first["entry_count"])
        self.assertEqual(
            hashlib.sha256(
                (
                    json.dumps(
                        first["entries"],
                        sort_keys=True,
                        separators=(",", ":"),
                    )
                    + "\n"
                ).encode("utf-8")
            ).hexdigest(),
            first["aggregate_sha256"],
        )
        # Deterministic: an unchanged tree yields an identical payload.
        self.assertEqual(first, second)

    def test_records_symlinks_by_link_text_without_following(self):
        (self.target / "stray-link").symlink_to(self.root / "outside")
        self.init_observation_journal()
        self.ok(
            "record-path-hashes", "--journal", self.journal,
            "--path", self.target,
        )

        entries = self.events()[1]["payload"]["entries"]
        self.assertEqual(
            {"type": "symlink", "target": str(self.root / "outside")},
            entries["stray-link"],
        )

    def test_does_not_mutate_the_inspected_path(self):
        before = {
            str(path): path.read_bytes()
            for path in self.target.rglob("*")
            if path.is_file()
        }
        self.init_observation_journal()
        self.ok(
            "record-path-hashes", "--journal", self.journal,
            "--path", self.target,
        )

        after = {
            str(path): path.read_bytes()
            for path in self.target.rglob("*")
            if path.is_file()
        }
        self.assertEqual(before, after)
        self.assertEqual(
            sorted(before), sorted(str(p) for p in self.target.rglob("*") if p.is_file())
        )

    def test_refuses_non_observation_journals(self):
        program = self.state / "program" / "journal.jsonl"
        self.ok(
            "init-journal", "--journal", program,
            "--kind", "program", "--label", "not-an-observation",
        )

        _stdout, stderr = self.expect_fail(
            "record-path-hashes", "--journal", program, "--path", self.target,
        )
        self.assertIn("observation", stderr)

    def test_refuses_a_missing_path(self):
        self.init_observation_journal()

        _stdout, stderr = self.expect_fail(
            "record-path-hashes", "--journal", self.journal,
            "--path", self.root / "absent",
        )
        self.assertIn("absent", stderr)


class ShadowExclusionResolverTests(unittest.TestCase):
    """Disposable resolver proof: with the parent-Drive project-local shadows
    excluded, global resolution selects the committed tracked skill for both
    Claude and Codex."""

    @classmethod
    def setUpClass(cls):
        cls.module = load_script(
            "agent_skill_shadow_tests", DOTFILES / "bin" / "agent-skill"
        )

    def setUp(self):
        self.home = Path(tempfile.mkdtemp(prefix="drive-shadow-home-")).resolve()
        self.addCleanup(shutil.rmtree, self.home, ignore_errors=True)
        self.drive = self.home / "My Drive"
        self.cwd = self.drive / "repos" / "project"
        self.cwd.mkdir(parents=True)
        self.shadow_dirs = []
        for side in (".claude", ".codex"):
            root = self.drive / side / "skills"
            write_skill(root, "fix-drive-errors")
            self.shadow_dirs.append(root / "fix-drive-errors")
        patcher = mock.patch.object(self.module, "HOME", self.home)
        patcher.start()
        self.addCleanup(patcher.stop)
        env = mock.patch.dict(
            os.environ,
            {
                "CLAUDE_HOME": str(self.home / ".claude-home"),
                "CODEX_HOME": str(self.home / ".codex-home"),
            },
        )
        env.start()
        self.addCleanup(env.stop)

    def resolve(self, tool: str):
        roots = self.module.discover_roots(self.cwd, tool)
        skills = self.module.discover_skills(roots)
        return self.module.resolve_skill(skills, "fix-drive-errors")

    def test_shadows_present_still_win_project_local_resolution(self):
        for tool in ("claude", "codex"):
            selected = self.resolve(tool)
            self.assertTrue(
                selected.root.source.startswith("project-local:"),
                selected.root.source,
            )
            self.assertTrue(
                str(selected.path).startswith(str(self.drive)), selected.path
            )

    def test_excluded_shadows_yield_the_committed_tracked_skill(self):
        for shadow in self.shadow_dirs:
            shutil.rmtree(shadow)

        for tool, tree in (("claude", "claude"), ("codex", "codex")):
            selected = self.resolve(tool)
            self.assertEqual("dotfiles", selected.root.source)
            self.assertEqual(
                DOTFILES / tree / "skills" / "fix-drive-errors" / "SKILL.md",
                selected.path,
            )


class ShadowRetirementAuditTests(unittest.TestCase):
    """Tested expected states of bin/ai-config-sync audit around the
    retirement of the four parent-Drive shadow directories."""

    SHADOW_NAMES = ("fix-drive-errors", "nosync")

    @classmethod
    def setUpClass(cls):
        cls.module = load_script(
            "ai_config_sync_shadow_tests", DOTFILES / "bin" / "ai-config-sync"
        )

    def setUp(self):
        self.home = Path(
            tempfile.mkdtemp(prefix="drive-shadow-audit-")
        ).resolve()
        self.addCleanup(shutil.rmtree, self.home, ignore_errors=True)
        self.workspace = self.home / "My Drive"
        for side in (".claude", ".codex"):
            for name in self.SHADOW_NAMES:
                write_skill(self.workspace / side / "skills", name)

    def parent_drive_problems(self) -> list[str]:
        problems: list[str] = []
        self.module.audit_parent_drive_skill_pairs(
            problems, workspace=self.workspace
        )
        return problems

    def test_audit_passes_with_all_four_shadows_present(self):
        self.assertEqual([], self.parent_drive_problems())

    def test_audit_passes_after_all_four_shadows_are_retired_together(self):
        for side in (".claude", ".codex"):
            for name in self.SHADOW_NAMES:
                shutil.rmtree(self.workspace / side / "skills" / name)

        # Retirement leaves empty skill roots first, then removes them.
        self.assertEqual([], self.parent_drive_problems())
        for side in (".claude", ".codex"):
            shutil.rmtree(self.workspace / side)
        self.assertEqual([], self.parent_drive_problems())

    def test_partial_retirement_is_reported_as_drift(self):
        shutil.rmtree(self.workspace / ".codex" / "skills" / "nosync")

        self.assertEqual(
            ["Parent-Drive skill lacks Codex counterpart: nosync"],
            self.parent_drive_problems(),
        )

    def duplicate_problems(self) -> list[str]:
        fake_root = self.workspace / "dotfiles"
        for tree in ("claude", "codex"):
            write_skill(
                fake_root / tree / "skills",
                "fix-drive-errors",
                TRACKED_FIXTURE_SKILL,
            )
        problems: list[str] = []
        with mock.patch.dict(
            os.environ,
            {
                "HOME": str(self.home),
                "CLAUDE_HOME": str(self.home / ".claude-home"),
                "CODEX_HOME": str(self.home / ".codex-home"),
            },
        ), mock.patch.object(self.module, "ROOT", fake_root):
            self.module.audit_duplicate_skill_names(problems)
        return problems

    def test_tracked_skill_duplicates_shadow_until_retirement(self):
        problems = self.duplicate_problems()

        self.assertTrue(
            any(
                "Duplicate claude skill name: fix-drive-errors" in problem
                for problem in problems
            ),
            problems,
        )

    def test_no_duplicate_once_the_shadows_are_retired(self):
        for side in (".claude", ".codex"):
            for name in self.SHADOW_NAMES:
                shutil.rmtree(self.workspace / side / "skills" / name)

        self.assertEqual([], self.duplicate_problems())


if __name__ == "__main__":
    unittest.main()
