from __future__ import annotations

import hashlib
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
HELPER = DOTFILES / "bin" / "ai-config-sync"
TEMP_PARENT = "/private/tmp" if sys.platform == "darwin" else "/tmp"
MODES = ("remind-claude", "remind-codex")
INSTRUCTIONS = ("CLAUDE.md", "projects/example/AGENTS.md")
SKILLS = (".claude/skills/example/SKILL.md", ".codex/skills/example/SKILL.md")
ENV = {
    "PATH": "/usr/bin:/bin",
    "LC_ALL": "C",
    "PYTHONDONTWRITEBYTECODE": "1",
    "GIT_CONFIG_NOSYSTEM": "1",
    "GIT_CONFIG_SYSTEM": os.devnull,
    "GIT_CONFIG_GLOBAL": os.devnull,
}


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


class AiConfigSyncReminderTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(
            prefix="current66-reminder-tests-", dir=TEMP_PARENT,
        )
        self.addCleanup(self.cleanup_fixture)
        self.root = Path(self.directory.name)
        template = self.root / "empty-git-template"
        template.mkdir()
        subprocess.run(
            ["/usr/bin/git", "init", "-q", f"--template={template}", str(self.root)],
            env=ENV, capture_output=True, text=True, check=True, timeout=10,
        )
        for rel in (*INSTRUCTIONS, *SKILLS):
            target = self.root / rel
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text("Synthetic reminder fixture.\n", encoding="utf-8")

    def cleanup_fixture(self):
        self.directory.cleanup()
        self.assertFalse(os.path.lexists(self.root))

    def manifest(self, value):
        (self.root / "ai-config-sync.json").write_text(json.dumps(value), encoding="utf-8")

    def remind(self, mode: str, rel: str, root: Path | None = None, *, track_target=True) -> str:
        root = self.root if root is None else root
        path = root / rel
        target_hash = digest(path) if track_target and path.is_file() else None
        source_hash = digest(HELPER)
        payload = {"hook_event_name": "PostToolUse", "cwd": str(root)}
        if mode == "remind-claude":
            payload.update(tool_name="Edit", tool_input={"file_path": str(path)})
        else:
            payload.update(
                tool_name="apply_patch",
                tool_input={"patch": f"*** Begin Patch\n*** Update File: {path}\n*** End Patch"},
            )
        result = subprocess.run(
            [sys.executable, "-I", "-B", str(HELPER), mode],
            cwd=TEMP_PARENT, env=ENV, input=json.dumps(payload),
            capture_output=True, text=True, check=False, timeout=10,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, "")
        self.assertEqual(digest(HELPER), source_hash)
        if target_hash is not None:
            self.assertEqual(digest(path), target_hash)
        if not result.stdout:
            return ""
        out = json.loads(result.stdout)
        self.assertEqual(set(out), {"hookSpecificOutput"})
        context = out["hookSpecificOutput"]
        self.assertEqual(context["hookEventName"], "PostToolUse")
        return context["additionalContext"]

    def check_paths(self, paths, suppressed):
        for mode in MODES:
            for rel in paths:
                with self.subTest(mode=mode, path=rel):
                    message = self.remind(mode, rel)
                    if suppressed:
                        self.assertEqual(message, "")
                    else:
                        self.assertTrue(message.startswith("Project-local "), message)
                        self.assertIn("changed; update ", message)

    def test_claude_only_suppresses_both_artifact_reminders(self):
        self.manifest({"local": {"skills": "claude-only", "instructions": "claude-only"}})
        self.check_paths((*INSTRUCTIONS, *SKILLS), True)

    def test_codex_only_suppresses_both_artifact_reminders(self):
        self.manifest({"local": {"skills": "codex-only", "instructions": "codex-only"}})
        self.check_paths((*INSTRUCTIONS, *SKILLS), True)

    def test_existing_nonpaired_status_policy_is_preserved(self):
        self.manifest({"local": {"skills": "unsupported", "instructions": "unsupported"}})
        self.check_paths((*INSTRUCTIONS, *SKILLS), True)

    def test_skill_opt_out_does_not_suppress_instruction_reminders(self):
        self.manifest({"local": {"skills": "claude-only"}})
        self.check_paths(SKILLS, True)
        self.check_paths(INSTRUCTIONS, False)

    def test_instruction_opt_out_does_not_suppress_skill_reminders(self):
        self.manifest({"local": {"instructions": "codex-only"}})
        self.check_paths(INSTRUCTIONS, True)
        self.check_paths(SKILLS, False)

    def test_missing_manifest_keeps_pairing_reminders(self):
        self.check_paths((*INSTRUCTIONS, *SKILLS), False)

    def test_explicit_paired_status_keeps_pairing_reminders(self):
        self.manifest({"local": {"skills": "paired", "instructions": "paired-custom"}})
        self.check_paths((*INSTRUCTIONS, *SKILLS), False)

    def test_malformed_json_keeps_pairing_reminders(self):
        (self.root / "ai-config-sync.json").write_text("{", encoding="utf-8")
        self.check_paths((*INSTRUCTIONS, *SKILLS), False)

    def test_invalid_utf8_keeps_pairing_reminders(self):
        (self.root / "ai-config-sync.json").write_bytes(b"\xff")
        self.check_paths((*INSTRUCTIONS, *SKILLS), False)

    def test_wrong_manifest_shapes_keep_pairing_reminders(self):
        for value in ([], {"local": []}, {"local": {"skills": "", "instructions": None}}):
            self.manifest(value)
            self.check_paths((*INSTRUCTIONS, *SKILLS), False)

    def test_unreadable_manifest_keeps_pairing_reminders(self):
        (self.root / "ai-config-sync.json").mkdir()
        self.check_paths((*INSTRUCTIONS, *SKILLS), False)

    def test_single_agent_does_not_suppress_unrelated_hook_reminders(self):
        self.manifest({"local": {"skills": "claude-only", "instructions": "claude-only"}})
        for mode in MODES:
            for rel in (".claude/hooks/example.py", ".codex/hooks/example.py"):
                with self.subTest(mode=mode, path=rel):
                    self.assertIn("add/adjust its", self.remind(mode, rel))
                    self.assertFalse((self.root / rel).exists())

    def test_global_reminders_are_unchanged(self):
        for mode in MODES:
            for rel, counterpart in (
                ("claude/skills/synthetic-reminder-only/SKILL.md", "codex/skills/synthetic-reminder-only"),
                ("codex/skills/synthetic-reminder-only/SKILL.md", "claude/skills/synthetic-reminder-only"),
                ("claude/CLAUDE.md", "codex/AGENTS.md"),
                ("codex/AGENTS.md", "claude/CLAUDE.md"),
            ):
                with self.subTest(mode=mode, path=rel):
                    # Nonexistent synthetic skill paths are path-classification
                    # inputs only. Existing global instructions are not opened.
                    message = self.remind(mode, rel, DOTFILES, track_target=False)
                    self.assertIn(f"update {counterpart} in the same session.", message)
                    if "synthetic-reminder-only" in rel:
                        self.assertFalse((DOTFILES / rel).exists())


if __name__ == "__main__":
    unittest.main()
