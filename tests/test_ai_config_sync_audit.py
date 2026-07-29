from __future__ import annotations

import io
import importlib.machinery
import importlib.util
import json
import subprocess
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest import mock


DOTFILES = Path("/Users/pablostafforini/My Drive/dotfiles")


def load_script(name: str, path: Path):
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


class AiConfigSyncAuditTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.module = load_script(
            "ai_config_sync_audit_script",
            DOTFILES / "bin" / "ai-config-sync",
        )

    def run_git(self, repo: Path, *args: str) -> None:
        subprocess.run(
            ["git", *args],
            cwd=repo,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )

    def write_file(self, repo: Path, rel: str, text: str = "body\n") -> None:
        path = repo / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")

    def make_repo(self, files: list[str]) -> Path:
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        repo = Path(temp.name)
        self.run_git(repo, "init")
        self.run_git(repo, "config", "user.email", "test@example.com")
        self.run_git(repo, "config", "user.name", "Test User")
        for rel in files:
            self.write_file(repo, rel)
        self.run_git(repo, "add", ".")
        self.run_git(repo, "commit", "-m", "initial")
        return repo

    def test_audit_reports_deleted_project_local_claude_skill_counterpart(self):
        repo = self.make_repo(
            [
                ".claude/skills/example/SKILL.md",
                ".codex/skills/example/SKILL.md",
            ]
        )
        (repo / ".claude/skills/example/SKILL.md").unlink()

        problems: list[str] = []
        self.module.audit_tracked_pair_deletions(problems, repo)

        self.assertEqual(
            [
                "Tracked paired artifact deleted on Claude side while Codex side remains: "
                ".claude/skills/example -> .codex/skills/example"
            ],
            problems,
        )

    def test_audit_reports_deleted_nested_project_local_claude_skill_counterpart(self):
        repo = self.make_repo(
            [
                "emacs/.claude/skills/emacs-freeze/SKILL.md",
                "emacs/.codex/skills/emacs-freeze/SKILL.md",
            ]
        )
        (repo / "emacs/.claude/skills/emacs-freeze/SKILL.md").unlink()

        problems: list[str] = []
        self.module.audit_tracked_pair_deletions(problems, repo)

        self.assertEqual(
            [
                "Tracked paired artifact deleted on Claude side while Codex side remains: "
                "emacs/.claude/skills/emacs-freeze -> emacs/.codex/skills/emacs-freeze"
            ],
            problems,
        )

    def test_audit_reports_deleted_global_programmatic_claude_skill_counterpart(self):
        repo = self.make_repo(
            [
                "claude/programmatic-skills/twitter/SKILL.md",
                "codex/programmatic-skills/twitter/SKILL.md",
            ]
        )
        (repo / "claude/programmatic-skills/twitter/SKILL.md").unlink()

        problems: list[str] = []
        self.module.audit_tracked_pair_deletions(problems, repo)

        self.assertEqual(
            [
                "Tracked paired artifact deleted on Claude side while Codex side remains: "
                "claude/programmatic-skills/twitter -> codex/programmatic-skills/twitter"
            ],
            problems,
        )

    def test_audit_ignores_deleted_runtime_files(self):
        repo = self.make_repo(
            [
                "claude/programmatic-skills/twitter-digest/digests/example.org",
                "codex/programmatic-skills/twitter-digest/digests/example.org",
            ]
        )
        (repo / "claude/programmatic-skills/twitter-digest/digests/example.org").unlink()

        problems: list[str] = []
        self.module.audit_tracked_pair_deletions(problems, repo)

        self.assertEqual([], problems)

    def test_remind_claude_emits_post_tool_additional_context_json(self):
        repo = self.make_repo(["CLAUDE.md", "AGENTS.md"])
        proc = subprocess.run(
            [sys.executable, str(DOTFILES / "bin" / "ai-config-sync"), "remind-claude"],
            cwd=repo,
            input=json.dumps(
                {
                    "hook_event_name": "PostToolUse",
                    "cwd": str(repo),
                    "tool_name": "Edit",
                    "tool_input": {"file_path": str(repo / "CLAUDE.md")},
                }
            ),
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )

        output = json.loads(proc.stdout)

        self.assertEqual(
            {
                "hookEventName": "PostToolUse",
                "additionalContext": "Project-local CLAUDE.md changed; update AGENTS.md in the same repo.",
            },
            output["hookSpecificOutput"],
        )

    def test_audit_known_project_instruction_pairs_reports_drift(self):
        repo = self.make_repo(["CLAUDE.md", "AGENTS.md"])
        self.write_file(repo, "CLAUDE.md", "same\n")
        self.write_file(repo, "AGENTS.md", "different\n")
        original_roots = self.module.KNOWN_PROJECT_INSTRUCTION_ROOTS
        self.module.KNOWN_PROJECT_INSTRUCTION_ROOTS = [repo]
        self.addCleanup(setattr, self.module, "KNOWN_PROJECT_INSTRUCTION_ROOTS", original_roots)

        problems: list[str] = []
        self.module.audit_known_project_instruction_pairs(problems)

        self.assertEqual(
            [
                f"Known project-local instruction pair is not synchronized: {repo}",
                "  - Project-local instruction drift after tool-specific normalization: CLAUDE.md / AGENTS.md",
            ],
            problems,
        )

    def test_audit_parent_drive_skill_aliases_reports_untracked_codex_copy(self):
        canonical_root = self.make_repo(
            [
                "google-drive/.claude/skills/fix-drive-errors/SKILL.md",
                "google-drive/.codex/skills/fix-drive-errors/SKILL.md",
            ]
        )
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        parent_drive_root = Path(temp.name)
        claude_alias = parent_drive_root / ".claude/skills/fix-drive-errors"
        codex_copy = parent_drive_root / ".codex/skills/fix-drive-errors"
        claude_alias.parent.mkdir(parents=True)
        codex_copy.mkdir(parents=True)
        claude_alias.symlink_to(
            canonical_root / "google-drive/.claude/skills/fix-drive-errors"
        )
        self.write_file(
            parent_drive_root,
            ".codex/skills/fix-drive-errors/SKILL.md",
            "stale\n",
        )

        self.assertTrue(
            hasattr(self.module, "audit_parent_drive_skill_aliases"),
            "parent-Drive aliases are not audited",
        )
        with (
            mock.patch.object(self.module, "ROOT", canonical_root),
            mock.patch.object(self.module, "PARENT_DRIVE_ROOT", parent_drive_root),
            mock.patch.object(
                self.module,
                "PARENT_DRIVE_SKILLS",
                ("fix-drive-errors",),
            ),
        ):
            problems: list[str] = []
            self.module.audit_parent_drive_skill_aliases(problems)

        self.assertEqual(
            [
                "Missing live parent Drive Codex fix-drive-errors skill symlink: "
                f"{codex_copy} -> "
                f"{canonical_root / 'google-drive/.codex/skills/fix-drive-errors'}"
            ],
            problems,
        )

    def test_audit_parent_drive_skill_aliases_reports_untracked_canonical_files(self):
        canonical_root = self.make_repo(["README.md"])
        parent_drive_root = canonical_root / "parent-drive"
        claude_skill = (
            canonical_root
            / "google-drive/.claude/skills/fix-drive-errors/SKILL.md"
        )
        codex_skill = (
            canonical_root
            / "google-drive/.codex/skills/fix-drive-errors/SKILL.md"
        )
        self.write_file(
            canonical_root,
            "google-drive/.claude/skills/fix-drive-errors/SKILL.md",
        )
        self.write_file(
            canonical_root,
            "google-drive/.codex/skills/fix-drive-errors/SKILL.md",
        )
        claude_alias = parent_drive_root / ".claude/skills/fix-drive-errors"
        codex_alias = parent_drive_root / ".codex/skills/fix-drive-errors"
        claude_alias.parent.mkdir(parents=True)
        codex_alias.parent.mkdir(parents=True)
        claude_alias.symlink_to(claude_skill.parent)
        codex_alias.symlink_to(codex_skill.parent)

        with (
            mock.patch.object(self.module, "ROOT", canonical_root),
            mock.patch.object(self.module, "PARENT_DRIVE_ROOT", parent_drive_root),
            mock.patch.object(
                self.module,
                "PARENT_DRIVE_SKILLS",
                ("fix-drive-errors",),
            ),
        ):
            problems: list[str] = []
            self.module.audit_parent_drive_skill_aliases(problems)

        self.assertEqual(
            [
                "Tracked parent Drive canonical skill file is not tracked: "
                "google-drive/.claude/skills/fix-drive-errors/SKILL.md",
                "Tracked parent Drive canonical skill file is not tracked: "
                "google-drive/.codex/skills/fix-drive-errors/SKILL.md",
            ],
            problems,
        )

    def test_audit_parent_drive_skill_aliases_reports_canonical_body_drift(self):
        canonical_root = self.make_repo(
            [
                "google-drive/.claude/skills/fix-drive-errors/SKILL.md",
                "google-drive/.codex/skills/fix-drive-errors/SKILL.md",
            ]
        )
        parent_drive_root = canonical_root / "parent-drive"
        claude_skill = (
            canonical_root
            / "google-drive/.claude/skills/fix-drive-errors/SKILL.md"
        )
        codex_skill = (
            canonical_root
            / "google-drive/.codex/skills/fix-drive-errors/SKILL.md"
        )
        codex_skill.write_text("drift\n", encoding="utf-8")
        claude_alias = parent_drive_root / ".claude/skills/fix-drive-errors"
        codex_alias = parent_drive_root / ".codex/skills/fix-drive-errors"
        claude_alias.parent.mkdir(parents=True)
        codex_alias.parent.mkdir(parents=True)
        claude_alias.symlink_to(claude_skill.parent)
        codex_alias.symlink_to(codex_skill.parent)

        with (
            mock.patch.object(self.module, "ROOT", canonical_root),
            mock.patch.object(self.module, "PARENT_DRIVE_ROOT", parent_drive_root),
            mock.patch.object(
                self.module,
                "PARENT_DRIVE_SKILLS",
                ("fix-drive-errors",),
            ),
        ):
            problems: list[str] = []
            self.module.audit_parent_drive_skill_aliases(problems)

        self.assertEqual(
            [
                "Tracked parent Drive skill pair is not synchronized: "
                "fix-drive-errors",
                "  - Project-local skill content drift after tool-specific "
                "frontmatter normalization: fix-drive-errors",
            ],
            problems,
        )

    def test_native_disabled_skill_audit_reports_unregistered_native_disables(self):
        root = self.make_repo(["skills-disabled.json", "codex/config.toml"])
        home = root / "home"
        self.write_file(
            root,
            "skills-disabled.json",
            json.dumps({"disabled": {"registered": {}}}),
        )
        self.write_file(
            root,
            "codex/config.toml",
            """
[[skills.config]]
path = "codex/skills/registered/SKILL.md"
enabled = false

[[skills.config]]
path = "codex/skills/stale-codex/SKILL.md"
enabled = false
""",
        )
        settings = home / ".claude/settings.local.json"
        settings.parent.mkdir(parents=True)
        settings.write_text(
            json.dumps(
                {
                    "skillOverrides": {
                        "registered": "off",
                        "stale-claude": "off",
                    }
                }
            ),
            encoding="utf-8",
        )

        with (
            mock.patch.object(self.module, "ROOT", root),
            mock.patch.object(
                self.module,
                "DISABLED_SKILLS_PATH",
                root / "skills-disabled.json",
            ),
            mock.patch.object(self.module.Path, "home", return_value=home),
        ):
            problems: list[str] = []
            self.module.check_native_disabled_skills(problems)

        self.assertEqual(
            [
                "Claude skillOverrides off entry missing from "
                "skills-disabled.json: stale-claude",
                "Codex skills.config disabled entry missing from "
                "skills-disabled.json: codex/skills/stale-codex/SKILL.md",
            ],
            problems,
        )

    def test_guard_commit_blocks_global_config_commit_when_full_audit_is_red(self):
        staged = {"claude/README.org"}
        audit_problem = "Hook missing from manifest: stale-hook.sh"
        output = io.StringIO()

        with (
            mock.patch.object(
                self.module,
                "read_input_json",
                return_value={"tool_input": {"command": "git commit -m test"}},
            ),
            mock.patch.object(self.module, "hook_cwd", return_value=DOTFILES),
            mock.patch.object(
                self.module,
                "command_paths_and_commit_repos",
                return_value=({}, {DOTFILES}),
            ),
            mock.patch.object(self.module, "dirty_paths", return_value=staged),
            mock.patch.object(self.module, "staged_paths", return_value=staged),
            mock.patch.object(
                self.module,
                "guard_manifests",
                return_value=({}, {}, {}, {}),
            ),
            mock.patch.object(self.module, "guard_changed_paths"),
            mock.patch.object(
                self.module,
                "audit_problems",
                return_value=[audit_problem],
                create=True,
            ),
            redirect_stdout(output),
        ):
            self.module.guard_commit()

        self.assertTrue(output.getvalue(), "red full audit should block the commit")
        result = json.loads(output.getvalue())
        hook_output = result["hookSpecificOutput"]
        self.assertEqual("deny", hook_output["permissionDecision"])
        self.assertIn(audit_problem, hook_output["permissionDecisionReason"])

    def test_guard_commit_does_not_run_full_audit_for_project_config_commit(self):
        repo = self.make_repo(["README.md"])
        staged = {".claude/hooks/example.sh"}
        output = io.StringIO()

        with (
            mock.patch.object(
                self.module,
                "read_input_json",
                return_value={"tool_input": {"command": "git commit -m test"}},
            ),
            mock.patch.object(self.module, "hook_cwd", return_value=repo),
            mock.patch.object(
                self.module,
                "command_paths_and_commit_repos",
                return_value=({}, {repo}),
            ),
            mock.patch.object(self.module, "dirty_paths", return_value=staged),
            mock.patch.object(self.module, "staged_paths", return_value=staged),
            mock.patch.object(self.module, "guard_changed_paths"),
            mock.patch.object(self.module, "audit_problems") as audit_mock,
            redirect_stdout(output),
        ):
            self.module.guard_commit()

        audit_mock.assert_not_called()
        self.assertEqual("", output.getvalue())


if __name__ == "__main__":
    unittest.main()
