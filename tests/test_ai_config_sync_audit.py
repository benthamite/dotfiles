from __future__ import annotations

import importlib.machinery
import importlib.util
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


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


if __name__ == "__main__":
    unittest.main()
