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


DOTFILES = Path(__file__).resolve().parents[1]


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

    def test_loaded_script_belongs_to_test_checkout(self):
        expected_root = Path(__file__).resolve().parents[1]

        self.assertEqual(
            expected_root / "bin" / "ai-config-sync",
            Path(self.module.__file__).resolve(),
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

    def test_guard_commit_blocks_global_config_commit_when_full_audit_is_red(self):
        staged = {"claude/README.org"}
        audit_problem = "Hook missing from manifest: stale-hook.sh"
        output = io.StringIO()

        with (
            mock.patch.object(self.module, "ROOT", DOTFILES),
            mock.patch.object(
                self.module,
                "read_input_json",
                return_value={"tool_input": {"command": "git commit -m test"}},
            ),
            mock.patch.object(self.module, "hook_cwd", return_value=DOTFILES),
            mock.patch.object(
                self.module,
                "command_paths_and_commit_repos",
                return_value=({}, {DOTFILES}, {}),
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
                return_value=({}, {repo}, {}),
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

    def make_parent_drive_workspace(self) -> Path:
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        return Path(temp.name)

    def test_parent_drive_pair_drift_is_reported(self):
        workspace = self.make_parent_drive_workspace()
        self.write_file(
            workspace,
            ".claude/skills/fix-drive-errors/SKILL.md",
            "---\nname: fix-drive-errors\nmodel: sonnet\n---\nnew recovery safeguards\n",
        )
        self.write_file(
            workspace,
            ".codex/skills/fix-drive-errors/SKILL.md",
            "---\nname: fix-drive-errors\n---\nold workflow\n",
        )
        self.write_file(workspace, ".claude/skills/nosync/SKILL.md")

        problems: list[str] = []
        self.module.audit_parent_drive_skill_pairs(problems, workspace)

        self.assertIn(
            "Parent-Drive skill content drift after tool-specific frontmatter normalization: fix-drive-errors",
            problems,
        )
        self.assertIn("Parent-Drive skill lacks Codex counterpart: nosync", problems)

    def test_parent_drive_pair_in_sync_passes(self):
        workspace = self.make_parent_drive_workspace()
        self.write_file(
            workspace,
            ".claude/skills/nosync/SKILL.md",
            "---\nname: nosync\nargument-hint: <dir>\nmodel: sonnet\n---\nshared body\n",
        )
        self.write_file(
            workspace,
            ".codex/skills/nosync/SKILL.md",
            "---\nname: nosync\n---\nshared body\n",
        )

        problems: list[str] = []
        self.module.audit_parent_drive_skill_pairs(problems, workspace)

        self.assertEqual([], problems)

    def test_stale_skill_override_without_record_is_reported(self):
        home = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: __import__("shutil").rmtree(home, ignore_errors=True))
        settings = home / ".claude" / "settings.local.json"
        settings.parent.mkdir(parents=True)
        settings.write_text(json.dumps({"skillOverrides": {"ghost-skill": "off"}}))

        problems: list[str] = []
        with (
            mock.patch.object(self.module, "disabled_skill_records", return_value={}),
            mock.patch("pathlib.Path.home", return_value=home),
        ):
            self.module.check_native_disabled_skills(problems)

        self.assertIn(
            "Claude skillOverrides off entry has no skills-disabled.json record: ghost-skill",
            problems,
        )

    def test_parent_drive_absent_workspace_is_skipped(self):
        workspace = self.make_parent_drive_workspace()

        problems: list[str] = []
        self.module.audit_parent_drive_skill_pairs(problems, workspace)

        self.assertEqual([], problems)

    def make_project_local_pair(self, claude_body: str, codex_body: str, evals: tuple[str, str]):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        root = Path(temp.name)
        for side, body, evaluation in (
            (".claude", claude_body, evals[0]),
            (".codex", codex_body, evals[1]),
        ):
            skill = root / side / "skills" / "publish-dotfiles"
            (skill / "evals").mkdir(parents=True)
            (skill / "SKILL.md").write_text(body, encoding="utf-8")
            (skill / "evals" / "scenarios.md").write_text(evaluation, encoding="utf-8")
        return root

    CLAUDE_SKILL = (
        "---\nname: publish-dotfiles\ndescription: Guarded publication.\n"
        "user-invocable: true\nmodel: opus\n---\n\nBody.\n"
    )
    CODEX_SKILL = "---\nname: publish-dotfiles\ndescription: Guarded publication.\n---\n\nBody.\n"

    def test_project_local_publish_dotfiles_pair_is_registered(self):
        self.assertIn("publish-dotfiles", self.module.DOTFILES_PROJECT_LOCAL_SKILLS)

    def test_dotfiles_project_local_allowlist_matches_actual_claude_skills(self):
        actual = {
            path.parent.name
            for path in (DOTFILES / ".claude/skills").glob("*/SKILL.md")
        }

        self.assertEqual(actual, self.module.DOTFILES_PROJECT_LOCAL_SKILLS)

    def test_dotfiles_project_local_allowlist_matches_actual_codex_skills(self):
        actual = {
            path.parent.name
            for path in (DOTFILES / ".codex/skills").glob("*/SKILL.md")
        }

        self.assertEqual(actual, self.module.DOTFILES_PROJECT_LOCAL_SKILLS)

    def test_default_project_local_documentation_owner_is_root_readme(self):
        repo = self.make_repo(["README.org"])

        self.assertEqual(
            [],
            self.module.local_skill_readme_problems(
                {"README.org"}, "example", "skills", repo
            ),
        )
        problems = self.module.local_skill_readme_problems(
            {".claude/skills/example/SKILL.md"}, "example", "skills", repo
        )

        self.assertEqual(1, len(problems))
        self.assertIn("README.org", problems[0])
        self.assertNotIn("Skills section", problems[0])

    def test_configured_manual_documentation_owner_must_change(self):
        repo = self.make_repo(["README.md"])
        self.write_file(repo, "docs/project-skills.org")
        self.write_file(
            repo,
            "ai-config-sync.json",
            json.dumps(
                {
                    "policy": {
                        "project_local_documentation_owner": "docs/project-skills.org",
                        "project_local_documentation_mode": "manual",
                    }
                }
            ),
        )

        self.assertEqual(
            [],
            self.module.local_skill_readme_problems(
                {"docs/project-skills.org"}, "example", "skills", repo
            ),
        )
        problems = self.module.local_skill_readme_problems(
            {"README.org"}, "example", "skills", repo
        )

        self.assertEqual(1, len(problems))
        self.assertIn("docs/project-skills.org", problems[0])
        self.assertNotIn("README.org Skills", problems[0])

    def test_manual_directory_owner_is_not_satisfied_by_a_changed_descendant(self):
        repo = self.make_repo(["README.md", ".claude/skills/example/SKILL.md"])
        owner = ".claude/skills"
        self.write_file(
            repo,
            "ai-config-sync.json",
            json.dumps(
                {
                    "policy": {
                        "project_local_documentation_owner": owner,
                        "project_local_documentation_mode": "manual",
                    }
                }
            ),
        )

        problems = self.module.local_skill_readme_problems(
            {".claude/skills/example/SKILL.md"}, "example", "skills", repo
        )

        self.assertEqual(1, len(problems))
        self.assertIn(owner, problems[0])

    def test_missing_or_deleted_manual_owner_fails_even_when_named_as_changed(self):
        owner = "docs/project-skills.org"

        for state in ("missing", "deleted"):
            with self.subTest(state=state):
                repo = self.make_repo(["README.md"])
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "manual",
                            }
                        }
                    ),
                )
                if state == "deleted":
                    self.write_file(repo, owner)
                    (repo / owner).unlink()

                problems = self.module.local_skill_readme_problems(
                    {owner}, "example", "skills", repo
                )

                self.assertEqual(1, len(problems))
                self.assertIn(owner, problems[0])

    def test_manual_owner_symlinks_fail_without_reading_external_content(self):
        owner = "docs/project-skills.org"

        for symlink_kind in ("owner", "folder"):
            with self.subTest(symlink_kind=symlink_kind):
                repo = self.make_repo(["README.md"])
                external = tempfile.TemporaryDirectory()
                self.addCleanup(external.cleanup)
                external_root = Path(external.name)
                external_owner = external_root / "project-skills.org"
                external_owner.write_text("external private content\n", encoding="utf-8")

                if symlink_kind == "owner":
                    (repo / "docs").mkdir()
                    (repo / owner).symlink_to(external_owner)
                else:
                    (repo / "docs").symlink_to(external_root, target_is_directory=True)

                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "manual",
                            }
                        }
                    ),
                )
                original_read_text = Path.read_text

                def guarded_read_text(path: Path, *args, **kwargs):
                    if path.resolve(strict=False) == external_owner.resolve(strict=False):
                        raise AssertionError("external owner content was read")
                    return original_read_text(path, *args, **kwargs)

                with mock.patch.object(Path, "read_text", guarded_read_text):
                    problems = self.module.local_skill_readme_problems(
                        {owner}, "example", "skills", repo
                    )

                self.assertEqual(1, len(problems))
                self.assertIn(owner, problems[0])
                self.assertNotIn("symlink", problems[0].lower())
                self.assertNotIn(str(external_root), problems[0])

    def test_invalid_documentation_mode_defaults_to_manual(self):
        repo = self.make_repo(["README.md"])
        owner = "docs/project-skills.org"
        self.write_file(repo, owner)
        self.write_file(
            repo,
            "ai-config-sync.json",
            json.dumps(
                {
                    "policy": {
                        "project_local_documentation_owner": owner,
                        "project_local_documentation_mode": "automatic",
                    }
                }
            ),
        )

        problems = self.module.local_skill_readme_problems(
            {".claude/skills/example/SKILL.md"}, "example", "skills", repo
        )

        self.assertEqual(1, len(problems))
        self.assertIn(owner, problems[0])
        self.assertEqual(
            [],
            self.module.local_skill_readme_problems(
                {owner}, "example", "skills", repo
            ),
        )

    def test_non_string_documentation_modes_default_to_manual(self):
        repo = self.make_repo(["README.md"])
        owner = "docs/project-skills.org"
        self.write_file(repo, owner)

        for mode in ([], {}, None, 1):
            with self.subTest(mode=mode):
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": mode,
                            }
                        }
                    ),
                )

                problems = self.module.local_skill_readme_problems(
                    {".claude/skills/example/SKILL.md"},
                    "example",
                    "skills",
                    repo,
                )

                self.assertEqual(1, len(problems))
                self.assertIn(owner, problems[0])

    def test_invalid_documentation_owner_fails_closed_to_root_readme(self):
        repo = self.make_repo(["README.org"])
        invalid_owners = (
            "",
            "/README.org",
            ".",
            "..",
            "docs/../README.org",
            "docs//README.org",
            "docs\\README.org",
            "C:/README.org",
            "C:README.org",
            "\\\\server\\share\\README.org",
        )

        for owner in invalid_owners:
            with self.subTest(owner=owner):
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "generated",
                            }
                        }
                    ),
                )

                problems = self.module.local_skill_readme_problems(
                    {"agents/skill-inventory.org"}, "example", "skills", repo
                )

                self.assertEqual(1, len(problems))
                self.assertIn("README.org", problems[0])
                self.assertEqual(
                    [],
                    self.module.local_skill_readme_problems(
                        {"README.org"}, "example", "skills", repo
                    ),
                )

    def test_dotfiles_linked_worktree_generated_owner_needs_no_noop_change(self):
        changed = {
            ".claude/skills/config-audit/SKILL.md",
            ".codex/skills/config-audit/SKILL.md",
        }

        self.assertTrue(self.module.same_git_repository(DOTFILES, self.module.ROOT))
        problems = self.module.local_skill_readme_problems(
            changed, "config-audit", "skills", DOTFILES
        )

        self.assertEqual([], problems)

    def test_repository_identity_uses_real_common_git_directory(self):
        workspace = tempfile.TemporaryDirectory()
        self.addCleanup(workspace.cleanup)
        workspace_root = Path(workspace.name)
        primary = workspace_root / "primary"
        linked = workspace_root / "linked"
        foreign = workspace_root / "foreign"
        primary.mkdir()
        foreign.mkdir()
        for repo in (primary, foreign):
            self.run_git(repo, "init")
            self.run_git(repo, "config", "user.email", "test@example.com")
            self.run_git(repo, "config", "user.name", "Test User")
            self.write_file(repo, "README.md")
            self.run_git(repo, "add", "README.md")
            self.run_git(repo, "commit", "-m", "initial")
        self.run_git(primary, "worktree", "add", "--detach", str(linked), "HEAD")

        self.assertTrue(self.module.same_git_repository(primary, linked))
        self.assertFalse(self.module.same_git_repository(primary, foreign))

    def test_noncanonical_generated_mode_still_requires_configured_owner(self):
        repo = self.make_repo(["README.md"])
        owner = "agents/skill-inventory.org"
        self.write_file(repo, owner)
        self.write_file(
            repo,
            "ai-config-sync.json",
            json.dumps(
                {
                    "policy": {
                        "project_local_documentation_owner": owner,
                        "project_local_documentation_mode": "generated",
                    }
                }
            ),
        )

        problems = self.module.local_skill_readme_problems(
            {".claude/skills/example/SKILL.md"}, "example", "skills", repo
        )

        self.assertEqual(1, len(problems))
        self.assertIn(owner, problems[0])
        self.assertEqual(
            [],
            self.module.local_skill_readme_problems(
                {owner}, "example", "skills", repo
            ),
        )

    def test_all_local_skill_guard_branches_use_the_repo_documentation_owner(self):
        owner = "docs/project-skills.org"
        cases = (
            (".claude", "skills"),
            (".codex", "skills"),
            (".claude", "programmatic-skills"),
            (".codex", "programmatic-skills"),
        )

        for side, skill_root in cases:
            with self.subTest(side=side, skill_root=skill_root):
                claude_skill = f".claude/{skill_root}/example/SKILL.md"
                codex_skill = f".codex/{skill_root}/example/SKILL.md"
                repo = self.make_repo(["README.md", claude_skill, codex_skill])
                self.write_file(repo, owner)
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "manual",
                            }
                        }
                    ),
                )
                changed_skill = f"{side}/{skill_root}/example/SKILL.md"
                changed_paths = {changed_skill, owner}
                problems: list[str] = []

                with mock.patch.object(
                    self.module,
                    "local_skill_readme_problems",
                    wraps=self.module.local_skill_readme_problems,
                ) as documentation_check:
                    self.module.guard_changed_paths(
                        repo,
                        changed_paths,
                        changed_paths,
                        changed_paths,
                        problems,
                        False,
                        {},
                        {},
                        {},
                        {},
                    )

                self.assertEqual([], problems)
                documentation_check.assert_called_once_with(
                    changed_paths, "example", skill_root, repo
                )

                problems = []
                skill_only = {changed_skill}
                self.module.guard_changed_paths(
                    repo,
                    skill_only,
                    skill_only,
                    skill_only,
                    problems,
                    False,
                    {},
                    {},
                    {},
                    {},
                )

                self.assertIn(owner, "\n".join(problems))

    def test_staged_local_skill_guards_require_the_real_manual_owner_file(self):
        owner = "docs/project-skills.org"
        cases = (
            (".claude", "skills"),
            (".codex", "skills"),
            (".claude", "programmatic-skills"),
            (".codex", "programmatic-skills"),
        )

        for side, skill_root in cases:
            with self.subTest(side=side, skill_root=skill_root):
                changed_skill = f"{side}/{skill_root}/example/SKILL.md"
                counterpart_side = ".codex" if side == ".claude" else ".claude"
                counterpart_skill = (
                    f"{counterpart_side}/{skill_root}/example/SKILL.md"
                )
                repo = self.make_repo(
                    [
                        "README.md",
                        changed_skill,
                        counterpart_skill,
                        owner,
                        "ai-config-sync.json",
                    ]
                )
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "manual",
                            }
                        }
                    ),
                )
                self.run_git(
                    repo,
                    "add",
                    "-f",
                    "--",
                    "ai-config-sync.json",
                    changed_skill,
                    counterpart_skill,
                )
                self.run_git(repo, "commit", "-m", "configure documentation owner")
                self.write_file(repo, changed_skill, "updated skill\n")
                self.write_file(repo, counterpart_skill, "updated skill\n")
                self.write_file(repo, owner, "updated documentation\n")
                self.run_git(
                    repo,
                    "add",
                    "-f",
                    "--",
                    changed_skill,
                    counterpart_skill,
                    owner,
                )
                payload = {
                    "tool_input": {
                        "command": "git commit -m test",
                        "cwd": str(repo),
                    }
                }

                def guard_output() -> str:
                    output = io.StringIO()
                    with (
                        mock.patch.object(self.module, "read_input_json", return_value=payload),
                        redirect_stdout(output),
                    ):
                        self.module.guard_commit()
                    return output.getvalue()

                self.assertEqual(
                    {changed_skill, counterpart_skill, owner},
                    self.module.staged_paths(repo),
                )
                self.assertEqual("", guard_output())

                self.run_git(repo, "restore", "--staged", "--", owner)
                self.assertEqual(
                    {changed_skill, counterpart_skill},
                    self.module.staged_paths(repo),
                )
                omitted_output = guard_output()
                self.assertTrue(omitted_output)
                self.assertIn(owner, omitted_output)

                (repo / owner).unlink()
                self.run_git(repo, "add", "-u", "--", owner)
                self.write_file(repo, owner, "unstaged regular replacement\n")
                self.assertEqual(
                    {changed_skill, counterpart_skill, owner},
                    self.module.staged_paths(repo),
                )
                deleted_output = guard_output()
                self.assertTrue(deleted_output)
                self.assertIn(owner, deleted_output)

    def test_staged_owner_symlinks_cannot_be_hidden_by_unstaged_regular_files(self):
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        owner = "docs/project-skills.org"

        for symlink_kind in ("owner", "parent"):
            with self.subTest(symlink_kind=symlink_kind):
                repo = self.make_repo(
                    [
                        "README.md",
                        claude_skill,
                        codex_skill,
                        owner,
                        "ai-config-sync.json",
                    ]
                )
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "manual",
                            }
                        }
                    ),
                )
                self.run_git(repo, "add", "ai-config-sync.json")
                self.run_git(repo, "commit", "-m", "configure documentation owner")
                self.write_file(repo, claude_skill, "updated skill\n")
                self.write_file(repo, codex_skill, "updated skill\n")
                self.run_git(repo, "add", claude_skill, codex_skill)

                external = tempfile.TemporaryDirectory()
                self.addCleanup(external.cleanup)
                external_root = Path(external.name)
                external_owner = external_root / "project-skills.org"
                external_owner.write_text("external private content\n", encoding="utf-8")
                (repo / owner).unlink()
                if symlink_kind == "owner":
                    (repo / owner).symlink_to(external_owner)
                    self.run_git(repo, "add", "-f", "--", owner)
                    (repo / owner).unlink()
                else:
                    (repo / "docs").rmdir()
                    (repo / "docs").symlink_to(external_root, target_is_directory=True)
                    self.run_git(repo, "add", "-A", "--", "docs")
                    (repo / "docs").unlink()
                    (repo / "docs").mkdir()
                self.write_file(repo, owner, "unstaged regular replacement\n")
                output = io.StringIO()
                original_read_text = Path.read_text

                def guarded_read_text(path: Path, *args, **kwargs):
                    if path.resolve(strict=False) == external_owner.resolve(strict=False):
                        raise AssertionError("external owner content was read")
                    return original_read_text(path, *args, **kwargs)

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {
                                "command": "git commit -m test",
                                "cwd": str(repo),
                            }
                        },
                    ),
                    mock.patch.object(Path, "read_text", guarded_read_text),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn(owner, output.getvalue())

    def test_update_add_does_not_plan_an_untracked_owner(self):
        owner = "docs/project-skills.org"
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        repo = self.make_repo(
            [claude_skill, codex_skill, "ai-config-sync.json"]
        )
        self.write_file(
            repo,
            "ai-config-sync.json",
            json.dumps(
                {
                    "policy": {
                        "project_local_documentation_owner": owner,
                        "project_local_documentation_mode": "manual",
                    }
                }
            ),
        )
        self.run_git(repo, "add", "ai-config-sync.json")
        self.run_git(repo, "commit", "-m", "configure owner")
        self.write_file(repo, claude_skill, "version two\n")
        self.write_file(repo, codex_skill, "version two\n")
        self.run_git(repo, "add", claude_skill, codex_skill)
        self.write_file(repo, owner, "untracked documentation\n")
        output = io.StringIO()

        with (
            mock.patch.object(
                self.module,
                "read_input_json",
                return_value={
                    "tool_input": {
                        "command": f"git add -u -- {owner} && git commit -m test",
                        "cwd": str(repo),
                    }
                },
            ),
            redirect_stdout(output),
        ):
            self.module.guard_commit()

        self.assertTrue(output.getvalue())
        self.assertIn(owner, output.getvalue())

    def test_staged_manifest_controls_owner_despite_unstaged_manifest_change(self):
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        manifest = "ai-config-sync.json"
        owner_a = "docs/owner-a.org"
        owner_b = "docs/owner-b.org"
        repo = self.make_repo(
            ["README.md", claude_skill, codex_skill, owner_a, owner_b, manifest]
        )
        staged_manifest = {
            "policy": {
                "project_local_documentation_owner": owner_a,
                "project_local_documentation_mode": "manual",
            }
        }
        working_manifest = {
            "policy": {
                "project_local_documentation_owner": owner_b,
                "project_local_documentation_mode": "manual",
            }
        }
        self.write_file(repo, manifest, json.dumps(staged_manifest))
        self.run_git(repo, "add", manifest)
        self.write_file(repo, manifest, json.dumps(working_manifest))
        self.write_file(repo, claude_skill, "updated skill\n")
        self.write_file(repo, codex_skill, "updated skill\n")
        self.write_file(repo, owner_b, "updated owner B\n")
        self.run_git(repo, "add", claude_skill, codex_skill, owner_b)
        payload = {
            "tool_input": {
                "command": "git commit -m test",
                "cwd": str(repo),
            }
        }

        def guard_output() -> str:
            output = io.StringIO()
            with (
                mock.patch.object(self.module, "read_input_json", return_value=payload),
                redirect_stdout(output),
            ):
                self.module.guard_commit()
            return output.getvalue()

        blocked = guard_output()
        self.assertTrue(blocked)
        self.assertIn(owner_a, blocked)
        self.assertNotIn(f"documentation owner {owner_b}", blocked)

        self.write_file(repo, owner_a, "updated owner A\n")
        self.run_git(repo, "add", owner_a)

        self.assertEqual("", guard_output())

    def test_unstaged_single_agent_opt_out_does_not_change_candidate_policy(self):
        skill = ".claude/skills/example/SKILL.md"
        manifest = "ai-config-sync.json"
        owner = "docs/project-skills.org"
        repo = self.make_repo(["README.md", skill, owner, manifest])
        staged_manifest = {
            "policy": {
                "project_local_documentation_owner": owner,
                "project_local_documentation_mode": "manual",
            }
        }
        working_manifest = {
            **staged_manifest,
            "local": {"skills": "claude-only"},
        }
        self.write_file(repo, manifest, json.dumps(staged_manifest))
        self.run_git(repo, "add", manifest)
        self.write_file(repo, manifest, json.dumps(working_manifest))
        self.write_file(repo, skill, "updated skill\n")
        self.run_git(repo, "add", skill)
        output = io.StringIO()

        with (
            mock.patch.object(
                self.module,
                "read_input_json",
                return_value={
                    "tool_input": {
                        "command": "git commit -m test",
                        "cwd": str(repo),
                    }
                },
            ),
            mock.patch.object(
                self.module, "local_skill_sync_problems", return_value=[]
            ),
            redirect_stdout(output),
        ):
            self.module.guard_commit()

        self.assertTrue(output.getvalue())
        self.assertIn(owner, output.getvalue())

    def test_one_shot_add_and_commit_uses_planned_manifest_and_owner(self):
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        manifest = "ai-config-sync.json"
        owner = "docs/project-skills.org"
        repo = self.make_repo(
            ["README.md", claude_skill, codex_skill, owner, manifest]
        )
        self.write_file(
            repo,
            manifest,
            json.dumps(
                {
                    "policy": {
                        "project_local_documentation_owner": owner,
                        "project_local_documentation_mode": "manual",
                    }
                }
            ),
        )
        self.write_file(repo, claude_skill, "updated skill\n")
        self.write_file(repo, codex_skill, "updated skill\n")
        self.write_file(repo, owner, "updated documentation\n")
        output = io.StringIO()

        with (
            mock.patch.object(
                self.module,
                "read_input_json",
                return_value={
                    "tool_input": {
                        "command": (
                            f"git add -- {manifest} {claude_skill} {codex_skill} {owner} "
                            "&& git commit -m test"
                        ),
                        "cwd": str(repo),
                    }
                },
            ),
            redirect_stdout(output),
        ):
            self.module.guard_commit()

        self.assertEqual("", output.getvalue())

    def test_nonstaging_or_excluding_add_does_not_plan_documentation_owner(self):
        owner = "docs/project-skills.org"
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        commands = (
            f"git add --dry-run -- {owner} && git commit -m test",
            f"git add {owner} --dry-run && git commit -m test",
            f"git add {owner} --refresh && git commit -m test",
            "git add --pathspec-from-file=paths.txt && git commit -m test",
            f"git add -- . ':(exclude){owner}' && git commit -m test",
        )

        for command in commands:
            with self.subTest(command=command):
                repo = self.make_repo(
                    [
                        "README.md",
                        claude_skill,
                        codex_skill,
                        owner,
                        "ai-config-sync.json",
                    ]
                )
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "manual",
                            }
                        }
                    ),
                )
                self.run_git(repo, "add", "ai-config-sync.json")
                self.run_git(repo, "commit", "-m", "configure owner")
                self.write_file(repo, claude_skill, "version two\n")
                self.write_file(repo, codex_skill, "version two\n")
                self.write_file(repo, owner, "updated documentation\n")
                self.write_file(repo, "paths.txt", owner + "\n")
                self.run_git(repo, "add", claude_skill, codex_skill)
                output = io.StringIO()

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {"command": command, "cwd": str(repo)}
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn(owner, output.getvalue())
                if "pathspec" in command or "exclude" in command:
                    self.assertIn("Cannot safely model", output.getvalue())

    def test_unsupported_commit_content_modes_are_blocked_explicitly(self):
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        commands = (
            f"git commit --include {claude_skill} {codex_skill} -m test",
            f"git commit --only {claude_skill} {codex_skill} -m test",
            "git commit --patch -m test",
            "git commit --pathspec-from-file=paths.txt -m test",
        )

        for command in commands:
            with self.subTest(command=command):
                repo = self.make_repo([claude_skill, codex_skill])
                self.write_file(repo, claude_skill, "version two\n")
                self.write_file(repo, codex_skill, "version two\n")
                self.write_file(repo, "paths.txt", claude_skill + "\n")
                output = io.StringIO()

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {"command": command, "cwd": str(repo)}
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn("Cannot safely model git commit", output.getvalue())

    def test_commit_all_with_attached_message_uses_working_candidate(self):
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"

        for command in ("git commit -am test", "git commit -amtest"):
            with self.subTest(command=command):
                repo = self.make_repo(
                    ["README.org", claude_skill, codex_skill]
                )
                self.write_file(repo, claude_skill, "version two\n")
                self.write_file(repo, codex_skill, "version two\n")
                self.write_file(repo, "README.org", "updated documentation\n")
                output = io.StringIO()

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {"command": command, "cwd": str(repo)}
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertEqual("", output.getvalue())

    def test_unreliable_add_commit_ordering_is_blocked_explicitly(self):
        owner = "docs/project-skills.org"
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        add = f"git add -- {claude_skill} {codex_skill} {owner}"
        commands = (
            f"{add}; git commit -m test",
            f"{add} || true; git commit -m test",
            f"{add} && touch intervening && git commit -m test",
            f"{add} && git reset -- {owner} && git commit -m test",
        )

        for command in commands:
            with self.subTest(command=command):
                repo = self.make_repo(
                    [claude_skill, codex_skill, owner, "ai-config-sync.json"]
                )
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "manual",
                            }
                        }
                    ),
                )
                self.run_git(repo, "add", "ai-config-sync.json")
                self.run_git(repo, "commit", "-m", "configure owner")
                self.write_file(repo, claude_skill, "version two\n")
                self.write_file(repo, codex_skill, "version two\n")
                self.write_file(repo, owner, "updated documentation\n")
                output = io.StringIO()

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {"command": command, "cwd": str(repo)}
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn("Cannot safely model", output.getvalue())

    def test_boolean_add_alternatives_are_blocked_explicitly(self):
        owner = "docs/project-skills.org"
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        commands = (
            (
                f"git add --dry-run -- {owner} || git add -- {owner} "
                "&& git commit -m test"
            ),
            (
                f"git add -- {owner} || git add -- {owner} "
                "&& git commit -m test"
            ),
        )

        for command in commands:
            with self.subTest(command=command):
                repo = self.make_repo(
                    [claude_skill, codex_skill, owner, "ai-config-sync.json"]
                )
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "manual",
                            }
                        }
                    ),
                )
                self.run_git(repo, "add", "ai-config-sync.json")
                self.run_git(repo, "commit", "-m", "configure owner")
                self.write_file(repo, claude_skill, "version two\n")
                self.write_file(repo, codex_skill, "version two\n")
                self.write_file(repo, owner, "updated documentation\n")
                self.run_git(repo, "add", claude_skill, codex_skill)
                output = io.StringIO()

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {"command": command, "cwd": str(repo)}
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn("Cannot safely model git add content", output.getvalue())

    def test_ignore_removal_add_modes_are_blocked_explicitly(self):
        owner = "docs/project-skills.org"
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"

        for option in ("--ignore-removal", "--no-all"):
            with self.subTest(option=option):
                repo = self.make_repo(
                    [claude_skill, codex_skill, owner, "ai-config-sync.json"]
                )
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "manual",
                            }
                        }
                    ),
                )
                self.run_git(repo, "add", "ai-config-sync.json")
                self.run_git(repo, "commit", "-m", "configure owner")
                (repo / claude_skill).unlink()
                self.write_file(repo, owner, "updated documentation\n")
                output = io.StringIO()

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {
                                "command": (
                                    f"git add {option} -- {claude_skill} {owner} "
                                    "&& git commit -m test"
                                ),
                                "cwd": str(repo),
                            }
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn(f"git add option {option}", output.getvalue())

    def test_newline_add_commit_chains_are_detected_and_blocked(self):
        owner = "docs/project-skills.org"
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"

        for separator in ("\n", "\r\n"):
            with self.subTest(separator=repr(separator)):
                repo = self.make_repo(
                    [claude_skill, codex_skill, owner, "ai-config-sync.json"]
                )
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "manual",
                            }
                        }
                    ),
                )
                self.run_git(repo, "add", "ai-config-sync.json")
                self.run_git(repo, "commit", "-m", "configure owner")
                self.write_file(repo, claude_skill, "version two\n")
                self.write_file(repo, codex_skill, "version two\n")
                self.write_file(repo, owner, "updated documentation\n")
                self.run_git(repo, "add", claude_skill, codex_skill)
                output = io.StringIO()

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {
                                "command": (
                                    f"git add -- {owner}{separator}git commit -m test"
                                ),
                                "cwd": str(repo),
                            }
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn("Cannot safely model git add content", output.getvalue())

                output = io.StringIO()
                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {
                                "command": (
                                    f"git add -- {owner} &&{separator}"
                                    "git commit -m test"
                                ),
                                "cwd": str(repo),
                            }
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertEqual("", output.getvalue())

    def test_quoted_newline_in_commit_message_is_not_a_shell_separator(self):
        owner = "README.org"
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        repo = self.make_repo([owner, claude_skill, codex_skill])
        self.write_file(repo, claude_skill, "version two\n")
        self.write_file(repo, codex_skill, "version two\n")
        self.write_file(repo, owner, "updated documentation\n")
        self.run_git(repo, "add", owner, claude_skill, codex_skill)
        output = io.StringIO()

        with (
            mock.patch.object(
                self.module,
                "read_input_json",
                return_value={
                    "tool_input": {
                        "command": "git commit -m 'line one\nline two'",
                        "cwd": str(repo),
                    }
                },
            ),
            redirect_stdout(output),
        ):
            self.module.guard_commit()

        self.assertEqual("", output.getvalue())

    def test_safe_git_command_wrappers_still_run_candidate_validation(self):
        owner = "README.org"
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"

        for wrapper in ("command", "env"):
            with self.subTest(wrapper=wrapper):
                repo = self.make_repo([owner, claude_skill, codex_skill])
                self.write_file(repo, claude_skill, "version two\n")
                self.write_file(repo, owner, "updated documentation\n")
                self.run_git(repo, "add", owner, claude_skill)
                output = io.StringIO()

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {
                                "command": f"{wrapper} git commit -m test",
                                "cwd": str(repo),
                            }
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn("content drift", output.getvalue())

    def test_unsafe_git_command_wrappers_are_blocked_explicitly(self):
        commands = (
            "env GIT_INDEX_FILE=/tmp/other-index git commit -m test",
            "env -i git commit -m test",
            "env -S 'git commit -m test'",
            "env -S 'NAME=value git commit -m test'",
            "env -S '-i git commit -m test'",
            "env --split-string='-u NAME git commit -m test'",
            "env --split-string 'git commit -m test'",
            "env --split-string='git commit -m test'",
            "command -p git commit -m test",
            "GIT_WORK_TREE=/tmp/other-tree git commit -m test",
            "GIT_INDEX_FILE=/tmp/git git commit -m test",
        )

        for command in commands:
            with self.subTest(command=command):
                repo = self.make_repo(["README.md"])
                output = io.StringIO()
                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {"command": command, "cwd": str(repo)}
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn("wrapped git commit", output.getvalue())

    def test_env_split_string_does_not_false_match_git_arguments(self):
        commands = (
            "env -S 'printf git commit'",
            "env -S 'NAME=value printf git commit'",
        )

        for command in commands:
            with self.subTest(command=command):
                repo = self.make_repo(["README.md"])
                output = io.StringIO()
                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {
                                "command": command,
                                "cwd": str(repo),
                            }
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertEqual("", output.getvalue())

    def test_staged_skill_drift_cannot_be_hidden_by_unstaged_counterpart(self):
        owner = "docs/project-skills.org"
        cases = (
            ("skills", ".claude", ".codex"),
            ("skills", ".codex", ".claude"),
            ("programmatic-skills", ".claude", ".codex"),
            ("programmatic-skills", ".codex", ".claude"),
        )

        for skill_root, staged_side, unstaged_side in cases:
            with self.subTest(
                skill_root=skill_root,
                staged_side=staged_side,
            ):
                staged_skill = f"{staged_side}/{skill_root}/example/SKILL.md"
                unstaged_skill = f"{unstaged_side}/{skill_root}/example/SKILL.md"
                repo = self.make_repo(
                    [
                        "README.md",
                        staged_skill,
                        unstaged_skill,
                        owner,
                        "ai-config-sync.json",
                    ]
                )
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "manual",
                            }
                        }
                    ),
                )
                self.run_git(
                    repo,
                    "add",
                    "-f",
                    "--",
                    "ai-config-sync.json",
                    staged_skill,
                    unstaged_skill,
                )
                self.run_git(repo, "commit", "-m", "configure owner")
                self.write_file(repo, staged_skill, "version two\n")
                self.write_file(repo, owner, "updated documentation\n")
                self.run_git(repo, "add", "-f", "--", staged_skill, owner)
                self.write_file(repo, unstaged_skill, "version two\n")
                output = io.StringIO()

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {
                                "command": "git commit -m test",
                                "cwd": str(repo),
                            }
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn("content drift", output.getvalue())

    def test_staged_aux_drift_cannot_be_hidden_by_unstaged_counterpart(self):
        owner = "docs/project-skills.org"
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        claude_aux = ".claude/skills/example/evals/scenarios.md"
        codex_aux = ".codex/skills/example/evals/scenarios.md"
        repo = self.make_repo(
            [
                "README.md",
                claude_skill,
                codex_skill,
                claude_aux,
                codex_aux,
                owner,
                "ai-config-sync.json",
            ]
        )
        self.write_file(
            repo,
            "ai-config-sync.json",
            json.dumps(
                {
                    "policy": {
                        "project_local_documentation_owner": owner,
                        "project_local_documentation_mode": "manual",
                    }
                }
            ),
        )
        self.run_git(repo, "add", "ai-config-sync.json")
        self.run_git(repo, "commit", "-m", "configure owner")
        (repo / claude_aux).write_bytes(b"\xff\x00candidate auxiliary\n")
        self.write_file(repo, owner, "updated documentation\n")
        self.run_git(repo, "add", claude_aux, owner)
        (repo / codex_aux).write_bytes(b"\xff\x00candidate auxiliary\n")
        output = io.StringIO()

        with (
            mock.patch.object(
                self.module,
                "read_input_json",
                return_value={
                    "tool_input": {
                        "command": "git commit -m test",
                        "cwd": str(repo),
                    }
                },
            ),
            redirect_stdout(output),
        ):
            self.module.guard_commit()

        self.assertTrue(output.getvalue())
        self.assertIn("Auxiliary project-local skill file drift", output.getvalue())

    def test_staged_skill_deletion_cannot_be_hidden_by_unstaged_replacement(self):
        owner = "docs/project-skills.org"
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        repo = self.make_repo(
            ["README.md", claude_skill, codex_skill, owner, "ai-config-sync.json"]
        )
        self.write_file(
            repo,
            "ai-config-sync.json",
            json.dumps(
                {
                    "policy": {
                        "project_local_documentation_owner": owner,
                        "project_local_documentation_mode": "manual",
                    }
                }
            ),
        )
        self.run_git(repo, "add", "ai-config-sync.json")
        self.run_git(repo, "commit", "-m", "configure owner")
        (repo / claude_skill).unlink()
        self.write_file(repo, owner, "updated documentation\n")
        self.run_git(repo, "add", "-u", claude_skill)
        self.run_git(repo, "add", owner)
        self.write_file(repo, claude_skill, "body\n")
        output = io.StringIO()

        with (
            mock.patch.object(
                self.module,
                "read_input_json",
                return_value={
                    "tool_input": {
                        "command": "git commit -m test",
                        "cwd": str(repo),
                    }
                },
            ),
            redirect_stdout(output),
        ):
            self.module.guard_commit()

        self.assertTrue(output.getvalue())
        self.assertIn("Missing Claude project-local skill file", output.getvalue())

    def test_candidate_manifest_requires_top_level_json_object(self):
        owner = "README.org"
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"

        for value in ([], "not an object", 7):
            with self.subTest(value=value):
                repo = self.make_repo(
                    [owner, claude_skill, codex_skill, "ai-config-sync.json"]
                )
                self.write_file(repo, "ai-config-sync.json", json.dumps(value))
                self.write_file(repo, claude_skill, "version two\n")
                self.write_file(repo, codex_skill, "version two\n")
                self.write_file(repo, owner, "updated documentation\n")
                self.run_git(
                    repo,
                    "add",
                    "ai-config-sync.json",
                    claude_skill,
                    codex_skill,
                    owner,
                )
                output = io.StringIO()

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {
                                "command": "git commit -m test",
                                "cwd": str(repo),
                            }
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn("top-level JSON object", output.getvalue())

    def test_candidate_manifest_rejects_malformed_list_sections(self):
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"

        for value in ({"skills": {}}, {"skills": [7]}):
            with self.subTest(value=value):
                repo = self.make_repo(
                    ["README.org", claude_skill, codex_skill, "ai-config-sync.json"]
                )
                self.write_file(repo, "ai-config-sync.json", json.dumps(value))
                self.write_file(repo, claude_skill, "version two\n")
                self.write_file(repo, codex_skill, "version two\n")
                self.write_file(repo, "README.org", "updated documentation\n")
                self.run_git(
                    repo,
                    "add",
                    "ai-config-sync.json",
                    claude_skill,
                    codex_skill,
                    "README.org",
                )
                output = io.StringIO()

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {
                                "command": "git commit -m test",
                                "cwd": str(repo),
                            }
                        },
                    ),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn("section skills", output.getvalue())

    def test_present_nonregular_or_binary_candidate_manifest_blocks(self):
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        manifest = "ai-config-sync.json"

        for state in ("symlink", "binary"):
            with self.subTest(state=state):
                repo = self.make_repo(
                    ["README.org", claude_skill, codex_skill, manifest]
                )
                self.write_file(repo, claude_skill, "version two\n")
                self.write_file(repo, codex_skill, "version two\n")
                self.write_file(repo, "README.org", "updated documentation\n")
                manifest_path = repo / manifest
                manifest_path.unlink()
                external = tempfile.TemporaryDirectory()
                self.addCleanup(external.cleanup)
                external_manifest = Path(external.name) / "manifest.json"
                external_manifest.write_text("{}\n", encoding="utf-8")
                if state == "symlink":
                    manifest_path.symlink_to(external_manifest)
                else:
                    manifest_path.write_bytes(b"\xff\xfe\x00not utf-8")
                self.run_git(
                    repo,
                    "add",
                    "README.org",
                    claude_skill,
                    codex_skill,
                    manifest,
                )
                output = io.StringIO()
                original_read_bytes = Path.read_bytes

                def guarded_read_bytes(path: Path, *args, **kwargs):
                    if path.resolve(strict=False) == external_manifest.resolve():
                        raise AssertionError("external manifest content was read")
                    return original_read_bytes(path, *args, **kwargs)

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {
                                "command": "git commit -m test",
                                "cwd": str(repo),
                            }
                        },
                    ),
                    mock.patch.object(Path, "read_bytes", guarded_read_bytes),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                expected = "regular file" if state == "symlink" else "UTF-8"
                self.assertIn(expected, output.getvalue())

    def test_touched_global_binary_manifest_blocks_without_decode_error(self):
        manifest = "ai-config-sync.json"
        repo = self.make_repo([manifest])
        (repo / manifest).write_bytes(b"\xff\xfe\x00not utf-8")
        self.run_git(repo, "add", manifest)
        output = io.StringIO()

        with (
            mock.patch.object(self.module, "ROOT", repo),
            mock.patch.object(self.module, "MANIFEST_PATH", repo / manifest),
            mock.patch.object(
                self.module,
                "PRIVATE_MANIFEST_PATH",
                repo / "ai-config-sync.private.json",
            ),
            mock.patch.object(
                self.module,
                "read_input_json",
                return_value={
                    "tool_input": {
                        "command": "git commit -m test",
                        "cwd": str(repo),
                    }
                },
            ),
            mock.patch.object(self.module, "audit_problems", return_value=[]),
            redirect_stdout(output),
        ):
            self.module.guard_commit()

        self.assertTrue(output.getvalue())
        self.assertIn("UTF-8", output.getvalue())

    def test_candidate_auxiliary_symlinks_are_rejected_without_target_reads(self):
        owner = "docs/project-skills.org"
        claude_skill = ".claude/skills/example/SKILL.md"
        codex_skill = ".codex/skills/example/SKILL.md"
        claude_aux = ".claude/skills/example/evals/scenarios.md"
        codex_aux = ".codex/skills/example/evals/scenarios.md"

        for targets_match in (True, False):
            with self.subTest(targets_match=targets_match):
                repo = self.make_repo(
                    [claude_skill, codex_skill, owner, "ai-config-sync.json"]
                )
                self.write_file(
                    repo,
                    "ai-config-sync.json",
                    json.dumps(
                        {
                            "policy": {
                                "project_local_documentation_owner": owner,
                                "project_local_documentation_mode": "manual",
                            }
                        }
                    ),
                )
                self.run_git(repo, "add", "ai-config-sync.json")
                self.run_git(repo, "commit", "-m", "configure owner")
                external = tempfile.TemporaryDirectory()
                self.addCleanup(external.cleanup)
                external_root = Path(external.name)
                first_target = external_root / "first.md"
                second_target = external_root / "second.md"
                first_target.write_text("private first\n", encoding="utf-8")
                second_target.write_text("private second\n", encoding="utf-8")
                for rel, target in (
                    (claude_aux, first_target),
                    (codex_aux, first_target if targets_match else second_target),
                ):
                    path = repo / rel
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.symlink_to(target)
                self.write_file(repo, owner, "updated documentation\n")
                self.run_git(repo, "add", claude_aux, codex_aux, owner)
                output = io.StringIO()
                original_read_bytes = Path.read_bytes

                def guarded_read_bytes(path: Path, *args, **kwargs):
                    if path.resolve(strict=False) in {
                        first_target.resolve(),
                        second_target.resolve(),
                    }:
                        raise AssertionError("external auxiliary content was read")
                    return original_read_bytes(path, *args, **kwargs)

                with (
                    mock.patch.object(
                        self.module,
                        "read_input_json",
                        return_value={
                            "tool_input": {
                                "command": "git commit -m test",
                                "cwd": str(repo),
                            }
                        },
                    ),
                    mock.patch.object(Path, "read_bytes", guarded_read_bytes),
                    redirect_stdout(output),
                ):
                    self.module.guard_commit()

                self.assertTrue(output.getvalue())
                self.assertIn("Auxiliary project-local skill file is not regular", output.getvalue())

    def test_local_single_agent_skill_opt_out_still_skips_pair_and_docs_guards(self):
        skill = ".claude/skills/example/SKILL.md"
        repo = self.make_repo(["README.md", skill, "ai-config-sync.json"])
        self.write_file(
            repo,
            "ai-config-sync.json",
            json.dumps({"local": {"skills": "claude-only"}}),
        )
        self.run_git(repo, "add", "ai-config-sync.json")
        self.run_git(repo, "commit", "-m", "configure single-agent skills")
        self.write_file(repo, skill, "updated skill\n")
        self.run_git(repo, "add", skill)
        output = io.StringIO()

        with (
            mock.patch.object(
                self.module,
                "read_input_json",
                return_value={
                    "tool_input": {
                        "command": "git commit -m test",
                        "cwd": str(repo),
                    }
                },
            ),
            redirect_stdout(output),
        ):
            self.module.guard_commit()

        self.assertEqual("", output.getvalue())

    def test_project_local_pair_passes_after_frontmatter_normalization(self):
        root = self.make_project_local_pair(
            self.CLAUDE_SKILL, self.CODEX_SKILL, ("Scenario one.\n", "Scenario one.\n")
        )

        with mock.patch.object(self.module, "ROOT", root):
            problems = self.module.project_local_skill_sync_problems("publish-dotfiles")

        self.assertEqual([], problems)

    def test_project_local_pair_reports_body_and_evaluation_drift(self):
        root = self.make_project_local_pair(
            self.CLAUDE_SKILL,
            self.CODEX_SKILL.replace("Body.", "Different body."),
            ("Scenario one.\n", "Scenario two.\n"),
        )

        with mock.patch.object(self.module, "ROOT", root):
            problems = self.module.project_local_skill_sync_problems("publish-dotfiles")

        self.assertEqual(
            [
                "Project-local skill content drift after tool-specific frontmatter "
                "normalization: publish-dotfiles",
                "Auxiliary project-local skill file drift: publish-dotfiles/evals/scenarios.md",
            ],
            problems,
        )

    def test_project_local_pair_reports_a_missing_evaluation_file(self):
        root = self.make_project_local_pair(
            self.CLAUDE_SKILL, self.CODEX_SKILL, ("Scenario one.\n", "Scenario one.\n")
        )
        (root / ".codex/skills/publish-dotfiles/evals/scenarios.md").unlink()

        with mock.patch.object(self.module, "ROOT", root):
            problems = self.module.project_local_skill_sync_problems("publish-dotfiles")

        self.assertEqual(
            ["Auxiliary project-local skill file only in Claude tree: publish-dotfiles/evals/scenarios.md"],
            problems,
        )


if __name__ == "__main__":
    unittest.main()
