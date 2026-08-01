from __future__ import annotations

import importlib.machinery
import importlib.util
import io
import json
import stat
import subprocess
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest import mock


REPO_ROOT = Path(__file__).resolve().parents[1]
SCRIPT = REPO_ROOT / "bin" / "docs-audit"


def load_script(name: str, path: Path):
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


def policy_for(
    *,
    required: list[str] | None = None,
    exempt: dict[str, str] | None = None,
    source_generated: list[dict[str, str]] | None = None,
    skill_inventory: str = "agents/skill-inventory.org",
) -> dict:
    return {
        "version": 1,
        "required_readmes": required or [],
        "exempt_readmes": exempt or {},
        "source_generated": source_generated or [],
        "skill_inventory": skill_inventory,
        "project_local_skill_documentation_owner": "agents/skill-inventory.org",
    }


class DocsAuditTestCase(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.module = load_script("docs_audit_script", SCRIPT)

    def run_git(self, repo: Path, *args: str) -> None:
        subprocess.run(
            ["git", *args],
            cwd=repo,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )

    def write_file(self, repo: Path, rel: str, contents: str | bytes = "body\n") -> None:
        path = repo / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        if isinstance(contents, bytes):
            path.write_bytes(contents)
        else:
            path.write_text(contents, encoding="utf-8")

    def make_repo(self, files: list[str] | dict[str, str | bytes]) -> Path:
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        repo = Path(temp.name) / "repo"
        repo.mkdir()
        self.run_git(repo, "init")
        self.run_git(repo, "config", "user.email", "test@example.com")
        self.run_git(repo, "config", "user.name", "Test User")
        items = files.items() if isinstance(files, dict) else ((rel, "body\n") for rel in files)
        for rel, contents in items:
            self.write_file(repo, rel, contents)
        self.run_git(repo, "add", ".")
        self.run_git(repo, "commit", "-m", "initial")
        return repo

    @staticmethod
    def skill_text(name: str, description: str) -> str:
        return f"---\nname: {name}\ndescription: {description}\n---\n"


class DocsAuditTests(DocsAuditTestCase):

    def test_repo_root_is_script_repository_root(self):
        self.assertEqual(SCRIPT.resolve().parents[1], self.module.REPO_ROOT)

    def test_git_files_calls_git_with_an_argument_array_and_no_shell(self):
        completed = subprocess.CompletedProcess([], 0, stdout="b\0a\0", stderr="")

        with mock.patch.object(self.module.subprocess, "run", return_value=completed) as run:
            self.assertEqual(["a", "b"], self.module.git_files(Path("/repo")))

        args, kwargs = run.call_args
        self.assertEqual(["git", "ls-files", "-z"], args[0])
        self.assertIs(kwargs["shell"], False)

    def test_tracked_top_level_directories_ignore_untracked_directories(self):
        root = self.make_repo(["alpha/config", "README.org"])
        self.write_file(root, "runtime/cache")

        self.assertEqual({"alpha"}, self.module.tracked_top_level_directories(root))

    def test_missing_required_readme_is_reported(self):
        root = self.make_repo(["alpha/config", "README.org"])
        policy = policy_for(required=["alpha"])

        self.assertEqual(
            ["Required subsystem README is missing: alpha/README.org"],
            self.module.readme_coverage_problems(root, policy),
        )

    def test_untracked_required_readme_is_reported_as_missing(self):
        root = self.make_repo(["alpha/config", "README.org"])
        self.write_file(root, "alpha/README.org")

        self.assertEqual(
            ["Required subsystem README is missing: alpha/README.org"],
            self.module.readme_coverage_problems(root, policy_for(required=["alpha"])),
        )

    def test_new_top_level_directory_requires_policy_decision(self):
        root = self.make_repo(["alpha/README.org", "new-app/config", "README.org"])
        policy = policy_for(required=["alpha"])

        self.assertEqual(
            ["Top-level directory has no README policy decision: new-app"],
            self.module.readme_coverage_problems(root, policy),
        )

    def test_required_and_exempt_policy_overlap_is_reported(self):
        root = self.make_repo(["alpha/README.org", "README.org"])
        policy = policy_for(required=["alpha"], exempt={"alpha": "Owned elsewhere."})

        self.assertEqual(
            ["README policy directory is both required and exempt: alpha"],
            self.module.readme_coverage_problems(root, policy),
        )

    def test_stale_policy_entry_is_reported(self):
        root = self.make_repo(["alpha/README.org", "README.org"])
        policy = policy_for(required=["alpha"], exempt={"retired": "No longer used."})

        self.assertEqual(
            ["README policy decision references missing top-level directory: retired"],
            self.module.readme_coverage_problems(root, policy),
        )

    def test_readme_coverage_problems_are_sorted(self):
        root = self.make_repo(["zeta/config", "alpha/config", "README.org"])

        self.assertEqual(
            [
                "Required subsystem README is missing: zeta/README.org",
                "Top-level directory has no README policy decision: alpha",
            ],
            self.module.readme_coverage_problems(root, policy_for(required=["zeta"])),
        )

    def test_root_map_reads_first_cells_beneath_exact_heading_only(self):
        text = (
            "| =before= | Ignore |\n"
            "* Directory mapping\n"
            "| =wrong-heading= | Ignore |\n"
            "* Directory map\n"
            "| Directory | Description |\n"
            "|-----------+-------------|\n"
            "| =alpha= | A |\n"
            "| =beta= | B |\n"
            "* Next section\n"
            "| =after= | Ignore |\n"
        )

        self.assertEqual(["alpha", "beta"], self.module.root_map_directories(text))

    def test_root_map_reports_missing_and_duplicate_directories(self):
        text = "* Directory map\n| =alpha= | A |\n| =alpha= | Again |\n"

        self.assertEqual(
            [
                "Root directory map contains duplicate row: alpha",
                "Root directory map is missing: beta",
            ],
            self.module.root_map_problems(text, {"alpha", "beta"}),
        )

    def test_root_map_reports_unexpected_directory(self):
        text = "* Directory map\n| =alpha= | A |\n| =retired= | Old |\n"

        self.assertEqual(
            ["Root directory map contains unexpected row: retired"],
            self.module.root_map_problems(text, {"alpha"}),
        )

    def test_tracked_readmes_ignore_untracked_runtime_readme(self):
        root = self.make_repo(["README.org", "alpha/README.org"])
        self.write_file(root, "runtime/README.org")

        self.assertEqual(
            [root / "README.org", root / "alpha/README.org"],
            self.module.tracked_readmes(root),
        )

    def test_tracked_readme_symlink_escape_is_not_read_or_accepted(self):
        root = self.make_repo(["README.org", "alpha/config"])
        external = root.parent / "external-readme.org"
        external.write_bytes(b"\xff\xfe[[file:external-secret.org]]")
        (root / "alpha/README.org").symlink_to(external)
        self.run_git(root, "add", "alpha/README.org")
        self.run_git(root, "commit", "-m", "add escaping readme")

        self.assertEqual(
            ["Required subsystem README is missing: alpha/README.org"],
            self.module.readme_coverage_problems(root, policy_for(required=["alpha"])),
        )
        self.assertEqual(
            ["Tracked README escapes repository: alpha/README.org"],
            self.module.readme_link_problems(root),
        )

    def test_tracked_root_readme_symlink_escape_is_not_read(self):
        policy = policy_for(required=["docs"])
        root = self.make_repo(
            {
                "docs/README.org": "Docs\n",
                "docs/readme-policy.json": json.dumps(policy),
            }
        )
        external = root.parent / "external-root-readme.org"
        external.write_bytes(b"\xff\xfe[[file:external-secret.org]]")
        (root / "README.org").symlink_to(external)
        self.run_git(root, "add", "README.org")
        self.run_git(root, "commit", "-m", "add escaping root readme")

        problems = self.module.audit_problems(root)

        self.assertIn("Tracked README escapes repository: README.org", problems)
        self.assertIn("Root directory map is missing: docs", problems)
        self.assertNotIn(
            "Broken README file link: README.org -> external-secret.org",
            problems,
        )

    def test_org_file_links_extracts_file_targets(self):
        text = (
            "[[file:manual.org][Manual]]\n"
            "[[file:notes.org::*Setup][Setup]]\n"
            "file:plain.org\n"
            "<file:angle.org>\n"
            "[[https://example.com][External]]\n"
        )

        self.assertEqual(
            ["manual.org", "notes.org::*Setup", "plain.org", "angle.org"],
            self.module.org_file_links(text),
        )

    def test_org_file_links_do_not_match_substrings_or_code(self):
        text = (
            "profile:missing.org\n"
            "=file:inline-verbatim.org=\n"
            "~file:inline-code.org~\n"
            "#+begin_src text\n"
            "file:source-block.org\n"
            "#+end_src\n"
        )

        self.assertEqual([], self.module.org_file_links(text))

    def test_org_file_links_ignore_fixed_width_literal_lines(self):
        text = (
            ": file:fixed.org\n"
            "   : file:indented-fixed.org\n"
            "file:real.org\n"
        )

        self.assertEqual(["real.org"], self.module.org_file_links(text))

    def test_org_file_links_ignore_inline_source_blocks(self):
        text = (
            "src_python{file:inline.org} file:real.org\n"
            "src_python[:results raw]{file:header-args.org}\n"
        )

        self.assertEqual(["real.org"], self.module.org_file_links(text))

    def test_org_file_links_ignore_comments_but_not_keywords_or_ordinary_lines(self):
        text = (
            "# file:commented.org\n"
            "   # file:indented-comment.org\n"
            "#+title: file:keyword.org\n"
            "See file:ordinary.org\n"
        )

        self.assertEqual(
            ["keyword.org", "ordinary.org"],
            self.module.org_file_links(text),
        )

    def test_relative_org_link_strips_search_anchor(self):
        root = self.make_repo(
            {
                "README.org": "Root\n",
                "alpha/README.org": (
                    "[[file:manual.org::*Setup][Bracket]]\n"
                    "file:manual.org::*Setup\n"
                    "<file:manual.org::*Setup>\n"
                ),
                "alpha/manual.org": "* Setup\n",
            }
        )

        self.assertEqual([], self.module.readme_link_problems(root))

    def test_broken_relative_org_link_is_reported(self):
        root = self.make_repo(
            {
                "README.org": "Root\n",
                "alpha/README.org": "[[file:missing.org][Missing]]\n",
            }
        )

        self.assertEqual(
            ["Broken README file link: alpha/README.org -> alpha/missing.org"],
            self.module.readme_link_problems(root),
        )

    def test_broken_plain_relative_org_link_is_reported(self):
        root = self.make_repo(
            {
                "README.org": "Root\n",
                "alpha/README.org": "file:missing.org\n",
            }
        )

        self.assertEqual(
            ["Broken README file link: alpha/README.org -> alpha/missing.org"],
            self.module.readme_link_problems(root),
        )

    def test_broken_angle_relative_org_link_is_reported(self):
        root = self.make_repo(
            {
                "README.org": "Root\n",
                "alpha/README.org": "<file:missing.org>\n",
            }
        )

        self.assertEqual(
            ["Broken README file link: alpha/README.org -> alpha/missing.org"],
            self.module.readme_link_problems(root),
        )

    def test_duplicate_link_syntaxes_report_one_broken_link(self):
        root = self.make_repo(
            {
                "README.org": (
                    "[[file:missing.org][Bracket]]\n"
                    "file:missing.org::*Setup\n"
                    "<file:missing.org::*Setup>\n"
                )
            }
        )

        self.assertEqual(
            ["Broken README file link: README.org -> missing.org"],
            self.module.readme_link_problems(root),
        )

    def test_readme_link_audit_ignores_external_and_absolute_links(self):
        root = self.make_repo(
            {
                "README.org": (
                    "[[https://example.com][External]]\n"
                    "[[file:/missing/absolute.org][Absolute]]\n"
                    "[[file:~/missing/home.org][Home]]\n"
                    "https://example.com/plain\n"
                    "file:/missing/plain-absolute.org\n"
                    "<file:/missing/angle-absolute.org>\n"
                )
            }
        )

        self.assertEqual([], self.module.readme_link_problems(root))

    def test_readme_link_audit_ignores_windows_absolute_links(self):
        root = self.make_repo(
            {
                "README.org": (
                    "[[file:C:/missing-bracket.org][Drive]]\n"
                    "file:C:\\missing-plain.org\n"
                    "<file:\\\\server\\share\\missing-angle.org>\n"
                    "file:relative.org\n"
                ),
                "relative.org": "Relative\n",
            }
        )

        self.assertEqual([], self.module.readme_link_problems(root))

    def test_relative_link_escaping_repository_is_rejected(self):
        root = self.make_repo(
            {
                "README.org": "Root\n",
                "alpha/README.org": "[[file:../../outside.org][Outside]]\n",
            }
        )
        self.write_file(root.parent, "outside.org")

        self.assertEqual(
            ["README file link escapes repository: alpha/README.org -> ../outside.org"],
            self.module.readme_link_problems(root),
        )

    def test_readme_link_audit_does_not_read_non_readme_target_contents(self):
        root = self.make_repo(
            {
                "README.org": "[[file:binary.dat][Binary]]\n",
                "binary.dat": b"\xff\xfe\x00",
            }
        )

        self.assertEqual([], self.module.readme_link_problems(root))

    def test_unreadable_tracked_readme_does_not_stop_other_link_findings(self):
        root = self.make_repo(
            {
                "README.org": "Root\n",
                "alpha/README.org": b"\xff\xfe\x00",
                "beta/README.org": "[[file:missing.org][Missing]]\n",
            }
        )

        self.assertEqual(
            [
                "Broken README file link: beta/README.org -> beta/missing.org",
                "Tracked README could not be read: alpha/README.org",
            ],
            self.module.readme_link_problems(root),
        )

    def test_missing_declared_generated_file_is_reported(self):
        root = self.make_repo(["README.org", "source.org"])
        policy = policy_for(
            source_generated=[{"source": "source.org", "generated": "out"}]
        )

        self.assertEqual(
            ["Declared generated file is missing: out (source: source.org)"],
            self.module.source_generated_problems(root, policy),
        )

    def test_directory_at_declared_generated_file_path_is_reported(self):
        root = self.make_repo(["README.org", "source.org", "out"])
        (root / "out").unlink()
        (root / "out").mkdir()
        policy = policy_for(
            source_generated=[{"source": "source.org", "generated": "out"}]
        )

        self.assertEqual(
            ["Declared generated file is missing: out (source: source.org)"],
            self.module.source_generated_problems(root, policy),
        )

    def test_missing_declared_source_file_is_reported(self):
        root = self.make_repo(["README.org", "out"])
        policy = policy_for(
            source_generated=[{"source": "source.org", "generated": "out"}]
        )

        self.assertEqual(
            ["Declared source file is missing: source.org (generated: out)"],
            self.module.source_generated_problems(root, policy),
        )

    def test_directory_at_declared_source_file_path_is_reported(self):
        root = self.make_repo(["README.org", "source.org", "out"])
        (root / "source.org").unlink()
        (root / "source.org").mkdir()
        policy = policy_for(
            source_generated=[{"source": "source.org", "generated": "out"}]
        )

        self.assertEqual(
            ["Declared source file is missing: source.org (generated: out)"],
            self.module.source_generated_problems(root, policy),
        )

    def test_load_policy_reads_repository_policy(self):
        policy = policy_for(required=["alpha"])
        root = self.make_repo(
            {
                "README.org": "Root\n",
                "docs/readme-policy.json": json.dumps(policy),
            }
        )

        self.assertEqual(policy, self.module.load_policy(root))

    def test_policy_validation_requires_an_object(self):
        root = self.make_repo(["README.org"])

        self.assertEqual(
            ["README policy must be a JSON object"],
            self.module.policy_validation_problems(root, []),
        )

    def test_policy_validation_reports_all_schema_errors_in_sorted_order(self):
        root = self.make_repo(["README.org"])
        policy = {
            "version": 2,
            "required_readmes": "alpha",
            "extra": True,
        }

        self.assertEqual(
            [
                "README policy contains unexpected key: extra",
                "README policy is missing required key: exempt_readmes",
                "README policy is missing required key: project_local_skill_documentation_owner",
                "README policy is missing required key: skill_inventory",
                "README policy is missing required key: source_generated",
                "README policy key has invalid type: required_readmes (expected array)",
                "README policy version must be exactly 1",
            ],
            self.module.policy_validation_problems(root, policy),
        )

    def test_policy_validation_checks_nested_value_types(self):
        root = self.make_repo(["README.org"])
        policy = policy_for(required=[7])
        policy["exempt_readmes"] = {"alpha": 7}
        policy["source_generated"] = [
            {"source": 7, "generated": "out", "extra": "value"},
            "not-an-object",
        ]
        policy["skill_inventory"] = 7
        policy["project_local_skill_documentation_owner"] = 7

        self.assertEqual(
            [
                "README policy source_generated[0] contains unexpected key: extra",
                "README policy source_generated[1] must be an object",
                "README policy value has invalid type: exempt_readmes[0] (expected string reason)",
                "README policy value has invalid type: project_local_skill_documentation_owner (expected string)",
                "README policy value has invalid type: required_readmes[0] (expected string)",
                "README policy value has invalid type: skill_inventory (expected string)",
                "README policy value has invalid type: source_generated[0].source (expected string)",
            ],
            self.module.policy_validation_problems(root, policy),
        )

    def test_policy_validation_rejects_unsafe_repository_paths(self):
        root = self.make_repo(["README.org"])
        external = root.parent / "outside"
        external.mkdir()
        (root / "escape").symlink_to(external, target_is_directory=True)
        policy = policy_for(required=[""])
        policy["exempt_readmes"] = {"../retired": "Retired."}
        policy["source_generated"] = [
            {"source": "/absolute/source.org", "generated": "C:\\outside\\out"}
        ]
        policy["skill_inventory"] = "escape/inventory.org"
        policy["project_local_skill_documentation_owner"] = "."

        problems = self.module.policy_validation_problems(root, policy)

        self.assertEqual(
            [
                "README policy path is invalid: exempt_readmes[0]",
                "README policy path is invalid: project_local_skill_documentation_owner",
                "README policy path is invalid: required_readmes[0]",
                "README policy path is invalid: skill_inventory",
                "README policy path is invalid: source_generated[0].generated",
                "README policy path is invalid: source_generated[0].source",
            ],
            problems,
        )
        self.assertNotIn(str(external.resolve()), "\n".join(problems))

    def test_invalid_policy_skips_policy_checks_but_reports_independent_problems(self):
        root = self.make_repo(
            {
                "README.org": (
                    "* Directory map\n"
                    "[[file:missing.org][Missing]]\n"
                ),
                "docs/README.org": "Docs\n",
                "docs/readme-policy.json": json.dumps({"version": 1}),
            }
        )

        problems = self.module.audit_problems(root)

        self.assertIn("README policy is missing required key: required_readmes", problems)
        self.assertIn("Broken README file link: README.org -> missing.org", problems)
        self.assertIn("Root directory map is missing: docs", problems)
        self.assertFalse(
            any(problem.startswith("Required subsystem README") for problem in problems)
        )

    def test_missing_policy_still_aggregates_skill_input_and_link_problems(self):
        root = self.make_repo(
            {
                "README.org": (
                    "* Directory map\n"
                    "| =claude= | Claude |\n"
                    "| =codex= | Codex |\n"
                    "[[file:missing.org][Missing]]\n"
                ),
                "claude/skills/bad/SKILL.md": (
                    "---\nname: bad\ndescription: # absent\n---\n"
                ),
                "claude/skills/duplicate/SKILL.md": self.skill_text(
                    "duplicate", "Claude description."
                ),
                "codex/skills/duplicate/SKILL.md": self.skill_text(
                    "duplicate", "Codex description."
                ),
            }
        )

        self.assertEqual(
            [
                "Broken README file link: README.org -> missing.org",
                "Conflicting skill definitions in scope Global for name duplicate: "
                "claude/skills/duplicate/SKILL.md, "
                "codex/skills/duplicate/SKILL.md",
                "Invalid skill frontmatter description "
                "(expected a nonempty single-line string): "
                "claude/skills/bad/SKILL.md",
                "README policy is missing or untracked: docs/readme-policy.json",
            ],
            self.module.audit_problems(root),
        )

    def test_invalid_policy_still_aggregates_skill_input_and_link_problems(self):
        root = self.make_repo(
            {
                "README.org": (
                    "* Directory map\n"
                    "| =claude= | Claude |\n"
                    "| =codex= | Codex |\n"
                    "| =docs= | Docs |\n"
                    "[[file:missing.org][Missing]]\n"
                ),
                "claude/skills/bad/SKILL.md": (
                    "---\nname: bad\ndescription: # absent\n---\n"
                ),
                "claude/skills/duplicate/SKILL.md": self.skill_text(
                    "duplicate", "Claude description."
                ),
                "codex/skills/duplicate/SKILL.md": self.skill_text(
                    "duplicate", "Codex description."
                ),
                "docs/README.org": "Docs\n",
                "docs/readme-policy.json": json.dumps({"version": 1}),
            }
        )

        self.assertEqual(
            [
                "Broken README file link: README.org -> missing.org",
                "Conflicting skill definitions in scope Global for name duplicate: "
                "claude/skills/duplicate/SKILL.md, "
                "codex/skills/duplicate/SKILL.md",
                "Invalid skill frontmatter description "
                "(expected a nonempty single-line string): "
                "claude/skills/bad/SKILL.md",
                "README policy is missing required key: exempt_readmes",
                "README policy is missing required key: "
                "project_local_skill_documentation_owner",
                "README policy is missing required key: required_readmes",
                "README policy is missing required key: skill_inventory",
                "README policy is missing required key: source_generated",
            ],
            self.module.audit_problems(root),
        )

    def test_valid_policy_reports_each_skill_input_problem_once(self):
        policy = policy_for(required=["claude", "codex", "docs"])
        root = self.make_repo(
            {
                "README.org": (
                    "* Directory map\n"
                    "| =claude= | Claude |\n"
                    "| =codex= | Codex |\n"
                    "| =docs= | Docs |\n"
                ),
                "claude/README.org": "Claude\n",
                "claude/skills/bad/SKILL.md": (
                    "---\nname: bad\ndescription: # absent\n---\n"
                ),
                "claude/skills/duplicate/SKILL.md": self.skill_text(
                    "duplicate", "Claude description."
                ),
                "codex/README.org": "Codex\n",
                "codex/skills/duplicate/SKILL.md": self.skill_text(
                    "duplicate", "Codex description."
                ),
                "docs/README.org": "Docs\n",
                "docs/readme-policy.json": json.dumps(policy),
            }
        )

        self.assertEqual(
            [
                "Conflicting skill definitions in scope Global for name duplicate: "
                "claude/skills/duplicate/SKILL.md, "
                "codex/skills/duplicate/SKILL.md",
                "Invalid skill frontmatter description "
                "(expected a nonempty single-line string): "
                "claude/skills/bad/SKILL.md",
            ],
            self.module.audit_problems(root),
        )

    def test_escaping_policy_path_cannot_be_satisfied_by_external_file(self):
        external_name = "outside-source.org"
        policy = policy_for(required=["docs"])
        policy["source_generated"] = [
            {"source": f"../{external_name}", "generated": "generated.out"}
        ]
        root = self.make_repo(
            {
                "README.org": (
                    "* Directory map\n"
                    "| =docs= | Documentation |\n"
                    "[[file:missing.org][Missing]]\n"
                ),
                "docs/README.org": "Docs\n",
                "docs/readme-policy.json": json.dumps(policy),
                "generated.out": "generated\n",
            }
        )
        self.write_file(root.parent, external_name, "external\n")

        problems = self.module.audit_problems(root)

        self.assertIn(
            "README policy path is invalid: source_generated[0].source",
            problems,
        )
        self.assertIn("Broken README file link: README.org -> missing.org", problems)
        self.assertFalse(
            any(problem.startswith("Declared source file") for problem in problems)
        )

    def test_audit_combines_all_problem_categories_in_sorted_order(self):
        policy = policy_for(
            required=["alpha", "docs"],
            source_generated=[{"source": "source.org", "generated": "out"}],
        )
        root = self.make_repo(
            {
                "README.org": (
                    "* Directory map\n"
                    "| =docs= | Documentation |\n"
                    "[[file:missing.org][Missing]]\n"
                ),
                "alpha/config": "config\n",
                "docs/README.org": "Docs\n",
                "docs/readme-policy.json": json.dumps(policy),
                "source.org": "source\n",
            }
        )

        self.assertEqual(
            [
                "Broken README file link: README.org -> missing.org",
                "Declared generated file is missing: out (source: source.org)",
                "Generated skill inventory is stale: agents/skill-inventory.org",
                "Required subsystem README is missing: alpha/README.org",
                "Root directory map is missing: alpha",
            ],
            self.module.audit_problems(root),
        )

    def test_untracked_root_readme_does_not_supply_map_or_links(self):
        policy = policy_for(required=["alpha", "docs"])
        root = self.make_repo(
            {
                "alpha/README.org": "Alpha\n",
                "docs/README.org": "Docs\n",
                "docs/readme-policy.json": json.dumps(policy),
            }
        )
        self.write_file(
            root,
            "README.org",
            (
                "* Directory map\n"
                "| =alpha= | Alpha |\n"
                "| =docs= | Documentation |\n"
                "[[file:untracked-missing.org][Missing]]\n"
            ),
        )

        problems = self.module.audit_problems(root)

        self.assertIn("Root README is missing or untracked: README.org", problems)
        self.assertIn("Root directory map is missing: alpha", problems)
        self.assertIn("Root directory map is missing: docs", problems)
        self.assertNotIn(
            "Broken README file link: README.org -> untracked-missing.org",
            problems,
        )

    def test_audit_root_option_reports_all_problems_and_exits_one(self):
        policy = policy_for(required=["alpha", "docs"])
        root = self.make_repo(
            {
                "README.org": "* Directory map\n| =docs= | Documentation |\n",
                "alpha/config": "config\n",
                "docs/README.org": "Docs\n",
                "docs/readme-policy.json": json.dumps(policy),
            }
        )
        output = io.StringIO()

        with redirect_stdout(output):
            result = self.module.main(["audit", "--root", str(root)])

        self.assertEqual(1, result)
        self.assertEqual(
            "Generated skill inventory is stale: agents/skill-inventory.org\n"
            "Required subsystem README is missing: alpha/README.org\n"
            "Root directory map is missing: alpha\n",
            output.getvalue(),
        )

    def test_audit_exits_zero_when_no_problems_exist(self):
        policy = policy_for(
            required=["alpha", "docs"],
            skill_inventory="skill-inventory.org",
        )
        root = self.make_repo(
            {
                "README.org": (
                    "* Directory map\n"
                    "| =alpha= | Alpha |\n"
                    "| =docs= | Documentation |\n"
                ),
                "alpha/README.org": "Alpha\n",
                "docs/README.org": "Docs\n",
                "docs/readme-policy.json": json.dumps(policy),
                "skill-inventory.org": self.module.render_skill_inventory([]),
            }
        )
        output = io.StringIO()

        with redirect_stdout(output):
            result = self.module.main(["audit", "--root", str(root)])

        self.assertEqual(0, result)
        self.assertEqual("", output.getvalue())


class DocsAuditInventoryTests(DocsAuditTestCase):
    def test_inventory_combines_paired_skill_paths(self):
        skill = self.skill_text("example", "Use when testing.")
        root = self.make_repo(
            {
                "README.org": "Root\n",
                "claude/skills/example/SKILL.md": skill,
                "codex/skills/example/SKILL.md": skill,
            }
        )

        self.assertEqual(
            [
                self.module.SkillInventoryRow(
                    name="example",
                    description="Use when testing.",
                    scope="Global",
                    tools=("Claude", "Codex"),
                    paths=(
                        "claude/skills/example/SKILL.md",
                        "codex/skills/example/SKILL.md",
                    ),
                )
            ],
            self.module.skill_inventory_rows(root),
        )

    def test_inventory_excludes_private_and_system_skills_before_reading(self):
        root = self.make_repo(
            {
                "README.org": "Root\n",
                "claude/private-skills/x/SKILL.md": b"\xff\xfeprivate",
                "codex/skills/.system/y/SKILL.md": b"\xff\xfesystem",
            }
        )

        self.assertEqual([], self.module.skill_inventory_rows(root))

    def test_public_skill_symlink_to_private_content_is_not_read(self):
        root = self.make_repo(
            {"claude/private-skills/secret/SKILL.md": b"\xff\xfeprivate"}
        )
        public = root / "claude/skills/public/SKILL.md"
        public.parent.mkdir(parents=True)
        public.symlink_to(root / "claude/private-skills/secret/SKILL.md")
        self.run_git(root, "add", "-f", "claude/skills/public/SKILL.md")
        self.run_git(root, "commit", "-m", "add public symlink")

        with mock.patch.object(
            Path,
            "read_text",
            side_effect=AssertionError("private contents were read"),
        ):
            problems = self.module.skill_inventory_input_problems(root)

        self.assertEqual(
            [
                "Tracked skill must not be a symlink: "
                "claude/skills/public/SKILL.md"
            ],
            problems,
        )

    def test_public_skill_symlink_to_public_content_is_not_read(self):
        root = self.make_repo({"README.org": "Root\n"})
        target = root / "claude/skills/target/SKILL.md"
        target.parent.mkdir(parents=True)
        target.write_text(self.skill_text("target", "Target description."))
        public = root / "claude/skills/public/SKILL.md"
        public.parent.mkdir(parents=True)
        public.symlink_to(target)
        self.run_git(root, "add", "claude/skills/public/SKILL.md")
        self.run_git(root, "commit", "-m", "add public symlink")

        with mock.patch.object(
            Path,
            "read_text",
            side_effect=AssertionError("symlink target contents were read"),
        ):
            problems = self.module.skill_inventory_input_problems(root)

        self.assertEqual(
            ["Tracked skill must not be a symlink: claude/skills/public/SKILL.md"],
            problems,
        )

    def test_inventory_audit_reports_exact_stale_output(self):
        root = self.make_repo(
            {
                "README.org": "Root\n",
                "claude/skills/example/SKILL.md": self.skill_text(
                    "example", "Use when testing."
                ),
            }
        )
        self.write_file(root, "agents/skill-inventory.org", "stale\n")

        self.assertEqual(
            ["Generated skill inventory is stale: agents/skill-inventory.org"],
            self.module.skill_inventory_problems(root, policy_for()),
        )

    def test_inventory_audit_reports_missing_output_as_stale(self):
        root = self.make_repo(
            {
                "claude/skills/example/SKILL.md": self.skill_text(
                    "example", "Use when testing."
                )
            }
        )

        self.assertEqual(
            ["Generated skill inventory is stale: agents/skill-inventory.org"],
            self.module.skill_inventory_problems(root, policy_for()),
        )

    def test_frontmatter_parses_quoted_single_line_values(self):
        root = self.make_repo(
            {
                "claude/skills/quoted/SKILL.md": (
                    "---\n"
                    "name: 'quoted'\n"
                    'description: "Use a quoted description."\n'
                    "extra: ignored\n"
                    "---\n"
                    "description: body content is ignored\n"
                )
            }
        )

        self.assertEqual(
            ("quoted", "Use a quoted description."),
            self.module.parse_skill_frontmatter(
                (root / "claude/skills/quoted/SKILL.md").read_text(),
                "claude/skills/quoted/SKILL.md",
            ),
        )

    def test_frontmatter_rejects_missing_and_multiline_descriptions(self):
        root = self.make_repo(
            {
                "claude/skills/missing/SKILL.md": "---\nname: missing\n---\n",
                "codex/skills/multiline/SKILL.md": (
                    "---\nname: multiline\ndescription: |\n  Not one line.\n---\n"
                ),
                "codex/skills/non-string/SKILL.md": (
                    "---\nname: non-string\ndescription: false\n---\n"
                ),
            }
        )

        self.assertEqual(
            [
                "Invalid skill frontmatter description "
                "(expected a nonempty single-line string): "
                "claude/skills/missing/SKILL.md",
                "Invalid skill frontmatter description "
                "(expected a nonempty single-line string): "
                "codex/skills/multiline/SKILL.md",
                "Invalid skill frontmatter description "
                "(expected a nonempty single-line string): "
                "codex/skills/non-string/SKILL.md",
            ],
            self.module.skill_inventory_input_problems(root),
        )

    def test_frontmatter_rejects_decoded_whitespace_and_newlines(self):
        root = self.make_repo(
            {
                "claude/skills/blank/SKILL.md": (
                    '---\nname: "   "\ndescription: Valid description.\n---\n'
                ),
                "codex/skills/wrapped/SKILL.md": (
                    '---\nname: wrapped\ndescription: "line\\nwrap"\n---\n'
                ),
            }
        )

        self.assertEqual(
            [
                "Invalid skill frontmatter description "
                "(expected a nonempty single-line string): "
                "codex/skills/wrapped/SKILL.md",
                "Invalid skill frontmatter name "
                "(expected a nonempty single-line string): "
                "claude/skills/blank/SKILL.md",
            ],
            self.module.skill_inventory_input_problems(root),
        )

    def test_frontmatter_rejects_values_outside_the_supported_subset(self):
        relative = "claude/skills/example/SKILL.md"
        cases = {
            "null comment": (
                "---\nname: example\ndescription: # absent\n---\n",
                "description",
            ),
            "inline comment": (
                "---\nname: example\ndescription: Useful. # comment\n---\n",
                "description",
            ),
            "unsupported sequence": (
                "---\nname: example\ndescription: - item\n---\n",
                "description",
            ),
            "unsupported mapping": (
                "---\nname: example\ndescription: key: value\n---\n",
                "description",
            ),
            "unsupported tabbed mapping": (
                "---\nname: example\ndescription: key:\tvalue\n---\n",
                "description",
            ),
            "spaced key before valid key": (
                "---\nname : old\nname: example\n"
                "description: Useful.\n---\n",
                "name",
            ),
            "quoted key before valid key": (
                "---\n'name': old\nname: example\n"
                "description: Useful.\n---\n",
                "name",
            ),
            "duplicate key": (
                "---\nname: old\nname: example\n"
                "description: Useful.\n---\n",
                "name",
            ),
        }

        for label, (text, field) in cases.items():
            with self.subTest(label=label):
                with self.assertRaises(ValueError) as raised:
                    self.module.parse_skill_frontmatter(text, relative)
                self.assertEqual(
                    self.module._invalid_frontmatter_problem(field, relative),
                    str(raised.exception),
                )

    def test_scope_classification_uses_fixed_precedence(self):
        paths_and_scopes = {
            "archive/claude/programmatic-skills/old/SKILL.md": "Archived",
            ".claude/programmatic-skills/hidden/SKILL.md": "Programmatic",
            "codex/programmatic-skills/script/SKILL.md": "Programmatic",
            "emacs/.claude/skills/emacs-local/SKILL.md": "Project-local: emacs",
            "macos/.codex/skills/macos-local/SKILL.md": "Project-local: macos",
            ".codex/skills/dotfiles-local/SKILL.md": "Project-local: dotfiles",
            "claude/skills/global/SKILL.md": "Global",
        }
        files = {
            path: self.skill_text(Path(path).parent.name, f"Description for {path}.")
            for path in paths_and_scopes
        }
        root = self.make_repo(files)
        self.run_git(
            root,
            "add",
            "-f",
            ".claude/programmatic-skills/hidden/SKILL.md",
        )
        self.run_git(root, "commit", "-m", "track ignored programmatic skill")

        actual = {
            path: row.scope
            for row in self.module.skill_inventory_rows(root)
            for path in row.paths
        }

        self.assertEqual(paths_and_scopes, actual)

    def test_unknown_public_root_is_rejected_without_reading_contents(self):
        root = self.make_repo(
            {"experimental/.claude/skills/x/SKILL.md": b"\xff\xfeunknown"}
        )

        self.assertEqual(
            [
                "Tracked skill is outside known public roots: "
                "experimental/.claude/skills/x/SKILL.md"
            ],
            self.module.skill_inventory_input_problems(root),
        )

    def test_path_must_identify_exactly_one_tool(self):
        root = self.make_repo(
            {
                "archive/claude/.codex/skills/ambiguous/SKILL.md": self.skill_text(
                    "ambiguous", "Ambiguous tool path."
                )
            }
        )

        self.assertEqual(
            [
                "Tracked skill path does not identify exactly one tool: "
                "archive/claude/.codex/skills/ambiguous/SKILL.md"
            ],
            self.module.skill_inventory_input_problems(root),
        )

    def test_rendering_escapes_org_cells_and_sorts_deterministically(self):
        rows = [
            self.module.SkillInventoryRow(
                name="zeta",
                description=r"A backslash \\ and a | pipe.",
                scope="Global",
                tools=("Codex",),
                paths=(r"codex/skills/zeta\\part|x/SKILL.md",),
            ),
            self.module.SkillInventoryRow(
                name="Alpha",
                description="First.",
                scope="Global",
                tools=("Claude",),
                paths=("claude/skills/alpha/SKILL.md",),
            ),
            self.module.SkillInventoryRow(
                name="local",
                description="Local.",
                scope="Project-local: dotfiles",
                tools=("Claude", "Codex"),
                paths=(".claude/skills/local/SKILL.md", ".codex/skills/local/SKILL.md"),
            ),
        ]

        rendered = self.module.render_skill_inventory(rows)

        self.assertTrue(
            rendered.startswith(
                "#+title: Tracked agent skill inventory\n\n"
                "This file is generated by =bin/docs-audit generate=. "
                "Do not edit it manually.\n"
                "It covers tracked public skills in this repository. "
                "Dynamic plugins, ignored\n"
                "runtime system skills, private skills, and skills in other "
                "repositories are\n"
                "outside its scope.\n"
            )
        )
        self.assertLess(rendered.index("| =Alpha= |"), rendered.index("| =zeta= |"))
        self.assertLess(
            rendered.index("* Global"),
            rendered.index("* Project-local: dotfiles"),
        )
        self.assertIn(r"A backslash \\\\ and a \| pipe.", rendered)
        self.assertIn(r"=codex/skills/zeta\\\\part\|x/SKILL.md=", rendered)
        self.assertEqual(rendered, self.module.render_skill_inventory(list(reversed(rows))))

    def test_same_name_in_different_scopes_stays_separate(self):
        skill = self.skill_text("shared", "Same description.")
        root = self.make_repo(
            {
                "claude/skills/shared/SKILL.md": skill,
                ".claude/skills/shared/SKILL.md": skill,
            }
        )

        rows = self.module.skill_inventory_rows(root)

        self.assertEqual(2, len(rows))
        self.assertEqual(
            {"Global", "Project-local: dotfiles"},
            {row.scope for row in rows},
        )

    def test_conflicting_descriptions_in_one_scope_fail_closed(self):
        root = self.make_repo(
            {
                "claude/skills/duplicate/SKILL.md": self.skill_text(
                    "duplicate", "Claude description."
                ),
                "codex/skills/duplicate/SKILL.md": self.skill_text(
                    "duplicate", "Codex description."
                ),
            }
        )

        self.assertEqual(
            [
                "Conflicting skill definitions in scope Global for name duplicate: "
                "claude/skills/duplicate/SKILL.md, "
                "codex/skills/duplicate/SKILL.md"
            ],
            self.module.skill_inventory_input_problems(root),
        )

    def test_generation_is_atomic_stable_and_preserves_output_on_invalid_input(self):
        skill_path = "claude/skills/example/SKILL.md"
        root = self.make_repo(
            {skill_path: self.skill_text("example", "Use when testing.")}
        )
        policy = policy_for()
        destination = root / policy["skill_inventory"]
        real_replace = self.module.os.replace

        with mock.patch.object(
            self.module.os, "replace", wraps=real_replace
        ) as replace:
            self.assertEqual([], self.module.write_skill_inventory(root, policy))

        source, target = map(Path, replace.call_args.args)
        self.assertEqual(destination.parent, source.parent)
        self.assertEqual(destination, target)
        self.assertEqual(0o644, stat.S_IMODE(destination.stat().st_mode))
        first = destination.read_bytes()

        self.assertEqual([], self.module.write_skill_inventory(root, policy))
        self.assertEqual(first, destination.read_bytes())

        entries_before_failed_replace = set(destination.parent.iterdir())
        with mock.patch.object(
            self.module.os,
            "replace",
            side_effect=OSError("forced replace failure"),
        ):
            self.assertEqual(
                [
                    "Generated skill inventory could not be written: "
                    "agents/skill-inventory.org"
                ],
                self.module.write_skill_inventory(root, policy),
            )
        self.assertEqual(first, destination.read_bytes())
        self.assertEqual(
            entries_before_failed_replace,
            set(destination.parent.iterdir()),
        )

        self.write_file(
            root,
            skill_path,
            "---\nname: example\ndescription: |\n  Invalid.\n---\n",
        )
        problems = self.module.write_skill_inventory(root, policy)

        self.assertTrue(problems)
        self.assertEqual(first, destination.read_bytes())

    def test_inventory_output_symlink_is_rejected_without_reading_target(self):
        policy = policy_for(required=["claude", "docs"])
        root = self.make_repo(
            {
                "README.org": (
                    "* Directory map\n"
                    "| =claude= | Claude |\n"
                    "| =docs= | Docs |\n"
                ),
                "claude/README.org": "Claude\n",
                "claude/skills/example/SKILL.md": self.skill_text(
                    "example", "Use when testing."
                ),
                "docs/README.org": "Docs\n",
                "docs/readme-policy.json": json.dumps(policy),
            }
        )
        destination = root / policy["skill_inventory"]
        destination.parent.mkdir(parents=True)
        target = root.parent / "private-inventory-target"
        target.write_bytes(b"\xff\xfeprivate")
        destination.symlink_to(target)
        real_read_text = Path.read_text

        def guarded_read_text(path: Path, *args, **kwargs):
            if path in {destination, target}:
                raise AssertionError("inventory symlink target was read")
            return real_read_text(path, *args, **kwargs)

        with mock.patch.object(Path, "read_text", new=guarded_read_text):
            problems = self.module.audit_problems(root)

        self.assertEqual(
            [
                "Generated skill inventory path contains a symlink: "
                "agents/skill-inventory.org"
            ],
            problems,
        )
        self.assertNotIn(
            str(target.resolve()),
            problems[0],
        )

    def test_generation_does_not_replace_inventory_output_symlink(self):
        policy = policy_for()
        target_contents = b"\xff\xfeprivate target"
        root = self.make_repo(
            {
                "claude/skills/example/SKILL.md": self.skill_text(
                    "example", "Use when testing."
                ),
            }
        )
        destination = root / policy["skill_inventory"]
        destination.parent.mkdir(parents=True)
        target = root.parent / "private-inventory-target"
        target.write_bytes(target_contents)
        destination.symlink_to(target)
        real_read_text = Path.read_text

        def guarded_read_text(path: Path, *args, **kwargs):
            if path in {destination, target}:
                raise AssertionError("inventory symlink target was read")
            return real_read_text(path, *args, **kwargs)

        with mock.patch.object(Path, "read_text", new=guarded_read_text):
            self.assertEqual(
                [
                    "Generated skill inventory path contains a symlink: "
                    "agents/skill-inventory.org"
                ],
                self.module.write_skill_inventory(root, policy),
            )
        self.assertTrue(destination.is_symlink())
        self.assertEqual(target_contents, target.read_bytes())

    def test_inventory_audit_rejects_parent_symlink_without_reading_target(self):
        policy = policy_for(required=["claude", "docs"])
        target_relative = "claude/private-skills/skill-inventory.org"
        root = self.make_repo(
            {
                "README.org": (
                    "* Directory map\n"
                    "| =claude= | Claude |\n"
                    "| =docs= | Docs |\n"
                ),
                "claude/README.org": "Claude\n",
                "claude/skills/example/SKILL.md": self.skill_text(
                    "example", "Use when testing."
                ),
                target_relative: b"\xff\xfeprivate inventory",
                "docs/README.org": "Docs\n",
                "docs/readme-policy.json": json.dumps(policy),
            }
        )
        destination = root / policy["skill_inventory"]
        target = root / target_relative
        destination.parent.symlink_to(target.parent, target_is_directory=True)
        target_reads: list[Path] = []
        real_read_text = Path.read_text

        def recording_read_text(path: Path, *args, **kwargs):
            if path in {destination, target}:
                target_reads.append(path)
            return real_read_text(path, *args, **kwargs)

        with mock.patch.object(Path, "read_text", new=recording_read_text):
            problems = self.module.audit_problems(root)

        self.assertEqual(
            [
                "Generated skill inventory path contains a symlink: "
                "agents/skill-inventory.org"
            ],
            problems,
        )
        self.assertEqual([], target_reads)
        self.assertNotIn(str(target.resolve()), problems[0])

    def test_generation_rejects_parent_symlink_before_creating_temporary_file(self):
        policy = policy_for()
        target_relative = "claude/private-skills/skill-inventory.org"
        target_contents = b"private inventory\n"
        root = self.make_repo(
            {
                "claude/skills/example/SKILL.md": self.skill_text(
                    "example", "Use when testing."
                ),
                target_relative: target_contents,
            }
        )
        destination = root / policy["skill_inventory"]
        target = root / target_relative
        destination.parent.symlink_to(target.parent, target_is_directory=True)
        target_entries = set(target.parent.iterdir())
        real_named_temporary_file = self.module.tempfile.NamedTemporaryFile

        with mock.patch.object(
            self.module.tempfile,
            "NamedTemporaryFile",
            wraps=real_named_temporary_file,
        ) as temporary_file:
            problems = self.module.write_skill_inventory(root, policy)

        self.assertEqual(
            [
                "Generated skill inventory path contains a symlink: "
                "agents/skill-inventory.org"
            ],
            problems,
        )
        temporary_file.assert_not_called()
        self.assertEqual(target_contents, target.read_bytes())
        self.assertEqual(target_entries, set(target.parent.iterdir()))

    def test_audit_problems_include_stale_inventory(self):
        policy = policy_for(required=["docs"], skill_inventory="skill-inventory.org")
        root = self.make_repo(
            {
                "README.org": (
                    "* Directory map\n"
                    "| =docs= | Documentation |\n"
                ),
                "docs/README.org": "Docs\n",
                "docs/readme-policy.json": json.dumps(policy),
                "skill-inventory.org": "stale\n",
            }
        )

        self.assertEqual(
            ["Generated skill inventory is stale: skill-inventory.org"],
            self.module.audit_problems(root),
        )

    def test_cli_generate_writes_configured_inventory_and_exits_zero(self):
        policy = policy_for()
        root = self.make_repo(
            {
                "docs/readme-policy.json": json.dumps(policy),
                "claude/skills/example/SKILL.md": self.skill_text(
                    "example", "Use when testing."
                ),
            }
        )
        output = io.StringIO()

        with redirect_stdout(output):
            result = self.module.main(["generate", "--root", str(root)])

        self.assertEqual(0, result)
        self.assertEqual("", output.getvalue())
        self.assertEqual(
            self.module.render_skill_inventory(self.module.skill_inventory_rows(root)),
            (root / policy["skill_inventory"]).read_text(),
        )

    def test_cli_invalid_root_reports_one_safe_diagnostic_without_traceback(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        non_git = Path(temporary.name)
        nonexistent = non_git / "does-not-exist"

        for command in ("generate", "audit"):
            for root in (non_git, nonexistent):
                with self.subTest(command=command, root=root.name):
                    output = io.StringIO()
                    with redirect_stdout(output):
                        result = self.module.main(
                            [command, "--root", str(root)]
                        )

                    self.assertEqual(2, result)
                    self.assertEqual(
                        "Repository root is unavailable or not a Git worktree\n",
                        output.getvalue(),
                    )
                    self.assertNotIn("Traceback", output.getvalue())


if __name__ == "__main__":
    unittest.main()
