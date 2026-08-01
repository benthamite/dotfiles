from __future__ import annotations

import importlib.machinery
import importlib.util
import io
import json
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
) -> dict:
    return {
        "version": 1,
        "required_readmes": required or [],
        "exempt_readmes": exempt or {},
        "source_generated": source_generated or [],
        "skill_inventory": "agents/skill-inventory.org",
        "project_local_skill_documentation_owner": "agents/skill-inventory.org",
    }


class DocsAuditTests(unittest.TestCase):
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

    def test_generate_reports_not_implemented_and_exits_two(self):
        output = io.StringIO()

        with redirect_stdout(output):
            result = self.module.main(["generate"])

        self.assertEqual(2, result)
        self.assertEqual("Skill inventory generation is not implemented\n", output.getvalue())

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
            "Required subsystem README is missing: alpha/README.org\n"
            "Root directory map is missing: alpha\n",
            output.getvalue(),
        )

    def test_audit_exits_zero_when_no_problems_exist(self):
        policy = policy_for(required=["alpha", "docs"])
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
            }
        )
        output = io.StringIO()

        with redirect_stdout(output):
            result = self.module.main(["audit", "--root", str(root)])

        self.assertEqual(0, result)
        self.assertEqual("", output.getvalue())


if __name__ == "__main__":
    unittest.main()
