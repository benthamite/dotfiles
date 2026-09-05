from __future__ import annotations

import json
import os
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
HOOKS = (
    (DOTFILES / "claude/hooks/require-doc-update.sh", "command"),
    (DOTFILES / "codex/hooks/require-doc-update.sh", "cmd"),
)


def permission_decision(result: subprocess.CompletedProcess[str]) -> str:
    if not result.stdout.strip():
        return "allow"
    output = json.loads(result.stdout)
    return output["hookSpecificOutput"].get("permissionDecision", "allow")


class DocUpdateHookTests(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        self.repo = Path(self.temp_dir.name) / "repo"
        self.repo.mkdir()
        subprocess.run(["git", "init", "-q", str(self.repo)], check=True)
        (self.repo / "README.md").write_text("# Example\n")
        subprocess.run(
            ["git", "-C", str(self.repo), "add", "README.md"],
            check=True,
        )
        subprocess.run(
            [
                "git",
                "-C",
                str(self.repo),
                "-c",
                "user.name=Test",
                "-c",
                "user.email=test@example.com",
                "commit",
                "-qm",
                "initial",
            ],
            check=True,
        )

    def run_hook(
        self,
        hook: Path,
        command_field: str,
        command: str = "git commit -m test",
    ):
        payload = {
            "tool_name": "exec_command",
            "tool_input": {
                command_field: command,
                "workdir": str(self.repo),
            },
        }
        return subprocess.run(
            ["bash", str(hook)],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
        )

    def test_allows_test_only_elisp_commit(self):
        for directory in ("test", "tests"):
            test_file = self.repo / directory / "helpers.el"
            test_file.parent.mkdir(exist_ok=True)
            test_file.write_text("(provide 'helpers)\n")
            subprocess.run(
                ["git", "-C", str(self.repo), "add", str(test_file)],
                check=True,
            )
            for hook, command_field in HOOKS:
                with self.subTest(directory=directory, hook=hook):
                    result = self.run_hook(hook, command_field)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(permission_decision(result), "allow")
            subprocess.run(
                ["git", "-C", str(self.repo), "reset", "-q", "HEAD", "--", directory],
                check=True,
            )

    def test_still_requires_docs_for_production_elisp(self):
        source = self.repo / "example.el"
        source.write_text("(provide 'example)\n")
        subprocess.run(
            ["git", "-C", str(self.repo), "add", str(source)],
            check=True,
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "deny")

    def test_allows_quoted_test_path_in_one_shot_add_and_commit(self):
        test_file = self.repo / "test" / "helper file.el"
        test_file.parent.mkdir()
        test_file.write_text("(provide 'helper)\n")
        command = 'git add "test/helper file.el" && git commit -m test'
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "allow")

    def test_requires_docs_for_normalized_production_path(self):
        source = self.repo / "example.el"
        source.write_text("(provide 'example)\n")
        (self.repo / "test").mkdir()
        command = "git add test/../example.el && git commit -m test"
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "deny")

    def test_requires_docs_for_quoted_production_path_with_shell_punctuation(self):
        source = self.repo / "source & support.el"
        source.write_text("(provide 'support)\n")
        command = 'git add "source & support.el" && git commit -m test'
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "deny")

    def git(self, *args):
        return subprocess.check_output(["git", "-C", str(self.repo), *args], text=True)

    def prepare_selection(self):
        (self.repo / "script.sh").write_text("echo before\n")
        (self.repo / "example.el").write_text(";; Version: 1.0\n(provide 'example)\n")
        self.git("add", "script.sh", "example.el")
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com", "commit", "-qm", "fixture")
        (self.repo / "script.sh").write_text("echo after\n")
        (self.repo / "example.el").write_text(";; Version: 1.0\n(message \"changed\")\n")
        self.git("add", "example.el")

    def assert_selection(self, command, expected):
        before = (self.repo / ".git/index").read_bytes()
        for hook, field in HOOKS:
            with self.subTest(hook=hook, command=command):
                result = self.run_hook(hook, field, command)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), expected, result.stdout)
                self.assertEqual((self.repo / ".git/index").read_bytes(), before)

    def test_only_shell_ignores_unrelated_staged_elisp(self):
        self.prepare_selection()
        self.assert_selection("git commit --only -m test -- script.sh", "allow")
        self.assert_selection("git add example.el && git commit -m test script.sh", "allow")
        self.assert_selection("git commit -omtest -- script.sh", "allow")
        self.assert_selection("git commit -m test", "deny")
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com", "commit", "--only", "-qm", "actual", "--", "script.sh")
        self.assertEqual(self.git("show", "--format=", "--name-only", "HEAD").strip(), "script.sh")
        self.assertEqual(self.git("diff", "--cached", "--name-only").strip(), "example.el")

    def test_explicit_selection_uses_tool_workdir_over_session_cwd(self):
        self.prepare_selection()
        elsewhere = self.repo.parent / "session"
        elsewhere.mkdir()
        for hook, field in HOOKS:
            payload = {
                "cwd": str(elsewhere),
                "tool_input": {field: "git commit --only -m test -- script.sh", "workdir": str(self.repo)},
            }
            result = subprocess.run(["bash", str(hook)], input=json.dumps(payload),
                                    cwd=elsewhere, text=True, capture_output=True)
            self.assertEqual(permission_decision(result), "allow", result.stdout)

    def test_only_elisp_cannot_borrow_unselected_docs(self):
        self.prepare_selection()
        (self.repo / "README.md").write_text("# Updated\n")
        self.git("add", "README.md")
        self.assert_selection("git commit --only -m test -- example.el", "deny")
        self.assert_selection("git add README.md && git commit --only -m test -- example.el", "deny")
        self.assert_selection("git commit --only -m test -- example.el README.md", "allow")

    def test_directory_selection_omits_untracked_documentation(self):
        self.prepare_selection()
        (self.repo / "package/doc").mkdir(parents=True)
        source = self.repo / "package/example.el"
        source.write_text("(provide 'package)\n")
        self.git("add", "package/example.el")
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com", "commit", "--only", "-qm", "package", "--", "package/example.el")
        source.write_text("(message \"changed\")\n")
        (self.repo / "package/doc/manual.org").write_text("Untracked manual\n")
        self.assert_selection("git commit --only -m test -- package", "deny")
        # Git confirms the untracked manual is excluded from that commit.
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com", "commit", "--only", "-qm", "actual", "--", "package")
        self.assertEqual(self.git("show", "--format=", "--name-only", "HEAD").strip(), "package/example.el")

    def test_only_includes_new_files_already_known_to_real_index(self):
        self.prepare_selection()
        (self.repo / "new.sh").write_text("echo new\n")
        self.git("add", "new.sh")
        self.assert_selection("git commit --only -m test -- new.sh", "allow")

    def prepare_ignored_selection(self, filename="existing.sh"):
        self.prepare_selection()
        directory = self.repo / ".claude/skills/example"
        directory.mkdir(parents=True)
        (self.repo / ".gitignore").write_text("**/.claude/\n")
        source = directory / filename
        source.write_text("before\n")
        selected = str(source.relative_to(self.repo))
        self.git("add", "--force", ".gitignore", selected)
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com",
                 "commit", "--only", "-qm", "ignored fixture", "--", ".gitignore", selected)
        source.write_text("after\n")
        return directory

    def test_only_commits_tracked_and_force_staged_ignored_files(self):
        directory = self.prepare_ignored_selection()
        (directory / "new.sh").write_text("echo new\n")
        (directory / "untracked.el").write_text("(message \"untracked\")\n")
        self.git("add", "--force", ".claude/skills/example/new.sh")
        self.assert_selection(
            "git commit --only -m test -- .claude/skills/example/existing.sh "
            ".claude/skills/example/new.sh", "allow")
        self.assert_selection("git commit --only -m test -- .claude", "allow")
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com",
                 "commit", "--only", "-qm", "actual", "--", ".claude")
        self.assertEqual(
            self.git("show", "--format=", "--name-only", "HEAD").splitlines(),
            [".claude/skills/example/existing.sh", ".claude/skills/example/new.sh"])
        self.assertEqual(self.git("diff", "--cached", "--name-only").strip(), "example.el")
        self.assertTrue((directory / "untracked.el").exists())

    def test_ignored_directory_selection_cannot_borrow_untracked_docs(self):
        directory = self.prepare_ignored_selection("existing.el")
        (directory / "doc").mkdir()
        (directory / "doc/manual.org").write_text("Untracked manual\n")
        self.assert_selection("git commit --only -m test -- .claude", "deny")

    def test_ignored_selection_still_rejects_clean_filters_without_execution(self):
        self.prepare_ignored_selection()
        (self.repo / ".gitattributes").write_text("*.sh filter=watch-test\n")
        self.git("config", "filter.watch-test.clean", "touch filter-ran; cat")
        self.assert_selection("git commit --only -m test -- .claude", "deny")
        self.assertFalse((self.repo / "filter-ran").exists())

    def test_only_uses_worktree_diff_for_version_exemption(self):
        self.prepare_selection()
        # The real index now contains only a version bump, but the candidate
        # working tree changes behavior. The version exemption must see that.
        (self.repo / "example.el").write_text(";; Version: 1.1\n(provide 'example)\n")
        self.git("add", "example.el")
        (self.repo / "example.el").write_text(";; Version: 1.1\n(message \"changed\")\n")
        self.assert_selection("git commit --only -m test -- example.el", "deny")

    def test_only_amend_preserves_inherited_elisp_requirements(self):
        self.prepare_selection()
        self.assert_selection("git commit --amend --only -m test -- script.sh", "deny")
        self.assert_selection("git commit --amend --only --no-edit", "deny")

    def test_unsupported_dynamic_paths_fail_closed(self):
        self.prepare_selection()
        self.assert_selection('git commit --only -m test -- "$TARGET"', "deny")
        self.assert_selection("git commit --pathspec-from-file=paths -m test", "deny")

    def test_help_does_not_inspect_unrelated_staging(self):
        self.prepare_selection()
        self.assert_selection("git commit -h", "allow")

    def test_read_only_search_for_commit_text_is_not_a_commit(self):
        self.prepare_selection()
        self.assert_selection("rg --files tests | rg secret_guard_parity; git status --short tests/test_secret_guard_parity.py; rg -n 'git commit|cwd|Temporary|setUp|class ' tests/test_secret_guard_parity.py", "allow")

    def test_stdin_heredoc_message_is_not_a_commit_path(self):
        (self.repo / "script.sh").write_text("echo changed\n")
        self.git("add", "script.sh")
        command = "git commit -q -F - <<'EOF'\nA commit message\nEOF"
        self.assert_selection(command, "allow")
        for hook, field in HOOKS:
            result = self.run_hook(hook.with_name("require-elisp-test-before-commit.sh"), field, command)
            self.assertEqual(permission_decision(result), "allow", result.stdout)

    def test_all_includes_unstaged_elisp(self):
        self.prepare_selection()
        self.git("reset", "-q", "HEAD", "--", "example.el")
        self.assert_selection("git commit -am test", "deny")

    def test_all_cannot_omit_a_pending_untracked_add(self):
        self.prepare_selection()
        self.git("reset", "-q", "HEAD", "--", "example.el")
        (self.repo / "example.el").write_text(";; Version: 1.0\n(provide 'example)\n")
        (self.repo / "new.el").write_text("(message \"new\")\n")
        self.assert_selection("git add new.el && git commit -am test", "deny")

    def test_elisp_evidence_gate_ignores_unselected_pending_add(self):
        self.prepare_selection()
        for hook, field in HOOKS:
            result = self.run_hook(
                hook.with_name("require-elisp-test-before-commit.sh"), field,
                "git add example.el && git commit --only -m test -- script.sh",
            )
            self.assertEqual(permission_decision(result), "allow", result.stdout)

    def test_readme_gate_cannot_borrow_unselected_pending_readme(self):
        self.prepare_selection()
        (self.repo / "claude/hooks").mkdir(parents=True)
        (self.repo / "claude/hooks/example.sh").write_text("echo hook\n")
        (self.repo / "claude/README.org").write_text("Overview\n")
        self.git("add", "claude/hooks/example.sh", "claude/README.org")
        for hook, field in HOOKS:
            result = self.run_hook(
                hook.with_name("require-readme-update.sh"), field,
                "git add claude/README.org && git commit --only -m test -- claude/hooks/example.sh",
            )
            self.assertEqual(permission_decision(result), "deny", result.stdout)
            result = self.run_hook(
                hook.with_name("require-readme-update.sh"), field,
                "git add claude/hooks/example.sh && git commit --only -m test -- script.sh",
            )
            self.assertEqual(permission_decision(result), "allow", result.stdout)

    def test_helper_failure_and_invalid_data_deny(self):
        library = self.repo / "lib-staged-files.sh"
        library.write_text((HOOKS[0][0].parent / library.name).read_text())
        helper = self.repo / "commit-file-selection.py"
        for body in ("raise SystemExit(1)", "print('null')", "print('{\"mode\": \"selection\"}')"):
            helper.write_text(body + "\n")
            result = subprocess.run(
                ["bash", "-c", 'source "$TEST_LIBRARY"; echo wrongly-allowed'],
                env={**os.environ, "TEST_LIBRARY": str(library), "COMMAND": "git commit -m test"},
                cwd=self.repo, text=True, capture_output=True,
            )
            self.assertEqual(permission_decision(result), "deny", result.stdout)

    def test_candidate_does_not_execute_clean_filters(self):
        self.prepare_selection()
        (self.repo / ".gitattributes").write_text("*.el filter=watch-test\n")
        self.git("config", "filter.watch-test.clean", "touch filter-ran; cat")
        self.assert_selection("git commit --only -m test -- example.el", "deny")
        self.assertFalse((self.repo / "filter-ran").exists())

    def test_candidate_checks_attributes_in_its_own_index(self):
        self.prepare_selection()
        attributes = self.repo / ".gitattributes"
        attributes.write_text("*.el filter=watch-test\n")
        self.git("add", ".gitattributes")
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com", "commit", "--only", "-qm", "attributes", "--", ".gitattributes")
        attributes.unlink()
        self.git("add", ".gitattributes")
        self.git("config", "filter.watch-test.clean", "touch filter-ran; cat")
        # The real index sees no attributes; the candidate restores HEAD and
        # must reject its filter before staging selected worktree content.
        self.assert_selection("git commit --only -m test -- example.el", "deny")
        self.assertFalse((self.repo / "filter-ran").exists())

    def prepare_generated_manual(self, headers, outputs, manual="README.org"):
        source = self.repo / manual
        source.parent.mkdir(parents=True, exist_ok=True)
        source.write_text(headers + "\nBody.\n")
        for output in outputs:
            target = self.repo / output
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text("generated before\n")
        self.git("add", "--", manual, *outputs)
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com",
                 "commit", "-qm", "manual fixture")
        source.write_text(headers + "\nChanged body.\n")
        self.git("add", "--", manual)

    def assert_generated_refusal(self, path, command="git commit -m test"):
        before = (self.repo / ".git/index").read_bytes()
        for hook, field in HOOKS:
            with self.subTest(hook=hook, path=path):
                result = self.run_hook(hook, field, command)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(permission_decision(result), "deny", result.stdout)
                self.assertIn(path, deny_reason(result))
                self.assertEqual(json.loads(result.stdout)["hookSpecificOutput"]["hookEventName"],
                                 "PreToolUse")
                self.assertEqual((self.repo / ".git/index").read_bytes(), before)

    def test_texinfo_only_header_keeps_source_texi_basename(self):
        self.prepare_generated_manual("#+TEXINFO_FILENAME: package.info",
                                      ["README.texi", "package.info"])
        (self.repo / "README.texi").write_text("changed texi\n")
        self.assert_generated_refusal("README.texi")
        self.assert_selection("git add README.texi && git commit -m test", "allow")

    def test_unstaged_manual_header_edit_cannot_hide_index_outputs(self):
        self.prepare_generated_manual("#+EXPORT_FILE_NAME: package.info",
                                      ["package.texi", "package.info"])
        (self.repo / "package.texi").write_text("changed texi\n")
        (self.repo / "README.org").write_text("#+TITLE: Unstaged different header\n")
        self.assert_generated_refusal("package.texi")
        self.assert_selection("git add package.texi && git commit -m test", "allow")

    def test_unstaged_manual_removal_cannot_hide_index_outputs(self):
        self.prepare_generated_manual("#+EXPORT_FILE_NAME: package.info", ["package.texi"])
        (self.repo / "README.org").unlink()
        (self.repo / "package.texi").write_text("changed texi\n")
        self.assert_generated_refusal("package.texi")

    def test_unselected_working_manual_header_does_not_require_its_outputs(self):
        self.prepare_generated_manual("#+TITLE: Plain manual", ["foreign.texi"])
        (self.repo / "README.org").write_text("#+EXPORT_FILE_NAME: foreign.info\n")
        (self.repo / "foreign.texi").write_text("foreign output\n")
        self.assert_selection("git commit -m test", "allow")

    def test_only_commit_reads_selected_working_manual_header(self):
        self.prepare_generated_manual("#+EXPORT_FILE_NAME: old.info", ["old.texi", "new.texi"])
        (self.repo / "README.org").write_text("#+EXPORT_FILE_NAME: new.info\n")
        (self.repo / "new.texi").write_text("changed new output\n")
        self.assert_generated_refusal("new.texi", "git commit --only -m test -- README.org")
        self.assert_selection("git commit --only -m test -- README.org new.texi", "allow")

    def test_amend_only_reads_inherited_manual_from_candidate(self):
        self.prepare_generated_manual("#+EXPORT_FILE_NAME: package.info", ["package.texi"])
        (self.repo / "script.sh").write_text("before\n")
        self.git("add", "script.sh")
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com",
                 "commit", "-qm", "manual and script")
        (self.repo / "README.org").write_text("#+TITLE: Unselected header\n")
        (self.repo / "script.sh").write_text("after\n")
        (self.repo / "package.texi").write_text("changed output\n")
        self.assert_generated_refusal("package.texi",
                                      "git commit --amend --only -m test -- script.sh")

    def test_literal_pending_add_reads_working_manual_header(self):
        self.prepare_generated_manual("#+EXPORT_FILE_NAME: old.info", ["old.texi", "new.texi"])
        (self.repo / "README.org").write_text("#+EXPORT_FILE_NAME: new.info\n")
        (self.repo / "new.texi").write_text("changed output\n")
        self.assert_generated_refusal("new.texi", "git add README.org && git commit -m test")
        self.assert_selection("git add README.org new.texi && git commit -m test", "allow")

    def test_uncertain_pending_add_header_requires_separate_staging(self):
        self.prepare_generated_manual("#+EXPORT_FILE_NAME: old.info",
                                      ["doc/old.texi", "doc/new.texi"], manual="doc/manual.org")
        (self.repo / "doc/manual.org").write_text("#+EXPORT_FILE_NAME: new.info\n")
        (self.repo / "doc/new.texi").write_text("changed output\n")
        for command in ("git add doc && git commit -m test",
                        "git add -A && git commit -m test",
                        'git add "$TARGET" && git commit -m test'):
            self.assert_generated_refusal("stage pending manual changes separately", command)

    def test_pending_unrelated_directory_does_not_consume_manual_drift(self):
        self.prepare_generated_manual("#+EXPORT_FILE_NAME: old.info", ["old.texi", "foreign.texi"])
        (self.repo / "README.org").write_text("#+EXPORT_FILE_NAME: foreign.info\n")
        (self.repo / "foreign.texi").write_text("unselected output\n")
        (self.repo / "scripts").mkdir()
        (self.repo / "scripts/check.sh").write_text("echo checked\n")
        self.assert_selection("git add scripts && git commit -m test", "allow")

    def test_uncertain_add_without_generated_manual_remains_allowed(self):
        self.prepare_generated_manual("#+TITLE: Plain manual", [])
        (self.repo / "README.org").write_text("#+TITLE: Changed plain manual\n")
        self.assert_selection("git add -A && git commit -m test", "allow")

    def test_large_candidate_manual_is_not_passed_through_argv(self):
        self.prepare_generated_manual("#+EXPORT_FILE_NAME: package.info", ["package.texi"])
        manual = self.repo / "README.org"
        manual.write_text("#+EXPORT_FILE_NAME: package.info\n" + "ordinary body\n" * 25000)
        (self.repo / "package.texi").write_text("changed output\n")
        self.assert_generated_refusal("package.texi", "git commit --only -m test -- README.org")

    def test_distinct_headers_bind_info_independently(self):
        self.prepare_generated_manual(
            "#+EXPORT_FILE_NAME: guide.info\n#+TEXINFO_FILENAME: package.info",
            ["guide.texi", "package.info"])
        (self.repo / "package.info").write_text("changed info\n")
        self.assert_generated_refusal("package.info")
        self.assert_selection("git add package.info && git commit -m test", "allow")

    def test_deleted_dash_prefixed_generated_output_is_still_checked(self):
        self.prepare_generated_manual(
            "#+EXPORT_FILE_NAME: -guide.info", ["-guide.texi"])
        (self.repo / "-guide.texi").unlink()
        self.assert_generated_refusal("-guide.texi")

    def test_reversed_mixed_case_headers_bind_texi_independently(self):
        self.prepare_generated_manual(
            "  #+TeXiNfO_FiLeNaMe: package.info\n#+ExPoRt_FiLe_NaMe: guide.texi",
            ["guide.texi", "package.info"])
        (self.repo / "guide.texi").write_text("changed texi\n")
        self.assert_generated_refusal("guide.texi")

    def test_quoted_info_and_shell_quoted_spaced_paths(self):
        self.prepare_generated_manual(
            '#+EXPORT_FILE_NAME: user guide.info\n#+TEXINFO_FILENAME: "package manual.info"',
            ["doc/user guide.texi", "doc/package manual.info"],
            manual="doc/source manual.org")
        self.git("reset", "-q", "HEAD", "--", "doc/source manual.org")
        (self.repo / "doc/package manual.info").write_text("changed info\n")
        command = 'git add "doc/source manual.org" && git commit -m test'
        self.assert_generated_refusal("doc/package manual.info", command)
        self.assert_selection(
            'git add "doc/source manual.org" "doc/package manual.info" && git commit -m test',
            "allow")

    def test_contained_relative_and_absolute_outputs_are_preserved(self):
        self.prepare_generated_manual(
            f'#+EXPORT_FILE_NAME: ../generated/guide.info\n#+TEXINFO_FILENAME: "{self.repo}/generated/package.info"',
            ["generated/guide.texi", "generated/package.info"],
            manual="doc/manual.org")
        (self.repo / "generated/package.info").write_text("changed info\n")
        self.assert_generated_refusal("generated/package.info")
        (self.repo / "generated/guide.texi").write_text("changed texi\n")
        self.git("add", "generated/package.info")
        self.assert_generated_refusal("generated/guide.texi")
        self.assert_selection(
            "git add generated/guide.texi && git commit -m test", "allow")

    def test_literal_block_headers_are_not_output_declarations(self):
        for kind in ("src org", "example", "comment", "export texinfo"):
            with self.subTest(kind=kind):
                headers = (f"#+begin_{kind}\n#+TEXINFO_FILENAME: ignored.info\n"
                           f"#+end_{kind.split()[0]}\n#+TEXINFO_FILENAME: package.info")
                self.prepare_generated_manual(headers, ["README.texi", "package.info"])
                (self.repo / "README.texi").write_text("changed texi\n")
                self.assert_generated_refusal("README.texi")
                self.git("add", "README.texi")

    def test_nonliteral_block_output_declarations_remain_active(self):
        self.prepare_generated_manual(
            "#+begin_quote\n#+EXPORT_FILE_NAME: guide.info\n#+end_quote\n"
            "#+begin_special\n#+TEXINFO_FILENAME: package.info\n#+end_special",
            ["guide.texi", "package.info"])
        (self.repo / "package.info").write_text("changed info\n")
        self.assert_generated_refusal("package.info")

    def test_ambiguous_and_escaping_output_names_fail_closed(self):
        cases = (
            "#+TEXINFO_FILENAME: one.info\n#+TEXINFO_FILENAME: two.info",
            "#+EXPORT_FILE_NAME: one.info\n#+EXPORT_FILE_NAME: two.info",
            "#+TEXINFO_FILENAME: ../outside.info",
            f"#+TEXINFO_FILENAME: {self.repo.parent}/outside.info",
            '#+EXPORT_FILE_NAME: "guide.info"\n#+TEXINFO_FILENAME: package.info',
        )
        for headers in cases:
            with self.subTest(headers=headers):
                (self.repo / "README.org").write_text(headers + "\nBody.\n")
                self.git("add", "README.org")
                self.assert_generated_refusal("output names")

    def test_symlink_output_escape_fails_closed(self):
        outside = self.repo.parent / "outside"
        outside.mkdir()
        (self.repo / "generated").symlink_to(outside, target_is_directory=True)
        (self.repo / "README.org").write_text("#+TEXINFO_FILENAME: generated/package.info\n")
        self.git("add", "README.org")
        self.assert_generated_refusal("output names")

    def test_non_texinfo_manual_does_not_require_generated_outputs(self):
        self.prepare_generated_manual("#+TITLE: Guide", ["README.texi"])
        (self.repo / "README.texi").write_text("unrelated output\n")
        self.assert_selection("git commit -m test", "allow")

    def test_export_filename_replaces_its_final_extension(self):
        self.prepare_generated_manual("#+EXPORT_FILE_NAME: guide.manual", ["guide.texi"])
        (self.repo / "guide.texi").write_text("changed texi\n")
        self.assert_generated_refusal("guide.texi")

    def test_org_manual_diagnostic_names_the_available_skill(self):
        (self.repo / "README.org").write_text("#+TITLE: Guide\n")
        (self.repo / "example.el").write_text("(provide 'example)\n")
        self.git("add", "example.el")
        for hook, field in HOOKS:
            result = self.run_hook(hook, field)
            self.assertEqual(permission_decision(result), "deny", result.stdout)
            self.assertIn("document-elisp-package", deny_reason(result))
            self.assertNotIn("/doc-elisp", deny_reason(result))

    @unittest.skipUnless(shutil.which("emacs") and shutil.which("makeinfo"),
                         "native Org and makeinfo are required")
    def test_native_org_relative_info_and_default_output_paths(self):
        expression = r"""(progn
          (setq enable-local-variables nil enable-local-eval nil
                enable-dir-local-variables nil)
          (require 'ox-texinfo)
          (setq org-export-use-babel nil org-export-before-processing-hook nil
                org-export-before-parsing-hook nil)
          (with-temp-buffer
            (setq buffer-file-name (getenv "TEST_MANUAL_SOURCE")
                  default-directory (file-name-directory buffer-file-name))
            (insert-file-contents buffer-file-name)
            (org-mode)
            (org-export-to-file 'texinfo
              (org-export-output-file-name ".texi"))))"""
        for suffix, info_header, expected in (
            ("relative", "#+TEXINFO_FILENAME: package.info\n", "doc/package.info"),
            ("default", "", "generated/guide.info"),
        ):
            with self.subTest(case=suffix):
                self.prepare_generated_manual(
                    "#+TITLE: Fixture\n#+EXPORT_FILE_NAME: ../generated/guide.info\n" + info_header,
                    ["generated/guide.texi"], manual="doc/manual.org")
                result = subprocess.run(
                    ["emacs", "--batch", "-Q", "--eval", expression],
                    env={**os.environ, "TEST_MANUAL_SOURCE": str(self.repo / "doc/manual.org")},
                    cwd=self.repo / "doc", text=True, capture_output=True)
                self.assertEqual(result.returncode, 0, result.stderr)
                result = subprocess.run(
                    ["makeinfo", "--no-split", "../generated/guide.texi"],
                    cwd=self.repo / "doc", text=True, capture_output=True)
                self.assertEqual(result.returncode, 0, result.stderr)
                actual = sorted(str(path.relative_to(self.repo))
                                for path in self.repo.rglob("*.info"))
                self.assertIn(expected, actual)
                self.assert_generated_refusal(expected)
                # The next fixture checks a distinct export without stale Info files.
                for path in self.repo.rglob("*.info"):
                    path.unlink()


def deny_reason(result: subprocess.CompletedProcess[str]) -> str:
    if not result.stdout.strip():
        return ""
    output = json.loads(result.stdout)
    return output["hookSpecificOutput"].get("permissionDecisionReason", "")


class SkillHelperDocUpdateHookTests(unittest.TestCase):
    """Keep skill-helper documentation independent of package manuals."""

    git = DocUpdateHookTests.git
    assert_selection = DocUpdateHookTests.assert_selection

    def run_hook(self, hook, command_field, command="git commit -m test"):
        payload = {
            "tool_name": "exec_command",
            "tool_input": {command_field: command, "workdir": str(self.repo)},
        }
        # Execute the actual shebang, including macOS /bin/bash 3.2; PATH may
        # otherwise choose Homebrew Bash and hide native array/nounset failures.
        return subprocess.run([str(hook)], input=json.dumps(payload), text=True,
                              capture_output=True, check=False)

    def setUp(self):
        DocUpdateHookTests.setUp(self)
        self.manual = "emacs/extras/doc/example.org"
        path = self.repo / self.manual
        path.parent.mkdir(parents=True)
        path.write_text("#+title: Example package\n")
        self.git("add", self.manual)
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com",
                 "commit", "-qm", "package manual fixture")

    def prepare_skill(self, root="claude/skills", name="example"):
        owner = f"{root}/{name}"
        paths = {
            "helper": f"{owner}/scripts/check.el",
            "skill": f"{owner}/SKILL.md",
            "reference": f"{owner}/references/diagnostics.md",
        }
        for label, relative in paths.items():
            path = self.repo / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("(kill-emacs 0)\n" if label == "helper" else "Before.\n")
        self.git("add", "--force", "--", *paths.values())
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com",
                 "commit", "-qm", "skill fixture")
        (self.repo / paths["helper"]).write_text("(kill-emacs 1)\n")
        return paths

    def change_document(self, relative):
        (self.repo / relative).write_text("Updated documented behavior.\n")

    def test_all_four_explicit_skill_roots_accept_selected_own_skill(self):
        for root in ("claude/skills", "codex/skills", ".claude/skills", ".codex/skills"):
            with self.subTest(root=root):
                paths = self.prepare_skill(root)
                self.change_document(paths["skill"])
                self.git("add", "--force", "--", paths["helper"], paths["skill"])
                self.assert_selection("git commit -m test", "allow")

    def test_selected_reference_satisfies_only_its_owner(self):
        paths = self.prepare_skill()
        other = self.prepare_skill("codex/skills")
        self.change_document(paths["reference"])
        self.git("add", "--", paths["helper"], paths["reference"])
        self.assert_selection("git commit -m test", "allow")
        self.git("add", "--", other["helper"])
        self.assert_selection("git commit -m test", "deny")

    def test_helper_requires_changed_selected_own_documentation(self):
        paths = self.prepare_skill()
        self.git("add", "--", paths["helper"])
        self.assert_selection("git commit -m test", "deny")
        self.change_document(paths["skill"])
        self.assert_selection("git commit -m test", "deny")
        unrelated = self.repo / "codex/skills/other/SKILL.md"
        unrelated.parent.mkdir(parents=True)
        unrelated.write_text("Unrelated skill.\n")
        self.git("add", "--", str(unrelated))
        self.assert_selection("git commit -m test", "deny")
        self.git("add", "--", paths["skill"])
        self.assert_selection("git commit -m test", "allow")

    def test_only_helper_cannot_borrow_unselected_skill_documentation(self):
        paths = self.prepare_skill()
        self.change_document(paths["skill"])
        self.git("add", "--", paths["helper"], paths["skill"])
        self.assert_selection(f"git commit --only -m test -- {paths['helper']}", "deny")
        self.assert_selection(
            f"git commit --only -m test -- {paths['helper']} {paths['skill']}", "allow")

    def test_deleted_documentation_does_not_satisfy_helper(self):
        paths = self.prepare_skill()
        (self.repo / paths["skill"]).unlink()
        self.git("add", "--", paths["helper"], paths["skill"])
        self.assert_selection("git commit -m test", "deny")
        self.assert_selection(
            f"git commit --only -m test -- {paths['helper']} {paths['skill']}", "deny")

    def test_document_presence_uses_candidate_not_unstaged_worktree(self):
        paths = self.prepare_skill()
        self.change_document(paths["skill"])
        self.git("add", "--", paths["helper"], paths["skill"])
        (self.repo / paths["skill"]).unlink()
        self.assert_selection("git commit -m test", "allow")
        self.assert_selection(
            f"git commit --only -m test -- {paths['helper']} {paths['skill']}", "deny")

    def test_skill_documentation_does_not_satisfy_mixed_package_change(self):
        paths = self.prepare_skill()
        self.change_document(paths["skill"])
        package = self.repo / "emacs/extras/example.el"
        package.write_text("(provide 'example)\n")
        self.git("add", "--", paths["helper"], paths["skill"], str(package))
        self.assert_selection("git commit -m test", "deny")
        self.change_document(self.manual)
        self.git("add", "--", self.manual)
        self.assert_selection("git commit -m test", "allow")

    def test_package_manual_does_not_satisfy_undocumented_skill_helper(self):
        paths = self.prepare_skill()
        self.change_document(self.manual)
        self.git("add", "--", paths["helper"], self.manual)
        self.assert_selection("git commit -m test", "deny")

    def test_non_script_and_nonstandard_root_paths_still_require_package_docs(self):
        paths = self.prepare_skill()
        self.change_document(paths["skill"])
        candidates = (
            "scripts/check.el", "claude/skills/example/assets/example.el",
            "claude/skills/example/example.el", "claude/skills/example/nested/scripts/check.el",
            "other/claude/skills/example/scripts/check.el", "claude/skills/example/scripts/../outside.el",
        )
        for relative in candidates:
            with self.subTest(path=relative):
                self.git("reset", "-q", "HEAD", "--")
                path = self.repo / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("(provide 'example)\n")
                self.git("add", "--", str(path), paths["skill"])
                self.assert_selection("git commit -m test", "deny")

    def test_helper_documentation_is_required_without_a_package_manual(self):
        self.git("rm", "--", "README.md", self.manual)
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com",
                 "commit", "-qm", "remove package manuals")
        paths = self.prepare_skill()
        self.git("add", "--", paths["helper"])
        self.assert_selection("git commit -m test", "deny")
        self.change_document(paths["skill"])
        self.git("add", "--", paths["skill"])
        self.assert_selection("git commit -m test", "allow")

    def test_combined_staging_of_skill_helpers_requires_separate_staging(self):
        paths = self.prepare_skill()
        self.change_document(paths["skill"])
        for command in (
            f"git add {paths['helper']} {paths['skill']} && git commit -m test",
            "git add claude/skills/example && git commit -m test",
            "git add -A && git commit -m test",
        ):
            with self.subTest(command=command):
                for hook, field in HOOKS:
                    result = self.run_hook(hook, field, command)
                    self.assertEqual(permission_decision(result), "deny", result.stdout)
                    self.assertIn("separately", deny_reason(result))
        self.git("add", "--", paths["helper"], paths["skill"])
        self.assert_selection("git commit -m test", "allow")

    def test_resolved_all_amend_and_combined_only_candidates_are_supported(self):
        paths = self.prepare_skill()
        self.assert_selection("git commit --all -m test", "deny")
        self.change_document(paths["skill"])
        self.assert_selection("git commit --all -m test", "allow")
        self.assert_selection(
            f"git add {paths['skill']} && git commit --only -m test -- "
            f"{paths['helper']} {paths['skill']}", "allow")
        # The amend's inherited addition already contains this owner's docs.
        self.assert_selection(
            f"git commit --amend --only -m test -- {paths['helper']}", "allow")

    def test_amend_only_cannot_borrow_unselected_skill_documentation(self):
        paths = self.prepare_skill()
        self.git("add", "--", paths["helper"])
        self.git("-c", "user.name=Test", "-c", "user.email=test@example.com",
                 "commit", "-qm", "helper-only amend fixture")
        (self.repo / paths["helper"]).write_text("(kill-emacs 2)\n")
        self.change_document(paths["skill"])
        self.git("add", "--", paths["skill"])
        self.assert_selection(
            f"git commit --amend --only -m test -- {paths['helper']}", "deny")
        self.assert_selection(
            f"git commit --amend --only -m test -- {paths['helper']} {paths['skill']}", "allow")

    def test_copied_index_preserves_racy_source_detection_for_all_and_include(self):
        paths = self.prepare_skill()
        source = self.repo / paths["helper"]
        index = self.repo / ".git/index"
        # Model a same-size edit within the index timestamp's precision. The
        # real index correctly treats this cached stat entry as racy; copying
        # it with a newer timestamp must not turn it into a false clean entry.
        self.git("config", "core.trustctime", "false")
        self.git("config", "core.checkStat", "minimal")
        timestamp = 1_700_000_000_000_000_000
        source.write_text("(kill-emacs 0)\n")
        os.utime(source, ns=(timestamp, timestamp))
        self.git("add", "--", paths["helper"])
        source.write_text("(kill-emacs 1)\n")
        os.utime(source, ns=(timestamp, timestamp))
        os.utime(index, ns=(timestamp, timestamp))
        original_index = index.read_bytes()
        self.assertIn(paths["helper"], self.git("--no-optional-locks", "diff", "--name-only"))
        for command in ("git commit --all -m test",
                        f"git commit --include -m test -- {paths['helper']}"):
            for hook, field in HOOKS:
                with self.subTest(command=command, hook=hook):
                    result = subprocess.run(
                        ["python3", "-B", str(hook.with_name("commit-file-selection.py"))],
                        input=command, env={**os.environ, "COMMIT_FILE_CWD": str(self.repo)},
                        text=True, capture_output=True, check=False,
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)
                    candidate = json.loads(result.stdout)
                    self.assertEqual(candidate.get("mode"), "selection", candidate)
                    self.assertIn(paths["helper"], candidate["staged"].splitlines())
                    self.assertIn("M\t" + paths["helper"], candidate["status"].splitlines())
                    self.assertEqual(index.read_bytes(), original_index)
                    self.assertEqual(index.stat().st_mtime_ns, timestamp)
                    guarded = self.run_hook(hook, field, command)
                    self.assertEqual(guarded.returncode, 0, guarded.stderr)
                    self.assertEqual(permission_decision(guarded), "deny", guarded.stdout)


class DocUpdateHookRepoPathTests(unittest.TestCase):
    """Cover a repository whose path contains a space, as ~/My Drive/dotfiles does.

    The other fixtures build their repository directly under `tempfile`, whose
    paths never contain a space, so none of this was exercised before.
    """

    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        # A second repository standing in for the session's own working
        # directory: it has a root README.org and no doc/ directory, so a hook
        # that resolves it instead of the commit's repository refuses.
        self.elsewhere = Path(self.temp_dir.name) / "elsewhere"
        self.elsewhere.mkdir()
        subprocess.run(["git", "init", "-q", str(self.elsewhere)], check=True)
        (self.elsewhere / "README.org").write_text("#+title: Elsewhere\n")
        subprocess.run(
            ["git", "-C", str(self.elsewhere), "add", "README.org"], check=True
        )
        subprocess.run(
            [
                "git", "-C", str(self.elsewhere),
                "-c", "user.name=Test",
                "-c", "user.email=test@example.com",
                "commit", "-qm", "initial",
            ],
            check=True,
        )
        self.repo = Path(self.temp_dir.name) / "My Drive" / "repo"
        self.doc = self.repo / "emacs" / "extras" / "doc"
        self.doc.mkdir(parents=True)
        subprocess.run(["git", "init", "-q", str(self.repo)], check=True)
        # Both a root README.org and a nested doc/ directory, as dotfiles has, so
        # that the two refusal messages can be told apart.
        (self.repo / "README.org").write_text("#+title: Example\n")
        (self.doc / "manual.org").write_text("#+title: Manual\n")
        subprocess.run(["git", "-C", str(self.repo), "add", "-A"], check=True)
        subprocess.run(
            [
                "git", "-C", str(self.repo),
                "-c", "user.name=Test",
                "-c", "user.email=test@example.com",
                "commit", "-qm", "initial",
            ],
            check=True,
        )

    def run_hook(
        self, hook, command_field, command, cwd=None, workdir=None, home=None
    ):
        payload = {
            "tool_name": "exec_command",
            "tool_input": {
                command_field: command,
                "workdir": str(workdir or self.repo),
            },
        }
        env = None
        if home is not None:
            env = {**os.environ, "HOME": str(home)}
        return subprocess.run(
            ["bash", str(hook)],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
            cwd=str(cwd) if cwd else None,
            env=env,
        )

    def test_nested_doc_directory_is_found_when_the_path_has_a_space(self):
        (self.repo / "example.el").write_text("(provide 'example)\n")
        subprocess.run(
            ["git", "-C", str(self.repo), "add", "example.el"], check=True
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, "git commit -m test")
                self.assertEqual(permission_decision(result), "deny")
                self.assertIn("doc/*.org", deny_reason(result))
                self.assertNotIn("README.org is not included", deny_reason(result))

    def test_one_shot_add_and_commit_accepts_a_nested_doc_org(self):
        (self.repo / "example.el").write_text("(provide 'example)\n")
        (self.doc / "manual.org").write_text("#+title: Manual\nUpdated.\n")
        command = "git add example.el emacs/extras/doc/manual.org && git commit -m test"
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_cd_target_is_honoured_when_the_message_uses_command_substitution(self):
        """A commit message built with $(...) must not lose the `cd` target.

        `cd <repo> && git commit -m "$(cat <<EOF ...)"` is the ordinary way to
        write a multi-line message, and the session's own working directory is
        frequently a different repository. Resolving the wrong repository does
        not merely misreport: it evaluates the guard against a tree that has
        neither the staged Elisp nor the staged manual, so it can refuse a
        correct commit and, when the other repository happens to hold a staged
        doc file, wave through one that should be refused.
        """
        (self.repo / "example.el").write_text("(provide 'example)\n")
        (self.doc / "manual.org").write_text("#+title: Manual\nSubstitution.\n")
        command = (
            f'cd "{self.repo}" && git add example.el emacs/extras/doc/manual.org '
            '&& git commit -q -m "$(printf %s subject)"'
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(
                    hook, command_field, command, cwd=self.elsewhere,
                    workdir=self.elsewhere,
                )
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_cd_target_written_through_home_is_resolved(self):
        """`cd "$HOME/..."` is how these paths are normally written.

        The target is extracted by parameter expansion alone, which never
        expands `$HOME`, so the directory test failed on the literal string and
        the session's repository was silently used instead.
        """
        (self.repo / "example.el").write_text("(provide 'example)\n")
        (self.doc / "manual.org").write_text("#+title: Manual\nVia HOME.\n")
        relative = self.repo.relative_to(Path(self.temp_dir.name))
        command = (
            f'cd "$HOME/{relative}" && git add example.el '
            "emacs/extras/doc/manual.org && git commit -q -m subject"
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(
                    hook, command_field, command, cwd=self.elsewhere,
                    workdir=self.elsewhere, home=self.temp_dir.name,
                )
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_a_generated_texi_staged_by_the_same_command_is_accepted(self):
        """The hook runs before the git add it is inspecting.

        Refusing because the generated file is unstaged, when the very command
        being judged stages it, blocks a correct commit.
        """
        manual = self.doc / "manual.org"
        manual.write_text(
            "#+title: Manual\n#+texinfo_filename: manual.info\nBody.\n"
        )
        generated = self.doc / "manual.texi"
        generated.write_text("@node Top\n")
        subprocess.run(
            ["git", "-C", str(self.repo), "add", "-A"], check=True
        )
        subprocess.run(
            [
                "git", "-C", str(self.repo),
                "-c", "user.name=Test", "-c", "user.email=test@example.com",
                "commit", "-qm", "manual",
            ],
            check=True,
        )
        manual.write_text(
            "#+title: Manual\n#+texinfo_filename: manual.info\nBody changed.\n"
        )
        subprocess.run(
            ["git", "-C", str(self.repo), "add", "emacs/extras/doc/manual.org"],
            check=True,
        )
        generated.write_text("@node Top\n@c regenerated\n")
        command = (
            f'cd "{self.repo}" && git add emacs/extras/doc/manual.texi '
            "&& git commit -q -m x"
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertNotIn("generated Texinfo", deny_reason(result))

    def test_an_apostrophe_in_a_heredoc_message_is_not_read_as_arguments(self):
        """Prose in a heredoc is data, not arguments.

        An apostrophe in a commit message ("the hook's own context") makes the
        shell lexer raise, and the parser then fails closed by synthesising a
        .el path, so a commit staging no Elisp at all is refused.
        """
        command = (
            "cat > /tmp/msg.txt <<'EOF'\n"
            "hooks: fix the session's own repository\n"
            "EOF\n"
            f'cd "{self.repo}" && git add README.org && git commit -q -F /tmp/msg.txt'
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_a_heredoc_body_naming_an_el_file_is_not_read_as_arguments(self):
        """A message that talks about Elisp is not a commit of Elisp."""
        command = (
            "cat > /tmp/msg.txt <<'EOF'\n"
            "tests: cover example.el and git commit paths\n"
            "EOF\n"
            f'cd "{self.repo}" && git add README.org && git commit -q -F /tmp/msg.txt'
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_genuinely_unparseable_arguments_still_fail_closed(self):
        """Fail-closed is kept for argument text that cannot be lexed."""
        (self.repo / "example.el").write_text("(provide 'example)\n")
        command = f'cd "{self.repo}" && git add example.el \'unclosed && git commit -q -m x'
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(hook, command_field, command)
                self.assertEqual(permission_decision(result), "deny")

    def test_a_cd_after_a_preamble_still_decides_the_repository(self):
        """The cd need not be the first thing in the command.

        Writing a message to a file, or setting a variable, before changing
        directory is ordinary, and the repository the command commits in is the
        same either way.
        """
        (self.repo / "example.el").write_text("(provide 'example)\n")
        (self.doc / "manual.org").write_text("#+title: Manual\nAfter preamble.\n")
        command = (
            "MSG=/tmp/some-message.txt && "
            f'cd "{self.repo}" && git add example.el '
            "emacs/extras/doc/manual.org && git commit -q -F $MSG"
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(
                    hook, command_field, command, cwd=self.elsewhere,
                    workdir=self.elsewhere,
                )
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_the_last_cd_in_the_chain_decides_the_repository(self):
        (self.repo / "example.el").write_text("(provide 'example)\n")
        (self.doc / "manual.org").write_text("#+title: Manual\nLast cd.\n")
        command = (
            f'cd "{self.elsewhere}" && echo staging ; cd "{self.repo}" && '
            "git add example.el emacs/extras/doc/manual.org && git commit -q -m x"
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(
                    hook, command_field, command, cwd=self.elsewhere,
                    workdir=self.elsewhere,
                )
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def test_an_unresolvable_cd_target_is_not_swapped_for_the_session_repo(self):
        """An unresolvable target must not be answered from a different tree.

        Evaluating the session's repository instead is unsound in both
        directions: it refuses correct commits, and it approves ones it should
        refuse whenever that other tree happens to hold a staged manual.
        """
        (self.repo / "example.el").write_text("(provide 'example)\n")
        command = (
            'cd "$SOME_UNSET_VARIABLE/repo" && git add example.el '
            "&& git commit -q -m subject"
        )
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(
                    hook, command_field, command, cwd=self.elsewhere,
                    workdir=self.elsewhere,
                )
                self.assertNotIn("README.org", deny_reason(result))

    def test_one_shot_add_and_commit_does_not_depend_on_the_hook_cwd(self):
        (self.repo / "example.el").write_text("(provide 'example)\n")
        (self.doc / "manual.org").write_text("#+title: Manual\nUpdated again.\n")
        command = "git add example.el emacs/extras/doc/manual.org && git commit -m test"
        for hook, command_field in HOOKS:
            with self.subTest(hook=hook):
                result = self.run_hook(
                    hook, command_field, command, cwd=self.elsewhere
                )
                self.assertEqual(
                    permission_decision(result), "allow", deny_reason(result)
                )

    def run_functions_exec_hook(self, command):
        source = (
            "const r = await tools.exec_command("
            + json.dumps({"cmd": command, "workdir": str(self.elsewhere)})
            + "); text(r.output);"
        )
        transcript = Path(self.temp_dir.name) / "transcript.jsonl"
        transcript.write_text(
            json.dumps(
                {
                    "type": "response_item",
                    "payload": {
                        "type": "custom_tool_call",
                        "name": "exec",
                        "input": source,
                    },
                }
            )
            + "\n"
        )
        payload = {
            "tool_name": "Bash",
            "cwd": str(self.repo),
            "transcript_path": str(transcript),
            "tool_input": {"command": command},
        }
        return subprocess.run(
            ["bash", str(DOTFILES / "codex/hooks/require-doc-update.sh")],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
            cwd=self.repo,
        )

    def test_functions_exec_uses_nested_standalone_repo_context(self):
        """A nested literal workdir owns the commit documentation policy."""
        source = self.elsewhere / "example.el"
        source.write_text("(provide 'example)\n")
        manual = self.elsewhere / "README.org"
        manual.write_text("#+title: Elsewhere\nUpdated.\n")
        result = self.run_functions_exec_hook(
            "git add README.org example.el && git commit -m test"
        )
        self.assertEqual(permission_decision(result), "allow", deny_reason(result))

    def test_functions_exec_still_requires_nested_repo_manual(self):
        """Unpacking the nested workdir must not bypass the documentation gate."""
        source = self.elsewhere / "example.el"
        source.write_text("(provide 'example)\n")
        result = self.run_functions_exec_hook(
            "git add example.el && git commit -m test"
        )
        self.assertEqual(permission_decision(result), "deny")
        self.assertIn("README.org", deny_reason(result))
        self.assertNotIn("doc/*.org", deny_reason(result))


if __name__ == "__main__":
    unittest.main()
