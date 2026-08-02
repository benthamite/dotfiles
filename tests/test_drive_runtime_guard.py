"""Tests for the Drive runtime-state guard pair and its shared parser.

The paired PreToolUse guards (claude/hooks/block-drive-runtime-command.sh and
codex/hooks/block-drive-runtime-command.sh) must be byte-identical, executable,
and must DENY state-creating commands whose effective project, cwd, or explicit
target lies under ~/My Drive. All command parsing lives in the single Python
module lib/drive_runtime_command.py; a command the module cannot model is
`indeterminate` and is denied — there is no silent allow fallback and no
ask/approval response.
"""

from __future__ import annotations

import json
import os
import subprocess
import unittest
from pathlib import Path

from lib.drive_runtime_command import parse_command


ROOT = Path(__file__).resolve().parents[1]
HOME = Path.home()
DRIVE = HOME / "My Drive"
DRIVE_PROJECT = str(DRIVE / "drive-runtime-guard-testproj")
EXTERNAL_PROJECT = str(HOME / "repos" / "drive-runtime-guard-testproj")

CLAUDE_GUARD = ROOT / "claude" / "hooks" / "block-drive-runtime-command.sh"
CODEX_GUARD = ROOT / "codex" / "hooks" / "block-drive-runtime-command.sh"
DISPATCHER = ROOT / "claude" / "hooks" / "pretooluse-bash.sh"

EXACT_REASON = (
    "Active dependency, build, cache, or worktree state is not allowed under "
    "~/My Drive; migrate the repository or run the workflow from its approved "
    "external workspace."
)


def decision_of(segments: list[dict]) -> str:
    """Collapse parser segments into the guard's allow/deny decision."""
    if not isinstance(segments, list) or not segments:
        return "deny"
    if all(seg.get("verdict") == "allow" for seg in segments):
        return "allow"
    return "deny"


class ParseCommandPolicyTest(unittest.TestCase):
    """Direct unit tests of lib.drive_runtime_command.parse_command()."""

    def assert_decision(self, command: str, cwd: str, expected: str) -> None:
        segments = parse_command(command, cwd)
        self.assertEqual(
            decision_of(segments),
            expected,
            f"command={command!r} cwd={cwd!r} segments={segments!r}",
        )

    def assert_indeterminate(self, command: str, cwd: str = EXTERNAL_PROJECT) -> None:
        segments = parse_command(command, cwd)
        self.assertTrue(
            any(seg.get("verdict") == "indeterminate" for seg in segments),
            f"expected indeterminate: command={command!r} segments={segments!r}",
        )
        # Indeterminate must never coexist with a partial parse that the guard
        # could mistake for a full allow.
        self.assertEqual(decision_of(segments), "deny")

    # --- npm ---------------------------------------------------------------

    def test_npm_install_in_drive_cwd_is_denied(self):
        self.assert_decision("npm install", DRIVE_PROJECT, "deny")

    def test_npm_i_alias_in_drive_cwd_is_denied(self):
        self.assert_decision("npm i", DRIVE_PROJECT, "deny")

    def test_npm_ci_in_drive_cwd_is_denied(self):
        self.assert_decision("npm ci", DRIVE_PROJECT, "deny")

    def test_npm_install_outside_drive_is_allowed(self):
        self.assert_decision("npm install", EXTERNAL_PROJECT, "allow")

    def test_npm_run_build_dev_test_in_drive_cwd_are_denied(self):
        for script in ("build", "dev", "test"):
            self.assert_decision(f"npm run {script}", DRIVE_PROJECT, "deny")

    def test_npm_run_build_outside_drive_is_allowed(self):
        self.assert_decision("npm run build", EXTERNAL_PROJECT, "allow")

    def test_npm_prefix_pointing_into_drive_is_denied(self):
        self.assert_decision(
            f'npm --prefix "{DRIVE_PROJECT}" install', EXTERNAL_PROJECT, "deny"
        )

    def test_npm_prefix_equals_form_into_drive_is_denied(self):
        self.assert_decision(
            f'npm --prefix="{DRIVE_PROJECT}" run build', EXTERNAL_PROJECT, "deny"
        )

    def test_npm_prefix_pointing_outside_drive_is_allowed_from_drive_cwd(self):
        self.assert_decision(
            f'npm --prefix "{EXTERNAL_PROJECT}" install', DRIVE_PROJECT, "allow"
        )

    def test_npm_ls_in_drive_cwd_is_allowed(self):
        self.assert_decision("npm ls", DRIVE_PROJECT, "allow")

    # --- uv ----------------------------------------------------------------

    def test_uv_sync_in_drive_cwd_is_denied(self):
        self.assert_decision("uv sync", DRIVE_PROJECT, "deny")

    def test_uv_sync_outside_drive_is_allowed(self):
        self.assert_decision("uv sync", EXTERNAL_PROJECT, "allow")

    def test_project_bound_uv_run_in_drive_cwd_is_denied(self):
        self.assert_decision("uv run reasoning --help", DRIVE_PROJECT, "deny")

    def test_uv_run_outside_drive_is_allowed(self):
        self.assert_decision("uv run reasoning --help", EXTERNAL_PROJECT, "allow")

    def test_uv_project_flag_into_drive_is_denied(self):
        self.assert_decision(
            f'uv --project "{DRIVE_PROJECT}" sync', EXTERNAL_PROJECT, "deny"
        )

    def test_uv_pip_install_in_drive_cwd_is_denied(self):
        self.assert_decision("uv pip install requests", DRIVE_PROJECT, "deny")

    def test_uv_pip_list_in_drive_cwd_is_allowed(self):
        self.assert_decision("uv pip list", DRIVE_PROJECT, "allow")

    # --- python / venv / pip ------------------------------------------------

    def test_python_m_venv_in_drive_cwd_is_denied(self):
        self.assert_decision("python3 -m venv .venv", DRIVE_PROJECT, "deny")

    def test_python_m_venv_targeting_drive_from_outside_is_denied(self):
        self.assert_decision(
            f'python3 -B -m venv "{DRIVE_PROJECT}/.venv"', EXTERNAL_PROJECT, "deny"
        )

    def test_python_m_venv_external_target_with_bytecode_guard_is_allowed(self):
        self.assert_decision(
            f'python3 -B -m venv "{EXTERNAL_PROJECT}/.venv"', DRIVE_PROJECT, "allow"
        )

    def test_pip_install_in_drive_cwd_is_denied(self):
        self.assert_decision("pip install requests", DRIVE_PROJECT, "deny")

    def test_pip3_install_in_drive_cwd_is_denied(self):
        self.assert_decision("pip3 install requests", DRIVE_PROJECT, "deny")

    def test_python_m_pip_install_in_drive_cwd_is_denied(self):
        self.assert_decision("python3 -m pip install requests", DRIVE_PROJECT, "deny")

    def test_python_m_pip_install_with_dash_b_in_drive_cwd_is_still_denied(self):
        self.assert_decision(
            "python3 -B -m pip install requests", DRIVE_PROJECT, "deny"
        )

    def test_pip_install_editable_drive_path_is_denied(self):
        self.assert_decision(
            f'pip install -e "{DRIVE_PROJECT}"', EXTERNAL_PROJECT, "deny"
        )

    def test_pip_install_target_into_drive_is_denied(self):
        self.assert_decision(
            f'pip install --target "{DRIVE_PROJECT}/vendor" requests',
            EXTERNAL_PROJECT,
            "deny",
        )

    def test_pip_install_outside_drive_is_allowed(self):
        self.assert_decision("pip install requests", EXTERNAL_PROJECT, "allow")

    def test_pip_list_in_drive_cwd_is_allowed(self):
        self.assert_decision("pip list", DRIVE_PROJECT, "allow")

    def test_python_without_bytecode_guard_in_drive_cwd_is_denied(self):
        self.assert_decision("python3 script.py", DRIVE_PROJECT, "deny")

    def test_python_with_dash_b_in_drive_cwd_is_allowed(self):
        self.assert_decision("python3 -B script.py", DRIVE_PROJECT, "allow")

    def test_python_with_env_prefix_in_drive_cwd_is_allowed(self):
        self.assert_decision(
            "PYTHONDONTWRITEBYTECODE=1 python3 script.py", DRIVE_PROJECT, "allow"
        )

    def test_python_outside_drive_without_dash_b_is_allowed(self):
        self.assert_decision("python3 script.py", EXTERNAL_PROJECT, "allow")

    def test_python_explicit_drive_script_without_dash_b_is_denied(self):
        self.assert_decision(
            f'python3 "{DRIVE_PROJECT}/tool.py"', EXTERNAL_PROJECT, "deny"
        )

    def test_python_version_in_drive_cwd_is_allowed(self):
        self.assert_decision("python3 --version", DRIVE_PROJECT, "allow")

    # --- pytest -------------------------------------------------------------

    def test_pytest_bare_in_drive_cwd_is_denied(self):
        self.assert_decision("pytest", DRIVE_PROJECT, "deny")

    def test_pytest_with_only_bytecode_guard_in_drive_cwd_is_denied(self):
        self.assert_decision(
            "PYTHONDONTWRITEBYTECODE=1 pytest", DRIVE_PROJECT, "deny"
        )

    def test_pytest_with_only_cache_guard_in_drive_cwd_is_denied(self):
        self.assert_decision("pytest -p no:cacheprovider", DRIVE_PROJECT, "deny")

    def test_pytest_with_both_guards_in_drive_cwd_is_allowed(self):
        self.assert_decision(
            "PYTHONDONTWRITEBYTECODE=1 pytest -p no:cacheprovider",
            DRIVE_PROJECT,
            "allow",
        )

    def test_python_m_pytest_with_both_guards_in_drive_cwd_is_allowed(self):
        self.assert_decision(
            "python3 -B -m pytest -p no:cacheprovider", DRIVE_PROJECT, "allow"
        )

    def test_pytest_bare_outside_drive_is_allowed(self):
        self.assert_decision("pytest", EXTERNAL_PROJECT, "allow")

    # --- git worktree -------------------------------------------------------

    def test_git_worktree_add_targeting_drive_is_denied(self):
        self.assert_decision(
            f'git worktree add "{DRIVE_PROJECT}/wt" branch', EXTERNAL_PROJECT, "deny"
        )

    def test_git_worktree_add_relative_in_drive_cwd_is_denied(self):
        self.assert_decision("git worktree add ../wt", DRIVE_PROJECT, "deny")

    def test_git_worktree_add_external_target_is_allowed(self):
        self.assert_decision(
            f'git worktree add "{EXTERNAL_PROJECT}/wt" branch', DRIVE_PROJECT, "allow"
        )

    def test_git_dash_c_worktree_add_relative_resolves_against_dash_c(self):
        self.assert_decision(
            f'git -C "{DRIVE_PROJECT}" worktree add wt', EXTERNAL_PROJECT, "deny"
        )

    def test_git_worktree_list_in_drive_cwd_is_allowed(self):
        self.assert_decision("git worktree list", DRIVE_PROJECT, "allow")

    def test_git_status_in_drive_cwd_is_allowed(self):
        self.assert_decision("git status", DRIVE_PROJECT, "allow")

    # --- chains, subshells, env prefixes, quoting ---------------------------

    def test_cd_into_drive_then_npm_install_is_denied(self):
        self.assert_decision(
            f'cd "{DRIVE_PROJECT}" && npm install', EXTERNAL_PROJECT, "deny"
        )

    def test_cd_out_of_drive_then_npm_install_is_allowed(self):
        self.assert_decision(
            f'cd "{EXTERNAL_PROJECT}" && npm install', DRIVE_PROJECT, "allow"
        )

    def test_subshell_cd_into_drive_then_npm_install_is_denied(self):
        self.assert_decision(
            f'(cd "{DRIVE_PROJECT}" && npm install)', EXTERNAL_PROJECT, "deny"
        )

    def test_subshell_cd_does_not_leak_into_outer_scope(self):
        # The cd applies inside the subshell only; the outer pytest still runs
        # under the Drive cwd and must be denied.
        self.assert_decision(
            f'(cd "{EXTERNAL_PROJECT}" && npm ci); pytest', DRIVE_PROJECT, "deny"
        )

    def test_subshell_external_state_command_from_drive_cwd_is_allowed(self):
        self.assert_decision(
            f'(cd "{EXTERNAL_PROJECT}" && npm ci)', DRIVE_PROJECT, "allow"
        )

    def test_env_prefix_is_stripped_before_classification(self):
        self.assert_decision("NODE_ENV=production npm install", DRIVE_PROJECT, "deny")

    def test_tilde_path_target_into_drive_is_denied(self):
        self.assert_decision(
            "git worktree add ~/My\\ Drive/wt", EXTERNAL_PROJECT, "deny"
        )

    def test_pipeline_segments_are_evaluated_individually(self):
        self.assert_decision("npm install | tail -5", DRIVE_PROJECT, "deny")

    def test_read_only_command_mentioning_python_is_allowed(self):
        self.assert_decision("grep -r python .", DRIVE_PROJECT, "allow")

    # --- pyenv exec transparent wrapper -------------------------------------

    # The manifest's exact tangodb smoke argv. The guard sees only argv, not
    # the manifest's env (PYTHONDONTWRITEBYTECODE=1), and the argv alone lacks
    # bytecode prevention: pytest's dual guard therefore denies under Drive,
    # while the external-workspace run the manifest prescribes is allowed.
    TANGODB_SMOKE = "pyenv exec python -m pytest -q -p no:cacheprovider"

    def test_pyenv_exec_tangodb_smoke_argv_in_drive_cwd_is_denied(self):
        self.assert_decision(self.TANGODB_SMOKE, DRIVE_PROJECT, "deny")

    def test_pyenv_exec_tangodb_smoke_argv_outside_drive_is_allowed(self):
        self.assert_decision(self.TANGODB_SMOKE, EXTERNAL_PROJECT, "allow")

    def test_pyenv_exec_npm_ci_in_drive_cwd_is_denied(self):
        self.assert_decision("pyenv exec npm ci", DRIVE_PROJECT, "deny")

    def test_pyenv_exec_npm_ci_outside_drive_is_allowed(self):
        self.assert_decision("pyenv exec npm ci", EXTERNAL_PROJECT, "allow")

    def test_pyenv_non_exec_subcommand_keeps_current_verdict(self):
        # Only "pyenv exec" is transparent; other subcommands keep the
        # pre-existing unknown-command classification (pinned here as allow).
        for cwd in (DRIVE_PROJECT, EXTERNAL_PROJECT):
            self.assert_decision("pyenv install 3.12", cwd, "allow")

    # --- find/fd exec delegation (reviewer probes) --------------------------

    def test_find_exec_npm_ci_in_drive_cwd_is_denied(self):
        self.assert_decision("find . -type d -exec npm ci ;", DRIVE_PROJECT, "deny")

    def test_find_execdir_npm_ci_in_drive_cwd_is_denied(self):
        self.assert_decision("find . -execdir npm ci ;", DRIVE_PROJECT, "deny")

    def test_find_exec_with_escaped_terminator_is_denied_under_drive(self):
        self.assert_decision("find . -exec npm ci \\;", DRIVE_PROJECT, "deny")

    def test_fd_x_npm_ci_in_drive_cwd_is_denied(self):
        self.assert_decision("fd -x npm ci", DRIVE_PROJECT, "deny")

    def test_fd_exec_pip_install_in_drive_cwd_is_denied(self):
        self.assert_decision("fd --exec pip install x", DRIVE_PROJECT, "deny")

    def test_fd_exec_batch_state_command_is_denied_under_drive(self):
        self.assert_decision("fd -e py -X pytest", DRIVE_PROJECT, "deny")

    def test_find_ok_state_command_is_denied_under_drive(self):
        self.assert_decision("find . -ok npm install ;", DRIVE_PROJECT, "deny")

    def test_find_without_exec_flag_stays_read_only_allowed_under_drive(self):
        self.assert_decision("find . -name '*.py'", DRIVE_PROJECT, "allow")

    def test_find_exec_of_read_only_command_is_allowed_under_drive(self):
        # Delegated read-only commands (with or without the {} placeholder)
        # keep find allowed; only state-creating or unmodelable delegations
        # deny.
        self.assert_decision(
            "find . -name '*.py' -exec grep -l pattern {} ;", DRIVE_PROJECT, "allow"
        )

    def test_find_exec_state_command_outside_drive_is_allowed(self):
        self.assert_decision("find . -exec npm ci ;", EXTERNAL_PROJECT, "allow")

    def test_find_exec_placeholder_in_state_command_is_denied(self):
        # {} inside a state-creating delegated command cannot be resolved
        # safely: indeterminate, denied even outside Drive.
        segments = parse_command(
            "find . -type d -exec npm --prefix {} ci ;", EXTERNAL_PROJECT
        )
        self.assertEqual(decision_of(segments), "deny")

    def test_find_over_drive_path_with_state_exec_is_denied_from_outside(self):
        self.assert_decision(
            f'find "{DRIVE_PROJECT}" -type d -exec npm ci ;', EXTERNAL_PROJECT, "deny"
        )

    def test_nested_find_exec_delegation_is_still_denied(self):
        self.assert_decision(
            "find . -exec find . -exec npm ci ; ;", DRIVE_PROJECT, "deny"
        )

    # --- indeterminate forms are denied, never partially parsed -------------

    def test_unterminated_quote_is_indeterminate(self):
        self.assert_indeterminate('npm install "x')

    def test_heredoc_is_indeterminate(self):
        self.assert_indeterminate("python3 - <<EOF\nprint(1)\nEOF")

    def test_command_substitution_is_indeterminate(self):
        self.assert_indeterminate("npm install $(cat packages.txt)")

    def test_process_substitution_is_indeterminate(self):
        self.assert_indeterminate("pytest <(generate-tests)")

    def test_backtick_substitution_is_indeterminate(self):
        self.assert_indeterminate("npm install `cat packages.txt`")

    def test_shell_function_definition_is_indeterminate(self):
        self.assert_indeterminate("foo() ( npm install )")

    def test_shell_keyword_is_indeterminate(self):
        self.assert_indeterminate("while true; do npm install; done")

    def test_redirection_is_indeterminate(self):
        self.assert_indeterminate("npm install > install.log")

    def test_stderr_redirection_is_indeterminate(self):
        self.assert_indeterminate("npm install 2>&1")

    def test_variable_expansion_is_indeterminate(self):
        self.assert_indeterminate('npm --prefix "$DIR" install')

    def test_unknown_wrapper_of_state_command_is_indeterminate(self):
        self.assert_indeterminate("mystery-wrapper npm install")

    def test_shell_interpreter_wrapper_is_indeterminate(self):
        self.assert_indeterminate("bash -c 'npm install'")

    def test_indeterminate_is_denied_even_outside_drive(self):
        segments = parse_command("npm install > install.log", EXTERNAL_PROJECT)
        self.assertEqual(decision_of(segments), "deny")


class GuardFileTest(unittest.TestCase):
    """Byte parity, executable modes, and hook-JSON behavior of the guards."""

    def test_guard_files_are_byte_identical(self):
        self.assertEqual(CLAUDE_GUARD.read_bytes(), CODEX_GUARD.read_bytes())

    def test_executable_modes_on_disk(self):
        for path in (
            ROOT / "bin" / "drive-workspace",
            ROOT / "bin" / "drive-workspace-native.jxa",
            CLAUDE_GUARD,
            CODEX_GUARD,
        ):
            with self.subTest(path=str(path)):
                out = subprocess.run(
                    ["stat", "-f", "%Lp", str(path)],
                    capture_output=True,
                    text=True,
                    check=True,
                ).stdout.strip()
                self.assertEqual(out, "755", f"disk mode of {path} is {out}")

    def test_executable_modes_in_git_index(self):
        rels = [
            "bin/drive-workspace",
            "bin/drive-workspace-native.jxa",
            "claude/hooks/block-drive-runtime-command.sh",
            "codex/hooks/block-drive-runtime-command.sh",
        ]
        out = subprocess.run(
            ["git", "-C", str(ROOT), "ls-files", "-s", "--", *rels],
            capture_output=True,
            text=True,
            check=True,
        ).stdout
        modes = {
            line.split()[3]: line.split()[0] for line in out.strip().splitlines()
        }
        for rel in rels:
            with self.subTest(path=rel):
                self.assertEqual(modes.get(rel), "100755", f"git mode for {rel}")

    def run_guard(self, guard: Path, payload: dict) -> dict | None:
        result = subprocess.run(
            ["bash", str(guard)],
            input=json.dumps(payload),
            capture_output=True,
            text=True,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        if not result.stdout.strip():
            return None
        return json.loads(result.stdout)

    def claude_payload(self, command: str, cwd: str) -> dict:
        return {
            "tool_name": "Bash",
            "tool_input": {"command": command},
            "cwd": cwd,
        }

    def codex_payload(self, command: str, cwd: str) -> dict:
        return {
            "tool_name": "exec_command",
            "tool_input": json.dumps({"command": command, "workdir": cwd}),
        }

    def assert_denied(self, output: dict | None) -> None:
        self.assertIsNotNone(output, "expected a deny decision, got silence")
        hso = output["hookSpecificOutput"]
        self.assertEqual(hso["permissionDecision"], "deny")
        self.assertEqual(hso["permissionDecisionReason"], EXACT_REASON)

    def test_both_guards_deny_npm_install_with_drive_cwd(self):
        for guard in (CLAUDE_GUARD, CODEX_GUARD):
            with self.subTest(guard=str(guard)):
                out = self.run_guard(
                    guard, self.claude_payload("npm install", DRIVE_PROJECT)
                )
                self.assert_denied(out)

    def test_both_guards_allow_npm_install_outside_drive(self):
        for guard in (CLAUDE_GUARD, CODEX_GUARD):
            with self.subTest(guard=str(guard)):
                out = self.run_guard(
                    guard, self.claude_payload("npm install", EXTERNAL_PROJECT)
                )
                self.assertIsNone(out)

    def test_guard_handles_codex_string_payload(self):
        out = self.run_guard(
            CODEX_GUARD, self.codex_payload("npm install", DRIVE_PROJECT)
        )
        self.assert_denied(out)
        out = self.run_guard(
            CODEX_GUARD, self.codex_payload("npm install", EXTERNAL_PROJECT)
        )
        self.assertIsNone(out)

    def test_guard_denies_indeterminate_command(self):
        out = self.run_guard(
            CLAUDE_GUARD,
            self.claude_payload("npm install > install.log", EXTERNAL_PROJECT),
        )
        self.assert_denied(out)

    def test_guard_allows_benign_command_with_drive_cwd(self):
        out = self.run_guard(
            CLAUDE_GUARD, self.claude_payload("git status --short", DRIVE_PROJECT)
        )
        self.assertIsNone(out)

    def test_guard_denies_find_fd_exec_delegation_under_drive(self):
        for command in (
            "find . -type d -exec npm ci \\;",
            "find . -execdir npm ci \\;",
            "fd -x npm ci",
            "fd --exec pip install x",
        ):
            for guard in (CLAUDE_GUARD, CODEX_GUARD):
                with self.subTest(guard=str(guard), command=command):
                    out = self.run_guard(
                        guard, self.claude_payload(command, DRIVE_PROJECT)
                    )
                    self.assert_denied(out)

    def test_guard_allows_find_fd_exec_delegation_outside_drive(self):
        for command in ("find . -exec npm ci \\;", "fd -x npm ci"):
            with self.subTest(command=command):
                out = self.run_guard(
                    CLAUDE_GUARD, self.claude_payload(command, EXTERNAL_PROJECT)
                )
                self.assertIsNone(out)

    def test_guard_denies_pyenv_exec_pytest_under_drive_allows_outside(self):
        command = "pyenv exec python -m pytest -q -p no:cacheprovider"
        for guard in (CLAUDE_GUARD, CODEX_GUARD):
            with self.subTest(guard=str(guard)):
                out = self.run_guard(
                    guard, self.claude_payload(command, DRIVE_PROJECT)
                )
                self.assert_denied(out)
                out = self.run_guard(
                    guard, self.claude_payload(command, EXTERNAL_PROJECT)
                )
                self.assertIsNone(out)

    def test_guard_masks_quoted_prose(self):
        out = self.run_guard(
            CLAUDE_GUARD,
            self.claude_payload(
                "git commit -m 'document the npm install workflow'", DRIVE_PROJECT
            ),
        )
        self.assertIsNone(out)

    def test_guard_never_asks(self):
        # The guard's response vocabulary is deny-or-silence; an ask/approval
        # decision must never appear in the script.
        text = CLAUDE_GUARD.read_text()
        self.assertNotIn('"ask"', text)
        self.assertNotIn("permissionDecision\": \"ask", text)


class DispatcherRegistrationTest(unittest.TestCase):
    """The Claude dispatcher must route Bash commands through the guard."""

    def run_dispatcher(self, payload: dict) -> subprocess.CompletedProcess:
        return subprocess.run(
            [str(DISPATCHER)],
            input=json.dumps(payload),
            capture_output=True,
            text=True,
        )

    def test_dispatcher_denies_npm_install_with_drive_cwd(self):
        result = self.run_dispatcher(
            {
                "tool_name": "Bash",
                "tool_input": {"command": "npm install"},
                "cwd": DRIVE_PROJECT,
            }
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        output = json.loads(result.stdout)
        hso = output["hookSpecificOutput"]
        self.assertEqual(hso["permissionDecision"], "deny")
        self.assertEqual(hso["permissionDecisionReason"], EXACT_REASON)

    def test_dispatcher_stays_silent_for_external_npm_install(self):
        result = self.run_dispatcher(
            {
                "tool_name": "Bash",
                "tool_input": {"command": "npm install"},
                "cwd": EXTERNAL_PROJECT,
            }
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "")

    def test_codex_hooks_json_registers_the_guard(self):
        config = json.loads((ROOT / "codex" / "hooks.json").read_text())
        commands = [
            hook.get("command", "")
            for entries in config.get("hooks", {}).values()
            for entry in entries
            for hook in entry.get("hooks", [])
        ]
        self.assertTrue(
            any("block-drive-runtime-command.sh" in cmd for cmd in commands),
            "codex/hooks.json does not register block-drive-runtime-command.sh",
        )


if __name__ == "__main__":
    unittest.main()
