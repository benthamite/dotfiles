"""Tests for 1Password CLI routing.

Every `op` invocation must reach either `op-automations` (read-only service
account, no prompt) or `op-desktop` (one authorized pty session, one Touch ID
prompt per 10-minute window). Reaching the raw binary with no controlling
terminal is what produces a prompt per command.

Three independent things have to hold, and each has failed in practice:

1. `shell/shims/op` routes correctly, including the case that actually broke --
   a personal-vault reference while an Automations service-account token happens
   to be in the environment.
2. Routing survives the agent harness. The Claude Code and Codex Bash tools
   source a snapshot ending in a frozen `export PATH=...` that puts
   /opt/homebrew/bin ahead of shell/shims, so the `op` shell function in
   .zshenv, not PATH order, is what holds there.
3. No in-tree script calls the raw binary. Child processes inherit neither the
   function nor a guaranteed PATH, so scripts must name a wrapper themselves.
   `ahrefs-api-guard` did not, and produced a stray prompt on 2026-08-01.
"""

from __future__ import annotations

import os
import re
import stat
import subprocess
import tempfile
import textwrap
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SHIM = ROOT / "shell" / "shims" / "op"
SHIMS_DIR = ROOT / "shell" / "shims"

# Files allowed to invoke bare `op` in a subprocess. Both export a service-account
# token first, so their reads are prompt-free by construction. Anything new here
# needs the same property or a wrapper -- see op_reader_for() in ahrefs-api-guard.
BARE_OP_ALLOWED = {
    "claude/bin/slack.py",
    "claude/bin/_gworkspace_auth.py",
    "claude/bin/update-gworkspace-refresh-token",
}

# Directories whose contents are policy text about `op` rather than callers of it.
SCAN_SKIP_PREFIXES = (
    "tests/",
    "archive/",
    "claude/hooks/",
    "codex/hooks/",
)


def write_executable(path: Path, content: str) -> None:
    path.write_text(textwrap.dedent(content).lstrip())
    path.chmod(path.stat().st_mode | stat.S_IXUSR)


class ShimRoutingTest(unittest.TestCase):
    """The shim sends each invocation to the wrapper that can serve it."""

    def route(self, *args: str, env_extra: dict[str, str] | None = None) -> str:
        """Run the shim with stub wrappers on PATH; return what it routed to."""
        with tempfile.TemporaryDirectory() as tmp:
            tmp_path = Path(tmp)
            for name in ("op-desktop", "op-automations"):
                write_executable(
                    tmp_path / name,
                    f"""
                    #!/bin/sh
                    printf '{name} %s\\n' "$*"
                    """,
                )
            # A stub for the real binary too, so a routing bug cannot reach the
            # actual 1Password CLI and prompt during a test run.
            fake_bin = tmp_path / "fake-op"
            write_executable(
                fake_bin,
                """
                #!/bin/sh
                printf 'REAL %s\\n' "$*"
                """,
            )
            env = dict(os.environ)
            env["PATH"] = f"{tmp_path}:{env['PATH']}"
            env.pop("OP_SERVICE_ACCOUNT_TOKEN", None)
            env.pop("OP_SHIM_BYPASS", None)
            env.pop("OP_NO_SHIM", None)
            if env_extra:
                env.update(env_extra)
            # Point the shim's REAL at the stub by shadowing the resolved paths.
            script = SHIM.read_text().replace(
                "REAL=/opt/homebrew/bin/op", f"REAL={fake_bin}"
            ).replace('[ -x "$REAL" ] || REAL=/usr/local/bin/op', "")
            shim_copy = tmp_path / "op-under-test"
            write_executable(shim_copy, script)
            result = subprocess.run(
                ["sh", str(shim_copy), *args],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            return result.stdout.strip()

    def test_personal_vault_reference_uses_the_broker(self):
        out = self.route("read", "op://Employee/Ahrefs - Claude Tag/credential")
        self.assertTrue(out.startswith("op-desktop "), out)

    def test_automations_reference_uses_the_service_account(self):
        out = self.route("read", "op://Automations/Ahrefs API/credential")
        self.assertTrue(out.startswith("op-automations "), out)

    def test_explicit_automations_vault_flag_uses_the_service_account(self):
        self.assertTrue(
            self.route("item", "get", "Foo", "--vault", "Automations").startswith(
                "op-automations "
            )
        )
        self.assertTrue(
            self.route("item", "get", "Foo", "--vault=Automations").startswith(
                "op-automations "
            )
        )

    def test_ambient_service_token_does_not_hijack_a_personal_vault_read(self):
        """The bug this shim exists to prevent.

        `ahrefs-api-guard --op-ref op://Employee/...` inherited a service-account
        context and failed, because that account cannot read personal vaults.
        A personal reference must reach the broker regardless of the token.
        """
        out = self.route(
            "read",
            "op://Employee/Foo/credential",
            env_extra={"OP_SERVICE_ACCOUNT_TOKEN": "irrelevant"},
        )
        self.assertTrue(out.startswith("op-desktop "), out)

    def test_automations_read_with_token_present_skips_the_extra_hop(self):
        out = self.route(
            "read",
            "op://Automations/Foo/credential",
            env_extra={"OP_SERVICE_ACCOUNT_TOKEN": "present"},
        )
        self.assertTrue(out.startswith("REAL "), out)

    def test_bypass_env_vars_reach_the_binary_directly(self):
        """op-desktop's broker and op-automations set these to avoid recursion."""
        for var in ("OP_SHIM_BYPASS", "OP_NO_SHIM"):
            out = self.route("read", "op://Employee/Foo/x", env_extra={var: "1"})
            self.assertTrue(out.startswith("REAL "), f"{var}: {out}")

    def test_streaming_subcommands_reach_the_binary_directly(self):
        """op-desktop cannot forward stdin, so these must not be routed to it."""
        for args in (
            ("run", "--", "env"),
            ("inject",),
            ("signin",),
            ("document", "create", "file.txt"),
            ("read", "-"),
        ):
            out = self.route(*args)
            self.assertTrue(out.startswith("REAL "), f"{args}: {out}")


class RoutingSurvivesAgentShellTest(unittest.TestCase):
    """Routing must hold in the shells the agent harness actually uses."""

    def zsh(self, args: list[str], command: str) -> str:
        result = subprocess.run(
            ["zsh", *args, command],
            capture_output=True,
            text=True,
            check=False,
        )
        return result.stdout.strip()

    def test_op_is_a_function_in_a_non_interactive_shell(self):
        """PATH order cannot be trusted in the harness; the function is what holds.

        The Bash tool sources a snapshot whose last act is a frozen
        `export PATH=...` placing /opt/homebrew/bin ahead of shell/shims. That
        rewrites PATH but not function definitions.
        """
        self.assertEqual(self.zsh(["-c"], "whence -w op"), "op: function")

    def test_shims_precede_homebrew_in_a_login_interactive_shell(self):
        """This is the PATH the harness captures into its snapshot."""
        path = self.zsh(["-lic"], "print -r -- $PATH")
        entries = path.split(":")
        self.assertIn(str(SHIMS_DIR), entries, path)
        self.assertIn("/opt/homebrew/bin", entries, path)
        self.assertLess(
            entries.index(str(SHIMS_DIR)),
            entries.index("/opt/homebrew/bin"),
            f"shims must precede Homebrew, got: {path}",
        )


class NoRawBinaryCallersTest(unittest.TestCase):
    """No tracked script may reach the 1Password binary without a wrapper."""

    def tracked_files(self) -> list[str]:
        result = subprocess.run(
            ["git", "ls-files"],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        return [
            rel
            for rel in result.stdout.splitlines()
            if not rel.startswith(SCAN_SKIP_PREFIXES)
        ]

    def test_no_absolute_path_op_invocations(self):
        """An absolute path defeats both the shim and the function."""
        pattern = re.compile(r"(?<!\w)/(?:opt/homebrew|usr/local|usr)/bin/op(?!\w)")
        offenders = []
        for rel in self.tracked_files():
            # The shim and the broker resolve the real binary on purpose.
            if rel in {"shell/shims/op", "bin/op-desktop"}:
                continue
            path = ROOT / rel
            try:
                text = path.read_text()
            except (UnicodeDecodeError, FileNotFoundError, IsADirectoryError):
                continue
            if pattern.search(text):
                offenders.append(rel)
        self.assertEqual(offenders, [], f"absolute-path op invocations: {offenders}")

    def test_no_new_bare_op_subprocess_callers(self):
        """Child processes get neither the function nor a guaranteed PATH."""
        pattern = re.compile(r"""\[\s*["']op["']\s*,""")
        offenders = []
        for rel in self.tracked_files():
            if rel in BARE_OP_ALLOWED or rel == "shell/shims/op":
                continue
            path = ROOT / rel
            try:
                text = path.read_text()
            except (UnicodeDecodeError, FileNotFoundError, IsADirectoryError):
                continue
            if pattern.search(text):
                offenders.append(rel)
        self.assertEqual(
            offenders,
            [],
            "these call bare `op` in a subprocess; name op-desktop or "
            f"op-automations instead: {offenders}",
        )


if __name__ == "__main__":
    unittest.main()
