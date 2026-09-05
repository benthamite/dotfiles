"""PostToolUse hook output contract.

Claude Code validates hook JSON: ``hookSpecificOutput`` must carry
``hookEventName`` and the payload field is ``additionalContext``; a bare
``message`` field fails validation and the output is dropped.  JSON printed
alongside a non-zero exit is ignored as well, so failure branches that want
Claude to react must exit 0 and use top-level ``decision``/``reason``.

These tests exercise every output branch of the two hooks that regressed
(``load-elisp-after-edit.sh`` and ``regenerate-manual-after-edit.sh``) and
add a static guard over every tracked hook so the invalid shape cannot be
reintroduced by either copy of a paired script.
"""
from __future__ import annotations

import json
import os
import re
import subprocess
import tempfile
import unittest
from pathlib import Path

DOTFILES = Path(__file__).resolve().parents[1]
CLAUDE_RELOAD = DOTFILES / "claude/hooks/load-elisp-after-edit.sh"
MANUAL_HOOKS = (
    DOTFILES / "claude/hooks/regenerate-manual-after-edit.sh",
    DOTFILES / "codex/hooks/regenerate-manual-after-edit.sh",
)
HOOK_DIRS = (DOTFILES / "claude/hooks", DOTFILES / "codex/hooks")

FAKE_EMACSCLIENT = r"""#!/bin/sh
case "$FAKE_MODE" in
  resolve-error) echo "emacsclient: can't find socket" >&2; exit 1 ;;
esac
case "$*" in
  *format-build-reload-status*)
    case "$FAKE_MODE" in
      finished) printf '"finished:loaded"\n' ;;
      failed) printf '"failed:byte-compile error"\n' ;;
      *) printf '"queued:Build queued"\n' ;;
    esac ;;
  *)
    case "$FAKE_MODE" in
      nil) printf 'nil\n' ;;
      no-token) printf '"slack"\n' ;;
      *) printf '"slack:token-1"\n' ;;
    esac ;;
esac
"""

FAKE_EMACS = r"""#!/bin/sh
if [ "$FAKE_MODE" = "export-fails" ]; then
  echo "Symbol's value as variable is void: boom" >&2
  exit 1
fi
for arg in "$@"; do
  case "$arg" in
    *.org) org="$arg" ;;
  esac
done
printf 'Generated Texinfo fixture\n' > "$MANUAL_EXPORT_OUTPUT"
"""

FAKE_MAKEINFO = r"""#!/bin/sh
out=""
while [ $# -gt 0 ]; do
  case "$1" in
    -o) out="$2"; shift ;;
  esac
  shift
done
[ -n "$out" ] && printf 'info\n' > "$out"
"""


def assert_posttooluse_context(test: unittest.TestCase, result, needle: str):
    test.assertEqual(result.returncode, 0, result.stderr)
    payload = json.loads(result.stdout)
    specific = payload["hookSpecificOutput"]
    test.assertEqual(specific["hookEventName"], "PostToolUse")
    test.assertNotIn("message", specific)
    test.assertIn(needle, specific["additionalContext"])


def assert_posttooluse_failure(test: unittest.TestCase, result, needle: str):
    test.assertEqual(result.returncode, 0, result.stderr)
    payload = json.loads(result.stdout)
    test.assertEqual(payload["decision"], "block")
    test.assertIn(needle, payload["reason"])
    test.assertNotIn("hookSpecificOutput", payload)


class ReloadHookOutputContractTests(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        root = Path(self.temp_dir.name)
        self.repo = root / "repo"
        self.repo.mkdir()
        subprocess.run(["git", "init", "-q", str(self.repo)], check=True)
        self.elisp_file = self.repo / "elpaca/sources/emacs-slack/slack-feed.el"
        self.elisp_file.parent.mkdir(parents=True)
        self.elisp_file.write_text("(provide 'slack-feed)\n")
        self.fake_bin = root / "bin"
        self.fake_bin.mkdir()
        emacsclient = self.fake_bin / "emacsclient"
        emacsclient.write_text(FAKE_EMACSCLIENT)
        emacsclient.chmod(0o755)

    def run_hook(self, mode: str, **extra_env):
        env = os.environ.copy()
        env["FAKE_MODE"] = mode
        env["PATH"] = f"{self.fake_bin}:{env['PATH']}"
        env["ELPACA_RELOAD_POLL_INTERVAL_SECONDS"] = "0"
        env.update(extra_env)
        payload = {"tool_input": {"file_path": str(self.elisp_file)}}
        return subprocess.run(
            ["bash", str(CLAUDE_RELOAD)],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
            env=env,
        )

    def test_completed_rebuild_is_additional_context(self):
        assert_posttooluse_context(self, self.run_hook("finished"), "Completed rebuild+reload of slack")

    def test_unresolved_package_is_additional_context(self):
        assert_posttooluse_context(self, self.run_hook("nil"), "no elpaca package was resolved")

    def test_missing_token_is_additional_context(self):
        assert_posttooluse_context(self, self.run_hook("no-token"), "no completion token")

    def test_git_operation_skip_is_additional_context(self):
        (self.repo / ".git" / "MERGE_HEAD").touch()
        assert_posttooluse_context(self, self.run_hook("finished"), "Git operation is in progress")

    def test_failed_rebuild_prompts_claude_with_exit_zero(self):
        assert_posttooluse_failure(self, self.run_hook("failed"), "Failed rebuild+reload of slack")

    def test_timeout_prompts_claude_with_exit_zero(self):
        result = self.run_hook("queued", ELPACA_RELOAD_TIMEOUT_SECONDS="0")
        assert_posttooluse_failure(self, result, "Timed out after 0s")

    def test_resolve_error_prompts_claude_with_exit_zero(self):
        assert_posttooluse_failure(self, self.run_hook("resolve-error"), "Failed to resolve the edited Elisp package")


class ManualHookOutputContractTests(unittest.TestCase):
    def setUp(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp_dir.cleanup)
        root = Path(self.temp_dir.name)
        self.docs = root / "docs"
        self.docs.mkdir()
        self.org = self.docs / "manual.org"
        self.org.write_text("#+title: Manual\n#+texinfo_filename: manual.info\n\n* Intro\n")
        (self.docs / "manual.info").write_text("stale\n")
        self.fake_bin = root / "bin"
        self.fake_bin.mkdir()
        for name, body in (("emacs", FAKE_EMACS), ("makeinfo", FAKE_MAKEINFO)):
            script = self.fake_bin / name
            script.write_text(body)
            script.chmod(0o755)

    def run_hook(self, hook: Path, mode: str):
        env = os.environ.copy()
        env["FAKE_MODE"] = mode
        env["PATH"] = f"{self.fake_bin}:{env['PATH']}"
        payload = {"tool_name": "Edit", "tool_input": {"file_path": str(self.org)}}
        return subprocess.run(
            ["bash", str(hook)],
            input=json.dumps(payload),
            text=True,
            capture_output=True,
            check=False,
            env=env,
        )

    def test_regeneration_reports_additional_context(self):
        for hook in MANUAL_HOOKS:
            with self.subTest(hook=hook.parents[1].name):
                result = self.run_hook(hook, "ok")
                assert_posttooluse_context(self, result, "Regenerated 1 .texi and 1 .info")
                self.assertEqual((self.docs / "manual.info").read_text(), "info\n")

    def test_export_failure_reports_additional_context(self):
        for hook in MANUAL_HOOKS:
            with self.subTest(hook=hook.parents[1].name):
                result = self.run_hook(hook, "export-fails")
                assert_posttooluse_context(self, result, "Texinfo export failed")


class HookOutputShapeStaticTests(unittest.TestCase):
    """No tracked hook may emit hookSpecificOutput without hookEventName or
    with the invalid ``message`` field.  Both copies of paired hooks are
    scanned so a fix in one tree cannot regress silently in the other."""

    def test_every_hook_specific_output_carries_event_name(self):
        offenders = []
        for hook_dir in HOOK_DIRS:
            for script in sorted(hook_dir.glob("*.sh")):
                text = script.read_text()
                if "hookSpecificOutput" not in text:
                    continue
                if "hookEventName" not in text:
                    offenders.append(f"{script.relative_to(DOTFILES)}: missing hookEventName")
                if re.search(r'"message"\s*:', text):
                    offenders.append(f"{script.relative_to(DOTFILES)}: uses invalid message field")
        self.assertEqual(offenders, [])


if __name__ == "__main__":
    unittest.main()
