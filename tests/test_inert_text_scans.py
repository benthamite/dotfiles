"""Text-scan guards must not read prose as commands; commit parsers must not
read shell redirections as pathspecs.

Cluster 1 (2026-09-18 inbox triage): the GitHub write, Ahrefs and walk-list
guards scanned raw command text, so a commit message or a heredoc fed to a
data sink that merely named a push, the Ahrefs host or the walk-list store was
denied. They now scan text with `mask_heredoc_bodies` and
`mask_git_commit_messages` (lib-heredoc.sh) applied, exactly as the
sensitive-read guard does. Heredocs fed to a shell or interpreter, and
`sh -c '…'`, stay in the scan.

Cluster 2: commit-file-selection.py read `2>&1`, `> file` and a joined `<<EOF`
as pathspecs and denied with "no tracked files match"; ai-config-sync refused
the `-qm` short-option cluster that the selection helper accepts.
"""

from __future__ import annotations

import importlib.machinery
import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DISPATCHER = ROOT / "claude" / "hooks" / "pretooluse-bash.sh"
GITHUB = {
    "claude": ROOT / "claude" / "hooks" / "block-github-write-command.sh",
    "codex": ROOT / "codex" / "hooks" / "block-github-write-command.sh",
}
AHREFS = {
    "dispatcher": DISPATCHER,
    "claude": ROOT / "claude" / "hooks" / "block-unguarded-ahrefs-api.sh",
    "codex": ROOT / "codex" / "hooks" / "block-unguarded-ahrefs-api.sh",
}
WALK = {
    "dispatcher": DISPATCHER,
    "claude": ROOT / "claude" / "hooks" / "block-walk-list-access.sh",
    "codex": ROOT / "codex" / "hooks" / "block-walk-list-access.sh",
}
UNOWNED = "https://github.com/someone-else/not-owned.git"


def payload(name: str, command: str) -> str:
    if name == "codex":
        return json.dumps({"tool_name": "functions.exec_command", "tool_input": {"cmd": command}})
    return json.dumps({"tool_name": "Bash", "tool_input": {"command": command}})


def decision(guard: Path, name: str, command: str) -> str:
    result = subprocess.run(["bash", str(guard)], input=payload(name, command), text=True,
                            capture_output=True, check=False, cwd=ROOT)
    assert result.returncode == 0, result.stderr
    if not result.stdout.strip():
        return "allow"
    return json.loads(result.stdout)["hookSpecificOutput"].get("permissionDecision") or "allow"


class GitHubGuardIgnoresProse(unittest.TestCase):
    ALLOW = [
        "git commit -m 'automation: git push runs from CI now'",
        'git -C /tmp/somerepo commit -m "prep; then gh pr create from the branch"',
        "cat <<'EOF' > notes.md\ngh pr create --repo someone-else/not-owned --title x\ngit push origin main\nEOF",
        "git commit -F - <<'EOF'\ndocs: mention gh secret set in the runbook\nEOF",
    ]
    DENY = [
        f"git push {UNOWNED} master",
        f"bash <<'EOF'\ngit push {UNOWNED} master\nEOF",
        f"sh -c 'git push {UNOWNED} master'",
        f"git commit -m 'x' && git push {UNOWNED} master",
        "gh pr create --repo someone-else/not-owned --title x --body y",
    ]

    def test_prose_mentions_are_allowed(self):
        for name, guard in GITHUB.items():
            for command in self.ALLOW:
                with self.subTest(guard=name, command=command):
                    self.assertEqual(decision(guard, name, command), "allow")

    def test_real_writes_stay_denied(self):
        for name, guard in GITHUB.items():
            for command in self.DENY:
                with self.subTest(guard=name, command=command):
                    self.assertEqual(decision(guard, name, command), "deny")

    def test_dispatcher_agrees(self):
        for command in self.ALLOW:
            with self.subTest(command=command):
                self.assertEqual(decision(DISPATCHER, "claude", command), "allow")
        for command in self.DENY:
            with self.subTest(command=command):
                self.assertEqual(decision(DISPATCHER, "claude", command), "deny")


class AhrefsGuardIgnoresProse(unittest.TestCase):
    ALLOW = [
        "git commit -m 'route every api.ahrefs.com call through ahrefs-api-guard'",
        "cat <<'EOF' > notes.md\nThe host api.ahrefs.com is metered.\nEOF",
    ]
    DENY = [
        "curl https://api.ahrefs.com/v3/site-explorer/all-backlinks",
        "bash <<'EOF'\ncurl https://api.ahrefs.com/v3/site-explorer/all-backlinks\nEOF",
    ]

    def test_all_copies(self):
        for name, guard in AHREFS.items():
            for command in self.ALLOW:
                with self.subTest(guard=name, command=command):
                    self.assertEqual(decision(guard, "claude" if name == "dispatcher" else name, command), "allow")
            for command in self.DENY:
                with self.subTest(guard=name, command=command):
                    self.assertEqual(decision(guard, "claude" if name == "dispatcher" else name, command), "deny")


class WalkListGuardIgnoresProse(unittest.TestCase):
    ALLOW = [
        "git commit -m 'walk-list: move the store to ~/.claude/walk-list-data'",
        "cat <<'EOF' > notes.md\nItems live under .claude/walk-list-data/<uuid>.\nEOF",
    ]
    DENY = [
        "cat ~/.claude/walk-list-data/abc/input.txt",
        "bash <<'EOF'\ncat ~/.claude/walk-list-data/abc/input.txt\nEOF",
    ]

    def test_all_copies(self):
        for name, guard in WALK.items():
            for command in self.ALLOW:
                with self.subTest(guard=name, command=command):
                    self.assertEqual(decision(guard, "claude" if name == "dispatcher" else name, command), "allow")
            for command in self.DENY:
                with self.subTest(guard=name, command=command):
                    self.assertEqual(decision(guard, "claude" if name == "dispatcher" else name, command), "deny")


class CommitSelectionIgnoresRedirections(unittest.TestCase):
    HELPERS = [ROOT / "claude" / "hooks" / "commit-file-selection.py",
               ROOT / "codex" / "hooks" / "commit-file-selection.py"]

    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tempdir.cleanup)
        self.repo = Path(self.tempdir.name)
        env = {**os.environ, "GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@x",
               "GIT_COMMITTER_NAME": "t", "GIT_COMMITTER_EMAIL": "t@x"}
        subprocess.run(["git", "init", "-q", str(self.repo)], check=True)
        (self.repo / "README.org").write_text("* readme\n")
        subprocess.run(["git", "-C", str(self.repo), "add", "README.org"], check=True)
        subprocess.run(["git", "-C", str(self.repo), "commit", "-q", "-m", "init"], check=True, env=env)
        (self.repo / "README.org").write_text("* readme\nmore\n")
        subprocess.run(["git", "-C", str(self.repo), "add", "README.org"], check=True)

    def select(self, helper: Path, command: str) -> dict:
        env = {**os.environ, "COMMIT_FILE_CWD": str(self.repo)}
        env.pop("COMMIT_FILE_RECORD", None)
        result = subprocess.run(["python3", str(helper)], input=command, text=True,
                                capture_output=True, check=True, env=env, cwd=self.repo)
        return json.loads(result.stdout)

    def test_redirections_after_the_commit_are_not_pathspecs(self):
        for helper in self.HELPERS:
            for command in (
                "git commit -q -m 'x' 2>&1 | tail -1",
                "git commit -m 'x' > /dev/null",
                "git commit -m 'x' >/tmp/o 2>&1",
                "git commit -m 'x' 2>/dev/null; git log --oneline -1",
                "git commit -F - <<'EOF'\nmessage\nEOF",
                "git commit -F - <<EOF\nmessage\nEOF",
            ):
                with self.subTest(helper=helper.parent.parent.name, command=command):
                    result = self.select(helper, command)
                    self.assertNotIn("error", result, result)
                    # No pathspec: an ordinary index commit, never a
                    # "no tracked files match" failure on a redirect token.
                    self.assertEqual(result["mode"], "index")

    def test_heredoc_operator_inside_a_quoted_message_is_text(self):
        # A message that mentions <<EOF, or spans lines with an apostrophe,
        # used to swallow the rest of the command and fail with "No closing
        # quotation" (the first cluster-2 commit itself tripped it).
        for helper in self.HELPERS:
            for command in (
                'git commit -q -m "docs: accept a joined <<EOF delimiter\n\nsh -c \'…\' hid a push."',
                "git commit -m 'note: <<EOF is text here' README.org",
            ):
                with self.subTest(helper=helper.parent.parent.name, command=command):
                    result = self.select(helper, command)
                    self.assertNotIn("error", result, result)

    def test_a_real_pathspec_after_a_redirection_target_is_still_read(self):
        for helper in self.HELPERS:
            with self.subTest(helper=helper.parent.parent.name):
                result = self.select(helper, "git commit -m 'x' > /dev/null -- README.org")
                self.assertNotIn("error", result, result)
                self.assertEqual(result.get("paths"), ["README.org"])

    def test_errors_are_prefixed(self):
        for helper in self.HELPERS:
            with self.subTest(helper=helper.parent.parent.name):
                result = self.select(helper, "git commit -m 'x' -- nonexistent.txt")
                self.assertTrue(result.get("error", "").startswith("BLOCKED: "), result)


class DocUpdateGateIgnoresHeredocTextInMessages(unittest.TestCase):
    """require-doc-update.sh has its own heredoc stripper; a `<<EOF` on a
    middle line of a quoted message used to swallow the rest of the command,
    fail to lex, and be reported as staged Elisp."""

    HOOKS = {"claude": ROOT / "claude" / "hooks" / "require-doc-update.sh"}

    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tempdir.cleanup)
        self.repo = Path(self.tempdir.name)
        env = {**os.environ, "GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@x",
               "GIT_COMMITTER_NAME": "t", "GIT_COMMITTER_EMAIL": "t@x"}
        subprocess.run(["git", "init", "-q", str(self.repo)], check=True)
        (self.repo / "README.org").write_text("* readme\n")
        subprocess.run(["git", "-C", str(self.repo), "add", "README.org"], check=True)
        subprocess.run(["git", "-C", str(self.repo), "commit", "-q", "-m", "init"], check=True, env=env)
        (self.repo / "README.org").write_text("* readme\nmore\n")
        subprocess.run(["git", "-C", str(self.repo), "add", "README.org"], check=True)

    def test_multiline_message_naming_a_heredoc_operator_is_allowed(self):
        message = "docs: accept a joined <<EOF delimiter\n\nsh -c \\'...\\' hid a push; 2>&1 is not a path."
        command = f'cd "{self.repo}" && git commit -q -m "{message}" && git log --oneline -1'
        for name, hook in self.HOOKS.items():
            with self.subTest(hook=name):
                result = subprocess.run(["bash", str(hook)], input=payload(name, command), text=True,
                                        capture_output=True, check=False, cwd=self.repo)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout.strip(), "", result.stdout)


class AiConfigSyncAcceptsShortClusters(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        loader = importlib.machinery.SourceFileLoader("ai_config_sync_under_test", str(ROOT / "bin" / "ai-config-sync"))
        spec = importlib.util.spec_from_loader(loader.name, loader)
        cls.module = importlib.util.module_from_spec(spec)
        sys.modules[loader.name] = cls.module  # dataclasses resolve the defining module by name
        loader.exec_module(cls.module)

    def test_qm_cluster_matches_the_helper(self):
        for args in (["-qm", "msg"], ["-q", "-m", "msg"], ["-qmmsg"], ["-qs", "-m", "msg"]):
            with self.subTest(args=args):
                selected, problem = self.module.commit_selection(args)
                self.assertIsNone(problem, problem)
                self.assertIsNone(selected)

    def test_incomplete_cluster_is_reported(self):
        selected, problem = self.module.commit_selection(["-qm"])
        self.assertIsNone(selected)
        self.assertIn("incomplete", problem or "")


if __name__ == "__main__":
    unittest.main()
