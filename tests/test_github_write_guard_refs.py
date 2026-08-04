"""Ref-level behavior of the GitHub write guards, on a repo with no write grant.

The rule: gate writes to refs other people consume, not writes to the repo.
Creating a topic branch and opening a pull request against it are how you
contribute to a repo you do not own, they are reviewable, and they are
reversible. Landing on the default branch, rewriting history, deleting refs and
merging are none of those things, so they stay gated by the allowlist.

Repo-level allowlisting is tested separately, in test_github_write_guard.py.

Note on the fixture: the existing parity harness runs the guard with
``cwd=ROOT``, and dotfiles is itself an allowlisted repo, so a bare
``git push origin main`` there is allowed for reasons that have nothing to do
with the ref. These tests run inside a throwaway repo whose ``origin`` points
at a chosen target, which is the only way a "denied" expectation here means
what it says. The fixture is a mixin rather than a base test class on purpose:
subclassing a TestCase to change the remote also inherits its expectations,
and pytest does not honor ``load_tests``, so the inherited deny cases would run
against the allowlisted repo and fail.
"""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

# This is the only test module here that imports another one, and pytest run
# from the repo root does not put tests/ on the path. Reusing the parity
# harness beats duplicating it: GUARDS is where the hook paths are defined, and
# a second copy in a security test is a copy that can drift.
sys.path.insert(0, str(Path(__file__).resolve().parent))

from test_github_write_guard import GUARDS, decision, payload  # noqa: E402

# A repo that cannot become allowlisted by accident. Using a real one here
# would silently turn these deny cases green the day it gets declared on a
# project, which is the failure mode this whole file exists to catch.
UNPRIVILEGED_REMOTE = "https://github.com/example/unowned.git"
UNPRIVILEGED_REPO = "example/unowned"
ALLOWLISTED_REMOTE = "https://github.com/benthamite/dotfiles.git"


class GuardInRepoMixin:
    """Run the guards inside a temp checkout whose origin is ``REMOTE``."""

    REMOTE = UNPRIVILEGED_REMOTE

    def setUp(self) -> None:
        super().setUp()
        self.tempdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tempdir.cleanup)
        self.bin_dir = Path(self.tempdir.name)
        self.repo = self.bin_dir / "checkout"
        self.repo.mkdir()
        for args in (
            ["init", "-q", "-b", "main"],
            ["remote", "add", "origin", self.REMOTE],
        ):
            subprocess.run(
                ["git", "-C", str(self.repo), *args], check=True, capture_output=True
            )

        # A fake gh on PATH that records any invocation: the guard must reach a
        # verdict without shelling out to the real one.
        self.gh_log = self.bin_dir / "gh-args"
        fake_gh = self.bin_dir / "gh"
        fake_gh.write_text(
            "#!/usr/bin/env bash\nset -eu\n"
            'printf \'called\\n\' >> "$FAKE_GH_LOG"\nexit 1\n',
            encoding="utf-8",
        )
        os.chmod(fake_gh, 0o755)

    def assert_both(self, command: str, *, expected: str) -> None:
        for tool in GUARDS:
            with self.subTest(tool=tool, command=command):
                if self.gh_log.exists():
                    self.gh_log.unlink()
                env = os.environ.copy()
                env["PATH"] = os.pathsep.join((str(self.bin_dir), env["PATH"]))
                env["FAKE_GH_LOG"] = str(self.gh_log)
                result = subprocess.run(
                    ["bash", str(GUARDS[tool])],
                    input=payload(tool, command),
                    capture_output=True,
                    text=True,
                    check=True,
                    cwd=self.repo,
                    env=env,
                )
                self.assertEqual(decision(result), expected)


class UnprivilegedRepoRefRulesTests(GuardInRepoMixin, unittest.TestCase):
    REMOTE = UNPRIVILEGED_REMOTE

    # --- what the change permits (these three currently fail) ---

    def test_creating_a_topic_branch_is_allowed(self) -> None:
        # The case that motivated the change: contributing to another team's
        # repo by PR must not need a standing write grant on that repo.
        self.assert_both(
            "git push -u origin my-feature-branch",
            expected="allow",
        )

    def test_explicit_refspec_to_a_topic_branch_is_allowed(self) -> None:
        self.assert_both("git push origin HEAD:my-topic-branch", expected="allow")

    def test_opening_a_pull_request_is_allowed(self) -> None:
        # Allowing the push but gating the PR would leave the friction where it
        # was. Reviewer notification is governed by the separate
        # ask-before-externally-visible rule, not by this guard.
        self.assert_both(
            f"gh pr create --draft --repo {UNPRIVILEGED_REPO} --title t --body b",
            expected="allow",
        )

    def test_editing_a_pull_request_body_is_allowed(self) -> None:
        # Several repos require the description to be completed after the fact —
        # epoch-website-astro wants Cloudflare preview links filled in once the
        # build lands. Gating this makes their own required workflow impossible
        # to finish and leaves the branch push pointless.
        self.assert_both(
            f"gh pr edit 1177 --repo {UNPRIVILEGED_REPO} --body-file body.md",
            expected="allow",
        )

    # --- what must keep being denied ---

    def test_pushing_the_default_branch_is_still_denied(self) -> None:
        for command in (
            "git push origin main",
            "git push origin HEAD:main",
            "git push origin master",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="deny")

    def test_a_bare_push_is_denied_because_its_target_is_implicit(self) -> None:
        # `git push` with no refspec follows push.default and the branch's
        # upstream, so the command alone does not say whether it lands on a
        # topic branch or on main. Uncertainty resolves to deny.
        self.assert_both("git push", expected="deny")

    def test_force_push_is_denied_even_on_a_topic_branch(self) -> None:
        # Refinement 1. Force-pushing destroys commits that may not be ours:
        # branch names collide, and "a branch I created" is not something the
        # guard can verify from the command. Non-fast-forward is the real line,
        # not the ref name. --force-with-lease is safer but still a rewrite.
        for flag in ("--force", "-f", "--force-with-lease"):
            with self.subTest(flag=flag):
                self.assert_both(
                    f"git push {flag} origin my-topic-branch", expected="deny"
                )

    def test_deleting_a_remote_ref_is_denied(self) -> None:
        # Refinement 2. Deletion is not PR-shaped, and the remote cannot undo it.
        for command in (
            "git push origin --delete my-topic-branch",
            "git push origin :my-topic-branch",
        ):
            with self.subTest(command=command):
                self.assert_both(command, expected="deny")

    def test_merging_a_pull_request_is_still_denied(self) -> None:
        # The sharpest line in the change: a merge writes to the default
        # branch, so "PR work is fine" must not be read as covering it.
        self.assert_both(
            f"gh pr merge 1176 --repo {UNPRIVILEGED_REPO}", expected="deny"
        )

    def test_pushing_a_deploy_branch_is_denied(self) -> None:
        # Not every consumed branch is called main; repos deploy from these.
        for ref in ("release/2026-08", "staging", "production", "gh-pages"):
            with self.subTest(ref=ref):
                self.assert_both(f"git push origin {ref}", expected="deny")

    def test_mirror_and_all_pushes_are_denied(self) -> None:
        # Each expands to refs the command never names, including main.
        for flag in ("--mirror", "--all", "--tags"):
            with self.subTest(flag=flag):
                self.assert_both(f"git push {flag} origin", expected="deny")


class AllowlistedRepoUnchangedTests(GuardInRepoMixin, unittest.TestCase):
    """Loosening the ref rules must not narrow an allowlisted repo's access."""

    REMOTE = ALLOWLISTED_REMOTE

    def test_default_branch_push_stays_allowed(self) -> None:
        self.assert_both("git push origin main", expected="allow")

    def test_force_push_stays_allowed(self) -> None:
        self.assert_both("git push --force origin main", expected="allow")

    def test_delete_stays_allowed(self) -> None:
        self.assert_both("git push origin --delete old-branch", expected="allow")


if __name__ == "__main__":
    unittest.main()
