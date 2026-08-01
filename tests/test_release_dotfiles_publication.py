"""Structural tests binding release-dotfiles to the guarded publication workflow.

A release publishes a branch and a tag to a public repository, so it must go
through the same two-layer review as an ordinary publication. These tests pin
that the release skills call the workflow rather than pushing on their own.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

RELEASE_SKILLS = (
    REPO_ROOT / ".claude" / "skills" / "release-dotfiles" / "SKILL.md",
    REPO_ROOT / ".codex" / "skills" / "release-dotfiles" / "SKILL.md",
)

UNSAFE_COMMANDS = (
    re.compile(r"git push\s+origin"),
    re.compile(r"git push[^\n`]*--follow-tags"),
    re.compile(r"git push[^\n`]*--tags"),
    re.compile(r"git push[^\n`]*--force"),
    re.compile(r"git push[^\n`]*-f\b"),
)


class ReleaseDotfilesPublicationTests(unittest.TestCase):
    def skills(self):
        for path in RELEASE_SKILLS:
            self.assertTrue(path.is_file(), "missing release skill: %s" % path)
            yield path, path.read_text(encoding="utf-8")

    def test_both_skills_depend_on_the_publication_workflow(self):
        for path, text in self.skills():
            self.assertIn("publish-dotfiles", text, path)
            self.assertIn("bin/dotfiles-publish", text, path)

    def test_release_mode_scan_follows_the_profile_confirmation(self):
        for path, text in self.skills():
            self.assertRegex(text, r"dotfiles-publish scan --mode release", str(path))
            self.assertIn("--release-notes", text, path)
            self.assertIn("--tag", text, path)
            profile_gate = text.find("profile tested successfully")
            release_scan = text.find("dotfiles-publish scan --mode release")
            self.assertGreater(
                release_scan,
                profile_gate,
                "%s runs the release scan before the profile gate" % path,
            )

    def test_history_repair_invalidates_the_profile_test_and_restarts_the_scan(self):
        for path, text in self.skills():
            self.assertIn("repair", text, path)
            self.assertRegex(
                text,
                r"(?i)(fresh scan|scan again|new scan|start a new run)",
                str(path),
            )
            self.assertRegex(
                text,
                r"(?i)profile (confirmation|test).{0,120}(void|invalid|again|rerun)",
                str(path),
            )

    def test_release_notes_and_tag_text_are_reviewed(self):
        for path, text in self.skills():
            self.assertIn("public-text", text, path)
            self.assertRegex(text, r"(?i)review.{0,80}release notes", str(path))

    def test_the_tag_is_created_only_after_a_clean_review(self):
        for path, text in self.skills():
            clean_review = text.find("review-status")
            tag_creation = text.find("git tag ")
            self.assertGreater(tag_creation, -1, path)
            self.assertGreater(
                tag_creation,
                clean_review,
                "%s creates the tag before the review is clean" % path,
            )

    def test_publication_is_one_authorized_branch_and_tag_push(self):
        for path, text in self.skills():
            self.assertRegex(
                text,
                r'bin/dotfiles-publish push --run "\$RUN_ID" --tag "\$NEW_VERSION"',
                str(path),
            )

    def test_the_github_release_waits_for_verified_remote_refs(self):
        for path, text in self.skills():
            push_call = text.find("dotfiles-publish push --run")
            release_call = text.find("gh release create")
            self.assertGreater(release_call, -1, path)
            self.assertGreater(
                release_call,
                push_call,
                "%s creates the GitHub release before the authorized push" % path,
            )

    def test_no_unsafe_push_command_remains(self):
        for path, text in self.skills():
            for pattern in UNSAFE_COMMANDS:
                self.assertIsNone(
                    pattern.search(text),
                    "%s still contains %s" % (path, pattern.pattern),
                )

    def test_accept_never_skips_a_security_review(self):
        for path, text in self.skills():
            self.assertRegex(
                text,
                r"(?i)`?--accept`? never skips",
                str(path),
            )
            self.assertRegex(text, r"(?i)--accept.{0,200}(security|publication) review", str(path))


if __name__ == "__main__":
    unittest.main()
