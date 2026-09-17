"""Classify synthetic edits through the real guard; never apply their patches."""

import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
GUARD = ROOT / "codex/hooks/block-secret-leak.sh"
MARKER = "ghp_" + "A" * 36  # Deliberately fake credential-shaped input.
PUBLIC = "/fixture/public.txt"
EXEMPT = "/fixture/.env.local"


def patch(*sections):
    return "*** Begin Patch\n" + "\n".join(sections) + "\n*** End Patch"


def add(destination, content):
    return "*** Add File: " + destination + "\n+" + content


def update(source, content="ordinary", destination=None):
    move = "\n*** Move to: " + destination if destination else ""
    return "*** Update File: " + source + move + "\n@@\n-old\n+" + content


class SecretPatchGuardTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="secret-patch-guard-")
        self.addCleanup(self.temporary.cleanup)
        self.directory = Path(self.temporary.name)
        self.directory.chmod(0o700)

    def classify(self, tool_input, *, tool="apply_patch", guard=GUARD):
        payload = json.dumps({"tool_name": tool, "tool_input": tool_input})
        env = {
            "PATH": os.defpath + ":/opt/homebrew/bin:/usr/local/bin",
            "HOME": str(self.directory), "LANG": "C", "PYTHONDONTWRITEBYTECODE": "1",
        }
        result = subprocess.run(
            ["/bin/bash", str(guard)], input=payload, capture_output=True,
            text=True, cwd=self.directory, env=env, timeout=15,
        )
        self.assertNotIn(MARKER, result.stdout + result.stderr)
        self.assertIn(result.returncode, (0, 2))
        if result.returncode == 2:
            return "deny"
        if not result.stdout.strip():
            return "allow"
        return json.loads(result.stdout)["hookSpecificOutput"]["permissionDecision"]

    def assert_patch(self, text, expected, **extra):
        self.assertEqual(self.classify({"command": text, **extra}), expected)

    def test_mixed_destinations_cannot_exempt_public_secret_in_either_order(self):
        for sensitive in (
            "/fixture/.zshenv-secrets", "/fixture/.env.op", EXEMPT,
            "/fixture/.password-store/item",
        ):
            sections = (add(sensitive, "ordinary"), add(PUBLIC, MARKER))
            for ordered in (sections, sections[::-1]):
                with self.subTest(destination=sensitive, reverse=ordered != sections):
                    self.assert_patch(patch(*ordered), "deny")

    def test_exempt_deletion_does_not_exempt_public_add_or_update(self):
        deletion = "*** Delete File: " + EXEMPT
        for public in (add(PUBLIC, MARKER), update(PUBLIC, MARKER)):
            self.assert_patch(patch(deletion, public), "deny")
            self.assert_patch(patch(public, deletion), "deny")

    def test_exempt_secrets_do_not_block_ordinary_changes(self):
        for sensitive in (add(EXEMPT, MARKER), update(EXEMPT, MARKER)):
            for public in (add(PUBLIC, "ordinary"), update(PUBLIC)):
                self.assert_patch(patch(sensitive, public), "allow")
                self.assert_patch(patch(public, sensitive), "allow")

    def test_move_destinations_control_exemption(self):
        self.assert_patch(patch(update(PUBLIC, MARKER, EXEMPT)), "allow")
        self.assert_patch(patch(update(EXEMPT, MARKER, "/fixture/other.env.local")), "allow")
        self.assert_patch(patch(update(PUBLIC, MARKER, "/fixture/renamed.txt")), "deny")
        self.assert_patch(patch(update(PUBLIC, "ordinary", "/fixture/renamed.txt")), "allow")

    def test_move_out_of_secret_store_denies_unseen_unchanged_content(self):
        self.assert_patch(patch(update(EXEMPT, "ordinary", PUBLIC)), "deny")
        self.assert_patch(patch("*** Update File: " + EXEMPT + "\n*** Move to: " + PUBLIC), "deny")
        for whitespace in (" ", "\t", "\u00a0", "\u2003"):
            self.assert_patch(patch(update(EXEMPT + whitespace, "ordinary", PUBLIC)), "deny")

    def test_exempt_move_does_not_exempt_another_public_file(self):
        moved = update(EXEMPT, "ordinary", "/fixture/other.env.local")
        self.assert_patch(patch(moved, add(PUBLIC, MARKER)), "deny")

    def test_public_context_and_removed_lines_keep_existing_scan_policy(self):
        for prefix in (" ", "-"):
            ordinary = "*** Update File: " + PUBLIC + "\n@@\n" + prefix + MARKER + "\n+ordinary"
            self.assert_patch(patch(add(EXEMPT, "ordinary"), ordinary), "deny")

    def test_content_cannot_impersonate_an_exempt_file_header(self):
        body = "*** Add File: " + EXEMPT + "\n+" + MARKER
        self.assert_patch(patch(add(PUBLIC, body)), "deny")
        context = "*** Update File: " + PUBLIC + "\n@@\n *** Update File: " + EXEMPT
        self.assert_patch(patch(context + "\n+" + MARKER), "deny")

    def test_native_patch_whitespace_remains_accepted(self):
        ordinary = patch(add(PUBLIC, "ordinary"))
        for text in ("\n\t " + ordinary + " \t\n", ordinary.replace("\n", "\r\n"),
                     ordinary.replace("*** Begin Patch", "*** Begin Patch \t"),
                     ordinary.replace("public.txt", "public.txt \t"),
                     patch("  " + add(PUBLIC, "ordinary"))):
            self.assert_patch(text, "allow")
        self.assert_patch(patch("*** Update File: " + PUBLIC + "\n@@\n\n+ordinary\n*** End of File \t\n"), "allow")
        self.assert_patch(patch(update(PUBLIC)).replace("*** End Patch", "  *** End Patch"), "allow")

    def test_incidental_exempt_path_cannot_override_patch_destinations(self):
        self.assert_patch(patch(add(PUBLIC, MARKER)), "deny", file_path=EXEMPT)

    def test_all_supported_payload_fields_and_string_forms(self):
        text = patch(add(EXEMPT, "ordinary"), add(PUBLIC, MARKER))
        for field in ("patch", "command", "input", "text"):
            self.assertEqual(self.classify({field: text}), "deny")
        for value in (text, json.dumps({"command": text})):
            self.assertEqual(self.classify(value), "deny")

    def test_malformed_patch_cannot_silently_drop_content(self):
        for text in (
            "ordinary", patch(add(EXEMPT, "ordinary")) + "\ntrailing",
            "*** Begin Patch\n" + add(EXEMPT, "ordinary"),
            patch("*** Move to: " + EXEMPT),
            patch("*** Delete File: " + EXEMPT + "\n+ordinary"),
            patch(add(EXEMPT, "ordinary") + "\n*** Move to: " + PUBLIC),
        ):
            self.assert_patch(text, "deny")

    def test_native_single_file_exceptions_stay_scoped_to_the_destination(self):
        for tool, field in (("Write", "content"), ("Edit", "new_string")):
            for destination, expected in ((PUBLIC, "deny"), (EXEMPT, "allow")):
                self.assertEqual(self.classify({"file_path": destination, field: MARKER}, tool=tool), expected)
            content = patch(add(EXEMPT, MARKER))
            self.assertEqual(self.classify({"file_path": PUBLIC, field: content}, tool=tool), "deny")
        # Claude has one destination per Write and needs no patch parser.
        claude = ROOT / "claude/hooks/block-secret-leak.sh"
        for destination, expected in ((PUBLIC, "deny"), (EXEMPT, "allow")):
            self.assertEqual(self.classify({"file_path": destination, "content": MARKER},
                                           tool="Write", guard=claude), expected)


if __name__ == "__main__":
    unittest.main()
