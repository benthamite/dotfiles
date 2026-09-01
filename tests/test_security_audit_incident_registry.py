#!/usr/bin/env python3

import json
import stat
import subprocess
import tempfile
import unittest
from pathlib import Path


DOTFILES = Path(__file__).resolve().parents[1]
HELPERS = {
    "claude": DOTFILES
    / "macos/.claude/skills/security-audit/scripts/credential-incident-registry.py",
    "codex": DOTFILES
    / "macos/.codex/skills/security-audit/scripts/credential-incident-registry.py",
}
SKILLS = {
    "claude": DOTFILES / "macos/.claude/skills/security-audit/SKILL.md",
    "codex": DOTFILES / "macos/.codex/skills/security-audit/SKILL.md",
}


class CredentialIncidentRegistryTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.registry = self.root / "state/credential-incidents.json"
        self.input = self.root / "record.json"
        self.record = {
            "incident_id": "example-provider-api-2026",
            "provider": "Example Provider",
            "credential_type": "API credential",
            "credential_fingerprint": "0123456789abcdef0123",
            "finding_fingerprints": ["abcdef0123456789abcd"],
            "status": "awaiting-provider-revocation",
            "last_verified_at": "2026-09-01T12:00:00Z",
            "last_verified_result": "accepted",
            "verification_method": "provider-api",
            "provider_references": [
                {"kind": "provider-ticket", "id": "1234567"},
                {"kind": "gmail-message", "id": "1a05cded5f3b3816"},
            ],
            "locations": [
                {
                    "path": "config/service.json",
                    "commits": ["a" * 40],
                    "finding_fingerprints": ["abcdef0123456789abcd"],
                }
            ],
            "next_action": "await-provider",
            "summary": "The provider endpoint accepted the historical credential.",
        }

    def write_input(self, record=None, mode=0o600):
        self.input.write_text(json.dumps(record or self.record), encoding="utf-8")
        self.input.chmod(mode)

    def run_helper(self, *arguments, check=True):
        command = [
            str(HELPERS["codex"]),
            "--registry",
            str(self.registry),
            *arguments,
        ]
        return subprocess.run(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=check,
        )

    def test_paired_helpers_and_instructions_are_identical(self):
        self.assertEqual(HELPERS["claude"].read_bytes(), HELPERS["codex"].read_bytes())
        self.assertEqual(SKILLS["claude"].read_text(), SKILLS["codex"].read_text())
        for path in HELPERS.values():
            self.assertTrue(path.stat().st_mode & stat.S_IXUSR)
        for path in SKILLS.values():
            text = path.read_text()
            self.assertIn("scripts/credential-incident-registry.py", text)
            self.assertIn("A registry record is evidence,\nnot an allowlist", text)

    def test_record_list_lookup_and_validate(self):
        self.write_input()
        recorded = self.run_helper("record", "--input", str(self.input))
        self.assertEqual("recorded: example-provider-api-2026\n", recorded.stdout)
        self.assertEqual(0o600, stat.S_IMODE(self.registry.stat().st_mode))
        self.assertEqual(0o700, stat.S_IMODE(self.registry.parent.stat().st_mode))

        listed = self.run_helper("list").stdout
        self.assertIn("example-provider-api-2026\tExample Provider", listed)
        self.assertIn("awaiting-provider-revocation", listed)
        self.assertNotIn("historical credential", listed)

        found = self.run_helper(
            "lookup", "--fingerprint", "abcdef0123456789abcd"
        ).stdout
        payload = json.loads(found)
        self.assertEqual("example-provider-api-2026", payload["incident_id"])
        self.assertNotIn("secret", found.lower())

        validated = self.run_helper("validate").stdout
        self.assertIn("(1 incidents)", validated)

    def test_unknown_fingerprint_is_a_clean_miss(self):
        result = self.run_helper(
            "lookup", "--fingerprint", "ffffffffffffffffffff", check=False
        )
        self.assertEqual(1, result.returncode)
        self.assertEqual("", result.stdout)
        self.assertEqual("", result.stderr)

    def test_record_rejects_secret_bearing_or_unknown_fields(self):
        exposed = dict(self.record)
        exposed["summary"] = "token=abcdefghijklmnopqrstuvwxyz1234567890"
        self.write_input(exposed)
        result = self.run_helper("record", "--input", str(self.input), check=False)
        self.assertEqual(2, result.returncode)
        self.assertIn("appears to contain credential material", result.stderr)
        self.assertFalse(self.registry.exists())

        unlabelled = dict(self.record)
        unlabelled["summary"] = "abcdefghijklmnopqrstuvwxyz1234567890"
        self.write_input(unlabelled)
        result = self.run_helper("record", "--input", str(self.input), check=False)
        self.assertEqual(2, result.returncode)
        self.assertIn("appears to contain credential material", result.stderr)
        self.assertFalse(self.registry.exists())

        unknown = dict(self.record)
        unknown["raw_value"] = "redacted"
        self.write_input(unknown)
        result = self.run_helper("record", "--input", str(self.input), check=False)
        self.assertEqual(2, result.returncode)
        self.assertIn("unsupported fields", result.stderr)
        self.assertFalse(self.registry.exists())

    def test_record_requires_private_input_and_private_existing_state(self):
        self.write_input(mode=0o644)
        result = self.run_helper("record", "--input", str(self.input), check=False)
        self.assertEqual(2, result.returncode)
        self.assertIn("record input is group- or world-accessible", result.stderr)

        self.write_input(mode=0o600)
        self.run_helper("record", "--input", str(self.input))
        self.registry.chmod(0o644)
        result = self.run_helper("validate", check=False)
        self.assertEqual(2, result.returncode)
        self.assertIn("registry is group- or world-accessible", result.stderr)

    def test_registry_symlink_is_refused(self):
        self.write_input()
        self.run_helper("record", "--input", str(self.input))
        linked = self.root / "linked-registry.json"
        linked.symlink_to(self.registry)
        self.registry = linked
        result = self.run_helper("validate", check=False)
        self.assertEqual(2, result.returncode)
        self.assertIn("registry is a symlink", result.stderr)


if __name__ == "__main__":
    unittest.main()
