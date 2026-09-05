#!/usr/bin/env python3

import copy
import json
import stat
import subprocess
import sys
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
        self.input.write_text(json.dumps(self.record if record is None else record), encoding="utf-8")
        self.input.chmod(mode)

    def run_helper(self, *arguments, check=True):
        command = [
            sys.executable,
            "-B",
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

    def write_state(self, record):
        record = copy.deepcopy(record)
        incident_id = record.pop("incident_id")
        record["updated_at"] = "2026-09-01T12:00:00Z"
        self.registry.parent.mkdir(mode=0o700, exist_ok=True)
        self.registry.write_text(
            json.dumps({"schema": 1, "incidents": {incident_id: record}}), encoding="utf-8"
        )
        self.registry.chmod(0o600)

    def assert_safe_rejection(self, result, forbidden=()):
        self.assertEqual(2, result.returncode)
        self.assertEqual("", result.stdout)
        self.assertTrue(result.stderr.startswith("error: "), result.stderr)
        self.assertNotIn("Traceback", result.stderr)
        for value in forbidden:
            self.assertNotIn(value, result.stdout + result.stderr)

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


    def test_record_rejects_credentials_in_metadata_without_writing_or_echoing(self):
        marker = "ghp_" + "A" * 36
        slack_marker = "xoxb-" + "synthetic-credential"
        cases = []
        for field in ("provider", "credential_type", "summary", "status", "last_verified_at"):
            record = copy.deepcopy(self.record)
            record[field] = marker
            cases.append((field, record))
        for field in ("id", "kind"):
            record = copy.deepcopy(self.record)
            record["provider_references"][0][field] = marker
            cases.append(("reference " + field, record))
        for value in (marker, "archive/" + marker + "/config.json", "token=" + marker):
            record = copy.deepcopy(self.record)
            record["locations"][0]["path"] = value
            cases.append(("path " + str(len(value)), record))
        for field in ("incident_id", "next_action", "last_verified_result", "verification_method"):
            record = copy.deepcopy(self.record)
            record[field] = marker
            cases.append((field, record))
        record = copy.deepcopy(self.record)
        record[marker] = "redacted"
        cases.append(("unknown field", record))
        record = copy.deepcopy(self.record)
        record["incident_id"] = slack_marker
        cases.append(("valid-shaped secret incident id", record))
        for name, record in cases:
            with self.subTest(name=name):
                self.write_input(record)
                result = self.run_helper("record", "--input", str(self.input), check=False)
                self.assert_safe_rejection(result, (marker, slack_marker))
                self.assertFalse(self.registry.parent.exists())

    def test_unlabelled_reference_and_path_tokens_are_rejected(self):
        marker = "SyntheticUnlabelledCredential" + "Z" * 12
        for field in ("reference", "path"):
            with self.subTest(field=field):
                record = copy.deepcopy(self.record)
                if field == "reference":
                    record["provider_references"][0]["id"] = marker
                else:
                    record["locations"][0]["path"] = "config/" + marker
                self.write_input(record)
                self.assert_safe_rejection(
                    self.run_helper("record", "--input", str(self.input), check=False), (marker,)
                )
                self.assertFalse(self.registry.exists())

    def test_structured_ids_and_realistic_git_paths_round_trip(self):
        self.record["provider_references"] += [
            {"kind": "provider-ticket", "id": "123e4567-e89b-12d3-a456-426614174000"},
            {"kind": "provider-ticket", "id": "abcdef0123456789" * 2},
        ]
        self.record["locations"] += [
            {"path": "macos/.codex/skills/security-audit/scripts/credential-incident-registry.py"},
            {"path": "archive/" + "a" * 40 + "/config/service.json"},
        ]
        self.write_input()
        self.run_helper("record", "--input", str(self.input))
        actual = json.loads(self.run_helper("show", "--id", self.record["incident_id"]).stdout)
        self.assertEqual(self.record["provider_references"], actual["provider_references"])
        self.assertEqual(self.record["locations"], actual["locations"])

    def test_malformed_registry_is_rejected_before_any_output_or_write(self):
        marker = "ghp_" + "B" * 36
        cases = []
        for field in ("id", "kind"):
            record = copy.deepcopy(self.record)
            record["provider_references"][0][field] = marker
            cases.append(record)
        record = copy.deepcopy(self.record)
        record["locations"][0]["path"] = "config/" + marker
        cases.append(record)
        record = copy.deepcopy(self.record)
        record[marker] = "redacted"
        cases.append(record)
        for kind in ([], {}, None, 42):
            record = copy.deepcopy(self.record)
            record["provider_references"][0]["kind"] = kind
            cases.append(record)
        for index, record in enumerate(cases):
            self.write_state(record)
            before = self.registry.read_bytes()
            modified = self.registry.stat().st_mtime_ns
            for arguments in (("list",), ("lookup", "--fingerprint", "0123456789abcdef0123")):
                with self.subTest(record=index, command=arguments[0]):
                    self.assert_safe_rejection(self.run_helper(*arguments, check=False), (marker,))
                    self.assertEqual(before, self.registry.read_bytes())
                    self.assertEqual(modified, self.registry.stat().st_mtime_ns)

    def test_valid_and_missing_registry_reads_do_not_change_state(self):
        commands = (("list",), ("lookup", "--fingerprint", "0123456789abcdef0123"))
        for arguments in commands:
            self.run_helper(*arguments, check=False)
            self.assertFalse(self.registry.parent.exists())
        self.write_state(self.record)
        before = self.registry.read_bytes()
        modified = self.registry.stat().st_mtime_ns
        for arguments in commands:
            self.run_helper(*arguments)
            self.assertEqual(before, self.registry.read_bytes())
            self.assertEqual(modified, self.registry.stat().st_mtime_ns)

    def test_rejected_update_preserves_existing_registry(self):
        self.write_state(self.record)
        before = self.registry.read_bytes()
        self.record["provider_references"][0]["id"] = "ghp_" + "C" * 36
        self.write_input()
        self.assert_safe_rejection(
            self.run_helper("record", "--input", str(self.input), check=False),
            (self.record["provider_references"][0]["id"],),
        )
        self.assertEqual(before, self.registry.read_bytes())

    def test_errors_do_not_echo_unknown_ids_or_filesystem_paths(self):
        marker = "ghp_" + "D" * 36
        self.assert_safe_rejection(
            self.run_helper("show", "--id", marker, check=False), (marker,)
        )
        self.assert_safe_rejection(
            self.run_helper("record", "--input", str(self.root / marker), check=False), (marker,)
        )

    def test_malformed_json_and_schema_fail_cleanly(self):
        self.registry.parent.mkdir(mode=0o700)
        for content in ("{", "[]", '{"schema": true, "incidents": {}}'):
            self.registry.write_text(content, encoding="utf-8")
            self.registry.chmod(0o600)
            self.assert_safe_rejection(self.run_helper("list", check=False))
            self.assertEqual(content, self.registry.read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
