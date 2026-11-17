#!/usr/bin/env python3

import copy
import importlib.util
import json
import os
import stat
import subprocess
import sys
import tempfile
import time
import unittest
from unittest import mock
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
        self.temporary = tempfile.TemporaryDirectory(prefix="current64-registry-tests-",
                                                     dir="/private/tmp" if sys.platform == "darwin" else "/tmp")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.registry = self.root / "state/credential-incidents.json"
        self.input = self.root / "record.json"
        self.environment = {"PATH": "/usr/bin:/bin:/usr/sbin:/sbin", "LC_ALL": "C",
                            "PYTHONDONTWRITEBYTECODE": "1", "GIT_CONFIG_NOSYSTEM": "1",
                            "GIT_CONFIG_GLOBAL": os.devnull, "XDG_STATE_HOME": str(self.root / "global-state")}
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
            "-I",
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
            cwd=self.root,
            env=self.environment,
            timeout=20,
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

    def accepted_risk_record(self):
        record = copy.deepcopy(self.record)
        record.update(status="accepted-risk", next_action="none", risk_acceptance={
            "accepted_at": "2026-09-02T12:00:00.123456+00:00",
            "authorization": "The owner explicitly accepted this historical exposure.",
            "rationale": "The remaining access is limited to synthetic fixture data.",
        })
        return record

    def test_accepted_risk_round_trip_preserves_evidence_and_validity_result(self):
        record = self.accepted_risk_record()
        self.write_input(record)
        self.run_helper("record", "--input", str(self.input))
        actual = json.loads(self.run_helper("lookup", "--fingerprint",
                                           record["credential_fingerprint"]).stdout)
        self.assertEqual(record["risk_acceptance"], actual["risk_acceptance"])
        self.assertEqual("accepted-risk", actual["status"])
        self.assertEqual("accepted", actual["last_verified_result"])
        self.assertEqual("none", actual["next_action"])
        self.assertIn("accepted-risk", self.run_helper("list").stdout)
        self.run_helper("validate")

    def test_existing_acceptance_formats_are_read_without_rewriting_evidence(self):
        structured = self.accepted_risk_record()
        legacy = copy.deepcopy(structured)
        legacy["authorization"] = legacy.pop("risk_acceptance")["authorization"]
        legacy["next_action"] = "No further revocation requested by the owner."
        for record in (structured, legacy):
            with self.subTest(legacy="authorization" in record):
                self.write_state(record)
                state = json.loads(self.registry.read_text())
                state["incidents"][record["incident_id"]]["updated_at"] = (
                    "2026-09-02T12:00:00.123456+00:00")
                self.registry.write_text(json.dumps(state))
                before = (self.registry.read_bytes(), self.registry.stat().st_mtime_ns,
                          self.registry.stat().st_mode)
                for command in ("list", "validate"):
                    self.run_helper(command)
                actual = json.loads(self.run_helper("show", "--id", record["incident_id"]).stdout)
                found = json.loads(self.run_helper("lookup", "--fingerprint",
                                                  record["credential_fingerprint"]).stdout)
                self.assertEqual({"incident_id": record["incident_id"],
                                  **state["incidents"][record["incident_id"]]}, actual)
                self.assertEqual(actual, found)
                self.assertEqual(before, (self.registry.read_bytes(), self.registry.stat().st_mtime_ns,
                                         self.registry.stat().st_mode))
                self.assertEqual([self.registry.name], [p.name for p in self.registry.parent.iterdir()])

    def test_recording_another_incident_preserves_legacy_acceptance(self):
        legacy = self.accepted_risk_record()
        legacy["authorization"] = legacy.pop("risk_acceptance")["authorization"]
        legacy["next_action"] = "The owner accepted the historical exposure."
        self.write_state(legacy)
        before = json.loads(self.registry.read_bytes())["incidents"][legacy["incident_id"]]
        other = copy.deepcopy(self.record)
        other["incident_id"] = "example-independent-incident"
        other["credential_fingerprint"] = "f" * 20
        self.write_input(other)
        self.run_helper("record", "--input", str(self.input))
        state = json.loads(self.registry.read_bytes())
        self.assertEqual(before, state["incidents"][legacy["incident_id"]])
        self.assertEqual(2, len(state["incidents"]))

    def test_new_acceptances_require_structured_evidence_and_enum_next_action(self):
        legacy = self.accepted_risk_record()
        legacy["authorization"] = legacy.pop("risk_acceptance")["authorization"]
        cases = [legacy]
        missing = self.accepted_risk_record()
        missing.pop("risk_acceptance")
        cases.append(missing)
        prose_action = self.accepted_risk_record()
        prose_action["next_action"] = "No further action requested."
        cases.append(prose_action)
        mixed = self.accepted_risk_record()
        mixed["authorization"] = "The owner accepted this historical exposure."
        cases.append(mixed)
        for record in cases:
            with self.subTest(fields=sorted(record)):
                self.write_input(record)
                self.assert_safe_rejection(self.run_helper("record", "--input", str(self.input), check=False))
                self.assertFalse(self.registry.exists())

    def test_invalid_acceptance_evidence_is_rejected_on_reads_and_writes(self):
        marker = "ghp_" + "Q" * 36
        valid = self.accepted_risk_record()
        cases = []
        for value in (None, [], {}, {**valid["risk_acceptance"], "extra": "unexpected"}):
            record = copy.deepcopy(valid)
            record["risk_acceptance"] = value
            cases.append(record)
        for field in ("authorization", "rationale"):
            for value in ("", " ", 42, "x" * 401, marker, "evidence\nwith controls"):
                record = copy.deepcopy(valid)
                record["risk_acceptance"][field] = value
                cases.append(record)
        for value in ("2026-02-30T12:00:00Z", "2026-09-02T12:00:00+03:00",
                      "2026-09-02T12:00:00-00:00", "2026-09-02T12:00:00",
                      "2026-09-02T12:00:00.1234567Z", "2026-09-02T12:00:00.Z"):
            record = copy.deepcopy(valid)
            record["risk_acceptance"]["accepted_at"] = value
            cases.append(record)
        legacy = copy.deepcopy(valid)
        legacy["authorization"] = marker
        legacy.pop("risk_acceptance")
        cases.append(legacy)
        legacy_action = copy.deepcopy(legacy)
        legacy_action["authorization"] = "Owner accepted this exposure."
        legacy_action["next_action"] = marker
        cases.append(legacy_action)
        for index, record in enumerate(cases):
            with self.subTest(case=index):
                self.write_state(record)
                before = self.registry.read_bytes()
                self.assert_safe_rejection(self.run_helper("list", check=False), (marker,))
                self.write_input(record)
                self.assert_safe_rejection(self.run_helper("record", "--input", str(self.input), check=False),
                                           (marker,))
                self.assertEqual(before, self.registry.read_bytes())

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

    def import_helper(self):
        spec = importlib.util.spec_from_file_location("owned_registry_helper", HELPERS["codex"])
        helper = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(helper)
        return helper

    def run_scoped(self, *arguments, environment=None):
        return subprocess.run([sys.executable, "-I", "-B", str(HELPERS["codex"]), *map(str, arguments)],
                              cwd=self.root, env=environment or self.environment,
                              capture_output=True, text=True, timeout=20)

    def test_both_public_clis_work_from_an_unrelated_directory(self):
        self.write_input()
        for runtime, helper in HELPERS.items():
            selected = self.root / runtime / "credential-incidents.json"
            for args in (("record", "--input", str(self.input)), ("validate",), ("list",)):
                with self.subTest(runtime=runtime, command=args[0]):
                    result = subprocess.run([str(helper), "--registry", str(selected), *args],
                                            cwd=self.root, env=self.environment, capture_output=True, text=True, timeout=20)
                    self.assertEqual(0, result.returncode, result.stderr)
        self.assertEqual(self.record, json.loads(self.input.read_text()))

    def test_location_fingerprints_participate_in_lookup(self):
        self.record["finding_fingerprints"] = []
        self.write_state(self.record)
        result = self.run_helper("lookup", "--fingerprint", "abcdef0123456789abcd")
        self.assertEqual(self.record["incident_id"], json.loads(result.stdout)["incident_id"])

    def test_lookup_preserves_all_matching_records_as_a_json_object_stream(self):
        self.write_input()
        self.run_helper("record", "--input", str(self.input))
        self.record["incident_id"] = "owned-another-incident"
        self.write_input()
        self.run_helper("record", "--input", str(self.input))
        output = self.run_helper("lookup", "--fingerprint", "0123456789abcdef0123").stdout
        decoder = json.JSONDecoder()
        records = []
        while output.strip():
            record, end = decoder.raw_decode(output.lstrip())
            records.append(record)
            output = output.lstrip()[end:]
        self.assertEqual({"example-provider-api-2026", "owned-another-incident"},
                         {record["incident_id"] for record in records})

    def test_invalid_calendar_values_are_rejected(self):
        for value in ("2026-02-30", "2026-02-30T12:00:00Z", "2026-13-01T12:00:00Z", "2026-01-01T25:00:00Z"):
            with self.subTest(value=value):
                self.record["last_verified_at"] = value
                self.write_input()
                self.assert_safe_rejection(self.run_helper("record", "--input", str(self.input), check=False))
                self.assertFalse(self.registry.parent.exists())

    def test_duplicate_json_fields_and_deep_json_are_rejected(self):
        self.registry.parent.mkdir(mode=0o700)
        for content in ('{"schema":1,"schema":1,"incidents":{}}', '[' * 2000 + ']' * 2000):
            self.registry.write_text(content)
            self.registry.chmod(0o600)
            self.assert_safe_rejection(self.run_helper("list", check=False))
            self.assertEqual(content, self.registry.read_text())
        self.write_input()
        self.input.write_text(self.input.read_text().replace('"provider":', '"provider":"First", "provider":', 1))
        self.assert_safe_rejection(self.run_helper("record", "--input", str(self.input), check=False))

    def test_special_and_oversized_input_or_registry_is_refused_without_blocking(self):
        self.registry.parent.mkdir(mode=0o700)
        for path, arguments, limit in ((self.input, ("record", "--input", str(self.input)), 256 * 1024),
                                       (self.registry, ("list",), 8 * 1024 * 1024)):
            with self.subTest(path=path.name):
                os.mkfifo(path, 0o600)
                self.assert_safe_rejection(self.run_helper(*arguments, check=False))
                path.unlink()
                with path.open("wb") as stream:
                    stream.truncate(limit + 1)
                path.chmod(0o600)
                self.assert_safe_rejection(self.run_helper(*arguments, check=False))
                self.assertEqual(limit + 1, path.stat().st_size)
                path.unlink()

    def test_parent_links_hardlinks_and_unsafe_directory_are_not_repaired(self):
        self.write_state(self.record)
        link = self.root / "ancestor-link"
        link.symlink_to(self.registry.parent, target_is_directory=True)
        self.assert_safe_rejection(self.run_scoped("--registry", link / self.registry.name, "list"))
        hardlink = self.root / "same-inode.json"
        os.link(self.registry, hardlink)
        self.assert_safe_rejection(self.run_helper("list", check=False))
        hardlink.unlink()
        self.registry.parent.chmod(0o755)
        self.write_input()
        before = self.registry.read_bytes()
        self.assert_safe_rejection(self.run_helper("record", "--input", str(self.input), check=False))
        self.assertEqual(0o755, stat.S_IMODE(self.registry.parent.stat().st_mode))
        self.assertEqual(before, self.registry.read_bytes())

    def test_owner_checks_are_descriptor_bound(self):
        helper = self.import_helper()
        self.write_input()
        actual_uid = os.getuid()
        with mock.patch.object(helper.os, "getuid", return_value=actual_uid + 1):
            with self.assertRaisesRegex(helper.RegistryError, "not owned"):
                helper.read_input(self.input)
        self.assertEqual(self.record, json.loads(self.input.read_text()))

    def test_source_replacement_during_read_is_not_a_clean_missing_registry(self):
        helper = self.import_helper()
        self.write_state(self.record)
        original_read = helper.os.read
        changed = False
        def replace_after_read(descriptor, size):
            nonlocal changed
            content = original_read(descriptor, size)
            if not changed:
                changed = True
                self.registry.rename(self.root / "retained-original.json")
            return content
        with mock.patch.object(helper.os, "read", side_effect=replace_after_read):
            with self.assertRaisesRegex(helper.RegistryError, "changed during"):
                helper.load_state(self.registry)
        self.assertTrue((self.root / "retained-original.json").is_file())

    def test_missing_or_corrupt_start_does_not_select_global_state(self):
        for start in (self.root / "missing", self.input):
            if start == self.input:
                self.write_input()
            self.assert_safe_rejection(self.run_scoped("--start", start, "path"))
        (self.root / ".git").write_text("not a git pointer\n")
        self.assert_safe_rejection(self.run_scoped("--start", self.root, "path"))
        self.assertFalse((self.root / "global-state").exists())

    def test_non_git_start_uses_explicit_absolute_state_home_without_creation(self):
        result = self.run_scoped("--start", self.root, "path")
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertEqual(str(self.root / "global-state/security-audit/credential-incidents.json"), result.stdout.strip())
        self.assertFalse((self.root / "global-state").exists())
        self.assert_safe_rejection(self.run_scoped("--start", self.root, "path",
                                  environment=dict(self.environment, XDG_STATE_HOME="relative-state")))

    def test_git_path_ignores_ambient_git_redirects(self):
        for name in ("repo-a", "repo-b"):
            target = self.root / name
            target.mkdir()
            subprocess.run(["git", "-C", str(target), "init", "-q"], env=self.environment, check=True, timeout=10)
        result = self.run_scoped("--start", self.root / "repo-a", "path", environment=dict(
            self.environment, GIT_DIR=str(self.root / "repo-b/.git"), GIT_WORK_TREE=str(self.root / "repo-b")))
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertEqual(str(self.root / "repo-a/.git/dotfiles-publish/credential-incidents.json"), result.stdout.strip())
        self.assertFalse((self.root / "repo-a/.git/dotfiles-publish").exists())

    def test_direct_helper_and_git_resolution_ignore_target_path_and_pythonpath(self):
        shadow = self.root / "untrusted-path"
        shadow.mkdir()
        marker = self.root / "must-not-run"
        for name in ("git", "python3"):
            script = shadow / name
            script.write_text('#!/bin/sh\n/usr/bin/touch "' + str(marker) + '"\nexit 1\n')
            script.chmod(0o700)
        (shadow / "sitecustomize.py").write_text('from pathlib import Path\nPath(' + repr(str(marker)) + ').touch()\n')
        environment = dict(self.environment, PATH=str(shadow), PYTHONPATH=str(shadow))
        for helper in HELPERS.values():
            result = subprocess.run([str(helper), "--start", str(self.root), "path"], cwd=self.root,
                                    env=environment, capture_output=True, text=True, timeout=20)
            self.assertEqual(0, result.returncode, result.stderr)
            self.assertFalse(marker.exists())

    def test_git_operational_failure_and_timeout_are_not_global_fallbacks(self):
        helper = self.import_helper()
        with mock.patch.object(helper.subprocess, "run", return_value=subprocess.CompletedProcess([], 1)):
            with self.assertRaises(helper.RegistryError):
                helper.default_registry_path(self.root)
        with mock.patch.object(helper.subprocess, "run", side_effect=subprocess.TimeoutExpired("git", 10)):
            with self.assertRaises(helper.RegistryError):
                helper.default_registry_path(self.root)
        self.assertFalse((self.root / "global-state").exists())

    def test_same_id_cannot_change_credential_identity_but_metadata_can_replace(self):
        self.write_state(self.record)
        before = self.registry.read_bytes()
        self.record["credential_fingerprint"] = "f" * 20
        self.write_input()
        self.assert_safe_rejection(self.run_helper("record", "--input", str(self.input), check=False))
        self.assertEqual(before, self.registry.read_bytes())
        self.record["credential_fingerprint"] = "0123456789abcdef0123"
        self.record["summary"] = "Updated synthetic assessment."
        self.record["provider_references"] = []
        self.write_input()
        self.run_helper("record", "--input", str(self.input))
        actual = json.loads(self.run_helper("show", "--id", self.record["incident_id"]).stdout)
        self.assertEqual([], actual["provider_references"])
        self.assertEqual(self.record["summary"], actual["summary"])

    def test_simultaneous_public_writers_preserve_each_incident(self):
        children = []
        try:
            for index in range(8):
                record = copy.deepcopy(self.record)
                record["incident_id"] = f"owned-worker-{index}"
                record["credential_fingerprint"] = f"{index:020x}"
                path = self.root / f"record-{index}.json"
                path.write_text(json.dumps(record))
                path.chmod(0o600)
                children.append(subprocess.Popen([sys.executable, "-I", "-B", str(HELPERS["codex"]),
                    "--registry", str(self.registry), "record", "--input", str(path)], cwd=self.root,
                    env=self.environment, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True))
            for child in children:
                stdout, stderr = child.communicate(timeout=20)
                self.assertEqual(0, child.returncode, stderr)
                self.assertTrue(stdout.startswith("recorded:"))
        finally:
            for child in children:
                if child.poll() is None:
                    child.kill()
                child.communicate(timeout=5)
        state = json.loads(self.registry.read_text())
        self.assertEqual({f"owned-worker-{index}" for index in range(8)}, set(state["incidents"]))
        self.assertEqual(0o600, stat.S_IMODE(self.registry.stat().st_mode))
        self.assertEqual(0o600, stat.S_IMODE(self.registry.with_name(self.registry.name + ".lock").stat().st_mode))
        self.assertEqual([], list(self.registry.parent.glob("*.tmp-*")))

    def test_lock_timeout_is_bounded_and_cannot_replace_existing_state(self):
        helper = self.import_helper()
        self.write_state(self.record)
        before = self.registry.read_bytes()
        with helper.open_directory(self.registry.parent, private=True) as directory:
            with helper.writer_lock(directory, self.registry.name + ".lock"):
                with mock.patch.object(helper, "LOCK_SECONDS", 0.05):
                    started = time.monotonic()
                    with self.assertRaisesRegex(helper.RegistryError, "lock timed out"):
                        with helper.writer_lock(directory, self.registry.name + ".lock"):
                            self.fail("second writer acquired held lock")
                    self.assertLess(time.monotonic() - started, 1)
        self.assertEqual(before, self.registry.read_bytes())

    def test_read_commands_do_not_create_lock_or_change_mode_content_or_mtime(self):
        self.write_state(self.record)
        before = self.registry.read_bytes(), self.registry.stat().st_mtime_ns, self.registry.stat().st_mode
        for command in ("list", "validate", "show"):
            arguments = (command, "--id", self.record["incident_id"]) if command == "show" else (command,)
            self.run_helper(*arguments)
            self.assertEqual(before, (self.registry.read_bytes(), self.registry.stat().st_mtime_ns,
                                      self.registry.stat().st_mode))
            self.assertEqual([self.registry.name], [path.name for path in self.registry.parent.iterdir()])


if __name__ == "__main__":
    unittest.main()
