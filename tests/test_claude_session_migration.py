"""Offline Claude relocation regressions, confined to disposable synthetic stores."""

from __future__ import annotations

import importlib.util
import json
import os
import stat
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "claude/skills/move-session-log/scripts/move_session_log.py"
OLD = "/fixture/old"
NEW = "/fixture/new"
OTHER = "/fixture/other"
SID = "11111111-2222-3333-4444-555555555555"
OTHER_SID = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"


def encoded(project):
    return project.replace("/", "-").replace(".", "-").replace(" ", "-")


def jsonl(*rows):
    return "".join(json.dumps(row) + "\n" for row in rows).encode()


def tree_snapshot(root):
    result = {}
    for path in [root, *sorted(root.rglob("*"))]:
        info = path.lstat()
        key = str(path.relative_to(root))
        data = os.readlink(path) if path.is_symlink() else path.read_bytes() if path.is_file() else None
        result[key] = (info.st_mode, info.st_ino, info.st_mtime_ns, data)
    return result


class ClaudeSessionMigrationTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="claude-migration-test-", dir="/tmp")
        self.addCleanup(self.temporary.cleanup)
        self.base = Path(self.temporary.name)
        self.config = self.base / "config"
        self.source = self.config / "projects" / encoded(OLD)
        self.source.mkdir(parents=True)
        self.target = self.config / "projects" / encoded(NEW)
        self.transcript = self.source / f"{SID}.jsonl"
        self.transcript.write_bytes(jsonl({"sessionId": SID, "type": "user", "cwd": OLD}))
        self.history = self.config / "history.jsonl"
        self.history.write_bytes(jsonl({"sessionId": SID, "project": OLD, "display": "fixture"}))
        self.backup = self.base / "recovery"
        self.env = {**os.environ, "CLAUDE_CONFIG_DIR": str(self.config), "PYTHONDONTWRITEBYTECODE": "1"}

    def run_cli(self, *args):
        return subprocess.run([sys.executable, "-B", str(SCRIPT), *map(str, args)],
                              env=self.env, capture_output=True, text=True, timeout=60)

    def apply(self, *args):
        return self.run_cli(*args, "--offline", "--backup-dir", self.backup)

    def load_adapter(self):
        spec = importlib.util.spec_from_file_location("fixture_claude_migration", SCRIPT)
        module = importlib.util.module_from_spec(spec)
        with mock.patch.dict(os.environ, self.env), mock.patch.object(sys, "path", [str(SCRIPT.parent), *sys.path]):
            spec.loader.exec_module(module)
        return module

    def add_sidecar(self):
        path = self.source / SID / "subagents" / "agent-fixture.jsonl"
        path.parent.mkdir(parents=True)
        path.write_bytes(jsonl({"sessionId": SID, "agentId": "fixture", "type": "assistant", "cwd": OLD}))
        (self.source / SID / "tool-results").mkdir()
        (self.source / SID / "tool-results" / "result.txt").write_text(OLD + " opaque fixture")
        return path

    def assert_refused_without_changes(self, *args, reason=None):
        before = tree_snapshot(self.config)
        result = self.apply(*args)
        self.assertNotEqual(result.returncode, 0, result.stdout)
        self.assertNotIn("Traceback", result.stderr)
        if reason is not None:
            self.assertIn(reason, result.stderr)
        self.assertEqual(tree_snapshot(self.config), before)
        self.assertFalse(self.backup.exists())
        return result

    def test_apply_requires_offline_and_new_backup(self):
        before = tree_snapshot(self.config)
        for flags in ((), ("--offline",), ("--backup-dir", str(self.backup))):
            with self.subTest(flags=flags):
                result = self.run_cli("--rename", OLD, NEW, *flags)
                self.assertNotEqual(result.returncode, 0, result.stdout)
                self.assertEqual(tree_snapshot(self.config), before)
                self.assertFalse(self.backup.exists())

    def test_both_dry_runs_are_completely_read_only(self):
        self.add_sidecar()
        before = tree_snapshot(self.base)
        for args in (("--rename", OLD, NEW), (SID, "--project", NEW)):
            with self.subTest(args=args):
                result = self.run_cli(*args, "--dry-run")
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(tree_snapshot(self.base), before)

    def test_existing_transcript_is_never_overwritten(self):
        self.target.mkdir()
        (self.target / self.transcript.name).write_bytes(b"concurrent destination transcript\n")
        self.assert_refused_without_changes(SID, "--project", NEW, reason="Expected one exact session")

    def test_existing_sidecar_is_never_nested_or_merged(self):
        self.add_sidecar()
        target_sidecar = self.target / SID
        target_sidecar.mkdir(parents=True)
        (target_sidecar / "keep.txt").write_text("concurrent sidecar")
        self.assert_refused_without_changes(SID, "--project", NEW, reason="absent destination")

    def test_existing_destination_with_colliding_other_origin_is_refused(self):
        project, other = "/fixture/a.b", "/fixture/a-b"
        target = self.source.parent / encoded(project)
        target.mkdir()
        (target / f"{OTHER_SID}.jsonl").write_bytes(jsonl({"sessionId": OTHER_SID, "cwd": other}))
        self.assert_refused_without_changes(SID, "--project", project, reason="different project origin")

    def test_existing_empty_destination_is_safe_to_populate(self):
        self.target.mkdir()
        result = self.apply(SID, "--project", NEW)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue((self.target / self.transcript.name).is_file())

    def test_existing_verified_destination_preserves_auxiliary_bytes_without_reading_them(self):
        self.target.mkdir()
        owner = self.target / f"{OTHER_SID}.jsonl"
        owner.write_bytes(jsonl({"sessionId": OTHER_SID, "cwd": NEW}))
        auxiliary = self.target / "sessions-index.json"
        auxiliary.write_bytes(b"opaque existing auxiliary data")
        adapter = self.load_adapter()
        original_read = adapter.MigrationPlan.read

        def read(plan, path):
            self.assertNotEqual(Path(path).resolve(), auxiliary.resolve())
            return original_read(plan, path)

        with mock.patch.object(adapter.MigrationPlan, "read", read):
            plan, _summary = adapter.build_single_plan(SID, NEW)
            plan.run(offline=True, backup_dir=self.backup)
        self.assertEqual(auxiliary.read_bytes(), b"opaque existing auxiliary data")
        self.assertTrue((self.target / self.transcript.name).is_file())

    def test_existing_destination_owner_change_after_planning_refuses_before_writes(self):
        self.target.mkdir()
        owner = self.target / f"{OTHER_SID}.jsonl"
        owner.write_bytes(jsonl({"sessionId": OTHER_SID, "cwd": NEW}))
        adapter = self.load_adapter()
        plan, _summary = adapter.build_single_plan(SID, NEW)
        owner.write_bytes(jsonl({"sessionId": OTHER_SID, "cwd": OTHER}))
        before = tree_snapshot(self.config)
        with self.assertRaisesRegex(adapter.MigrationError, "Captured input changed"):
            plan.run(offline=True, backup_dir=self.backup)
        self.assertEqual(tree_snapshot(self.config), before)
        self.assertFalse(self.backup.exists())

    def test_existing_destination_membership_change_after_planning_refuses(self):
        self.target.mkdir()
        adapter = self.load_adapter()
        plan, _summary = adapter.build_single_plan(SID, NEW)
        (self.target / "unplanned.txt").write_text("concurrent artifact")
        before = tree_snapshot(self.config)
        with self.assertRaises(adapter.MigrationError):
            plan.run(offline=True, backup_dir=self.backup)
        self.assertEqual(tree_snapshot(self.config), before)
        self.assertFalse(self.backup.exists())

    def test_unknown_destination_auxiliary_files_are_not_ownership_evidence(self):
        self.target.mkdir()
        (self.target / "sessions-index.json").write_text("{}")
        self.assert_refused_without_changes(SID, "--project", NEW, reason="no independently verified project owner")

    def test_dangling_destination_symlink_is_a_collision(self):
        self.target.mkdir()
        (self.target / self.transcript.name).symlink_to(self.base / "missing")
        self.assert_refused_without_changes(SID, "--project", NEW, reason="Unsupported linked or special transcript")

    def test_single_dry_run_reports_collision_too(self):
        self.target.mkdir()
        (self.target / self.transcript.name).write_text("destination")
        before = tree_snapshot(self.base)
        result = self.run_cli(SID, "--project", NEW, "--dry-run")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Expected one exact session", result.stderr)
        self.assertEqual(tree_snapshot(self.base), before)

    def test_rename_rewrites_nested_subagents_but_not_tool_results(self):
        self.add_sidecar()
        result = self.apply("--rename", OLD, NEW)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(self.source.exists())
        nested = self.target / SID / "subagents/agent-fixture.jsonl"
        self.assertEqual(json.loads(nested.read_text())["cwd"], NEW)
        self.assertEqual((self.target / SID / "tool-results/result.txt").read_text(), OLD + " opaque fixture")
        self.assertIn("session files scanned: 2", result.stdout)
        self.assertIn("Resume behavior has not been tested", result.stdout)
        self.assertTrue(self.backup.is_dir())
        self.assertEqual(stat.S_IMODE(self.backup.stat().st_mode), 0o700)

    def test_tool_result_jsonl_remains_opaque_even_when_it_looks_like_metadata(self):
        self.add_sidecar()
        directory = self.source / SID / "tool-results"
        payloads = {"metadata.jsonl": jsonl({"sessionId": SID, "cwd": OLD, "type": "assistant"}),
                    "truncated.jsonl": b'{"sessionId": arbitrary raw tool output'}
        for name, raw in payloads.items():
            (directory / name).write_bytes(raw)
        result = self.apply("--rename", OLD, NEW)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("session files scanned: 2", result.stdout)
        for name, raw in payloads.items():
            self.assertEqual((self.target / SID / "tool-results" / name).read_bytes(), raw)

    def test_unknown_sidecar_jsonl_path_refuses_instead_of_guessing_routing(self):
        directory = self.source / SID
        directory.mkdir()
        (directory / "unknown.jsonl").write_bytes(jsonl({"sessionId": SID, "cwd": OLD}))
        self.assert_refused_without_changes("--rename", OLD, NEW, reason="Unsupported sidecar JSONL path")

    def test_single_import_creates_one_new_bucket_and_preserves_later_cwd(self):
        self.add_sidecar()
        with self.transcript.open("ab") as handle:
            handle.write(jsonl({"sessionId": SID, "cwd": OTHER, "type": "user"}))
        with self.history.open("ab") as handle:
            handle.write(jsonl({"sessionId": SID, "project": OTHER, "display": "later cwd"}))
        self.transcript.chmod(0o640)
        result = self.apply(SID, "--project", NEW)
        self.assertEqual(result.returncode, 0, result.stderr)
        moved = self.target / self.transcript.name
        self.assertFalse(self.transcript.exists())
        self.assertEqual(stat.S_IMODE(moved.stat().st_mode), 0o640)
        self.assertEqual([json.loads(line)["cwd"] for line in moved.read_text().splitlines()], [NEW, OTHER])
        self.assertEqual([json.loads(line)["project"] for line in self.history.read_text().splitlines()], [NEW, OTHER])
        self.assertTrue((self.target / SID / "subagents/agent-fixture.jsonl").is_file())
        self.assertFalse((self.target / SID / SID).exists())

    def test_completed_single_import_can_be_previewed_again_without_changes(self):
        self.add_sidecar()
        result = self.apply(SID, "--project", NEW)
        self.assertEqual(result.returncode, 0, result.stderr)
        before = tree_snapshot(self.base)
        result = self.run_cli(SID, "--project", NEW, "--dry-run")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("already belongs to target; no changes", result.stdout)
        self.assertEqual(tree_snapshot(self.base), before)

    def test_target_only_session_with_stale_history_is_not_a_successful_noop(self):
        self.source.rename(self.target)
        (self.target / self.transcript.name).write_bytes(jsonl({"sessionId": SID, "cwd": NEW}))
        self.assert_refused_without_changes(SID, "--project", NEW, reason="disagree about session origin")

    def test_concurrent_history_append_invalidates_captured_plan(self):
        adapter = self.load_adapter()
        plan, _summary = adapter.build_rename_plan(OLD, NEW)
        concurrent = jsonl({"sessionId": OTHER_SID, "project": OTHER, "display": "concurrent"})
        with self.history.open("ab") as handle:
            handle.write(concurrent)
        before = tree_snapshot(self.config)
        with self.assertRaises(adapter.MigrationError):
            plan.run(offline=True, backup_dir=self.backup)
        self.assertEqual(tree_snapshot(self.config), before)
        self.assertTrue(self.history.read_bytes().endswith(concurrent))
        self.assertTrue(self.transcript.is_file())

    def test_already_relocated_bucket_membership_is_revalidated(self):
        self.source.rename(self.target)
        adapter = self.load_adapter()
        plan, _summary = adapter.build_rename_plan(OLD, NEW)
        concurrent = self.target / f"{OTHER_SID}.jsonl"
        concurrent.write_bytes(jsonl({"sessionId": OTHER_SID, "cwd": OLD}))
        before = tree_snapshot(self.config)
        with self.assertRaises(adapter.MigrationError):
            plan.run(offline=True, backup_dir=self.backup)
        self.assertEqual(tree_snapshot(self.config), before)

    def test_open_history_writer_is_refused_without_losing_append(self):
        with self.history.open("ab") as writer:
            result = self.apply("--rename", OLD, NEW)
            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertIn("open writer", result.stderr)
            writer.write(jsonl({"sessionId": OTHER_SID, "project": OTHER}))
        self.assertTrue(self.transcript.is_file())
        self.assertEqual(json.loads(self.history.read_text().splitlines()[-1])["sessionId"], OTHER_SID)
        self.assertFalse(self.backup.exists())

    def test_encoded_bucket_collision_does_not_move_another_project(self):
        old = "/fixture/a.b"
        other = "/fixture/a-b"
        collision = self.source.parent / encoded(old)
        self.source.rename(collision)
        (collision / self.transcript.name).write_bytes(jsonl({"sessionId": SID, "cwd": old}))
        (collision / f"{OTHER_SID}.jsonl").write_bytes(jsonl({"sessionId": OTHER_SID, "cwd": other}))
        self.history.write_bytes(jsonl({"sessionId": SID, "project": old}))
        self.assert_refused_without_changes("--rename", old, NEW, reason="mixed project origins")

    def test_distinct_paths_with_identical_encoding_are_refused(self):
        self.assert_refused_without_changes("--rename", "/fixture/a.b", "/fixture/a-b", reason="same encoded bucket")

    def test_identity_mismatch_unknown_origin_and_mixed_history_fail_closed(self):
        for rows, history in (
            ([{"sessionId": OTHER_SID, "cwd": OLD}], [{"sessionId": SID, "project": OLD}]),
            ([{"cwd": OLD}], [{"sessionId": SID, "project": OLD}]),
            ([{"sessionId": SID}], []),
            ([{"sessionId": SID, "cwd": OLD}], [{"sessionId": SID, "project": OTHER}]),
        ):
            with self.subTest(rows=rows):
                self.transcript.write_bytes(jsonl(*rows))
                self.history.write_bytes(jsonl(*history))
                self.assert_refused_without_changes("--rename", OLD, NEW)

    def test_history_can_establish_origin_when_matching_transcript_has_no_cwd(self):
        self.transcript.write_bytes(jsonl({"sessionId": SID, "type": "summary"}))
        result = self.apply("--rename", OLD, NEW)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(self.history.read_text())["project"], NEW)

    def test_only_exact_session_uuid_is_accepted(self):
        for session_id in ("11111111", f"prefix-{SID}", f"../{SID}"):
            with self.subTest(session_id=session_id):
                self.assert_refused_without_changes(session_id, "--project", NEW, reason="exact lowercase UUIDs")

    def test_unknown_bucket_artifact_and_orphan_sidecar_are_refused(self):
        unknown = self.source / "sessions-index.json"
        unknown.write_text("{}")
        self.assert_refused_without_changes("--rename", OLD, NEW, reason="Unsupported bucket artifact")
        unknown.unlink()
        (self.source / OTHER_SID).mkdir()
        self.assert_refused_without_changes("--rename", OLD, NEW, reason="Orphan session sidecar")

    def test_single_import_does_not_move_unrelated_bucket_artifacts(self):
        unknown = self.source / "sessions-index.json"
        unknown.write_bytes(b"opaque metadata")
        result = self.apply(SID, "--project", NEW)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(unknown.read_bytes(), b"opaque metadata")

    def test_sidecar_symlink_and_conflicting_agent_identity_are_refused(self):
        nested = self.add_sidecar()
        external = self.base / "external.jsonl"
        external.write_bytes(nested.read_bytes())
        nested.unlink()
        nested.symlink_to(external)
        self.assert_refused_without_changes("--rename", OLD, NEW)
        nested.unlink()
        nested.write_bytes(jsonl({"sessionId": OTHER_SID, "cwd": OLD}))
        self.assert_refused_without_changes("--rename", OLD, NEW)

    def test_malformed_transcript_or_history_is_rejected_before_any_move(self):
        original = self.transcript.read_bytes()
        for malformed in (b'{"sessionId":', b'[]\n', b'{"cwd":"a","cwd":"b"}\n', b'\xff\n'):
            with self.subTest(malformed=malformed):
                self.transcript.write_bytes(malformed)
                self.assert_refused_without_changes("--rename", OLD, NEW)
        self.transcript.write_bytes(original)
        self.history.write_bytes(b'{"sessionId":')
        self.assert_refused_without_changes("--rename", OLD, NEW)

    def test_non_json_whitespace_is_not_silently_accepted_as_a_blank_row(self):
        original = self.transcript.read_bytes()
        for separator in (b"\x0c\n", b"\x0b\n", "\u2028\n".encode()):
            with self.subTest(separator=separator):
                self.transcript.write_bytes(original + separator)
                self.assert_refused_without_changes("--rename", OLD, NEW, reason="Invalid JSONL object")

    def test_rewrite_preserves_all_other_bytes_including_payloads_and_crlf(self):
        raw = ('{ "message": {"cwd":"' + OLD + '", "text":"line\u2028separator"}, '
               '"sessionId" : "' + SID + '", "cwd" : "' + OLD + '", "type":"user" }\r\n')
        untouched = ' \r\n{"type":"summary", "payload":{"cwd":"' + OLD + '"}}\n'
        self.transcript.write_bytes((raw + untouched).encode())
        history_raw = ('{ "sessionId":"' + SID + '", "project":"' + OLD + '", '
                       '"display":"' + OLD + '", "payload":{"project":"' + OLD + '"}}\n')
        nested_id_row = json.dumps({"sessionId": OTHER_SID, "project": OLD, "payload": {"sessionId": SID}}) + "\n"
        self.history.write_bytes((history_raw + nested_id_row).encode())
        result = self.apply("--rename", OLD, NEW)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual((self.target / self.transcript.name).read_bytes(),
                         (raw.replace('"cwd" : "' + OLD + '"', '"cwd" : "' + NEW + '"') + untouched).encode())
        self.assertEqual(self.history.read_bytes(),
                         (history_raw.replace('"project":"' + OLD + '"', '"project":"' + NEW + '"', 1) + nested_id_row).encode())

    def test_shared_history_symlink_is_preserved(self):
        shared = self.base / "shared-history.jsonl"
        self.history.rename(shared)
        self.history.symlink_to(shared)
        result = self.apply("--rename", OLD, NEW)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(self.history.is_symlink())
        self.assertEqual(json.loads(shared.read_text())["project"], NEW)

    def test_explicit_projects_root_symlink_is_preserved(self):
        projects = self.config / "projects"
        shared = self.base / "shared-projects"
        projects.rename(shared)
        projects.symlink_to(shared, target_is_directory=True)
        result = self.apply("--rename", OLD, NEW)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(projects.is_symlink())
        self.assertTrue((shared / encoded(NEW) / self.transcript.name).is_file())
        self.assertFalse((shared / encoded(OLD)).exists())

    def test_default_does_not_even_parse_project_settings(self):
        settings = self.config / ".claude.json"
        settings.write_bytes(b"opaque malformed settings intentionally outside scope")
        before = settings.read_bytes()
        result = self.apply("--rename", OLD, NEW)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(settings.read_bytes(), before)

    def test_missing_history_and_uninventoried_custom_profiles_are_reported(self):
        self.history.unlink()
        sibling = self.base / ".claude-other"
        sibling.mkdir()
        unrelated = sibling / "history.jsonl"
        original = jsonl({"sessionId": SID, "project": OLD})
        unrelated.write_bytes(original)
        result = self.apply("--rename", OLD, NEW)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(f"history store: absent/unupdated: {self.history}", result.stdout)
        self.assertIn("selected Claude configuration and directly resolved aliases only", result.stdout)
        self.assertIn("sibling/custom profile histories were not inventoried", result.stdout)
        self.assertFalse(self.history.exists())
        self.assertEqual(unrelated.read_bytes(), original)

    def test_explicit_synthetic_settings_migration_is_backed_up(self):
        settings = self.config / ".claude.json"
        settings.write_text(json.dumps({"projects": {OLD: {"hasTrustDialogAccepted": True}, OTHER: {}}}))
        original = settings.read_bytes()
        result = self.apply("--rename", OLD, NEW, "--migrate-project-settings")
        self.assertEqual(result.returncode, 0, result.stderr)
        projects = json.loads(settings.read_text())["projects"]
        self.assertNotIn(OLD, projects)
        self.assertTrue(projects[NEW]["hasTrustDialogAccepted"])
        self.assertIn(OTHER, projects)
        self.assertTrue(any(path.is_file() and path.read_bytes() == original for path in self.backup.rglob("*")))
        before = tree_snapshot(self.base)
        result = self.run_cli("--rename", OLD, NEW, "--migrate-project-settings", "--dry-run")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(tree_snapshot(self.base), before)

    def test_settings_collision_or_schema_failure_prevents_session_move(self):
        settings = self.config / ".claude.json"
        for value in ({"projects": {OLD: {}, NEW: {}}}, {"projects": []}, {"projects": {OLD: False}}):
            with self.subTest(value=value):
                settings.write_text(json.dumps(value))
                self.assert_refused_without_changes("--rename", OLD, NEW, "--migrate-project-settings")

    def test_settings_flag_is_rejected_for_single_import(self):
        self.assert_refused_without_changes(SID, "--project", NEW, "--migrate-project-settings")

    def test_rename_rejects_an_explicit_single_session_project_argument(self):
        self.assert_refused_without_changes("--rename", OLD, NEW, "--project", OTHER,
                                           reason="--rename cannot be combined")

    def test_identical_rename_is_a_read_only_noop_without_offline_claim(self):
        before = tree_snapshot(self.base)
        result = self.run_cli("--rename", OLD, OLD)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(tree_snapshot(self.base), before)


if __name__ == "__main__":
    unittest.main()
