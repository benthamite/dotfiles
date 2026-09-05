"""Offline adapter regressions using disposable stores; never real Codex data."""

from __future__ import annotations

import contextlib
import importlib.util
import io
import json
import os
import sqlite3
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


SCRIPT = Path(__file__).resolve().parents[1] / "codex/skills/move-session-log/scripts/move_session_log.py"
sys.path.insert(0, str(SCRIPT.parent))
with mock.patch.dict(os.environ, {"CODEX_HOME": "/tmp/codex-migration-test-unopened-root"}):
    SPEC = importlib.util.spec_from_file_location("codex_migration_test_adapter", SCRIPT)
    ADAPTER = importlib.util.module_from_spec(SPEC)
    SPEC.loader.exec_module(ADAPTER)
import migration_safety as SAFETY


SESSION = "11111111-2222-3333-4444-555555555555"
OTHER_SESSION = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
OLD = "/synthetic/old project"
NEW = "/synthetic/new project"
OTHER = "/synthetic/other-repository"


def jsonl(*objects):
    return b"".join(json.dumps(obj, ensure_ascii=False).encode() + b"\n" for obj in objects)


def metadata(identity=SESSION, cwd=OLD):
    return {"type": "session_meta", "payload": {"id": identity, "cwd": cwd}}


def digest(root):
    return {str(path.relative_to(root)): path.read_bytes()
            for path in sorted(root.rglob("*")) if path.is_file()}


class CodexSessionMigrationTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="codex-migration-test-", dir="/tmp")
        self.addCleanup(temporary.cleanup)
        self.base = Path(temporary.name).resolve()
        self.home = self.base / ".codex-a"
        (self.home / "sessions").mkdir(parents=True)
        self.rollout = self.home / "sessions" / f"rollout-{SESSION}.jsonl"
        self.rollout.write_bytes(jsonl(metadata()))
        self.backup = self.base / "backup"
        self.addCleanup(mock.patch.stopall)
        mock.patch.object(ADAPTER, "CODEX_HOME", self.home).start()
        mock.patch.dict(os.environ, {"CODEX_HOME": str(self.home), "PYTHONDONTWRITEBYTECODE": "1"}).start()
        # This boundary replaces OS process inspection, not any migration logic.
        self.writers = mock.patch.object(SAFETY, "_check_writers").start()

    def database(self, home=None, records=None):
        path = (home or self.home) / "state_5.sqlite"
        with contextlib.closing(sqlite3.connect(path)) as conn, conn:
            conn.execute("CREATE TABLE threads (id TEXT PRIMARY KEY, rollout_path TEXT, cwd TEXT)")
            conn.executemany("INSERT INTO threads VALUES (?, ?, ?)", records or [(SESSION, str(self.rollout), OLD)])
        return path

    def profile(self, name=".codex-b", shared=True):
        home = self.base / name
        home.mkdir()
        if shared:
            (home / "sessions").symlink_to(self.home / "sessions", target_is_directory=True)
        else:
            (home / "sessions").mkdir()
        return home

    def make_plan(self, rename=False):
        return ADAPTER.make_plan(None if rename else SESSION, OLD if rename else None, NEW)

    def apply(self, rename=False):
        plan, report = self.make_plan(rename)
        backup = plan.run(offline=True, backup_dir=self.backup)
        self.assertEqual(backup, self.backup)
        return report

    def wal_database(self):
        path = self.home / "state_5.sqlite"
        # Simulate a stopped/crashed fixture writer, retaining committed WAL data.
        factory = (
            "import os, sqlite3, sys; c=sqlite3.connect(sys.argv[1]); "
            "c.execute('PRAGMA journal_mode=WAL'); "
            "c.execute('CREATE TABLE threads (id TEXT PRIMARY KEY, rollout_path TEXT, cwd TEXT)'); "
            "c.execute('INSERT INTO threads VALUES (?, ?, ?)', (sys.argv[2], 'synthetic', sys.argv[3])); "
            "c.execute('INSERT INTO threads VALUES (?, ?, ?)', (sys.argv[4], 'unrelated', sys.argv[5])); "
            "c.commit(); os._exit(0)"
        )
        subprocess.run([sys.executable, "-c", factory, str(path), SESSION, OLD, OTHER_SESSION, OTHER],
                       check=True, env=dict(os.environ), capture_output=True, timeout=10)
        self.assertTrue(Path(str(path) + "-wal").exists())
        return path

    def test_filename_cannot_override_exact_metadata_identity(self):
        self.rollout.write_bytes(jsonl(metadata(OTHER_SESSION)))
        before = digest(self.home)
        with self.assertRaisesRegex(SAFETY.MigrationError, "exact session identity"):
            self.make_plan()
        self.assertEqual(digest(self.home), before)

    def test_complete_uuid_required_and_metadata_wins_over_unrelated_filename(self):
        with self.assertRaisesRegex(SAFETY.MigrationError, "canonical UUID"):
            ADAPTER.make_plan(SESSION[:8], None, NEW)
        renamed = self.rollout.with_name(f"rollout-{OTHER_SESSION}.jsonl")
        self.rollout.rename(renamed)
        self.apply()
        self.assertEqual(json.loads(renamed.read_bytes())["payload"]["cwd"], NEW)

    def test_duplicate_identity_across_archives_refuses_before_changes(self):
        archived = self.home / "archived_sessions"
        archived.mkdir()
        (archived / "different-name.jsonl").write_bytes(self.rollout.read_bytes())
        before = digest(self.home)
        with self.assertRaisesRegex(SAFETY.MigrationError, "same session identity"):
            self.make_plan()
        self.assertEqual(digest(self.home), before)

    def test_only_origin_routing_metadata_changes_and_other_raw_lines_survive(self):
        tool = b' { "type":"response_item", "payload":{"type":"function_call","arguments":"{\\"cwd\\":\\"/synthetic/old project\\",\\"workdir\\":\\"/synthetic/other-repository\\",\\"project\\":\\"release-label\\"}"}}\r\n'
        result = b'{ "type":"response_item", "payload":{"result":{"cwd":"/synthetic/old project","project":"customer-project"}}}\n'
        unrelated = jsonl({"type": "turn_context", "payload": {"cwd": OTHER}})
        original = jsonl(metadata(), {"type": "turn_context", "payload": {"cwd": OLD}}) + unrelated + tool + result + b"\n"
        self.rollout.write_bytes(original)
        for rename in (False, True):
            with self.subTest(rename=rename):
                plan, report = self.make_plan(rename)
                self.assertEqual(report["session_fields"], 2)
                expected = plan.rewrites[self.rollout]
                self.assertTrue(expected.endswith(unrelated + tool + result + b"\n"))
                plan.run(dry_run=True)
                self.assertEqual(self.rollout.read_bytes(), original)
        self.apply()
        self.assertTrue(self.rollout.read_bytes().endswith(unrelated + tool + result + b"\n"))

    def test_index_uses_only_its_top_level_identity_and_origin(self):
        other = jsonl({"session_id": OTHER_SESSION, "cwd": OLD, "nested": {"id": SESSION}})
        unrelated = jsonl({"session_id": SESSION, "cwd": OTHER})
        history = self.home / "history.jsonl"
        history.write_bytes(jsonl({"session_id": SESSION, "cwd": OLD, "nested": {"cwd": OLD}}) + other + unrelated)
        index = self.home / "session_index.jsonl"
        index.write_bytes(jsonl({"id": OTHER_SESSION, "cwd": OLD, "nested": {"id": SESSION}}))
        index_before = index.read_bytes()
        self.apply()
        updated = history.read_bytes()
        self.assertTrue(updated.endswith(other + unrelated))
        first = json.loads(updated.splitlines()[0])
        self.assertEqual(first["cwd"], NEW)
        self.assertEqual(first["nested"], {"cwd": OLD})
        self.assertEqual(index.read_bytes(), index_before)

    def test_changed_rows_preserve_every_unrelated_token_byte(self):
        token = b'"\\/synthetic\\/old project"'
        header = (b'{ "type" : "session_meta", "payload" : { "id" : "' + SESSION.encode()
                  + b'", "cw\\u0064" : ' + token
                  + b', "note" : "caf\\u00e9", "literal" : "\\\\n", "nested" : {"cwd" : '
                  + token + b'} } }\r\n')
        context = (b'{"type":"turn_context", "payload": { "cwd":' + token
                   + ', "note" : "café", "tool": { "arguments": "{\\"cwd\\":\\"/synthetic/old project\\"}"} }}\n'.encode())
        history = (b'{"session_id":"' + SESSION.encode() + b'", "cwd" : ' + token
                   + b', "project" : ' + token + b', "text":"caf\\u00e9  ", "data": {"cwd":'
                   + token + b'} }\n')
        self.rollout.write_bytes(header + context)
        history_path = self.home / "history.jsonl"
        history_path.write_bytes(history)
        self.apply()
        replacement = json.dumps(NEW).encode()
        self.assertEqual(self.rollout.read_bytes(), header.replace(token, replacement, 1)
                         + context.replace(token, replacement, 1))
        self.assertEqual(history_path.read_bytes(), history.replace(token, replacement, 2))

    def test_sql_updates_exact_id_not_rollout_substring(self):
        database = self.database(records=[(SESSION, "actual", OLD), (OTHER_SESSION, str(self.rollout), OLD)])
        report = self.apply()
        self.assertEqual(report["database_rows"], 1)
        with contextlib.closing(sqlite3.connect(database)) as conn:
            self.assertEqual(dict(conn.execute("SELECT id, cwd FROM threads")), {SESSION: NEW, OTHER_SESSION: OLD})

    def test_dry_run_reads_committed_wal_without_mutating_original_artifacts(self):
        database = self.wal_database()
        before = digest(self.home)
        real_connect = sqlite3.connect
        opened = []

        def connect(path, *args, **kwargs):
            opened.append(str(path))
            return real_connect(path, *args, **kwargs)

        with mock.patch.object(ADAPTER.sqlite3, "connect", side_effect=connect):
            plan, report = self.make_plan()
            plan.run(dry_run=True, backup_dir=self.backup)
        self.assertEqual(report["database_rows"], 1)
        self.assertTrue(opened)
        self.assertTrue(all("codex-migration-sqlite-" in path for path in opened))
        self.assertNotIn(str(database), opened)
        self.assertEqual(digest(self.home), before)
        self.assertFalse(self.backup.exists())

    def test_apply_preserves_latest_wal_data_and_backs_up_original_companions(self):
        database = self.wal_database()
        wal = Path(str(database) + "-wal")
        original_wal = wal.read_bytes()
        self.apply()
        with contextlib.closing(sqlite3.connect(database)) as conn:
            self.assertEqual(dict(conn.execute("SELECT id, cwd FROM threads")), {SESSION: NEW, OTHER_SESSION: OTHER})
        manifest = json.loads((self.backup / "manifest.json").read_bytes())
        item = next(item for item in manifest["inputs"] if item["originalPath"] == str(wal))
        self.assertEqual((self.backup / item["backup"]).read_bytes(), original_wal)
        self.assertTrue(all(path.stat().st_mode & 0o077 == 0 for path in self.backup.iterdir()))

    def test_database_symlink_uses_resolved_wal_location(self):
        alias = self.wal_database()
        storage = self.base / "shared-database"
        storage.mkdir()
        target = storage / "actual.sqlite"
        for suffix in ("", "-wal", "-shm"):
            Path(str(alias) + suffix).rename(Path(str(target) + suffix))
        alias.symlink_to(target)
        before = digest(self.base)
        plan, report = self.make_plan()
        plan.run(dry_run=True)
        self.assertEqual(report["database_rows"], 1)
        self.assertEqual(digest(self.base), before)
        plan.run(offline=True, backup_dir=self.backup)
        self.assertTrue(alias.is_symlink())
        with contextlib.closing(sqlite3.connect(target)) as conn:
            self.assertEqual(dict(conn.execute("SELECT id, cwd FROM threads")), {SESSION: NEW, OTHER_SESSION: OTHER})

    def test_all_database_schemas_preflight_before_first_rollout_write(self):
        self.database()
        sibling = self.profile()
        with contextlib.closing(sqlite3.connect(sibling / "state_5.sqlite")) as conn, conn:
            conn.execute("CREATE TABLE unrelated (value TEXT)")
        before = digest(self.base)
        with self.assertRaisesRegex(SAFETY.MigrationError, "schema"):
            self.make_plan()
        self.assertEqual(digest(self.base), before)
        self.assertFalse(self.backup.exists())

    def test_shared_profiles_update_separate_indices_but_exclude_other_stores(self):
        sibling = self.profile()
        unrelated = self.profile(".codex-other", shared=False)
        custom = self.profile("custom-account", shared=True)
        for home in (self.home, sibling, unrelated, custom):
            self.database(home)
            (home / "history.jsonl").write_bytes(jsonl({"session_id": SESSION, "cwd": OLD}))
            (home / "session_index.jsonl").write_bytes(jsonl({"id": SESSION, "cwd": OLD}))
        report = self.apply(rename=True)
        self.assertEqual(set(report["homes"]), {str(self.home), str(sibling)})
        self.assertEqual(report["database_rows"], 2)
        for home in (self.home, sibling, unrelated, custom):
            expected = NEW if home in (self.home, sibling) else OLD
            self.assertEqual(json.loads((home / "history.jsonl").read_bytes())["cwd"], expected)
            self.assertEqual(json.loads((home / "session_index.jsonl").read_bytes())["cwd"], expected)

    def test_symlinked_shared_history_is_rewritten_once_without_losing_alias(self):
        sibling = self.profile()
        history = self.home / "history.jsonl"
        history.write_bytes(jsonl({"session_id": SESSION, "cwd": OLD}))
        alias = sibling / "history.jsonl"
        alias.symlink_to(history)
        report = self.apply()
        self.assertEqual(report["history_fields"], 1)
        self.assertTrue(alias.is_symlink())
        self.assertEqual(json.loads(alias.read_bytes())["cwd"], NEW)

    def test_duplicate_history_alias_retargeting_invalidates_plan(self):
        sibling = self.profile()
        history = self.home / "history.jsonl"
        history.write_bytes(jsonl({"session_id": SESSION, "cwd": OLD}))
        alias = sibling / "history.jsonl"
        alias.symlink_to(history)
        plan, _report = self.make_plan()
        alternate = self.base / "alternate-history.jsonl"
        alternate.write_bytes(history.read_bytes())
        alias.unlink()
        alias.symlink_to(alternate)
        before = self.rollout.read_bytes()
        with self.assertRaises(SAFETY.MigrationError):
            plan.run(offline=True, backup_dir=self.backup)
        self.assertEqual(self.rollout.read_bytes(), before)

    def test_rollout_leaf_symlink_outside_store_is_refused_without_reading_it(self):
        outside = self.base / "outside.jsonl"
        outside.write_bytes(jsonl(metadata(OTHER_SESSION)))
        (self.home / "sessions" / "alias.jsonl").symlink_to(outside)
        with mock.patch.object(ADAPTER, "captured_header") as read_header:
            with self.assertRaisesRegex(SAFETY.MigrationError, "symlinks"):
                self.make_plan()
        read_header.assert_not_called()

    def test_single_session_in_sibling_archive_is_found(self):
        sibling = self.profile()
        archived = sibling / "archived_sessions"
        archived.mkdir()
        target = archived / self.rollout.name
        self.rollout.rename(target)
        self.apply()
        self.assertEqual(json.loads(target.read_bytes())["payload"]["cwd"], NEW)

    def test_missing_metadata_reported_and_later_creation_refused(self):
        plan, report = self.make_plan()
        self.assertEqual(set(report["missing"]), {str(self.home / name) for name in ("history.jsonl", "session_index.jsonl", "state_5.sqlite")})
        original = self.rollout.read_bytes()
        (self.home / "history.jsonl").write_bytes(b"")
        with self.assertRaises(SAFETY.MigrationError):
            plan.run(offline=True, backup_dir=self.backup)
        self.assertEqual(self.rollout.read_bytes(), original)
        self.assertFalse(self.backup.exists())

    def test_unrecognized_database_generation_is_not_reported_as_missing(self):
        (self.home / "state_6.sqlite").write_bytes(b"synthetic unsupported generation")
        with self.assertRaisesRegex(SAFETY.MigrationError, "Unsupported Codex state"):
            self.make_plan()

    def test_coexisting_unknown_database_generation_refuses_before_any_write(self):
        self.database()
        (self.home / "state_6.sqlite").write_bytes(b"synthetic unsupported generation")
        before = digest(self.home)
        with self.assertRaisesRegex(SAFETY.MigrationError, "generation"):
            self.make_plan()
        self.assertEqual(digest(self.home), before)

    def test_new_rollout_or_shared_profile_invalidates_inventory(self):
        for change in ("rollout", "profile", "generation"):
            with self.subTest(change=change):
                plan, _report = self.make_plan(rename=True)
                before = self.rollout.read_bytes()
                if change == "rollout":
                    added = self.rollout.with_name(f"rollout-{OTHER_SESSION}.jsonl")
                    added.write_bytes(jsonl(metadata(OTHER_SESSION)))
                elif change == "profile":
                    added = self.profile()
                else:
                    added = self.home / "state_6.sqlite"
                    added.write_bytes(b"synthetic new generation")
                with self.assertRaises(SAFETY.MigrationError):
                    plan.run(offline=True, backup_dir=self.backup)
                self.assertEqual(self.rollout.read_bytes(), before)
                self.assertFalse(self.backup.exists())
                if change == "profile":
                    (added / "sessions").unlink()
                    added.rmdir()
                else:
                    added.unlink()

    def test_inventory_change_between_rewrites_stops_remaining_writes(self):
        history = self.home / "history.jsonl"
        history.write_bytes(jsonl({"session_id": SESSION, "cwd": OLD}))
        before = history.read_bytes()
        plan, _report = self.make_plan(rename=True)
        real_replace = os.replace
        added = self.rollout.with_name(f"rollout-{OTHER_SESSION}.jsonl")

        def replace(source, destination):
            result = real_replace(source, destination)
            if Path(destination) == self.rollout:
                added.write_bytes(jsonl(metadata(OTHER_SESSION)))
            return result

        with mock.patch.object(SAFETY.os, "replace", side_effect=replace):
            with self.assertRaises(SAFETY.MigrationError):
                plan.run(offline=True, backup_dir=self.backup)
        self.assertEqual(history.read_bytes(), before)
        self.assertEqual(json.loads(self.rollout.read_bytes())["payload"]["cwd"], NEW)
        self.assertEqual(json.loads(added.read_bytes())["payload"]["cwd"], OLD)
        self.assertTrue((self.backup / "manifest.json").exists())

    def test_unknown_trigger_cannot_delete_an_unrelated_thread(self):
        database = self.database(records=[(SESSION, "actual", OLD), (OTHER_SESSION, "other", OTHER)])
        with contextlib.closing(sqlite3.connect(database)) as conn, conn:
            conn.execute(f"CREATE TRIGGER unsafe AFTER UPDATE ON threads BEGIN DELETE FROM threads WHERE id = '{OTHER_SESSION}'; END")
        before = digest(self.home)
        with self.assertRaisesRegex(SAFETY.MigrationError, "trigger"):
            self.make_plan()
        self.assertEqual(digest(self.home), before)
        self.assertFalse(self.backup.exists())

    def test_published_timestamp_triggers_and_cwd_indexes_preserve_all_other_fields(self):
        database = self.database(records=[(SESSION, "actual", OLD), (OTHER_SESSION, "other", OTHER)])
        with contextlib.closing(sqlite3.connect(database)) as conn, conn:
            for column in ("created_at", "updated_at", "created_at_ms", "updated_at_ms", "recency_at", "recency_at_ms"):
                conn.execute(f"ALTER TABLE threads ADD COLUMN {column} INTEGER DEFAULT 7")
            for column in ("created_at", "updated_at"):
                conn.execute(f"CREATE TRIGGER threads_{column}_ms_after_insert AFTER INSERT ON threads WHEN NEW.{column}_ms IS NULL BEGIN UPDATE threads SET {column}_ms = NEW.{column} * 1000 WHERE id = NEW.id; END")
                conn.execute(f"CREATE TRIGGER threads_{column}_ms_after_update AFTER UPDATE OF {column} ON threads WHEN NEW.{column} != OLD.{column} AND NEW.{column}_ms IS OLD.{column}_ms BEGIN UPDATE threads SET {column}_ms = NEW.{column} * 1000 WHERE id = NEW.id; END")
            conn.execute("CREATE TRIGGER threads_recency_at_after_insert AFTER INSERT ON threads WHEN NEW.recency_at_ms = 0 BEGIN UPDATE threads SET recency_at = NEW.updated_at, recency_at_ms = COALESCE(NEW.updated_at_ms, NEW.updated_at * 1000) WHERE id = NEW.id; END")
            conn.execute("CREATE INDEX fixture_cwd_sort ON threads(cwd, updated_at_ms DESC, id DESC)")
            before = conn.execute("SELECT * FROM threads ORDER BY id").fetchall()
        self.apply()
        with contextlib.closing(sqlite3.connect(database)) as conn:
            after = conn.execute("SELECT * FROM threads ORDER BY id").fetchall()
        expected = [tuple(NEW if row[0] == SESSION and index == 2 else value
                          for index, value in enumerate(row)) for row in before]
        self.assertEqual(after, expected)

    def test_generated_columns_and_cwd_foreign_keys_are_unsupported(self):
        database = self.database()
        with contextlib.closing(sqlite3.connect(database)) as conn, conn:
            conn.execute("ALTER TABLE threads ADD COLUMN derived TEXT GENERATED ALWAYS AS (cwd || '!') VIRTUAL")
        with self.assertRaisesRegex(SAFETY.MigrationError, "columns"):
            self.make_plan()
        database.unlink()
        database = self.database()
        with contextlib.closing(sqlite3.connect(database)) as conn, conn:
            conn.execute("CREATE TABLE children (cwd TEXT REFERENCES threads(cwd) ON UPDATE CASCADE)")
        with self.assertRaisesRegex(SAFETY.MigrationError, "foreign-key"):
            self.make_plan()

    def test_project_associations_are_preserved_and_explicitly_reported(self):
        database = self.database()
        with contextlib.closing(sqlite3.connect(database)) as conn, conn:
            conn.execute("CREATE TABLE projects (id TEXT PRIMARY KEY, name TEXT)")
            conn.execute("CREATE TABLE project_roots (project_id TEXT REFERENCES projects(id), path TEXT)")
            conn.execute("ALTER TABLE threads ADD COLUMN project_id TEXT REFERENCES projects(id)")
            conn.execute("INSERT INTO projects VALUES ('fixture-project', 'Original project')")
            conn.execute("INSERT INTO project_roots VALUES ('fixture-project', ?)", (OLD,))
            conn.execute("UPDATE threads SET project_id = 'fixture-project'")
        report = self.apply()
        self.assertEqual(set(report["unupdated_project_metadata"]),
                         {f"{database}: {field}" for field in ("threads.project_id", "projects", "project_roots")})
        with contextlib.closing(sqlite3.connect(database)) as conn:
            self.assertEqual(conn.execute("SELECT project_id, cwd FROM threads").fetchone(), ("fixture-project", NEW))
            self.assertEqual(conn.execute("SELECT path FROM project_roots").fetchone(), (OLD,))

    def test_detected_post_update_mismatch_rolls_back_before_commit(self):
        database = self.database(records=[(SESSION, "actual", OLD), (OTHER_SESSION, "other", OTHER)])
        plan, _report = self.make_plan()
        real_connect = sqlite3.connect
        commits = []

        class FaultConnection:
            def __init__(self, connection):
                self.connection = connection

            def execute(self, sql, parameters=()):
                result = self.connection.execute(sql, parameters)
                if sql.startswith("UPDATE threads SET cwd"):
                    self.connection.execute("DELETE FROM threads WHERE id = ?", (OTHER_SESSION,))
                return result

            def set_authorizer(self, _callback):
                # Inject an otherwise-prevented engine side effect to exercise readback rollback.
                return None

            def commit(self):
                commits.append(True)
                self.connection.commit()

            def close(self):
                self.connection.close()

        with mock.patch.object(ADAPTER.sqlite3, "connect", side_effect=lambda *args, **kwargs: FaultConnection(real_connect(*args, **kwargs))):
            with self.assertRaisesRegex(SAFETY.MigrationError, "not committed"):
                ADAPTER.database_action(plan, database,
                                        [database, *(Path(str(database) + suffix) for suffix in ADAPTER.SQLITE_COMPANIONS)],
                                        [(SESSION, OLD)], NEW)
        self.assertEqual(commits, [])
        with contextlib.closing(real_connect(database)) as conn:
            self.assertEqual(dict(conn.execute("SELECT id, cwd FROM threads")), {SESSION: OLD, OTHER_SESSION: OTHER})

    def test_changed_input_refuses_and_preserves_new_append(self):
        plan, _report = self.make_plan()
        with self.rollout.open("ab") as writer:
            writer.write(jsonl({"type": "event_msg", "payload": {"message": "later"}}))
        changed = self.rollout.read_bytes()
        with self.assertRaises(SAFETY.MigrationError):
            plan.run(offline=True, backup_dir=self.backup)
        self.assertEqual(self.rollout.read_bytes(), changed)
        self.assertFalse(self.backup.exists())

    def test_open_writer_refusal_keeps_existing_append_handle_connected(self):
        plan, _report = self.make_plan()
        original = self.rollout.read_bytes()
        with self.rollout.open("ab") as writer:
            self.writers.side_effect = SAFETY.MigrationError("An input has an open writer")
            with self.assertRaisesRegex(SAFETY.MigrationError, "open writer"):
                plan.run(offline=True, backup_dir=self.backup)
            writer.write(b"later synthetic append\n")
        self.assertEqual(self.rollout.read_bytes(), original + b"later synthetic append\n")
        self.assertFalse(self.backup.exists())

    def test_apply_requires_offline_and_new_backup_directory(self):
        plan, _report = self.make_plan()
        before = digest(self.home)
        for kwargs in ({}, {"offline": True}, {"backup_dir": self.backup}):
            with self.subTest(kwargs=kwargs), self.assertRaises(SAFETY.MigrationError):
                plan.run(**kwargs)
        self.assertEqual(digest(self.home), before)
        self.assertFalse(self.backup.exists())

    def test_malformed_targeted_files_fail_before_any_change(self):
        valid = self.rollout.read_bytes()
        cases = [valid + b'{"type":', valid + jsonl(metadata()),
                 valid + b'{"type":"turn_context","payload":{"cwd":"a","cwd":"b"}}\n']
        for damaged in cases:
            with self.subTest(damaged=damaged):
                self.rollout.write_bytes(damaged)
                with self.assertRaises(SAFETY.MigrationError):
                    self.make_plan()
                self.assertEqual(self.rollout.read_bytes(), damaged)
        self.rollout.write_bytes(valid)
        (self.home / "history.jsonl").write_bytes(b'{"session_id":')
        with self.assertRaises(SAFETY.MigrationError):
            self.make_plan()
        self.assertEqual(self.rollout.read_bytes(), valid)

    def test_non_json_constants_and_non_lf_separators_refuse(self):
        valid = self.rollout.read_bytes()
        cases = [valid + b'{"data":' + constant + b'}\n'
                 for constant in (b"NaN", b"Infinity", b"-Infinity")]
        cases += [valid.rstrip(b"\n") + b"\x0b" + jsonl({"type": "event_msg"}),
                  valid + b"\x0c\n"]
        for damaged in cases:
            with self.subTest(prefix=damaged[:80]):
                self.rollout.write_bytes(damaged)
                with self.assertRaises(SAFETY.MigrationError):
                    self.make_plan()
                self.assertEqual(self.rollout.read_bytes(), damaged)

    def test_json_recursion_failure_is_sanitized(self):
        with mock.patch.object(ADAPTER.json, "loads", side_effect=RecursionError("private fixture payload")):
            with self.assertRaisesRegex(SAFETY.MigrationError, "Malformed") as error:
                ADAPTER.decode_row(b'{"data":[]}')
        self.assertNotIn("private fixture payload", str(error.exception))

    def test_database_changed_after_preflight_is_refused_before_file_rewrite(self):
        database = self.database()
        plan, _report = self.make_plan()
        original = self.rollout.read_bytes()
        with contextlib.closing(sqlite3.connect(database)) as conn, conn:
            conn.execute("UPDATE threads SET cwd = ?", (OTHER,))
        with self.assertRaises(SAFETY.MigrationError):
            plan.run(offline=True, backup_dir=self.backup)
        self.assertEqual(self.rollout.read_bytes(), original)
        with contextlib.closing(sqlite3.connect(database)) as conn:
            self.assertEqual(conn.execute("SELECT cwd FROM threads").fetchone(), (OTHER,))

    def test_partial_sql_failure_returns_nonzero_with_recoverable_originals(self):
        self.database()
        original = self.rollout.read_bytes()
        output, error = io.StringIO(), io.StringIO()
        argv = [str(SCRIPT), SESSION, "--project", NEW, "--offline", "--backup-dir", str(self.backup)]
        with mock.patch.object(sys, "argv", argv), contextlib.redirect_stdout(output), contextlib.redirect_stderr(error):
            with mock.patch.object(ADAPTER, "database_action", side_effect=RuntimeError("private fixture payload")):
                result = ADAPTER.main()
        self.assertEqual(result, 1)
        self.assertNotIn("private fixture payload", error.getvalue())
        self.assertIn(str(self.backup), error.getvalue())
        self.assertNotIn("rewritten:", output.getvalue())
        self.assertEqual(json.loads(self.rollout.read_bytes())["payload"]["cwd"], NEW)
        manifest = json.loads((self.backup / "manifest.json").read_bytes())
        item = next(item for item in manifest["inputs"] if item["originalPath"] == str(self.rollout))
        self.assertEqual((self.backup / item["backup"]).read_bytes(), original)
        self.assertEqual(manifest["journal"][-1]["status"], "failed-or-uncertain")

    def test_cli_dry_run_reports_coverage_without_claiming_runtime_verification(self):
        output = io.StringIO()
        with mock.patch.object(sys, "argv", [str(SCRIPT), SESSION, "--project", NEW, "--dry-run"]):
            with contextlib.redirect_stdout(output):
                self.assertEqual(ADAPTER.main(), 0)
        self.assertIn("custom homes were not inventoried", output.getvalue())
        self.assertIn("Runtime resume/reload was not tested", output.getvalue())
        self.assertIn("absent/unupdated", output.getvalue())

    @unittest.skipUnless(sys.platform == "darwin", "Native writer inspection is Darwin-specific")
    def test_native_cli_apply_and_repeat_preview_use_real_writer_inspection(self):
        database = self.wal_database()
        history = self.home / "history.jsonl"
        history.write_bytes(jsonl({"session_id": SESSION, "cwd": OLD}))
        command = [sys.executable, "-B", str(SCRIPT), SESSION, "--project", NEW]
        result = subprocess.run([*command, "--offline", "--backup-dir", str(self.backup)],
                                env=dict(os.environ, CODEX_HOME=str(self.home)),
                                capture_output=True, text=True, timeout=60)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(self.rollout.read_bytes())["payload"]["cwd"], NEW)
        self.assertEqual(json.loads(history.read_bytes())["cwd"], NEW)
        with contextlib.closing(sqlite3.connect(database)) as conn:
            self.assertEqual(dict(conn.execute("SELECT id, cwd FROM threads")), {SESSION: NEW, OTHER_SESSION: OTHER})
        before = digest(self.home)
        result = subprocess.run([*command, "--dry-run"],
                                env=dict(os.environ, CODEX_HOME=str(self.home)),
                                capture_output=True, text=True, timeout=60)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("session path fields rewritten: 0", result.stdout)
        self.assertIn("state_db thread cwd rows rewritten: 0", result.stdout)
        self.assertEqual(digest(self.home), before)

    @unittest.skipUnless(sys.platform == "darwin", "Native writer inspection is Darwin-specific")
    def test_native_cli_refuses_real_open_writer_and_preserves_later_append(self):
        history = self.home / "history.jsonl"
        original = jsonl({"session_id": SESSION, "cwd": OLD})
        history.write_bytes(original)
        rollout_before = self.rollout.read_bytes()
        appended = jsonl({"session_id": OTHER_SESSION, "cwd": OTHER})
        with history.open("ab") as writer:
            result = subprocess.run([sys.executable, "-B", str(SCRIPT), SESSION,
                                     "--project", NEW, "--offline", "--backup-dir", str(self.backup)],
                                    env=dict(os.environ, CODEX_HOME=str(self.home)),
                                    capture_output=True, text=True, timeout=60)
            self.assertNotEqual(result.returncode, 0, result.stdout)
            self.assertIn("open writer", result.stderr)
            writer.write(appended)
        self.assertEqual(history.read_bytes(), original + appended)
        self.assertEqual(self.rollout.read_bytes(), rollout_before)
        self.assertFalse(self.backup.exists())


if __name__ == "__main__":
    unittest.main()
