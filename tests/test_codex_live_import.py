"""Closed-session import regressions with live, disposable SQLite stores."""

from __future__ import annotations

import importlib.util
import json
import os
from pathlib import Path
import sqlite3
import subprocess
import sys
import tempfile
import threading
import unittest
from unittest import mock


SCRIPT = Path(__file__).resolve().parents[1] / "codex/skills/move-session-log/scripts/move_session_log.py"
SESSION = "11111111-2222-3333-4444-555555555555"
OTHER_SESSION = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
OLD = "/synthetic/original-project"
NEW = "/synthetic/destination-project"
OTHER = "/synthetic/unrelated-project"


def jsonl(*rows):
    return b"".join(json.dumps(row).encode() + b"\n" for row in rows)


def contents(root):
    return {str(path.relative_to(root)): path.read_bytes()
            for path in root.rglob("*") if path.is_file()}


class CodexLiveImportTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="codex-live-import-test-", dir="/tmp")
        self.addCleanup(temporary.cleanup)
        self.base = Path(temporary.name).resolve()
        self.home = self.base / ".codex-test"
        (self.home / "sessions").mkdir(parents=True)
        self.rollout = self.home / "sessions" / f"rollout-{SESSION}.jsonl"
        self.preserved = jsonl(
            {"type": "turn_context", "payload": {"cwd": OTHER}},
            {"type": "response_item", "payload": {"cwd": OLD, "text": OLD}},
        )
        self.original = jsonl(
            {"type": "session_meta", "payload": {"id": SESSION, "cwd": OLD}},
            {"type": "turn_context", "payload": {"cwd": OLD}},
        ) + self.preserved
        self.rollout.write_bytes(self.original)
        # Actual Codex history/index entries have no project-routing fields.
        self.history = self.home / "history.jsonl"
        self.history.write_bytes(jsonl(
            {"session_id": SESSION, "text": "historical prompt " + OLD},
            {"session_id": OTHER_SESSION, "text": "unrelated"},
        ))
        self.index = self.home / "session_index.jsonl"
        self.index.write_bytes(jsonl(
            {"id": SESSION, "thread_name": "Selected conversation"},
            {"id": OTHER_SESSION, "thread_name": "Unrelated conversation"},
        ))
        self.database = self.home / "state_5.sqlite"
        with sqlite3.connect(self.database) as conn:
            conn.execute("CREATE TABLE threads (id TEXT PRIMARY KEY, rollout_path TEXT, cwd TEXT, title TEXT)")
            conn.executemany("INSERT INTO threads VALUES (?, ?, ?, ?)", [
                (SESSION, str(self.rollout), OLD, "selected"),
                (OTHER_SESSION, "unrelated-rollout", OTHER, "unrelated"),
            ])
        conn.close()
        self.backup = self.base / "recovery"
        self.environment = {**os.environ, "CODEX_HOME": str(self.home), "PYTHONDONTWRITEBYTECODE": "1"}
        for name in ("CODEX_THREAD_ID", "CODEX_SESSION_ID"):
            self.environment.pop(name, None)

    def cli(self, *args, environment=None):
        return subprocess.run(
            [sys.executable, str(SCRIPT), "--project", NEW, SESSION,
             "--backup-dir", str(self.backup), *args],
            env=environment or self.environment, capture_output=True, text=True, timeout=30,
        )

    def rows(self):
        conn = sqlite3.connect(self.database)
        try:
            return conn.execute("SELECT * FROM threads ORDER BY id").fetchall()
        finally:
            conn.close()

    def assert_moved(self):
        rows = [json.loads(line) for line in self.rollout.read_bytes().splitlines()]
        self.assertEqual([row["payload"]["cwd"] for row in rows[:2]], [NEW, NEW])
        self.assertTrue(self.rollout.read_bytes().endswith(self.preserved))
        selected = next(row for row in self.rows() if row[0] == SESSION)
        self.assertEqual(selected, (SESSION, str(self.rollout), NEW, "selected"))

    def test_closed_session_moves_while_unrelated_sqlite_writer_stays_live(self):
        stop = threading.Event()
        ready = threading.Event()
        writes = []
        errors = []

        def writer():
            conn = sqlite3.connect(self.database, timeout=5)
            try:
                conn.execute("PRAGMA journal_mode=WAL")
                conn.execute("PRAGMA wal_autocheckpoint=0")
                while not stop.is_set():
                    title = f"unrelated update {len(writes)}"
                    conn.execute("UPDATE threads SET title = ? WHERE id = ?", (title, OTHER_SESSION))
                    conn.commit()
                    writes.append(title)
                    ready.set()
                    stop.wait(0.01)
            except Exception as error:
                errors.append(error)
                ready.set()
            finally:
                conn.close()

        thread = threading.Thread(target=writer)
        thread.start()
        self.addCleanup(thread.join)
        self.addCleanup(stop.set)
        self.assertTrue(ready.wait(5))
        self.assertFalse(errors)
        before_history, before_index = self.history.read_bytes(), self.index.read_bytes()
        count_before = len(writes)
        result = self.cli()
        count_after = len(writes)
        stop.set()
        thread.join(5)
        self.assertFalse(thread.is_alive())
        self.assertFalse(errors)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertGreater(count_after, count_before)
        self.assert_moved()
        self.assertEqual(self.history.read_bytes(), before_history)
        self.assertEqual(self.index.read_bytes(), before_index)
        other = next(row for row in self.rows() if row[0] == OTHER_SESSION)
        self.assertEqual(other, (OTHER_SESSION, "unrelated-rollout", OTHER, writes[-1]))
        manifest = json.loads((self.backup / "manifest.json").read_text())
        self.assertTrue(manifest["journal"])
        self.assertTrue(all(entry["status"] == "completed" for entry in manifest["journal"]))
        self.assertTrue(any(path.read_bytes() == self.original for path in self.backup.rglob("*") if path.is_file()))
        self.assertEqual(len(manifest["databaseSnapshots"]), 1)
        snapshot = self.backup / manifest["databaseSnapshots"][0]["backup"]
        conn = sqlite3.connect(snapshot)
        try:
            self.assertEqual(conn.execute("SELECT cwd FROM threads WHERE id = ?", (SESSION,)).fetchone(), (OLD,))
            self.assertEqual(conn.execute("PRAGMA quick_check").fetchall(), [("ok",)])
        finally:
            conn.close()

    def test_open_unchanged_history_does_not_block_import(self):
        before = self.history.read_bytes()
        with self.history.open("ab") as writer:
            result = self.cli()
            appended = jsonl({"session_id": OTHER_SESSION, "text": "still writable"})
            writer.write(appended)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_moved()
        self.assertEqual(self.history.read_bytes(), before + appended)

    def test_open_history_needing_routing_rewrite_is_refused(self):
        self.history.write_bytes(jsonl({"session_id": SESSION, "cwd": OLD}))
        before = contents(self.home)
        with self.history.open("ab"):
            result = self.cli()
        self.assertNotEqual(result.returncode, 0)
        self.assertRegex(result.stderr.lower(), "writer|open|active")
        self.assertEqual(contents(self.home), before)
        self.assertFalse(self.backup.exists())

    def test_closed_history_routing_fields_update_without_touching_other_rows(self):
        unrelated = jsonl({"session_id": OTHER_SESSION, "cwd": OLD})
        self.history.write_bytes(jsonl({"session_id": SESSION, "cwd": OLD}) + unrelated)
        result = self.cli()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_moved()
        self.assertEqual(json.loads(self.history.read_bytes().splitlines()[0])["cwd"], NEW)
        self.assertTrue(self.history.read_bytes().endswith(unrelated))

    def test_open_target_transcript_is_refused_without_changes(self):
        before = contents(self.home)
        with self.rollout.open("ab"):
            result = self.cli()
        self.assertNotEqual(result.returncode, 0)
        self.assertRegex(result.stderr.lower(), "writer|open|active")
        self.assertEqual(contents(self.home), before)
        self.assertFalse(self.backup.exists())

    def test_current_session_identity_is_refused_even_without_open_file(self):
        before = contents(self.home)
        result = self.cli(environment={**self.environment, "CODEX_THREAD_ID": SESSION})
        self.assertNotEqual(result.returncode, 0)
        self.assertRegex(result.stderr.lower(), "current|active|running")
        self.assertEqual(contents(self.home), before)
        self.assertFalse(self.backup.exists())

    def test_dry_run_changes_no_store_or_backup(self):
        before = contents(self.home)
        result = self.cli("--dry-run")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(contents(self.home), before)
        self.assertFalse(self.backup.exists())

    def test_malformed_target_is_refused_before_mutation(self):
        with self.rollout.open("ab") as handle:
            handle.write(b'{"type":"turn_context","payload":')
        before = contents(self.home)
        result = self.cli()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Malformed", result.stderr)
        self.assertEqual(contents(self.home), before)
        self.assertFalse(self.backup.exists())

    def test_unsupported_schema_is_refused_before_mutation(self):
        with sqlite3.connect(self.database) as conn:
            conn.execute("CREATE TRIGGER unexpected AFTER UPDATE OF cwd ON threads BEGIN UPDATE threads SET title='changed'; END")
        conn.close()
        before = contents(self.home)
        result = self.cli()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("trigger", result.stderr.lower())
        self.assertEqual(contents(self.home), before)
        self.assertFalse(self.backup.exists())

    def test_target_database_cwd_change_after_preview_refuses_before_rollout_rewrite(self):
        sys.path.insert(0, str(SCRIPT.parent))
        self.addCleanup(sys.path.remove, str(SCRIPT.parent))
        spec = importlib.util.spec_from_file_location("codex_live_import_test_adapter", SCRIPT)
        adapter = importlib.util.module_from_spec(spec)
        with mock.patch.dict(os.environ, self.environment):
            spec.loader.exec_module(adapter)
            import live_import
            import migration_safety
            plan, _report = live_import.make_live_plan(SESSION, NEW, adapter=adapter)
            with sqlite3.connect(self.database) as conn:
                conn.execute("UPDATE threads SET cwd = ? WHERE id = ?", (OTHER, SESSION))
            conn.close()
            with self.assertRaises(migration_safety.MigrationError):
                plan.run(backup_dir=self.backup)
        self.assertEqual(self.rollout.read_bytes(), self.original)
        self.assertEqual(next(row for row in self.rows() if row[0] == SESSION)[2], OTHER)


if __name__ == "__main__":
    unittest.main()
