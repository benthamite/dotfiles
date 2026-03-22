"""Tests for gmail_maildir_sync.state."""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path

from gmail_maildir_sync.state import MessageRecord, SnapshotRecord, StateDB


def make_db(tmp_dir: str) -> StateDB:
    return StateDB(Path(tmp_dir) / "state.db")


def make_message(
    gmail_id: str = "msg1",
    folder: str = "Inbox",
    filename: str = "1234.G1.host:2,S",
    flags: str = "S",
    labels: list[str] | None = None,
) -> MessageRecord:
    return MessageRecord(
        gmail_id=gmail_id,
        gmail_thread_id="thread1",
        maildir_folder=folder,
        maildir_filename=filename,
        flags=flags,
        gmail_labels=labels if labels is not None else ["INBOX"],
        internal_date=1700000000,
        message_id="<abc@example.com>",
    )


class TestSchemaCreation(unittest.TestCase):
    def test_tables_exist(self):
        with tempfile.TemporaryDirectory() as tmp:
            db = make_db(tmp)
            cursor = db.conn.execute(
                "SELECT name FROM sqlite_master WHERE type='table'"
            )
            tables = {row[0] for row in cursor.fetchall()}
            db.close()

        self.assertIn("sync_state", tables)
        self.assertIn("messages", tables)
        self.assertIn("labels", tables)
        self.assertIn("maildir_snapshot", tables)

    def test_idempotent_init(self):
        """Opening the DB twice should not raise (IF NOT EXISTS guards)."""
        with tempfile.TemporaryDirectory() as tmp:
            db1 = make_db(tmp)
            db1.close()
            db2 = make_db(tmp)
            db2.close()


class TestHistoryId(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.db = make_db(self.tmp.name)

    def tearDown(self):
        self.db.close()
        self.tmp.cleanup()

    def test_get_returns_none_when_unset(self):
        self.assertIsNone(self.db.get_history_id())

    def test_set_and_get(self):
        self.db.set_history_id("99999")
        self.assertEqual(self.db.get_history_id(), "99999")

    def test_overwrite(self):
        self.db.set_history_id("100")
        self.db.set_history_id("200")
        self.assertEqual(self.db.get_history_id(), "200")


class TestMessages(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.db = make_db(self.tmp.name)

    def tearDown(self):
        self.db.close()
        self.tmp.cleanup()

    def test_upsert_and_get(self):
        rec = make_message()
        self.db.upsert_message(rec)
        self.db.commit()
        fetched = self.db.get_message("msg1", "Inbox")
        self.assertIsNotNone(fetched)
        self.assertEqual(fetched.gmail_id, "msg1")
        self.assertEqual(fetched.maildir_folder, "Inbox")
        self.assertEqual(fetched.flags, "S")

    def test_get_returns_none_when_absent(self):
        result = self.db.get_message("nonexistent", "Inbox")
        self.assertIsNone(result)

    def test_upsert_replaces_existing(self):
        rec = make_message(flags="S")
        self.db.upsert_message(rec)
        updated = make_message(flags="FS", filename="1234.G1.host:2,FS")
        self.db.upsert_message(updated)
        self.db.commit()
        fetched = self.db.get_message("msg1", "Inbox")
        self.assertEqual(fetched.flags, "FS")

    def test_gmail_labels_round_trip(self):
        rec = make_message(labels=["INBOX", "STARRED"])
        self.db.upsert_message(rec)
        self.db.commit()
        fetched = self.db.get_message("msg1", "Inbox")
        self.assertEqual(fetched.gmail_labels, ["INBOX", "STARRED"])

    def test_get_messages_by_gmail_id_multiple_folders(self):
        self.db.upsert_message(make_message(folder="Inbox"))
        self.db.upsert_message(make_message(folder="Starred", filename="1234.G1.host:2,FS"))
        self.db.commit()
        records = self.db.get_messages_by_gmail_id("msg1")
        self.assertEqual(len(records), 2)
        folders = {r.maildir_folder for r in records}
        self.assertIn("Inbox", folders)
        self.assertIn("Starred", folders)

    def test_get_messages_by_gmail_id_empty(self):
        self.assertEqual(self.db.get_messages_by_gmail_id("missing"), [])

    def test_delete_message(self):
        self.db.upsert_message(make_message())
        self.db.commit()
        self.db.delete_message("msg1", "Inbox")
        self.db.commit()
        self.assertIsNone(self.db.get_message("msg1", "Inbox"))

    def test_delete_message_only_removes_specified_folder(self):
        self.db.upsert_message(make_message(folder="Inbox"))
        self.db.upsert_message(make_message(folder="Starred", filename="x:2,FS"))
        self.db.commit()
        self.db.delete_message("msg1", "Inbox")
        self.db.commit()
        self.assertIsNone(self.db.get_message("msg1", "Inbox"))
        self.assertIsNotNone(self.db.get_message("msg1", "Starred"))

    def test_message_id_field(self):
        rec = make_message()
        self.db.upsert_message(rec)
        self.db.commit()
        fetched = self.db.get_message("msg1", "Inbox")
        self.assertEqual(fetched.message_id, "<abc@example.com>")

    def test_internal_date_field(self):
        rec = make_message()
        self.db.upsert_message(rec)
        self.db.commit()
        fetched = self.db.get_message("msg1", "Inbox")
        self.assertEqual(fetched.internal_date, 1700000000)


class TestLabels(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.db = make_db(self.tmp.name)

    def tearDown(self):
        self.db.close()
        self.tmp.cleanup()

    def test_upsert_and_get_label_id(self):
        self.db.upsert_label("Label_123", "Refiled", "user")
        self.db.commit()
        self.assertEqual(self.db.get_label_id("Refiled"), "Label_123")

    def test_get_label_id_returns_none_when_absent(self):
        self.assertIsNone(self.db.get_label_id("NonExistent"))

    def test_get_label_name(self):
        self.db.upsert_label("INBOX", "Inbox", "system")
        self.db.commit()
        self.assertEqual(self.db.get_label_name("INBOX"), "Inbox")

    def test_get_label_name_returns_none_when_absent(self):
        self.assertIsNone(self.db.get_label_name("MISSING"))

    def test_get_all_labels(self):
        self.db.upsert_label("INBOX", "Inbox", "system")
        self.db.upsert_label("Label_1", "Work", "user")
        self.db.commit()
        all_labels = self.db.get_all_labels()
        self.assertEqual(all_labels["INBOX"], "Inbox")
        self.assertEqual(all_labels["Label_1"], "Work")

    def test_get_all_labels_empty(self):
        self.assertEqual(self.db.get_all_labels(), {})

    def test_upsert_label_replaces(self):
        self.db.upsert_label("Label_1", "OldName", "user")
        self.db.upsert_label("Label_1", "NewName", "user")
        self.db.commit()
        self.assertEqual(self.db.get_label_name("Label_1"), "NewName")


class TestSnapshot(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.db = make_db(self.tmp.name)

    def tearDown(self):
        self.db.close()
        self.tmp.cleanup()

    def _make_snap(self, folder="Inbox", filename="1234.G1.host:2,S", flags="S", mtime=1.0):
        return SnapshotRecord(maildir_folder=folder, filename=filename, flags=flags, mtime=mtime)

    def test_upsert_and_get_snapshot(self):
        self.db.upsert_snapshot(self._make_snap())
        self.db.commit()
        records = self.db.get_snapshot("Inbox")
        self.assertEqual(len(records), 1)
        self.assertEqual(records[0].filename, "1234.G1.host:2,S")

    def test_get_snapshot_returns_empty_for_unknown_folder(self):
        self.assertEqual(self.db.get_snapshot("Drafts"), [])

    def test_get_full_snapshot(self):
        self.db.upsert_snapshot(self._make_snap(folder="Inbox"))
        self.db.upsert_snapshot(self._make_snap(folder="Sent", filename="5678.G2.host:2,S"))
        self.db.commit()
        all_snap = self.db.get_full_snapshot()
        folders = {r.maildir_folder for r in all_snap}
        self.assertIn("Inbox", folders)
        self.assertIn("Sent", folders)

    def test_clear_snapshot(self):
        self.db.upsert_snapshot(self._make_snap())
        self.db.commit()
        self.db.clear_snapshot()
        self.assertEqual(self.db.get_full_snapshot(), [])

    def test_upsert_snapshot_replaces(self):
        self.db.upsert_snapshot(self._make_snap(flags="S", mtime=1.0))
        self.db.upsert_snapshot(self._make_snap(flags="FS", mtime=2.0))
        self.db.commit()
        records = self.db.get_snapshot("Inbox")
        self.assertEqual(len(records), 1)
        self.assertEqual(records[0].flags, "FS")
        self.assertEqual(records[0].mtime, 2.0)

    def test_mtime_preserved(self):
        self.db.upsert_snapshot(self._make_snap(mtime=1234567.89))
        self.db.commit()
        records = self.db.get_snapshot("Inbox")
        self.assertAlmostEqual(records[0].mtime, 1234567.89, places=2)


if __name__ == "__main__":
    unittest.main()
