"""Tests for gmail_maildir_sync.push."""

from __future__ import annotations

import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from gmail_maildir_sync.config import Config
from gmail_maildir_sync.maildir import ensure_maildir_folders
from gmail_maildir_sync.push import push, _folder_to_label_id
from gmail_maildir_sync.state import MessageRecord, SnapshotRecord, StateDB


def make_config(maildir_root: Path, state_dir: Path) -> Config:
    return Config(
        client_id="test-client-id",
        client_secret="test-client-secret",
        refresh_token="test-refresh-token",
        user_email="test@example.com",
        maildir_root=maildir_root,
        state_dir=state_dir,
        db_path=state_dir / "state.db",
        token_path=state_dir / "token.json",
        batch_size=10,
        max_results=500,
    )


def _write_file(folder_path: Path, filename: str, content: bytes = b"msg") -> Path:
    """Write a file into the cur/ subdirectory of a Maildir folder."""
    path = folder_path / "cur" / filename
    path.write_bytes(content)
    return path


def _snap(folder: str, filename: str, flags: str, mtime: float = 1.0) -> SnapshotRecord:
    return SnapshotRecord(maildir_folder=folder, filename=filename, flags=flags, mtime=mtime)


class PushTestBase(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = Path(self.tmp.name)
        self.maildir_root = root / "mail"
        self.state_dir = root / "state"
        self.state_dir.mkdir(parents=True)
        self.db = StateDB(self.state_dir / "state.db")
        self.config = make_config(self.maildir_root, self.state_dir)
        ensure_maildir_folders(self.maildir_root)
        self.service = MagicMock()

    def tearDown(self):
        self.db.close()
        self.tmp.cleanup()


class TestFlagChangeDetection(PushTestBase):
    """Flag changes between snapshot and current state produce label modifications."""

    @patch("gmail_maildir_sync.push.batch_modify_messages")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_mark_read_sends_remove_unread(self, mock_snap, mock_batch):
        mock_batch.return_value = (1, 0)
        gmail_id = "10"
        old_filename = f"1234.G{gmail_id}.host:2,"   # no flags (unread)
        new_filename = f"1234.G{gmail_id}.host:2,S"  # S flag (read)

        # Snapshot: unread (no S)
        self.db.upsert_snapshot(_snap("Inbox", old_filename, ""))
        self.db.commit()

        # Current: read (S)
        _write_file(self.maildir_root / "Inbox", new_filename)

        push(self.config, self.db, self.service)

        mock_batch.assert_called_once()
        mods = mock_batch.call_args[0][1]
        self.assertEqual(len(mods), 1)
        self.assertIn("UNREAD", mods[0]["remove_labels"])

    @patch("gmail_maildir_sync.push.batch_modify_messages")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_star_message_sends_add_starred(self, mock_snap, mock_batch):
        mock_batch.return_value = (1, 0)
        gmail_id = "20"
        old_filename = f"1234.G{gmail_id}.host:2,S"   # read, not starred
        new_filename = f"1234.G{gmail_id}.host:2,FS"  # read + starred

        self.db.upsert_snapshot(_snap("Inbox", old_filename, "S"))
        self.db.commit()

        _write_file(self.maildir_root / "Inbox", new_filename)

        push(self.config, self.db, self.service)

        mods = mock_batch.call_args[0][1]
        self.assertEqual(len(mods), 1)
        self.assertIn("STARRED", mods[0]["add_labels"])

    @patch("gmail_maildir_sync.push.batch_modify_messages")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_unstar_message_sends_remove_starred(self, mock_snap, mock_batch):
        mock_batch.return_value = (1, 0)
        gmail_id = "30"
        old_filename = f"1234.G{gmail_id}.host:2,FS"
        new_filename = f"1234.G{gmail_id}.host:2,S"

        self.db.upsert_snapshot(_snap("Inbox", old_filename, "FS"))
        self.db.commit()

        _write_file(self.maildir_root / "Inbox", new_filename)

        push(self.config, self.db, self.service)

        mods = mock_batch.call_args[0][1]
        self.assertEqual(len(mods), 1)
        self.assertIn("STARRED", mods[0]["remove_labels"])

    @patch("gmail_maildir_sync.push.batch_modify_messages")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_unchanged_flags_produces_no_modification(self, mock_snap, mock_batch):
        gmail_id = "40"
        filename = f"1234.G{gmail_id}.host:2,S"

        self.db.upsert_snapshot(_snap("Inbox", filename, "S"))
        self.db.commit()

        _write_file(self.maildir_root / "Inbox", filename)

        push(self.config, self.db, self.service)

        mock_batch.assert_not_called()


class TestFolderMoveDetection(PushTestBase):
    """Folder moves produce add/remove label modifications."""

    @patch("gmail_maildir_sync.push.batch_modify_messages")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_move_inbox_to_starred_removes_inbox_adds_starred(self, mock_snap, mock_batch):
        mock_batch.return_value = (1, 0)
        gmail_id = "50"
        filename = f"1234.G{gmail_id}.host:2,S"

        # Snapshot: in Inbox
        self.db.upsert_snapshot(_snap("Inbox", filename, "S"))
        self.db.commit()

        # Current: in Starred (not Inbox)
        _write_file(self.maildir_root / "Starred", filename)

        push(self.config, self.db, self.service)

        mock_batch.assert_called_once()
        mods = mock_batch.call_args[0][1]
        self.assertEqual(len(mods), 1)
        mod = mods[0]
        self.assertIn("STARRED", mod["add_labels"])
        self.assertIn("INBOX", mod["remove_labels"])

    @patch("gmail_maildir_sync.push.batch_modify_messages")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_move_to_sent_adds_sent_label(self, mock_snap, mock_batch):
        mock_batch.return_value = (1, 0)
        gmail_id = "60"
        filename = f"1234.G{gmail_id}.host:2,S"

        self.db.upsert_snapshot(_snap("Inbox", filename, "S"))
        self.db.commit()

        _write_file(self.maildir_root / "Sent", filename)

        push(self.config, self.db, self.service)

        mock_batch.assert_called_once()
        mods = mock_batch.call_args[0][1]
        mod = mods[0]
        self.assertIn("SENT", mod["add_labels"])
        self.assertIn("INBOX", mod["remove_labels"])

    @patch("gmail_maildir_sync.push.batch_modify_messages")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_move_to_refiled_uses_custom_label_id(self, mock_snap, mock_batch):
        mock_batch.return_value = (1, 0)
        gmail_id = "70"
        filename = f"1234.G{gmail_id}.host:2,S"

        # Register Refiled label in DB
        self.db.upsert_label("Label_99", "Refiled", "user")
        self.db.commit()

        self.db.upsert_snapshot(_snap("Inbox", filename, "S"))
        self.db.commit()

        _write_file(self.maildir_root / "Refiled", filename)

        push(self.config, self.db, self.service)

        mock_batch.assert_called_once()
        mods = mock_batch.call_args[0][1]
        mod = mods[0]
        self.assertIn("Label_99", mod["add_labels"])
        self.assertIn("INBOX", mod["remove_labels"])


class TestDeletionDetection(PushTestBase):
    """Messages present in snapshot but absent from current state are trashed."""

    @patch("gmail_maildir_sync.push.trash_message")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_deleted_message_trashed(self, mock_snap, mock_trash):
        gmail_id = "80"
        filename = f"1234.G{gmail_id}.host:2,S"

        # Snapshot has the message, but no file on disk
        self.db.upsert_snapshot(_snap("Inbox", filename, "S"))
        self.db.commit()

        push(self.config, self.db, self.service)

        mock_trash.assert_called_once_with(self.service, gmail_id)

    @patch("gmail_maildir_sync.push.trash_message")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_deleted_message_removed_from_state(self, mock_snap, mock_trash):
        gmail_id = "90"
        filename = f"1234.G{gmail_id}.host:2,S"

        # Add message to state DB messages table too
        self.db.upsert_message(
            MessageRecord(
                gmail_id=gmail_id,
                gmail_thread_id="t1",
                maildir_folder="Inbox",
                maildir_filename=filename,
                flags="S",
                gmail_labels=["INBOX"],
                internal_date=1700000000,
                message_id=None,
            )
        )
        self.db.upsert_snapshot(_snap("Inbox", filename, "S"))
        self.db.commit()

        push(self.config, self.db, self.service)

        recs = self.db.get_messages_by_gmail_id(gmail_id)
        self.assertEqual(recs, [])

    @patch("gmail_maildir_sync.push.trash_message")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_present_message_not_trashed(self, mock_snap, mock_trash):
        gmail_id = "100"
        filename = f"1234.G{gmail_id}.host:2,S"

        self.db.upsert_snapshot(_snap("Inbox", filename, "S"))
        self.db.commit()

        # File exists on disk
        _write_file(self.maildir_root / "Inbox", filename)

        push(self.config, self.db, self.service)

        mock_trash.assert_not_called()


class TestModificationsCollectedForBatch(PushTestBase):
    """Multiple flag changes are collected into a single batch_modify_messages call."""

    @patch("gmail_maildir_sync.push.batch_modify_messages")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_multiple_changes_batched_together(self, mock_snap, mock_batch):
        mock_batch.return_value = (2, 0)

        for gmail_id, old_flags, new_flags in [
            ("201", "", "S"),   # mark read
            ("202", "S", "FS"), # star
        ]:
            old_fn = f"1234.G{gmail_id}.host:2,{old_flags}"
            new_fn = f"1234.G{gmail_id}.host:2,{new_flags}"
            self.db.upsert_snapshot(_snap("Inbox", old_fn, old_flags))
            _write_file(self.maildir_root / "Inbox", new_fn)

        self.db.commit()

        push(self.config, self.db, self.service)

        mock_batch.assert_called_once()
        mods = mock_batch.call_args[0][1]
        self.assertEqual(len(mods), 2)

    @patch("gmail_maildir_sync.push.batch_modify_messages")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_gmail_id_present_in_each_modification(self, mock_snap, mock_batch):
        mock_batch.return_value = (1, 0)
        gmail_id = "210"
        old_fn = f"1234.G{gmail_id}.host:2,"
        new_fn = f"1234.G{gmail_id}.host:2,S"

        self.db.upsert_snapshot(_snap("Inbox", old_fn, ""))
        self.db.commit()

        _write_file(self.maildir_root / "Inbox", new_fn)

        push(self.config, self.db, self.service)

        mods = mock_batch.call_args[0][1]
        self.assertEqual(mods[0]["gmail_id"], gmail_id)

    @patch("gmail_maildir_sync.push.batch_modify_messages")
    @patch("gmail_maildir_sync.push._update_snapshot_from_current")
    def test_dry_run_does_not_call_batch(self, mock_snap, mock_batch):
        gmail_id = "220"
        old_fn = f"1234.G{gmail_id}.host:2,"
        new_fn = f"1234.G{gmail_id}.host:2,S"

        self.db.upsert_snapshot(_snap("Inbox", old_fn, ""))
        self.db.commit()

        _write_file(self.maildir_root / "Inbox", new_fn)

        result = push(self.config, self.db, self.service, dry_run=True)

        self.assertTrue(result)
        mock_batch.assert_not_called()


class TestFolderToLabelId(unittest.TestCase):
    """Unit tests for _folder_to_label_id helper."""

    def test_inbox(self):
        self.assertEqual(_folder_to_label_id("Inbox", None), "INBOX")

    def test_sent(self):
        self.assertEqual(_folder_to_label_id("Sent", None), "SENT")

    def test_trash(self):
        self.assertEqual(_folder_to_label_id("Trash", None), "TRASH")

    def test_starred(self):
        self.assertEqual(_folder_to_label_id("Starred", None), "STARRED")

    def test_drafts(self):
        self.assertEqual(_folder_to_label_id("Drafts", None), "DRAFT")

    def test_refiled_with_id(self):
        self.assertEqual(_folder_to_label_id("Refiled", "Label_42"), "Label_42")

    def test_refiled_without_id_returns_none(self):
        self.assertIsNone(_folder_to_label_id("Refiled", None))

    def test_all_folder_returns_none(self):
        # "All" is virtual, has no label ID
        self.assertIsNone(_folder_to_label_id("All", None))


if __name__ == "__main__":
    unittest.main()
