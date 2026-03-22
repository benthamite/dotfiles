"""Tests for gmail_maildir_sync.pull."""

from __future__ import annotations

import base64
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from googleapiclient.errors import HttpError

from gmail_maildir_sync.config import Config
from gmail_maildir_sync.maildir import labels_to_folders
from gmail_maildir_sync.pull import _write_message_to_maildir, _incremental_pull
from gmail_maildir_sync.state import MessageRecord, StateDB


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


def make_raw_message() -> str:
    """Return a base64url-encoded minimal RFC 2822 message."""
    raw = b"From: sender@example.com\r\nTo: test@example.com\r\nSubject: Test\r\n\r\nHello"
    return base64.urlsafe_b64encode(raw).decode()


def make_gmail_message(
    gmail_id: str = "msg1",
    thread_id: str = "thread1",
    label_ids: list[str] | None = None,
    internal_date: str = "1700000000000",
    message_id: str = "<test@example.com>",
) -> dict:
    """Build a minimal Gmail API message dict."""
    if label_ids is None:
        label_ids = ["INBOX"]
    return {
        "id": gmail_id,
        "threadId": thread_id,
        "labelIds": label_ids,
        "internalDate": internal_date,
        "raw": make_raw_message(),
        "payload": {
            "headers": [{"name": "Message-ID", "value": message_id}]
        },
    }


class TestLabelsToFoldersMapping(unittest.TestCase):
    """Verify labels_to_folders mapping for various label combinations."""

    def test_inbox_only(self):
        self.assertIn("Inbox", labels_to_folders(["INBOX"]))

    def test_inbox_and_starred(self):
        folders = labels_to_folders(["INBOX", "STARRED"])
        self.assertIn("Inbox", folders)
        self.assertIn("Starred", folders)

    def test_sent_label(self):
        self.assertIn("Sent", labels_to_folders(["SENT"]))

    def test_draft_label(self):
        self.assertIn("Drafts", labels_to_folders(["DRAFT"]))

    def test_trash_label(self):
        self.assertIn("Trash", labels_to_folders(["TRASH"]))

    def test_refiled_with_custom_label_id(self):
        folders = labels_to_folders(["Label_99"], refiled_label_id="Label_99")
        self.assertIn("Refiled", folders)

    def test_unmapped_label_goes_to_all(self):
        folders = labels_to_folders(["CATEGORY_PROMOTIONS"])
        self.assertEqual(folders, ["All"])

    def test_inbox_priority_over_starred(self):
        folders = labels_to_folders(["STARRED", "INBOX"])
        self.assertEqual(folders[0], "Inbox")

    def test_spam_not_mapped(self):
        # SPAM has no explicit folder mapping
        folders = labels_to_folders(["SPAM"])
        self.assertEqual(folders, ["All"])


class TestWriteMessageToMaildir(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = Path(self.tmp.name)
        self.maildir_root = root / "mail"
        self.state_dir = root / "state"
        self.state_dir.mkdir(parents=True)
        self.db = StateDB(self.state_dir / "state.db")
        self.config = make_config(self.maildir_root, self.state_dir)
        # Create maildir folder structure
        from gmail_maildir_sync.maildir import ensure_maildir_folders
        ensure_maildir_folders(self.maildir_root)

    def tearDown(self):
        self.db.close()
        self.tmp.cleanup()

    def test_writes_message_to_inbox(self):
        msg = make_gmail_message(label_ids=["INBOX"])
        _write_message_to_maildir(self.config, self.db, msg, None, False)
        self.db.commit()

        rec = self.db.get_message("msg1", "Inbox")
        self.assertIsNotNone(rec)
        self.assertEqual(rec.gmail_id, "msg1")

    def test_message_file_created(self):
        msg = make_gmail_message(label_ids=["INBOX"])
        _write_message_to_maildir(self.config, self.db, msg, None, False)

        cur_dir = self.maildir_root / "Inbox" / "cur"
        files = list(cur_dir.iterdir())
        self.assertEqual(len(files), 1)

    def test_spam_message_skipped(self):
        msg = make_gmail_message(label_ids=["SPAM", "INBOX"])
        _write_message_to_maildir(self.config, self.db, msg, None, False)

        rec = self.db.get_message("msg1", "Inbox")
        self.assertIsNone(rec)

    def test_message_with_no_raw_skipped(self):
        msg = make_gmail_message(label_ids=["INBOX"])
        msg["raw"] = ""
        _write_message_to_maildir(self.config, self.db, msg, None, False)

        rec = self.db.get_message("msg1", "Inbox")
        self.assertIsNone(rec)

    def test_starred_message_hardlinked(self):
        msg = make_gmail_message(label_ids=["INBOX", "STARRED"])
        _write_message_to_maildir(self.config, self.db, msg, None, False)
        self.db.commit()

        # Message should be in Inbox (primary) and Starred (hardlink)
        inbox_rec = self.db.get_message("msg1", "Inbox")
        starred_rec = self.db.get_message("msg1", "Starred")
        self.assertIsNotNone(inbox_rec)
        self.assertIsNotNone(starred_rec)

    def test_flags_set_correctly_for_read_message(self):
        msg = make_gmail_message(label_ids=["INBOX"])  # no UNREAD -> read
        _write_message_to_maildir(self.config, self.db, msg, None, False)
        self.db.commit()

        rec = self.db.get_message("msg1", "Inbox")
        self.assertIn("S", rec.flags)

    def test_flags_set_correctly_for_unread_message(self):
        msg = make_gmail_message(label_ids=["INBOX", "UNREAD"])
        _write_message_to_maildir(self.config, self.db, msg, None, False)
        self.db.commit()

        rec = self.db.get_message("msg1", "Inbox")
        self.assertNotIn("S", rec.flags)

    def test_refiled_message_goes_to_refiled_folder(self):
        msg = make_gmail_message(label_ids=["Label_42"])
        _write_message_to_maildir(self.config, self.db, msg, "Label_42", False)
        self.db.commit()

        rec = self.db.get_message("msg1", "Refiled")
        self.assertIsNotNone(rec)

    def test_include_all_false_skips_all_only_message(self):
        # Message with no mapped label -> All folder only -> skipped when include_all=False
        msg = make_gmail_message(label_ids=["CATEGORY_UPDATES"])
        _write_message_to_maildir(self.config, self.db, msg, None, False)

        recs = self.db.get_messages_by_gmail_id("msg1")
        self.assertEqual(recs, [])

    def test_message_id_header_stored(self):
        msg = make_gmail_message(message_id="<unique@test.com>")
        _write_message_to_maildir(self.config, self.db, msg, None, False)
        self.db.commit()

        rec = self.db.get_message("msg1", "Inbox")
        self.assertEqual(rec.message_id, "<unique@test.com>")


class TestIncrementalPullHistoryProcessing(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = Path(self.tmp.name)
        self.maildir_root = root / "mail"
        self.state_dir = root / "state"
        self.state_dir.mkdir(parents=True)
        self.db = StateDB(self.state_dir / "state.db")
        self.config = make_config(self.maildir_root, self.state_dir)
        from gmail_maildir_sync.maildir import ensure_maildir_folders
        ensure_maildir_folders(self.maildir_root)

    def tearDown(self):
        self.db.close()
        self.tmp.cleanup()

    def _make_service(self, history_records, new_history_id, fetched_messages=None):
        service = MagicMock()
        if fetched_messages is None:
            fetched_messages = []
        return service, history_records, new_history_id, fetched_messages

    @patch("gmail_maildir_sync.pull.get_history")
    @patch("gmail_maildir_sync.pull.batch_get_messages")
    @patch("gmail_maildir_sync.pull._update_snapshot")
    def test_no_history_records_updates_history_id(self, mock_snap, mock_batch, mock_history):
        mock_history.return_value = ([], "55555")
        service = MagicMock()

        result = _incremental_pull(self.config, self.db, service, "11111", None, False, False)

        self.assertTrue(result)
        self.assertEqual(self.db.get_history_id(), "55555")

    @patch("gmail_maildir_sync.pull.get_history")
    @patch("gmail_maildir_sync.pull.batch_get_messages")
    @patch("gmail_maildir_sync.pull._update_snapshot")
    def test_messages_added_triggers_fetch(self, mock_snap, mock_batch, mock_history):
        history = [{"messagesAdded": [{"message": {"id": "new_msg"}}]}]
        mock_history.return_value = (history, "22222")
        mock_batch.return_value = [make_gmail_message(gmail_id="new_msg")]
        service = MagicMock()

        _incremental_pull(self.config, self.db, service, "11111", None, False, False)

        mock_batch.assert_called_once()
        fetched_ids = set(mock_batch.call_args[0][1])
        self.assertIn("new_msg", fetched_ids)

    @patch("gmail_maildir_sync.pull.get_history")
    @patch("gmail_maildir_sync.pull.batch_get_messages")
    @patch("gmail_maildir_sync.pull._update_snapshot")
    def test_messages_deleted_removes_from_maildir(self, mock_snap, mock_batch, mock_history):
        # Pre-populate a message in state and on disk
        msg = make_gmail_message(gmail_id="dead_msg")
        _write_message_to_maildir(self.config, self.db, msg, None, False)
        self.db.commit()

        history = [{"messagesDeleted": [{"message": {"id": "dead_msg"}}]}]
        mock_history.return_value = (history, "33333")
        mock_batch.return_value = []
        service = MagicMock()

        _incremental_pull(self.config, self.db, service, "11111", None, False, False)

        recs = self.db.get_messages_by_gmail_id("dead_msg")
        self.assertEqual(recs, [])

    @patch("gmail_maildir_sync.pull.get_history")
    @patch("gmail_maildir_sync.pull.batch_get_messages")
    @patch("gmail_maildir_sync.pull._update_snapshot")
    def test_label_change_triggers_fetch(self, mock_snap, mock_batch, mock_history):
        history = [{"labelsAdded": [{"message": {"id": "label_msg"}}]}]
        mock_history.return_value = (history, "44444")
        mock_batch.return_value = [make_gmail_message(gmail_id="label_msg")]
        service = MagicMock()

        _incremental_pull(self.config, self.db, service, "11111", None, False, False)

        mock_batch.assert_called_once()

    @patch("gmail_maildir_sync.pull.get_history")
    @patch("gmail_maildir_sync.pull._full_pull")
    def test_history_id_expired_triggers_full_sync(self, mock_full, mock_history):
        # Simulate a 404 HttpError from the Gmail history API
        resp = MagicMock()
        resp.status = 404
        mock_history.side_effect = HttpError(resp=resp, content=b"Gone")
        mock_full.return_value = True
        service = MagicMock()

        result = _incremental_pull(self.config, self.db, service, "old_id", None, False, False)

        mock_full.assert_called_once()
        self.assertTrue(result)

    @patch("gmail_maildir_sync.pull.get_history")
    @patch("gmail_maildir_sync.pull.batch_get_messages")
    @patch("gmail_maildir_sync.pull._update_snapshot")
    def test_dry_run_skips_writes(self, mock_snap, mock_batch, mock_history):
        history = [{"messagesAdded": [{"message": {"id": "new_msg"}}]}]
        mock_history.return_value = (history, "55555")
        service = MagicMock()

        result = _incremental_pull(self.config, self.db, service, "11111", None, False, dry_run=True)

        self.assertTrue(result)
        mock_batch.assert_not_called()

    @patch("gmail_maildir_sync.pull.get_history")
    def test_non_404_http_error_propagates(self, mock_history):
        resp = MagicMock()
        resp.status = 500
        mock_history.side_effect = HttpError(resp=resp, content=b"Server Error")
        service = MagicMock()

        with self.assertRaises(HttpError):
            _incremental_pull(self.config, self.db, service, "11111", None, False, False)


if __name__ == "__main__":
    unittest.main()
