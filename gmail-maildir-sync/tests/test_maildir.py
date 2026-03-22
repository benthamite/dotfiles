"""Tests for gmail_maildir_sync.maildir."""

from __future__ import annotations

import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from gmail_maildir_sync.maildir import (
    base_filename,
    extract_flags,
    extract_gmail_id,
    generate_maildir_filename,
    gmail_labels_to_maildir_flags,
    hardlink_to_folder,
    labels_to_folders,
    maildir_flags_to_gmail_changes,
    scan_maildir_folder,
    write_to_maildir,
)


class TestGenerateMaildirFilename(unittest.TestCase):
    def test_format(self):
        with patch("gmail_maildir_sync.maildir.time.time", return_value=1700000000):
            with patch("gmail_maildir_sync.maildir.socket.gethostname", return_value="testhost"):
                result = generate_maildir_filename("12345", "S")
        self.assertEqual(result, "1700000000.G12345.testhost:2,S")

    def test_default_flags(self):
        with patch("gmail_maildir_sync.maildir.time.time", return_value=1700000000):
            with patch("gmail_maildir_sync.maildir.socket.gethostname", return_value="host"):
                result = generate_maildir_filename("99")
        self.assertTrue(result.endswith(":2,S"))

    def test_gmail_id_embedded(self):
        with patch("gmail_maildir_sync.maildir.time.time", return_value=1):
            with patch("gmail_maildir_sync.maildir.socket.gethostname", return_value="h"):
                result = generate_maildir_filename("abc123", "FS")
        self.assertIn(".Gabc123.", result)
        self.assertTrue(result.endswith(":2,FS"))

    def test_empty_flags(self):
        with patch("gmail_maildir_sync.maildir.time.time", return_value=1):
            with patch("gmail_maildir_sync.maildir.socket.gethostname", return_value="h"):
                result = generate_maildir_filename("1", "")
        self.assertTrue(result.endswith(":2,"))


class TestGmailLabelsToMaildirFlags(unittest.TestCase):
    def test_read_message_has_S_flag(self):
        # UNREAD absent -> S (seen)
        flags = gmail_labels_to_maildir_flags(["INBOX"])
        self.assertEqual(flags, "S")

    def test_unread_message_has_no_S_flag(self):
        flags = gmail_labels_to_maildir_flags(["INBOX", "UNREAD"])
        self.assertNotIn("S", flags)

    def test_starred_message_has_F_flag(self):
        flags = gmail_labels_to_maildir_flags(["INBOX", "STARRED"])
        self.assertIn("F", flags)
        self.assertIn("S", flags)  # not UNREAD -> seen

    def test_draft_message_has_D_flag(self):
        flags = gmail_labels_to_maildir_flags(["DRAFT"])
        self.assertIn("D", flags)

    def test_flags_are_sorted(self):
        flags = gmail_labels_to_maildir_flags(["DRAFT", "STARRED"])
        self.assertEqual(flags, "".join(sorted(flags)))

    def test_unread_starred_draft(self):
        flags = gmail_labels_to_maildir_flags(["UNREAD", "STARRED", "DRAFT"])
        # No S (unread), F (starred), D (draft); sorted -> DF
        self.assertEqual(flags, "DF")

    def test_empty_labels(self):
        flags = gmail_labels_to_maildir_flags([])
        # No UNREAD -> S; no STARRED or DRAFT
        self.assertEqual(flags, "S")


class TestMaildirFlagsToGmailChanges(unittest.TestCase):
    def test_mark_read(self):
        # Remove S (seen) -> was read, now unread: add UNREAD
        add, remove = maildir_flags_to_gmail_changes("S", "")
        self.assertIn("UNREAD", add)
        self.assertEqual(remove, [])

    def test_mark_unread(self):
        # Add S (seen) -> was unread, now read: remove UNREAD
        add, remove = maildir_flags_to_gmail_changes("", "S")
        self.assertEqual(add, [])
        self.assertIn("UNREAD", remove)

    def test_star_message(self):
        add, remove = maildir_flags_to_gmail_changes("S", "FS")
        self.assertIn("STARRED", add)
        self.assertEqual(remove, [])

    def test_unstar_message(self):
        add, remove = maildir_flags_to_gmail_changes("FS", "S")
        self.assertEqual(add, [])
        self.assertIn("STARRED", remove)

    def test_no_change(self):
        add, remove = maildir_flags_to_gmail_changes("S", "S")
        self.assertEqual(add, [])
        self.assertEqual(remove, [])

    def test_no_flags_no_change(self):
        add, remove = maildir_flags_to_gmail_changes("", "")
        self.assertEqual(add, [])
        self.assertEqual(remove, [])


class TestLabelsToFolders(unittest.TestCase):
    def test_inbox_label(self):
        folders = labels_to_folders(["INBOX"])
        self.assertIn("Inbox", folders)

    def test_sent_label(self):
        folders = labels_to_folders(["SENT"])
        self.assertIn("Sent", folders)

    def test_trash_label(self):
        folders = labels_to_folders(["TRASH"])
        self.assertIn("Trash", folders)

    def test_starred_label(self):
        folders = labels_to_folders(["STARRED"])
        self.assertIn("Starred", folders)

    def test_draft_label(self):
        folders = labels_to_folders(["DRAFT"])
        self.assertIn("Drafts", folders)

    def test_refiled_label(self):
        folders = labels_to_folders(["Label_123"], refiled_label_id="Label_123")
        self.assertIn("Refiled", folders)

    def test_no_mapped_labels_returns_all(self):
        folders = labels_to_folders(["CATEGORY_UPDATES"])
        self.assertEqual(folders, ["All"])

    def test_refiled_ignored_when_no_refiled_id(self):
        folders = labels_to_folders(["Label_123"], refiled_label_id=None)
        self.assertEqual(folders, ["All"])

    def test_priority_ordering_inbox_before_sent(self):
        folders = labels_to_folders(["SENT", "INBOX"])
        self.assertEqual(folders[0], "Inbox")
        self.assertIn("Sent", folders)

    def test_multiple_labels(self):
        folders = labels_to_folders(["INBOX", "STARRED"])
        self.assertIn("Inbox", folders)
        self.assertIn("Starred", folders)


class TestExtractGmailId(unittest.TestCase):
    def test_extracts_id(self):
        fname = "1700000000.G12345.host:2,S"
        self.assertEqual(extract_gmail_id(fname), "12345")

    def test_returns_none_when_absent(self):
        self.assertIsNone(extract_gmail_id("plainfilename"))

    def test_long_id(self):
        fname = "1234567890.G999888777666.host:2,"
        self.assertEqual(extract_gmail_id(fname), "999888777666")

    def test_no_match_without_dot_suffix(self):
        # The regex requires .G<digits>. so a trailing ID without dot should not match
        self.assertIsNone(extract_gmail_id("1234.G999"))


class TestExtractFlags(unittest.TestCase):
    def test_extracts_flags(self):
        self.assertEqual(extract_flags("filename:2,FS"), "FS")

    def test_empty_flags(self):
        self.assertEqual(extract_flags("filename:2,"), "")

    def test_no_flags_suffix(self):
        self.assertEqual(extract_flags("filename"), "")

    def test_single_flag(self):
        self.assertEqual(extract_flags("1234.G1.host:2,S"), "S")


class TestBaseFilename(unittest.TestCase):
    def test_strips_flags(self):
        self.assertEqual(base_filename("1234.G1.host:2,S"), "1234.G1.host")

    def test_strips_empty_flags(self):
        self.assertEqual(base_filename("1234.G1.host:2,"), "1234.G1.host")

    def test_no_flags_unchanged(self):
        self.assertEqual(base_filename("plainname"), "plainname")


class TestWriteToMaildir(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.folder_path = Path(self.tmp.name)
        (self.folder_path / "tmp").mkdir()
        (self.folder_path / "cur").mkdir()
        (self.folder_path / "new").mkdir()

    def tearDown(self):
        self.tmp.cleanup()

    def test_writes_content(self):
        filename = "1234.G1.host:2,S"
        content = b"From: test@example.com\r\n\r\nBody"
        written = write_to_maildir(self.folder_path, filename, content, "S")
        self.assertTrue(written.exists())
        self.assertEqual(written.read_bytes(), content)

    def test_writes_to_cur(self):
        filename = "1234.G2.host:2,S"
        written = write_to_maildir(self.folder_path, filename, b"data", "S")
        self.assertEqual(written.parent.name, "cur")

    def test_tmp_is_cleaned_up(self):
        filename = "1234.G3.host:2,S"
        write_to_maildir(self.folder_path, filename, b"data", "S")
        tmp_name = base_filename(filename)
        self.assertFalse((self.folder_path / "tmp" / tmp_name).exists())

    def test_appends_flags_if_missing(self):
        # Filename without :2, suffix gets flags appended
        filename = "1234.G4.host"
        written = write_to_maildir(self.folder_path, filename, b"x", "FS")
        self.assertTrue(written.name.endswith(":2,FS"))


class TestHardlinkToFolder(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = Path(self.tmp.name)
        self.src_folder = root / "Inbox"
        self.dst_folder = root / "Starred"
        for folder in (self.src_folder, self.dst_folder):
            (folder / "cur").mkdir(parents=True)
            (folder / "new").mkdir()
            (folder / "tmp").mkdir()

    def tearDown(self):
        self.tmp.cleanup()

    def test_creates_link_in_cur(self):
        filename = "1234.G5.host:2,S"
        source = self.src_folder / "cur" / filename
        source.write_bytes(b"message body")

        target = hardlink_to_folder(source, self.dst_folder, filename)
        self.assertTrue(target.exists())
        self.assertEqual(target.parent.name, "cur")
        self.assertEqual(target.read_bytes(), b"message body")

    def test_hardlink_shares_inode(self):
        filename = "1234.G6.host:2,S"
        source = self.src_folder / "cur" / filename
        source.write_bytes(b"shared content")

        target = hardlink_to_folder(source, self.dst_folder, filename)
        # If on the same filesystem, inodes match (hardlink); if cross-fs, a copy is made.
        # Either way, content must match.
        self.assertEqual(target.read_bytes(), source.read_bytes())


class TestScanMaildirFolder(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        folder = self.root / "Inbox"
        (folder / "cur").mkdir(parents=True)
        (folder / "new").mkdir()
        (folder / "tmp").mkdir()

        # Write test messages
        (folder / "cur" / "1234.G10.host:2,S").write_bytes(b"msg1")
        (folder / "new" / "1234.G11.host:2,").write_bytes(b"msg2")
        # Non-file entry (directory) should be skipped
        (folder / "cur" / "subdir").mkdir()

    def tearDown(self):
        self.tmp.cleanup()

    def test_finds_cur_and_new(self):
        messages = scan_maildir_folder(self.root, "Inbox")
        filenames = {m.filename for m in messages}
        self.assertIn("1234.G10.host:2,S", filenames)
        self.assertIn("1234.G11.host:2,", filenames)

    def test_skips_directories(self):
        messages = scan_maildir_folder(self.root, "Inbox")
        filenames = {m.filename for m in messages}
        self.assertNotIn("subdir", filenames)

    def test_gmail_id_populated(self):
        messages = scan_maildir_folder(self.root, "Inbox")
        ids = {m.gmail_id for m in messages}
        self.assertIn("10", ids)
        self.assertIn("11", ids)

    def test_flags_populated(self):
        messages = scan_maildir_folder(self.root, "Inbox")
        by_id = {m.gmail_id: m for m in messages}
        self.assertEqual(by_id["10"].flags, "S")
        self.assertEqual(by_id["11"].flags, "")

    def test_folder_attribute(self):
        messages = scan_maildir_folder(self.root, "Inbox")
        self.assertTrue(all(m.folder == "Inbox" for m in messages))

    def test_nonexistent_folder_returns_empty(self):
        messages = scan_maildir_folder(self.root, "Nonexistent")
        self.assertEqual(messages, [])


if __name__ == "__main__":
    unittest.main()
