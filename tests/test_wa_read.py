"""Tests for bin/wa-read: allowlist enforcement, read-only access, display."""

import os
import sqlite3
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "bin" / "wa-read"

# Matches the schema in ~/repos/wa-archive/archive.go.
SCHEMA = """
CREATE TABLE chats (key TEXT PRIMARY KEY, name TEXT NOT NULL DEFAULT '',
  is_group INTEGER NOT NULL DEFAULT 0, updated_at INTEGER NOT NULL DEFAULT 0);
CREATE TABLE aliases (alias TEXT PRIMARY KEY, key TEXT NOT NULL);
CREATE TABLE messages (chat_key TEXT NOT NULL, id TEXT NOT NULL,
  sender_key TEXT NOT NULL DEFAULT '', sender_name TEXT NOT NULL DEFAULT '',
  from_me INTEGER NOT NULL DEFAULT 0, ts INTEGER NOT NULL, text TEXT NOT NULL DEFAULT '',
  quoted_id TEXT NOT NULL DEFAULT '', media_type TEXT NOT NULL DEFAULT '',
  file_name TEXT NOT NULL DEFAULT '', source TEXT NOT NULL, PRIMARY KEY (chat_key, id));
CREATE TABLE mutations (chat_key TEXT NOT NULL, target_id TEXT NOT NULL,
  event_id TEXT NOT NULL, kind TEXT NOT NULL, ts INTEGER NOT NULL,
  text TEXT NOT NULL DEFAULT '', PRIMARY KEY (chat_key, target_id, event_id));
CREATE TABLE sync_chunks (notification_id TEXT PRIMARY KEY, sync_type TEXT NOT NULL,
  chunk_order INTEGER NOT NULL, progress INTEGER NOT NULL, conversations INTEGER NOT NULL,
  messages INTEGER NOT NULL, received_at INTEGER NOT NULL);
"""

ALLOWED = "111@g.us"
OTHER = "222@g.us"
ALIASED_PN = "5491100000000@s.whatsapp.net"
ALIASED_LID = "999@lid"


class WaReadTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = Path(self.tmp.name)
        self.env = dict(os.environ, XDG_DATA_HOME=str(root / "data"),
                        XDG_STATE_HOME=str(root / "state"), XDG_CONFIG_HOME=str(root / "config"))
        (root / "data" / "wa-archive").mkdir(parents=True)
        (root / "state" / "wa-archive").mkdir(parents=True)
        (root / "config" / "wa-archive").mkdir(parents=True)
        self.db = root / "data" / "wa-archive" / "messages.db"
        self.allowlist = root / "config" / "wa-archive" / "allowlist"
        conn = sqlite3.connect(self.db)
        conn.executescript(SCHEMA)
        conn.executemany("INSERT INTO chats VALUES (?, ?, ?, ?)", [
            (ALLOWED, "Tesorería", 1, 100), (OTHER, "Tesorería privada", 1, 90),
            (ALIASED_PN, "Ro", 0, 80)])
        conn.execute("INSERT INTO aliases VALUES (?, ?)", (ALIASED_LID, ALIASED_PN))
        conn.executemany(
            "INSERT INTO messages (chat_key, id, sender_name, ts, text, source) VALUES (?, ?, ?, ?, ?, ?)", [
                (ALLOWED, "1", "Eitan", 100, "needle allowed", "live"),
                (ALLOWED, "2", "Eitan", 101, "will be edited", "live"),
                (ALLOWED, "3", "Eitan", 102, "will be deleted", "live"),
                (OTHER, "4", "X", 95, "needle secret", "live"),
                (ALIASED_PN, "5", "Ro", 80, "hola", "history")])
        conn.executemany("INSERT INTO mutations VALUES (?, ?, ?, ?, ?, ?)", [
            (ALLOWED, "2", "e1", "edit", 110, "edited text"),
            (ALLOWED, "3", "e2", "revoke", 111, "")])
        conn.commit()
        conn.close()

    def tearDown(self):
        self.tmp.cleanup()

    def write_allowlist(self, text):
        self.allowlist.write_text(text)

    def run_cli(self, *args):
        return subprocess.run([sys.executable, str(SCRIPT), *args], env=self.env,
                              capture_output=True, text=True)

    def test_missing_allowlist_refuses(self):
        for args in (["chats"], ["show", ALLOWED], ["search", "needle"]):
            r = self.run_cli(*args)
            self.assertNotEqual(r.returncode, 0, args)
            self.assertIn("no allowlist", r.stderr)

    def test_empty_and_malformed_allowlist_refuse(self):
        self.write_allowlist("# only a comment\n\n")
        self.assertIn("is empty", self.run_cli("chats").stderr)
        self.write_allowlist(f"{ALLOWED}\nTesorería\n")
        r = self.run_cli("show", ALLOWED)
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("malformed", r.stderr)

    def test_chats_lists_only_allowlisted(self):
        self.write_allowlist(f"{ALLOWED}  # Tesorería\n")
        out = self.run_cli("chats").stdout
        self.assertIn(ALLOWED, out)
        self.assertNotIn(OTHER, out)

    def test_show_refuses_unlisted_chat_by_jid_and_name(self):
        self.write_allowlist(f"{ALLOWED}\n")
        self.assertNotEqual(self.run_cli("show", OTHER).returncode, 0)
        # The name substring matches the unlisted chat too, but only the
        # allowlisted one is a candidate.
        r = self.run_cli("show", "Tesorería")
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertNotIn("needle secret", r.stdout)
        self.assertNotEqual(self.run_cli("show", "privada").returncode, 0)

    def test_search_is_confined_to_allowlist(self):
        self.write_allowlist(f"{ALLOWED}\n")
        out = self.run_cli("search", "needle").stdout
        self.assertIn("needle allowed", out)
        self.assertNotIn("needle secret", out)
        self.assertNotEqual(self.run_cli("search", "needle", "--chat", OTHER).returncode, 0)

    def test_edits_revokes_and_coverage_are_shown(self):
        self.write_allowlist(f"{ALLOWED}\n")
        out = self.run_cli("show", ALLOWED).stdout
        self.assertIn("edited text [edited]", out)
        self.assertNotIn("will be edited", out)
        self.assertIn("[deleted]", out)
        self.assertNotIn("will be deleted", out)
        self.assertIn("# coverage:", out)

    def test_lid_allowlist_entry_resolves_through_alias(self):
        self.write_allowlist(f"{ALIASED_LID}\n")
        r = self.run_cli("show", ALIASED_LID)
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertIn("hola", r.stdout)

    def test_unlinked_marker_warns(self):
        self.write_allowlist(f"{ALLOWED}\n")
        (Path(self.env["XDG_STATE_HOME"]) / "wa-archive" / "unlinked").write_text("x")
        self.assertIn("unlinked", self.run_cli("chats").stderr)

    def test_stale_or_missing_heartbeat_warns(self):
        self.write_allowlist(f"{ALLOWED}\n")
        self.assertIn("no wa-archive heartbeat", self.run_cli("chats").stderr)
        (Path(self.env["XDG_STATE_HOME"]) / "wa-archive" / "heartbeat.json").write_text(
            '{"event": "write", "at": "2020-01-01T00:00:00Z"}')
        self.assertIn("minutes old", self.run_cli("chats").stderr)

    def test_database_is_opened_read_only(self):
        self.write_allowlist(f"{ALLOWED}\n")
        before = self.db.read_bytes()
        self.run_cli("show", ALLOWED)
        self.run_cli("search", "needle")
        self.assertEqual(before, self.db.read_bytes())
        source = SCRIPT.read_text()
        self.assertIn("?mode=ro", source)
        for forbidden in ("INSERT", "UPDATE", "DELETE", "commit("):
            self.assertNotIn(forbidden, source)


if __name__ == "__main__":
    unittest.main()
