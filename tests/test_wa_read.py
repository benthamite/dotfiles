"""Tests for bin/wa-read: deny-list enforcement, read-only access, display."""

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

OPEN = "111@g.us"
DENIED = "222@g.us"
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
        self.denylist = root / "config" / "wa-archive" / "denylist"
        conn = sqlite3.connect(self.db)
        conn.executescript(SCHEMA)
        conn.executemany("INSERT INTO chats VALUES (?, ?, ?, ?)", [
            (OPEN, "Tesorería", 1, 100), (DENIED, "Tesorería privada", 1, 90),
            (ALIASED_PN, "Ro", 0, 80)])
        conn.execute("INSERT INTO aliases VALUES (?, ?)", (ALIASED_LID, ALIASED_PN))
        conn.executemany(
            "INSERT INTO messages (chat_key, id, sender_name, ts, text, source) VALUES (?, ?, ?, ?, ?, ?)", [
                (OPEN, "1", "Eitan", 100, "needle open", "live"),
                (OPEN, "2", "Eitan", 101, "will be edited", "live"),
                (OPEN, "3", "Eitan", 102, "will be deleted", "live"),
                (DENIED, "4", "X", 95, "needle secret", "live"),
                (ALIASED_PN, "5", "Ro", 80, "hola needle", "history")])
        conn.executemany("INSERT INTO mutations VALUES (?, ?, ?, ?, ?, ?)", [
            (OPEN, "2", "e1", "edit", 110, "edited text"),
            (OPEN, "3", "e2", "revoke", 111, "")])
        conn.commit()
        conn.close()

    def tearDown(self):
        self.tmp.cleanup()

    def write_denylist(self, text):
        self.denylist.write_text(text)

    def run_cli(self, *args):
        return subprocess.run([sys.executable, str(SCRIPT), *args], env=self.env,
                              capture_output=True, text=True)

    def test_without_denylist_every_chat_is_readable(self):
        out = self.run_cli("chats").stdout
        for key in (OPEN, DENIED, ALIASED_PN):
            self.assertIn(key, out)
        self.assertIn("needle secret", self.run_cli("search", "needle").stdout)

    def test_malformed_denylist_refuses(self):
        self.write_denylist(f"{DENIED}\nTesorería privada\n")
        r = self.run_cli("show", OPEN)
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("malformed", r.stderr)

    def test_denied_chat_is_hidden_from_chats_show_and_search(self):
        self.write_denylist(f"{DENIED}  # Tesorería privada\n")
        self.assertNotIn(DENIED, self.run_cli("chats").stdout)
        self.assertNotEqual(self.run_cli("show", DENIED).returncode, 0)
        self.assertNotEqual(self.run_cli("show", "privada").returncode, 0)
        out = self.run_cli("search", "needle").stdout
        self.assertIn("needle open", out)
        self.assertNotIn("needle secret", out)
        self.assertNotEqual(self.run_cli("search", "needle", "--chat", DENIED).returncode, 0)

    def test_name_substring_must_match_one_chat(self):
        r = self.run_cli("show", "Tesorería")
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("matches 2 chats", r.stderr)

    def test_denylisted_lid_hides_its_canonical_chat(self):
        self.write_denylist(f"{ALIASED_LID}\n")
        self.assertNotEqual(self.run_cli("show", ALIASED_PN).returncode, 0)
        self.assertNotIn("hola needle", self.run_cli("search", "needle").stdout)

    def test_lid_resolves_through_alias(self):
        r = self.run_cli("show", ALIASED_LID)
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertIn("hola", r.stdout)

    def test_edits_revokes_and_coverage_are_shown(self):
        out = self.run_cli("show", OPEN).stdout
        self.assertIn("edited text [edited]", out)
        self.assertNotIn("will be edited", out)
        self.assertIn("[deleted]", out)
        self.assertNotIn("will be deleted", out)
        self.assertIn("# coverage:", out)

    def test_unlinked_marker_warns(self):
        (Path(self.env["XDG_STATE_HOME"]) / "wa-archive" / "unlinked").write_text("x")
        self.assertIn("unlinked", self.run_cli("chats").stderr)

    def test_stale_or_missing_heartbeat_warns(self):
        self.assertIn("no wa-archive heartbeat", self.run_cli("chats").stderr)
        (Path(self.env["XDG_STATE_HOME"]) / "wa-archive" / "heartbeat.json").write_text(
            '{"event": "write", "at": "2020-01-01T00:00:00Z"}')
        self.assertIn("minutes old", self.run_cli("chats").stderr)

    def test_database_is_opened_read_only(self):
        before = self.db.read_bytes()
        self.run_cli("show", OPEN)
        self.run_cli("search", "needle")
        self.assertEqual(before, self.db.read_bytes())
        source = SCRIPT.read_text()
        self.assertIn("?mode=ro", source)
        for forbidden in ("INSERT", "UPDATE", "DELETE", "commit("):
            self.assertNotIn(forbidden, source)


if __name__ == "__main__":
    unittest.main()
