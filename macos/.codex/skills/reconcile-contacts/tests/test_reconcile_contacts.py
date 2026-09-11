#!/usr/bin/env python3
"""Owned synthetic reconciliation and strict batch-BBDB regression checks."""

from __future__ import annotations

import importlib.util
import contextlib
import io
import json
import os
import shutil
import signal
import sqlite3
import subprocess
import sys
import tempfile
import time
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest import mock


SCRIPT = Path(__file__).parents[1] / "scripts/reconcile-contacts.py"
SPEC = importlib.util.spec_from_file_location("reconcile_contacts", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


def record(name: str, email: str, birthday, uuid: str) -> dict:
    first, last = name.split(" ", 1)
    return {
        "first": first, "mid": "", "last": last, "maiden": "",
        "nick": "", "sfx": "", "org": [], "aka": [],
        "mail": [email], "urls": [], "phones": [],
        "birthday": birthday, "uuid": uuid,
    }


def apple_seconds(value: str) -> float:
    date = datetime.strptime(value, "%Y-%m-%d").replace(tzinfo=timezone.utc)
    return (date - MODULE.APPLE_REFERENCE_DATE).total_seconds()


class NormalizeBirthdayTests(unittest.TestCase):
    def test_normalizes_contacts_timestamp(self):
        self.assertEqual(
            MODULE.normalize_birthday(apple_seconds("1963-09-27")),
            "1963-09-27")

    def test_normalizes_yearless_encodings(self):
        self.assertEqual(MODULE.normalize_birthday("1604-07-12"), "07-12")
        self.assertEqual(
            MODULE.normalize_birthday(apple_seconds("1604-07-12")), "07-12")
        self.assertEqual(MODULE.normalize_birthday("07-12"), "07-12")

    def test_normalizes_legacy_bbdb_encodings(self):
        self.assertEqual(MODULE.normalize_birthday("22-01-1997"), "1997-01-22")
        self.assertEqual(MODULE.normalize_birthday("1974-1-20"), "1974-01-20")

    def test_rejects_unknown_encoding(self):
        with self.assertRaisesRegex(ValueError, "Unsupported birthday"):
            MODULE.normalize_birthday("July 12")


class BirthdayReconciliationTests(unittest.TestCase):
    def test_reports_all_mismatch_classes_and_fails_self_check(self):
        bbdb = [
            record("BBDB Only", "bbdb@example.com", "1990-01-02", "b1"),
            record("Contacts Only", "contacts@example.com", None, "b2"),
            record("Date Conflict", "conflict@example.com", "1992-04-10", "b3"),
            record("Yearless Match", "yearless@example.com", "07-12", "b4"),
        ]
        contacts = [
            record("BBDB Only", "bbdb@example.com", None, "c1"),
            record("Contacts Only", "contacts@example.com",
                   apple_seconds("1985-03-04"), "c2"),
            record("Date Conflict", "conflict@example.com",
                   apple_seconds("1987-04-10"), "c3"),
            record("Yearless Match", "yearless@example.com",
                   apple_seconds("1604-07-12"), "c4"),
        ]

        report = MODULE.reconcile(bbdb, contacts)

        self.assertEqual(len(report["birthday_bbdb_only"]), 1)
        self.assertEqual(len(report["birthday_contacts_only"]), 1)
        self.assertEqual(len(report["birthday_conflicts"]), 1)
        self.assertEqual(
            report["birthday_conflicts"][0]["contacts_birthday"],
            "1987-04-10")
        self.assertRegex(MODULE.self_check(report)[0], "3 reciprocal candidate")

    def test_ambiguous_legacy_dates_are_not_guessed(self):
        with self.assertRaisesRegex(ValueError, "Ambiguous"):
            MODULE.normalize_birthday("12-07-1604")

    def test_invalid_birthdays_are_reported_without_hiding_other_pairs(self):
        bbdb = [record("Invalid Date", "invalid@example.invalid", "July 12", "b1"),
                record("Valid Date", "valid@example.invalid", "1990-01-02", "b2")]
        contacts = [record("Invalid Date", "invalid@example.invalid", None, "c1"),
                    record("Valid Date", "valid@example.invalid", None, "c2")]
        report = MODULE.reconcile(bbdb, contacts)
        self.assertEqual(report["status"], "incomplete")
        self.assertEqual(len(report["birthday_errors"]), 1)
        self.assertEqual(len(report["birthday_bbdb_only"]), 1)

    def test_unknown_year_is_a_precision_difference_not_date_conflict(self):
        report = MODULE.reconcile([record("Owned Person", "owned@example.invalid", "07-12", "b1")],
                                  [record("Owned Person", "owned@example.invalid", "1990-07-12", "c1")])
        self.assertEqual(report["birthday_conflicts"], [])
        self.assertEqual(len(report["birthday_precision_differences"]), 1)
        self.assertEqual(report["status"], "review_required")


class IdentityTests(unittest.TestCase):
    def test_shared_email_and_cross_key_conflicts_cannot_look_clean(self):
        bbdb = [record("Alice Able", "shared@example.invalid", None, "b1"),
                record("Bob Baker", "shared@example.invalid", None, "b2")]
        contacts = [record("Alice Able", "shared@example.invalid", None, "c1"),
                    record("Bob Baker", "shared@example.invalid", None, "c2")]
        report = MODULE.reconcile(bbdb, contacts)
        self.assertEqual(report["status"], "review_required")
        self.assertTrue(any(row["kind"] == "mail" for row in report["ambiguous_keys"]))
        self.assertTrue(report["ambiguous_matches"])
        self.assertTrue(report["birthday_not_compared"])

    def test_nonlatin_names_remain_distinct_and_name_only_is_weak(self):
        self.assertNotEqual(MODULE.normalize("Анна Smith"), MODULE.normalize("Мария Smith"))
        self.assertEqual(MODULE.normalize("王 小明"), "王 小明")
        owned = record("王 小明", "", None, "b1")
        other = dict(owned, uuid="c1")
        report = MODULE.reconcile([owned], [other])
        self.assertEqual(report["status"], "review_required")
        self.assertEqual(report["candidate_matches"][0]["assessment"], "name_only_candidate")
        self.assertEqual(MODULE.normalize("Straße"), "strasse")

    def test_nickname_with_no_surname_does_not_add_a_bare_key(self):
        owned = record("Long First", "", None, "b1")
        owned.update(first="Long First", last="", nick="Short")
        self.assertNotIn("short", MODULE.name_variants(owned))
        owned.update(first="", nick="Short", org=["Shared Employer"])
        self.assertTrue(MODULE.is_person(owned))
        self.assertNotIn("name:shared employer", MODULE.match_keys(owned))

    def test_facebook_requires_an_exact_origin_and_profile_path(self):
        self.assertEqual(MODULE.facebook_id("https://www.facebook.com/alice?ref=fixture"), "alice")
        for url in ("https://facebook.com.evil.invalid/redirect/facebook.com/alice",
                    "https://evilfacebook.com/alice", "https://facebook.com/groups/123",
                    "https://user@facebook.com/alice", "https://facebook.com/alice/posts/1"):
            self.assertIsNone(MODULE.facebook_id(url))

    def test_small_unmatched_count_and_bad_ids_are_unresolved(self):
        bbdb = [record("Alice Able", "alice@example.invalid", None, "b1"),
                record("Bob Baker", "bob@example.invalid", None, "b2"),
                record("Cara Cole", "cara@example.invalid", None, "b3")]
        contacts = bbdb[:2] + [record("Dana Dale", "dana@example.invalid", None, "c3")]
        self.assertEqual(MODULE.reconcile(bbdb, contacts)["status"], "review_required")
        bbdb[0]["uuid"] = None
        self.assertEqual(MODULE.reconcile(bbdb, contacts)["status"], "incomplete")


# Entity numbers mirror the shape of the private store: several record kinds
# share ZABCDRECORD and only the contact entity is a contact card.
CONTACT_ENTITY = 22
ENTITY_NAMES = {19: "ABCDGroup", CONTACT_ENTITY: "ABCDContact", 24: "ABCDInfo", 25: "CNCDContainer"}


def create_contacts(path, records):
    with contextlib.closing(sqlite3.connect(path)) as connection:
        connection.executescript(
            "CREATE TABLE Z_PRIMARYKEY (Z_ENT INTEGER PRIMARY KEY,Z_NAME TEXT);"
            "CREATE TABLE ZABCDRECORD (Z_PK INTEGER PRIMARY KEY,Z_ENT INTEGER,ZFIRSTNAME TEXT,ZMIDDLENAME TEXT,"
            "ZLASTNAME TEXT,ZMAIDENNAME TEXT,ZNICKNAME TEXT,ZSUFFIX TEXT,ZORGANIZATION TEXT,ZBIRTHDAY,ZUNIQUEID TEXT);"
            "CREATE TABLE ZABCDEMAILADDRESS (ZOWNER INTEGER,ZADDRESS TEXT);"
            "CREATE TABLE ZABCDURLADDRESS (ZOWNER INTEGER,ZURL TEXT);"
            "CREATE TABLE ZABCDPHONENUMBER (ZOWNER INTEGER,ZLABEL TEXT,ZFULLNUMBER TEXT);")
        connection.executemany("INSERT INTO Z_PRIMARYKEY VALUES (?,?)", ENTITY_NAMES.items())
        for index, item in enumerate(records, 1):
            connection.execute("INSERT INTO ZABCDRECORD VALUES (?,?,?,?,?,?,?,?,?,?,?)",
                               (index, CONTACT_ENTITY, item['first'], item['mid'], item['last'], item['maiden'],
                                item['nick'], item['sfx'], '; '.join(item['org']), item['birthday'], item['uuid']))
            connection.executemany("INSERT INTO ZABCDEMAILADDRESS VALUES (?,?)", ((index, value) for value in item['mail']))
            connection.executemany("INSERT INTO ZABCDURLADDRESS VALUES (?,?)", ((index, value) for value in item['urls']))
            connection.executemany("INSERT INTO ZABCDPHONENUMBER VALUES (?,?,?)", ((index, label, number) for label, number in item['phones']))
        connection.commit()


VALID_BBDB = (';;; file-format: 9\n'
              '["Alice" "Able" nil nil nil nil nil ("alice@example.invalid") nil "b1" "2026-01-01" "2026-01-01" nil]\n')


class SourceTests(unittest.TestCase):
    def setUp(self):
        staging = tempfile.TemporaryDirectory(prefix="reconcile-source-tests-", dir=str(Path('/tmp').resolve()))
        self.addCleanup(staging.cleanup)
        self.root = Path(staging.name)
        self.db = self.root / "owned.sqlite"
        create_contacts(self.db, [record("Alice Able", "alice@example.invalid", None, "c1")])
        self.bbdb = self.root / "owned.bbdb"
        self.bbdb.write_text(VALID_BBDB)
        self.dump = SCRIPT.with_name("dump-bbdb.el")

    def test_encoded_sqlite_uri_cannot_create_a_fragment_prefix_file(self):
        selected = self.root / "owned#fragment?query.sqlite"
        create_contacts(selected, [record("Alice Able", "alice@example.invalid", None, "c1")])
        before = selected.read_bytes()
        records = MODULE.load_contacts(selected)
        self.assertEqual(len(records), 1)
        self.assertEqual(selected.read_bytes(), before)
        self.assertFalse((self.root / "owned").exists())
        with self.assertRaises(MODULE.InputError):
            MODULE.find_carddav_source()

    def test_unknown_population_and_orphans_are_explicit_incomplete_coverage(self):
        with contextlib.closing(sqlite3.connect(self.db)) as connection, connection:
            connection.execute("INSERT INTO ZABCDRECORD (Z_PK,Z_ENT,ZUNIQUEID) VALUES (2,22,'unprojected-id')")
            connection.execute("INSERT INTO ZABCDEMAILADDRESS VALUES (99,'orphan@example.invalid')")
        metadata = {}
        records = MODULE.load_contacts(self.db, metadata=metadata)
        self.assertEqual(len(records), 1)
        self.assertEqual(metadata['status'], 'incomplete')
        self.assertEqual(metadata['omitted_without_supported_fields'], 1)
        self.assertEqual(metadata['orphan_related_rows'], {'email': 1})
        with self.assertRaises(MODULE.InputError):
            MODULE.load_contacts(self.db)

    def test_non_contact_entity_rows_are_excluded_without_incompleteness(self):
        with contextlib.closing(sqlite3.connect(self.db)) as connection, connection:
            connection.executemany(
                "INSERT INTO ZABCDRECORD (Z_PK,Z_ENT,ZUNIQUEID) VALUES (?,?,?)",
                [(2, 19, 'group-1:ABGroup'), (3, 19, 'group-2:ABGroup'),
                 (4, 24, 'info:ABInfo'), (5, 25, 'container:ABContainer')])
        metadata = {}
        records = MODULE.load_contacts(self.db, metadata=metadata)
        self.assertEqual([item['uuid'] for item in records], ['c1'])
        self.assertEqual(metadata['status'], 'complete')
        self.assertEqual(metadata['records'], 1)
        self.assertEqual(metadata['omitted_without_supported_fields'], 0)
        self.assertEqual(metadata['non_contact_rows'],
                         {'ABCDGroup': 2, 'ABCDInfo': 1, 'CNCDContainer': 1})
        self.assertEqual(len(MODULE.load_contacts(self.db)), 1)
        with contextlib.closing(sqlite3.connect(self.db)) as connection, connection:
            connection.execute("INSERT INTO ZABCDEMAILADDRESS VALUES (2,'group-owned@example.invalid')")
        metadata = {}
        MODULE.load_contacts(self.db, metadata=metadata)
        self.assertEqual(metadata['status'], 'incomplete')
        self.assertEqual(metadata['orphan_related_rows'], {'email': 1})

    def test_mapped_unrecognized_record_entity_fails_closed(self):
        with contextlib.closing(sqlite3.connect(self.db)) as connection, connection:
            connection.execute("INSERT INTO Z_PRIMARYKEY VALUES (99,'FutureContactCard')")
            connection.execute(
                "INSERT INTO ZABCDRECORD (Z_PK,Z_ENT,ZFIRSTNAME,ZUNIQUEID) "
                "VALUES (2,99,'Synthetic future card','future-card')")
        metadata = {}
        with self.assertRaisesRegex(MODULE.InputError, 'Unsupported Contacts record entity'):
            MODULE.load_contacts(self.db, metadata=metadata)
        self.assertNotIn('status', metadata)
        with self.assertRaises(MODULE.InputError):
            MODULE.load_contacts(self.db)

    def test_unknown_or_missing_record_entities_fail_closed(self):
        with contextlib.closing(sqlite3.connect(self.db)) as connection, connection:
            connection.execute("INSERT INTO ZABCDRECORD (Z_PK,Z_ENT,ZFIRSTNAME,ZUNIQUEID) VALUES (2,99,'Nobody','c2')")
        with self.assertRaises(MODULE.InputError):
            MODULE.load_contacts(self.db, metadata={})
        with contextlib.closing(sqlite3.connect(self.db)) as connection, connection:
            connection.execute("UPDATE ZABCDRECORD SET Z_ENT=NULL WHERE Z_PK=2")
        with self.assertRaises(MODULE.InputError):
            MODULE.load_contacts(self.db, metadata={})
        with contextlib.closing(sqlite3.connect(self.db)) as connection, connection:
            connection.execute("DELETE FROM ZABCDRECORD WHERE Z_PK=2")
            connection.execute("DROP TABLE Z_PRIMARYKEY")
        with self.assertRaises(MODULE.InputError):
            MODULE.load_contacts(self.db, metadata={})

    def test_one_transaction_prevents_torn_cross_table_snapshot(self):
        writer = sqlite3.connect(self.db)
        self.addCleanup(writer.close)
        writer.execute("PRAGMA journal_mode=WAL")
        writer.commit()
        original = sqlite3.connect
        changed = []
        class Connection:
            def __init__(self, actual):
                self.actual = actual
            def __getattr__(self, name):
                return getattr(self.actual, name)
            def execute(self, sql, *args):
                if sql.startswith("select ZOWNER, ZADDRESS") and not changed:
                    changed.append(True)
                    writer.execute("UPDATE ZABCDRECORD SET ZFIRSTNAME='Changed'")
                    writer.execute("UPDATE ZABCDEMAILADDRESS SET ZADDRESS='changed@example.invalid'")
                    writer.commit()
                return self.actual.execute(sql, *args)
        with mock.patch.object(MODULE.sqlite3, "connect", side_effect=lambda *args, **kwargs: Connection(original(*args, **kwargs))):
            records = MODULE.load_contacts(self.db)
        self.assertTrue(changed)
        self.assertEqual(records[0]['first'], 'Alice')
        self.assertEqual(records[0]['mail'], ['alice@example.invalid'])
        self.assertEqual(writer.execute("SELECT ZFIRSTNAME FROM ZABCDRECORD").fetchone(), ('Changed',))

    def test_connection_is_closed_on_schema_failure(self):
        broken = self.root / "wrong-schema.sqlite"
        original = sqlite3.connect
        original(broken).close()
        captured = []
        def connect(*args, **kwargs):
            connection = original(*args, **kwargs)
            captured.append(connection)
            return connection
        with mock.patch.object(MODULE.sqlite3, "connect", side_effect=connect):
            with self.assertRaises(MODULE.InputError):
                MODULE.load_contacts(broken)
        with self.assertRaises(sqlite3.ProgrammingError):
            captured[0].execute("SELECT 1")

    def test_duplicate_ids_and_query_limits_fail_closed(self):
        with contextlib.closing(sqlite3.connect(self.db)) as connection, connection:
            connection.execute("INSERT INTO ZABCDRECORD (Z_PK,Z_ENT,ZFIRSTNAME,ZUNIQUEID) VALUES (2,22,'Other','c1')")
        with self.assertRaisesRegex(MODULE.InputError, 'duplicate'):
            MODULE.load_contacts(self.db)
        with mock.patch.object(MODULE, "MAX_CONTACT_ROWS", 1):
            with self.assertRaisesRegex(MODULE.InputError, 'row limit'):
                MODULE.load_contacts(self.db)

    def test_special_and_oversized_bbdb_sources_fail_before_emacs(self):
        fifo = self.root / "owned.fifo"
        os.mkfifo(fifo)
        with self.assertRaises(MODULE.InputError):
            MODULE.bbdb_source_bytes(fifo)
        with mock.patch.object(MODULE, 'run_bbdb_dump') as run:
            with self.assertRaises(MODULE.InputError):
                MODULE.load_bbdb(fifo, self.dump)
            with mock.patch.object(MODULE, 'MAX_BBDB_BYTES', 10):
                with self.assertRaises(MODULE.InputError):
                    MODULE.load_bbdb(self.bbdb, self.dump)
            run.assert_not_called()

    @unittest.skipUnless(shutil.which('emacs'), 'fresh batch Emacs unavailable')
    def test_unquoted_iso_shaped_birthday_stays_an_unsupported_type(self):
        self.bbdb.write_text(VALID_BBDB.replace(') nil "b1"', ') ((birthday . 1980-02-03)) "b1"'))
        metadata = {}
        bbdb = MODULE.load_bbdb(self.bbdb, self.dump, metadata=metadata)
        self.assertEqual(bbdb[0]['birthday_type'], 'unsupported')
        self.assertEqual(metadata['status'], 'incomplete')
        self.assertEqual(metadata['unsupported_birthday_types'], 1)
        report = MODULE.reconcile(bbdb, [record('Alice Able', 'alice@example.invalid', None, 'c1')])
        self.assertEqual(report['status'], 'incomplete')
        self.assertEqual(len(report['birthday_errors']), 1)
        self.assertEqual(report['birthday_bbdb_only'], [])

    @unittest.skipUnless(shutil.which('emacs'), 'fresh batch Emacs unavailable')
    def test_mutation_after_snapshot_is_not_parsed_or_overwritten(self):
        before = self.bbdb.read_bytes()
        changed = before.replace(b'Alice', b'Changed')
        original = MODULE.run_bbdb_dump
        def run(*args, **kwargs):
            self.bbdb.write_bytes(changed)
            self.assertEqual(Path(kwargs['env']['BBDB_FILE']).read_bytes(), before)
            checked = original(*args, **kwargs)
            self.assertEqual(json.loads(checked.stdout)[0]['first'], 'Alice')
            return checked
        with mock.patch.object(MODULE, 'run_bbdb_dump', side_effect=run):
            with self.assertRaisesRegex(MODULE.InputError, 'changed'):
                MODULE.load_bbdb(self.bbdb, self.dump)
        self.assertEqual(self.bbdb.read_bytes(), changed)

    @unittest.skipUnless(shutil.which('emacs'), 'fresh batch Emacs unavailable')
    def test_native_strict_dump_rejects_corruption_without_prefix_or_file_edits(self):
        body = VALID_BBDB.split('\n', 1)[1]
        cases = {'junk': VALID_BBDB + ')\n' + body.replace('b1','b2'),
                 'truncated': VALID_BBDB + '["unfinished"',
                 'wrong_format': VALID_BBDB.replace('format: 9', 'format: 8'),
                 'missing_header': body,
                 'indented': VALID_BBDB.replace('["Alice"', ' ["Alice"'),
                 'duplicate_ids': VALID_BBDB + body,
                 'inter_record_blank': VALID_BBDB + '\n' + body.replace('b1', 'b2'),
                 'inter_record_comment': VALID_BBDB + '; comment\n' + body.replace('b1', 'b2'),
                 'persisted_cache': VALID_BBDB.replace('"2026-01-01" nil]', '"2026-01-01" [nil]]'),
                 'executable_form': VALID_BBDB + '(error "must not be evaluated")\n',
                 'bad_phone': VALID_BBDB.replace('nil nil nil ("alice', 'nil (["bad"]) nil ("alice')}
        for label, content in cases.items():
            with self.subTest(case=label):
                self.bbdb.write_text(content)
                before = self.bbdb.read_bytes()
                result = subprocess.run(['emacs', '-Q', '--batch', '-l', str(self.dump)],
                                        env=dict(os.environ, BBDB_FILE=str(self.bbdb)), capture_output=True, timeout=20)
                self.assertEqual(result.returncode, 2)
                self.assertEqual(result.stdout, b'')
                self.assertEqual(self.bbdb.read_bytes(), before)

    @unittest.skipUnless(shutil.which('emacs'), 'fresh batch Emacs unavailable')
    def test_native_snapshot_preserves_affix_phone_components_and_uuid(self):
        self.bbdb.write_text(VALID_BBDB.replace('"Able" nil nil nil nil nil',
                                              '"Able" ("Jr.") nil nil (["mobile" 212 12 1234 0]) nil'))
        original = MODULE.run_bbdb_dump
        snapshots = []
        def run(*args, **kwargs):
            snapshot = Path(kwargs['env']['BBDB_FILE'])
            snapshots.append(snapshot)
            self.assertNotEqual(snapshot, self.bbdb)
            self.assertEqual(snapshot.read_bytes(), self.bbdb.read_bytes())
            self.assertEqual(snapshot.stat().st_mode & 0o777, 0o600)
            self.assertEqual(snapshot.parent.stat().st_mode & 0o777, 0o700)
            return original(*args, **kwargs)
        metadata = {}
        before = self.bbdb.read_bytes()
        with mock.patch.object(MODULE, 'run_bbdb_dump', side_effect=run):
            records = MODULE.load_bbdb(self.bbdb, self.dump, metadata=metadata)
        self.assertEqual(records[0]['uuid'], 'b1')
        self.assertEqual(records[0]['sfx'], 'Jr.')
        self.assertEqual(records[0]['affix'], ['Jr.'])
        self.assertEqual(records[0]['phones'], [('mobile', '212-012-1234')])
        self.assertEqual(records[0]['phone_components'], [['mobile', 212, 12, 1234, 0]])
        self.assertEqual(metadata['status'], 'complete')
        self.assertEqual(self.bbdb.read_bytes(), before)
        self.assertTrue(all(not path.exists() for path in snapshots))

    @unittest.skipUnless(shutil.which('emacs'), 'fresh batch Emacs unavailable')
    def test_public_cli_explicit_sources_exit_status_and_no_source_defaults(self):
        rejected = subprocess.run([sys.executable, '-I', '-B', str(SCRIPT), '--json'], capture_output=True, timeout=10)
        self.assertEqual(rejected.returncode, 2)
        checked = subprocess.run([sys.executable, '-I', '-B', str(SCRIPT), '--bbdb-file', str(self.bbdb),
                                  '--contacts-db', str(self.db), '--json'], capture_output=True, timeout=20)
        self.assertEqual(checked.returncode, 0, checked.stderr)
        report = json.loads(checked.stdout)
        self.assertEqual(report['status'], 'no_reported_differences')
        self.assertEqual(report['sources']['contacts']['status'], 'complete')
        with contextlib.closing(sqlite3.connect(self.db)) as connection, connection:
            connection.execute("INSERT INTO ZABCDRECORD (Z_PK,Z_ENT,ZUNIQUEID) VALUES (2,22,'unknown-fields')")
        checked = subprocess.run([sys.executable, '-I', '-B', str(SCRIPT), '--bbdb-file', str(self.bbdb),
                                  '--contacts-db', str(self.db), '--json'], capture_output=True, timeout=20)
        self.assertEqual(checked.returncode, 1, checked.stderr)
        report = json.loads(checked.stdout)
        self.assertEqual(report['status'], 'incomplete')
        self.assertTrue(report['coverage']['source_problems'])

    def test_text_nested_entries_are_bounded_and_escape_controls(self):
        owned = record('Long Name', 'owned@example.invalid', None, 'b1')
        owned['first'] = '\x1b[31m' + 'x' * 5000
        metadata = {'status': 'complete'}
        def loaded(*_args, **kwargs):
            kwargs['metadata'].update(metadata)
            return [owned]
        output = io.StringIO()
        with mock.patch.object(MODULE, 'load_bbdb', side_effect=loaded), \
                mock.patch.object(MODULE, 'load_contacts', side_effect=loaded), contextlib.redirect_stdout(output):
            MODULE.main(['--bbdb-file', str(self.bbdb), '--contacts-db', str(self.db)])
        self.assertNotIn('\x1b', output.getvalue())
        self.assertIn('[truncated; use --json]', output.getvalue())
        self.assertLess(max(map(len, output.getvalue().splitlines())), 1100)

    def test_owned_non_emacs_wrapper_timeout_reaps_descendants_not_neighbor(self):
        marker = self.root / 'owned-wrapper-pids.json'
        wrapper = self.root / 'owned-wrapper.py'
        wrapper.write_text(
            'import json,os,subprocess,sys,time\n'
            'from pathlib import Path\n'
            'child=subprocess.Popen([sys.executable,"-I","-B","-c","import time; time.sleep(30)"])\n'
            f'Path({str(marker)!r}).write_text(json.dumps([os.getpid(),child.pid]))\n'
            'child.wait()\n')
        neighbor = subprocess.Popen([sys.executable, '-I', '-B', '-c', 'import time; time.sleep(30)'])
        prior = {kind: signal.getsignal(kind) for kind in (signal.SIGTERM, signal.SIGHUP, signal.SIGINT)}
        try:
            started = time.monotonic()
            with self.assertRaisesRegex(MODULE.InputError, 'deadline'):
                MODULE.run_bbdb_dump([sys.executable, '-I', '-B', str(wrapper)], timeout=0.5)
            self.assertLess(time.monotonic() - started, 4)
            self.assertIsNone(neighbor.poll())
            for pid in json.loads(marker.read_text()):
                deadline = time.monotonic() + 2
                while time.monotonic() < deadline:
                    try:
                        os.kill(pid, 0)
                    except ProcessLookupError:
                        break
                    time.sleep(0.02)
                else:
                    self.fail('owned fixture descendant survived group timeout')
        finally:
            neighbor.terminate()
            neighbor.wait(timeout=5)
        self.assertEqual({kind: signal.getsignal(kind) for kind in prior}, prior)

    def test_cancellation_handlers_restore_after_spawn_failure(self):
        prior = {kind: signal.getsignal(kind) for kind in (signal.SIGTERM, signal.SIGHUP, signal.SIGINT)}
        with self.assertRaises(OSError):
            MODULE.run_bbdb_dump([str(self.root / 'nonexistent-owned-parser')])
        self.assertEqual({kind: signal.getsignal(kind) for kind in prior}, prior)

    def test_public_cancellation_stops_only_owned_fake_parser_and_cleans_snapshot(self):
        for signum in (signal.SIGTERM, signal.SIGHUP, signal.SIGINT):
            with self.subTest(signal=signum):
                self._assert_public_cancellation(signum)

    def _assert_public_cancellation(self, signum):
        fake_bin = self.root / ('owned-bin-' + str(signum))
        fake_bin.mkdir()
        marker = self.root / ('fake-parser-ready-' + str(signum) + '.json')
        fake = fake_bin / 'emacs'
        fake.write_text(
            '#!/usr/bin/python3\nimport json,os,subprocess,sys,time\nfrom pathlib import Path\n'
            'child=subprocess.Popen([sys.executable,"-I","-B","-c","import time; time.sleep(30)"])\n'
            f'Path({str(marker)!r}).write_text(json.dumps({{"pids":[os.getpid(),child.pid],"snapshot":os.environ["BBDB_FILE"]}}))\n'
            'child.wait()\n')
        fake.chmod(0o700)
        neighbor = subprocess.Popen([sys.executable, '-I', '-B', '-c', 'import time; time.sleep(30)'])
        env = dict(os.environ, PATH=str(fake_bin) + os.pathsep + os.environ.get('PATH', ''))
        running = subprocess.Popen([sys.executable, '-I', '-B', str(SCRIPT), '--bbdb-file', str(self.bbdb),
                                    '--contacts-db', str(self.db), '--json'],
                                   env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        before = self.bbdb.read_bytes()
        try:
            deadline = time.monotonic() + 10
            while not marker.exists() and running.poll() is None and time.monotonic() < deadline:
                time.sleep(0.02)
            self.assertTrue(marker.exists(), 'owned fake parser never reached readiness')
            owned = json.loads(marker.read_text())
            running.send_signal(signum)
            stdout, stderr = running.communicate(timeout=10)
            self.assertEqual(running.returncode, 128 + signum, stderr)
            self.assertEqual(json.loads(stdout)['status'], 'interrupted')
            self.assertEqual(json.loads(stdout)['signal'], signum)
            self.assertFalse(Path(owned['snapshot']).exists())
            self.assertEqual(self.bbdb.read_bytes(), before)
            self.assertIsNone(neighbor.poll())
            for pid in owned['pids']:
                deadline = time.monotonic() + 2
                while time.monotonic() < deadline:
                    try:
                        os.kill(pid, 0)
                    except ProcessLookupError:
                        break
                    time.sleep(0.02)
                else:
                    self.fail('owned fake parser descendant survived cancellation')
        finally:
            if running.poll() is None:
                running.terminate()
            running.communicate(timeout=10)
            neighbor.terminate()
            neighbor.wait(timeout=5)

    @unittest.skipUnless(os.environ.get('RECONCILE_BBDB_SOURCE') and shutil.which('emacs'),
                         'explicit representative BBDB source not selected')
    def test_representative_bbdb_loader_accepts_and_roundtrips_owned_format9(self):
        source = Path(os.environ['RECONCILE_BBDB_SOURCE']).resolve()
        library = source / 'lisp/bbdb.el'
        self.assertTrue(library.is_file())
        roundtrip = self.root / 'roundtrip.bbdb'
        driver = self.root / 'roundtrip.el'
        driver.write_text(
            "(require 'cl-lib)\n"
            f"(setq user-emacs-directory {json.dumps(str(self.root) + '/')})\n"
            f"(setq bbdb-file {json.dumps(str(self.bbdb))} bbdb-file-remote nil bbdb-check-auto-save-file nil bbdb-read-only nil)\n"
            f"(load {json.dumps(str(library))} nil t t)\n"
            "(let ((records (bbdb-records)))\n"
            "  (unless (and (= (length records) 1) (equal (bbdb-record-uuid (car records)) \"b1\")) (error \"Fixture identity mismatch\"))\n"
            "  (bbdb-overwrite-record-internal (car records))\n"
            "  (with-current-buffer (bbdb-buffer)\n"
            f"    (write-region (point-min) (point-max) {json.dumps(str(roundtrip))} nil 'silent)\n"
            "    (set-buffer-modified-p nil)))\n")
        before = self.bbdb.read_bytes()
        checked = subprocess.run(['emacs', '-Q', '--batch', '-L', str(source / 'lisp'), '-l', str(driver)],
                                 capture_output=True, timeout=20)
        self.assertEqual(checked.returncode, 0, checked.stderr)
        self.assertEqual(self.bbdb.read_bytes(), before)
        self.assertEqual(MODULE.load_bbdb(roundtrip, self.dump), MODULE.load_bbdb(self.bbdb, self.dump))

    @unittest.skipUnless(os.environ.get('RECONCILE_BBDB_SOURCE') and shutil.which('emacs'),
                         'explicit representative BBDB source not selected')
    def test_native_bbdb_and_strict_dump_both_reject_inter_record_trivia(self):
        source = Path(os.environ['RECONCILE_BBDB_SOURCE']).resolve()
        library = source / 'lisp/bbdb.el'
        driver = self.root / 'reject-gap.el'
        driver.write_text(
            f"(setq user-emacs-directory {json.dumps(str(self.root) + '/')})\n"
            f"(setq bbdb-file {json.dumps(str(self.bbdb))} bbdb-file-remote nil bbdb-check-auto-save-file nil bbdb-read-only t)\n"
            f"(load {json.dumps(str(library))} nil t t)\n(bbdb-records)\n")
        second = VALID_BBDB.split('\n', 1)[1].replace('b1', 'b2').replace('Alice', 'Bob')
        for gap in ('\n', '; inter-record comment\n'):
            self.bbdb.write_text(VALID_BBDB + gap + second)
            before = self.bbdb.read_bytes()
            native = subprocess.run(['emacs', '-Q', '--batch', '-L', str(source / 'lisp'), '-l', str(driver)],
                                    capture_output=True, timeout=20)
            self.assertNotEqual(native.returncode, 0)
            strict = subprocess.run(['emacs', '-Q', '--batch', '-l', str(self.dump)],
                                    env=dict(os.environ, BBDB_FILE=str(self.bbdb)), capture_output=True, timeout=20)
            self.assertEqual(strict.returncode, 2)
            self.assertEqual(strict.stdout, b'')
            self.assertEqual(self.bbdb.read_bytes(), before)

    def test_skips_non_unique_identity_matches(self):
        bbdb = [record("Shared Name", "one@example.com", "1990-01-02", "b1")]
        contacts = [
            record("Shared Name", "one@example.com", None, "c1"),
            record("Shared Name", "two@example.com", "1990-01-02", "c2"),
        ]

        report = MODULE.reconcile(bbdb, contacts)

        self.assertEqual(report["birthday_bbdb_only"], [])
        self.assertEqual(report["birthday_contacts_only"], [])
        self.assertEqual(report["birthday_conflicts"], [])


if __name__ == "__main__":
    unittest.main()
