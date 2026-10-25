import importlib.machinery
import importlib.util
import io
import json
import os
import pathlib
import sqlite3
import tempfile
import unittest
from contextlib import redirect_stdout


ROOT = pathlib.Path(__file__).resolve().parents[1]
SKILLS = [ROOT / "macos" / tool / "skills" / "security-audit" for tool in (".claude", ".codex")]
SCRIPT = SKILLS[0] / "scripts" / "tcc-grants.py"


def load_module():
    loader = importlib.machinery.SourceFileLoader("tcc_grants", str(SCRIPT))
    spec = importlib.util.spec_from_loader("tcc_grants", loader)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def make_db(path, rows):
    """Create a synthetic TCC database with the columns the helper reads."""
    connection = sqlite3.connect(path)
    connection.execute("CREATE TABLE access (service TEXT, client TEXT, client_type INTEGER, "
                       "auth_value INTEGER, indirect_object_identifier TEXT, last_modified INTEGER)")
    connection.executemany("INSERT INTO access VALUES (?, ?, ?, ?, ?, ?)", rows)
    connection.commit()
    connection.close()


class TccGrantsTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()
        self.directory = tempfile.TemporaryDirectory()
        self.base = pathlib.Path(self.directory.name)

    def tearDown(self):
        self.directory.cleanup()

    def report(self, *paths):
        output = io.StringIO()
        with redirect_stdout(output):
            self.mod.main([arg for path in paths for arg in ("--db", str(path))])
        return json.loads(output.getvalue())["databases"]

    def test_osascript_accessibility_is_flagged_as_generic_executor(self):
        db = self.base / "TCC.db"
        make_db(db, [("kTCCServiceAccessibility", "/usr/bin/osascript", 1, 2, None, 1699857331)])
        [result] = self.report(db)
        self.assertEqual(result["status"], "read")
        [grant] = result["grants"]
        self.assertEqual(grant["service"], "Accessibility")
        self.assertTrue(grant["generic_executor"])
        self.assertEqual(grant["granted"], "2023-11-13")

    def test_versioned_interpreter_full_disk_access_is_flagged_and_stale_detected(self):
        db = self.base / "TCC.db"
        missing = str(self.base / "Cellar/python@3.14/3.14.7/bin/python3.14")
        make_db(db, [("kTCCServiceSystemPolicyAllFiles", missing, 1, 2, None, 0)])
        [grant] = self.report(db)[0]["grants"]
        self.assertTrue(grant["generic_executor"])
        self.assertTrue(grant["stale"])

    def test_terminal_automation_keeps_its_target(self):
        db = self.base / "TCC.db"
        make_db(db, [("kTCCServiceAppleEvents", "com.apple.Terminal", 0, 2,
                      "com.apple.systemevents", 0)])
        [grant] = self.report(db)[0]["grants"]
        self.assertTrue(grant["generic_executor"])
        self.assertEqual(grant["target"], "com.apple.systemevents")

    def test_agent_app_accessibility_is_flagged(self):
        db = self.base / "TCC.db"
        make_db(db, [("kTCCServiceAccessibility", "com.openai.codex", 0, 2, None, 0)])
        [grant] = self.report(db)[0]["grants"]
        self.assertTrue(grant["generic_executor"])

    def test_specific_app_denied_grant_and_other_service_are_not_flagged(self):
        db = self.base / "TCC.db"
        make_db(db, [("kTCCServiceAccessibility", "com.knollsoft.Rectangle", 0, 2, None, 0),
                     ("kTCCServiceAccessibility", "/usr/bin/osascript", 1, 0, None, 0),
                     ("kTCCServiceMicrophone", "/usr/bin/osascript", 1, 2, None, 0)])
        grants = self.report(db)[0]["grants"]
        self.assertEqual([(g["client"], g["generic_executor"]) for g in grants],
                         [("com.knollsoft.Rectangle", False)])

    def test_unreadable_database_is_reported_not_treated_as_empty(self):
        db = self.base / "TCC.db"
        db.write_bytes(b"not a database")
        [result] = self.report(db)
        self.assertEqual(result["status"], "unreadable")
        self.assertTrue(result["reason"])

    def test_missing_database_is_distinct_from_empty(self):
        [result] = self.report(self.base / "absent.db")
        self.assertEqual(result["status"], "missing")

    def test_helper_does_not_modify_the_database(self):
        db = self.base / "TCC.db"
        make_db(db, [("kTCCServiceAccessibility", "/usr/bin/osascript", 1, 2, None, 0)])
        before = (db.read_bytes(), os.stat(db).st_mtime_ns)
        self.report(db)
        self.assertEqual((db.read_bytes(), os.stat(db).st_mtime_ns), before)

    def container_db(self, uuid="ADAFCFCA-5A2B-45A7-BF44-FBDC9A1A9D84"):
        return (self.mod.CONTAINER_PREFIX + uuid + "/Data" + self.mod.CONTAINER_SUFFIX)

    def test_lsof_output_yields_only_container_tcc_databases(self):
        container = self.container_db()
        output = "\n".join(["p766", "fcwd", "n/", "ftxt", "n/usr/lib/dyld", "f3",
                            "n" + container, "f4", "n" + container,
                            "f5", "n/Library/Application Support/com.apple.TCC/TCC.db"])
        self.assertEqual(self.mod.tcc_paths_from_lsof(output), [container])

    def test_legacy_user_database_is_read_when_present(self):
        legacy = self.base / "TCC.db"
        make_db(legacy, [("kTCCServiceAppleEvents", "com.apple.Terminal", 0, 2,
                          "com.apple.finder", 0)])
        result = self.mod.user_db_report(legacy, discover=lambda: self.fail("no lookup"))
        self.assertEqual(result["status"], "read")
        self.assertEqual(result["grants"][0]["target"], "com.apple.finder")

    def test_container_user_database_is_read_when_legacy_path_is_gone(self):
        # Synthetic stand-in for the macOS 27 container: the discovered path is read.
        container = self.base / "container" / "TCC.db"
        container.parent.mkdir()
        make_db(container, [("kTCCServiceAppleEvents", "com.apple.Terminal", 0, 2,
                             "com.apple.systemevents", 0)])
        result = self.mod.user_db_report(self.base / "absent.db",
                                         discover=lambda: ([str(container)], None))
        self.assertEqual(result["status"], "read")
        self.assertIn("container", result["location"])
        self.assertEqual(result["grants"][0]["target"], "com.apple.systemevents")

    def test_unknown_user_location_is_not_reported_as_missing(self):
        result = self.mod.user_db_report(self.base / "absent.db", discover=lambda: ([], None))
        self.assertEqual(result["status"], "location-unknown")
        self.assertIn("no container TCC.db", result["reason"])

    def test_failed_container_lookup_is_reported_as_unknown_with_its_error(self):
        result = self.mod.user_db_report(self.base / "absent.db",
                                         discover=lambda: ([], "lsof failed: boom"))
        self.assertEqual(result["status"], "location-unknown")
        self.assertIn("lsof failed: boom", result["reason"])

    def test_ambiguous_container_lookup_is_reported_as_unknown(self):
        result = self.mod.user_db_report(
            self.base / "absent.db",
            discover=lambda: ([self.container_db("A"), self.container_db("B")], None))
        self.assertEqual(result["status"], "location-unknown")

    def test_denied_path_lookup_is_unreadable_not_missing(self):
        locked = self.base / "locked"
        locked.mkdir()
        db = locked / "TCC.db"
        make_db(db, [])
        locked.chmod(0)
        try:
            [result] = self.report(db)
        finally:
            locked.chmod(0o700)
        self.assertEqual(result["status"], "unreadable")

    def test_claude_and_codex_copies_are_identical(self):
        self.assertEqual(*(skill.joinpath("scripts/tcc-grants.py").read_bytes() for skill in SKILLS))


if __name__ == "__main__":
    unittest.main()
