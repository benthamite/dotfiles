import datetime as dt
import importlib.machinery
import importlib.util
import io
import json
import tempfile
import unittest
import urllib.error
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin/sync-google-birthdays"
LOADER = importlib.machinery.SourceFileLoader("sync_google_birthdays", str(SCRIPT))
SPEC = importlib.util.spec_from_loader(LOADER.name, LOADER)
MODULE = importlib.util.module_from_spec(SPEC)
LOADER.exec_module(MODULE)


class BirthdayTests(unittest.TestCase):
    def test_reconciliation_guard_rejects_any_difference(self):
        report = {
            "bbdb_count": 2800,
            "contacts_count": 2800,
            "birthday_conflicts": [{"name": "Conflict"}],
            "self_check_problems": ["one conflict"],
        }
        completed = mock.Mock(returncode=1, stdout=json.dumps(report), stderr="")
        with mock.patch.object(MODULE.subprocess, "run", return_value=completed):
            with self.assertRaisesRegex(RuntimeError, "not reconciled"):
                MODULE.assert_reconciled(Path("reconcile.py"))

    def test_reconciliation_attestation_accepts_only_exact_bbdb_snapshot(self):
        report = {"bbdb_count": 2800, "contacts_count": 2800}
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            bbdb = directory / "bbdb.el"
            attestation = directory / "reconciliation.json"
            bbdb.write_text("reconciled content")
            MODULE.write_reconciliation_attestation(report, attestation, bbdb)
            MODULE.assert_reconciliation_attested(attestation, bbdb)
            bbdb.write_text("changed content")
            with self.assertRaisesRegex(RuntimeError, "changed since"):
                MODULE.assert_reconciliation_attested(attestation, bbdb)

    def test_normalizes_supported_birthday_formats(self):
        self.assertEqual(MODULE.normalize_birthday("1992-04-10"), (1992, 4, 10))
        self.assertEqual(MODULE.normalize_birthday("22-01-1997"), (1997, 1, 22))
        self.assertEqual(MODULE.normalize_birthday("1974-1-20"), (1974, 1, 20))
        self.assertEqual(MODULE.normalize_birthday("07-12"), (None, 7, 12))

    def test_event_is_recurring_all_day_private_and_reminder_free(self):
        record = {
            "stable_id": "uuid-1", "display_name": "Lucía Ejemplo",
            "year": 1986, "month": 7, "day": 12,
        }
        event = MODULE.desired_event(record)
        self.assertEqual(event["summary"], "Lucía Ejemplo's birthday")
        self.assertEqual(event["start"], {"date": "2000-07-12"})
        self.assertEqual(event["end"], {"date": "2000-07-13"})
        self.assertEqual(event["recurrence"], ["RRULE:FREQ=YEARLY"])
        self.assertEqual(event["reminders"], {"useDefault": False})
        self.assertEqual(event["transparency"], "transparent")
        self.assertEqual(event["visibility"], "private")

    def test_yearless_event_uses_neutral_anchor(self):
        event = MODULE.desired_event({
            "stable_id": "uuid-2", "display_name": "Someone",
            "year": None, "month": 2, "day": 28,
        })
        self.assertEqual(event["start"]["date"], "2000-02-28")
        self.assertEqual(
            event["extendedProperties"]["private"]["birthday"], "02-28"
        )

    def test_event_id_is_stable_and_google_safe(self):
        first = MODULE.event_id("same UUID")
        second = MODULE.event_id("same UUID")
        self.assertEqual(first, second)
        self.assertRegex(first, r"^[a-v0-9]{5,1024}$")

    def test_plan_changes_is_idempotent_and_prunes_only_supplied_events(self):
        desired = [MODULE.desired_event({
            "stable_id": "keep", "display_name": "Keep Person",
            "year": 1990, "month": 1, "day": 2,
        })]
        inserts, updates, deletes = MODULE.plan_changes(desired, desired)
        self.assertEqual((inserts, updates, deletes), ([], [], []))

        changed = [{**desired[0], "summary": "Old name"}]
        extra = MODULE.desired_event({
            "stable_id": "stale", "display_name": "Stale Person",
            "year": 1991, "month": 2, "day": 3,
        })
        inserts, updates, deletes = MODULE.plan_changes(desired, changed + [extra])
        self.assertEqual(inserts, [])
        self.assertEqual(updates, desired)
        self.assertEqual(deletes, [extra])

    def test_source_digest_matches_canonical_format(self):
        records = [{
            "stable_id": "u", "display_name": "N",
            "year": None, "month": 7, "day": 12,
        }]
        self.assertEqual(
            MODULE.source_digest(records),
            "b2ff97541082b0b746ab8f285599c0c2590f14d0d6252582c191f30b6da5a5b9",
        )

    def test_pruning_requires_digest_of_exact_event_ids(self):
        deletes = [{"id": "a"}, {"id": "b"}]
        digest = MODULE.prune_digest(deletes)
        with self.assertRaisesRegex(RuntimeError, "--allow-prune-digest"):
            MODULE.validate_pruning(deletes, None)
        MODULE.validate_pruning(deletes, digest)
        with self.assertRaisesRegex(RuntimeError, "--allow-prune-digest"):
            MODULE.validate_pruning([{"id": "c"}, {"id": "d"}], digest)

    def test_writes_are_verified_before_deletes(self):
        calls = []

        class Api:
            def request(self, method, path, params=None, body=None):
                calls.append((method, body and body.get("id")))
                return {}

        inserted = {"id": "new"}
        deleted = {"id": "old"}
        with (
            mock.patch.object(MODULE, "managed_events", return_value=[inserted]),
            mock.patch.object(MODULE, "verify_present") as verify,
        ):
            MODULE.apply_changes(Api(), "calendar", [inserted], [], [deleted], 0)
        self.assertEqual(calls, [("POST", "new"), ("DELETE", None)])
        verify.assert_called_once_with([inserted], [inserted])

    def test_retryable_quota_403_is_retried(self):
        detail = json.dumps({
            "error": {"errors": [{"reason": "userRateLimitExceeded"}]}
        }).encode()
        error = urllib.error.HTTPError(
            "https://example.invalid", 403, "Forbidden", {}, io.BytesIO(detail)
        )
        self.addCleanup(error.close)
        response = mock.MagicMock()
        response.__enter__.return_value.read.return_value = b"{}"
        api = MODULE.CalendarApi("token")
        with (
            mock.patch.object(MODULE.urllib.request, "urlopen", side_effect=[error, response]),
            mock.patch.object(MODULE.time, "sleep") as sleep,
            mock.patch.object(MODULE.random, "uniform", return_value=0),
        ):
            self.assertEqual(api.request("GET", "/test"), {})
        sleep.assert_called_once_with(1)

    def test_missing_state_adopts_only_marked_existing_calendar(self):
        class Api:
            def pages(self, path, params=None):
                self.listed = (path, params)
                yield {"id": "calendar-id", "summary": MODULE.CALENDAR_NAME}

            def request(self, method, path, params=None, body=None):
                self.requested = (method, path)
                return {
                    "summary": MODULE.CALENDAR_NAME,
                    "description": MODULE.CALENDAR_DESCRIPTION,
                }

        with tempfile.TemporaryDirectory() as directory:
            state = Path(directory) / "state.json"
            with mock.patch.object(MODULE, "STATE_FILE", state):
                self.assertEqual(
                    MODULE.calendar_id(Api(), MODULE.CALENDAR_NAME),
                    ("calendar-id", False),
                )
                self.assertEqual(json.loads(state.read_text())["calendar_id"], "calendar-id")


if __name__ == "__main__":
    unittest.main()
