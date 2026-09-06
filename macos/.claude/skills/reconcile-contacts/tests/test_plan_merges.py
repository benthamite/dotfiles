"""Review-only planner tests; never read a contact store or execute AppleScript."""

from __future__ import annotations

import contextlib
import copy
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import sys
import types
import unittest
from unittest import mock


SKILL = Path(__file__).resolve().parents[1]


def load(name, filename):
    spec = importlib.util.spec_from_file_location(name, SKILL / "scripts" / filename)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


PLANNER = load("review_only_planner", "plan-merges.py")
MATCHER = load("review_only_matcher", "reconcile-contacts.py")


def record(uid, first="Alex", last="Example", mail=None, **fields):
    result = dict(first=first, mid="", last=last, maiden="", nick="", sfx="", org=[], aka=[],
                  mail=mail or [], urls=[], phones=[], birthday=None, uuid=uid)
    result.update(fields)
    return result


def fixture_reader(bbdb, contacts, bbdb_status="complete", contacts_status="complete"):
    def load_bbdb(path, dump, *, metadata):
        metadata.update(status=bbdb_status, projection="synthetic_fixture")
        return bbdb
    def load_contacts(path, *, metadata):
        metadata.update(status=contacts_status, projection="synthetic_fixture")
        return contacts
    return types.SimpleNamespace(load_bbdb=mock.Mock(side_effect=load_bbdb),
                                 load_contacts=mock.Mock(side_effect=load_contacts),
                                 build_index=MATCHER.build_index, match_keys=MATCHER.match_keys)


class ReviewOnlyPlannerTests(unittest.TestCase):
    def setUp(self):
        self.bbdb = [record("bbdb", mail=["canonical@example.invalid"])]
        self.contacts = [record("one", mail=["one@example.invalid"]),
                         record("two", mail=["two@example.invalid"])]

    def test_name_only_group_is_review_evidence_not_deletion_authority(self):
        plan = PLANNER.build_plan(MATCHER, self.bbdb, self.contacts)
        self.assertEqual(plan["mode"], "review_only")
        self.assertFalse(plan["executable_mutations"])
        self.assertEqual(plan["counts"]["candidate_groups"], 1)
        group, = plan["candidate_groups"]
        self.assertEqual([member["record"]["uuid"] for member in group["contacts"]], ["one", "two"])
        self.assertTrue(all(all(key.startswith("name:") for key in member["matching_keys"]) for member in group["contacts"]))
        self.assertIn("identity is unverified", group["reason"])
        for key in ("survivor_uid", "losers", "rename", "add_mail", "add_phones", "add_urls"):
            self.assertNotIn(key, group)

    def test_all_projected_values_survive_without_lossy_deduplication(self):
        self.contacts[0].update(mid="Middle", urls=["https://example.invalid/A"],
                                phones=[("home", "1-800-FLOWERS")], birthday=None,
                                extra_future_reader_field={"note": "kept verbatim"})
        self.contacts[1].update(urls=["https://example.invalid/a"], phones=[("work", "1-800-FOOD")],
                                birthday="1980-02-03")
        before = copy.deepcopy((self.bbdb, self.contacts))
        plan = PLANNER.build_plan(MATCHER, self.bbdb, self.contacts)
        actual = [member["record"] for member in plan["candidate_groups"][0]["contacts"]]
        self.assertEqual(actual, self.contacts)
        self.assertEqual((self.bbdb, self.contacts), before)
        self.assertEqual(actual[0]["mid"], "Middle")
        self.assertEqual(actual[1]["birthday"], "1980-02-03")
        self.assertEqual(actual[0]["extra_future_reader_field"], {"note": "kept verbatim"})
        actual[0]["mail"].append("changed@example.invalid")
        self.assertEqual((self.bbdb, self.contacts), before)

    def test_duplicate_or_missing_ids_are_errors_not_self_deletion_plans(self):
        for store in ("bbdb", "contacts"):
            for uid in ("", " ", None, 3, " one ", "same"):
                with self.subTest(store=store, uid=uid):
                    bbdb, contacts = copy.deepcopy((self.bbdb, self.contacts))
                    selected = bbdb if store == "bbdb" else contacts
                    if uid == "same":
                        selected.append(copy.deepcopy(selected[0]))
                    else:
                        selected[0]["uuid"] = uid
                    with self.assertRaisesRegex(PLANNER.PlanInputError, "record ID"):
                        PLANNER.build_plan(MATCHER, bbdb, contacts)

    def test_partial_and_malformed_projections_are_errors(self):
        for field, value in (("first", None), ("mail", None), ("mail", [3]), ("phones", ["not-a-pair"]),
                             ("phones", [("home", None)]), ("birthday", float("nan"))):
            with self.subTest(field=field, value=value):
                contacts = copy.deepcopy(self.contacts)
                contacts[0][field] = value
                with self.assertRaises(ValueError):
                    PLANNER.build_plan(MATCHER, self.bbdb, contacts)
        for bbdb, contacts in (([], self.contacts), (self.bbdb, [])):
            with self.assertRaisesRegex(ValueError, "nonempty"):
                PLANNER.build_plan(MATCHER, bbdb, contacts)

    def test_ambiguous_cross_key_evidence_is_preserved_not_silently_dropped(self):
        bbdb = [record("first", first="First", mail=["one@example.invalid"]),
                record("second", first="Second", mail=["two@example.invalid"])]
        contacts = [record("contact", first="First", mail=["two@example.invalid"])]
        plan = PLANNER.build_plan(MATCHER, bbdb, contacts)
        self.assertEqual(plan["candidate_groups"], [])
        ambiguous, = plan["ambiguous_contacts"]
        self.assertEqual(ambiguous["record"], contacts[0])
        self.assertEqual([item["record"]["uuid"] for item in ambiguous["candidate_bbdb_records"]], ["first", "second"])

    def test_projection_digest_binds_only_the_recorded_projection(self):
        first = PLANNER.build_plan(MATCHER, self.bbdb, self.contacts)
        self.contacts[0]["birthday"] = "1980-02-03"
        second = PLANNER.build_plan(MATCHER, self.bbdb, self.contacts)
        self.assertNotEqual(first["projection_digests"]["contacts_sha256"], second["projection_digests"]["contacts_sha256"])
        self.assertIn("not complete database bytes", second["projection_digests"]["scope"])

    def test_old_apple_script_api_refuses_without_output(self):
        output = io.StringIO()
        with self.assertRaisesRegex(ValueError, "not supported"):
            PLANNER.emit_applescript([], output)
        self.assertEqual(output.getvalue(), "")

    def test_legacy_cli_refuses_before_import_or_store_reads(self):
        for arguments in ([], ["--dry-run"], ["--bbdb-file", "/fixture/bbdb", "--contacts-db", "/fixture/contacts"]):
            with self.subTest(arguments=arguments), mock.patch.object(PLANNER, "load_reconciler") as reader, contextlib.redirect_stderr(io.StringIO()):
                with self.assertRaises(SystemExit) as raised:
                    PLANNER.main(arguments)
                self.assertEqual(raised.exception.code, 2)
                reader.assert_not_called()
        native = subprocess.run([sys.executable, "-I", "-B", str(SKILL / "scripts/plan-merges.py")],
                                capture_output=True, text=True, timeout=5)
        self.assertEqual(native.returncode, 2)
        self.assertEqual(native.stdout, "")

    def test_main_loads_only_explicit_sources_and_emits_full_review_json(self):
        reader = fixture_reader(self.bbdb, self.contacts)
        output = io.StringIO()
        with mock.patch.object(PLANNER, "load_reconciler", return_value=reader), contextlib.redirect_stdout(output):
            status = PLANNER.main(["--bbdb-file", "/fixture/selected-bbdb", "--contacts-db", "/fixture/selected-source", "--json"])
        self.assertEqual(status, 0)
        plan = json.loads(output.getvalue())
        self.assertEqual(plan["sources"]["bbdb"]["selected_file"], "/fixture/selected-bbdb")
        self.assertEqual(plan["sources"]["contacts"]["selected_file"], "/fixture/selected-source")
        self.assertEqual(reader.load_bbdb.call_args.args, (Path("/fixture/selected-bbdb"), SKILL / "scripts/dump-bbdb.el"))
        self.assertEqual(reader.load_contacts.call_args.args, (Path("/fixture/selected-source"),))
        self.assertEqual(plan["mode"], "review_only")

    def test_main_rejects_incomplete_reads_without_partial_plan(self):
        for side in ("bbdb", "contacts"):
            reader = fixture_reader(self.bbdb, self.contacts, **{side + "_status": "partial"})
            output = io.StringIO()
            with mock.patch.object(PLANNER, "load_reconciler", return_value=reader), contextlib.redirect_stdout(output):
                status = PLANNER.main(["--bbdb-file", "/fixture/bbdb", "--contacts-db", "/fixture/contacts", "--json"])
            self.assertEqual(status, 2)
            self.assertEqual(json.loads(output.getvalue())["status"], "input_error")
            self.assertNotIn("candidate_groups", output.getvalue())
            if side == "bbdb":
                reader.load_contacts.assert_not_called()

    def test_reader_failure_is_typed_without_leaking_private_diagnostics(self):
        reader = fixture_reader(self.bbdb, self.contacts)
        reader.load_contacts.side_effect = ValueError("private diagnostic content")
        output = io.StringIO()
        with mock.patch.object(PLANNER, "load_reconciler", return_value=reader), contextlib.redirect_stdout(output):
            status = PLANNER.main(["--bbdb-file", "/fixture/bbdb", "--contacts-db", "/fixture/contacts", "--json"])
        self.assertEqual(status, 2)
        report = json.loads(output.getvalue())
        self.assertEqual(report["phase"], "read_contacts")
        self.assertNotIn("private diagnostic content", output.getvalue())

    def test_typed_parser_interruption_is_not_mislabeled_as_bad_input(self):
        class ParserInterrupted(ValueError):
            def __init__(self, signum):
                super().__init__("private cancellation diagnostic")
                self.signum = signum
                self.exit_code = 128 + signum
        for signum in (1, 2, 15):
            reader = fixture_reader(self.bbdb, self.contacts)
            reader.ParserInterrupted = ParserInterrupted
            reader.load_bbdb.side_effect = ParserInterrupted(signum)
            output = io.StringIO()
            with mock.patch.object(PLANNER, "load_reconciler", return_value=reader), contextlib.redirect_stdout(output):
                status = PLANNER.main(["--bbdb-file", "/fixture/bbdb", "--contacts-db", "/fixture/contacts", "--json"])
            self.assertEqual(status, 128 + signum)
            report = json.loads(output.getvalue())
            self.assertEqual(report["status"], "interrupted")
            self.assertEqual(report["signal"], signum)
            self.assertNotIn("private cancellation diagnostic", output.getvalue())
            self.assertNotIn("candidate_groups", output.getvalue())
            reader.load_contacts.assert_not_called()

    def test_summary_is_bounded_and_escapes_contact_text(self):
        bbdb = [record("b" + str(i), first="Person", last=str(i)) for i in range(25)]
        contacts = [record(f"c{i}-{j}", first="Person", last=str(i), sfx="\x1b[31m\n" + "x" * 500)
                    for i in range(25) for j in range(7)]
        plan = PLANNER.build_plan(MATCHER, bbdb, contacts)
        output = io.StringIO()
        PLANNER.emit_summary(plan, output)
        self.assertNotIn("\x1b", output.getvalue())
        self.assertIn("additional group(s) omitted", output.getvalue())
        self.assertIn("additional row(s) omitted", output.getvalue())
        self.assertLess(len(output.getvalue()), 25000)
        self.assertTrue(output.getvalue().startswith("Review only."))


if __name__ == "__main__":
    unittest.main()
