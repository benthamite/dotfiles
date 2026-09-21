"""Read-only bibliography validation, including the real command's JSON contract."""

from __future__ import annotations

import copy
import importlib.util
import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("bib_entry_check", ROOT / "lib/python/bib_entry_check.py")
checker = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(checker)
POLICY = ROOT / "agents/bibliography-policy.md"


def book(key="Author2000ExampleBook"):
    return {"key": key, "entrytype": "book", "fields": {
        "author": "Author, Ada", "title": "Example book", "date": "2000",
        "publisher": "Example Press", "location": "Example City", "langid": "english"}}


def chapter():
    return {"key": "Writer2000ExampleChapter", "entrytype": "incollection", "fields": {
        "author": "Writer, Will", "title": "Example chapter", "crossref": "Editor2000Collection"}}


def collection():
    return {"key": "Editor2000Collection", "entrytype": "collection", "fields": {
        "editor": "Editor, Eve", "title": "Collected examples", "date": "2000",
        "publisher": "Example Press", "location": "Example City", "langid": "english"}}


class EntryChecks(unittest.TestCase):
    def setUp(self):
        self.rules = checker.load_policy(POLICY)

    def check(self, value, **kwargs):
        return checker.check_entries(value, self.rules, **kwargs)

    def test_structural_success_still_requires_editorial_review(self):
        result = self.check(book())
        self.assertEqual(result["structural_status"], "pass")
        self.assertFalse(result["semantic_review_complete"])
        self.assertTrue(result["results"][0]["semantic_review_required"])

    def test_reports_missing_fields_and_invalid_values_separately(self):
        entry = book()
        entry["fields"].update(author="{ } { }", publisher="", date="forthcoming")
        result = self.check(entry)["results"][0]
        self.assertIn({"any_of": ["author", "editor"]}, result["missing_fields"])
        self.assertIn({"any_of": ["publisher"]}, result["missing_fields"])
        self.assertEqual([item["field"] for item in result["invalid_fields"]], ["date"])

    def test_preserves_legitimate_optional_fields_and_input(self):
        entry = book()
        entry["fields"].update(abstract="A substantive summary.", keywords="example", rating="8",
                               translator="Reader, Rita", origdate="1899?", annotation="Personal note")
        before = copy.deepcopy(entry)
        self.assertEqual(self.check(entry)["structural_status"], "pass")
        self.assertEqual(entry, before)

    def test_identifier_checks_do_not_claim_edition_identity(self):
        entry = book()
        entry["fields"].update(doi="10.1234/example", isbn="978-0-262-03384-8; 0-262-03384-4")
        self.assertEqual(self.check(entry)["structural_status"], "pass")
        entry["fields"].update(doi="https://doi.org/10.1234/example", isbn="9780262033849")
        result = self.check(entry)["results"][0]
        self.assertEqual({item["field"] for item in result["invalid_fields"]}, {"doi", "isbn"})

    def test_calendar_errors_and_broken_metadata_syntax(self):
        entry = book()
        entry["fields"].update(date="2023-02-29", urldate="2024-01", title="Example {unfinished",
                               url="https://example.org/invalid path")
        result = self.check(entry)["results"][0]
        self.assertEqual({item["field"] for item in result["invalid_fields"]},
                         {"date", "urldate", "title", "url"})
        entry["fields"].update(date="2024-02-29", urldate="2024-03-01", title=r"Example \{literal",
                               url="https://example.org/work")
        self.assertEqual(self.check(entry)["structural_status"], "pass")

    def test_labelled_date_range_requires_an_actual_year(self):
        entry = book()
        entry["fields"]["date"] = "1899~/1901?"
        self.assertEqual(self.check(entry)["structural_status"], "pass")
        entry["fields"]["date"] = "undated"
        self.assertEqual(self.check(entry)["structural_status"], "fail")

    def test_crossref_inherits_only_from_explicit_parent(self):
        missing = self.check(chapter())["results"][0]
        self.assertTrue(any(item["field"] == "crossref" for item in missing["invalid_fields"]))
        result = self.check(chapter(), parents=collection())
        self.assertEqual(result["structural_status"], "pass")
        self.assertEqual(result["results"][0]["inherited_fields"]["booktitle"],
                         {"key": "Editor2000Collection", "field": "title"})
        self.assertEqual(result["results"][1]["role"], "parent")
        child = chapter()
        del child["fields"]["author"]
        self.assertIn({"any_of": ["author"]}, self.check(child, parents=collection())["results"][0]["missing_fields"])

    def test_crossref_chain_cycles_and_incomplete_parents(self):
        parent = collection()
        grandparent = book("Earlier1900Book")
        parent["fields"].update(crossref=grandparent["key"])
        del parent["fields"]["publisher"]
        result = self.check(chapter(), parents=[parent, grandparent])
        self.assertEqual(result["structural_status"], "pass")
        self.assertEqual(result["results"][0]["inherited_fields"]["publisher"]["key"], grandparent["key"])
        self.assertEqual(result["results"][0]["inherited_fields"]["booktitle"],
                         {"key": parent["key"], "field": "title"})
        del grandparent["fields"]["author"]
        self.assertEqual(self.check(chapter(), parents=[parent, grandparent])["structural_status"], "fail")
        grandparent["fields"]["crossref"] = parent["key"]
        result = self.check(chapter(), parents=[parent, grandparent])
        self.assertTrue(all(any("cycle" in problem["reason"] for problem in item["invalid_fields"])
                            for item in result["results"]))

    def test_malformed_or_duplicate_entries_are_explicit_errors(self):
        for value in ([], {}, {"key": "A", "entrytype": "book", "fields": {"date": 2000}}, [book(), book()]):
            with self.subTest(value=value), self.assertRaises(checker.CheckError):
                self.check(value)
        with self.assertRaises(checker.CheckError):
            checker.read_json('{"key":"A","key":"B"}')

    def test_unknown_type_is_not_accepted_as_generic_misc(self):
        entry = book()
        entry["entrytype"] = "newtype"
        result = self.check(entry)["results"][0]
        self.assertEqual(result["structural_status"], "fail")
        self.assertEqual(result["invalid_fields"][0]["field"], "entrytype")

    def test_policy_is_the_only_source_of_required_fields_and_bans(self):
        rules = copy.deepcopy(self.rules)
        rules["types"]["book"]["required"].append("series")
        rules["forbidden_fields"]["synthetic_debris"] = "Synthetic test-only ban"
        with tempfile.TemporaryDirectory() as tmp:
            policy = Path(tmp) / "policy.md"
            policy.write_text("```json\n" + json.dumps({"bib_entry_check": rules}) + "\n```\n")
            entry = book()
            entry["fields"]["synthetic_debris"] = "example"
            result = checker.check_entries(entry, checker.load_policy(policy))["results"][0]
            self.assertIn({"any_of": ["series"]}, result["missing_fields"])
            self.assertEqual(result["invalid_fields"][0]["reason"], "Synthetic test-only ban")
            policy.write_text(policy.read_text() * 2)
            with self.assertRaises(checker.CheckError):
                checker.load_policy(policy)

    def test_invalid_policy_cannot_silently_weaken_validation(self):
        cases = []
        bad_type = copy.deepcopy(self.rules)
        bad_type["types"]["book"] = {"require": ["title"]}
        cases.append(bad_type)
        bad_check = copy.deepcopy(self.rules)
        bad_check["field_rules"]["date"] = ["date"]
        cases.append(bad_check)
        bad_section = copy.deepcopy(self.rules)
        bad_section["unknown"] = {}
        cases.append(bad_section)
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "policy.md"
            for rules in cases:
                path.write_text("```json\n" + json.dumps({"bib_entry_check": rules}) + "\n```\n")
                with self.subTest(rules=rules), self.assertRaises(checker.CheckError):
                    checker.load_policy(path)

    def test_present_pdf_header_is_not_content_or_layout_acceptance(self):
        with tempfile.TemporaryDirectory() as tmp:
            attachment = Path(tmp) / "example.pdf"
            attachment.write_bytes(b"%PDF-1.4\nsynthetic header only")
            before = attachment.read_bytes()
            entry = book()
            entry["fields"]["file"] = str(attachment)
            result = self.check(entry, check_files=True)
            self.assertEqual(result["structural_status"], "pass")
            self.assertEqual(result["results"][0]["file_checks"][0]["status"], "present")
            self.assertFalse(result["semantic_review_complete"])
            self.assertEqual(attachment.read_bytes(), before)
            attachment.write_bytes(b"<html>Not a PDF</html>")
            self.assertEqual(self.check(entry, check_files=True)["structural_status"], "fail")

    def test_absent_empty_and_relative_attachments(self):
        self.assertEqual(self.check(book(), check_files=True)["results"][0]["file_checks"][0]["status"], "absent")
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "empty.pdf").touch()
            entry = book()
            entry["fields"]["file"] = "empty.pdf"
            self.assertEqual(self.check(entry, check_files=True)["structural_status"], "fail")
            self.assertEqual(self.check(entry, check_files=True, file_roots=[root])["structural_status"], "fail")
            (root / "empty.pdf").write_bytes(b"%PDF-1.4\n")
            self.assertEqual(self.check(entry, check_files=True, file_roots=[root])["structural_status"], "pass")
            nested = root / "nested"
            nested.mkdir()
            entry["fields"]["file"] = "../empty.pdf"
            self.assertEqual(self.check(entry, check_files=True, file_roots=[nested])["structural_status"], "fail")


class CommandChecks(unittest.TestCase):
    def run_command(self, data=None, *args):
        process = subprocess.run([str(ROOT / "bin/bib-entry-check"), *args],
                                 input=json.dumps(data) if data is not None else "", text=True,
                                 capture_output=True, timeout=10,
                                 env={**os.environ, "PYTHONDONTWRITEBYTECODE": "1"})
        self.assertEqual(process.stderr, "")
        return process.returncode, json.loads(process.stdout)

    def test_stdin_and_exit_codes(self):
        code, value = self.run_command(book())
        self.assertEqual(code, 0)
        self.assertFalse(value["semantic_review_complete"])
        entry = book()
        del entry["fields"]["title"]
        self.assertEqual(self.run_command(entry)[0], 1)
        code, value = self.run_command({})
        self.assertEqual(code, 2)
        self.assertIn("error", value)

    def test_describe_and_explicit_parent_file(self):
        code, value = self.run_command(None, "--describe")
        self.assertEqual(code, 0)
        self.assertEqual(value["bib_entry_check"], checker.load_policy(POLICY))
        with tempfile.TemporaryDirectory() as tmp:
            parents = Path(tmp) / "parents.json"
            parents.write_text(json.dumps(collection()))
            code, value = self.run_command(chapter(), "--parents", str(parents))
            self.assertEqual(code, 0)
            self.assertEqual(len(value["results"]), 2)


if __name__ == "__main__":
    unittest.main()
