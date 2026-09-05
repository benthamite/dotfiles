#!/usr/bin/env python3
"""Tests for the redacted GitGuardian incident reader."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import io
import json
import pathlib
import unittest
import urllib.error
import urllib.request
import urllib.response
from contextlib import redirect_stderr, redirect_stdout
from email.message import Message
from unittest.mock import patch


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "gitguardian-incidents"
LOADER = importlib.machinery.SourceFileLoader("gitguardian_incidents", str(SCRIPT))
SPEC = importlib.util.spec_from_loader(LOADER.name, LOADER)
assert SPEC is not None
MODULE = importlib.util.module_from_spec(SPEC)
LOADER.exec_module(MODULE)


class FakeResponse:
    def __init__(self, payload, link=None):
        self.payload = json.dumps(payload).encode()
        self.headers = {"Link": link} if link else {}

    def __enter__(self):
        return self

    def __exit__(self, *args):
        return False

    def read(self, _limit):
        return self.payload


class GitGuardianIncidentTests(unittest.TestCase):
    def test_api_request_keeps_token_in_header_and_requests_privacy_mode(self):
        seen = {}

        def opener(request: urllib.request.Request, timeout: int):
            seen["request"] = request
            seen["timeout"] = timeout
            return FakeResponse([])

        payload, link = MODULE._api_get(
            "https://api.gitguardian.com/v1/incidents/secrets", "gg_pat_private", opener
        )

        self.assertEqual(payload, [])
        self.assertIsNone(link)
        self.assertEqual(seen["timeout"], 30)
        request = seen["request"]
        self.assertNotIn("gg_pat_private", request.full_url)
        self.assertEqual(request.get_header("Authorization"), "Token gg_pat_private")
        self.assertEqual(request.get_header("X-privacy-mode"), "true")

    def test_pagination_refuses_to_forward_token_off_origin(self):
        with self.assertRaisesRegex(MODULE.TriageError, "unsafe pagination"):
            MODULE._next_url(
                "https://api.gitguardian.com/v1/incidents/secrets",
                '<https://attacker.invalid/steal>; rel="next"',
            )

    def test_default_transport_rejects_redirect_before_forwarding_token(self):
        origin = "https://api.gitguardian.com/v1/incidents/secrets"
        targets = (
            "https://attacker.invalid/steal",
            "https://api.gitguardian.com/v1/redirected",
        )
        for code in (301, 302, 303, 307, 308):
            for target in targets:
                with self.subTest(code=code, target=target):
                    requests = []

                    class RecordingHTTPSHandler(urllib.request.HTTPSHandler):
                        def https_open(self, request):
                            requests.append(request)
                            headers = Message()
                            if len(requests) == 1:
                                headers["Location"] = target
                                response_code = code
                            else:
                                response_code = 200
                            response = urllib.response.addinfourl(
                                io.BytesIO(b"[]"), headers, request.full_url,
                                response_code,
                            )
                            response.msg = "redirect" if len(requests) == 1 else "OK"
                            return response

                    with patch.object(
                        urllib.request, "HTTPSHandler", RecordingHTTPSHandler
                    ):
                        with self.assertRaisesRegex(
                            MODULE.TriageError, f"HTTP {code}$"
                        ) as failure:
                            MODULE._api_get(origin, "gg_pat_private")

                    self.assertEqual([request.full_url for request in requests], [origin])
                    self.assertEqual(
                        requests[0].get_header("Authorization"), "Token gg_pat_private"
                    )
                    self.assertNotIn(target, str(failure.exception))
                    self.assertNotIn("gg_pat_private", str(failure.exception))

    def test_rate_limit_retry_is_bounded(self):
        calls = []
        delays = []

        def opener(_request, timeout):
            self.assertEqual(timeout, 30)
            calls.append(True)
            if len(calls) == 1:
                raise urllib.error.HTTPError(
                    "https://api.gitguardian.com/v1/incidents/secrets",
                    429,
                    "rate limited",
                    {"Retry-After": "99"},
                    None,
                )
            return FakeResponse([])

        payload, _ = MODULE._api_get(
            "https://api.gitguardian.com/v1/incidents/secrets",
            "gg_pat_private",
            opener,
            delays.append,
        )

        self.assertEqual(payload, [])
        self.assertEqual(len(calls), 2)
        self.assertEqual(delays, [MODULE.MAX_RETRY_AFTER_SECONDS])

    def test_summary_uses_fixed_redacted_schema(self):
        secret = "gg_pat_must_never_escape"
        incident = {
            "id": 42,
            "date": "2026-08-31T10:20:30Z",
            "detector": {"display_name": "GitGuardian API Token"},
            "severity": "critical",
            "validity": "invalid",
            "occurrences_count": {"nested_secret": secret},
            "status": "TRIGGERED",
            "secret_hash": secret,
            "matches": [{"match": secret}],
            "share_url": secret,
            "custom_tags": [{"key": secret, "value": secret}],
            "feedback_list": [{"answers": [{"text": secret}]}],
            "occurrences": [
                {
                    "filepath": "shell/example",
                    "source": {"full_name": "owner/repo", "visibility": "public"},
                    "matches": [{"match": secret}],
                }
            ],
        }

        summary = MODULE.summarize_incident(incident)

        self.assertEqual(
            set(summary),
            {
                "id",
                "detector",
                "severity",
                "validity",
                "occurrences",
                "source",
                "visibility",
                "filepath",
                "date",
                "status",
                "ignore_reason",
                "secret_revoked",
                "triggered_at",
                "resolved_at",
                "ignored_at",
                "tags",
            },
        )
        self.assertEqual(summary["occurrences"], 1)
        self.assertNotIn(secret, json.dumps(summary))

    def test_occurrence_projection_never_returns_match_or_arbitrary_fields(self):
        secret = "sensitive_occurrence_value_must_never_escape"
        occurrence = {
            "id": 91,
            "incident_id": 42,
            "sha": "a" * 40,
            "filepath": "shell/example",
            "presence": "removed",
            "date": "2026-08-31T10:20:30Z",
            "source": {
                "id": 12,
                "type": "github",
                "full_name": "owner/repo",
                "visibility": "public",
                "credential": secret,
            },
            "change_type": "context",
            "tags": [
                "FROM_HISTORICAL_SCAN",
                "IGNORED_IN_CHECK_RUN",
                "REVOCABLE_BY_GG",
                "NONE",
                secret,
            ],
            "matches": [
                {
                    "name": "apikey",
                    "match": secret,
                    "indice_start": 32,
                    "indice_end": 79,
                    "pre_line_start": None,
                    "pre_line_end": None,
                    "post_line_start": 7,
                    "post_line_end": 7,
                }
            ],
            "author_info": secret,
            "url": secret,
            "secret": secret,
        }

        summary = MODULE.summarize_occurrence(occurrence)

        self.assertEqual(
            set(summary),
            {
                "id",
                "incident_id",
                "sha",
                "filepath",
                "source",
                "source_id",
                "source_type",
                "visibility",
                "presence",
                "change_type",
                "date",
                "tags",
                "matches",
            },
        )
        self.assertEqual(
            summary["tags"],
            ["FROM_HISTORICAL_SCAN", "IGNORED_IN_CHECK_RUN", "REVOCABLE_BY_GG"],
        )
        self.assertEqual(summary["change_type"], "context")
        self.assertEqual(
            summary["matches"],
            [
                {
                    "name": "apikey",
                    "index_start": 32,
                    "index_end": 79,
                    "pre_line_start": None,
                    "pre_line_end": None,
                    "post_line_start": 7,
                    "post_line_end": 7,
                }
            ],
        )
        self.assertNotIn(secret, json.dumps(summary))

    def test_list_fetches_every_page_and_outputs_only_summaries(self):
        calls = []
        details = {
            1: {
                "id": 1,
                "detector": {"display_name": "B"},
                "status": "TRIGGERED",
                "occurrences": [],
            },
            2: {
                "id": 2,
                "detector": {"display_name": "A"},
                "status": "TRIGGERED",
                "occurrences": [],
            },
        }

        def getter(url, token):
            self.assertEqual(token, "gg_pat_private")
            calls.append(url)
            if url.endswith("status=TRIGGERED"):
                return ([details[1]], '<https://api.gitguardian.com/v1/page-2>; rel="next"')
            if url.endswith("status=ASSIGNED"):
                return ([], None)
            if url.endswith("page-2"):
                return ([details[2]], None)
            self.fail("bulk listing unexpectedly fetched incident detail")

        summaries = MODULE.list_open_incidents("gg_pat_private", getter)

        self.assertEqual([item["id"] for item in summaries], [2, 1])
        self.assertEqual(len(calls), 3)
        self.assertTrue(all(item["source"] == "?" for item in summaries))

    def test_closed_list_enumerates_resolved_and_ignored(self):
        calls = []

        def getter(url, token):
            self.assertEqual(token, "gg_pat_private")
            calls.append(url)
            if url.endswith("status=RESOLVED"):
                return (
                    [{"id": 8, "status": "RESOLVED"}],
                    '<https://api.gitguardian.com/v1/closed-resolved-2>; rel="next"',
                )
            if url.endswith("closed-resolved-2"):
                return ([{"id": 10, "status": "RESOLVED"}], None)
            if url.endswith("status=IGNORED"):
                return (
                    [{"id": 9, "status": "IGNORED"}],
                    '<https://api.gitguardian.com/v1/closed-ignored-2>; rel="next"',
                )
            if url.endswith("closed-ignored-2"):
                return ([{"id": 11, "status": "IGNORED"}], None)
            self.fail("unexpected URL")

        summaries = MODULE.list_closed_incidents("gg_pat_private", getter)

        self.assertEqual({item["id"] for item in summaries}, {8, 9, 10, 11})
        self.assertEqual({item["status"] for item in summaries}, {"RESOLVED", "IGNORED"})
        self.assertTrue(any(url.endswith("status=RESOLVED") for url in calls))
        self.assertTrue(any(url.endswith("status=IGNORED") for url in calls))
        self.assertTrue(any(url.endswith("closed-resolved-2") for url in calls))
        self.assertTrue(any(url.endswith("closed-ignored-2") for url in calls))

    def test_bulk_listing_deduplicates_identical_ids_and_rejects_conflicts(self):
        item = {"id": 8, "status": "RESOLVED", "validity": "invalid"}

        def duplicate_getter(url, _token):
            if url.endswith("status=RESOLVED"):
                return ([item, dict(item)], None)
            return ([], None)

        summaries = MODULE.list_closed_incidents("gg_pat_private", duplicate_getter)
        self.assertEqual([summary["id"] for summary in summaries], [8])

        def conflicting_getter(url, _token):
            if url.endswith("status=RESOLVED"):
                return ([item], None)
            return ([{"id": 8, "status": "IGNORED", "validity": "invalid"}], None)

        with self.assertRaisesRegex(MODULE.TriageError, "conflicting duplicate"):
            MODULE.list_closed_incidents("gg_pat_private", conflicting_getter)

    def test_detail_fetches_every_occurrence_page_without_leaking_values(self):
        secret = "sensitive_detail_value_must_never_escape"
        calls = []

        def occurrence(occurrence_id, sha):
            return {
                "id": occurrence_id,
                "incident_id": 42,
                "sha": sha,
                "filepath": "shell/example",
                "presence": "removed",
                "date": "2026-08-31T10:20:30Z",
                "source": {
                    "id": 12,
                    "type": "github",
                    "full_name": "owner/repo",
                    "visibility": "public",
                },
                "change_type": "addition",
                "tags": ["FROM_HISTORICAL_SCAN"],
                "matches": [
                    {
                        "name": "apikey",
                        "match": secret,
                        "indice_start": 10,
                        "indice_end": 20,
                    }
                ],
                "secret": secret,
            }

        def getter(url, token):
            self.assertEqual(token, "gg_pat_private")
            calls.append(url)
            if url.endswith("/incidents/secrets/42?with_occurrences=0"):
                return (
                    {
                        "id": 42,
                        "detector": {"display_name": "Provider Token"},
                        "status": "RESOLVED",
                        "validity": "invalid",
                        "secret_revoked": True,
                        "occurrences_count": 101,
                        "secret_hash": secret,
                        "occurrences": None,
                    },
                    None,
                )
            if "cursor=next" in url:
                return ([occurrence(101, "b" * 40)], None)
            return (
                [occurrence(number, "a" * 40) for number in range(1, 101)],
                '<https://api.gitguardian.com/v1/occurrences/secrets?cursor=next>; rel="next"',
            )

        detail = MODULE.get_incident(42, "gg_pat_private", getter)

        self.assertEqual(detail["id"], 42)
        self.assertEqual(detail["status"], "RESOLVED")
        self.assertEqual(detail["validity"], "invalid")
        self.assertIs(detail["secret_revoked"], True)
        self.assertEqual(len(detail["occurrence_details"]), 101)
        self.assertEqual(
            {item["incident_id"] for item in detail["occurrence_details"]}, {42}
        )
        self.assertEqual(detail["occurrence_details"][0]["id"], 1)
        self.assertEqual(detail["occurrence_details"][-1]["id"], 101)
        self.assertEqual(len(calls), 3)
        self.assertNotIn(secret, json.dumps(detail))

    def test_detail_rejects_an_occurrence_for_another_incident(self):
        def getter(url, _token):
            if "/incidents/secrets/42" in url:
                return ({"id": 42, "occurrences": None}, None)
            return ([{"id": 1, "incident_id": 99}], None)

        with self.assertRaisesRegex(MODULE.TriageError, "invalid occurrence"):
            MODULE.get_incident(42, "gg_pat_private", getter)

    def test_detail_deduplicates_occurrence_ids_and_rejects_conflicts(self):
        occurrence = {
            "id": 1,
            "incident_id": 42,
            "sha": "a" * 40,
            "filepath": "one.txt",
        }

        def duplicate_getter(url, _token):
            if "/incidents/secrets/42" in url:
                return ({"id": 42, "occurrences": None}, None)
            return ([occurrence, dict(occurrence)], None)

        detail = MODULE.get_incident(42, "gg_pat_private", duplicate_getter)
        self.assertEqual([item["id"] for item in detail["occurrence_details"]], [1])

        def conflicting_getter(url, _token):
            if "/incidents/secrets/42" in url:
                return ({"id": 42, "occurrences": None}, None)
            changed = dict(occurrence, filepath="two.txt")
            return ([occurrence, changed], None)

        with self.assertRaisesRegex(MODULE.TriageError, "conflicting duplicate"):
            MODULE.get_incident(42, "gg_pat_private", conflicting_getter)

    def test_detail_rejects_occurrence_count_mismatch_after_deduplication(self):
        occurrence = {"id": 1, "incident_id": 42}

        def getter(url, _token):
            if "/incidents/secrets/42" in url:
                return ({"id": 42, "occurrences_count": 2, "occurrences": None}, None)
            return ([occurrence, dict(occurrence)], None)

        with self.assertRaisesRegex(MODULE.TriageError, "occurrence count mismatch"):
            MODULE.get_incident(42, "gg_pat_private", getter)

    def test_repeated_pagination_link_is_rejected(self):
        occurrence_url = (
            "https://api.gitguardian.com/v1/occurrences/secrets?per_page=100&incident_id=42"
        )

        def getter(url, _token):
            if "/incidents/secrets/42" in url:
                return ({"id": 42, "occurrences": None}, None)
            self.assertEqual(url, occurrence_url)
            return ([], f'<{occurrence_url}>; rel="next"')

        with self.assertRaisesRegex(MODULE.TriageError, "pagination cycle"):
            MODULE.get_incident(42, "gg_pat_private", getter)

    def test_later_occurrence_page_failure_returns_no_partial_detail(self):
        calls = []

        def getter(url, _token):
            calls.append(url)
            if "/incidents/secrets/42" in url:
                return ({"id": 42, "occurrences": None}, None)
            if len(calls) == 2:
                return (
                    [{"id": 1, "incident_id": 42}],
                    '<https://api.gitguardian.com/v1/occurrences/secrets?cursor=next>; rel="next"',
                )
            raise MODULE.TriageError("redacted later-page failure")

        with self.assertRaisesRegex(MODULE.TriageError, "later-page failure"):
            MODULE.get_incident(42, "gg_pat_private", getter)

    def test_main_prints_a_json_array(self):
        original_load = MODULE.load_token
        original_list = MODULE.list_open_incidents
        MODULE.load_token = lambda: "gg_pat_private"
        MODULE.list_open_incidents = lambda _token: [{"id": 7}]
        try:
            stdout = io.StringIO()
            with redirect_stdout(stdout):
                code = MODULE.main(["list-open"])
        finally:
            MODULE.load_token = original_load
            MODULE.list_open_incidents = original_list
        self.assertEqual(code, 0)
        self.assertEqual(json.loads(stdout.getvalue()), [{"id": 7}])

    def test_main_get_prints_one_redacted_detail_object(self):
        original_load = MODULE.load_token
        original_get = MODULE.get_incident
        MODULE.load_token = lambda: "gg_pat_private"
        MODULE.get_incident = lambda incident_id, _token: {"id": incident_id}
        try:
            stdout = io.StringIO()
            with redirect_stdout(stdout):
                code = MODULE.main(["get", "42"])
        finally:
            MODULE.load_token = original_load
            MODULE.get_incident = original_get
        self.assertEqual(code, 0)
        self.assertEqual(json.loads(stdout.getvalue()), {"id": 42})

    def test_main_prints_no_partial_json_when_detail_fails(self):
        original_load = MODULE.load_token
        original_get = MODULE.get_incident
        MODULE.load_token = lambda: "gg_pat_private"

        def fail(_incident_id, _token):
            raise MODULE.TriageError("redacted failure")

        MODULE.get_incident = fail
        try:
            stdout = io.StringIO()
            stderr = io.StringIO()
            with redirect_stdout(stdout), redirect_stderr(stderr):
                code = MODULE.main(["get", "42"])
        finally:
            MODULE.load_token = original_load
            MODULE.get_incident = original_get
        self.assertEqual(code, 1)
        self.assertEqual(stdout.getvalue(), "")
        self.assertEqual(stderr.getvalue(), "error: redacted failure\n")


if __name__ == "__main__":
    unittest.main()
