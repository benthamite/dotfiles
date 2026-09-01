#!/usr/bin/env python3
"""Tests for the redacted GitGuardian incident reader."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import io
import json
import pathlib
import unittest
import urllib.request
from contextlib import redirect_stdout


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

    def test_summary_uses_fixed_redacted_schema(self):
        secret = "gg_pat_must_never_escape"
        incident = {
            "id": 42,
            "date": "2026-08-31T10:20:30Z",
            "detector": {"display_name": "GitGuardian API Token"},
            "severity": "critical",
            "validity": "invalid",
            "occurrences_count": 2,
            "status": "TRIGGERED",
            "secret_hash": secret,
            "matches": [{"match": secret}],
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
            },
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
                return ([{"id": 1}], '<https://api.gitguardian.com/v1/page-2>; rel="next"')
            if url.endswith("status=ASSIGNED"):
                return ([], None)
            if url.endswith("page-2"):
                return ([{"id": 2}], None)
            return (details[int(url.rsplit("/", 1)[1])], None)

        summaries = MODULE.list_open_incidents("gg_pat_private", getter)

        self.assertEqual([item["id"] for item in summaries], [2, 1])
        self.assertEqual(len(calls), 5)

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


if __name__ == "__main__":
    unittest.main()
