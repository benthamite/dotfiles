import importlib.machinery
import importlib.util
import json
import sys
import unittest
from pathlib import Path
from urllib.error import HTTPError


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "claude" / "bin" / "ahrefs-api-guard"


def load_module():
    loader = importlib.machinery.SourceFileLoader("ahrefs_api_guard", str(SCRIPT))
    spec = importlib.util.spec_from_loader("ahrefs_api_guard", loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class FakeResponse:
    def __init__(self, payload):
        self.payload = payload

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False

    def read(self):
        return json.dumps(self.payload).encode("utf-8")


class AhrefsApiGuardTest(unittest.TestCase):
    def setUp(self):
        self.mod = load_module()

    def usage_response(self, used=399_460, limit=1_000_000):
        return {
            "limits_and_usage": {
                "units_usage_workspace": used,
                "units_limit_workspace": limit,
                "units_usage_api_key": 392_119,
                "units_limit_api_key": None,
                "usage_reset_date": "2026-07-07T00:00:00Z",
            }
        }

    def test_usage_remaining_is_derived_from_live_limit(self):
        def fake_urlopen(request, timeout):
            self.assertTrue(request.full_url.endswith("/subscription-info/limits-and-usage"))
            return FakeResponse(self.usage_response())

        usage = self.mod.fetch_usage("key", self.mod.DEFAULT_BASE_URL, urlopen=fake_urlopen)

        self.assertEqual(usage.limit, 1_000_000)
        self.assertEqual(usage.used, 399_460)
        self.assertEqual(usage.remaining, 600_540)

    def test_budget_blocks_when_projected_remaining_below_reserve(self):
        usage = self.mod.Usage(
            used=399_460,
            limit=400_000,
            remaining=540,
            key_usage=None,
            key_limit=None,
            reset_date="2026-07-07T00:00:00Z",
        )

        with self.assertRaises(self.mod.GuardError):
            self.mod.assert_budget_allows_call(
                usage,
                min_remaining=100_000,
                estimated_cost=4_250,
            )

    def test_budget_allows_when_live_upgrade_has_reserve(self):
        usage = self.mod.Usage(
            used=399_460,
            limit=1_000_000,
            remaining=600_540,
            key_usage=None,
            key_limit=None,
            reset_date="2026-07-07T00:00:00Z",
        )

        self.mod.assert_budget_allows_call(
            usage,
            min_remaining=100_000,
            estimated_cost=4_250,
        )

    def test_free_usage_endpoint_does_not_need_paid_call_guard(self):
        self.assertTrue(self.mod.is_free_usage_endpoint("/subscription-info/limits-and-usage"))
        self.assertFalse(self.mod.is_free_usage_endpoint("/site-explorer/all-backlinks"))


if __name__ == "__main__":
    unittest.main()
