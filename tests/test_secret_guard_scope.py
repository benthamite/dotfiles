"""The public-route registry must not grow; see docs/secret-guard.md."""
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import guard_harness  # noqa: E402,F401 - puts lib/python on sys.path

from agent_guard import classifiers  # noqa: E402
from agent_guard.policies import secret_leak  # noqa: E402

FROZEN_ROUTE_COUNT = 42


class SecretGuardScopeTests(unittest.TestCase):
    def test_public_route_registry_does_not_grow(self):
        self.assertLessEqual(
            len(classifiers.load("public-url-scan").ROUTES), FROZEN_ROUTE_COUNT,
            "Do not add per-site exemptions to the secret guard; "
            "read docs/secret-guard.md (use WebFetch for public URLs).")

    def test_deny_message_points_to_the_rules(self):
        text = Path(secret_leak.__file__).read_text()
        self.assertIn("docs/secret-guard.md", text)
        self.assertNotIn("diagnose and repair the classifier", text)


if __name__ == "__main__":
    unittest.main()
