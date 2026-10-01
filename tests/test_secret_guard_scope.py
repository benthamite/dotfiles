"""The secret guard must not regrow per-site exemptions; see docs/secret-guard.md."""
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import guard_harness  # noqa: E402,F401 - puts lib/python on sys.path

from agent_guard import url_heuristic  # noqa: E402
from agent_guard.policies import secret_leak  # noqa: E402

POLICIES = Path(secret_leak.__file__).parent


class SecretGuardScopeTests(unittest.TestCase):
    def test_no_public_route_registry(self):
        # Intended difference 3: the opaque-token heuristic only logs, so the
        # 42-route exemption registry that softened its denials is gone.
        self.assertFalse(hasattr(url_heuristic, "ROUTES"))
        for policy in POLICIES.glob("*.py"):
            with self.subTest(policy=policy.name):
                self.assertNotIn("public-url-scan", policy.read_text())

    def test_deny_message_points_to_the_rules(self):
        text = Path(secret_leak.__file__).read_text()
        self.assertIn("docs/secret-guard.md", text)
        self.assertNotIn("diagnose and repair the classifier", text)


if __name__ == "__main__":
    unittest.main()
