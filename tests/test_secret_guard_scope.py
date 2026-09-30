"""The public-route registry must not grow; see docs/secret-guard.md."""
import importlib.util
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
FROZEN_ROUTE_COUNT = 42


def load(runtime):
    source = ROOT / runtime / "hooks/lib-public-url-scan.py"
    spec = importlib.util.spec_from_file_location(f"scan_{runtime}", source)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class SecretGuardScopeTests(unittest.TestCase):
    def test_public_route_registry_does_not_grow(self):
        for runtime in ("claude", "codex"):
            with self.subTest(runtime=runtime):
                self.assertLessEqual(
                    len(load(runtime).ROUTES), FROZEN_ROUTE_COUNT,
                    "Do not add per-site exemptions to the secret guard; "
                    "read docs/secret-guard.md (use WebFetch for public URLs).")

    def test_deny_message_points_to_the_rules(self):
        for runtime in ("claude", "codex"):
            text = (ROOT / runtime / "hooks/block-secret-leak.sh").read_text()
            with self.subTest(runtime=runtime):
                self.assertIn("docs/secret-guard.md", text)
                self.assertNotIn("diagnose and repair the classifier", text)


if __name__ == "__main__":
    unittest.main()
