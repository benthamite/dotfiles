import importlib.util
import plistlib
import tempfile
import unittest
from pathlib import Path


def load_module():
    path = Path(__file__).resolve().parents[1] / "launchd/bin/launchd-audit.py"
    spec = importlib.util.spec_from_file_location("launchd_audit", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class LaunchdAuditTests(unittest.TestCase):
    def test_read_plist_extracts_job_fields(self):
        module = load_module()
        with tempfile.TemporaryDirectory() as directory:
            tmp_path = Path(directory)
            plist = tmp_path / "com.example.job.plist"
            plist.write_bytes(
                plistlib.dumps(
                    {
                        "Label": "com.example.job",
                        "ProgramArguments": ["/bin/echo", "hello"],
                        "StartCalendarInterval": {"Hour": 9, "Minute": 30},
                        "StandardOutPath": str(tmp_path / "job.out.log"),
                        "StandardErrorPath": str(tmp_path / "job.err.log"),
                    }
                )
            )

            info = module.read_plist(plist)

            self.assertEqual(
                info,
                {
                    "path": str(plist),
                    "label": "com.example.job",
                    "program_arguments": ["/bin/echo", "hello"],
                    "start_calendar_interval": {"Hour": 9, "Minute": 30},
                    "stdout": str(tmp_path / "job.out.log"),
                    "stderr": str(tmp_path / "job.err.log"),
                },
            )

    def test_audit_reports_managed_job_missing_live_plist(self):
        module = load_module()
        with tempfile.TemporaryDirectory() as directory:
            tmp_path = Path(directory)
            registry = {
                "managed_jobs": [
                    {
                        "label": "com.example.missing",
                        "canonical_plist": "launchd/agents/private/com.example.missing.plist",
                        "live_plist": str(tmp_path / "missing.plist"),
                    }
                ],
                "unmanaged_first_party_jobs": [],
                "ignored_jobs": [],
            }

            findings = module.audit_registry(
                registry,
                live_plists={},
                loaded_labels=set(),
                repo_root=tmp_path,
            )

            self.assertIn(
                {
                    "severity": "ERROR",
                    "label": "com.example.missing",
                    "message": "managed job live plist missing",
                    "path": str(tmp_path / "missing.plist"),
                },
                findings,
            )

    def test_audit_reports_unregistered_first_party_live_plist(self):
        module = load_module()
        with tempfile.TemporaryDirectory() as directory:
            tmp_path = Path(directory)
            live_path = tmp_path / "com.stafforini.extra.plist"
            registry = {"managed_jobs": [], "unmanaged_first_party_jobs": [], "ignored_jobs": []}
            live = {"com.stafforini.extra": {"path": str(live_path), "label": "com.stafforini.extra"}}

            findings = module.audit_registry(
                registry,
                live_plists=live,
                loaded_labels=set(),
                repo_root=tmp_path,
            )

            self.assertEqual(
                findings,
                [
                    {
                        "severity": "WARN",
                        "label": "com.stafforini.extra",
                        "message": "first-party live plist is not listed in registry",
                        "path": str(live_path),
                    }
                ],
            )

    def test_audit_ignores_registered_third_party_live_plist(self):
        module = load_module()
        with tempfile.TemporaryDirectory() as directory:
            tmp_path = Path(directory)
            live_path = tmp_path / "com.vendor.job.plist"
            registry = {
                "managed_jobs": [],
                "unmanaged_first_party_jobs": [],
                "ignored_jobs": [{"label": "com.vendor.job", "reason": "third-party app"}],
            }
            live = {"com.vendor.job": {"path": str(live_path), "label": "com.vendor.job"}}

            findings = module.audit_registry(
                registry,
                live_plists=live,
                loaded_labels={"com.vendor.job"},
                repo_root=tmp_path,
            )

            self.assertEqual(findings, [])

    def test_audit_reports_managed_job_not_loaded(self):
        module = load_module()
        with tempfile.TemporaryDirectory() as directory:
            tmp_path = Path(directory)
            live_path = tmp_path / "com.example.job.plist"
            canonical = tmp_path / "launchd/agents/private/com.example.job.plist"
            canonical.parent.mkdir(parents=True)
            canonical.write_text("placeholder")
            live_path.write_bytes(plistlib.dumps({"Label": "com.example.job"}))
            registry = {
                "managed_jobs": [
                    {
                        "label": "com.example.job",
                        "canonical_plist": "launchd/agents/private/com.example.job.plist",
                        "live_plist": str(live_path),
                    }
                ],
                "unmanaged_first_party_jobs": [],
                "ignored_jobs": [],
            }

            findings = module.audit_registry(
                registry,
                live_plists={"com.example.job": {"path": str(live_path), "label": "com.example.job"}},
                loaded_labels=set(),
                repo_root=tmp_path,
            )

            self.assertIn(
                {
                    "severity": "WARN",
                    "label": "com.example.job",
                    "message": "managed job is not loaded in launchctl",
                    "path": str(live_path),
                },
                findings,
            )

    def test_audit_reports_managed_job_not_symlinked_to_canonical(self):
        module = load_module()
        with tempfile.TemporaryDirectory() as directory:
            tmp_path = Path(directory)
            live_path = tmp_path / "com.example.job.plist"
            canonical = tmp_path / "launchd/agents/private/com.example.job.plist"
            canonical.parent.mkdir(parents=True)
            canonical.write_text("placeholder")
            live_path.write_bytes(plistlib.dumps({"Label": "com.example.job"}))
            registry = {
                "managed_jobs": [
                    {
                        "label": "com.example.job",
                        "canonical_plist": "launchd/agents/private/com.example.job.plist",
                        "live_plist": str(live_path),
                    }
                ],
                "unmanaged_first_party_jobs": [],
                "ignored_jobs": [],
            }

            findings = module.audit_registry(
                registry,
                live_plists={"com.example.job": {"path": str(live_path), "label": "com.example.job"}},
                loaded_labels={"com.example.job"},
                repo_root=tmp_path,
            )

            self.assertIn(
                {
                    "severity": "WARN",
                    "label": "com.example.job",
                    "message": "managed job live plist is not symlinked to canonical plist",
                    "path": str(live_path),
                },
                findings,
            )

    def test_parse_launchctl_print_extracts_labels(self):
        module = load_module()
        output = """
gui/501 = {
    services = {
        123 = {
            label = com.stafforini.example
        }
        456 = {
            label = homebrew.mxcl.postgresql@17
        }
    }
}
"""

        self.assertEqual(
            module.parse_launchctl_labels(output),
            {"com.stafforini.example", "homebrew.mxcl.postgresql@17"},
        )

    def test_parse_launchctl_print_extracts_table_style_labels(self):
        module = load_module()
        output = """
services = {
    0      0  com.stafforini.brew-codex-upgrade
    0      0  application.com.apple.Terminal.123456
}
"""

        self.assertEqual(
            module.parse_launchctl_labels(output),
            {"com.stafforini.brew-codex-upgrade", "application.com.apple.Terminal.123456"},
        )


if __name__ == "__main__":
    unittest.main()
