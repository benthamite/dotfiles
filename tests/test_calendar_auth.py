"""Canonical Calendar grant replacement and broker routing regressions."""

import importlib.machinery
import importlib.util
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
loader = importlib.machinery.SourceFileLoader("calendar_auth", str(ROOT / "bin/gcalcli-epoch"))
spec = importlib.util.spec_from_loader(loader.name, loader)
calendar = importlib.util.module_from_spec(spec)
loader.exec_module(calendar)


class CalendarAuthTests(unittest.TestCase):
    def test_broker_read_never_invokes_raw_op_or_bootstrap_pass(self):
        with mock.patch.object(calendar.auth.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, "grant\n")) as run, mock.patch.object(calendar.auth, "_pass_show", side_effect=AssertionError("bootstrap bypass")):
            self.assertEqual(calendar.auth._op_read("op://Automations/item/credential"), "grant")
            self.assertEqual(run.call_args.args[0], ["op-automations", "read", "op://Automations/item/credential"])
            self.assertEqual(run.call_args.kwargs["timeout"], 45)

    def test_rejected_account_preserves_old_cache(self):
        self.check_refresh("wrong@example.com", False)

    def test_validated_rotation_replaces_expired_cache_privately(self):
        self.check_refresh("pablo@epoch.ai", True)

    def check_refresh(self, identity, succeeds):
        with tempfile.TemporaryDirectory() as directory:
            target = Path(directory) / "oauth"
            target.write_bytes(b"previous expired grant")
            credentials = mock.Mock()
            service = mock.Mock()
            service.calendars.return_value.get.return_value.execute.return_value = {"id": identity}
            with mock.patch.object(calendar.auth, "_read_env", return_value=("client", "secret", "new grant")), mock.patch.object(calendar, "Credentials", return_value=credentials), mock.patch.object(calendar, "build", return_value=service), mock.patch.object(calendar.env, "data_file_paths", return_value=[target]), mock.patch.object(calendar.pickle, "dump", side_effect=lambda obj, out: out.write(b"new validated grant")):
                if succeeds:
                    calendar.refresh_credentials()
                    self.assertEqual(target.read_bytes(), b"new validated grant")
                    self.assertEqual(target.stat().st_mode & 0o777, 0o600)
                    credentials.refresh.assert_called_once()
                else:
                    with self.assertRaisesRegex(ValueError, "wrong Calendar account"):
                        calendar.refresh_credentials()
                    self.assertEqual(target.read_bytes(), b"previous expired grant")
            self.assertEqual(list(Path(directory).iterdir()), [target])


if __name__ == "__main__":
    unittest.main()
