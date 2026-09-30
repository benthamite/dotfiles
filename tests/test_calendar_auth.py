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
    def test_broker_read_never_invokes_raw_op(self):
        with mock.patch.object(calendar.auth.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, "grant\n")) as run:
            self.assertEqual(calendar.auth._op_read("op://Automations/item/credential"), "grant")
            self.assertEqual(run.call_args.args[0], ["op-automations", "cache", "read", "op://Automations/item/credential"])
            self.assertEqual(run.call_args.kwargs["timeout"], 45)

    def test_personal_items_use_the_cached_broker_read(self):
        with mock.patch.object(calendar.auth.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, "client\n")) as run:
            self.assertEqual(calendar.auth._personal_automation("google-workspace-client-id"), "client")
            self.assertEqual(
                run.call_args.args[0],
                ["op-automations", "@personal", "cache", "read", "op://Automation/google-workspace-client-id/password"],
            )

    def test_forget_drops_every_epoch_credential_once(self):
        with mock.patch.object(calendar.auth.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, "")) as run:
            calendar.auth.forget_cached_credentials("epoch")
        self.assertEqual(
            [call.args[0] for call in run.call_args_list],
            [
                ["op-automations", "@personal", "cache", "forget", "op://Automation/google-workspace-client-id/password"],
                ["op-automations", "@personal", "cache", "forget", "op://Automation/google-workspace-client-secret/password"],
                ["op-automations", "cache", "forget", calendar.auth.REFRESH_TOKEN_OP["epoch"]],
            ],
        )

    def test_rejected_cached_grant_is_forgotten_and_read_fresh_once(self):
        credentials = mock.Mock()
        service = mock.Mock()
        service.calendars.return_value.get.return_value.execute.return_value = {"id": "someone@example.com"}
        with mock.patch.object(calendar, "refreshed_credentials", side_effect=[calendar.RefreshError("invalid_grant"), credentials]) as refreshed, mock.patch.object(calendar.auth, "forget_cached_credentials") as forget, mock.patch.object(calendar, "build", return_value=service):
            with self.assertRaisesRegex(ValueError, "wrong Calendar account"):
                calendar.refresh_credentials()
        forget.assert_called_once_with("epoch")
        self.assertEqual(refreshed.call_count, 2)

    def test_second_rejection_is_not_retried(self):
        with mock.patch.object(calendar, "refreshed_credentials", side_effect=calendar.RefreshError("invalid_grant")) as refreshed, mock.patch.object(calendar.auth, "forget_cached_credentials"):
            with self.assertRaises(calendar.RefreshError):
                calendar.refresh_credentials()
        self.assertEqual(refreshed.call_count, 2)

    def test_rejected_token_refresh_forgets_and_retries_once(self):
        rejection = calendar.auth.urllib.error.HTTPError(calendar.auth.TOKEN_URL, 400, "Bad Request", {}, None)
        response = mock.Mock()
        response.read.return_value = b'{"access_token": "token", "expires_in": 3600}'
        with mock.patch.object(calendar.auth, "_request_access_token", side_effect=[rejection, response]) as request, mock.patch.object(calendar.auth, "forget_cached_credentials") as forget:
            self.assertEqual(calendar.auth._refresh_access_token("epoch")["token"], "token")
        forget.assert_called_once_with("epoch")
        self.assertEqual(request.call_count, 2)

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
