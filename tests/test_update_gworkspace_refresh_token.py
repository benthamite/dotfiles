import contextlib
import importlib.machinery
import importlib.util
import io
import sys
import types
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "claude/bin/update-gworkspace-refresh-token"


def load_module():
    flow_module = types.ModuleType("google_auth_oauthlib.flow")
    flow_module.InstalledAppFlow = mock.Mock()
    package = types.ModuleType("google_auth_oauthlib")
    package.flow = flow_module
    with mock.patch.dict(sys.modules, {
        "google_auth_oauthlib": package,
        "google_auth_oauthlib.flow": flow_module,
    }):
        loader = importlib.machinery.SourceFileLoader("update_token", str(SCRIPT))
        spec = importlib.util.spec_from_loader(loader.name, loader)
        module = importlib.util.module_from_spec(spec)
        loader.exec_module(module)
    return module


class UpdateTokenTests(unittest.TestCase):
    def test_epoch_scope_supports_sync_and_send(self):
        module = load_module()
        self.assertIn("https://mail.google.com/", module.SCOPES["epoch"])
        self.assertNotIn(
            "https://www.googleapis.com/auth/gmail.modify",
            module.SCOPES["epoch"],
        )

    def test_personal_scope_is_limited_to_app_created_calendars(self):
        module = load_module()
        self.assertIn(
            "https://www.googleapis.com/auth/calendar.app.created",
            module.SCOPES["personal"],
        )
        self.assertNotIn(
            "https://www.googleapis.com/auth/calendar",
            module.SCOPES["personal"],
        )
        self.assertIn(
            "https://www.googleapis.com/auth/calendar.calendarlist.readonly",
            module.SCOPES["personal"],
        )

    def test_incremental_consent_is_validated_before_storage(self):
        module = load_module()
        flow = mock.Mock()
        credentials = mock.Mock(
            refresh_token="new-secret-token",
            granted_scopes=module.SCOPES["personal"],
            scopes=module.SCOPES["personal"],
        )
        flow.run_local_server.return_value = credentials
        module.InstalledAppFlow.from_client_config.return_value = flow
        stderr = io.StringIO()
        with (
            mock.patch.object(sys, "argv", [str(SCRIPT), "--account", "personal"]),
            mock.patch.object(module, "read_personal_automation", side_effect=["id", "secret"]),
            mock.patch.object(module, "op_inject") as update,
            mock.patch.object(module, "forget_cached_token") as forget,
            contextlib.redirect_stderr(stderr),
        ):
            module.main()
        flow.run_local_server.assert_called_once_with(
            port=8080,
            open_browser=True,
            include_granted_scopes="true",
            prompt="consent",
            login_hint="pablo.stafforini@gmail.com",
        )
        update.assert_called_once_with(
            module.TOKEN_OP_ENTRIES["personal"], "new-secret-token"
        )
        forget.assert_called_once_with(module.TOKEN_OP_ENTRIES["personal"])
        self.assertNotIn("new-secret-token", stderr.getvalue())

    def test_incremental_consent_accepts_previously_granted_extra_scopes(self):
        module = load_module()
        flow = mock.Mock()
        credentials = mock.Mock(
            refresh_token="new-secret-token",
            granted_scopes=module.SCOPES["personal"]
            + ["https://www.googleapis.com/auth/drive"],
            scopes=module.SCOPES["personal"],
        )
        flow.run_local_server.return_value = credentials
        module.InstalledAppFlow.from_client_config.return_value = flow
        with (
            mock.patch.object(sys, "argv", [str(SCRIPT), "--account", "personal"]),
            mock.patch.object(module, "read_personal_automation", side_effect=["id", "secret"]),
            mock.patch.object(module, "op_inject") as update,
            mock.patch.object(module, "forget_cached_token"),
            mock.patch.dict(module.os.environ, {}, clear=True),
        ):
            module.main()
            self.assertEqual(module.os.environ["OAUTHLIB_RELAX_TOKEN_SCOPE"], "1")
        update.assert_called_once_with(
            module.TOKEN_OP_ENTRIES["personal"], "new-secret-token"
        )

    def test_forget_targets_the_cached_reference_of_each_store(self):
        module = load_module()
        with mock.patch.object(module.subprocess, "run") as run:
            module.forget_cached_token(module.TOKEN_OP_ENTRIES["personal"])
            module.forget_cached_token(module.TOKEN_OP_ENTRIES["epoch"])
        self.assertEqual(
            [call.args[0] for call in run.call_args_list],
            [
                ["op-automations", "@personal", "cache", "forget",
                 "op://Automation/google-workspace-refresh-token-personal/password"],
                ["op-automations", "cache", "forget",
                 "op://Automations/Google Workspace OAuth - Pablo Epoch/credential"],
            ],
        )


if __name__ == "__main__":
    unittest.main()
