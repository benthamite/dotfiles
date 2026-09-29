import importlib.util
import contextlib
import io
import unittest
from unittest.mock import patch
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def load_module(name, path):
    spec = importlib.util.spec_from_file_location(name, ROOT / path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class GoogleWorkspaceCliTest(unittest.TestCase):
    def test_gmail_whoami_uses_mailbox_grant_and_selected_account(self):
        gmail = load_module("gmail_cli", "claude/bin/gmail.py")
        for account in gmail.ACCOUNTS:
            with self.subTest(account=account):
                args = gmail.build_parser().parse_args(["--account", account, "whoami"])
                output = io.StringIO()
                with patch.object(gmail, "api_request", return_value={
                    "emailAddress": "mailbox@example.com", "messagesTotal": 42,
                }) as request, contextlib.redirect_stdout(output):
                    args.func(args)
                request.assert_called_once_with(
                    "GET", "https://gmail.googleapis.com/gmail/v1/users/me/profile",
                    account=account,
                )
                self.assertEqual(output.getvalue(), "mailbox@example.com\n")

    def test_gmail_whoami_fails_when_identity_is_missing(self):
        gmail = load_module("gmail_cli", "claude/bin/gmail.py")
        args = gmail.build_parser().parse_args(["--account", "personal", "whoami"])
        with patch.object(gmail, "api_request", return_value={}):
            with self.assertRaisesRegex(SystemExit, "did not return an email address"):
                args.func(args)

    def test_gmail_account_flag_works_before_subcommand(self):
        gmail = load_module("gmail_cli", "claude/bin/gmail.py")

        args = gmail.build_parser().parse_args(
            ["--account", "personal", "query", "in:anywhere Kitt"]
        )

        self.assertEqual(args.account, "personal")

    def test_gmail_account_flag_works_after_subcommand(self):
        gmail = load_module("gmail_cli", "claude/bin/gmail.py")

        args = gmail.build_parser().parse_args(
            ["query", "--account", "personal", "in:anywhere Kitt"]
        )

        self.assertEqual(args.account, "personal")

    def test_sheets_account_flag_works_before_subcommand(self):
        sheets = load_module("sheets_cli", "claude/bin/sheets.py")

        args = sheets.build_parser().parse_args(
            ["--account", "personal", "info", "spreadsheet-id"]
        )

        self.assertEqual(args.account, "personal")

    def test_sheets_account_flag_works_after_subcommand(self):
        sheets = load_module("sheets_cli", "claude/bin/sheets.py")

        args = sheets.build_parser().parse_args(
            ["info", "--account", "personal", "spreadsheet-id"]
        )

        self.assertEqual(args.account, "personal")


if __name__ == "__main__":
    unittest.main()
