import importlib.util
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def load_module(name, path):
    spec = importlib.util.spec_from_file_location(name, ROOT / path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class GoogleWorkspaceCliTest(unittest.TestCase):
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
