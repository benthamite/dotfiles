"""full_text renders forwarded messages, attachments and files, not just `text`."""

import importlib.util
from pathlib import Path
import unittest

SOURCE = Path(__file__).resolve().parents[1] / "claude/bin/slack.py"


class FullTextTests(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location("slack_full_text_test", SOURCE)
        self.mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.mod)

    def test_forwarded_message_with_empty_text_shows_its_content(self):
        m = {"text": "", "attachments": [{"is_share": True, "author_name": "Agus",
                                           "text": "Cuenta Bancaria (USD)\nGalicia"}]}
        out = self.mod.full_text(m)
        self.assertIn("[Forwarded message from Agus]", out)
        self.assertIn("Cuenta Bancaria (USD)", out)

    def test_files_and_plain_text(self):
        m = {"text": "hola", "files": [{"name": "factura.pdf"}]}
        self.assertEqual(self.mod.full_text(m), "hola\n[File: factura.pdf]")


if __name__ == "__main__":
    unittest.main()
