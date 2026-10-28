"""wa-send refuses text that WhatsApp would format by accident."""

import importlib.machinery
import importlib.util
from pathlib import Path
import unittest

SOURCE = Path(__file__).resolve().parents[1] / "bin/wa-send"


class FormattingTests(unittest.TestCase):
    def setUp(self):
        loader = importlib.machinery.SourceFileLoader("wa_send_test", str(SOURCE))
        spec = importlib.util.spec_from_loader("wa_send_test", loader)
        self.mod = importlib.util.module_from_spec(spec)
        loader.exec_module(self.mod)

    def test_tildes_meaning_about_are_caught(self):
        text = "hay ~ARS 110.000 de una diseñadora. Al comprador de hoy (~$1.505) son unos USD 4.000"
        self.assertTrue(self.mod.formatted_spans(text))

    def test_bold_italic_and_mono_are_caught(self):
        for text in ("precio *final* ya", "un _ejemplo_ claro", "```código```"):
            self.assertTrue(self.mod.formatted_spans(text), text)

    def test_plain_text_passes(self):
        for text in ("factura 20000000001_006_00002_00000001.pdf", "3 * 4 = 12 y 5 * 6",
                     "unos USD 4.000", "hola ~ chau ~ ok"):
            self.assertEqual(self.mod.formatted_spans(text), [], text)


if __name__ == "__main__":
    unittest.main()
