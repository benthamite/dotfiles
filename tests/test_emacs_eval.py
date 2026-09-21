"""Exercise response limits in real clean Emacs, without the user's server."""

import base64
import importlib.machinery
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import unittest

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[1]
loader = importlib.machinery.SourceFileLoader("emacs_eval", str(ROOT / "bin/emacs-eval"))
spec = importlib.util.spec_from_loader(loader.name, loader)
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)


class BoundedResponses(unittest.TestCase):
    def evaluate(self, expression):
        form = "(princ " + module.wrapped_expression(expression) + ")"
        run = subprocess.run(["emacs", "-Q", "--batch", "--eval", form],
                             capture_output=True, text=True, timeout=10, check=True)
        return json.loads(base64.b64decode(run.stdout).decode("utf-8"))

    def test_unicode_and_control_characters_roundtrip(self):
        result = self.evaluate('(list t "Valéry’s Cahiers" "line\\nnext\\r")')
        self.assertTrue(result["ok"])
        self.assertFalse(result["truncated"])
        self.assertIn("Valéry’s", result["result"])

    def test_accidental_database_return_never_walks_hash_contents(self):
        result = self.evaluate("(let ((db (make-hash-table))) (dotimes (i 50000) (puthash i (make-string 1000 120) db)) (let ((databases (list (list (cons 'entries db))))) (memq (car databases) databases)))")
        self.assertTrue(result["ok"])
        self.assertTrue(result["truncated"])
        self.assertIn("50000 entries", result["result"])
        self.assertLess(len(result["result"]), 200)

    def test_circular_list_is_bounded(self):
        result = self.evaluate("(let ((items (list 'cycle))) (setcdr items items) items)")
        self.assertTrue(result["truncated"])
        self.assertLess(len(result["result"]), 300)

    def test_long_string_is_shortened_before_printing(self):
        result = self.evaluate("(make-string 10000000 120)")
        self.assertTrue(result["truncated"])
        self.assertLess(len(result["result"]), 1100)

    def test_nested_values_have_total_and_depth_limits(self):
        result = self.evaluate("(let ((v 'end)) (dotimes (_ 1000) (setq v (list v v v))) v)")
        self.assertTrue(result["truncated"])
        self.assertLessEqual(len(result["result"]), 8193)

    def test_error_objects_are_summarized_without_printing_them(self):
        result = self.evaluate("(signal 'wrong-type-argument (list 'stringp (make-hash-table)))")
        self.assertFalse(result["ok"])
        self.assertIn("wrong-type-argument", result["result"])
        self.assertTrue(result["truncated"])


if __name__ == "__main__":
    unittest.main()
