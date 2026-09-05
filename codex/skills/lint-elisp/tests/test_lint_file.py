#!/usr/bin/env python3
"""Run the literal lint helper only on disposable native Emacs fixtures."""
from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


HELPER = Path(__file__).resolve().parents[1] / "scripts/lint-file.el"
EMACS = shutil.which("emacs")
HEADER = ";;; fixture.el --- Synthetic lint fixture -*- lexical-binding: t; -*-\n\n;;; Commentary:\n\n;; Test fixture.\n\n;;; Code:\n\n"
FOOTER = "\n(provide 'fixture)\n;;; fixture.el ends here\n"
CLEAN = '(defun fixture-value ()\n  "Return the fixture value."\n  1)\n'


class LintFileTests(unittest.TestCase):
    def setUp(self):
        self.assertIsNotNone(EMACS, "Native Emacs is required; no test was performed")
        self.temporary = tempfile.TemporaryDirectory(prefix="lint-file-tests-", dir="/tmp")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.target = self.root / "fixture.el"
        self.target.write_text(HEADER + CLEAN + FOOTER)

    def run_helper(self, target=None, *, before=None, load_paths=(), args=None):
        argv = [EMACS, "-Q", "--batch"]
        for path in load_paths:
            argv += ["-L", str(path)]
        if before:
            prelude = self.root / "fault.el"
            prelude.write_text(before)
            argv += ["-l", str(prelude)]
        argv += ["-l", str(HELPER)]
        argv += args if args is not None else ["--", str(target or self.target)]
        result = subprocess.run(argv, cwd=self.root, text=True, capture_output=True,
                                env=dict(os.environ, TMPDIR=str(self.root)), timeout=30)
        self.last_stderr = result.stderr
        data = json.loads(result.stdout)
        self.assertEqual(data["schemaVersion"], 1)
        self.assertTrue(data["emacsVersion"])
        self.assertEqual(data["compilerWarningPolicy"], "all")
        retained = data["cleanup"]["retainedPath"]
        if retained:
            self.addCleanup(self.clean_retained, Path(retained))
        return result.returncode, data

    def clean_retained(self, root):
        self.assertEqual(root.parent.resolve(), Path("/tmp").resolve())
        self.assertTrue(root.name.startswith("lint-elisp-"))
        for path in root.iterdir():
            self.assertIn(path.name, {"compiled.elc", "unexpected"})
            self.assertTrue(path.is_file())
            self.assertFalse(path.is_symlink())
            path.unlink()
        root.rmdir()

    def test_clean_reports_both_stages_and_exact_content_identity(self):
        status, data = self.run_helper()
        self.assertEqual(status, 0, data)
        self.assertEqual(data["resolvedTarget"], str(self.target.resolve()))
        self.assertEqual(data["sha256Before"], hashlib.sha256(self.target.read_bytes()).hexdigest())
        self.assertEqual(data["sha256Before"], data["sha256After"])
        self.assertTrue(data["sourceUnchanged"])
        for stage in ("compiler", "checkdoc"):
            self.assertTrue(data[stage]["completed"])
            self.assertEqual(data[stage]["diagnostics"], [])
        self.assertTrue(data["cleanup"]["completed"])
        self.assertFalse(self.target.with_suffix(".elc").exists())

    def test_checkdoc_notes_are_not_clean(self):
        self.target.write_text(HEADER + CLEAN.replace("value.", "value") + FOOTER)
        original = self.target.read_bytes()
        status, data = self.run_helper()
        self.assertEqual(status, 1, data)
        self.assertTrue(data["checkdoc"]["completed"])
        self.assertTrue(data["checkdoc"]["diagnostics"])
        self.assertEqual(self.target.read_bytes(), original)

    def test_compiler_warning_is_not_clean(self):
        self.target.write_text(HEADER + CLEAN.replace("  1)", "  fixture-undefined)") + FOOTER)
        status, data = self.run_helper()
        self.assertEqual(status, 1, data)
        self.assertTrue(data["compiler"]["completed"])
        self.assertTrue(data["compiler"]["diagnostics"])

    def test_syntax_and_compile_time_errors_are_incomplete(self):
        for body in ("(defun fixture-value (", '(eval-when-compile (error "Synthetic compile failure"))'):
            with self.subTest(body=body):
                self.target.write_text(HEADER + body + FOOTER)
                status, data = self.run_helper()
                self.assertEqual(status, 2, data)
                self.assertFalse(data["compiler"]["completed"])
                self.assertEqual(data["compiler"]["status"], "error")

    def test_unterminated_string_does_not_become_an_empty_checkdoc_result(self):
        self.target.write_text(HEADER + '(defun fixture-value ()\n  "Return a fixture value."\n  (error "Fixture failure' + FOOTER)
        status, data = self.run_helper()
        self.assertEqual(status, 2, data)
        self.assertFalse(data["compiler"]["completed"])
        self.assertFalse(data["checkdoc"]["completed"])
        self.assertTrue(data["checkdoc"]["errors"])

    def test_no_byte_compile_preserves_adjacent_output(self):
        self.target.write_text((HEADER + CLEAN + FOOTER).replace(
            "lexical-binding: t;", "lexical-binding: t; no-byte-compile: t;"))
        adjacent = self.target.with_suffix(".elc")
        adjacent.write_bytes(b"existing adjacent output")
        status, data = self.run_helper()
        self.assertEqual(status, 2, data)
        self.assertEqual(data["compiler"]["status"], "skipped")
        self.assertFalse(data["compiler"]["completed"])
        self.assertTrue(data["checkdoc"]["completed"])
        self.assertEqual(adjacent.read_bytes(), b"existing adjacent output")

    def test_literal_path_and_stdout_noise_remain_one_json_result(self):
        directory = self.root / 'space \' " $value `token` (part);\nnext'
        directory.mkdir()
        target = directory / "fixture.el"
        target.write_text(HEADER + '(eval-when-compile (message "compile message") (princ "compile output"))\n' + CLEAN + FOOTER)
        status, data = self.run_helper(target)
        self.assertEqual(status, 0, data)
        self.assertEqual(data["target"], str(target))
        self.assertIn("compile message", self.last_stderr)

    def test_source_directory_and_source_suffix_precede_stale_dependencies(self):
        stale = self.root / "stale-build"
        stale.mkdir()
        (stale / "fixture-helper.el").write_text('(error "Wrong build directory")')
        (self.root / "fixture-helper.el").write_text(
            '(defmacro fixture-helper-value () 1)\n(provide \'fixture-helper)\n')
        sibling = self.root / "fixture-helper.elc"
        sibling.write_text('(error "Stale compiled dependency")')
        adjacent = self.target.with_suffix(".elc")
        adjacent.write_bytes(b"existing target elc")
        self.target.write_text(HEADER + "(require 'fixture-helper)\n" +
                               CLEAN.replace("  1)", "  (fixture-helper-value))") + FOOTER)
        status, data = self.run_helper(load_paths=[stale])
        self.assertEqual(status, 0, data)
        self.assertEqual(adjacent.read_bytes(), b"existing target elc")
        self.assertEqual(sibling.read_text(), '(error "Stale compiled dependency")')

    def test_file_and_directory_local_eval_are_not_executed(self):
        marker = self.root / "must-not-exist"
        form = f'(write-region "unsafe" nil {json.dumps(str(marker))})'
        (self.root / ".dir-locals.el").write_text(f'((nil . ((eval . {form}))))')
        self.target.write_text(HEADER + CLEAN + FOOTER +
                               f'\n;; Local Variables:\n;; eval: {form}\n;; End:\n')
        status, data = self.run_helper()
        self.assertEqual(status, 0, data)
        self.assertFalse(marker.exists())

    def test_nil_compiler_return_and_error_callback_are_not_clean(self):
        for behavior in ("nil", '(funcall byte-compile-log-warning-function "Synthetic error" 1 nil :error) t'):
            with self.subTest(behavior=behavior):
                prelude = "(require 'bytecomp)\n" + f"(defun byte-compile-file (&rest _) {behavior})\n"
                status, data = self.run_helper(before=prelude)
                self.assertEqual(status, 2, data)
                self.assertFalse(data["compiler"]["completed"])

    def test_checkdoc_exception_is_not_swallowed(self):
        status, data = self.run_helper(before="(require 'checkdoc)\n(defun checkdoc-current-buffer (&rest _) (error \"Synthetic Checkdoc failure\"))")
        self.assertEqual(status, 2, data)
        self.assertFalse(data["checkdoc"]["completed"])
        self.assertTrue(data["checkdoc"]["errors"])

    def test_source_change_is_detected_without_claiming_atomicity(self):
        self.target.write_text(HEADER + '(eval-when-compile\n  (write-region "\\n;; Changed.\\n" nil byte-compile-current-file t))\n' + CLEAN + FOOTER)
        status, data = self.run_helper()
        self.assertEqual(status, 2, data)
        self.assertFalse(data["sourceUnchanged"])
        self.assertNotEqual(data["sha256Before"], data["sha256After"])

    def test_unexpected_artifacts_are_retained_outside_configured_temp_root(self):
        self.target.write_text(HEADER + '(eval-when-compile\n  (write-region "unexpected" nil\n    (expand-file-name "unexpected" (file-name-directory (byte-compile-dest-file byte-compile-current-file)))))\n' + CLEAN + FOOTER)
        status, data = self.run_helper()
        self.assertEqual(status, 2, data)
        self.assertFalse(data["cleanup"]["completed"])
        self.assertEqual((Path(data["cleanup"]["retainedPath"]) / "unexpected").read_text(), "unexpected")

    def test_invalid_arguments_never_report_clean(self):
        wrong = self.root / "fixture.txt"
        wrong.write_text("not lisp")
        for args in ([], ["--", "fixture.el"], ["--", str(self.root)],
                     ["--", str(wrong)], ["--", str(self.root / "missing.el")],
                     ["--", "/ssh:synthetic:/fixture.el"], ["--", str(self.target), "extra"]):
            with self.subTest(args=args):
                status, data = self.run_helper(args=args)
                self.assertEqual(status, 2, data)
                self.assertFalse(data["compiler"]["completed"])
                self.assertFalse(data["checkdoc"]["completed"])


if __name__ == "__main__":
    unittest.main(verbosity=2)
