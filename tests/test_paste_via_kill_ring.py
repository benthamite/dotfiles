"""Native checks of the documented staging form, not a live paste handoff.

Only standalone Emacs -Q and synthetic private files are used. Every clipboard
callback is replaced at the interprogram boundary; no server, OS clipboard,
credentials, user buffers or saved kill history are accessed.
"""

import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
SKILLS = [ROOT / side / "skills/paste-via-kill-ring/SKILL.md" for side in ("codex", "claude")]
EMACS = shutil.which("emacs")


@unittest.skipUnless(EMACS, "Native batch Emacs is required")
class PasteViaKillRingTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="paste-ring-fixture-", dir="/tmp")
        self.addCleanup(temporary.cleanup)
        self.directory = Path(temporary.name).resolve()
        self.payload = self.directory / 'payload with ñ and "quotes".txt'
        documents = [path.read_text(encoding="utf-8") for path in SKILLS]
        self.assertEqual(documents[0], documents[1])
        forms = re.findall(r"```elisp\n(.*?)\n```", documents[0], re.DOTALL)
        self.assertEqual(len(forms), 1)
        self.assertEqual(forms[0].count('"/ABSOLUTE/PRIVATE/PAYLOAD"'), 1)
        self.form = forms[0].replace('"/ABSOLUTE/PRIVATE/PAYLOAD"', json.dumps(str(self.payload), ensure_ascii=False))

    def write_payload(self, text):
        with self.payload.open("xb") as stream:
            os.fchmod(stream.fileno(), 0o600)
            stream.write(text.encode("utf-8"))

    def native(self, body, *, transform="nil"):
        expression = f"""(let ((kill-ring (list "fixture-prior-kill"))
                              (kill-ring-yank-pointer nil)
                              (kill-transform-function {transform})
                              (kill-ring-max 60)
                              (kill-do-not-save-duplicates nil)
                              (save-interprogram-paste-before-kill t)
                              (interprogram-cut-function
                               (lambda (_) (error "Unexpected clipboard export")))
                              (interprogram-paste-function
                               (lambda () (error "Unexpected clipboard import"))))
                         (setq kill-ring-yank-pointer kill-ring)
                         {body})"""
        result = subprocess.run([EMACS, "-Q", "--batch", "--eval", expression],
                                cwd=self.directory, capture_output=True, text=True,
                                timeout=10, check=False)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, "")
        return result.stdout

    def test_original_recipe_can_return_payload_and_calls_clipboard_boundaries(self):
        result = self.native("""(let* ((exports nil)
                                       (imports 0)
                                       (interprogram-cut-function
                                        (lambda (text) (push text exports) text))
                                       (interprogram-paste-function
                                        (lambda () (setq imports (1+ imports)) "fixture-clipboard"))
                                       (value (kill-new "fixture-payload")))
                                  (prin1 (list (equal value "fixture-payload")
                                               (equal exports '("fixture-payload"))
                                               (= imports 1)
                                               (member "fixture-clipboard" kill-ring))))""")
        self.assertEqual(result, '(t t t ("fixture-clipboard" "fixture-prior-kill"))')

    def test_documented_form_preserves_text_point_and_pointer_without_clipboard_calls(self):
        payloads = ['Quotes " and slash \\ with ñ and λ', "first\r\nsecond\r\n", "  first\nsecond\t ", ""]
        for payload in payloads:
            with self.subTest(payload=repr(payload)):
                self.write_payload(payload)
                expected = json.dumps(payload, ensure_ascii=False)
                result = self.native(f"""(with-temp-buffer
                                           (insert "fixture-buffer")
                                           (goto-char 4)
                                           (let* ((original (current-buffer))
                                                  (value {self.form}))
                                             (prin1 (list (eq value 'paste-staged)
                                                          (equal-including-properties (car kill-ring) {expected})
                                                          (eq kill-ring-yank-pointer kill-ring)
                                                          (eq original (current-buffer))
                                                          (= (point) 4)
                                                          (equal (buffer-string) "fixture-buffer")
                                                          (equal (cdr kill-ring) '("fixture-prior-kill"))))))""")
                self.assertEqual(result, "(t t t t t t t)")
                self.assertEqual(self.payload.read_bytes(), payload.encode("utf-8"))
                self.payload.unlink()

    def test_documented_form_returns_fixed_acknowledgement_not_text(self):
        payload = "fixture-private-content-must-not-be-returned"
        self.write_payload(payload)
        result = self.native(f"(prin1 {self.form})")
        self.assertEqual(result, "paste-staged")
        self.assertNotIn(payload, result)

    def test_transform_change_rejection_and_error_are_not_silent_success(self):
        payload = "fixture-input-not-for-diagnostics"
        self.write_payload(payload)
        transforms = ['(lambda (_) "fixture-transformed")', '(lambda (_) nil)',
                      '(lambda (text) (error "Fixture transform saw %s" text))',
                      '(lambda (text) (aset text 0 ?X) text)',
                      '(lambda (text) (propertize text \'yank-handler \'(ignore)))']
        for transform in transforms:
            with self.subTest(transform=transform):
                result = self.native(f"""(condition-case failure
                                           (prin1 {self.form})
                                         (error (princ (error-message-string failure))))""",
                                     transform=transform)
                self.assertEqual(result, "Paste staging failed")
                self.assertNotIn(payload, result)

    def test_missing_payload_returns_sanitized_failure(self):
        result = self.native(f"""(condition-case failure
                                   (prin1 {self.form})
                                 (error (princ (error-message-string failure))))""")
        self.assertEqual(result, "Paste staging failed")
        self.assertNotIn(str(self.payload), result)

    def test_transform_rejection_is_not_certified_by_an_existing_matching_entry(self):
        self.write_payload("fixture-prior-kill")
        result = self.native(f"""(condition-case failure
                                   (prin1 {self.form})
                                 (error (princ (error-message-string failure))))""",
                             transform="(lambda (_) nil)")
        self.assertEqual(result, "Paste staging failed")

    def test_later_yank_lookup_can_supersede_a_verified_ring_entry(self):
        self.write_payload("fixture-staged")
        result = self.native(f"""(let ((value {self.form}))
                                  (let ((interprogram-cut-function nil)
                                        (interprogram-paste-function (lambda () "fixture-later-clipboard")))
                                    (prin1 (list (eq value 'paste-staged)
                                                 (equal (current-kill 0) "fixture-later-clipboard")
                                                 (equal (cadr kill-ring) "fixture-staged")))))""")
        self.assertEqual(result, "(t t t)")


if __name__ == "__main__":
    unittest.main()
