"""Tests for the update-log closeout receipt writer."""

from __future__ import annotations

import json
import stat
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).with_name("write_receipt.py")


class WriteReceiptTest(unittest.TestCase):
    def run_writer(self, *arguments: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(SCRIPT), *arguments],
            check=False,
            capture_output=True,
            text=True,
        )

    def test_success_embeds_valid_evidence_and_uses_private_permissions(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            evidence = root / "check.json"
            receipt = root / "closeout.json"
            evidence.write_text(
                json.dumps({"status": "success", "ok": True}), encoding="utf-8"
            )

            result = self.run_writer(
                "--receipt-file", str(receipt),
                "--status", "success",
                "--message", "Closeout complete.",
                "--evidence", str(evidence),
            )

            payload = json.loads(receipt.read_text(encoding="utf-8"))
            mode = stat.S_IMODE(receipt.stat().st_mode)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(payload["status"], "success")
        self.assertEqual(payload["evidence"], [{"status": "success", "ok": True}])
        self.assertEqual(mode, 0o600)

    def test_failed_evidence_writes_failure_receipt_and_returns_nonzero(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            evidence = root / "check.json"
            receipt = root / "closeout.json"
            evidence.write_text(
                json.dumps({"status": "failure", "ok": False}), encoding="utf-8"
            )

            result = self.run_writer(
                "--receipt-file", str(receipt),
                "--status", "success",
                "--message", "Closeout complete.",
                "--evidence", str(evidence),
            )

            payload = json.loads(receipt.read_text(encoding="utf-8"))

        self.assertEqual(result.returncode, 1)
        self.assertEqual(payload["status"], "failure")
        self.assertIn("non-success status", payload["message"])

    def test_explicit_failure_returns_nonzero(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            receipt = Path(temporary) / "closeout.json"
            result = self.run_writer(
                "--receipt-file", str(receipt),
                "--status", "failure",
                "--message", "Required check failed.",
            )
            payload = json.loads(receipt.read_text(encoding="utf-8"))

        self.assertEqual(result.returncode, 1)
        self.assertEqual(payload["status"], "failure")


if __name__ == "__main__":
    unittest.main()
