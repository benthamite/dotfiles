"""Tests for the update-log closeout receipt writer."""

from __future__ import annotations

import json
import io
import importlib.util
import os
import stat
import subprocess
import sys
import tempfile
import unittest
from concurrent.futures import ThreadPoolExecutor
from contextlib import redirect_stderr
from pathlib import Path
from unittest import mock


SCRIPT = Path(__file__).with_name("write_receipt.py")
SPEC = importlib.util.spec_from_file_location("receipt_writer_under_test", SCRIPT)
WRITER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(WRITER)


class WriteReceiptTest(unittest.TestCase):
    def run_writer(self, *arguments: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, "-B", "-W", "error::ResourceWarning", str(SCRIPT), *arguments],
            check=False,
            capture_output=True,
            text=True,
            timeout=10,
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

    def test_existing_file_and_symlink_targets_are_preserved(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            original = root / "original.json"
            original.write_bytes(b"existing private receipt\n")
            link = root / "receipt-link.json"
            link.symlink_to(original)
            dangling = root / "dangling.json"
            dangling.symlink_to(root / "absent.json")
            for target in (original, link, dangling):
                with self.subTest(target=target.name):
                    result = self.run_writer("--receipt-file", str(target), "--status", "success",
                                             "--message", "Synthetic closeout.")
                    self.assertEqual(result.returncode, 2)
                    self.assertNotIn("Traceback", result.stderr)
                    self.assertEqual(original.read_bytes(), b"existing private receipt\n")
            self.assertTrue(link.is_symlink())
            self.assertTrue(dangling.is_symlink())
            self.assertFalse((root / "absent.json").exists())

    def test_late_publication_collision_preserves_other_writer(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            target = Path(temporary) / "receipt.json"
            original_link = os.link

            def collide(source, destination, *args, **kwargs):
                target.write_bytes(b"concurrent receipt\n")
                return original_link(source, destination, *args, **kwargs)

            with mock.patch.object(WRITER.os, "link", side_effect=collide):
                with self.assertRaises(FileExistsError):
                    WRITER.write_atomic(target, {"status": "success"})
            self.assertEqual(target.read_bytes(), b"concurrent receipt\n")
            self.assertEqual(list(target.parent.glob(".*.tmp")), [])

    def test_malformed_evidence_is_failure_without_raw_value_disclosure(self) -> None:
        cases = [
            '{"status": []}', '{"status": {}}', '{"status": true}',
            '{"status": "success", "ok": 0}', '{"status": "success", "ok": null}',
            '{"status": "success", "ok": "false"}',
            '{"status": "success", "schema_version": true}',
            '{"status": "success", "schema_version": 2}',
            '{"status": "failure", "status": "success"}',
            '{"status": "success", "number": NaN}',
            '{"status": "success", "number": 1e999}',
            '{"status": "SYNTHETIC_PRIVATE_VALUE"}',
        ]
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            evidence = root / "evidence.json"
            for number, content in enumerate(cases):
                with self.subTest(number=number):
                    evidence.write_text(content, encoding="utf-8")
                    receipt = root / f"receipt-{number}.json"
                    result = self.run_writer("--receipt-file", str(receipt), "--status", "success",
                                             "--message", "Synthetic closeout.",
                                             "--evidence", str(evidence))
                    self.assertEqual(result.returncode, 1)
                    self.assertTrue(receipt.exists(), result.stderr)
                    payload = json.loads(receipt.read_text())
                    self.assertEqual(payload["status"], "failure")
                    self.assertNotIn("Traceback", result.stderr)
                    self.assertNotIn("SYNTHETIC_PRIVATE_VALUE", result.stderr + receipt.read_text())

    def test_blank_message_cannot_publish_success(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            target = Path(temporary) / "receipt.json"
            result = self.run_writer("--receipt-file", str(target), "--status", "success",
                                     "--message", " \n\t")
            self.assertEqual(result.returncode, 1)
            self.assertEqual(json.loads(target.read_text())["status"], "failure")

    def test_supported_success_and_no_op_evidence_shapes(self) -> None:
        cases = [
            {"status": "success"},
            {"status": "no-op"},
            {"schema_version": 1, "command": "closeout", "status": "success", "ok": True,
             "applied": True, "read_only": False, "commit": "a" * 40},
            {"schema_version": 1, "command": "closeout", "status": "no-op", "ok": True,
             "applied": False, "read_only": True, "commit": None},
        ]
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            for number, payload in enumerate(cases):
                with self.subTest(number=number):
                    evidence, receipt = root / f"input-{number}.json", root / f"output-{number}.json"
                    evidence.write_text(json.dumps(payload))
                    result = self.run_writer("--receipt-file", str(receipt), "--status", "success",
                                             "--message", "Synthetic closeout.", "--evidence", str(evidence))
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(json.loads(receipt.read_text())["evidence"], [payload])

    def test_failed_evidence_cannot_overwrite_an_existing_receipt(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            evidence, receipt = root / "evidence.json", root / "receipt.json"
            evidence.write_text('{"status":"failure","ok":false}')
            receipt.write_bytes(b"preserve existing receipt\n")
            result = self.run_writer("--receipt-file", str(receipt), "--status", "success",
                                     "--message", "Synthetic closeout.", "--evidence", str(evidence))
            self.assertEqual(result.returncode, 2)
            self.assertEqual(receipt.read_bytes(), b"preserve existing receipt\n")
            self.assertNotIn("Traceback", result.stderr)

    def test_unreadable_special_and_oversized_evidence_fail_without_hanging(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            regular = root / "regular.json"
            regular.write_text('{"status":"success"}')
            linked = root / "link.json"
            linked.symlink_to(regular)
            fifo = root / "fifo.json"
            os.mkfifo(fifo)
            oversized = root / "oversized.json"
            oversized.write_bytes(b" " * (WRITER.MAX_EVIDENCE_BYTES + 1))
            invalid = root / "invalid.json"
            invalid.write_bytes(b"\xffSYNTHETIC_PRIVATE_VALUE")
            for number, evidence in enumerate((linked, fifo, oversized, invalid, root, root / "missing")):
                with self.subTest(number=number):
                    receipt = root / f"failure-{number}.json"
                    result = self.run_writer("--receipt-file", str(receipt), "--status", "success",
                                             "--message", "Synthetic closeout.", "--evidence", str(evidence))
                    self.assertEqual(result.returncode, 1, result.stderr)
                    self.assertEqual(json.loads(receipt.read_text())["status"], "failure")
                    self.assertEqual(stat.S_IMODE(receipt.stat().st_mode), 0o600)
                    self.assertNotIn("Traceback", result.stderr)
                    self.assertNotIn("SYNTHETIC_PRIVATE_VALUE", result.stderr + receipt.read_text())

    def test_evidence_replacement_during_read_is_refused(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            evidence = Path(temporary) / "evidence.json"
            evidence.write_text('{"status":"success"}')
            original_fstat = os.fstat
            calls = 0

            def replace_after_read(descriptor):
                nonlocal calls
                calls += 1
                result = original_fstat(descriptor)
                if calls == 2:
                    replacement = evidence.with_name("replacement.json")
                    replacement.write_bytes(evidence.read_bytes())
                    os.replace(replacement, evidence)
                return result

            with mock.patch.object(WRITER.os, "fstat", side_effect=replace_after_read):
                with self.assertRaisesRegex(ValueError, "changed"):
                    WRITER.load_evidence(evidence)
            self.assertEqual(calls, 2)

    def test_new_parent_directories_are_private_without_changing_existing_modes(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            root.chmod(0o755)
            receipt = root / "new/child/receipt.json"
            result = self.run_writer("--receipt-file", str(receipt), "--status", "no-op",
                                     "--message", "No durable changes.")
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(stat.S_IMODE(root.stat().st_mode), 0o755)
            self.assertEqual(stat.S_IMODE((root / "new").stat().st_mode), 0o700)
            self.assertEqual(stat.S_IMODE(receipt.parent.stat().st_mode), 0o700)

    def test_publication_errors_are_nonzero_without_traceback(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            not_a_directory = root / "regular-file"
            not_a_directory.write_bytes(b"foreign content")
            for target in (root, not_a_directory / "receipt.json"):
                with self.subTest(target=target.name):
                    result = self.run_writer("--receipt-file", str(target), "--status", "success",
                                             "--message", "Synthetic closeout.")
                    self.assertEqual(result.returncode, 2)
                    self.assertNotIn("Traceback", result.stderr)
            self.assertEqual(not_a_directory.read_bytes(), b"foreign content")

    def test_public_concurrent_writers_have_exactly_one_winner(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            receipt = Path(temporary) / "receipt.json"

            def write(number):
                return self.run_writer("--receipt-file", str(receipt), "--status", "success",
                                       "--message", f"Synthetic writer {number}.")

            with ThreadPoolExecutor(max_workers=4) as executor:
                results = list(executor.map(write, range(4)))
            self.assertEqual(sorted(result.returncode for result in results), [0, 2, 2, 2])
            winner = next(index for index, result in enumerate(results) if result.returncode == 0)
            self.assertEqual(json.loads(receipt.read_text())["message"], f"Synthetic writer {winner}.")
            self.assertEqual(list(receipt.parent.glob(".*.tmp")), [])

    def test_serialized_size_failure_leaves_no_partial_receipt(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            receipt = Path(temporary) / "receipt.json"
            with mock.patch.object(WRITER, "MAX_RECEIPT_BYTES", 20):
                with self.assertRaisesRegex(ValueError, "limit"):
                    WRITER.write_atomic(receipt, {"status": "success", "message": "Synthetic output."})
            self.assertFalse(receipt.exists())
            self.assertEqual(list(receipt.parent.iterdir()), [])

    def test_directory_sync_failure_reports_uncertain_publication_without_overwrite(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            receipt = Path(temporary) / "receipt.json"
            original_fsync = os.fsync
            directory_seen = False

            def fail_directory_sync(descriptor):
                nonlocal directory_seen
                if stat.S_ISDIR(os.fstat(descriptor).st_mode):
                    directory_seen = True
                    raise OSError("SYNTHETIC_PRIVATE_VALUE")
                return original_fsync(descriptor)

            arguments = ["--receipt-file", str(receipt), "--status", "success",
                         "--message", "Synthetic closeout."]
            output = io.StringIO()
            with mock.patch.object(WRITER.os, "fsync", side_effect=fail_directory_sync):
                with redirect_stderr(output):
                    status = WRITER.main(arguments)
            self.assertTrue(directory_seen)
            self.assertEqual(status, 2)
            published = receipt.read_bytes()
            self.assertEqual(json.loads(published)["status"], "success")
            self.assertNotIn("SYNTHETIC_PRIVATE_VALUE", output.getvalue())
            self.assertIn("inspect target", output.getvalue())
            self.assertEqual(list(receipt.parent.glob(".*.tmp")), [])
            retry = self.run_writer(*arguments)
            self.assertEqual(retry.returncode, 2)
            self.assertEqual(receipt.read_bytes(), published)


if __name__ == "__main__":
    unittest.main()
