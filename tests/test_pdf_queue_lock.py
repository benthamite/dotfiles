"""The book-PDF queue ledger shared by the scheduled batch and the Emacs drain."""

import fcntl
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
COMMAND = ROOT / "bin" / "pdf-queue-lock"
_spec = importlib.util.spec_from_file_location(
    "pdf_queue_state", ROOT / "lib" / "python" / "pdf_queue_state.py")
queue = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(queue)


def record(status, **fields):
    return {"last_attempt": 1, "status": status, **fields}


class PdfQueueLockTest(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.state_dir = Path(directory.name)

    def run_cli(self, *args):
        return subprocess.run([sys.executable, str(COMMAND), "--state-dir", str(self.state_dir), *args],
                              capture_output=True, text=True, timeout=60)

    def books(self):
        return queue.read_state(self.state_dir)["books"]

    def test_conditional_merge_changes_only_the_named_record(self):
        queue.update_record(self.state_dir, "Alpha", record=record("staged", attempt="/a"))
        queue.update_record(self.state_dir, "Beta", record=record("running"))
        completed = self.run_cli("merge", "Alpha", json.dumps({"status": "attaching"}),
                                 "--expect-status", "staged")
        self.assertEqual(completed.returncode, 0, completed.stderr)
        self.assertEqual(json.loads(completed.stdout)["status"], "attaching")
        self.assertEqual(self.books(), {"Alpha": record("attaching", attempt="/a"),
                                        "Beta": record("running")})

    def test_status_mismatch_exits_3_without_writing(self):
        queue.update_record(self.state_dir, "Alpha", record=record("attaching"))
        before = (self.state_dir / "state.json").read_text()
        completed = self.run_cli("merge", "Alpha", json.dumps({"status": "attaching"}),
                                 "--expect-status", "staged")
        self.assertEqual(completed.returncode, 3)
        self.assertEqual((self.state_dir / "state.json").read_text(), before)
        self.assertEqual(self.run_cli("merge", "Absent", "{}").returncode, 3)

    def test_delete_clears_a_record(self):
        queue.update_record(self.state_dir, "Alpha", record=record("attach-error"))
        completed = self.run_cli("delete", "Alpha", "--expect-status", "attach-error")
        self.assertEqual(completed.returncode, 0, completed.stderr)
        self.assertEqual(completed.stdout.strip(), "null")
        self.assertEqual(self.books(), {})

    def test_invalid_change_is_refused(self):
        queue.update_record(self.state_dir, "Alpha", record=record("staged"))
        for fields in ('{"status": "invented"}', '["not an object"]', "not json"):
            with self.subTest(fields=fields):
                self.assertEqual(self.run_cli("merge", "Alpha", fields).returncode, 1)
        self.assertEqual(self.books()["Alpha"]["status"], "staged")

    def test_concurrent_writers_never_lose_each_others_records(self):
        script = ("import importlib.util, sys\n"
                  f"spec = importlib.util.spec_from_file_location('q', {str(_spec.origin)!r})\n"
                  "q = importlib.util.module_from_spec(spec); spec.loader.exec_module(q)\n"
                  "from pathlib import Path\n"
                  "for i in range(40):\n"
                  "    q.update_record(Path(sys.argv[1]), f'{sys.argv[2]}{i}',"
                  " record={'last_attempt': i, 'status': 'staged'})\n")
        writers = [subprocess.Popen([sys.executable, "-c", script, str(self.state_dir), prefix])
                   for prefix in ("batch", "drain")]
        for writer in writers:
            self.assertEqual(writer.wait(timeout=60), 0)
        self.assertEqual(len(self.books()), 80)

    def test_busy_lock_times_out_instead_of_hanging(self):
        with (self.state_dir / "state.lock").open("a") as lock:
            fcntl.flock(lock, fcntl.LOCK_EX)
            with self.assertRaises(TimeoutError):
                queue.update_record(self.state_dir, "Alpha", record=record("staged"), timeout=0.2)


if __name__ == "__main__":
    unittest.main()
